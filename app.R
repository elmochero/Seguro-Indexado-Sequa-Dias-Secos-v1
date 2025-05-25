# Cargar librerías necesarias
library(shiny)
library(raster)
library(readxl)
library(dplyr)
library(zoo)
library(naniar)
library(hydroTSM)
library(hydroGOF)
library(matrixStats)
library(writexl)
library(maps)
library(viridis)
library(wesanderson)
library(exactextractr)
library(ncdf4)
library(reshape2)

# ----- Funciones generales -----

# (Opcional) Establecer directorio de trabajo, en Shiny lo ideal es usar rutas relativas o absolutas ya definidas
set_working_directory <- function(path) {
  setwd(path)
  getwd()
}

# Leer datos de PISCO (asegúrate de que la ruta y los archivos existan)
read_pisco_data <- function(path) {
  days <- seq(as.Date('1981-01-01'), as.Date('2016-12-31'), by = 'days')
  dpr_peru <- brick(paste0(path, '/pisco_dpr_v2.1.nc'))
  dtx_peru <- brick(paste0(path, '/pisco_dtx_v1.1.nc'))
  dtn_peru <- brick(paste0(path, '/pisco_dtn_v1.1.nc'))
  list(days = days, dpr_peru = dpr_peru, dtx_peru = dtx_peru, dtn_peru = dtn_peru)
}

# Extraer serie de tiempo para un punto de interés
extract_time_series <- function(data, x, y) {
  gauge <- data.frame(x = x, y = y)
  coordinates(gauge) <- ~ x + y
  dpr <- round(raster::extract(data$dpr_peru, gauge), 1) %>% as.vector()
  dtx <- round(raster::extract(data$dtx_peru, gauge), 1) %>% as.vector()
  dtn <- round(raster::extract(data$dtn_peru, gauge), 1) %>% as.vector()
  data.frame(Fecha = data$days, dpr = dpr, dtx = dtx, dtn = dtn)
}

# Filtrar los datos para los meses seleccionados
filter_data <- function(df, meses_a_analizar) {
  df$Mes <- as.numeric(format(as.Date(df$Fecha), "%m"))
  df$Anio <- as.numeric(format(as.Date(df$Fecha), "%Y"))
  df %>% filter(Mes %in% meses_a_analizar)
}

# Calcular los días secos consecutivos
calculate_dry_days <- function(df_seleccion, limite_dia_seco) {
  df_seleccion %>%
    mutate(dia_seco = ifelse(dpr <= limite_dia_seco, 1, 0)) %>%
    group_by(Anio) %>%
    mutate(dias_secos_consecutivos = ifelse(is.na(dia_seco), 0, ave(dia_seco, cumsum(dia_seco == 0), FUN = cumsum))) %>%
    ungroup()
}

# Calcular resultados para cada umbral de días consecutivos
calculate_results <- function(df_seleccion, valores_consecutivos, suma_asegurada) {
  resultados <- list()
  for (valor in valores_consecutivos) {
    df_res <- df_seleccion %>%
      group_by(Anio) %>%
      summarize(
        pago_anual = ifelse(any(dias_secos_consecutivos > valor), suma_asegurada, 0),
        probabilidad = ifelse(any(dias_secos_consecutivos > valor, na.rm = TRUE), 1, 0)
      )
    
    probabilidad_promedio <- mean(df_res$probabilidad, na.rm = TRUE)
    periodo_retorno <- ifelse(probabilidad_promedio > 0, 1 / probabilidad_promedio, NA)
    
    df_res <- df_res %>%
      mutate(
        probabilidad_promedio = probabilidad_promedio,
        periodo_retorno = periodo_retorno
        )
    
    # Si no hay ningún evento que supere el umbral, se omite
    if (max(df_seleccion$dias_secos_consecutivos, na.rm = TRUE) < valor) {
      next
    }
    
    resultados[[paste0("X = ", valor)]] <- df_res
  }
  resultados
}

# Graficar resultados: Se dibujan barras (una por cada umbral) en un panel
plot_results <- function(resultados, valores_consecutivos, suma_asegurada, df) {
  par(mfrow = c(1, length(valores_consecutivos)), mar = c(5, 5, 4, 2))
  total_asegurado <- suma_asegurada * length(unique(df$Anio))
  
  for (valor in valores_consecutivos) {
    datos_grafico <- resultados[[paste0("X = ", valor)]]
    if (is.null(datos_grafico) || all(datos_grafico$pago_anual == 0, na.rm = TRUE)) {
      # Si no existen datos para este umbral, se dibuja un gráfico vacío
      plot.new()
      title(main = paste("Pagos anuales (X =", valor, ")"))
      next
    }
    barplot(
      datos_grafico$pago_anual,
      names.arg = datos_grafico$Anio,
      main = paste("Pagos anuales (X =", valor, ")"),
      col = "skyblue",
      xlab = "Año",
      ylab = "Monto pagado",
      space = 0.5,
      border = "black",
      las = 2,
      cex.names = 0.8,
      ylim = c(0, max(datos_grafico$pago_anual, na.rm = TRUE) * 1.1)
    )
  }
}

# Generar texto de resultados (para mostrar en la app)
results_text <- function(resultados, valores_consecutivos, suma_asegurada, df) {
  total_asegurado <- suma_asegurada * length(unique(df$Anio))
  costos <- numeric(length(valores_consecutivos))
  out <- ""
  for (i in seq_along(valores_consecutivos)) {
    valor <- valores_consecutivos[i]
    datos_grafico <- resultados[[paste0("X = ", valor)]]
    if (is.null(datos_grafico)) next
    probabilidad_promedio <- mean(datos_grafico$probabilidad, na.rm = TRUE)
    periodo_retorno_promedio <- mean(datos_grafico$periodo_retorno, na.rm = TRUE)
    out <- paste0(out, "\nResultados para X = ", valor, ":\n",
                  "Probabilidad de ocurrencia anual del evento: ", round(probabilidad_promedio * 100, 2), "%\n",
                  "Periodo de retorno: ", round(periodo_retorno_promedio, 2), " años\n")
    costo_total <- sum(datos_grafico$pago_anual)
    costos[i] <- ifelse(total_asegurado == 0, NA, (costo_total / total_asegurado) * 100)
  }
  out <- paste0(out, "\nCosto del seguro como porcentaje del total asegurado:\n")
  for(i in seq_along(valores_consecutivos)) {
    out <- paste0(out, "X = ", valores_consecutivos[i], ": ", round(costos[i], 2), "%\n")
  }
  out
}

# ----- Interfaz Shiny -----
ui <- fluidPage(
  titlePanel("Análisis días secos consecutivos"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x", "Longitud (x) [ingrese la longitud en grados hasta un decimal]:", value = -69.3, min = -180, max = 180),
      numericInput("y", "Latitud (y) [ingrese la latitud en grados hasta un decimal]:", value = -18.3, min = -90, max = 90),
      numericInput("limite_dia_seco", "Valor límite para día seco (mm):", value = 1, min = 0),
      textInput("consecutive_days", "Días consecutivos (ingrese sólo tres valores separados por coma):", value = "3,4,5"),
      numericInput("suma_asegurada", "Suma asegurada por año:", value = 1000, min = 0),
      checkboxGroupInput("meses_a_analizar", "Meses a analizar:",
                         choices = 1:12, selected = c(1,2,3)),
      actionButton("analyze", "Analizar")
    ),
    mainPanel(
      plotOutput("plot"),
      br(),
      h4("Resultados del análisis:"),
      verbatimTextOutput("resultsText")
    )
  )
)

# ----- Lógica del Servidor -----
server <- function(input, output, session) {
  # Se ejecuta el análisis al presionar el botón "Analizar"
  analysis <- eventReactive(input$analyze, {
    set_working_directory('D:/Pisco-data')
    data <- read_pisco_data('D:/Pisco-data')
    df <- extract_time_series(data, input$x, input$y)
    df_seleccion <- filter_data(df, as.integer(input$meses_a_analizar))
    df_seleccion <- calculate_dry_days(df_seleccion, input$limite_dia_seco)
    consecutive_days <- as.integer(unlist(strsplit(input$consecutive_days, ",")))
    resultados <- calculate_results(df_seleccion, consecutive_days, input$suma_asegurada)
    list(df_seleccion = df_seleccion, resultados = resultados, consecutive_days = consecutive_days)
  })
  
  output$plot <- renderPlot({
    req(analysis())
    resultados <- analysis()$resultados
    suma_asegurada <- input$suma_asegurada
    
    # Borrar archivos anteriores
    old_files <- list.files(pattern = "^grafico_.*\\.png$")
    file.remove(old_files)
    
    par(mfrow = c(length(resultados), 1))  # Si deseas todos los gráficos en una misma ventana
    
    for (nombre in names(resultados)) {
      df_plot <- resultados[[nombre]]
      pagos <- df_plot$pago_anual
      nombres <- df_plot$Anio
      
      # Mostrar gráfico en la app
      barplot(
        pagos,
        names.arg = nombres,
        las = 2,
        col = "skyblue",
        main = paste("Pagos anuales (", nombre, ")"),
        ylab = "Monto pagado",
        cex.names = 0.7
      )
      
      # Guardar el mismo gráfico como PNG
      png_filename <- paste0("grafico_", gsub(" = ", "_", nombre), ".png")
      png(png_filename, width = 1000, height = 600, res = 120)
      barplot(
        pagos,
        names.arg = nombres,
        las = 2,
        col = "skyblue",
        main = paste("Pagos anuales (", nombre, ")"),
        ylab = "Monto pagado",
        cex.names = 0.8
      )
      dev.off()
    }
  })
  
  output$resultsText <- renderText({
    req(analysis())
    results_text(
      analysis()$resultados,
      analysis()$consecutive_days,
      input$suma_asegurada,
      analysis()$df_seleccion
    )
  })
}

# Ejecutar la app Shiny
shinyApp(ui = ui, server = server)
