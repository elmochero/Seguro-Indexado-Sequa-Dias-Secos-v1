# Seguro Indexado Sequía - Días Secos v1

Esta aplicación en Shiny calcula la **tasa pura de riesgo** para un **seguro indexado por sequía**, basado en el número de **días secos consecutivos**. Utiliza datos diarios de precipitación de la base **PISCO (SENAMHI)** para el período **1981–2016**.

Es la **versión 1.0** de una serie de herramientas aplicadas al diseño de **seguros agroclimáticos paramétricos**.

---

## 📂 Estructura del proyecto


---

## 📌 Requisitos

- R y RStudio instalados
- Paquetes: `shiny`, `raster`, `ncdf4`, `dplyr`, `ggplot2`, etc.
- Archivos NetCDF del SENAMHI:
  - `pisco_dpr_v2.1.nc`
  - `pisco_dtn_v1.1.nc`
  - `pisco_dtx_v1.1.nc`

---

## 🚀 Instrucciones

1. Descargue los archivos NetCDF y colóquelos en una carpeta (por defecto `D:/Pisco-data`).
2. Ejecute `app.R` desde RStudio.
3. Ingrese coordenadas, umbrales, meses y presione **“Analizar”**.
4. Se mostrarán los resultados y se guardarán automáticamente los gráficos generados.

---

## 📄 Documentación

Incluye un tutorial en Word con ejemplos paso a paso:

📁 `documentos/Seguro indexado sequía-días secos v 1.0.docx`

---

## 🛡️ Licencia

Este proyecto puede ser usado libremente con fines académicos, investigativos o de desarrollo de políticas públicas. Para fines comerciales, por favor contactar al autor.

Este proyecto está licenciado bajo una Licencia **Creative Commons Atribución-NoComercial 4.0 Internacional (CC BY-NC 4.0)**.  
Consulta el texto completo de la licencia [aquí](https://creativecommons.org/licenses/by-nc/4.0/).


---

## 🙌 Autor

Yonel — desarrollado con el apoyo de herramientas de código abierto y datos del SENAMHI.
