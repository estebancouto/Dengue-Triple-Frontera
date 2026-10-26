# Dengue en la Triple Frontera: scripts de análisis de tesis

[English](README.en.md) · [Revisión de reproducibilidad](docs/REPRODUCIBILITY.md) · [Inventario de scripts](docs/script_inventory.tsv)

Código en R de la tesis de Esteban Manuel Couto sobre vigilancia epidemiológica y dinámica del dengue en la Triple Frontera de Argentina, Brasil y Paraguay. El estudio observacional retrospectivo comprende notificaciones de casos sospechosos entre octubre de 2009 y septiembre de 2023 en Puerto Iguazú, Foz do Iguaçu, Ciudad del Este, Hernandarias, Minga Guazú y Presidente Franco, a partir de SNVS, SINAN y DGVS.

## Tesis doctoral de referencia

**Este README documenta los scripts de análisis de la tesis doctoral indicada a continuación.** El repositorio reúne los distintos componentes analíticos de la tesis; no corresponde exclusivamente a un artículo científico. Su utilización en publicaciones derivadas requiere identificar los módulos y resultados utilizados en cada manuscrito.

Couto, Esteban Manuel. *Dengue en la triple frontera entre Argentina, Brasil y Paraguay: dinámica temporal y comparación entre municipios según caracterización del sistema de salud y vigilancia*. Rio de Janeiro: Fundação Oswaldo Cruz, Escola Nacional de Saúde Pública Sergio Arouca; 2026. Tesis (Doctorado en Epidemiología en Salud Pública). 275 f.: ilustraciones en color y mapas. Bibliografía: f. 187–229.

- **Orientador:** Luiz Antônio Bastos Camacho.
- **Coorientadores:** Leonardo Soares Bastos y Zoraida del Carmen Fernandez Grillo.
- **Descriptores del registro bibliográfico:** Dengue; Saúde na Fronteira; Vigilância Epidemiológica; Sistemas de Saúde; Saúde Pública.

Referencia transcrita y normalizada a partir de la ficha bibliográfica proporcionada por el autor. Para citar la tesis, utilizar esta referencia; para identificar el código ejecutado, añadir la URL del repositorio y el commit o versión correspondiente.

## Código externo para la estimación de Rt

El cálculo de Rt utiliza funciones del trabajo de Codeço y colaboradores:

Codeço CT, Villela DAM, Coelho FC. Estimating the effective reproduction number of dengue considering temperature-dependent generation intervals. *Epidemics*. 2018;25:101–111. [doi:10.1016/j.epidem.2018.05.011](https://doi.org/10.1016/j.epidem.2018.05.011).

El método considera intervalos de generación dependientes de la temperatura y fue aplicado en el artículo a Foz do Iguaçu y Río de Janeiro. El [repositorio original](https://github.com/AlertaDengue/publication-data/tree/master/Codeco-et-al-2017) conserva la carpeta `Codeco-et-al-2017`, aunque la publicación definitiva es de **2018**.

Los archivos requeridos son:

- [EstRtGT_v4.R](https://github.com/AlertaDengue/publication-data/blob/master/Codeco-et-al-2017/EstRtGT_v4.R): incluye `est.R.Temp()` y `evalGenTimeDist()`.
- [sumgamma_v2.R](https://github.com/AlertaDengue/publication-data/blob/master/Codeco-et-al-2017/sumgamma_v2.R): contiene funciones para la distribución de sumas de variables gamma.
- [Rt_calc-example.rmd](https://github.com/AlertaDengue/publication-data/blob/master/Codeco-et-al-2017/Rt_calc-example.rmd): ejemplo de uso de los autores.

Los encabezados de los archivos atribuyen su desarrollo a **Daniel Villela**. Son código externo y no deben atribuirse al autor de esta tesis. Los dos archivos fueron localizados y revisados en su fuente pública, pero **no están incluidos en este paquete**. Para ejecutar los scripts preparados, deben estar disponibles en `R/external/`; confirmar los términos de reutilización antes de redistribuirlos.

**Dependencia pendiente:** `EstRtGT_v4.R` ejecuta `read.csv("./SE.csv")` al cargarse. `SE.csv` no figura en la carpeta consultada del repositorio original. Es necesario resolver esa lectura y comprobar su uso antes de ejecutar el análisis. La identificación de la fuente resuelve la procedencia del código, pero no acredita por sí misma la ejecución completa ni la equivalencia de resultados con la tesis.

## Estado y alcance

Esta versión organiza 18 scripts aportados por el autor para facilitar su revisión y su uso como material de apoyo de publicaciones. **No constituye todavía una reproducción validada de los resultados de la tesis o de un artículo.** No se incluyen bases individuales ni resultados numéricos verificados. Las poblaciones analíticas cambian entre módulos: no debe interpretarse el conjunto completo de notificaciones como el denominador de todos los análisis. En varias series `CP` se construye excluyendo descartados; debe cotejarse con la definición de caso probable del manuscrito.

Se conservaron cálculos, filtros y modelos. Se modificaron ubicaciones de scripts, llamadas `source()` y rutas dependientes de una computadora. Las versiones anteriores permanecen en el historial Git; los adjuntos del autor son la fuente de esta reorganización. Los dos scripts antiguos `2.armonizacion.R` y `3.Validacion_armo.R` se retiran del árbol activo para evitar confusión con las versiones `2.1` y `3.1`.

## Organización y dependencias

| Módulo | Archivos dentro de `scripts/` | Función y dependencias |
|---|---|---|
| Datos | `01_data/01_import.R` | Importación original; tramo de Argentina incompleto |
| Armonización | `01_data/02_harmonize.R` | Recodificación de bases nacionales y exportación a `source/csv/armo/` |
| Validación | `01_data/03_validate.R` | Tablas y gráficos de armonización; requiere bases armonizadas |
| Transmisión | `02_transmission/01_build_weekly_rt.R` | Series, población, clima y Rt; llama a los seis scripts de `rt/`; Brasil se calcula en el principal |
| Descriptores | `02_transmission/02_epidemic_features.R` y `03_feature_tables.R` | Indicadores epidémicos; el segundo vuelve a ejecutar el primero |
| Vigilancia | `03_surveillance/01_quality.R`, `02_quality_figures.R`, `03_quality_tables.R` | Indicadores y comparaciones; gráficos y tablas llaman al primero |
| Series | `04_time_series/01_monthly.R`, `02_weekly.R` | Series mensuales y semanales; requieren bases armonizadas |
| Integración | `04_time_series/03_integrated_figures.R` | Requiere series semanales, Rt, descriptores y archivo de sombreado |

`R/external/` reserva las funciones externas de Rt; `config/` contiene inventarios de paquetes y carpetas; `docs/` documenta entradas, procedencia y pendientes. Las rutas locales `source/` y `salida/` se mantienen para conservar las convenciones de los análisis y se excluyen de Git.

## Software y preparación

Se requiere R con soporte para `|>` (R ≥ 4.1); esto es un requisito sintáctico, no una versión validada. RStudio es opcional. La versión original de R y las versiones de paquetes no están documentadas en los archivos recibidos. `config/packages.txt` es un inventario estático, no un entorno congelado. Incluye `colorspaces`, cuyo nombre debe aclararse frente a `colorspace`, también usado en el código. Las exportaciones con `webshot`/`webshot2` pueden requerir un navegador y componentes adicionales. Revisar el uso de `parallel::mcmapply` según el sistema operativo.

Desde la raíz del repositorio:

```r
source("scripts/setup.R")        # crea carpetas e informa paquetes faltantes
source("scripts/check_syntax.R") # analiza sintaxis sin ejecutar los análisis
```

Los scripts originales utilizan `pacman::p_load()`, que puede instalar paquetes automáticamente. Revisar el inventario antes de ejecutarlos. No existe todavía `renv.lock`: no es posible restaurar el entorno original con `renv::restore()`. Tras completar y validar el flujo en un entorno limpio, registrar las versiones con `renv::snapshot()` y conservar `sessionInfo()`.

## Datos y ejecución

Las bases no se distribuyen. El acceso a SNVS, SINAN y DGVS debe gestionarse por los canales institucionales que correspondan, con autorización para el uso previsto; este repositorio no concede acceso ni permisos de redistribución. No se presupone que todas las fuentes tengan las mismas restricciones.

El [inventario de entradas](docs/input_inventory.tsv) indica las rutas literales esperadas por cada script. Colocar localmente los DBC de SINAN en `source/SINAN/`, los Excel de Paraguay y el catálogo de hospitales en `source/Paraguay/`, los insumos argentinos en `source/TAD/` y los CSV intermedios en `source/csv/`. Se necesitan también población anual, clima semanal y las dos funciones de `R/external/`. El inventario de rutas no reemplaza un diccionario de variables ni certifica la compatibilidad de las bases.

**No hay un comando de ejecución integral validado.** Con las entradas autorizadas completas y después de resolver los bloqueos, ejecutar los módulos desde la raíz. Por ejemplo, la armonización se invoca con:

```r
source("scripts/01_data/02_harmonize.R", encoding = "UTF-8")
```

Orden de dependencias: importación → armonización → validación y ramas de vigilancia/series/transmisión. La integración gráfica requiere previamente la serie semanal, Rt y los descriptores epidémicos. Los seis scripts de Rt necesitan objetos del script principal; no ejecutarlos en sesiones independientes. Los pendientes y errores detectados impiden asegurar que esa secuencia termine sin intervención.

## Salidas y uso en un artículo

Las salidas previstas incluyen tablas y figuras de armonización (`salida/articulo_1/`), calidad de vigilancia (`salida/articulo_2/`), Rt, descriptores y series (`salida/articulo_4/`). Son rutas del código original, no una correspondencia ya validada con tablas de un manuscrito. Completar [el registro de trazabilidad](docs/manuscript_traceability.tsv) con cada figura, tabla, versión de datos y verificación antes de citar resultados.

Para la versión enviada a una revista: resolver los bloqueos, ejecutar con datos autorizados, contrastar todas las salidas con el manuscrito, fijar dependencias y archivar una versión identificada por etiqueta y commit. No se ha asignado DOI ni publicado una versión final.

## Autoría, cita y licencia

Autor de los análisis: **Esteban Manuel Couto**. Contacto: [perfil de GitHub](https://github.com/estebancouto). Citar la tesis mediante la referencia anterior y, para el código, indicar la fecha de consulta y el commit utilizado. No se ha asignado DOI al repositorio ni se ha suministrado una referencia de artículo derivado.

No se ha seleccionado una licencia. La disponibilidad pública del repositorio no equivale a una licencia de reutilización. Confirmar por separado los derechos sobre código, funciones externas y datos.
