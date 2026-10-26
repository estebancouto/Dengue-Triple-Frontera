# Revisión técnica / Technical reproducibility review

Revisión estática: 26 de octubre de 2026. Los hallazgos corresponden a los scripts recibidos, no a resultados ejecutados. Static review only; findings are not validated analysis results.

## Bloqueos / Blockers

1. **Funciones externas ausentes / Missing Rt helpers.** `R/external/EstRtGT_v4.R` y `sumgamma_v2.R` no están incluidos. El principal usa `evalGenTimeDist` y `est.R.Temp`; La fuente pública se identificó: [Codeço et al., 2018](https://doi.org/10.1016/j.epidem.2018.05.011), [código original](https://github.com/AlertaDengue/publication-data/tree/master/Codeco-et-al-2017). Los encabezados atribuyen el código a Daniel Villela. No se han incorporado los archivos al paquete. Además, `EstRtGT_v4.R` lee `./SE.csv` al cargarse; este archivo no figura en la carpeta original consultada. Resolver esa dependencia y confirmar términos de redistribución.
2. **Importación argentina incompleta / Incomplete Argentine import.** `01_data/01_import.R` termina con una nota de reparación y no exporta los CSV argentinos requeridos por armonización. Falta además el flujo explícito para C2 nominal. No se inventaron conversiones.
3. **Sin entradas ni entorno original / Missing inputs and original environment.** No se suministraron datos, población, clima, catálogo de hospitales, `sessionInfo()` ni lockfile. No es posible validar denominadores, estimaciones o equivalencia con el manuscrito.
4. **Entrada auxiliar sin productor identificado / Unresolved auxiliary input.** Los módulos de transmisión e integración leen `salida/articulo_4/sombras_rt.csv`. Se crean objetos `sombras_rt`, pero no se identificó una exportación de ese archivo en los 18 scripts.
5. **Dependencias y objetos / Dependencies and objects.** `colorspaces` aparece en `p_load`, mientras otro script usa `colorspace`. En el principal Rt se construye `df.result.lo_br` con `ymin/ymax`, pero luego se usan `lower/upper` en gráficos y renombrados. Verificar las dimensiones de `conf.int` antes de corregir; no se sustituyó una fórmula por conjetura.

## Puntos importantes / Important review items

- Se reemplazan valores faltantes por cero de forma amplia en transmisión, descriptores e integración. Separar ausencia de notificación, ausencia de casos y falta de estimación Rt. / Distinguish missing reports, zero cases and missing Rt estimates.
- Se excluye la semana 53 y se mezclan `yearweek`, año calendario y año epidémico. Validar continuidad y correspondencia entre países. / Validate week conventions and temporal continuity.
- Los intervalos de Rt usan simulaciones (`nsim = 1000`) sin semilla explícita en los archivos recibidos; registrar una semilla y estrategia de RNG paralelo en la versión validada. No se añadió una semilla arbitraria que pudiera cambiar resultados. / Record validated RNG settings.
- El script principal ejecuta dos veces el subscript argentino. Verificar si la segunda llamada es necesaria. / Argentina Rt is sourced twice.
- `04_time_series/01_monthly.R` exporta `media_casos` también con nombres `feat_stl` y `feat_todos`. Revisar el objeto correcto y regenerar resultados. / Feature filenames currently receive the mean-count object.
- `03_surveillance/03_quality_tables.R` reutiliza `T2_periodo_por_municipio_crierre.doc` para cierre y VPP. `02_quality_figures.R` termina mostrando `graf3` pero guarda `graf2` en `Graf1.tiff`. Revisar colisiones y correspondencia figura/archivo. / Review overwritten outputs and plot-object mismatches.
- `CP` suele representar casos no descartados; confirmar que el texto del manuscrito no lo equipare automáticamente con una definición normativa de probable. / Check analysis-case definitions.
- Validar claves, cardinalidad de uniones, denominadores poblacionales, duplicados, recodificaciones diagnósticas y comparaciones múltiples antes de congelar resultados. / Validate joins, denominators, recodes and inference.

## Cambios realizados / Changes made

Los 18 scripts se reorganizaron por módulo; se actualizaron llamadas internas y rutas relativas, normalizando saltos de línea a LF. No se cambiaron cálculos, umbrales, filtros, parámetros de modelos ni objetos exportados. Los problemas anteriores se conservan visibles hasta su validación. El inventario registra los SHA-256 de los adjuntos originales. Las versiones remotas previas permanecen recuperables en Git.

## Verificación / Verification

- Confirmados: 18 scripts incorporados, rutas internas actualizadas, ausencia de rutas absolutas de Windows en la versión preparada, inventarios de paquetes e insumos.
- Dos llamadas `source()` externas siguen pendientes; se documentan sin crear implementaciones ficticias.
- No se detectaron credenciales mediante búsqueda estática; no se agregaron bases individuales ni salidas de análisis.
- R/Rscript no está disponible en el entorno de preparación. **No se ejecutó el parser de R, ningún modelo ni la generación de resultados.** Se incluye `scripts/check_syntax.R` para el control posterior con R. No se afirma que una inspección de texto equivalga a validación sintáctica.
- No se completaron pruebas con datos sintéticos, comparación con manuscrito ni reproducción numérica. These remain pending.

## Antes del envío / Before submission

Completar funciones externas e importación, resolver los errores documentados, ejecutar desde una sesión limpia, guardar `sessionInfo()`, generar `renv.lock` con el entorno validado, completar el registro de trazabilidad, decidir licencia y crear una versión archivada. La referencia de Rt está identificada en ambos README. La referencia e implementación de Tabataba todavía deben cotejarse con la tesis.
