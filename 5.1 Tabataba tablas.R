# Analisis de indicadores epidemiologicos
# Tesis de Doctorado - INMET/Fiocruz
# Autor: Esteban Manuel Couto

# Librerias----
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2, tableone, 
  RColorBrewer, scales, gt, data.table, gridExtra, cowplot, writexl,
  R0
)

# Directorio de trabajo
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")

# Carga de datos
source("source/script/5.Analisis tabataba.R")

todos<-list(
  IGR     = descriptores_IGR_2,
  IGU     = descriptores_IGU_2,
  PY_MG   = descriptores_MG_2,
  PY_HER  = descriptores_HER_2,
  PY_PF   = descriptores_PF_2,
  PY_CDE  = descriptores_CDE_2
)


## 1) Añadir idx_DcFin (la SE numérica) dentro de cada tibble de `todos`
todos_idx <- imap(
  todos,
  ~ .x %>% 
    mutate(
      # extrae el número antes de " - 2010", " - 2011", etc.
      idx_DcFin = str_extract(DcFin, "^[0-9]+") %>% as.integer(),
      lugar     = .y  # nombre de la lista: IGR, IGU, PY_MG, etc.
    )
)

## 2) Crear resumen de extremos (máximos y mínimos) para cada lugar
vars_intervalo <- c(
  "Incidencia", "Velocidad", "Dsmax", "Rtmax", "Dcmax",
  "idx_DcFin", "Tp", "idx_rT"
)

resumen_extremos <- imap_dfr(
  todos_idx,
  function(df, nombre_lugar) {
    
    # --- máximos ---
    tabla_max <- map_dfr(
      vars_intervalo,
      function(v) {
        df_ok <- df %>% filter(!is.na(.data[[v]]))
        if (nrow(df_ok) == 0) return(NULL)
        
        fila <- df_ok %>% slice_max(.data[[v]], n = 1, with_ties = FALSE)
        
        tibble(
          lugar     = nombre_lugar,
          indicador = v,
          tipo      = "max",
          valor     = fila[[v]],
          ANIO_EPI_LAB  = fila$ANIO_EPI_LAB
        )
      }
    )
    
    # --- mínimos ---
    tabla_min <- map_dfr(
      vars_intervalo,
      function(v) {
        df_ok <- df %>% filter(!is.na(.data[[v]]))
        if (nrow(df_ok) == 0) return(NULL)
        
        fila <- df_ok %>% slice_min(.data[[v]], n = 1, with_ties = FALSE)
        
        tibble(
          lugar     = nombre_lugar,
          indicador = v,
          tipo      = "min",
          valor     = fila[[v]],
          ANIO_EPI_LAB  = fila$ANIO_EPI_LAB
        )
      }
    )
    
    bind_rows(tabla_max, tabla_min)
  }
)

resumen_extremos

## 1) Tabla ancha de valores numéricos ----------------------------
resumen_valor_ancho <- resumen_extremos %>% 
  mutate(
    col_valor = paste(indicador, tipo, "valor", sep = "_")
  ) %>% 
  select(lugar, col_valor, valor) %>% 
  pivot_wider(
    names_from  = col_valor,
    values_from = valor
  )

## 2) Tabla ancha de años (ANIO_EPI_LAB) --------------------------
resumen_anio_ancho <- resumen_extremos %>% 
  mutate(
    col_anio = paste(indicador, tipo, "ANIO_EPI_LAB", sep = "_")
  ) %>% 
  select(lugar, col_anio, ANIO_EPI_LAB) %>% 
  pivot_wider(
    names_from  = col_anio,
    values_from = ANIO_EPI_LAB
  )

## 3) Unir todo por lugar -----------------------------------------
resumen_ancho <- resumen_valor_ancho %>% 
  left_join(resumen_anio_ancho, by = "lugar")

resumen_ancho


# Crear valores por año 
base_unida <- bind_rows(todos_idx)

vars_max <- c("Incidencia", "Velocidad", "Dsmax", "Rtmax", "Dcmax", "idx_DcFin")
vars_min <- c("Tp", "idx_rT")

# máximos por año e indicador (entre todos los lugares)
extremos_max_anio <- map_dfr(
  vars_max,
  function(v) {
    base_unida %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(ANIO_EPI_LAB) %>%
      slice_max(.data[[v]], n = 1, with_ties = FALSE) %>%  # rompe empates arbitrariamente
      ungroup() %>%
      transmute(
        ANIO_EPI_LAB,
        indicador = v,
        tipo      = "max",
        valor     = .data[[v]],
        lugar
      )
  }
)

# mínimos por año e indicador (entre todos los lugares)
extremos_min_anio <- map_dfr(
  vars_min,
  function(v) {
    base_unida %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(ANIO_EPI_LAB) %>%
      slice_min(.data[[v]], n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        ANIO_EPI_LAB,
        indicador = v,
        tipo      = "min",
        valor     = .data[[v]],
        lugar
      )
  }
)

extremos_anio <- bind_rows(extremos_max_anio, extremos_min_anio)

  # Tabla ancha de valores numéricos
  extremos_valor_ancho <- extremos_anio %>%
  mutate(
    col_valor = paste(indicador, tipo, "valor", sep = "_")
  ) %>%
  select(ANIO_EPI_LAB, col_valor, valor) %>%
  pivot_wider(
    names_from  = col_valor,
    values_from = valor
  )

#  Tabla ancha de lugares
extremos_lugar_ancho <- extremos_anio %>%
  mutate(
    col_lugar = paste(indicador, tipo, "lugar", sep = "_")
  ) %>%
  select(ANIO_EPI_LAB, col_lugar, lugar) %>%
  pivot_wider(
    names_from  = col_lugar,
    values_from = lugar
  )

# Unir ambas por año
extremos_anio_ancho <- extremos_valor_ancho %>%
  left_join(extremos_lugar_ancho, by = "ANIO_EPI_LAB")

extremos_anio_ancho



# Resumen de promedios y medianas por indicador y tipo
resumen_municipio <- base_unida %>% 
  group_by(lugar) %>% 
  summarise(
    # tamaño de epidemia
    Total_med    = median(Total, na.rm = TRUE),
    Total_min    = min(Total,   na.rm = TRUE),
    Total_max    = max(Total,   na.rm = TRUE),
    
    # incidencia
    Inc_med      = median(Incidencia, na.rm = TRUE),
    Inc_min      = min(Incidencia,    na.rm = TRUE),
    Inc_max      = max(Incidencia,    na.rm = TRUE),
    
    # velocidad de ascenso
    Vel_med      = median(Velocidad,  na.rm = TRUE),
    Vel_min      = min(Velocidad,     na.rm = TRUE),
    Vel_max      = max(Velocidad,     na.rm = TRUE),
    
    # duración del tramo de alta incidencia / intensidad
    Dsmax_med    = median(Dsmax,      na.rm = TRUE),
    Dsmax_min    = min(Dsmax,         na.rm = TRUE),
    Dsmax_max    = max(Dsmax,         na.rm = TRUE),
    
    # duración en casos (acumulación hasta Dcmax)
    Dcmax_med    = median(Dcmax,      na.rm = TRUE),
    Dcmax_min    = min(Dcmax,         na.rm = TRUE),
    Dcmax_max    = max(Dcmax,         na.rm = TRUE),
    
    # máxima transmisibilidad
    Rtmax_med    = median(Rtmax,      na.rm = TRUE),
    Rtmax_min    = min(Rtmax,         na.rm = TRUE),
    Rtmax_max    = max(Rtmax,         na.rm = TRUE),
    
    # momento del pico
    Tp_med       = median(Tp,         na.rm = TRUE),
    Tp_min       = min(Tp,            na.rm = TRUE),
    Tp_max       = max(Tp,            na.rm = TRUE),
    
    # momento del máximo Rt que usaste (idx_rT)
    idx_rT_med   = median(idx_rT,     na.rm = TRUE),
    idx_rT_min   = min(idx_rT,        na.rm = TRUE),
    idx_rT_max   = max(idx_rT,        na.rm = TRUE),
    
    .groups = "drop"
  )

#Acomodo
resumen_lindo <- resumen_municipio %>% 
  # construir columnas combinando mediana y rango
  mutate(
    Total       = sprintf("%.0f (%.0f–%.0f)", Total_med,   Total_min,   Total_max),
    Incidencia  = sprintf("%.1f (%.1f–%.1f)", Inc_med,     Inc_min,     Inc_max),
    Velocidad   = sprintf("%.2f (%.2f–%.2f)", Vel_med,     Vel_min,     Vel_max),
    Dsmax       = sprintf("%.0f (%.0f–%.0f)", Dsmax_med,   Dsmax_min,   Dsmax_max),
    Dcmax       = sprintf("%.0f (%.0f–%.0f)", Dcmax_med,   Dcmax_min,   Dcmax_max),
    Rtmax       = sprintf("%.1f (%.1f–%.1f)", Rtmax_med,   Rtmax_min,   Rtmax_max),
    Tp          = sprintf("%.0f (%.0f–%.0f)", Tp_med,      Tp_min,      Tp_max),
    idx_rT      = sprintf("%.0f (%.0f–%.0f)", idx_rT_med,  idx_rT_min,  idx_rT_max)
  ) %>% 
  # dejar solo columnas finales para la tabla
  select(
    lugar, Total, Incidencia, Velocidad, Dsmax, Dcmax, Rtmax, Tp, idx_rT
  )

resumen_lindo


# Guardar resumen en Excel
write_xlsx(resumen_ancho, "salida/articulo_4/Indicadores/resumen_extremos_tabataba.xlsx")
write_xlsx(extremos_anio, "salida/articulo_4/Indicadores/extremos_por_anio_tabataba.xlsx")
write_xlsx(extremos_anio_ancho, "salida/articulo_4/Indicadores/extremos_por_anio_ancho_tabataba.xlsx")
write_xlsx(resumen_lindo, "salida/articulo_4/Indicadores/resumen_municipio_tabataba.xlsx")
