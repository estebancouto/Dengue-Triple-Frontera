
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
# Ejecutar desde la raíz del proyecto / Run from the project root.
# Carga de datos
Base_SE_Final<-read.csv2("source/csv/tabataba/Base_SE_Final_CP.csv")


# cargo funciones----
# Función para contar días consecutivos con casos > 5 y resetear el contador si hay un día con casos <= 5
reset_counter <- function(x) {
  counter <- 0
  result <- integer(length(x))
  for (i in seq_along(x)) {
    if (x[i] > 5) {
      counter <- counter + 1
    } else {
      counter <- 0
    }
    result[i] <- counter
  }
  result
}

# Función para contar días consecutivos con casos = 0 y resetear el contador si hay un día con casos > 0
reset_counter_sin <- function(x) {
  counter <- 0
  result <- integer(length(x))
  for (i in seq_along(x)) {
    if (x[i] == 0) {
      counter <- counter + 1
    } else {
      counter <- 0
    }
    result[i] <- counter
  }
  result
}  

#Función para contar días consecutivos con positividad > 0.3 y resetear el contador si hay un día con positividad <= 0.3
counter_positividad <- function(x) {
  counter <- 0
  result <- integer(length(x))
  for (i in seq_along(x)) {
    if (x[i] > 0.3) {
      counter <- counter + 1
    } else {
      counter <- 0
    }
    result[i] <- counter
  }
  result
}



# Función para contar días consecutivos con rT > 1 y resetear el contador si hay un día con rT <= 1
reset_counter_rT <- function(x) {
  counter <- 0
  result <- integer(length(x))
  for (i in seq_along(x)) {
    if (x[i] > 1) {
      counter <- counter + 1
    } else {
      counter <- 0
    }
    result[i] <- counter
  }
  result
}

# Pre-procesamiento de datos----

Base_SE_Final <- Base_SE_Final %>%
  mutate(
    ANIO_EPI = if_else(SE >= 40, YEAR, YEAR - 1),
    ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1),
    SE_epi = if_else(SE >= 40, SE - 39, SE + 13)  # Re-etiquetado de SE para año epidemiológico
  ) %>% filter(ANIO_EPI < 2023) %>%
  mutate(across(everything(), ~replace(., is.na(.), 0))) %>%
  arrange(ANIO_EPI, SE_epi) %>%
  mutate(
    Den_ar_IGR_transmision_Totales_SE_ = reset_counter(Den_ar_Total_CP),
    Den_ar_IGR_transmision_Confirmados_SE_ = reset_counter(Den_ar_Total_CC),
    Den_br_IGU_transmision_Totales_SE_ = reset_counter(Den_br_Total_CP),
    Den_br_IGU_transmision_Confirmados_SE_ = reset_counter(Den_br_Total_CC),
    Den_py_AP_transmision_Totales_SE_ = reset_counter(Den_py_AP_Total_CP),
    Den_py_AP_transmision_Confirmados_SE_ = reset_counter(Den_py_AP_Total_CC),
    Den_ar_IGR_transmision_rT_SE_ = reset_counter_rT(Den_ar_IGR_Total_Rt),
    Den_ar_IGR_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_ar),
    Den_br_IGU_transmision_rT_SE_ = reset_counter_rT(Den_br_IGU_Total_Rt),
    Den_br_IGU_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_br),
    Den_py_AP_transmision_rT_SE_ = reset_counter_rT(Den_py_AP_Total_Rt),
    Den_py_AP_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_py_AP),
    Den_ar_IGR_transmision_Totales_SIN_ = reset_counter_sin(Den_ar_Total_CP),
    Den_ar_IGR_transmision_Confirmados_SIN_ = reset_counter_sin(Den_ar_Total_CC),
    Den_br_IGU_transmision_Totales_SIN_ = reset_counter_sin(Den_br_Total_CP),
    Den_br_IGU_transmision_Confirmados_SIN_ = reset_counter_sin(Den_br_Total_CC),
    Den_py_AP_transmision_Totales_SIN_ = reset_counter_sin(Den_py_AP_Total_CP),
    Den_py_AP_transmision_Confirmados_SIN_ = reset_counter_sin(Den_py_AP_Total_CC),
    
    # py_MG
    Den_py_MG_transmision_Totales_SE_ = reset_counter(Den_py_MG_Total_CP),
    Den_py_MG_transmision_Confirmados_SE_ = reset_counter(Den_py_MG_Total_CC),
    Den_py_MG_transmision_rT_SE_ = reset_counter_rT(Den_py_MG_Total_Rt),
    Den_py_MG_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_py_MG),
    Den_py_MG_transmision_Totales_SIN_ = reset_counter_sin(Den_py_MG_Total_CP),
    Den_py_MG_transmision_Confirmados_SIN_ = reset_counter_sin(Den_py_MG_Total_CC),
    
    # py_HER
    Den_py_HER_transmision_Totales_SE_ = reset_counter(Den_py_HER_Total_CP),
    Den_py_HER_transmision_Confirmados_SE_ = reset_counter(Den_py_HER_Total_CC),
    Den_py_HER_transmision_rT_SE_ = reset_counter_rT(Den_py_HER_Total_Rt),
    Den_py_HER_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_py_HER),
    Den_py_HER_transmision_Totales_SIN_ = reset_counter_sin(Den_py_HER_Total_CP),
    Den_py_HER_transmision_Confirmados_SIN_ = reset_counter_sin(Den_py_HER_Total_CC),
    
    # py_PF
    Den_py_PF_transmision_Totales_SE_ = reset_counter(Den_py_PF_Total_CP),
    Den_py_PF_transmision_Confirmados_SE_ = reset_counter(Den_py_PF_Total_CC),
    Den_py_PF_transmision_rT_SE_ = reset_counter_rT(Den_py_PF_Total_Rt),
    Den_py_PF_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_py_PF),
    Den_py_PF_transmision_Totales_SIN_ = reset_counter_sin(Den_py_PF_Total_CP),
    Den_py_PF_transmision_Confirmados_SIN_ = reset_counter_sin(Den_py_PF_Total_CC),
    
    # py_CDE
    Den_py_CDE_transmision_Totales_SE_ = reset_counter(Den_py_CDE_Total_CP),
    Den_py_CDE_transmision_Confirmados_SE_ = reset_counter(Den_py_CDE_Total_CC),
    Den_py_CDE_transmision_rT_SE_ = reset_counter_rT(Den_py_CDE_Total_Rt),
    Den_py_CDE_transmision_TasaPositividad_SE = counter_positividad(positividad_lab_py_CDE),
    Den_py_CDE_transmision_Totales_SIN_ = reset_counter_sin(Den_py_CDE_Total_CP),
    Den_py_CDE_transmision_Confirmados_SIN_ = reset_counter_sin(Den_py_CDE_Total_CC)
  )


# Análisis de indicadores epidemiológicos----
# --- IGR (Argentina) ----
descriptores_IGR_2 <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_ar_Total_CP, na.rm = TRUE),
    Incidencia = Total /mean(Pop_ar_IGR, na.rm = TRUE)* 100000,
    
    Pico_N  = max(Den_ar_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_ar_Total_CP))) NA_integer_ else which.max(Den_ar_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_ar_IGR_transmision_Totales_SE_ == 3,   na.rm = TRUE),
    Dcmax = max(Den_ar_IGR_transmision_Totales_SE_,        na.rm = TRUE),
    Ds3   = sum(Den_ar_IGR_transmision_Totales_SIN_ == 3,  na.rm = TRUE),
    Dsmax = max(Den_ar_IGR_transmision_Totales_SIN_,       na.rm = TRUE),
    Rtmax = max(Den_ar_IGR_transmision_rT_SE_,             na.rm = TRUE),
    
    # Fin "simple": primera SE con totales = 0 luego del pico
    DcFin = {
      idx <- which(Den_ar_IGR_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    # Fin "confirmado": además con positividad <0,3 y Rt ≤1
    DcFin_Conf = {
      idx <- which(
        Den_ar_IGR_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_ar_IGR_transmision_TasaPositividad_SE == 0 &  # <30 %
          Den_ar_IGR_transmision_rT_SE_ == 0                # Rt ≤ 1
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_ar_IGR_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_ar_IGR_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_ar_IGR_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_ar, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_ar_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT]          else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>%
  ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) 

descriptores_IGR<-descriptores_IGR_2 %>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# --- AP (Paraguay) ----
descriptores_AP <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_py_AP_Total_CP, na.rm = TRUE),
    Incidencia = sum(Den_py_AP_incidencia, na.rm = TRUE),
    
    Pico_N  = max(Den_py_AP_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_py_AP_Total_CP))) NA_integer_ else which.max(Den_py_AP_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_py_AP_transmision_Totales_SE_ == 3, na.rm = TRUE),
    Dcmax = max(Den_py_AP_transmision_Totales_SE_, na.rm = TRUE),
    Ds3   = sum(Den_py_AP_transmision_Totales_SIN_ == 3, na.rm = TRUE),
    Dsmax = max(Den_py_AP_transmision_Totales_SIN_, na.rm = TRUE),
    Rtmax = max(Den_py_AP_transmision_rT_SE_, na.rm = TRUE),
    
    DcFin = {
      idx <- which(Den_py_AP_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    DcFin_Conf = {
      idx <- which(
        Den_py_AP_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_py_AP_transmision_TasaPositividad_SE == 0 &
          Den_py_AP_transmision_rT_SE_ == 0
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_py_AP_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_py_AP_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_py_AP_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_py_AP, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_py_AP_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT] else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>% ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) %>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# --- IGU (Brasil) ----
descriptores_IGU_2 <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_br_Total_CP, na.rm = TRUE),
    Incidencia = Total /mean(Pop_br_IGU, na.rm = TRUE)* 100000,
      Pico_N  = max(Den_br_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_br_Total_CP))) NA_integer_ else which.max(Den_br_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_br_IGU_transmision_Totales_SE_ == 3, na.rm = TRUE),
    Dcmax = max(Den_br_IGU_transmision_Totales_SE_, na.rm = TRUE),
    Ds3   = sum(Den_br_IGU_transmision_Totales_SIN_ == 3, na.rm = TRUE),
    Dsmax = max(Den_br_IGU_transmision_Totales_SIN_, na.rm = TRUE),
    Rtmax = max(Den_br_IGU_transmision_rT_SE_, na.rm = TRUE),
    
    DcFin = {
      idx <- which(Den_br_IGU_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    DcFin_Conf = {
      idx <- which(
        Den_br_IGU_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_br_IGU_transmision_TasaPositividad_SE == 0 &
          Den_br_IGU_transmision_rT_SE_ == 0
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_br_IGU_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_br_IGU_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_br_IGU_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_br, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_br_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT] else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>% ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) 
descriptores_IGU <- descriptores_IGU_2%>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# --- MG (Paraguay) ----
descriptores_MG_2 <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_py_MG_Total_CP, na.rm = TRUE),
    Incidencia = Total /mean(Pop_py_MG, na.rm = TRUE)* 100000,
    Pico_N  = max(Den_py_MG_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_py_MG_Total_CP))) NA_integer_ else which.max(Den_py_MG_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_py_MG_transmision_Totales_SE_ == 3, na.rm = TRUE),
    Dcmax = max(Den_py_MG_transmision_Totales_SE_, na.rm = TRUE),
    Ds3   = sum(Den_py_MG_transmision_Totales_SIN_ == 3, na.rm = TRUE),
    Dsmax = max(Den_py_MG_transmision_Totales_SIN_, na.rm = TRUE),
    Rtmax = max(Den_py_MG_transmision_rT_SE_, na.rm = TRUE),
    
    DcFin = {
      idx <- which(Den_py_MG_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    DcFin_Conf = {
      idx <- which(
        Den_py_MG_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_py_MG_transmision_TasaPositividad_SE == 0 &
          Den_py_MG_transmision_rT_SE_ == 0
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_py_MG_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_py_MG_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_py_MG_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_py_MG, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_py_MG_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT] else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>% ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) 
descriptores_MG<- descriptores_MG_2 %>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# --- HER (Paraguay) ----
descriptores_HER_2 <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_py_HER_Total_CP, na.rm = TRUE),
    Incidencia = Total /mean(Pop_py_HERNANDARIAS, na.rm = TRUE)* 100000,
    Pico_N  = max(Den_py_HER_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_py_HER_Total_CP))) NA_integer_ else which.max(Den_py_HER_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_py_HER_transmision_Totales_SE_ == 3, na.rm = TRUE),
    Dcmax = max(Den_py_HER_transmision_Totales_SE_, na.rm = TRUE),
    Ds3   = sum(Den_py_HER_transmision_Totales_SIN_ == 3, na.rm = TRUE),
    Dsmax = max(Den_py_HER_transmision_Totales_SIN_, na.rm = TRUE),
    Rtmax = max(Den_py_HER_transmision_rT_SE_, na.rm = TRUE),
    
    DcFin = {
      idx <- which(Den_py_HER_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    DcFin_Conf = {
      idx <- which(
        Den_py_HER_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_py_HER_transmision_TasaPositividad_SE == 0 &
          Den_py_HER_transmision_rT_SE_ == 0
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_py_HER_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_py_HER_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_py_HER_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_py_HER, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_py_HER_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT] else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>% ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) 
descriptores_HER<- descriptores_HER_2 %>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# --- PF (Paraguay) ----
descriptores_PF_2 <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_py_PF_Total_CP, na.rm = TRUE),
    Incidencia = Total /mean(Pop_py_PF, na.rm = TRUE)* 100000,
    
    Pico_N  = max(Den_py_PF_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_py_PF_Total_CP))) NA_integer_ else which.max(Den_py_PF_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_py_PF_transmision_Totales_SE_ == 3, na.rm = TRUE),
    Dcmax = max(Den_py_PF_transmision_Totales_SE_, na.rm = TRUE),
    Ds3   = sum(Den_py_PF_transmision_Totales_SIN_ == 3, na.rm = TRUE),
    Dsmax = max(Den_py_PF_transmision_Totales_SIN_, na.rm = TRUE),
    Rtmax = max(Den_py_PF_transmision_rT_SE_, na.rm = TRUE),
    
    DcFin = {
      idx <- which(Den_py_PF_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    DcFin_Conf = {
      idx <- which(
        Den_py_PF_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_py_PF_transmision_TasaPositividad_SE == 0 &
          Den_py_PF_transmision_rT_SE_ == 0
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_py_PF_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_py_PF_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_py_PF_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_py_PF, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_py_PF_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT] else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>% ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) 
descriptores_PF <-descriptores_PF_2 %>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# --- CDE (Paraguay) ----
descriptores_CDE_2 <- Base_SE_Final %>%
  group_by(ANIO_EPI) %>%
  summarize(
    Total      = sum(Den_py_CDE_Total_CP, na.rm = TRUE),
    Incidencia = Total /mean(Pop_py_CDE, na.rm = TRUE)* 100000,
    Pico_N  = max(Den_py_CDE_Total_CP, na.rm = TRUE),
    Tp_idx  = if (all(is.na(Den_py_CDE_Total_CP))) NA_integer_ else which.max(Den_py_CDE_Total_CP),
    Tp      = if (!is.na(Tp_idx)) SE_epi[Tp_idx] else NA_real_,
    Pico_SE = if (!is.na(Tp_idx)) paste(SE[Tp_idx], "-", YEAR[Tp_idx]) else NA_character_,
    
    Dc3   = sum(Den_py_CDE_transmision_Totales_SE_ == 3, na.rm = TRUE),
    Dcmax = max(Den_py_CDE_transmision_Totales_SE_, na.rm = TRUE),
    Ds3   = sum(Den_py_CDE_transmision_Totales_SIN_ == 3, na.rm = TRUE),
    Dsmax = max(Den_py_CDE_transmision_Totales_SIN_, na.rm = TRUE),
    Rtmax = max(Den_py_CDE_transmision_rT_SE_, na.rm = TRUE),
    
    DcFin = {
      idx <- which(Den_py_CDE_transmision_Totales_SE_ == 0 & SE_epi > Tp)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    DcFin_Conf = {
      idx <- which(
        Den_py_CDE_transmision_Totales_SE_ == 0 &
          SE_epi > Tp &
          Den_py_CDE_transmision_TasaPositividad_SE == 0 &
          Den_py_CDE_transmision_rT_SE_ == 0
      )
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    idx_rT = {
      cnt <- Den_py_CDE_transmision_rT_SE_
      n   <- length(cnt)
      ix  <- which(head(cnt, -2) == 1 &
                     cnt[-c(1, n)] == 2 &
                     tail(cnt, -2) == 3)[1]
      if (length(ix)) ix else NA_integer_
    },
    
    Comienzo_SE = if (!is.na(idx_rT)) paste(SE[idx_rT], "-", YEAR[idx_rT]) else NA_character_,
    Rt3         = sum(Den_py_CDE_transmision_rT_SE_ == 3, na.rm = TRUE),
    
    Positividad_30_SE = {
      idx <- which(Den_py_CDE_transmision_TasaPositividad_SE >= 1)
      if (length(idx) > 0) paste(SE[idx[1]], "-", YEAR[idx[1]]) else NA_character_
    },
    
    Positividad_max = max(positividad_lab_py_CDE, na.rm = TRUE),
    Comienzo_casos  = if (!is.na(idx_rT)) Den_py_CDE_Total_CP[idx_rT] else NA_real_,
    Comienzo_Rt     = if (!is.na(idx_rT)) SE_epi[idx_rT] else NA_real_,
    
    Velocidad = if (!is.na(Tp) & !is.na(Comienzo_Rt) & Tp != Comienzo_Rt)
      (Pico_N - Comienzo_casos) / (Tp - Comienzo_Rt)
    else NA_real_
  ) %>% ungroup() %>%
  mutate(ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1)) 
descriptores_CDE<- descriptores_CDE_2 %>%
  select(ANIO_EPI_LAB, Total, Incidencia, Comienzo_SE, Positividad_30_SE,
         Velocidad, Pico_N, Pico_SE, Positividad_max,
         Dc3, Dcmax, Ds3, Dsmax, Rtmax, Rt3, DcFin, DcFin_Conf)


# Guardar resultados----
write_csv(descriptores_IGR_2, "salida/articulo_4/Indicadores/Epi_features_Arg.csv")
write.csv(descriptores_IGU_2, "salida/articulo_4/Indicadores/Epi_features_BR.csv")
write.csv(descriptores_MG_2, "salida/articulo_4/Indicadores/Epi_features_PY_MG.csv")
write.csv(descriptores_HER_2, "salida/articulo_4/Indicadores/Epi_features_PY_HER.csv")
write.csv(descriptores_PF_2, "salida/articulo_4/Indicadores/Epi_features_PY_PF.csv")
write.csv(descriptores_CDE_2, "salida/articulo_4/Indicadores/Epi_features_PY_CDE.csv")

write_xlsx(
  list(
    IGR     = descriptores_IGR,
    AP      = descriptores_AP,
    IGU     = descriptores_IGU,
    PY_MG   = descriptores_MG,
    PY_HER  = descriptores_HER,
    PY_PF   = descriptores_PF,
    PY_CDE  = descriptores_CDE
  ),
  path = "salida/articulo_4/Indicadores/Epi_features_todos.xlsx"
)

write_xlsx(
  list(
    IGR     = descriptores_IGR_2,
    IGU     = descriptores_IGU_2,
    PY_MG   = descriptores_MG_2,
    PY_HER  = descriptores_HER_2,
    PY_PF   = descriptores_PF_2,
    PY_CDE  = descriptores_CDE_2
  ),
  path = "salida/articulo_4/Indicadores/Epi_features_todos_sucio.xlsx"
)

