
# Analisis de indicadores epidemiologicos
# Tesis de Doctorado - INMET/Fiocruz
# Autor: Esteban Manuel Couto

## Atributos de los sistemas de vigilancia----
# Librerias----
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2, tableone, 
  RColorBrewer, scales, gt, data.table, gridExtra, cowplot,
  tsibble
)

# Directorio de trabajo
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")

dengue_total<-read.csv2("source/csv/armo/dengue_total.csv")
dengue_ar_agrupados<-read.csv2("source/csv/armo/dengue_ar_agrupados.csv")

#Incluyo la base agrupados

ar_long <- dengue_ar_agrupados %>%
  pivot_longer(cols = c(menores, adultos, ancianos, sin_especificacion),
               names_to = "re",
               values_to = "casos") %>%
  mutate(
    re = if_else(re == "sin_especificacion", NA_character_, re)
  ) %>%
  filter(year_e > 2008, year_e < 2023) %>%
  mutate(
    re = as.factor(re),
    criterio = as.factor(criterio),
    classi_fin = as.factor(classi_fin),
    sistema = as.factor(sistema),
    pais = as.factor(pais),
    distrito = as.factor(distrito),
    tipo_base = as.factor(tipo_base)
  ) %>%
  select(year_e,year, se, re, casos, criterio, sistema,
         pais, distrito, classi_fin, tipo_base,mes) %>%
  uncount(weights = casos)


dengue_total <- bind_rows(
  dengue_total,
  ar_long
)


# 0) Base estandarizada para tsibble mensual por País x Municipio x Tipo x Clasificación ----
dengue0 <- dengue_total %>%
  # 1) Fechas a Date (solo las que vamos a usar más adelante)
  mutate(
    fecha       = as.Date(fecha),
    fis         = as.Date(fis),
    fecha_carga = as.Date(fecha_carga)
  ) %>%
  # 2) País a códigos (ar/br/py) para facet y labellers
  mutate(
    Pais = case_when(
      str_to_lower(pais) == "argentina" ~ "Argentina",
      str_to_lower(pais) == "brasil"    ~ "Brasil",
      str_to_lower(pais) == "paraguay"  ~ "Paraguay",
      TRUE                              ~ pais
    )
  ) %>%
  # 3) Municipio:
  #    - Para PY: mapear a CDE / HERNANDARIAS / MG / PF (para tus filtros).
  #    - Para BR/AR: conservar los nombres originales (con acentos).
  mutate(
    Municipio = case_when(
      distrito == "CIUDAD DEL ESTE"   ~ "Ciudad del Este",
      distrito == "HERNANDARIAS"      ~ "Hernandarias",
      distrito == "MINGA GUAZU"       ~ "Minga Guazú",
      distrito == "PRESIDENTE FRANCO" ~ "Pte. Franco",
      TRUE                                                ~ distrito
    )
  ) %>%
  # 4) Clasificación (CS/CC/OT) desde classi_fin cruda:
  #    confirmado -> CC; sospechoso -> CS; descartado/NA -> OT
  mutate(
    Clasificacion = case_when(
      classi_fin == "confirmado"  ~ "CC",
      classi_fin == "sospechoso"  ~ "CS",
      classi_fin == "descartado"  ~ "CD",
      TRUE                        ~ "CS"  # incluye NA
    )
  ) %>%
  # 5) Tipo para mantener tu convención de filtros
  mutate(Tipo = "Den") %>%
  # 6) Índice mensual (yearmonth) usando year/mes si están presentes; si faltan, usar fecha/fis
  mutate(
    mes  = suppressWarnings(as.integer(mes)),
    year = suppressWarnings(as.integer(year)),
    # fecha_ref mínima (sin reglas de atributos todavía): usar fis si existe; si no, fecha; si no, fecha_carga
    fecha_ref_bruta = coalesce(fis, fecha, fecha_carga),
    # índice mensual
    fecha_m = dplyr::coalesce(
      yearmonth(sprintf("%04d-%02d", year, mes)),
      yearmonth(floor_date(fecha_ref_bruta, "month"))
    )
  ) %>%
  # 7) Selección ordenada de columnas (las crudas + estandarizadas)
  select(
    # crudas
    X, fecha, fecha_carga, fis, fecha_lab, fecha_min, re, classi_fin, sexo,
    criterio, distrito, se, mes, year, year_e, sistema, pais, serotipo,
    test_validado, resul_validado, dato_labo, classi_validada,
    classi_validada_sosp, test, resultado, tipo_base, fecha_cierre,
    # nuevas/estándar
    Pais, Municipio, Clasificacion, Tipo, fecha_ref_bruta, fecha_m
  )
# ---- Tsibble mensual por País x Municipio x Tipo x Clasificación ----
# 1) Agregar a mensual y crear tsibble
tsibble_Base_Mensual_Final <- dengue0 %>%
  filter(!is.na(fecha_m)) %>%
  group_by(Pais, Municipio, Tipo, Clasificacion, fecha = fecha_m) %>%
  summarise(valor = n(), .groups = "drop") %>%
  as_tsibble(index = fecha, key = c(Pais, Municipio, Tipo, Clasificacion))

# 2) Chequeos 
tsibble_Base_Mensual_Final %>% dplyr::slice_head(n = 6)
tsibble_Base_Mensual_Final %>% dplyr::count(Pais, Clasificacion) %>% arrange(Pais)

# 3) Tsibble por país 
tsibble_por_pais <- tsibble_Base_Mensual_Final %>%
  index_by(fecha) %>%
  group_by(Pais, Tipo, Clasificacion) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup() %>%
  as_tsibble(index = fecha, key = c(Pais, Tipo, Clasificacion))
# ---- Calcular atributos por registro ----

# 0) Conversión de fechas y aseguramos tipos
dengue_attr_base <- dengue0 %>%  # si no tenés dengue0, usá dengue_total
  mutate(
    fecha        = as_date(fecha),
    fis          = as_date(fis),
    fecha_carga  = as_date(fecha_carga),
    fecha_lab    = as_date(fecha_lab),
    fecha_cierre = as_date(fecha_cierre),
    mes  = suppressWarnings(as.integer(mes)),
    year = suppressWarnings(as.integer(year))
  )

# 1) Atributos de oportunidad (en días)
dengue_attr_base <- dengue_attr_base %>%
  mutate(
    Retraso_Not  = as.numeric(difftime(fecha,       fis, units = "days")),  # FIS -> notificación
    Retraso_Lab  = as.numeric(difftime(fecha_lab,   fis, units = "days")),  # FIS -> laboratorio
    Temporalidad = as.numeric(difftime(fecha_cierre,fis, units = "days"))   # FIS -> cierre
  )

# 2) Completitud interna reducida (EN %) — REGLA NUEVA
#    6 variables base SIEMPRE + 1 variable (fecha_lab) SOLO si criterio == "laboratorio"
vars_base <- c("classi_fin","criterio","fis","sexo","re","fecha")  # (son 6)
var_lab   <- "fecha_lab"

dengue_attr_base <- dengue_attr_base %>%
  mutate(
    criterio = tolower(criterio),
    is_lab   = !is.na(criterio) & criterio == "laboratorio",
    
    # Numerador/denominador 6 variables base (siempre)
    num_base = rowSums(across(all_of(vars_base), ~ !is.na(.x) ), na.rm = TRUE),
    den_base = length(vars_base),
    
    # 7ma variable SOLO si is_lab
    num_lab  = if_else(is_lab & !is.na(.data[[var_lab]]) , 1L, 0L),
    den_lab  = if_else(is_lab, 1L, 0L),
    
    # Totales condicionales
    completos = num_base + num_lab,
    n_campos  = den_base + den_lab,
    
    Completitud_redu = if_else(n_campos > 0, 100 * completos / n_campos, NA_real_),
    
    Completitud_redu_cat = case_when(
      Completitud_redu > 90 ~ "excelente",
      Completitud_redu > 75 ~ "buena",
      Completitud_redu > 50 ~ "regular",
      is.finite(Completitud_redu) ~ "baja",
      TRUE ~ NA_character_
    ),
    
    # (opcionales de diagnóstico, dejo los nombres que ya usabas)
    Completitud_5vars = 100 * num_base / den_base,
    Completitud_7vars = if_else(is_lab, 100 * (num_base + num_lab) / (den_base + 1), NA_real_)
  )



# 4) Consistencia (reglas simples, como veníamos)
dengue_attr_base <- dengue_attr_base %>%
  mutate(
    Consistencia = case_when(
      !is.na(Retraso_Not) & Retraso_Not  < 0   ~ "No",
      !is.na(Retraso_Not) & Retraso_Not  > 10  ~ "No",
      criterio == "laboratorio" & !is.na(Retraso_Lab) & Retraso_Lab < 0 ~ "No",
      !is.na(Retraso_Lab) & Retraso_Lab >= 30  ~ "No",
      TRUE ~ "Sí"
    )
  )

# 5) Año y mes epidemiológico (misma lógica de tu script)
dengue_attr_base <- dengue_attr_base %>%
  mutate(
     MESe = case_when(
      !is.na(mes) & mes >= 10          ~ mes - 9,
      !is.na(mes) & mes >= 1 & mes <=9 ~ mes + 3,
      TRUE ~ NA_real_
    ),
    YEAR_E = factor(paste(year_e, year_e + 1, sep = "-"),
                         levels = paste(2008:2023, 2009:2024, sep = "-"))
  )



# 6) Quito outliers
dengue_attr_base <- dengue_attr_base %>%
  mutate(
    Retraso_Not_outlier = !is.na(Retraso_Not) & (Retraso_Not < 0 | Retraso_Not > 10),
    Retraso_Lab_outlier = !is.na(Retraso_Lab) & (Retraso_Lab < 0 | Retraso_Lab >= 30),
    Temporalidad_outlier = !is.na(Temporalidad) & (as.numeric(Temporalidad) < 0 | as.numeric(Temporalidad) >= 180),
    Retraso_Not_cln = ifelse(Retraso_Not_outlier, NA_real_, Retraso_Not),
    Retraso_Lab_cln = ifelse(Retraso_Lab_outlier, NA_real_, Retraso_Lab),
    Temporalidad_cln = ifelse(Temporalidad_outlier, NA_real_, Temporalidad)
  ) 


# 7) Salidas finales
Den_Calidad_Todos <- dengue_attr_base
Den_Calidad_CP    <- dengue_attr_base %>% filter(Clasificacion != "CD")  # Confirmados + Sospechosos



# ---- Calculo agrupados por año ----

# Resumen por año epidemiológico × municipio
resumen_indicadores <- Den_Calidad_Todos %>%
  group_by(YEAR_E, Municipio) %>%
  summarise(
    n = n(),
    # Consistencia: % de registros con "Sí"
    pct_consistencia = mean(Consistencia == "Sí", na.rm = TRUE) * 100,
    # VPP simple: confirmados / total
    confirmados = sum(Clasificacion == "CC", na.rm = TRUE),
    vpp_pct = (confirmados / n) * 100,
    # Positividad de laboratorio: positivos / (positivos + negativos)
    lab_pos = sum(resul_validado == "positivo", na.rm = TRUE),
    lab_neg = sum(resul_validado == "negativo", na.rm = TRUE),
    pos_testeados = lab_pos + lab_neg,
    positividad_lab = dplyr::if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(Municipio, YEAR_E) %>%
  mutate(
    across(c(pct_consistencia, vpp_pct, positividad_lab), ~round(., 1))
  )


# ---- Calculo agrupados por mes y periodos ----

base_periodos <- Den_Calidad_Todos %>%
  mutate(
    date_m = as.Date(paste(year, mes, "01", sep = "-")),
    Periodo = case_when(
      date_m >= as.Date("2020-03-01") & date_m <= as.Date("2021-09-30") ~ "COVID",
      date_m >= as.Date("2022-10-01") & date_m <= as.Date("2023-06-30") ~ "CHIKV",
      date_m >  as.Date("2021-09-30") & date_m <  as.Date("2022-10-01") ~ "INTER",
      TRUE ~ "PRE"
    )
  )
#Mes epidemiologico -------
#Triple Frontera#
meses <- Den_Calidad_Todos [(Den_Calidad_Todos$criterio == "laboratorio"), ]%>%
  group_by(year_e, YEAR_E, MESe, mes, classi_fin) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(year_e, YEAR_E, MESe, mes) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() %>%
  filter(as.numeric(year_e >2008) & as.numeric(year_e <=2022)) %>%
  mutate(MES_Epi = case_when(
    (classi_fin == "confirmado"  & proportion > 0.2) ~ "1",
    (classi_fin == "confirmado" & proportion <= 0.2) ~ "0"
  )) 

meses_lag_lead <- meses [!is.na(meses$MES_Epi),] %>% 
  mutate(prev_epi = lag(MES_Epi),
         next_epi = lead(MES_Epi)) %>%
  filter(MES_Epi == "1" & (prev_epi == "1" | next_epi == "1")) 

print(table(meses_lag_lead$mes, meses_lag_lead$MESe))

# Agrupar los datos ya filtrados para contar cuántos meses "epidémicos" hay por MES y por YEAR

# Asegurarse de que MES es factor en orden correcto
meses_lag_lead$MESe <- factor(meses_lag_lead$MESe,
                              levels = 1:12,
                              labels = c("Oct", "Nov", "Dic", "Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                         "Jul","Ago", "Sep"))
# Crear base para el heatmap: 1 fila por YEAR y MES
heatmap_data <- meses_lag_lead %>%
  filter(!is.na(MES_Epi)) %>%
  select(year_e,YEAR_E, MESe, MES_Epi, mes) %>%
  distinct()

# --- Resumen por periodo x año epidemiológico x municipio ------

base_periodos$MESe <- factor(base_periodos$MESe,
                              levels = 1:12,
                              labels = c("Oct", "Nov", "Dic", "Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                         "Jul","Ago", "Sep"))

base_periodos <- base_periodos %>% 
  left_join(heatmap_data, by = c( "MESe","YEAR_E","year_e","mes")) 

resumen_periodo <- base_periodos %>%
  group_by(Periodo, Municipio,pais, year_e, YEAR_E,MESe, mes, MES_Epi) %>%
  summarise(
    notificados = n(),
    casos_probables = sum (Clasificacion %in% c("CC", "CS")),
    mean_Retraso_Not = mean(Retraso_Not_cln, na.rm = TRUE),
    med_Retraso_Lab  = mean(Retraso_Lab_cln, na.rm = TRUE),
    mean_Temporalidad= mean(Temporalidad_cln, na.rm = TRUE),
    pct_consistencia = mean(Consistencia == "Sí", na.rm = TRUE) * 100,
    confirmados = sum(Clasificacion == "CC", na.rm = TRUE),
    pct_completitud = mean (Completitud_redu, na.rm = TRUE),
    vpp_pct     = (confirmados / notificados) * 100,
    lab_pos          = "Lab (+)",
      .groups = "drop_last"
  ) %>%
  rowwise() %>%
  ungroup() %>%
  select(-confirmados) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  arrange(pais, Municipio, Periodo, YEAR_E, MESe, mes)




write.csv2(resumen_periodo, "salida/articulo_2/resumen_periodo_epi_muni.csv", row.names = FALSE)


###Calculo VPP y TP por periodo
resumen_indicadores_municipio<- Den_Calidad_Todos %>%
  group_by( Municipio, mes, YEAR_E) %>%
  summarise(
    n = n(),    ,
    # VPP simple: confirmados / total
    confirmados = sum(Clasificacion == "CC", na.rm = TRUE),
    vpp_pct = (confirmados / n) * 100,
    # Positividad de laboratorio: positivos / (positivos + negativos)
    lab_pos = sum(resul_validado == "positivo", na.rm = TRUE),
    lab_neg = sum(resul_validado == "negativo", na.rm = TRUE),
    pos_testeados = lab_pos + lab_neg,
    positividad_lab = dplyr::if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(Municipio,YEAR_E, mes) %>%
  mutate(
    across(c( vpp_pct, positividad_lab), ~round(., 2))
  )

write.csv(resumen_indicadores_municipio, "salida/articulo_2/resumen_vpp_muni.csv", row.names = FALSE)

write.csv(resumen_indicadores, "salida/articulo_2/resumen_consistencia_muni2.csv", row.names = FALSE)

resumen_indicadores_municipio_periodos<- base_periodos %>%
  group_by( Municipio, mes, YEAR_E, Periodo, MESe, MES_Epi) %>%
  summarise(
    n = n(),    ,
    # VPP simple: confirmados / total
    confirmados = sum(Clasificacion == "CC", na.rm = TRUE),
    vpp_pct = (confirmados / n) * 100,
    # Positividad de laboratorio: positivos / (positivos + negativos)
    lab_pos = sum(resul_validado == "positivo", na.rm = TRUE),
    lab_neg = sum(resul_validado == "negativo", na.rm = TRUE),
    pos_testeados = lab_pos + lab_neg,
    positividad_lab = dplyr::if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(Municipio,YEAR_E, mes) %>%
  mutate(
    across(c( vpp_pct, positividad_lab), ~round(., 2))
  )


#Hago tsibble por variables VPP y TP
resumen_ts <- resumen_indicadores_municipio %>%
  # Si YEAR_E es algo tipo "2009-2010", me quedo con el primer año
  mutate(
    year_num = as.integer(str_sub(as.character(YEAR_E), 1, 4)),
    ym = yearmonth(make_date(year = year_num, month = mes, day = 1))
  ) %>%
  as_tsibble(
    index = ym,
    key   = Municipio
  )

