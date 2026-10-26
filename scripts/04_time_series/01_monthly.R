####Uno todas las bases de datos###
# Ejecutar desde la raíz del proyecto / Run from the project root.
#Paquetes
pacman::p_load(
  fpp3
)
#Cargo bases ----
dengue_ar_agrupados<-read.csv2("source/csv/armo/dengue_ar_agrupados.csv")
dengue_ar_total<-read.csv2("source/csv/armo/dengue_ar_total.csv")
den_py_total<-read.csv2("source/csv/armo/dengue_py_total.csv")
den_br_total<-read.csv2("source/csv/armo/dengue_br_total.csv")
dengue_total<-read.csv2("source/csv/armo/dengue_total.csv")


#Acomodo bases----
#Filtro por caso probable
dengue_ar_total<-dengue_ar_total%>%
  mutate(
    across(c(fecha,  fis, fecha_lab), as.Date),
    across(where(is.character), as.factor)
  ) %>% 
  filter(year_e > 2008, year_e < 2023)  %>%
  group_by( year_e,year, mes, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
  summarise(
    casos = n(),
    .groups = "drop"
  )


den_py_total <- den_py_total %>%
  mutate(
    across(c(fecha,  fis, fecha_lab), as.Date),
    across(where(is.character), as.factor) ) %>% 
  filter(year_e > 2008, year_e < 2023) %>% 
  select(-c(centro, noti_distrito, gravedad, test_pcr, test_ns1, test_igm)) %>%
  group_by( year_e,year, mes, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
  summarise(
    casos = n(),
    .groups = "drop"
  ) 

den_br_total <- den_br_total %>%
  mutate(
    across(c(fecha,  fis, fecha_lab), as.Date),
    across(where(is.character), as.factor)
  )  %>%
  filter(year_e > 2008, year_e < 2023) %>% select(-c(  gravedad  )) %>%
  group_by( year_e,year, mes, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
  summarise(
    casos = n(),
    .groups = "drop"
  ) 

# Agrego la base de agrupados

ar_long <- dengue_ar_agrupados %>%
  pivot_longer(cols = c(menores, adultos, ancianos, sin_especificacion),
               names_to = "re",
               values_to = "casos") %>%
  mutate(
    re = if_else(re == "sin_especificacion", NA_character_, re)
  ) %>%
  select(year_e,year, mes, re, casos, criterio, sistema, pais, distrito, classi_fin, tipo_base)


dengue_completo_ar <- bind_rows(
  dengue_ar_total,
  ar_long
)

# Función Cálculos mensuales básicos con estratos opcionales----
calc_mensual <- function(data, by = NULL) {
  # Requisitos mínimos
  req <- c("sistema", "pais", "distrito", "year", "mes", "classi_fin")
  falta <- setdiff(req, names(data))
  if (length(falta) > 0) {
    stop("Faltan columnas requeridas: ", paste(falta, collapse = ", "))
  }
  
  # Completar columnas faltantes de apoyo
  if (!"casos" %in% names(data)) data$casos <- 1L
  if (!"resul_validado" %in% names(data)) data$resul_validado <- NA_character_
  if (!"ym" %in% names(data)) {
    data <- mutate(
      data,
      year = as.integer(year),
      mes  = as.integer(mes),
      ym   = tsibble::yearmonth(lubridate::make_date(year, mes, 1))
    )
  }
  
  # Estratos opcionales: solo los que realmente existen en 'data'
  by <- intersect(if (is.null(by)) character(0) else by, names(data))
  
  claves <- c("sistema", "pais", "distrito", "year", "mes", "ym", by)
  
  data %>%
    group_by(across(all_of(claves))) %>%
    summarise(
      total_cp = sum(casos[classi_fin != "descartado"], na.rm = TRUE),
      total_cc = sum(casos[classi_fin == "confirmado"], na.rm = TRUE),
      total_casos = sum(casos, na.rm = TRUE),
      lab_pos  = sum(casos[resul_validado == "positivo"], na.rm = TRUE),
      lab_neg  = sum(casos[resul_validado == "negativo"], na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(
      testeados = lab_pos + lab_neg,
      positividad   = ifelse(testeados > 0, 100 * lab_pos / testeados, NA_real_),
      prop_conf = ifelse(total_casos > 0, 100 * total_cc / total_casos, NA_real_
    )) %>%
    arrange(pais, distrito, year, mes, across(all_of(by)))
}

#Bases mensuales----------
out_total_ar <- calc_mensual(dengue_completo_ar)
out_total_br <- calc_mensual(den_br_total)
out_total_py <- calc_mensual(den_py_total)

dengue_mes_total <- bind_rows(
  out_total_ar,
  out_total_br, 
  out_total_py)

# Por sexo y rango etario
out_sexo_re_ar <- calc_mensual(dengue_completo_ar, by = c("sexo", "re"))
out_sexo_re_br <- calc_mensual(den_br_total, by = c("sexo", "re"))
out_sexo_re_py <- calc_mensual(den_py_total, by = c("sexo", "re"))
dengue_mes_total_sexo_re <- bind_rows(
  out_sexo_re_ar,
  out_sexo_re_br,
  out_sexo_re_py
)

# Por criterio, classi_fin, serotipo y test
out_multi_ar <- calc_mensual(
  dengue_completo_ar,
  by = c("criterio", "classi_fin", "serotipo", "test")
)
out_multi_br <- calc_mensual(
  den_br_total,
  by = c("criterio", "classi_fin", "serotipo", "test")
)
out_multi_py <- calc_mensual(
  den_py_total,
  by = c("criterio", "classi_fin", "serotipo", "test")
)

dengue_mes_total_multi <- bind_rows(
  out_multi_ar,
  out_multi_br,
  out_multi_py
)
# Guardo resultados ----
write.csv2(dengue_mes_total, "source/csv/dengue_mes_art_4.csv", row.names = FALSE)
write.csv2(dengue_mes_total_sexo_re, "source/csv/dengue_mes_sexo_re_art_4.csv", row.names = FALSE)
write.csv2(dengue_mes_total_multi, "source/csv/dengue_total_multi_art_4.csv", row.names = FALSE)





#Hago Tsibble
dengue_ts <- dengue_mes_total %>%
   as_tsibble(
   key = c(sistema, pais, distrito),
    index = ym  ) %>%
  fill_gaps() %>% replace_na(list(total_cp = 0))  %>% 
  filter (ym >= yearmonth("2009 oct.")) %>% select(-c(year, mes))

---------------------------------------------------------------------------
  #Analisis de series temporales
-----------------------------------------------
  
  ##Features TF
 
media_casos <-dengue_ts %>% 
  group_by(distrito, pais)%>% 
  summarise(total_cp= sum (total_cp)) %>%
  features(total_cp, features = list(mean = mean, sd = sd)) %>% arrange (desc(mean))

write.csv(media_casos, "salida/articulo_4/series_temporales/media_casos_distrito_art4.csv", row.names = FALSE)

feat_stl <- dengue_ts %>% 
  group_by(distrito, pais)%>% 
  summarise(total_cp= sum (total_cp)) %>%
  features(total_cp, feat_stl)  

feat_todos <- dengue_ts %>% 
  group_by(distrito, pais)%>% 
  summarise(total_cp= sum (total_cp)) %>%
  features(total_cp,feature_set(pkgs = "feasts"))

write.csv2(media_casos, "salida/articulo_4/series_temporales/feat_stl_distrito_art4.csv", row.names = FALSE)
write.csv2(media_casos, "salida/articulo_4/series_temporales/feat_todos_distrito_art4.csv", row.names = FALSE)

  
########################
#Hacer graficos.
#STL - ACF - PACF
#Series de todas las variables
#Hacer serie de chikungunya