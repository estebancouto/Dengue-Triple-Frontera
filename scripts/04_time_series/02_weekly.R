####Uno todas las bases de datos###
# Ejecutar desde la raíz del proyecto / Run from the project root.
#Paquetes
pacman::p_load(
  fpp3, colorspace
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
  mutate(YEAR_SE = if_else(se == 1 & mes == 12, year + 1, year)) %>% 
  group_by( year_e,YEAR_SE, year, se, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
  summarise(
    casos = n(),
    .groups = "drop"
  )


den_py_total <- den_py_total %>%
  mutate(
    across(c(fecha,  fis, fecha_lab), as.Date),
    across(where(is.character), as.factor) ) %>% 
  filter(year_e > 2008, year_e < 2023) %>% 
  mutate(YEAR_SE = if_else(se == 1 & mes == 12, year + 1, year)) %>% 
  select(-c(centro, noti_distrito, gravedad, test_pcr, test_ns1, test_igm)) %>%
  group_by( year_e,YEAR_SE, se, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
  summarise(
    casos = n(),
    .groups = "drop"
  ) %>% mutate(distrito = case_when(
    distrito == "CIUDAD DEL ESTE" ~ "Cdad. del Este",
    distrito == "HERNANDARIAS" ~ "Hernandarias",
    distrito == "MINGA GUAZU" ~ "Minga Guazú",
    distrito == "PRESIDENTE FRANCO" ~ "Pte. Franco"
  ))

den_br_total <- den_br_total %>%
  mutate(
    across(c(fecha,  fis, fecha_lab), as.Date),
    across(where(is.character), as.factor)
  )  %>%
  filter(year_e > 2008, year_e < 2023) %>% select(-c(  gravedad  )) %>%
  mutate(YEAR_SE = if_else(se == 1 & mes == 12, year + 1, year)) %>% 
  group_by( year_e,YEAR_SE, se, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
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
    re = if_else(re == "sin_especificacion", NA_character_, re),
    YEAR_SE = if_else(se == 1 & mes == 12, year + 1, year))%>% 
  select(year_e,year,YEAR_SE, se, re, casos, criterio, sistema, pais, distrito, classi_fin, tipo_base)


dengue_completo_ar <- bind_rows(
  dengue_ar_total,
  ar_long
)


# Función Cálculos semanal básicos con estratos opcionales----
calc_semanal <- function(data, by = NULL) {
  # Requisitos mínimos
  req <- c( "pais", "distrito", "YEAR_SE", "se", "classi_fin")
  falta <- setdiff(req, names(data))
  if (length(falta) > 0) {
    stop("Faltan columnas requeridas: ", paste(falta, collapse = ", "))
  }
  
  # Completar columnas de apoyo
  if (!"casos" %in% names(data)) data$casos <- 1L
  if (!"resul_validado" %in% names(data)) data$resul_validado <- NA_character_
  
  # Eliminar semana 53
  data <- data %>%
    dplyr::mutate(
      YEAR_SE = as.integer(YEAR_SE),
      se   = as.integer(se)
    ) %>%
    dplyr::filter(is.na(se) | se <= 52)
  # si querés saber cuántas filas descartás, antes podés hacer:
  # dplyr::count(se) para revisar
  
  # Índice semanal con yearweek (ISO, ya sin 53)
  if (!"yw" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      yw = tsibble::yearweek(sprintf("%d W%02d", YEAR_SE, se))
    )
  }
  
  # Estratos opcionales
  by <- intersect(if (is.null(by)) character(0) else by, names(data))
  claves <- c("pais", "distrito", "YEAR_SE", "se", "yw", by)
  
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(
      total_cp    = sum(casos[classi_fin != "descartado"], na.rm = TRUE),
      total_cc    = sum(casos[classi_fin == "confirmado"], na.rm = TRUE),
      total_casos = sum(casos, na.rm = TRUE),
      lab_pos     = sum(casos[resul_validado == "positivo"], na.rm = TRUE),
      lab_neg     = sum(casos[resul_validado == "negativo"], na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(
      testeados   = lab_pos + lab_neg,
      positividad = dplyr::if_else(testeados > 0,
                                   100 * lab_pos / testeados,
                                   NA_real_),
      prop_conf   = dplyr::if_else(total_casos > 0,
                                   100 * total_cc / total_casos,
                                   NA_real_)
    ) %>%
    dplyr::arrange(pais, distrito, YEAR_SE, se,
                   dplyr::across(dplyr::all_of(by)))
}


#Bases semanal----------
out_total_ar <- calc_semanal(dengue_completo_ar) 
out_total_br <- calc_semanal(den_br_total)
out_total_py <- calc_semanal(den_py_total)

dengue_se_total <- bind_rows(
  out_total_ar,
  out_total_br, 
  out_total_py)

# Por sexo y rango etario
out_sexo_re_ar <- calc_semanal(dengue_completo_ar, by = c("sexo", "re"))
out_sexo_re_br <- calc_semanal(den_br_total, by = c("sexo", "re"))
out_sexo_re_py <- calc_semanal(den_py_total, by = c("sexo", "re"))
dengue_se_total_sexo_re <- bind_rows(
  out_sexo_re_ar,
  out_sexo_re_br,
  out_sexo_re_py
)

# Por criterio, classi_fin, serotipo y test
out_multi_ar <- calc_semanal(
  dengue_completo_ar,
  by = c("criterio", "classi_fin", "serotipo", "test")
)
out_multi_br <- calc_semanal(
  den_br_total,
  by = c("criterio", "classi_fin", "serotipo", "test")
)
out_multi_py <- calc_semanal(
  den_py_total,
  by = c("criterio", "classi_fin", "serotipo", "test")
)

dengue_se_total_multi <- bind_rows(
  out_multi_ar,
  out_multi_br,
  out_multi_py
)
# Guardo resultados ----
write.csv2(dengue_se_total, "source/csv/dengue_se_art_4.csv", row.names = FALSE)
write.csv2(dengue_se_total_sexo_re, "source/csv/dengue_se_sexo_re_art_4.csv", row.names = FALSE)
write.csv2(dengue_se_total_multi, "source/csv/dengue_se_total_multi_art_4.csv", row.names = FALSE)


 


#Hago Tsibble
ini <- yearweek("2009 W40")
fin <- yearweek("2023 W39")
idx <- seq(ini, fin, by = 1)
idx <- idx[week(idx) != 53]   # drop W53

# 1) Claves esperadas (todos los distritos)
keys <- dengue_se_total %>% distinct(pais, distrito)

dengue_ts <- expand_grid(keys, yw = idx) %>%
  left_join(dengue_se_total, by = c("pais","distrito","yw")) %>% 
  mutate (yw=yearweek(yw)) %>% 
   as_tsibble(
   key = c(pais, distrito),
    index = yw  ) %>%
  fill_gaps() %>% replace_na(list(total_cp = 0))  %>% 
  filter (yw >= yearweek("2009 W40"), !is.na(pais)) %>% select(-c(YEAR_SE, se)) 

---------------------------------------------------------------------------
#Analisis de series temporales-------------
### Casos crudos###  
##Features TF
 
media_casos <-dengue_ts  %>%
  features(total_cp, features = list(mean = mean, sd = sd)) %>% arrange (desc(mean))

write.csv(media_casos, "salida/articulo_4/series_temporales/media_casos_distrito_art4_se.csv", row.names = FALSE)

feat_stl_de2 <- dengue_ts %>% 
  features(total_cp, feat_stl, s.window = 51, t.window=103, robust = TRUE)  

feat_todos <- dengue_ts %>% 
  features(total_cp,feature_set(pkgs = "feasts"))

write.csv(feat_stl_de2, "salida/articulo_4/series_temporales/feat_stl_distrito_art4_se.csv", row.names = FALSE)
write.csv(feat_todos, "salida/articulo_4/series_temporales/feat_todos_distrito_art4_se.csv", row.names = FALSE)


tendencia_estacionalidad<-dengue_ts %>% 
  features(total_cp, feat_stl, s.window = 51, t.window=103, robust = TRUE)  %>%
  ggplot(aes(x = trend_strength,
             y = seasonal_strength_year,
             col = distrito)) +
  geom_point(size = 3) +          # puntos más grandes
  ggtitle("Relación entre tendencia y estacionalidad. Triple Frontera") +
  theme_minimal()
tendencia_estacionalidad
ggsave("salida/articulo_4/series_temporales/tendencia_estacionalidad.png", tendencia_estacionalidad, width = 10, height = 6)

#ACF de triple frontera ventana de ciclo anual----

acf_tf<-dengue_ts%>% 
  index_by(yw) %>% 
  summarise(total_cp= sum (total_cp)) %>% 
  ACF(total_cp, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera. 2009-2025")
acf_tf
ggsave("salida/articulo_4/series_temporales/Autocorrelacion_se_tf.png", acf_tf, width = 10, height = 6)

###Lag----

for (d in unique(dengue_ts$distrito)) {
  
  p <- dengue_ts |>
    filter(distrito == d) |>
    gg_lag(total_cp, geom = "point") +
    labs(
      title = paste("Lag plot de casos de dengue -", d),
      x = "lag(casos de dengue, k)"
    )
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/lag/lagplot_total_cp_", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}


#ACF de triple frontera ventana de ciclo anual----

ACF_Todos <-dengue_ts%>% 
  ACF(total_cp, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera")

ggsave(
  filename = paste0("salida/articulo_4/series_temporales/Autocorrelacion_crudo.png"),
  plot     = ACF_Todos,
  width    = 8,
  height   = 6,
  dpi      = 300)

for (d in unique(dengue_ts$distrito)) {
  
  p <- dengue_ts |>
    filter(distrito == d) |>
    ACF(total_cp, lag_max = 160)%>%
    autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera")+
    labs(
      title = paste("Autocorrelacion -", d),
      x = "semanas"
    )
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/Autocorrelacion", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}

#PACF de triple frontera ventana de ciclo anual----

dengue_ts%>% 
  PACF(total_cp, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera")

for (d in unique(dengue_ts$distrito)) {
  
  p <- dengue_ts |>
    filter(distrito == d) |>
    PACF(total_cp, lag_max = 160)%>%
    autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera")+
    labs(
      title = paste("Autocorrelacion -", d),
      x = "semanas"
    )
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/Autocorrelacion", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}

### Descomposición por distrito-----------

#Ventana ciclo 103, estacionalidad 51 semanas (bianual)----

for (d in unique(dengue_ts$distrito)) {
  
  ajuste_stl_tf_1 <- dengue_ts |>
    filter(distrito == d) |>
    model(STL(
      total_cp ~ trend(window = 103) +
        season(window = 51),
      robust = T)
    ) 
  
  p<-autoplot(ajuste_stl_tf_1 %>%
             components()) +
    labs(title = paste ("Descomposición STL de casos probables de dengue - 
       Ciclo bianual-",d ),
         y = "Número de casos probables") +
    theme_minimal() 
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/STL_bianual", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}


#Ventana ciclo 51, estacionalidad 51 semanas (anual)----

for (d in unique(dengue_ts$distrito)) {
  
  ajuste_stl_tf_2 <- dengue_ts |>
    filter(distrito == d) |>
    model(STL(
      total_cp ~ trend(window = 51) +
        season(window = 51),
      robust = T)
    ) 
  
  p<-autoplot(ajuste_stl_tf_2 %>%
                components()) +
    labs(title = paste ("Descomposición STL de casos probables de dengue - 
       Ciclo anual-",d ),
         y = "Número de casos probables") +
    theme_minimal() 
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/STL_anual", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}


### Grafico comparando las tendencias----
ajuste_stl_distritos <- dengue_ts |>
  model(
    STL(
      total_cp ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )


comp_distritos <- components(ajuste_stl_distritos) |>
  select(distrito, index = !!index_var(dengue_ts), trend)



## 2) Serie agregada (suma de todos los distritos) y su STL
dengue_ts_total <- dengue_ts |>
  index_by(index = yw) |>
  summarise(total_cp = sum(total_cp, na.rm = TRUE))

ajuste_stl_total <- dengue_ts_total |>
  model(
    STL(
      total_cp ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, trend)

## 3) Unir tendencias distrito + total
tendencias <- bind_rows(comp_distritos, comp_total)  %>%
  mutate(
    distrito = case_when(
      distrito == "Puerto Iguazú"     ~ "Puerto Iguazú (Arg.)",
      distrito == "Foz do Iguaçu"     ~ "Foz do Iguaçu (Bra.)",
      distrito == "Ciudad del Este"   ~ "Cdad. del Este (Par.)",
      distrito == "Cdad. del Este"    ~ "Cdad. del Este (Par.)",
      distrito == "Hernandarias"      ~ "Hernandarias (Par.)",
      distrito == "Minga Guazú"       ~ "Minga Guazú (Par.)",
      distrito == "Presidente Franco" ~ "Pte. Franco (Par.)",
      distrito == "Pte. Franco"       ~ "Pte. Franco (Par.)",
      distrito == "Total"              ~ "Triple Frontera (Total)",
      TRUE ~ distrito
    )
  ) %>%
  mutate(distrito = factor(distrito, levels = c(
    "Puerto Iguazú (Arg.)",
    "Foz do Iguaçu (Bra.)",
    "Cdad. del Este (Par.)",
    "Hernandarias (Par.)",
    "Minga Guazú (Par.)",
    "Pte. Franco (Par.)", 
    "Triple Frontera (Total)"
  )))

## 4) Gráfico único: todas las tendencias + agregada
p <- tendencias |>
  ggplot(aes(x = index, y = trend)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Tendencias (bianual) STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Tendencia (casos probables)"
  ) +
  theme_minimal()

print(p)

ggsave(
  filename = "salida/articulo_4/series_temporales/STL_tendencias_distritos_y_total.png",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300
)

##Anual
ajuste_stl_distritos <- dengue_ts |>
  model(
    STL(
      total_cp ~ trend(window = 51) +
        season(window = 51),
      robust = TRUE
    )
  )


comp_distritos <- components(ajuste_stl_distritos) |>
  select(distrito, index = !!index_var(dengue_ts), trend)
## 2) Serie agregada (suma de todos los distritos) y su STL
dengue_ts_total <- dengue_ts |>
  index_by(index = yw) |>
  summarise(total_cp = sum(total_cp, na.rm = TRUE))

ajuste_stl_total <- dengue_ts_total |>
  model(
    STL(
      total_cp ~ trend(window = 51) +
        season(window = 51),
      robust = TRUE
    )
  )

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, trend)

## 3) Unir tendencias distrito + total
tendencias <- bind_rows(comp_distritos, comp_total)

## 4) Gráfico único: todas las tendencias + agregada
p <- tendencias |>
  ggplot(aes(x = index, y = trend)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Tendencias (anual) STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Tendencia (casos probables)"
  ) +
  theme_minimal()

print(p)

ggsave(
  filename = "salida/articulo_4/series_temporales/STL_tendencias_distritos_y_total_W51.png",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300
)

  
### Grafico comparando la estacionalidad----
ajuste_stl_distritos <- dengue_ts |>
  model(
    STL(
      total_cp ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )


comp_distritos <- components(ajuste_stl_distritos) |>
  select(distrito, index = yw, season_year)

## 2) Serie agregada (suma de todos los distritos) y su STL
dengue_ts_total <- dengue_ts |>
  index_by(index = yw) |>
  summarise(total_cp = sum(total_cp, na.rm = TRUE))

ajuste_stl_total <- dengue_ts_total |>
  model(
    STL(
      total_cp ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, season_year)

## 3) Unir tendencias distrito + total
estacionalidad <- bind_rows(comp_distritos, comp_total)  %>%
  mutate(
    distrito = case_when(
      distrito == "Puerto Iguazú"     ~ "Puerto Iguazú (Arg.)",
      distrito == "Foz do Iguaçu"     ~ "Foz do Iguaçu (Bra.)",
      distrito == "Ciudad del Este"   ~ "Cdad. del Este (Par.)",
      distrito == "Cdad. del Este"    ~ "Cdad. del Este (Par.)",
      distrito == "Hernandarias"      ~ "Hernandarias (Par.)",
      distrito == "Minga Guazú"       ~ "Minga Guazú (Par.)",
      distrito == "Presidente Franco" ~ "Pte. Franco (Par.)",
      distrito == "Pte. Franco"       ~ "Pte. Franco (Par.)",
      distrito == "Total"              ~ "Triple Frontera (Total)",
      TRUE ~ distrito
    )
  ) %>%
  mutate(distrito = factor(distrito, levels = c(
    "Puerto Iguazú (Arg.)",
    "Foz do Iguaçu (Bra.)",
    "Cdad. del Este (Par.)",
    "Hernandarias (Par.)",
    "Minga Guazú (Par.)",
    "Pte. Franco (Par.)", 
    "Triple Frontera (Total)"
  )))
## 4) Gráfico único: todas las tendencias + agregada
p <- estacionalidad |>
  ggplot(aes(x = index, y = season_year)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Estacionalidad STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Tendencia (casos probables)"
  ) +
  theme_minimal()

print(p)

ggsave(
  filename = "salida/articulo_4/series_temporales/STL_estacionalidad_distritos_y_total.png",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300
)

#### Grafico de residuos----
comp_distritos <- components(ajuste_stl_distritos) |>
  select(distrito, index = yw, remainder)

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, remainder)

## Unir residuos distrito + total
residuos <- bind_rows(comp_distritos, comp_total)  %>%
  mutate(
    distrito = case_when(
      distrito == "Puerto Iguazú"     ~ "Puerto Iguazú (Arg.)",
      distrito == "Foz do Iguaçu"     ~ "Foz do Iguaçu (Bra.)",
      distrito == "Ciudad del Este"   ~ "Cdad. del Este (Par.)",
      distrito == "Cdad. del Este"    ~ "Cdad. del Este (Par.)",
      distrito == "Hernandarias"      ~ "Hernandarias (Par.)",
      distrito == "Minga Guazú"       ~ "Minga Guazú (Par.)",
      distrito == "Presidente Franco" ~ "Pte. Franco (Par.)",
      distrito == "Pte. Franco"       ~ "Pte. Franco (Par.)",
      distrito == "Total"              ~ "Triple Frontera (Total)",
      TRUE ~ distrito
    )
  ) %>%
  mutate(distrito = factor(distrito, levels = c(
    "Puerto Iguazú (Arg.)",
    "Foz do Iguaçu (Bra.)",
    "Cdad. del Este (Par.)",
    "Hernandarias (Par.)",
    "Minga Guazú (Par.)",
    "Pte. Franco (Par.)", 
    "Triple Frontera (Total)"
  )))
## 4) Gráfico único: todas las tendencias + agregada
p2 <- residuos |>
  ggplot(aes(x = index, y = remainder)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Componente residual STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Tendencia (casos probables)"
  ) +
  theme_minimal()

print(p2)

ggsave(
  filename = "salida/articulo_4/series_temporales/STL_residuos_distritos_y_total.png",
  plot     = p2,
  width    = 10,
  height   = 6,
  dpi      = 300
)


###Hago validación de residuos####  

##Modelo 1
residuos_stl1 <- ajuste_stl_distritos %>%
  components() %>%
  select(distrito, remainder)

residuos_stl1 %>% ACF(remainder, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación residuos.Modelo 1. Dengue en la triple frontera")

residuos_stl1 %>% 
features(remainder, ljung_box, lag = 160, dof = 0) ### Autocorrelación significativa

#Modelo 2
ajuste_stl_distritos_2 <- dengue_ts |>
  model(
    STL(
      total_cp ~ trend(window = 51) +
        season(window = 51),
      robust = TRUE
    )
  )

residuos_stl2 <- ajuste_stl_distritos_2 %>%
  components() %>%
  select(distrito, remainder)

residuos_stl2 %>% ACF(remainder, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación residuos.Modelo 2. Dengue en la triple frontera")

residuos_stl2 %>% 
  features(remainder, ljung_box, lag = 160, dof = 0) ### Autocorrelación significativa



#############################################
#Casos logaritmicos----
#Calculo el Log con 0

dengue_ts_0 <- dengue_ts %>% filter(total_cp != 0) %>% mutate(total_cp_log = log(total_cp)) %>% fill_gaps(total_cp_log= 0L)

##Modelo Log1
ajuste_stl_distritos_log1 <- dengue_ts_0 |>
  model(
    STL(
      total_cp_log ~ trend(window = 103) +
        season(window = "periodic"),
      robust = TRUE
    )
  )

residuos_stl_log <- ajuste_stl_distritos_log1 %>%
  components() %>%
  select(distrito, remainder)

residuos_stl_log %>% ACF(remainder, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación residuos. Modelo Log_1. Dengue en la triple frontera")

residuos_stl_log %>% 
  features(remainder, ljung_box, lag = 103, dof = 0) ### Autocorrelación significativa

#Modelo Log2
ajuste_stl_distritos_log2 <- dengue_ts_0 |>
  model(
    STL(
      total_cp_log ~ trend(window = 51) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) 
residuos_stl_log2 <- ajuste_stl_distritos_log2 %>%
  components() %>%
  select(distrito, remainder)

residuos_stl_log2 %>% ACF(remainder, lag_max = 160) %>%
  autoplot() + labs(title="Autocorrelación residuos.Modelo Log_2. Dengue en la triple frontera")

residuos_stl_log2 %>% 
  features(remainder, ljung_box, lag = 103, dof = 0) ### Autocorrelación significativa




###
feat_stl_de4 <- dengue_ts_0  %>% 
   features(total_cp_log, feat_stl, s.window = 51, t.window=103, robust = TRUE)  

write.csv2(feat_stl_de4, "salida/articulo_4/series_temporales/feat_stl_distrito_log_art4_se_v2.csv", row.names = FALSE)

ajuste_stl_distritos_log1 <- dengue_ts_0 |>
  model(
    STL(
      total_cp_log ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  ) 

for (d in unique(dengue_ts$distrito))  {
  p <- autoplot(ajuste_stl_distritos_log1 %>% filter(distrito == d) %>%
                  components()) +
    labs(title = paste ("Descomposición STL de casos probables de dengue - Log sin ceros. W103-W51
       Ciclo anual-",d ),
         y = "Número de casos probables") +
    theme_minimal()
  print(p)   # muestra en pantalla
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/STL_anual_mult_v3", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}  


# Versión 3
ajuste_stl_distritos_log2 <- dengue_ts_0 |>
  model(
    STL(
      total_cp_log ~ trend(window = 51) +
        season(window = 51),
      robust = TRUE
    )
  ) 

for (d in unique(dengue_ts$distrito))  {
  p <- autoplot(ajuste_stl_distritos_log2 %>% filter(distrito == d) %>%
                  components()) +
    labs(title = paste ("Descomposición STL de casos probables de dengue - Log sin ceros. W51-W51
       Ciclo anual-",d ),
         y = "Número de casos probables") +
    theme_minimal()
  print(p)   # muestra en pantalla
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/STL_anual_mult_v3_1", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
} 

### Grafico comparando la estacionalidad Logv2----

comp_distritos <- components(ajuste_stl_distritos_log1) |>
  select(distrito, index = yw, season_year)

## 2) Serie agregada (suma de todos los distritos) y su STL
dengue_ts_total_0 <- dengue_ts %>% 
  index_by(index = yw) |>
  summarise(total_cp = sum(total_cp, na.rm = TRUE)) %>%
  filter(total_cp != 0) %>%
  mutate(total_cp_log = log(total_cp)) %>%
  fill_gaps(total_cp_log= 0L)

ajuste_stl_total <- dengue_ts_total_0 |>
  model(
    STL(
      total_cp_log ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, season_year)

## 3) Unir tendencias distrito + total
estacionalidad <- bind_rows(comp_distritos, comp_total) 


## 4) Gráfico único: todas las estacionalidad + agregada
p <- estacionalidad |>
  ggplot(aes(x = index, y = season_year)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Estacionalidad STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Estacuibakudad (dengue)"
  ) +
  theme_minimal()

print(p)

ggsave(
  filename = "salida/articulo_4/series_temporales/STL_estacionalidad_distritos_y_total_Logv2.png",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300
)
### Grafico comparando las tendenciasLogv2----

comp_distritos <- components(ajuste_stl_distritos_log1) |>
  select(distrito, index = yw, trend)

## 2) Serie agregada (suma de todos los distritos) y su STL
ajuste_stl_total <- dengue_ts_total_0 |>
  model(
    STL(
      total_cp_log ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, trend)

## 3) Unir tendencias distrito + total
tendencias <- bind_rows(comp_distritos, comp_total)

tendencias<- tendencias


## 4) Gráfico único: todas las tendencias + agregada
p <- tendencias |>
  ggplot(aes(x = index, y = trend)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Tendencias (bianual) STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Tendencia (Dengue)"
  ) +
  theme_minimal()

print(p)

ggsave(
  filename = "salida/articulo_4/series_temporales/STL_tendencias_distritos_y_total_log_v2.png",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300
)


  
#ACF de triple frontera ventana de ciclo anual logV2----

G2<-dengue_ts_0 %>% 
  ACF(total_cp_log, lag_max = 160)  %>%
  autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera") 

  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/Autocorrelacion_todos_log_0", d, ".png"),
    plot     = G2,
    width    = 8,
    height   = 6,
    dpi      = 300)


for (d in unique(dengue_ts$distrito)) {
  
  p <- dengue_ts_0 |>
    filter(distrito == d) |>
    ACF(total_cp_log, lag_max = 160)%>%
    autoplot() + labs(title="Autocorrelación. Dengue en la triple frontera")+
    labs(
      title = paste("Autocorrelacion -", d),
      x = "semanas"
    )
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/Autocorrelacion_log_v2", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}

#PACF de triple frontera ventana de ciclo anual logV2----

dengue_ts_0%>% 
  PACF(total_cp_log, lag_max = 160) %>%
  autoplot() + labs(title="PACF. Dengue en la triple frontera")

for (d in unique(dengue_ts$distrito)) {
  
  p <- dengue_ts_0 |>
    filter(distrito == d) |>
    PACF(total_cp_log, lag_max = 160)%>%
    autoplot() + 
    labs(
      title = paste("PACF -", d),
      x = "semanas"
    )
  
  print(p)   # muestra en pantalla
  
  ggsave(
    filename = paste0("salida/articulo_4/series_temporales/Autocorrelacion_log_v2", d, ".png"),
    plot     = p,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
}


#Grafico tendencia por estacionalidad_Log
tendencia_estacionalidad2<-feat_stl_de4%>% ggplot(aes(x = trend_strength,
           y = seasonal_strength_year,
           col = distrito)) +
  geom_point(size = 3) +          # puntos más grandes
  ggtitle("Relación entre tendencia y estacionalidad. Triple Frontera_log_v2") +
  theme_minimal()
tendencia_estacionalidad2
ggsave("salida/articulo_4/series_temporales/tendencia_estacionalidad_log_v2.png", tendencia_estacionalidad2, width = 10, height = 6)


#################################
#Graficos de salida##
#Componentes comparando con Log 0

ajuste_stl_distritos <- dengue_ts_0 |>
  model(
  STL(
    total_cp_log ~ trend(window = 103) +
      season(window = 51),
    robust = TRUE
  )
)


comp_distritos <- components(ajuste_stl_distritos) |>
  select(distrito, index = !!index_var(dengue_ts_0), trend)

## 2) Serie agregada (suma de todos los distritos) y su STL
dengue_ts_total_0 <- dengue_ts_0 |>
  index_by(index = yw) |>
  summarise(total_cp = sum(total_cp, na.rm = TRUE))

ajuste_stl_total <- dengue_ts_total |>
  model(
    STL(
      total_cp ~ trend(window = 103) +
        season(window = 51),
      robust = TRUE
    )
  )

comp_total <- components(ajuste_stl_total) |>
  mutate(distrito = "Total") |>
  select(distrito, index, trend)

## 3) Unir tendencias distrito + total
tendencias <- bind_rows(comp_distritos, comp_total)

## 4) Gráfico único: todas las tendencias + agregada
p <- tendencias |>
  ggplot(aes(x = index, y = trend)) +
  geom_line() +
  facet_wrap(~ distrito, ncol=1, scales = "free_y") +
  labs(
    title = "Tendencias (bianual) STL de casos probables de dengue\nDistritos y total agregado",
    x = "Tiempo",
    y = "Tendencia (casos probables)"
  ) +
  theme_minimal()

print(p)

