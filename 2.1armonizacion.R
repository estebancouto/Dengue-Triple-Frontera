# Armonización de datos epidemiológicos en la Triple Frontera
# Autor: Esteban M. Couto
# Fecha: 2025‑09‑25

# --- 1. CARGA DE LIBRERIAS ----
# Se cargan las librerías necesarias para manipulación de datos, fechas y tablas
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2, stringi
)

# --- 2. CARGA DE BASES DE DATOS ----
# Cada base se carga con su nombre estandarizado y se le agrega el país y tipo de base
# Las bases fueron exportadas previamente a CSV desde sus formatos originales (source: 1.carga_bases). 
# Establece el directorio de trabajo donde están almacenadas las bases CSV
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")

# ---------------------- #
#    BRASIL 
# ---------------------- #

# Datos de 2009 a 2013
BR_Den_09_13 <- read.csv2("source/csv/Dengue_Foz_09_13_Crudo.csv") %>%
  clean_names() %>%
  mutate(pais = "brasil", tipo_base = "br_09_13")

# Datos de 2014 a 2023
BR_Den_14_23 <- read.csv2("source/csv/Dengue_Foz_14_23_Crudo.csv") %>%
  clean_names() %>%
  mutate(pais = "brasil", tipo_base = "br_14_23")

# ---------------------- #
#     ARGENTINA
# ---------------------- #

# SIVILA (SNVS 1)
AR_SNVS1 <- read.csv2("source/csv/Dengue_Ar_SNVS1_Crudo.csv") %>%
  clean_names() %>%
  mutate(pais = "argentina", tipo_base = "ar_sivila_nominal")

# Base C2 nominalizada (SNVS 1)
AR_C2 <- read.csv2("source/csv/Dengue_Ar_C2_Crudo.csv") %>%
  clean_names() %>%
  mutate(pais = "argentina", tipo_base = "ar_c2_nominal")

# Base C2 agrupada (SNVS 1)
AR_Agrupados <- read.csv2("source/TAD/SNVS1_Agrupado_V2.csv", fileEncoding = "latin1")%>%
  clean_names() %>%
  mutate(pais = "argentina", tipo_base = "ar_c2_agrupado")

# Sistema SNVS 2.0
AR_SNVS2 <- read.csv2("source/csv/Dengue_Ar_SNVS2_Crudo.csv") %>%
  clean_names() %>%
  mutate(pais = "argentina", tipo_base = "ar_snvs2")

# ---------------------- #
#     PARAGUAY
# ---------------------- #

# Datos de 2009 a 2020
PY_Den1 <- read.csv2("source/csv/dengue_py_crudo_09_20.csv") %>%
  clean_names() %>%
  mutate(pais = "paraguay", tipo_base = "py_09_20")

# Datos de 2021 a 2023
PY_Den2 <- read.csv2("source/csv/dengue_py_crudo_21_23.csv") %>%
  clean_names() %>%
  mutate(pais = "paraguay", tipo_base = "py_21_23")

# --- 3. FUNCION AUXILIAR PARA ORDENAR NOMBRES SIN ESPACIOS ----
ordenar_sin_espacios <- function(df, col) {
  df %>%
    mutate(valor_sin_espacios = gsub(" ", "", !!sym(col))) %>%
    arrange(valor_sin_espacios) %>%
    pull(!!sym(col))
}



# --- 4. PROCESAMIENTO DE BRASIL  ----
# ---- Brasil: Validación espacial ----
# Foz do Iguaçu = código IBGE 410830
# --- 4.1 Clasifica y estandariza variables para la base 2009-2013
municipios_df_1 <- BR_Den_09_13 %>%
  mutate(
    residentes = case_when(
      id_mn_resi == "410830" ~ "Si",
      id_mn_resi != "410830" ~ "No",
      TRUE ~ NA_character_
    ),
    notificacion = case_when(
      id_municip == "410830" ~ "Si",
      id_municip != "410830" ~ "No",
      TRUE ~ NA_character_
    ),
    classi_fin = case_when(
      classi_fin %in% c("1", "2", "3", "4") ~ "confirmado",
      classi_fin == "5" ~ "descartado",
      classi_fin == "8" ~ "sospechoso",
      TRUE ~ NA_character_
    ),
    infeccion = case_when(
      comuninf == "410830" ~ "Si",
      comuninf != "410830" ~ "No",
      TRUE ~ NA_character_
    ),
    infeccion_ext = case_when(
      comuninf == "410830" ~ "Si",
      comuninf != "410830" ~ "No",
      !is.na(copaisinf) & !(copaisinf %in% c("1","0")) ~ "Extranjero",
      TRUE ~ NA_character_
    ),
    tpautocto = case_when(
      tpautocto == "1" ~ "Si",
      tpautocto == "2" ~ "No",
      TRUE ~ NA_character_
    )
  ) %>% 
  select (residentes, notificacion, classi_fin, infeccion, infeccion_ext, tpautocto)

# --- 4.2 Clasifica y estandariza variables para la base 2014-2023
municipios_df_2 <- BR_Den_14_23 %>%
  mutate(
    residentes = case_when(
      id_mn_resi == "410830" ~ "Si",
      id_mn_resi != "410830" ~ "No",
      TRUE ~ NA_character_
    ),
    notificacion = case_when(
      id_municip == "410830" ~ "Si",
      id_municip != "410830" ~ "No",
      TRUE ~ NA_character_
    ),
    classi_fin = case_when(
      classi_fin == "10" ~ "confirmado",
      classi_fin == "11" ~ "confirmado",
      classi_fin == "12" ~ "confirmado",
      classi_fin == "5" ~ "descartado",
      classi_fin == "8" ~ "sospechoso",
      TRUE ~ NA_character_
    ),
    infeccion = case_when(
      comuninf == "410830" ~ "Si",
      comuninf != "410830" ~ "No",
      TRUE ~ NA_character_
    ),
    infeccion_ext = case_when(
      comuninf == "410830" ~ "Si",
      comuninf != "410830" ~ "No",
      copaisinf!= "1" ~ "Extranjero",
      TRUE ~ NA_character_
    ),
    tpautocto = case_when(
      tpautocto == "1" ~ "Si",
      tpautocto == "2" ~ "No",
      TRUE ~ NA_character_
    )
  )%>% 
  select (residentes, notificacion, classi_fin, infeccion, tpautocto, infeccion_ext )
# --- 4.3 Unifica ambas bases
municipios_df <- bind_rows(municipios_df_1, municipios_df_2)

# --- 4.4 Tabla de contingencia: ¿quiénes fueron notificados en Foz?
tabla_municipio_br <- tableby(
  notificacion ~ residentes + infeccion_ext,
  data = municipios_df,
  cat.stats = c("Nmiss", "countpct"),
  cat.simplify = FALSE,
  cat.droplevels = FALSE,
  cat.test = "chisq",
  test = FALSE
)
tabla_municipio_br_res <- summary(tabla_municipio_br, digits = 2, text = TRUE)
tabla_municipio_br_df <- as.data.frame(tabla_municipio_br_res)
colnames(tabla_municipio_br_df)[1] <- "variables"

flextable::set_flextable_defaults(font.color = "black",
    background.color = "white")

ft1 <- flextable(tabla_municipio_br_df) %>%
  add_header_row(
          values = c("", "notificación en foz"),
          colwidths = c(1, ncol(tabla_municipio_br_df)-1)) %>%
  add_header_lines( values = "Tabla 1. Distribución de casos vinculados a Foz do Iguaçu por residencia e infección") %>%
  set_table_properties( width = 0.5, layout = "autofit") %>%
  theme_booktabs() 

# --- 4.5 Visualiza la tabla
ft1

# Guarda la tabla como imagen


save_as_image(ft1, path = "salida/articulo_1/tabla1_notificacion_foz.png",
              webshot = "webshot2")  

# ---- Brasil: Recodificación  ----

# 4.6 Filtrar sólo notificados en Foz do Iguaçu (IBGE: 410830)
br_den_09_13 <- BR_Den_09_13 %>% filter(id_municip == "410830")
br_den_14_23 <- BR_Den_14_23 %>% filter(id_municip == "410830")

# 4.7 Convertir fechas a formato Date
br_den_09_13 <- br_den_09_13 %>%
  mutate(
    dt_notific = as.Date(dt_notific),
    dt_sin_pri = as.Date(dt_sin_pri),
    fecha_carga = as.Date(dt_digita),
    dt_invest = as.Date(dt_invest),
    dt_encerra = as.Date(dt_encerra),
    dt_soro = as.Date(dt_soro),
    dt_ns1 = as.Date(dt_ns1),
    dt_viral = as.Date(dt_viral),
    dt_pcr = as.Date(dt_pcr)
  )

br_den_14_23 <- br_den_14_23 %>%
  mutate(
    dt_notific = as.Date(dt_notific),
    dt_sin_pri = as.Date(dt_sin_pri),
    dt_invest = as.Date(dt_invest),
    dt_encerra = as.Date(dt_encerra),
    dt_soro = as.Date(dt_soro),
    dt_ns1 = as.Date(dt_ns1),
    dt_viral = as.Date(dt_viral),
    dt_pcr = as.Date(dt_pcr)
  )

# 4.8 Recodificación BR_Den_09_13 
br_den_09_13 <- br_den_09_13 %>%
  mutate(
    # Reagrupo por edad según códigos codificados (SINAN)
    re_br = case_when(
      nu_idade_n < 4015 ~ "menores",
      nu_idade_n >= 4015 & nu_idade_n <= 4064 ~ "adultos",
      nu_idade_n > 4064 ~ "ancianos"
    ),
    
    # Clasifico gravedad
    gravedad = case_when(
      classi_fin == "1" ~ "Dengue",
      classi_fin == "2" ~ "Dengue_con_Complicaciones",
      classi_fin %in% c("3", "4") ~ "Grave",
      classi_fin %in% c("5", "8") ~ NA_character_
    ),
    
    # Clasificación final
    classi_fin = case_when(
      classi_fin %in% c("1", "2", "3", "4") ~ "confirmado",
      classi_fin == "5" ~ "descartado",
      classi_fin == "8" ~ "sospechoso"
    ),
    
    # Tipo de test aplicado
    test = case_when(
      resul_ns1 %in% c("1", "2") ~ "ns1",
      resul_soro %in% c(1, 2) ~ "igm",
      resul_pcr %in% c(1, 2) ~ "pcr"
    ),
    
    # Resultado del test
    resultado = case_when(
      resul_ns1 == "1" ~ "positivo",
      resul_ns1 == "2" ~ "negativo",
      resul_soro == 1 ~ "positivo",
      resul_soro == 2 ~ "negativo",
      resul_pcr == 1 ~ "positivo",
      resul_pcr == 2 ~ "negativo"
    ),
    
    # Sexo del caso
    sexo = case_when(
      cs_sexo == "F" ~ "femenino",
      cs_sexo == "M" ~ "masculino"
    ),
    
    # Serotipo identificado
    serotipo = case_when(
      sorotipo == "1" ~ "DEN1",
      sorotipo == "2" ~ "DEN2",
      sorotipo == "4" ~ "DEN4"
    ),
    
    # Criterio de confirmación
    criterio = case_when(
      criterio == "1" ~ "laboratorio",
      criterio == "2" ~ "nexo",
      criterio == "3" ~ NA_character_
    ),
    
    # Fechas principales
    fecha = dt_notific,
    fis = dt_sin_pri,
    fecha_lab = coalesce(dt_ns1, dt_viral, dt_pcr, dt_soro),
fecha_cierre = dt_encerra    )

#4.9 Recodificación BR_Den_14_23
br_den_14_23 <- br_den_14_23 %>%
  mutate(
    re_br = case_when(
      nu_idade_n < 4015 ~ "menores",
      nu_idade_n >= 4015 & nu_idade_n <= 4064 ~ "adultos",
      nu_idade_n > 4064 ~ "ancianos"
    ),
    
    gravedad = case_when(
      classi_fin == "10" ~ "Dengue",
      classi_fin == "11" ~ "Dengue con complicaciones",
      classi_fin == "12" ~ "DG"
    ),
    
    classi_fin = case_when(
      classi_fin %in% c("10", "11", "12") ~ "confirmado",
      classi_fin == "5" ~ "descartado",
      classi_fin == "8" ~ "sospechoso"
    ),
    
    test = case_when(
      resul_ns1 %in% c(1, 2) ~ "ns1",
      resul_soro %in% c(1, 2) ~ "igm",
      resul_pcr %in% c(1, 2) ~ "pcr"
    ),
    
    resultado = case_when(
      resul_ns1 == 1 ~ "positivo",
      resul_ns1 == 2 ~ "negativo",
      resul_soro == 1 ~ "positivo",
      resul_soro == 2 ~ "negativo",
      resul_pcr == 1 ~ "positivo",
      resul_pcr == 2 ~ "negativo"
    ),
    
    sexo = case_when(
      cs_sexo == "F" ~ "femenino",
      cs_sexo == "M" ~ "masculino"
    ),
    
    serotipo = case_when(
      sorotipo == "1" ~ "DEN1",
      sorotipo == "2" ~ "DEN2",
      sorotipo == "4" ~ "DEN4"
    ),
    
    criterio = case_when(
      criterio == "1" ~ "laboratorio",
      criterio == "2" ~ "nexo",
      criterio == "3" ~ NA_character_
    ),
    
    fecha = dt_notific,
    fis = dt_sin_pri,
    fecha_lab = coalesce(dt_ns1, dt_pcr, dt_soro, dt_prnt),
    fecha_cierre = dt_encerra 
    
  )

# 2009–2013
br_den_09_13 <- br_den_09_13 %>%
  mutate(
    dias_soro = as.numeric(dt_soro - fis),
    dias_ns1  = as.numeric(dt_ns1  - fis),
    dias_pcr  = as.numeric(dt_pcr  - fis),
    
    test_validado = case_when(
      as.numeric(resul_soro) == 2 & !is.na(dias_soro) & dias_soro >= 7 ~ "igm",
      as.numeric(resul_pcr)  == 1 & !is.na(dias_pcr)  & dias_pcr  <= 7 ~ "pcr",
      as.numeric(resul_ns1)  == 1 & !is.na(dias_ns1)  & dias_ns1  <= 7 ~ "ns1",
      as.numeric(resul_soro) == 1 & !is.na(dias_soro) & dias_soro  > 5 ~ "igm",
      as.numeric(resul_pcr)  == 2                                     ~ "pcr",
      as.numeric(resul_ns1)  == 2                                     ~ "ns1",
      TRUE ~ NA_character_
    ),
    
    resul_validado = case_when(
      as.numeric(resul_soro) == 2 & !is.na(dias_soro) & dias_soro >= 7 ~ "negativo",
      as.numeric(resul_pcr)  == 1 & !is.na(dias_pcr)  & dias_pcr  <= 7 ~ "positivo",
      as.numeric(resul_ns1)  == 1 & !is.na(dias_ns1)  & dias_ns1  <= 7 ~ "positivo",
      as.numeric(resul_soro) == 1 & !is.na(dias_soro) & dias_soro  > 5 ~ "positivo",
      as.numeric(resul_pcr)  == 2                                     ~ "negativo",
      as.numeric(resul_ns1)  == 2                                     ~ "negativo",
      TRUE ~ NA_character_
    ),
    
    dato_labo = case_when(
      as.numeric(resul_soro) == 2 & !is.na(dias_soro) & dias_soro >= 7 ~ "descartado",
      (as.numeric(resul_soro) == 1 & !is.na(dias_soro) & dias_soro > 5) |
        (as.numeric(resul_ns1)  == 1 & !is.na(dias_ns1)  & dias_ns1 <= 5) |
        (as.numeric(resul_pcr)  == 1 & !is.na(dias_pcr)  & dias_pcr <= 5) ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b",
      TRUE ~ NA_character_
    ),
    
    classi_validada_sosp = case_when(
      dato_labo %in% c("confirmado","sospechoso") & classi_fin %in% c("confirmado","sospechoso") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo %in% c("confirmado","sospechoso") & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado","sospechoso") ~ "b",
      TRUE ~ NA_character_
    )
  )

# 2014–2023
br_den_14_23 <- br_den_14_23 %>%
  mutate(
    dias_soro = as.numeric(dt_soro - fis),
    dias_ns1  = as.numeric(dt_ns1  - fis),
    dias_pcr  = as.numeric(dt_pcr  - fis),
    
    test_validado = case_when(
      as.numeric(resul_soro) == 2 & !is.na(dias_soro) & dias_soro >= 7 ~ "igm",
      as.numeric(resul_pcr)  == 1 & !is.na(dias_pcr)  & dias_pcr  <= 7 ~ "pcr",
      as.numeric(resul_ns1)  == 1 & !is.na(dias_ns1)  & dias_ns1  <= 7 ~ "ns1",
      as.numeric(resul_soro) == 1 & !is.na(dias_soro) & dias_soro  > 5 ~ "igm",
      as.numeric(resul_pcr)  == 2                                     ~ "pcr",
      as.numeric(resul_ns1)  == 2                                     ~ "ns1",
      TRUE ~ NA_character_
    ),
    
    resul_validado = case_when(
      as.numeric(resul_soro) == 2 & !is.na(dias_soro) & dias_soro >= 7 ~ "negativo",
      as.numeric(resul_pcr)  == 1 & !is.na(dias_pcr)  & dias_pcr  <= 7 ~ "positivo",
      as.numeric(resul_ns1)  == 1 & !is.na(dias_ns1)  & dias_ns1  <= 7 ~ "positivo",
      as.numeric(resul_soro) == 1 & !is.na(dias_soro) & dias_soro  > 5 ~ "positivo",
      as.numeric(resul_pcr)  == 2                                     ~ "negativo",
      as.numeric(resul_ns1)  == 2                                     ~ "negativo",
      TRUE ~ NA_character_
    ),
    
    dato_labo = case_when(
      as.numeric(resul_soro) == 2 & !is.na(dias_soro) & dias_soro >= 7 ~ "descartado",
      (as.numeric(resul_soro) == 1 & !is.na(dias_soro) & dias_soro > 5) |
        (as.numeric(resul_ns1)  == 1 & !is.na(dias_ns1)  & dias_ns1 <= 5) |
        (as.numeric(resul_pcr)  == 1 & !is.na(dias_pcr)  & dias_pcr <= 5) ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b",
      TRUE ~ NA_character_
    ),
    
    classi_validada_sosp = case_when(
      dato_labo %in% c("confirmado","sospechoso") & classi_fin %in% c("confirmado","sospechoso") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo %in% c("confirmado","sospechoso") & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado","sospechoso") ~ "b",
      TRUE ~ NA_character_
    )
  )

# 4.10 Consolidación (incluyendo columnas validadas)
br_den_09_13_total <- br_den_09_13 %>%
  mutate(sistema = "sinan.2", pais = "brasil", re = re_br) %>%
  select(
    fecha, fis, fecha_carga, fecha_lab,fecha_cierre, re, classi_fin, gravedad, sexo,
    test, resultado, test_validado, resul_validado, dato_labo,
    classi_validada, classi_validada_sosp,
    serotipo, criterio, sistema, pais, tipo_base
  )

br_den_14_23_total <- br_den_14_23 %>%
  mutate(sistema = "sinan.3", pais = "brasil", re = re_br, fecha_carga = NA) %>%
  select(
    fecha, fis, fecha_carga, fecha_lab, fecha_cierre, re, classi_fin, gravedad, sexo,
    test, resultado, test_validado, resul_validado, dato_labo,
    classi_validada, classi_validada_sosp,
    serotipo, criterio, sistema, pais, tipo_base
  )

# 4.11 Unión final
den_br_total <- bind_rows(br_den_09_13_total, br_den_14_23_total)

# 4.12 Variables temporales con "fecha mínima" (regla 0–30 días)
den_br_total <- den_br_total %>%
  mutate(
    # si hay FIS y (fecha - FIS) ∈ [0,30], usar FIS; si falta fecha, usar FIS; si no, usar fecha; fallback a fecha_lab
    fecha_min = dplyr::case_when(
      is.na(fecha) & !is.na(fis) ~ fis,
      !is.na(fis) & !is.na(fecha) & dplyr::between(as.numeric(fecha - fis), 0, 30) ~ fis,
      TRUE ~ fecha
    ),
    fecha_min = dplyr::coalesce(fecha_min, fecha_lab),
    
    se     = lubridate::epiweek(fecha_min),
    mes    = lubridate::month(fecha_min),
    year   = lubridate::year(fecha_min),
    year_e = dplyr::if_else(se == 1 & mes == 12, year,
                            dplyr::if_else(se < 40, year - 1, year)),
    distrito = "Foz do Iguaçu"
  )

# salida
write.csv2(den_br_total, "source/csv/armo/dengue_br_total.csv")


# --- 5. PROCESAMIENTO DE ARGENTINA  ----
# Argentina: validación espacial ----

# 5.1. Filtrar SNVS1 y SNVS2 por localidad Iguazú
# Puerto Iguazú = id_localidad 3283 (SNVS1) y id_loc_indec 54063030 (SNVS2)
municipios_df_1_ar <- AR_SNVS1 %>%
  filter(id_localidad == "3283" | id_localidad_carga == "3283")

municipios_df_2_ar <- AR_SNVS2 %>%
  filter(id_loc_indec_residencia == "54063030" | id_loc_indec_carga == "54063030")

# 5.2. Clasificar y limpiar SNVS1
municipios_df_1_ar <- municipios_df_1_ar %>%
  mutate(
    residentes = if_else(id_localidad == "3283", "si", "no"),
    notificacion = if_else(id_localidad_carga == "3283", "si", "no"),
    snvs = "1",
    localidad_carga = case_when(
      id_localidad_carga == "3283" ~ "iguazú",
      id_provincia_carga == "17" ~ "misiones",
      TRUE ~ "otra"
    )
  ) %>%
  select(residentes, notificacion, snvs, localidad_carga)

# 5.3. Clasificar y limpiar SNVS2
municipios_df_2_ar <- municipios_df_2_ar %>%
  mutate(
    residentes = if_else(id_loc_indec_residencia == "54063030", "si", "no"),
    notificacion = if_else(id_loc_indec_carga == "54063030", "si", "no"),
    snvs = "2",
    localidad_carga = case_when(
      id_loc_indec_carga == "54063030" ~ "iguazú",
      id_provincia_carga == "14" ~ "misiones",
      TRUE ~ "otra"
    )
  ) %>%
  select(residentes, notificacion, snvs, localidad_carga)

# 5.4. Unir SNVS1 + SNVS2
municipios_df_ar <- bind_rows(municipios_df_1_ar, municipios_df_2_ar)

# 5.5. Tabla de contingencia según sistema SNVS
tabla_municipio_ar <- tableby(
  snvs ~ residentes + notificacion,
  data = municipios_df_ar,
  cat.stats = c("Nmiss", "countpct"),
  cat.simplify = FALSE,
  cat.droplevels = FALSE,
  cat.test = "chisq",
  test = FALSE
)

# 5.6. Formateo flextable
tabla_municipio_ar_res <- summary(tabla_municipio_ar, digits = 2, text = TRUE)
tabla_municipio_ar_df <- as.data.frame(tabla_municipio_ar_res)
colnames(tabla_municipio_ar_df)[1] <- "variables"

ft2 <- flextable(tabla_municipio_ar_df) %>%
  add_header_row(
    values = c("", "Casos vinculados a Puerto Iguazú (SNVS 1 y 2)"),
    colwidths = c(1, ncol(tabla_municipio_ar_df) - 1)
  ) %>%
  add_header_lines("Tabla 2. Distribución de casos vinculados a Puerto Iguazú por residencia y sitio probable de infección") %>%
  set_table_properties(width = 0.5, layout = "autofit")%>%
  footnote(i = 1, j = 1, value = as_paragraph("No se consideran las bases vinculadas al C2 por no tener datos de residencia"),
           part ="header")

# 5.7 Visualiza la tabla2
ft2

# Guarda la tabla como imagen

save_as_image(ft2, path = "salida/articulo_1/tabla2_notificacion_igr.png",
              webshot = "webshot2")  

# 5.8 Crear tabla descriptiva por sistema SNVS para residentes en Iguazú
tabla_municipio_ar_2 <- tableby(
  snvs ~ localidad_carga,
  data = municipios_df_ar |> filter(residentes == "si"),
  cat.stats = c("Nmiss", "countpct"),
  cat.simplify = FALSE,
  cat.droplevels = FALSE,
  cat.test = "chisq",
  test = FALSE
)

tabla_municipio_ar_res_2 <- summary(tabla_municipio_ar_2, digits = 2, text = TRUE)
tabla_municipio_ar_df_2 <- as.data.frame(tabla_municipio_ar_res_2)
colnames(tabla_municipio_ar_df_2)[1] <- "variables"

# Flextable
ft3 <- flextable(tabla_municipio_ar_df_2) %>%
  add_header_row(values = c("", "Casos de residentes de Puerto Iguazú según SNVS"), colwidths = c(1, 3)) %>%
  add_header_lines(values = "Tabla 3.Distribución de residentes de Puerto Iguazú por sitio de notificación y sistema de vigilancia") %>%
  set_table_properties(width = 0.5, layout = "autofit")  %>%
  footnote(i = 1, j = 1, value = as_paragraph("No se consideran las bases vinculadas al C2 por no tener datos de residencia"),
           part ="header")

# 5.7 Visualiza la tabla3
ft3

# Guarda la tabla como imagen

save_as_image(ft3, path = "salida/articulo_1/tabla3_notificacion_igr_snvs.png",
              webshot = "webshot2")  

# Argentina: Recodificación ----

# 5.8 SNVS 1

# Limpieza de fechas incoherentes: SOLO sobre columnas fecha
AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    fecha      = as.Date(fecha),
    fis        = as.Date(fis),
    fecha_lab  = as.Date(fecha_lab),
    fecha_carga= as.Date(fecha_carga)
  ) %>%
  mutate(
    fecha      = ifelse(fecha      <= as.Date("1900-01-01"), NA, fecha) %>% as.Date(origin = "1970-01-01"),
    fis        = ifelse(fis        <= as.Date("1900-01-01"), NA, fis) %>% as.Date(origin = "1970-01-01"),
    fecha_lab  = ifelse(fecha_lab  <= as.Date("1900-01-01"), NA, fecha_lab) %>% as.Date(origin = "1970-01-01"),
    fecha_carga= ifelse(fecha_carga<= as.Date("1900-01-01"), NA, fecha_carga) %>% as.Date(origin = "1970-01-01")
  ) 

# clasificación
AR_SNVS1 <- AR_SNVS1 %>% filter(id_localidad_carga == "3283") %>%
  mutate(
    classi_fin = case_when(
      id_etiologia %in% c("1322", "-4", "-2", "179") ~ "sospechoso",
      id_etiologia == "-1" ~ "descartado",
      id_etiologia %in% c("25", "30") ~ "confirmado"
    ),
    criterio = "laboratorio",  # Sivila tiene sólo casos de laboratorio
    temporalidad = as.numeric(difftime(fecha_carga, fis, units = "days")),
    test = case_when(
        str_detect(pruebas, regex("NS1", ignore_case = TRUE)) ~ "ns1",
        str_detect(pruebas, regex("RT-PCR", ignore_case = TRUE)) ~ "pcr",
        str_detect(pruebas, regex("IgM|Ig M", ignore_case = TRUE)) ~ "igm",
        TRUE ~ NA_character_
      ),
        # Recodificación por edad a partir de fechas
    re = case_when(
      is.na(fis) & difftime(fecha_lab, fn, units = "days") < 365 * 15 ~ "menores",
      is.na(fis) & difftime(fecha_lab, fn, units = "days") < 365 * 65 ~ "adultos",
      is.na(fis) & difftime(fecha_lab, fn, units = "days") >= 365 * 65 ~ "ancianos",
      is.na(fn) ~ NA_character_,
      !is.na(fis) & difftime(fis, fn, units = "days") < 365 * 15 ~ "menores",
      !is.na(fis) & difftime(fis, fn, units = "days") < 365 * 65 ~ "adultos",
      !is.na(fis) & difftime(fis, fn, units = "days") >= 365 * 65 ~ "ancianos",
      TRUE ~ NA_character_
    ),
    
    # Clasificación por tipo y tiempo de laboratorio
    dato_labo = case_when(
      difftime(fecha_lab, fis, units = "days") >= 7 & test == "igm" & resultado == "negativo" ~ "descartado",
      difftime(fecha_lab, fis, units = "days") > 5 & test == "igm" & resultado == "positivo" ~ "confirmado",
      difftime(fecha_lab, fis, units = "days") <= 5 & test %in% c("pcr", "ns1") & resultado == "positivo" ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    
    # Validación de clasificación
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b",
      TRUE ~ NA_character_
    ),
    classi_validada_sosp = case_when(
      dato_labo == "confirmado" & classi_fin %in% c("confirmado", "sospechoso") ~ "a",
      dato_labo == "sospechoso" & classi_fin %in% c("sospechoso", "confirmado") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "sospechoso" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado", "sospechoso") ~ "b",
      TRUE ~ NA_character_
    ),sexo = case_when(
      sexo == "Femenino" ~ "femenino",
      sexo == "Masculino" ~ "masculino"
    ))


# -------- Validación temporal SNVS1 (usa test/resultado ya presentes) --------
AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    dias_lab = as.numeric(fecha_lab - fis),
    
    test_validado = case_when(
      test == "igm" & !is.na(dias_lab) & dias_lab >= 7 & resultado == "negativo" ~ "igm",
      test == "pcr" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "pcr",
      test == "ns1" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "ns1",
      test == "igm" & !is.na(dias_lab) & dias_lab >  5 & resultado == "positivo" ~ "igm",
      test == "pcr" & resultado == "negativo"                                     ~ "pcr",
      test == "ns1" & resultado == "negativo"                                     ~ "ns1",
      TRUE ~ NA_character_
    ),
    
    resul_validado = case_when(
      test == "igm" & !is.na(dias_lab) & dias_lab >= 7 & resultado == "negativo" ~ "negativo",
      test == "pcr" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "positivo",
      test == "ns1" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "positivo",
      test == "igm" & !is.na(dias_lab) & dias_lab >  5 & resultado == "positivo" ~ "positivo",
      test %in% c("pcr","ns1") & resultado == "negativo"                          ~ "negativo",
      TRUE ~ NA_character_
    ),
    
    dato_labo = case_when(
      test == "igm" & !is.na(dias_lab) & dias_lab >= 7 & resultado == "negativo" ~ "descartado",
      (test == "igm" & !is.na(dias_lab) & dias_lab > 5 & resultado == "positivo") |
        (test == "ns1" & !is.na(dias_lab) & dias_lab <= 5 & resultado == "positivo") |
        (test == "pcr" & !is.na(dias_lab) & dias_lab <= 5 & resultado == "positivo") ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b",
      TRUE ~ NA_character_
    ),
    
    classi_validada_sosp = case_when(
      dato_labo %in% c("confirmado","sospechoso") & classi_fin %in% c("confirmado","sospechoso") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo %in% c("confirmado","sospechoso") & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado","sospechoso") ~ "b",
      TRUE ~ NA_character_
    )
  ) 

# -------- Validación temporal SNVS2 --------
# 5.9 SNVS 2 
# Clasificación y recodificación
# --- SNVS2: recodificación  ---
AR_SNVS2 <- AR_SNVS2 %>%
  filter(id_loc_indec_carga == "54063030") %>%
  mutate(
    test = case_when(
      determinacion == "Antígeno NS1" ~ "ns1",
      determinacion == "Genoma viral" ~ "pcr",
      determinacion == "IgM DENV"     ~ "igm",
      TRUE ~ NA_character_
    ),
    # Primero extraigo serotipo a partir del 'resultado' crudo
    serotipo = case_when(
      resultado == "DEN-1" ~ "DEN1",
      resultado == "DEN-2" ~ "DEN2",
      resultado == "DEN-4" ~ "DEN4",
      TRUE ~ NA_character_
    ),
    # Luego colapso resultado a positivo/negativo
    resultado = case_when(
      resultado %in% c("DEN-1","DEN-2","DEN-4","positivo","Caso de Dengue en brote con laboratorio (+)") ~ "positivo",
      resultado %in% c("negativo","No detectable") ~ "negativo",
      TRUE ~ NA_character_
    ), fecha = as.Date(fecha_consulta, "%d/%m/%Y"),
    fis = as.Date(fis),
    fecha_lab = as.Date(fecha_lab),
    fecha_carga = as.Date(fecha_apertura, "%d/%m/%Y")
  ) %>%
  mutate(
    classi_fin = case_when(
      clasificacion_manual %in% c("Caso confirmado DEN-1","Caso confirmado DEN-2","Caso confirmado DEN-4",
                                  "Caso confirmado sin serotipo","Caso confirmado","Caso probable",
                                  "Caso de Dengue en brote con laboratorio (+)") ~ "confirmado",
      clasificacion_manual %in% c("Caso confirmado por nexo epidemiológico autóctono",
                                  "Caso confirmado por nexo epidemiológico importado") ~ "confirmado",
      clasificacion_manual %in% c("Caso descartado","Caso descartado por epidemiología") ~ "descartado",
      clasificacion_manual %in% c("Caso sospechoso","Caso sospechoso no conclusivo") ~ "sospechoso",
      TRUE ~ NA_character_
    ),
    criterio = case_when(
      clasificacion_manual %in% c("Caso confirmado DEN-1","Caso confirmado DEN-2","Caso confirmado DEN-4",
                                  "Caso confirmado sin serotipo","Caso confirmado","Caso probable",
                                  "Caso sospechoso no conclusivo","Caso de Dengue en brote con laboratorio (+)") ~ "laboratorio",
      clasificacion_manual %in% c("Caso confirmado por nexo epidemiológico autóctono",
                                  "Caso confirmado por nexo epidemiológico importado",
                                  "Caso descartado por epidemiología","Caso sospechoso") ~ "nexo",
      clasificacion_manual == "Caso descartado" ~ "laboratorio",
      TRUE ~ NA_character_
    ),
    re = case_when(
      edad_diagnostico < 15  ~ "menores",
      edad_diagnostico < 65  ~ "adultos",
      edad_diagnostico >= 65 ~ "ancianos",
      TRUE ~ NA_character_
    ),
    sexo = case_when(
      sexo == "F" ~ "femenino",
      sexo == "M" ~ "masculino",
      TRUE ~ NA_character_
    )
  )

AR_SNVS2 <- AR_SNVS2 %>%
  mutate(
    dias_lab = as.numeric(fecha_lab - fis),
    
    test_validado = case_when(
      test == "igm" & !is.na(dias_lab) & dias_lab >= 7 & resultado == "negativo" ~ "igm",
      test == "pcr" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "pcr",
      test == "ns1" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "ns1",
      test == "igm" & !is.na(dias_lab) & dias_lab >  5 & resultado == "positivo" ~ "igm",
      test == "pcr" & resultado == "negativo"                                     ~ "pcr",
      test == "ns1" & resultado == "negativo"                                     ~ "ns1",
      TRUE ~ NA_character_
    ),
    
    resul_validado = case_when(
      test == "igm" & !is.na(dias_lab) & dias_lab >= 7 & resultado == "negativo" ~ "negativo",
      test == "pcr" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "positivo",
      test == "ns1" & !is.na(dias_lab) & dias_lab <= 7 & resultado == "positivo" ~ "positivo",
      test == "igm" & !is.na(dias_lab) & dias_lab >  5 & resultado == "positivo" ~ "positivo",
      test %in% c("pcr","ns1") & resultado == "negativo"                          ~ "negativo",
      TRUE ~ NA_character_
    ),
    
    dato_labo = case_when(
      test == "igm" & !is.na(dias_lab) & dias_lab >= 7 & resultado == "negativo" ~ "descartado",
      (test == "igm" & !is.na(dias_lab) & dias_lab > 5 & resultado == "positivo") |
        (test == "ns1" & !is.na(dias_lab) & dias_lab <= 5 & resultado == "positivo") |
        (test == "pcr" & !is.na(dias_lab) & dias_lab <= 5 & resultado == "positivo") ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b",
      TRUE ~ NA_character_
    ),
    
    classi_validada_sosp = case_when(
      dato_labo %in% c("confirmado","sospechoso") & classi_fin %in% c("confirmado","sospechoso") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo %in% c("confirmado","sospechoso") & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado","sospechoso") ~ "b",
      TRUE ~ NA_character_
    )
  )




# Limpieza de fechas incoherentes
#SNVS1
AR_SNVS1 <- AR_SNVS1 %>%
  filter(id_localidad_carga == "3283") %>%
  mutate(fecha = as.Date(fecha),
         fis = as.Date(fis),
         fecha_lab = as.Date(fecha_lab), 
         fecha_carga = as.Date(fecha_carga)
         )



# -------- SNVS1: variables temporales con fecha mínima --------
AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    fecha_min = dplyr::case_when(
      is.na(fecha) & !is.na(fis) ~ fis,
      !is.na(fis) & !is.na(fecha) & dplyr::between(as.numeric(fecha - fis), 0, 30) ~ fis,
      TRUE ~ fecha
    ),
    fecha_min = dplyr::coalesce(fecha_min, fecha_lab),
    
    se     = lubridate::epiweek(fecha_min),
    mes    = lubridate::month(fecha_min),
    year   = lubridate::year(fecha_min),
    year_e = dplyr::if_else(se == 1 & mes == 12, year,
                            dplyr::if_else(se < 40, year - 1, year)),
    sistema = "SNVS_1.0",
    pais    = "argentina",
    distrito = "Puerto Iguazú"
  ) %>%
  select(
    fecha, fis, fecha_lab, fecha_min, re, classi_fin, sexo,
    test, resultado, test_validado, resul_validado, dato_labo,
    classi_validada, classi_validada_sosp,
     criterio,
    se, mes, year, year_e, sistema, pais, distrito, tipo_base
  )


# -------- SNVS2: variables temporales con fecha mínima --------
AR_SNVS2 <- AR_SNVS2 %>%
  mutate(
    fecha_min = dplyr::case_when(
      is.na(fecha) & !is.na(fis) ~ fis,
      !is.na(fis) & !is.na(fecha) & dplyr::between(as.numeric(fecha - fis), 0, 30) ~ fis,
      TRUE ~ fecha
    ),
    fecha_min = dplyr::coalesce(fecha_min, fecha_lab),
    
    se     = lubridate::epiweek(fecha_min),
    mes    = lubridate::month(fecha_min),
    year   = lubridate::year(fecha_min),
    year_e = dplyr::if_else(se == 1 & mes == 12, year,
                            dplyr::if_else(se < 40, year - 1, year)),
    sistema = "SNVS_2.0",
    pais    = "argentina",
    distrito = "Puerto Iguazú"
  ) %>%
  select(
    fecha, fis, fecha_lab, fecha_min, re, classi_fin, sexo,
    test, resultado, test_validado, resul_validado, dato_labo,
    classi_validada, classi_validada_sosp,
    serotipo, criterio,
    se, mes, year, year_e, sistema, pais, distrito, tipo_base
  )

# C2
AR_C2 <- AR_C2 %>%
  mutate(
    pais = "argentina",
    # normalizo sexo a F/M primero (si te viene como texto largo)
    sexo = toupper(substr(trimws(sexo), 1, 1)),
    sexo = case_when(
      sexo == "F" ~ "femenino",
      sexo == "M" ~ "masculino",
      TRUE ~ NA_character_
    ),
    fecha      = as.Date(fecha_consulta),
    fis        = as.Date(fecha_inicio_sintomas),
    fecha_lab  = NA_Date_,
    test       = NA_character_,
    resultado  = NA_character_,
    serotipo   = NA_character_,
    sistema    = "SNVS_1.0",
    distrito   = "Puerto Iguazú",
    classi_fin = tolower(criterio_epi),
    criterio   = "nexo",
    edad       = as.numeric(edad),
    re = case_when(
      edad < 15 ~ "menores",
      edad < 65 ~ "adultos",
      edad >= 65 ~ "ancianos",
      TRUE ~ NA_character_
    ),
    # fecha mínima (0–30 días)
    fecha_min = dplyr::case_when(
      is.na(fecha) & !is.na(fis) ~ fis,
      !is.na(fis) & !is.na(fecha) & dplyr::between(as.numeric(fecha - fis), 0, 30) ~ fis,
      TRUE ~ fecha
    ),
    fecha_min = dplyr::coalesce(fecha_min, fecha_lab),
    se     = lubridate::epiweek(fecha_min),
    mes    = lubridate::month(fecha_min),
    year   = lubridate::year(fecha_min),
    year_e = dplyr::if_else(se == 1 & mes == 12, year,
                            dplyr::if_else(se < 40, year - 1, year))
  ) %>%
  select(
    fecha, fis, fecha_lab, fecha_min, re, classi_fin, sexo,
    test, resultado, serotipo, criterio,
    se, mes, year, year_e, sistema, pais, distrito, tipo_base
  )


#Agrupados
den_ar_agrupados <- AR_Agrupados %>% 
  mutate(
    se = semana, 
    year = anio,
    year_e = ifelse(se >= 40, year, year - 1),
    mes = case_when(
      se %in% 1:4   ~ 1,  se %in% 5:8   ~ 2,  se %in% 9:13 ~ 3,
      se %in% 14:17 ~ 4,  se %in% 18:21 ~ 5,  se %in% 22:26 ~ 6,
      se %in% 27:30 ~ 7,  se %in% 31:35 ~ 8,  se %in% 36:39 ~ 9,
      se %in% 40:43 ~ 10, se %in% 44:48 ~ 11, se %in% 49:52 ~ 12
    ),
    criterio = "nexo", sistema = "SNVS_1.0", pais = "argentina",
    distrito = "Puerto Iguazú", classi_fin = "confirmado"
  ) %>%
  group_by(year, se, year_e, mes, criterio, sistema, pais, distrito, classi_fin, tipo_base) %>%
  summarise(
    menores = sum(`menorque1` + iguala1 + de2a4 + de5a9 + `de10a14`, na.rm = TRUE),
    adultos = sum(`de15a24` + de25a34 + de35a44 + `de45a64`, na.rm = TRUE), # <-- agrego de25a34
    ancianos = sum(`mayora65`, na.rm = TRUE),
    sin_especificacion = sum(sin_especificacion, na.rm = TRUE)               # <-- na.rm
  ) %>%
  ungroup() %>%
  mutate(tipo_base = "ar_c2_agrupado")

# Integración final de bases
den_ar_total <- bind_rows(AR_SNVS1, AR_SNVS2, AR_C2)
# Guarda CSV si lo deseas
write.csv2(den_ar_total, "source/csv/armo/dengue_ar_total.csv")
write.csv2(den_ar_agrupados, "source/csv/armo/dengue_ar_agrupados.csv")

# --- 6. PROCESAMIENTO DE PARAGUAY  ----

# Paraguay: validación espacial ----

# 6.1 Armonizar nombres de variables entre PY_Den1 y PY_Den2 
nombres_armonizados <- c(
  fecha_de_notificacion = "fecha",
  fecha_inicio_de_fiebre_sintomas = "fis",
  fecha_de_toma_de_muestra = "fecha_lab",
  clasificacion_final = "classi_fin",
  hospitalizado = "gravedad",
  sexo = "sexo",
  inmunocromatografico_ns1_dengue = "test_ns1",
  elisa_ig_m_dengue = "test_igm",
  pcr_dengue = "test_pcr",
  serotipo = "serotipo",
  criterio = "criterio",
  departamento = "departamento",
  distrito = "distrito",
  departamento_de_residencia = "departamento",
  distrito_de_residencia = "distrito"
)

den_py_total <- PY_Den1 %>%
  rename_with(~ if_else(.x %in% names(nombres_armonizados), nombres_armonizados[.x], .x), .cols = everything())

den_py_nuevo <- PY_Den2 %>%
  rename_with(~ if_else(.x %in% names(nombres_armonizados), nombres_armonizados[.x], .x), .cols = everything())
# 6.2 Generar variables de Hospitales
#No tengo los datos para validar en la base PY_Den1
#Base PY_Den2 traigo base de datos de Hospitales de Superintendencia, hago match para identificar, si es posible, el distrito del Hospital o centro asistencial
# Agregamos distrito según hospital notificador
#Vinculo con base realizada por centro notificador y tipo

lista_ordenada <- den_py_nuevo %>%
  filter(!is.na(establecimiento_notificante)) %>%
  mutate(establecimiento_sin_espacios = gsub(" ", "", establecimiento_notificante)) %>%
  distinct(establecimiento_notificante, establecimiento_sin_espacios)

# 6.3. Cargamos la base de hospitales y la preparamos
hospitales <- read.csv2(
  "C:/Users/pc/Google Drive/INMET/Vigifronteiras/DATOS/Paraguay/Salud/Hospitales_py_final_Compaginado_final.csv",
  fileEncoding = "latin1"
) %>%
  mutate(establecimiento_sin_espacios = gsub(" ", "", Notificado_Original)) %>%
  select(-1)  # elimina una columna innecesaria si la primera es redundante

hospitales <- hospitales |>
  mutate(
    establecimiento_sin_espacios = case_when(
      str_detect(establecimiento_sin_espacios, "HOSPITALGENERALPEDIA") ~ "HE HOSPITALGENERALPEDIATRICOACOSTANU",
      str_detect(establecimiento_sin_espacios, "USPTOPDTEFRANCO") ~ "IPS USPTOPDTEFRANCO",
      str_detect(establecimiento_sin_espacios, "JOSA") ~ "CS JOSEEULOGIOESTIGARRIBIA(CAMPO9)",
      str_detect(establecimiento_sin_espacios, "CONCEPCIA") ~ "HR CONCEPCION",
      str_detect(establecimiento_sin_espacios, "CARMENDELPARANA") ~ "CS CARMENDELPARANA",
      str_detect(establecimiento_sin_espacios, "QUEMADURAS.*CENQUER") ~ "CENTRONACIONALDEQUEMADURASYCIRUGIASRECONSTRUCTIVAS CENQUER",
      str_detect(establecimiento_sin_espacios, "SANTAFEDELPARANA") ~ "CS SANTAFEDELPARANA",
      str_detect(establecimiento_sin_espacios, "CAPITA.*NMIRANDA") ~ "USF CAPITANMIRANDA",
      str_detect(establecimiento_sin_espacios, "EMBY") ~ "HD NEMBY",
      str_detect(establecimiento_sin_espacios, "LEONMALLORQU") ~ "CS JUANLEONMALLORQUIN",
      str_detect(establecimiento_sin_espacios, "PA") & str_detect(establecimiento_sin_espacios, "F") ~ "P´F",
      str_detect(establecimiento_sin_espacios, "FUNDACIA") ~ "FUNDACIONTESAI",
      str_detect(establecimiento_sin_espacios, "MINGAGUAZ") ~ "HD MINGAGUAZÚ",
      TRUE ~ establecimiento_sin_espacios
    )
  )
# Hago función para el match
normalizar_nombre <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>       # Elimina tildes
    toupper() |>                               # Todo en mayúsculas
    gsub("-", " ", x = _) |>                   # Reemplaza guiones por espacios
    gsub("\\.", "", x = _) |>                  # Elimina puntos
    gsub(",", "", x = _) |>
    gsub("\\s+", " ", x = _) |>                # Compacta múltiples espacios
    trimws()                                   # Elimina espacios en extremos
}
# Aplico la función a ambas bases

lista_ordenada$establecimiento_norm <- normalizar_nombre(lista_ordenada$establecimiento_sin_espacios)
hospitales$establecimiento_norm <- normalizar_nombre(hospitales$establecimiento_sin_espacios)
# 6.4. Hacemos el join por nombre sin espacios
hospitales_match <- left_join(
  lista_ordenada,
  hospitales,
  by = "establecimiento_norm",
  relationship = "many-to-many"
)
# 6.5. Unimos info de hospitales a la base de dengue
den_py_nuevo <- left_join(den_py_nuevo, hospitales_match, by = "establecimiento_notificante")

# 6.6. Filtramos casos con información válida para el análisis
den_py_tabla_espacial <- den_py_nuevo %>%
  mutate(Notificacion_Alto_Parana = na_if(Notificacion_Alto_Parana, "")) %>%
  filter( clasificacion_para == "DENGUE")

# 6.7 Generamos tabla de validación espacial
tabla_municipio_Py <- tableby(
  distrito ~ Notificacion_Alto_Parana,
  data = den_py_tabla_espacial,
  cat.stats = c("Nmiss", "countpct"),
  cat.simplify = FALSE,
  cat.droplevels = FALSE,
  cat.test = "chisq",
  test = FALSE
)

tabla_municipio_Py_df <- as.data.frame(summary(tabla_municipio_Py, digits = 2, text = TRUE))
colnames(tabla_municipio_Py_df)[1] <- "Variables"

ft_py <- flextable(tabla_municipio_Py_df) %>%
  add_header_row(values = c("", "Notificación en Alto Paraná"), colwidths = c(1, ncol(tabla_municipio_Py_df) - 1)) %>%
  add_header_lines(values = "Tabla 1. Distribución de notificaciones por distrito en Alto Paraná") %>%
  set_table_properties(width = 0.5, layout = "autofit") %>%
  theme_booktabs()%>%
  footnote(i = 1, j = 1, value = as_paragraph("Los datos no identificados corresponden a siglas o nombres de establecimientos no reconocidos"))

# 6.8 Visualiza la tabla
ft_py
# Guarda la tabla como imagen
save_as_image(ft_py, path = "salida/articulo_1/tabla1_notificacion_py.png",
              webshot = "webshot2")

# Paraguay: Recodificación----

# Limpieza de edad y sexo

den_py_total <- den_py_total %>%
  mutate(
    edad = case_when(
      grepl("^[0-9]+$", as.character(edad)) ~ as.numeric(edad),  # Edad numérica válida
      grepl("[a-zA-Z]", as.character(edad)) ~ 0,                 # Letras (meses/días): menor de 1 año
      TRUE ~ NA_real_
    ),
    edad = na_if(edad, 999),  # 999 como NA
    sexo = case_when(
      sexo == "FEMENINO" ~ "femenino",
      sexo == "MASCULINO" ~ "masculino",
      TRUE ~ NA_character_
    )
  )

# Normalizar serotipos
den_py_total <- den_py_total %>%
  mutate(
    serotipo = case_when(
      serotipo %in% c("DEN - 1", "DEN-1") ~ "DEN1",
      serotipo %in% c("DEN - 2", "DENGUE-2", "DEN-2") ~ "DEN2",
      serotipo %in% c("DENGUE-4", "DEN-4") ~ "DEN4",
      TRUE ~ serotipo
    )
  )

# Conversión de fechas
den_py_total <- den_py_total %>%
  mutate(
    fecha = as.Date(fecha, format = "%Y-%m-%d"),
    fis = as.Date(fis, format = "%Y-%m-%d"),
    fecha_lab = as.Date(fecha_lab, format = "%Y-%m-%d")
  ) %>%
  mutate(
    fecha = na_if(fecha, as.Date("1902-09-25")),
    fis = na_if(fis, as.Date("1902-09-25")),
    fecha_lab = case_when(
      fecha_lab %in% as.Date(c("1900-01-01", "1900-01-06", "5710-05-01")) ~ NA_Date_,
      TRUE ~ fecha_lab
    )
  )


# Clasificación final y criterio
den_py_total <- den_py_total %>%
  mutate(
    classi_fin = case_when(
      classi_fin == "CONFIRMADO" ~ "confirmado",
      classi_fin == "DESCARTADO" ~ "descartado",
      classi_fin == "PROBABLE" ~ "confirmado",
      classi_fin == "SOSPECHOSO" ~ "sospechoso",
      TRUE ~ NA_character_
    ),
    criterio = case_when(
      criterio == "EPIDEMIOLOGICO" ~ "nexo",
      criterio == "LABORATORIO" ~ "laboratorio",
      TRUE ~ NA_character_
    )
  )

#  Clasificación por edad
den_py_total <- den_py_total %>%
  mutate(
    re = case_when(
      edad < 15 ~ "menores",
      edad >= 15 & edad < 65 ~ "adultos",
      edad >= 65 ~ "ancianos",
      TRUE ~ NA_character_
    )
  )


#Base Py Nuevo

den_py_nuevo <- den_py_nuevo %>%
  mutate( fecha = case_when(
    grepl("^[0-9]+$", `fecha_de_notificacion_primera_consulta`) ~ 
      as.Date(as.numeric(`fecha_de_notificacion_primera_consulta`), origin = "1899-12-30"),
    grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", `fecha_de_notificacion_primera_consulta`) ~ 
      dmy(`fecha_de_notificacion_primera_consulta`),
    TRUE ~ NA_Date_
  ),
  fis = case_when(
    grepl("^[0-9]+$", `fecha_inicio_de_sintomas`) ~ 
      as.Date(as.numeric(`fecha_inicio_de_sintomas`), origin = "1899-12-30"),
    grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", `fecha_inicio_de_sintomas`) ~ 
      dmy(`fecha_inicio_de_sintomas`),
    TRUE ~ NA_Date_
  ),
  fecha_lab = case_when(
    grepl("^[0-9]+$", `toma_de_muestra`) ~ 
      as.Date(as.numeric(`toma_de_muestra`), origin = "1899-12-30"),
    grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", `toma_de_muestra`) ~ 
      dmy(`toma_de_muestra`),
    TRUE ~ NA_Date_
  )
  )%>% filter(clasificacion_para == "DENGUE")



# Armonizar edad y criterio
den_py_nuevo <- den_py_nuevo %>%
  mutate(
    edad3 = as.numeric(sub("\\D.*", "", edad)),
    edad2 = sub("\\d+", "", edad),
    edad = case_when(
      grepl("AÑO", edad2, ignore.case = TRUE) ~ edad3,
      grepl("DIA", edad2, ignore.case = TRUE) ~ 0,
      grepl("MES", edad2, ignore.case = TRUE) & edad3 < 12 ~ 0,
      grepl("MES", edad2, ignore.case = TRUE) & edad3 >= 12 & edad3 <= 24 ~ 1,
      grepl("MES", edad2, ignore.case = TRUE) & edad3 > 24 ~ edad3 %/% 12,
      grepl("SD", edad2, ignore.case = TRUE) ~ NA_real_,
      grepl("999", edad3, ignore.case = TRUE) ~ NA_real_,
      TRUE ~ edad3
    ),
    criterio = if_else(is.na(test_ns1) & is.na(test_igm) & is.na(test_pcr), "nexo", "laboratorio"),
    year = as.numeric(ano),
    re = case_when(
      edad < 15 ~ "menores",
      edad < 65 ~ "adultos",
      edad >= 65 ~ "ancianos",
      TRUE ~ NA_character_
    ))


# Validar test y asignar distrito de notificación
den_py_nuevo <- den_py_nuevo %>%
  mutate(
    noti_distrito = case_when(
      Notificacion_CDE == "si" ~ "CDE",
      Notificacion_PF == "si" ~ "PF",
      Notificacion_HERNANDARIAS == "si" ~ "HERNANDARIAS",
      Notificacion_MINGA_GUAZÚ == "si" ~ "MG"
    ),
    classi_fin = case_when(
      classi_fin == "CONFIRMADO" ~ "confirmado",
      classi_fin == "DESCARTADO" ~ "descartado",
      classi_fin == "PROBABLE" ~ "confirmado",
      classi_fin == "SOSPECHOSO" ~ "sospechoso",
      TRUE ~ NA_character_
    )
  )

# --- Validación temporal del test: den_py_nuevo ---
den_py_nuevo <- den_py_nuevo %>%
  mutate(
    ns1_raw = trimws(toupper(as.character(test_ns1))),
    igm_raw = trimws(toupper(as.character(test_igm))),
    pcr_raw = trimws(toupper(as.character(test_pcr))),
    dias_lab = as.numeric(fecha_lab - fis),
    
    test_validado = case_when(
      igm_raw == "NEGATIVO" & !is.na(dias_lab) & dias_lab >= 7 ~ "igm",
      pcr_raw %in% c("POSITIVO","DENGUE","DETECTABLE") & !is.na(dias_lab) & dias_lab <= 7 ~ "pcr",
      ns1_raw == "POSITIVO" & !is.na(dias_lab) & dias_lab <= 7 ~ "ns1",
      igm_raw == "POSITIVO" & !is.na(dias_lab) & dias_lab > 5 ~ "igm",
      pcr_raw == "NEGATIVO" ~ "pcr",
      ns1_raw == "NEGATIVO" ~ "ns1",
      TRUE ~ NA_character_
    ),
    
    resul_validado = case_when(
      igm_raw == "NEGATIVO" & !is.na(dias_lab) & dias_lab >= 7 ~ "negativo",
      pcr_raw %in% c("POSITIVO","DENGUE","DETECTABLE") & !is.na(dias_lab) & dias_lab <= 7 ~ "positivo",
      ns1_raw == "POSITIVO" & !is.na(dias_lab) & dias_lab <= 7 ~ "positivo",
      igm_raw == "POSITIVO" & !is.na(dias_lab) & dias_lab > 5 ~ "positivo",
      pcr_raw == "NEGATIVO" ~ "negativo",
      ns1_raw == "NEGATIVO" ~ "negativo",
      TRUE ~ NA_character_
    ),
    
    dato_labo = case_when(
      igm_raw == "NEGATIVO" & !is.na(dias_lab) & dias_lab >= 7 ~ "descartado",
      (igm_raw == "POSITIVO" & !is.na(dias_lab) & dias_lab > 5) |
        (ns1_raw == "POSITIVO" & !is.na(dias_lab) & dias_lab <= 5) |
        (pcr_raw %in% c("POSITIVO","DENGUE","DETECTABLE") & !is.na(dias_lab) & dias_lab <= 5) ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b",
      TRUE ~ NA_character_
    ),
    
    classi_validada_sosp = case_when(
      dato_labo %in% c("confirmado","sospechoso") & classi_fin %in% c("confirmado","sospechoso") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo %in% c("confirmado","sospechoso") & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado","sospechoso") ~ "b",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-ns1_raw, -igm_raw, -pcr_raw)

# Selección final y campos derivados
den_py_nuevo <- den_py_nuevo %>% filter (Notificacion_Alto_Parana != "no", clasificacion_para == "DENGUE") %>%
  select(
    centro = Notificado_modificado,
    Sistema,
    noti_distrito,
    sexo,
    distrito,
    gravedad,
    test_pcr,
    test_ns1,
    test_igm,
    classi_fin,
    fecha,
    fis,
    fecha_lab,
    edad,
    criterio, dato_labo, classi_validada, classi_validada_sosp, 
    test_validado,
    resul_validado,
    ano, tipo_base, re
  ) %>%
  mutate(across(
    c(centro, Sistema,  noti_distrito, sexo, distrito, gravedad,
      test_pcr, test_ns1, test_igm, classi_fin, criterio, test_validado, re),
    as.factor
  )) 



# 6.3 Unión de Bases
den_py_final <- bind_rows(den_py_total, den_py_nuevo)

# Limpieza final en den_py_final (fechas incoherentes, recodificaciones, selección de variables)
den_py_final <- den_py_final %>%
  mutate(
    fecha = case_when(fecha == as.Date("1902-09-25") ~ NA_Date_, TRUE ~ fecha),
    fis = case_when(fis == as.Date("1902-09-25") ~ NA_Date_, TRUE ~ fis),
    fecha_lab = case_when(fecha_lab <= as.Date("1920-12-21") ~ NA_Date_,
                          fecha_lab >= as.Date("2024-12-21") ~ NA_Date_,
                          TRUE ~ fecha_lab),
    sexo = case_when(sexo == "SD" ~ NA_character_,
                     sexo == "INDETERMINADO" ~ NA_character_,
                     sexo == "FEMENINO" ~ "femenino",
                     sexo == "MASCULINO" ~ "masculino",
                     TRUE ~ sexo),
    classi_fin = case_when(classi_fin == "descartado" ~ "descartado", TRUE ~ classi_fin),
    criterio = case_when(criterio == "laboratorio" ~ "laboratorio",
                         criterio == "epidemiologico" ~ "nexo",
                         TRUE ~ criterio),
    edad = if_else(edad == 999, NA_real_, edad),
    fecha_carga = as.Date(fecha),
    sistema = "dgvs",
    pais    = "paraguay",

    # -------- fecha mínima (0–30 días) --------
    fecha_min = dplyr::case_when(
      is.na(fecha) & !is.na(fis) ~ fis,
      !is.na(fis) & !is.na(fecha) & dplyr::between(as.numeric(fecha - fis), 0, 30) ~ fis,
      TRUE ~ fecha
    ),
    fecha_min = dplyr::coalesce(fecha_min, fecha_lab),
    test = test_validado, resultado = resul_validado,

    se     = lubridate::epiweek(fecha_min),
    mes    = lubridate::month(fecha_min),
    year   = lubridate::year(fecha_min),
    year_e = dplyr::if_else(se == 1 & mes == 12, year,
                            dplyr::if_else(se < 40, year - 1, year))
  ) %>%
  select(
    fecha, fecha_carga, fis, fecha_lab, fecha_min, re, classi_fin, sexo, criterio,
    distrito, se, mes, year, year_e,
    sistema, pais, serotipo, 
    centro, noti_distrito, gravedad, test_pcr, test_ns1, test_igm,
    test_validado, resul_validado,   dato_labo, classi_validada, classi_validada_sosp, test, resultado,
    tipo_base
  ) 

write.csv2(den_py_final, "source/csv/armo/dengue_py_total.csv")

# Unión total de todas las bases
dengue_total <- bind_rows(den_py_final, den_br_total, den_ar_total) %>%
  filter(year_e > 2008, year_e < 2023) %>% select(-c(centro, noti_distrito, gravedad, test_pcr, test_ns1, test_igm))

write.csv2(dengue_total, "source/csv/armo/dengue_total.csv")
