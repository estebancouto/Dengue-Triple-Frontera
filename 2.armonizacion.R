# Armonización de datos epidemiológicos en la Triple Frontera
# Autor: Esteban M. Couto
# Fecha: 2025‑09‑25

# --- 1. CARGA DE LIBRERIAS ----
# Se cargan las librerías necesarias para manipulación de datos, fechas y tablas
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2
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
      classi_fin %in% c("1", "2", "3", "4") ~ "Confirmado",
      classi_fin == "5" ~ "Descartado",
      classi_fin == "8" ~ "Sospechoso",
      TRUE ~ NA_character_
    ),
    infeccion = case_when(
      comuninf == "410830" ~ "Si",
      comuninf != "410830" ~ "No",
      TRUE ~ NA_character_
    )
  ) %>% 
  select (residentes, notificacion, classi_fin, infeccion)

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
      classi_fin == "10" ~ "Confirmado",
      classi_fin == "11" ~ "Confirmado",
      classi_fin == "12" ~ "Confirmado",
      classi_fin == "5" ~ "Descartado",
      classi_fin == "8" ~ "Sospechoso",
      TRUE ~ NA_character_
    ),
    infeccion = case_when(
      comuninf == "410830" ~ "Si",
      comuninf != "410830" ~ "No",
      TRUE ~ NA_character_
    )
  )%>% 
  select (residentes, notificacion, classi_fin, infeccion)
# --- 4.3 Unifica ambas bases
municipios_df <- bind_rows(municipios_df_1, municipios_df_2)

# --- 4.4 Tabla de contingencia: ¿quiénes fueron notificados en Foz?
tabla_municipio_br <- tableby(
  notificacion ~ residentes + infeccion,
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

# 4.6 Filtrar sólo residentes en Foz do Iguaçu (IBGE: 410830)
br_den_09_13 <- BR_Den_09_13 %>% filter(id_mn_resi == "410830")
br_den_14_23 <- BR_Den_14_23 %>% filter(id_mn_resi == "410830")

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
      classi_fin %in% c("1", "2", "3", "4") ~ "Confirmado",
      classi_fin == "5" ~ "Descartado",
      classi_fin == "8" ~ "Sospechoso"
    ),
    
    # Tipo de test aplicado
    test = case_when(
      resul_ns1 %in% c("1", "2") ~ "NS1",
      resul_soro %in% c(1, 2) ~ "IgM",
      resul_pcr %in% c(1, 2) ~ "PCR"
    ),
    
    # Resultado del test
    resultado = case_when(
      resul_ns1 == "1" ~ "Positivo",
      resul_ns1 == "2" ~ "Negativo",
      resul_soro == 1 ~ "Positivo",
      resul_soro == 2 ~ "Negativo",
      resul_pcr == 1 ~ "Positivo",
      resul_pcr == 2 ~ "Negativo"
    ),
    
    # Sexo del caso
    sexo = case_when(
      cs_sexo == "F" ~ "Femenino",
      cs_sexo == "M" ~ "Masculino"
    ),
    
    # Serotipo identificado
    serotipo = case_when(
      sorotipo == "1" ~ "DEN1",
      sorotipo == "2" ~ "DEN2",
      sorotipo == "4" ~ "DEN4"
    ),
    
    # Criterio de confirmación
    criterio = case_when(
      criterio == "1" ~ "Laboratorio",
      criterio == "2" ~ "Nexo",
      criterio == "3" ~ NA_character_
    ),
    
    # Fechas principales
    fecha = dt_notific,
    fis = dt_sin_pri,
    fecha_lab = coalesce(dt_ns1, dt_viral, dt_pcr, dt_soro),
    demora_cons = dt_notific - dt_sin_pri
    )

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
      classi_fin %in% c("10", "11", "12") ~ "Confirmado",
      classi_fin == "5" ~ "Descartado",
      classi_fin == "8" ~ "Sospechoso"
    ),
    
    test = case_when(
      resul_ns1 %in% c(1, 2) ~ "NS1",
      resul_soro %in% c(1, 2) ~ "IgM",
      resul_pcr %in% c(1, 2) ~ "PCR"
    ),
    
    resultado = case_when(
      resul_ns1 == 1 ~ "Positivo",
      resul_ns1 == 2 ~ "Negativo",
      resul_soro == 1 ~ "Positivo",
      resul_soro == 2 ~ "Negativo",
      resul_pcr == 1 ~ "Positivo",
      resul_pcr == 2 ~ "Negativo"
    ),
    
    sexo = case_when(
      cs_sexo == "F" ~ "Femenino",
      cs_sexo == "M" ~ "Masculino"
    ),
    
    serotipo = case_when(
      sorotipo == "1" ~ "DEN1",
      sorotipo == "2" ~ "DEN2",
      sorotipo == "4" ~ "DEN4"
    ),
    
    criterio = case_when(
      criterio == "1" ~ "Laboratorio",
      criterio == "2" ~ "Nexo",
      criterio == "3" ~ NA_character_
    ),
    
    fecha = dt_notific,
    fis = dt_sin_pri,
    fecha_lab = coalesce(dt_ns1, dt_pcr, dt_soro, dt_prnt),
    demora_cons = dt_notific - dt_sin_pri
    
  )

# 4.10 Consolidación y salida final

## SINAN 2
br_den_09_13_total <- br_den_09_13 %>%
  mutate(
    sistema = "sinan.2",
    pais = "brasil",
    re = re_br
  ) %>%
  select(
    fecha, fis, fecha_carga, fecha_lab, re, classi_fin, gravedad, sexo,
    test, resultado, serotipo, criterio, sistema, pais
  )

## SINAN 3
br_den_14_23_total <- br_den_14_23 %>%
  mutate(
    sistema = "sinan.3",
    pais = "brasil",
    re = re_br,
    fecha_carga = NA  # No disponible en esta base
  ) %>%
  select(
    fecha, fis, fecha_carga, fecha_lab, re, classi_fin, gravedad, sexo,
    test, resultado, serotipo, criterio, sistema, pais
  )

## 4.11 Unión final
den_br_total <- bind_rows(br_den_09_13_total, br_den_14_23_total)

### 4.12 Construcción de variables temporales por fecha minima
den_br_total<- den_br_total %>%
  mutate(
    se = ifelse(is.na(fis),
                ifelse(is.na(fecha_lab), epiweek(fecha), epiweek(fecha_lab)),
                epiweek(fis)),
    mes = ifelse(is.na(fis),
                 ifelse(is.na(fecha_lab), month(fecha), month(fecha_lab)),
                 month(fis)),
    year = ifelse(is.na(fis),
                  ifelse(is.na(fecha_lab), year(fecha), year(fecha_lab)),
                  year(fis)),
    year_e = ifelse(se >= 40, as.character(year), as.character(year - 1))
  )


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
  footnote(i = 1, j = 1, value = as_paragraph("No se consideran las bases vinculadas al C2 por no tener datos de residencia"))

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
  footnote(i = 1, j = 1, value = as_paragraph("No se consideran las bases vinculadas al C2 por no tener datos de residencia"))

# 5.7 Visualiza la tabla3
ft3

# Guarda la tabla como imagen

save_as_image(ft3, path = "salida/articulo_1/tabla3_notificacion_igr_snvs.png",
              webshot = "webshot2")  

# Argentina: Recodificación ----

# 5.8 SNVS 1
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
    
    # Recodificación por edad a partir de fechas
    re = case_when(
      is.na(fis) & difftime(fecha_lab, fn, units = "days") < 365 * 15 ~ "menores",
      is.na(fis) & difftime(fecha_lab, fn, units = "days") < 365 * 65 ~ "adultos",
      is.na(fis) ~ "ancianos",
      !is.na(fis) & difftime(fis, fn, units = "days") < 365 * 15 ~ "menores",
      !is.na(fis) & difftime(fis, fn, units = "days") < 365 * 65 ~ "adultos",
      TRUE ~ "ancianos"
    ),
    
    # Clasificación por tipo y tiempo de laboratorio
    dato_labo = case_when(
      difftime(fecha_lab, fis, units = "days") >= 7 & test == "IgM" & resultado == "negativo" ~ "descartado",
      difftime(fecha_lab, fis, units = "days") > 5 & test == "IgM" & resultado == "positivo" ~ "confirmado",
      difftime(fecha_lab, fis, units = "days") <= 5 & test %in% c("PCR", "NS1") & resultado == "positivo" ~ "confirmado",
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
    )
  )

# Limpieza de fechas incoherentes
AR_SNVS1[AR_SNVS1 <= "1900-01-01"] <- NA
AR_SNVS1[AR_SNVS1 == ""] <- NA


# 5.9 SNVS 2 
# Clasificación y recodificación
AR_SNVS2 <- AR_SNVS2 %>%
  filter(id_loc_indec_carga == "54063030") %>%
  mutate(
    test = case_when(
      determinacion == "Antígeno NS1" ~ "ns1",
      determinacion == "Genoma viral" ~ "pcr",
      determinacion == "IgM DENV" ~ "igm"
    ),
    prueba = case_when(
      tecnica %in% c("ELISA NS1", "RT-qPCR", "MAC-ELISA IgM", "ELISA IgM comercial") ~ test
    ),
    serotipo = case_when(
      resultado == "DEN-1" ~ "DEN1",
      resultado == "DEN-2" ~ "DEN2",
      resultado == "DEN-4" ~ "DEN4"
    ),
    resultado = case_when(
      resultado %in% c("DEN-1", "DEN-2", "DEN-4", "Positivo") ~ "positivo",
      resultado %in% c("Negativo", "No detectable") ~ "negativo"
    ),
    fecha = as.Date(fecha_apertura, format = "%d/%m/%Y"),
    fis = as.Date(fis, format = "%d/%m/%Y"),
    fecha_lab = as.Date(ftm, format = "%d/%m/%Y")
  ) %>%
  mutate(
    classi_fin = case_when(
      clasificacion_manual %in% c("caso confirmado den-1", "caso confirmado den-2", "caso confirmado den-4",
                                  "caso confirmado por nexo epidemiológico autóctono", "caso confirmado por nexo epidemiológico importado",
                                  "caso confirmado sin serotipo", "caso probable") ~ "confirmado",
      clasificacion_manual %in% c("caso descartado", "caso descartado por epidemiología") ~ "descartado",
      clasificacion_manual %in% c("caso sospechoso", "caso sospechoso no conclusivo") ~ "sospechoso"
    ),
    criterio = case_when(
      clasificacion_manual %in% c("caso confirmado den-1", "caso confirmado den-2", "caso confirmado den-4",
                                  "caso confirmado sin serotipo", "caso probable", "caso sospechoso no conclusivo") ~ "laboratorio",
      clasificacion_manual %in% c("caso confirmado por nexo epidemiológico autóctono", "caso confirmado por nexo epidemiológico importado",
                                  "caso descartado por epidemiología", "caso sospechoso") ~ "nexo",
      clasificacion_manual == "caso descartado" ~ "laboratorio"
    ),
    temporalidad = as.numeric(difftime(fecha, fis, units = "days")),
    re = case_when(
      edad_diagnostico < 15 ~ "menores",
      edad_diagnostico < 65 ~ "adultos",
      TRUE ~ "ancianos"
    ),
    sexo = case_when(
      sexo == "F" ~ "femenino",
      sexo == "M" ~ "masculino"
    ),
    dato_labo = case_when(
      difftime(fecha_lab, fis, units = "days") >= 7 & test == "igm" & resultado == "negativo" ~ "descartado",
      difftime(fecha_lab, fis, units = "days") > 5 & test == "igm" & resultado == "positivo" ~ "confirmado",
      difftime(fecha_lab, fis, units = "days") <= 5 & test %in% c("pcr", "ns1") & resultado == "positivo" ~ "confirmado",
      TRUE ~ "sospechoso"
    ),
    classi_validada = case_when(
      dato_labo == "confirmado" & classi_fin == "confirmado" ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin == "confirmado" ~ "b"
    ),
    classi_validada_sosp = case_when(
      dato_labo == "confirmado" & classi_fin %in% c("confirmado", "sospechoso") ~ "a",
      dato_labo == "descartado" & classi_fin == "descartado" ~ "d",
      dato_labo == "confirmado" & classi_fin == "descartado" ~ "c",
      dato_labo == "sospechoso" & classi_fin == "descartado" ~ "c",
      dato_labo == "descartado" & classi_fin %in% c("confirmado", "sospechoso") ~ "b"
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


#SNVS2

AR_SNVS2 <- AR_SNVS2 %>%
  filter(id_loc_indec_carga == "54063030") %>%
  mutate( serotipo = case_when(
    resultado == "DEN-1" ~ "DEN1",
    resultado == "DEN-2" ~ "DEN2",
    resultado == "DEN-4" ~ "DEN4"
  ),
  fecha = as.Date(fecha),
  fis = as.Date(fis),
  fecha_lab = as.Date(fecha_lab),
  fecha_carga = as.Date(fecha_carga)
  
  )

#variables temporales
AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    se = ifelse(is.na(fis), ifelse(is.na(fecha_lab), epiweek(fecha), epiweek(fecha_lab)), epiweek(fis)),
    mes = ifelse(is.na(fis), ifelse(is.na(fecha_lab), month(fecha), month(fecha_lab)), month(fis)),
    year = ifelse(is.na(fis), ifelse(is.na(fecha_lab), year(fecha), year(fecha_lab)), year(fis)),
    year_e = ifelse(se >= 40, as.character(year), as.character(year - 1))
  )

AR_SNVS2 <- AR_SNVS2 %>%
  mutate(
    se = ifelse(is.na(fis), ifelse(is.na(fecha_lab), epiweek(fecha), epiweek(fecha_lab)), epiweek(fis)),
    mes = ifelse(is.na(fis), ifelse(is.na(fecha_lab), month(fecha), month(fecha_lab)), month(fis)),
    year = ifelse(is.na(fis), ifelse(is.na(fecha_lab), year(fecha), year(fecha_lab)), year(fis)),
    year_e = ifelse(se >= 40, as.character(year), as.character(year - 1))
  )

# C2
AR_C2 <- AR_C2 %>%
  mutate(
    fecha_lab = NA_Date_,
    test = NA_character_,
    resultado = NA_character_,
    serotipo = NA_character_,
    sistema = "SNVS_1.0",
    distrito = "Puerto Iguazú",
    fecha = as.Date(fecha_consulta),
    fis = as.Date(fecha_inicio_sintomas),
    sexo = str_to_title(sexo),
    classi_fin = criterio_epi,
    criterio = "nexo",
    edad = as.numeric(edad),
    re = case_when(
      edad < 15 ~ "menores",
      edad < 65 ~ "adultos",
      TRUE ~ "ancianos"
    ),
    se = ifelse(!is.na(fis), epiweek(fis), epiweek(fecha)),
    mes = ifelse(!is.na(fis), month(fis), month(fecha)),
    year = ifelse(!is.na(fis), year(fis), year(fecha)),
    year_e = ifelse(se >= 40, as.character(year), as.character(year - 1))
  ) %>%
  select(
    fecha, fis, fecha_lab, re, classi_fin, sexo,
    test, resultado, serotipo, criterio,
    se, mes, year, year_e, sistema, pais, distrito
  )

#Agrupados
den_ar_agrupados <- AR_Agrupados %>% mutate(se= semana, 
                                        year=anio,
                                        year_e = ifelse(se >= 40, as.character(year), as.character(year - 1)),
                                        mes = case_when(se %in% c(1:4) ~ 1,
                                                        se %in% c(5:8) ~ 2,
                                                        se %in% c(9:13) ~ 3,
                                                        se %in% c(14:17) ~ 4,
                                                        se %in% c(18:21) ~ 5,
                                                        se %in% c(22:26) ~ 6,
                                                        se %in% c(27:30) ~ 7,
                                                        se %in% c(31:35) ~ 8,
                                                        se %in% c(36:39) ~ 9,
                                                        se %in% c(40:43) ~ 10,
                                                        se %in% c(44:48) ~ 11,
                                                        se %in% c(49:52) ~ 12),
                                        menores = sum(`menorque1`+ iguala1 + de2a4+ de5a9+ `de10a14`),
                                        adultos = sum(`de15a24`+ de35a44+ `de45a64`),
                                        ancianos = sum(`mayora65`),
                                        total = sum(menores+adultos+ancianos+ sin_especificacion),
                                        criterio = "nexo",
                                        sistema = "SNVS_1.0",
                                        pais = "argentina",
                                        distrito = "Puerto Iguazú",
                                        classi_fin= "confirmado") %>% 
                                select(se, year, year_e, mes, menores, adultos, ancianos, total, criterio, sistema, pais, distrito, classi_fin)

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
  fileEncoding = "ISO-8859-1"
) %>%
  mutate(establecimiento_sin_espacios = gsub(" ", "", Notificado_Original)) %>%
  select(-1)  # elimina una columna innecesaria si la primera es redundante

# 6.4. Hacemos el join por nombre sin espacios
hospitales_match <- left_join(lista_ordenada, hospitales, by = "establecimiento_sin_espacios")

# 6.5. Unimos info de hospitales a la base de dengue
den_py_nuevo <- left_join(den_py_nuevo, hospitales_match, by = "establecimiento_notificante")

# 6.6. Filtramos casos con información válida para el análisis
den_py_tabla_espacial <- den_py_nuevo %>%
  mutate(Notificacion_Alto_Parana = na_if(Notificacion_Alto_Parana, " ")) %>%
  filter(!is.na(Notificacion_Alto_Parana), clasificacion_para == "DENGUE")

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
  theme_booktabs()

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
      sexo == "FEMENINO" ~ "Femenino",
      sexo == "MASCULINO" ~ "Masculino",
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

#  Semana epidemiológica, mes, año
den_py_total <- den_py_total %>%
  mutate(
    se = ifelse(is.na(fis),
                ifelse(is.na(fecha_lab), epiweek(fecha), epiweek(fecha_lab)),
                epiweek(fis)),
    mes = ifelse(is.na(fis),
                 ifelse(is.na(fecha_lab), month(fecha), month(fecha_lab)),
                 month(fis)),
    year = as.numeric(ano),
    year_e = ifelse(se >= 40, as.character(year), as.character(year - 1))
  )

# Clasificación final y criterio
den_py_total <- den_py_total %>%
  mutate(
    classi_fin = case_when(
      classi_fin == "CONFIRMADO" ~ "Confirmado",
      classi_fin == "DESCARTADO" ~ "Descartado",
      classi_fin == "PROBABLE" ~ "Confirmado",
      classi_fin == "SOSPECHOSO" ~ "Sospechoso",
      TRUE ~ NA_character_
    ),
    criterio = case_when(
      criterio == "EPIDEMIOLOGICO" ~ "Nexo",
      criterio == "LABORATORIO" ~ "Laboratorio",
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
      TRUE ~ edad3
    ),
    criterio = if_else(is.na(test_ns1) & is.na(test_igm) & is.na(test_pcr), "Nexo", "Laboratorio"),
    year = as.numeric(ano)
  )

# Validar test y asignar distrito de notificación
den_py_nuevo <- den_py_nuevo %>%
  mutate(
    test_validado = case_when(
      test_igm == "NEGATIVO" & difftime(fecha_lab, fis, units = "days") >= 7 ~ "IgM",
      test_pcr == "DENGUE" & difftime(fecha_lab, fis, units = "days") <= 7 ~ "PCR",
      test_ns1 == "POSITIVO" & difftime(fecha_lab, fis, units = "days") <= 7 ~ "NS1",
      test_igm == "POSITIVO" & difftime(fecha_lab, fis, units = "days") > 5 ~ "IgM",
      test_pcr == "NEGATIVO" ~ "PCR",
      test_ns1 == "NEGATIVO" ~ "NS1",
      TRUE ~ NA_character_
    ),
    noti_distrito = case_when(
      Notificacion_CDE == "si" ~ "CDE",
      Notificacion_PF == "si" ~ "PF",
      Notificacion_HERNANDARIAS == "si" ~ "HERNANDARIAS",
      Notificacion_MINGA_GUAZÚ == "si" ~ "MG"
    ),
    classi_fin = case_when(
      classi_fin == "CONFIRMADO" ~ "Confirmado",
      classi_fin == "PROBABLE" ~ "Confirmado",
      classi_fin == "DESCARTADO" ~ "Descartado",
      classi_fin == "SOSPECHOSO" ~ "Sospechoso"
    )
  )

# Validar resultado
den_py_nuevo <- den_py_nuevo %>%
  mutate(
    resul_validado = case_when(
      test_igm == "NEGATIVO" & difftime(fecha_lab, fis, units = "days") >= 7 ~ "Negativo",
      test_pcr == "DENGUE" & difftime(fecha_lab, fis, units = "days") <= 7 ~ "Positivo",
      test_ns1 == "POSITIVO" & difftime(fecha_lab, fis, units = "days") <= 7 ~ "Positivo",
      test_igm == "POSITIVO" & difftime(fecha_lab, fis, units = "days") > 5 ~ "Positivo",
      test_pcr == "NEGATIVO" ~ "Negativo",
      test_ns1 == "NEGATIVO" ~ "Negativo",
      TRUE ~ NA_character_
    )
  )

# Selección final y campos derivados
den_py_nuevo <- den_py_nuevo %>% filter (Notificacion_Alto_Parana == "si", clasificacion_para == "DENGUE") %>%
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
    criterio,
    test_validado,
    resul_validado,
    ano
  ) %>%
  mutate(
    se = if_else(is.na(fis), if_else(is.na(fecha_lab), epiweek(fecha), epiweek(fecha_lab)), epiweek(fis)),
    mes = if_else(is.na(fis), if_else(is.na(fecha_lab), month(fecha), month(fecha_lab)), month(fis)),
    year_e = if_else(se >= 40, as.character(ano), as.character(as.numeric(ano) - 1))
  ) %>%
  mutate(across(
    c(centro, Sistema,  noti_distrito, sexo, distrito, gravedad,
      test_pcr, test_ns1, test_igm, classi_fin, criterio, test_validado),
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
    sexo = case_when(sexo == "sd" ~ NA_character_,
                     sexo == "femenino" ~ "Femenino",
                     sexo == "masculino" ~ "Masculino",
                     TRUE ~ sexo),
    classi_fin = case_when(classi_fin == "descartado" ~ "Descartado", TRUE ~ classi_fin),
    criterio = case_when(criterio == "laboratorio" ~ "Laboratorio",
                         criterio == "epidemiologico" ~ "Nexo",
                         TRUE ~ criterio),
    edad = if_else(edad == 999, NA_real_, edad),
    fecha_carga=as.Date(fecha)
  ) %>%
  mutate(sistema = "dgvs", pais = "paraguay") %>%
  select(
    fecha, fecha_carga, fis, fecha_lab, re, classi_fin, sexo, criterio,
    distrito, se, mes, year, year_e,
    sistema, pais, serotipo, 
    centro, noti_distrito, gravedad, test_pcr, test_ns1, test_igm,
    test_validado, resul_validado
  ) %>%
  rename(test = test_validado, resultado = resul_validado)

write.csv2(den_py_final, "source/csv/armo/dengue_py_total.csv")

# Unión total de todas las bases
dengue_total <- bind_rows(den_py_final, den_br_total, den_ar_total) %>%
  filter(year_e > 2008, year_e < 2023)

write.csv2(dengue_total, "source/csv/armo/dengue_total.csv")
