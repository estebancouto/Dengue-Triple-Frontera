#Salida de resultados para armonización
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2, tableone, 
  RColorBrewer, scales, gt, data.table, gridExtra, cowplot
)



setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")
#Cargo bases ----
dengue_ar_agrupados<-read.csv2("source/csv/armo/dengue_ar_agrupados.csv")
dengue_ar_total<-read.csv2("source/csv/armo/dengue_ar_total.csv")
den_py_total<-read.csv2("source/csv/armo/dengue_py_total.csv")
den_br_total<-read.csv2("source/csv/armo/dengue_br_total.csv")
dengue_total<-read.csv2("source/csv/armo/dengue_total.csv")

#Acomodo variables
dengue_total <- dengue_total %>%
  mutate(
    across(c(fecha, fecha_carga, fis, fecha_lab), as.Date),
    across(where(is.character), as.factor)
  )

summary(dengue_total)


summary(dengue_total)

# Agrupo por año y semana epidemiológica
dengue_resumen <- dengue_total %>%
  group_by( year_e,year, se, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado, tipo_base) %>%
  summarise(
    casos = n(),
    .groups = "drop"
  )

summary(dengue_resumen)

# Agrego la base de agrupados

ar_long <- den_ar_agrupados %>%
  pivot_longer(cols = c(menores, adultos, ancianos, sin_especificacion),
               names_to = "re",
               values_to = "casos") %>%
  mutate(
    re = if_else(re == "sin_especificacion", NA_character_, re)
  ) %>%
  select(year_e,year, se, re, casos, criterio, sistema, pais, distrito, classi_fin, tipo_base)

dengue_completo <- bind_rows(
  dengue_resumen,
  ar_long
)

# Tabla 1 resumen de bases ----

# paso tabla a casos
dengue_expandido <- dengue_completo %>%
  uncount(weights = casos) %>% 
  mutate ("año epidemiológico"= as.factor(paste0(year_e, "-", year_e + 1)))%>%
  mutate(
    tipo_base = case_when(
      tipo_base == "ar_c2_agrupado"   ~ "Argentina\nC2 Agrupado",
      tipo_base == "ar_c2_nominal"    ~ "Argentina\nC2 Nominal",
      tipo_base == "ar_sivila_nominal"~ "Argentina\nSIVILA Nominal",
      tipo_base == "ar_snvs2"         ~ "Argentina\nSNVS 2.0",
      tipo_base == "br_09_13"         ~ "Brasil\n2009-2013",
      tipo_base == "br_14_23"         ~ "Brasil\n2014-2023",
      tipo_base == "py_09_20"         ~ "Paraguay\n2009-2020",
      tipo_base == "py_21_23"         ~ "Paraguay\n2021-2023",
      TRUE ~ tipo_base),
    re = if_else(re == "ancianos", "adultos mayores", re),
    classi_fin = ifelse(is.na(classi_fin), "No Clasificado", classi_fin),
    test = as.character(test),  
    test = case_when(
      test == "igm" ~ "Serología",
      test == "ns1" ~ "Antígeno NS1",
      test == "pcr" ~ "RT-PCR",
      is.na(test) & !(tipo_base %in% c("Paraguay\n2009-2020")) ~ "No Estudiado",
      TRUE ~ test
    ),
  ) %>%
  rename(
    "rango etario" = re,
    "clasificación epidemiológica" = classi_fin
  ) %>%
  mutate(
    across(where(is.character), as.factor)
  )

# Variables de interés
varsU <- c(
  "sexo", "rango etario", "criterio", "clasificación epidemiológica", 
  "test"
)

# De estas, las categóricas (factorVars)
varsU_factor <- c(
  "sexo", "rango etario", "criterio", "clasificación epidemiológica", 
  "test"
) 



Tabla_IGR <- CreateTableOne(
  data = dengue_expandido,
  vars = varsU,
  factorVars = varsU_factor,
  strata = "tipo_base"
)

# Imprimir con NAs y niveles
Tabla_IGR <- print(
  Tabla_IGR,
  showAllLevels = F,
  test = FALSE,  
  missing = TRUE,
  format = list(big.mark = ","),
  padColnames = TRUE
)

Tabla_IGR %>% data.table (caption = "Tabla 2. Validación general de las bases de datos de Dengue. Triple Frontera")

Tabla_IGR_df <- Tabla_IGR %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")

ft <- flextable(Tabla_IGR_df)%>%
  theme_vanilla() %>%
  autofit() %>%
  align(j = 1, align = "right", part = "all") %>%
  align(j = 2:ncol(Tabla_IGR_df), align = "center", part = "all") %>%
  bold(part = "header") %>%
  set_caption("Tabla 2. Validación general de las bases de datos de Dengue. Triple Frontera")%>%
  fontsize(size = 10, part = "all")

ft 

# Exportar como imagen
save_as_image(ft, path = "salida/articulo_1/Tabla1_resumen.png")

# Tabla 2 resumen de paises ----

# paso tabla a casos
dengue_expandido <- dengue_completo %>%
  uncount(weights = casos) %>% 
  mutate ("año epidemiológico"= as.factor(paste0(year_e, "-", year_e + 1)))%>%
  mutate( pais = case_when(
    pais == "argentina" ~ "Argentina",
    pais == "brasil" ~ "Brasil",
    pais == "paraguay" ~ "Paraguay"),
    tipo_base = case_when(
      tipo_base == "ar_c2_agrupado"   ~ "C2 Agrupado",
      tipo_base == "ar_c2_nominal"    ~ "C2 Nominal",
      tipo_base == "ar_sivila_nominal"~ "SIVILA Nominal",
      tipo_base == "ar_snvs2"         ~ "SNVS 2.0",
      tipo_base == "br_09_13"         ~ "SINAN 2009-2013",
      tipo_base == "br_14_23"         ~ "SINAN 2014-2023",
      tipo_base == "py_09_20"         ~ "DGVS 2009-2020",
      tipo_base == "py_21_23"         ~ "DGVS 2021-2023",
      TRUE ~ tipo_base
    ),  
    test = case_when(
      test == "igm" ~ "Serología",
      test == "ns1" ~ "Antígeno NS1",
      test == "pcr" ~ "RT-PCR"),
    re = if_else(re == "ancianos", "adultos mayores", re)
  ) %>%
  rename(
    "rango etario" = re,
    "clasificación epidemiológica" = classi_fin
  ) %>%
  mutate(
    across(where(is.character), as.factor)
  )

# Variables de interés
varsU <- c(
  "sexo", "rango etario", "criterio", "clasificación epidemiológica", 
  "test", "tipo_base"
)

# De estas, las categóricas (factorVars)
varsU_factor <- c(
  "sexo", "rango etario", "criterio", "clasificación epidemiológica", 
  "test", "tipo_base"
) 


Tabla_pais <- CreateTableOne(
  data = dengue_expandido,
  vars = varsU,
  factorVars = varsU_factor,
  strata = "pais"
)

# Imprimir con NAs y niveles
Tabla_pais <- print(
  Tabla_pais,
  showAllLevels = F,
  test = FALSE,  
  missing = TRUE,
  format = list(big.mark = ","),
  padColnames = TRUE,
  addOverall = T
  
)


Tabla_pais %>% data.table (caption = "Tabla 2. Validación general de las bases de datos de Dengue. Triple Frontera")

Tabla_pais_df <- Tabla_pais %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")  %>%
  mutate(across(everything(), ~ ifelse(. %in% c("     0 ( 0.0) ", "   0 ( 0.0) ", "    0 ( 0.0) "), "-------", .)))

ft2 <- flextable(Tabla_pais_df) %>%
  theme_vanilla() %>%
  autofit() %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:ncol(Tabla_pais_df), align = "center", part = "all") %>%
  bold(part = "header") %>%
  set_caption("Tabla 2. Validación general de las bases de datos de Dengue. Triple Frontera") %>% 
  footnote(
    i = NULL, j = 4, part = "header",
    value = as_paragraph("Nota: La base DGVS 2009-2020 no contiene información sobre el test diagnostico utilizado.")
  ) %>%
  fontsize(size = 10, part = "all") 

ft2

# Exportar como imagen
save_as_image(ft2, path = "salida/articulo_1/Tabla2_resumen_pais.png")


# ===================================================================
# Análisis de clasificación epidemiológica de dengue
# Facet por país y tipo de base + tabla resumen
# ===================================================================

# -------------------------------------------------------------------
# Limpieza y preparación de datos
# -------------------------------------------------------------------
dengue_pais <- dengue_expandido 




# -------------------------------------------------------------------
# 1. Clasificación epidemiológica por base de datos y país
# -------------------------------------------------------------------
df_class <- dengue_pais %>%
  group_by(pais, tipo_base, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, tipo_base) %>%
  mutate(prop = n / sum(n))

graf1 <- ggplot(df_class, aes(x = tipo_base, y = prop,
                              fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais + tipo_base, scales = "free_x") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Clasificación epidemiológica por base y país",
    x = "Base de datos",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

graf1

ggsave(
  "salida/articulo_1/Grafico1_base_clasificacion_pais.tiff",
  graf1, width = 12, height = 8, dpi = 300
)

# -------------------------------------------------------------------
# 2. Evolución temporal de la clasificación epidemiológica por país
# -------------------------------------------------------------------
df_class_year <- dengue_pais %>%
  group_by(pais, `año epidemiológico`, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(prop = n / sum(n)) %>% 
  filter 

graf2 <- ggplot(df_class_year,
                aes(x = `año epidemiológico`, y = prop,
                    fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Evolución temporal de la clasificación epidemiológica por país",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf2 

ggsave(
  "salida/articulo_1/Grafico2_evolucion_clasificacion_pais.tiff",
  graf2, width = 10, height = 8, dpi = 300
)

# -------------------------------------------------------------------
# 3. Distribución de pruebas diagnósticas por país y año
# -------------------------------------------------------------------
df_test_year <- dengue_pais %>%
  filter(!is.na(test)) %>%
  group_by(pais, `año epidemiológico`, test) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(prop = n / sum(n))

graf3 <- ggplot(df_test_year, aes(x = `año epidemiológico`, y = prop, fill = test)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(
    title = "Distribución de pruebas diagnósticas por país y año",
    x = "Año epidemiológico",
    y = "Proporción (%)",
    caption = "Nota: no se disponen de datos de Paraguay hasta 2020."
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf3

ggsave(
  "salida/articulo_1/Grafico3_test_por_anio_pais.tiff",
  graf3, width = 10, height = 10, dpi = 300
)

# -------------------------------------------------------------------
# 4. Confirmados por laboratorio vs. por nexo, por país y base
# -------------------------------------------------------------------
df_confirm_lab <- dengue_pais %>%
  filter(`clasificación epidemiológica` == "confirmado") %>%
  group_by(pais, tipo_base, criterio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, tipo_base) %>%
  mutate(prop = n / sum(n))

graf4 <- ggplot(df_confirm_lab, aes(x = tipo_base, y = prop, fill = criterio)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais + tipo_base, scales = "free_x") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Confirmados por laboratorio vs. nexo, por país y base",
    x = "Base de datos",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

ggsave(
  "salida/articulo_1/Grafico4_confirmados_laboratorio_nexo_pais.tiff",
  graf4, width = 12, height = 8, dpi = 300
)

# -------------------------------------------------------------------
# 5. Tabla resumen: casos por clasificación, país y año epidemiológico
# -------------------------------------------------------------------
tabla_resumen <- dengue_pais %>%
  count(pais, `año epidemiológico`, `clasificación epidemiológica`) %>%
  arrange(pais, `año epidemiológico`, `clasificación epidemiológica`)

# Exportar tabla a Excel / CSV si lo deseas
# write.csv(tabla_resumen, "salida/articulo_1/tabla_resumen.csv", row.names = FALSE)

# O mostrarla como tabla formateada en RMarkdown / Quarto
tabla_resumen %>%
  gt() %>%
  tab_header(
    title = "Casos por clasificación, país y año epidemiológico"
  )


# 1. Clasificación epidemiológica por base de datos ----
df_class <- dengue_expandido %>%
  group_by(tipo_base, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(tipo_base) %>%
  mutate(prop = n / sum(n))

graf1 <- ggplot(df_class, aes(x = tipo_base, y = prop, fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Clasificación epidemiológica por base de datos",
    x = "Base de datos",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graf1

ggsave(
  filename = "salida/articulo_1/Grafico1_base_clasificacion.tiff",
  plot = graf1,
  width = 8, height = 6, dpi = 300
)

# 2. Evolución temporal de la clasificación epidemiológica ----
df_class_year <- dengue_expandido %>%
  group_by(`año epidemiológico`, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(`año epidemiológico`) %>%
  mutate(prop = n / sum(n))

graf2 <- ggplot(df_class_year, aes(x = `año epidemiológico`, y = prop, fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Evolución temporal de la clasificación epidemiológica",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf2

ggsave(
  filename = "salida/articulo_1/Grafico2_evolucion_clasificacion.tiff",
  plot = graf2,
  width = 8, height = 6, dpi = 300
)

# 3. Distribución de test diagnósticos por año epidemiológico ----
df_test_year <- dengue_expandido %>%
  filter(!is.na(test)) %>%
  group_by(`año epidemiológico`, test) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(`año epidemiológico`) %>%
  mutate(prop = n / sum(n))

graf3 <- ggplot(df_test_year, aes(x = `año epidemiológico`, y = prop, fill = test)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(
    title = "Distribución de pruebas diagnósticas por año epidemiológico",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graf3

ggsave(
  filename = "salida/articulo_1/Grafico3_test_por_anio.tiff",
  plot = graf3,
  width = 8, height = 6, dpi = 300
)

# 4. Confirmados por laboratorio vs por nexo, por base ----
df_confirm_lab <- dengue_expandido %>%
  filter(`clasificación epidemiológica` == "confirmado") %>%
  group_by(tipo_base, criterio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(tipo_base) %>%
  mutate(prop = n / sum(n))

graf4 <- ggplot(df_confirm_lab, aes(x = tipo_base, y = prop, fill = criterio)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Confirmados por laboratorio vs. por nexo",
    x = "Base de datos",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graf4

ggsave(
  filename = "salida/articulo_1/Grafico4_confirmados_laboratorio_nexo.tiff",
  plot = graf4,
  width = 8, height = 6, dpi = 300
)

###Por país----
df_class <- dengue_expandido %>%
  mutate( pais = case_when(
    pais == "argentina" ~ "Argentina",
    pais == "brasil" ~ "Brasil",
    pais == "paraguay" ~ "Paraguay") )%>%
  group_by(pais, tipo_base, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, tipo_base) %>%
  mutate(prop = n / sum(n))

graf1_pais <- ggplot(df_class, aes(x = tipo_base, y = prop, fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ pais, scales = "free_x") +
  labs(
    title = "Clasificación epidemiológica por base de datos y país",
    x = "Base de datos",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf1_pais

ggsave(
  filename = "salida/articulo_1/Grafico1b_base_clasificacion_por_pais.tiff",
  plot = graf1_pais,
  width = 10, height = 6, dpi = 300
)

#Por país y año epidemiológico----
df_class_year <- dengue_expandido %>%
  mutate( pais = case_when(
    pais == "argentina" ~ "Argentina",
    pais == "brasil" ~ "Brasil",
    pais == "paraguay" ~ "Paraguay") )%>%
  group_by(pais, `año epidemiológico`, test) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(prop = n / sum(n))

graf3_pais <- ggplot(df_class_year, aes(x = `año epidemiológico`, y = prop, fill = test)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(
    title = "Distribución de pruebas diagnósticas por año y país",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graf3_pais
ggsave(
  filename = "salida/articulo_1/Grafico3b_test_por_anio_y_pais.tiff",
  plot = graf3_pais,
  width = 8, height = 10, dpi = 300
)

## 4. Confirmados por laboratorio vs por nexo, por país ----
df_confirm_lab <- dengue_expandido %>%
  filter(`clasificación epidemiológica` == "confirmado") %>%
  group_by(pais, tipo_base, criterio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais, tipo_base) %>%
  mutate(prop = n / sum(n))


graf4_pais <- ggplot(df_confirm_lab, aes(x = tipo_base, y = prop, fill = criterio)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Confirmados por laboratorio vs. por nexo, por país",
    x = "Base de datos",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graf4_pais

ggsave(
  filename = "salida/articulo_1/Grafico4b_confirmados_laboratorio_nexo_por_pais.tiff",
  plot = graf4_pais,
  width = 10, height = 6, dpi = 300
)

### Clasificación epidemiológica por año y base de datos--------

df_class_year_base <- dengue_expandido %>%
  group_by(`año epidemiológico`, tipo_base, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(`año epidemiológico`, tipo_base) %>%
  mutate(prop = n / sum(n))

graf5_base <- ggplot(df_class_year_base, aes(x = `año epidemiológico`, y = prop, fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ tipo_base, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Evolución de la clasificación epidemiológica por base de datos",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "salida/articulo_1/Grafico5_clasificacion_por_anio_y_base.tiff",
  plot = graf5_base,
  width = 12, height = 8, dpi = 300
)

####Clasificación epidemiológica por año y país--------

df_class_year_pais <- dengue_expandido  %>%
  group_by(`año epidemiológico`, pais, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(`año epidemiológico`, pais) %>%
  mutate(prop = n / sum(n))

graf6_pais <- ggplot(df_class_year_pais, aes(x = `año epidemiológico`, y = prop, fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Evolución de la clasificación epidemiológica por país",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "salida/articulo_1/Grafico6_clasificacion_por_anio_y_pais.tiff",
  plot = graf6_pais,
  width = 10, height = 8, dpi = 300
)
#📈 La proporción de casos confirmados por año epidemiológico

df_confirmados_total <- dengue_expandido %>%
  group_by(`año epidemiológico`, `clasificación epidemiológica`) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = `clasificación epidemiológica`, values_from = n, values_fill = 0) %>%
  mutate(
    total = confirmado + descartado + sospechoso,
    prop_confirmado = confirmado / total
  )

graf7_prop_confirmados <- ggplot(df_confirmados_total, aes(x = `año epidemiológico`)) +
  # Línea de proporción de confirmados
  geom_line(aes(y = prop_confirmado), group = 1, color = "steelblue", size = 1.2) +
  geom_point(aes(y = prop_confirmado), color = "steelblue", size = 2) +
  
  # Línea de total de casos (en eje secundario)
  geom_line(aes(y = total / max(total, na.rm = TRUE)), group = 1, color = "darkred", linetype = "dashed", size = 1) +
  geom_point(aes(y = total / max(total, na.rm = TRUE)), color = "darkred", size = 2) +
  
  scale_y_continuous(
    name = "Proporción de confirmados",
    labels = scales::percent,
    sec.axis = sec_axis(~ . * max(df_confirmados_total$total, na.rm = TRUE),
                        name = "Total de casos notificados")
  ) +
  labs(
    title = "Proporción de casos confirmados y total de casos por año epidemiológico",
    x = "Año epidemiológico"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "darkred")
  )

ggsave(
  filename = "salida/articulo_1/Grafico7_prop_confirmados_y_total.tiff",
  plot = graf7_prop_confirmados,
  width = 10, height = 6, dpi = 300
)




## Proporción de confirmados por año y país
# Añadir columna de país desde 'tipo_base'
dengue_pais <- dengue_expandido 

# Agregado por año y país
df_confirmados_total_pais <- dengue_pais %>%
  count(`año epidemiológico`, pais, `clasificación epidemiológica`) %>%
  pivot_wider(
    names_from  = `clasificación epidemiológica`,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    total = confirmado + descartado + sospechoso,
    prop_confirmado = ifelse(total > 0, confirmado/total, NA_real_)
  )


# Normalizar total dentro de cada país para superponerlo a la escala 0–1
df_faceta <- df_confirmados_total_pais %>%
  group_by(pais) %>%
  mutate(total_norm = ifelse(max(total, na.rm = TRUE) > 0,
                             total / max(total, na.rm = TRUE), NA_real_)) %>%
  ungroup()

graf_prop_tot_faceta <- ggplot(df_faceta, aes(x = `año epidemiológico`)) +
  # Proporción de confirmados (eje principal)
  geom_line(aes(y = prop_confirmado, group = 1), size = 1.1) +
  geom_point(aes(y = prop_confirmado), size = 2) +
  # Total normalizado (0–1) (misma escala primaria)
  geom_line(aes(y = total_norm, group = 1), linetype = "dashed") +
  geom_point(aes(y = total_norm), size = 1.8) +
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proporción de confirmados (línea continua) y total normalizado (línea discontinua) por país",
    x = "Año epidemiológico", y = "Proporción / Total normalizado (0–1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "salida/articulo_1/Grafico7b_prop_confirmados_y_total_normalizado_por_pais.tiff",
  graf_prop_tot_faceta, width = 9, height = 10, dpi = 300
)

plot_prop_total_por_pais <- function(datos, pais_sel, ruta_salida) {
  df_p <- datos %>% filter(pais == pais_sel) %>% arrange(`año epidemiológico`)
  max_total <- max(df_p$total, na.rm = TRUE)
  if (!is.finite(max_total) || max_total == 0) max_total <- 1
  
  g <- ggplot(df_p, aes(x = `año epidemiológico`)) +
    # Proporción de confirmados (0–1) en eje primario
    geom_line(aes(y = prop_confirmado, group = 1), size = 1.2) +
    geom_point(aes(y = prop_confirmado), size = 2.2) +
    # Total de casos reescalado a 0–1 para superponer, con eje secundario lineal
    geom_line(aes(y = total / max_total, group = 1), linetype = "dashed", size = 1) +
    geom_point(aes(y = total / max_total), size = 2) +
    scale_y_continuous(
      name = "Proporción de confirmados",
      labels = percent,
      sec.axis = sec_axis(~ . * max_total, name = "Total de casos notificados")
    ) +
    labs(
      title = paste0("Proporción de confirmados y total de casos por año - ", pais_sel),
      x = "Año epidemiológico"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y.left  = element_text(color = "steelblue"),
      axis.title.y.right = element_text(color = "darkred")
    )
  
  ggsave(ruta_salida, g, width = 9, height = 6, dpi = 300)
  return(g)
}

# Generar y guardar una figura por país
paises <- c("Argentina", "Brasil", "Paraguay")
rutas <- file.path("salida/articulo_1",
                   paste0("Grafico7c_prop_confirmados_y_total_por_anio_", tolower(paises), ".tiff"))

plots_generados <- Map(
  plot_prop_total_por_pais,
  datos = list(df_confirmados_total_pais, df_confirmados_total_pais, df_confirmados_total_pais),
  pais_sel = paises,
  ruta_salida = rutas
)

####Grafico con tabla de casos y clasificación-------
# Proporciones por país, año y clasificación
# Totales por país y año epidemiológico (uno por barra/facet)
tot_anno <- dengue_pais %>%
  count(pais, `año epidemiológico`, name = "total")

df_prop_clasif <- dengue_pais |>
  group_by(pais, `año epidemiológico`, `clasificación epidemiológica`) |>
  summarise(casos = n(), .groups = "drop") |>
  group_by(pais, `año epidemiológico`) |>
  mutate(prop = casos / sum(casos))

# Gráfico con etiquetas de totales arriba de cada barra
graf_prop <- ggplot(df_prop_clasif,
                    aes(x = `año epidemiológico`, y = prop,
                        fill = `clasificación epidemiológica`)) +
  geom_bar(stat = "identity", position = "fill") +
  # etiquetas de totales (una por año dentro de cada pais)
  geom_text(
    data = tot_anno,
    aes(x = `año epidemiológico`, y = 1,
        label = paste0("n = ", scales::comma(total))),
    inherit.aes = FALSE,
    vjust = -0.25, size = 3.6
  ) +
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.10))  # espacio arriba para que no se corten
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Proporción de casos por clasificación epidemiológica",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    strip.text   = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    plot.margin    = margin(t = 10, r = 8, b = 10, l = 8)
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "salida/articulo_1/Proporciones_con_tabla_alineada2.tiff",
  graf_prop, width = 12, height = 12, dpi = 300
)

###Tabla proporciones

tabla_prop <- dengue_pais %>%
  # Conteo por país, año y clasificación
  count(pais, `año epidemiológico`, `clasificación epidemiológica`, name = "casos") %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(prop = casos / sum(casos)) %>%       # proporción en [0,1]
  ungroup() %>%
  # Etiqueta clara para los NA en clasificación
  mutate(clas = fct_explicit_na(`clasificación epidemiológica`, na_level = "No clasificado")) %>%
  select(pais, `año epidemiológico`, clas, prop) %>%
  # Tabla ancha: una fila por país–año
  pivot_wider(
    names_from  = clas,
    values_from = prop,
    values_fill = 0
  ) %>%
  arrange(pais, `año epidemiológico`)

tabla_prop_pct <- tabla_prop %>%
  mutate(across(-c(pais, `año epidemiológico`), ~ round(.x * 100, 1)))

#Proporción de criterio------------

tabla_prop_criterio <- dengue_pais %>%
  # Conteo por país, año y criterio
  count(pais, `año epidemiológico`, criterio, name = "casos") %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(prop = casos / sum(casos)) %>%
  ungroup() %>%
  # Etiqueta clara para los NA en criterio
  mutate(criterio = fct_explicit_na(criterio, na_level = "No especificado")) %>%
  select(pais, `año epidemiológico`, criterio, prop) %>%
  # Tabla ancha: una fila por país–año
  pivot_wider(
    names_from  = criterio,
    values_from = prop,
    values_fill = 0
  ) %>%
  arrange(pais, `año epidemiológico`)

# Convertir proporciones a porcentajes
tabla_prop_criterio_pct <- tabla_prop_criterio %>%
  mutate(across(-c(pais, `año epidemiológico`), ~ round(.x * 100, 1)))

tabla_prop_criterio_pct




df_prop_criterio <- dengue_pais %>%
  count(pais, `año epidemiológico`, criterio, name = "casos") %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(prop = casos / sum(casos))

graf_prop_criterio <- ggplot(df_prop_criterio,
                             aes(x = `año epidemiológico`, y = prop, fill = criterio)) +
  geom_bar(stat = "identity", position = "fill") +
  # etiquetas de totales (una por año dentro de cada pais)
  geom_text(
    data = tot_anno,
    aes(x = `año epidemiológico`, y = 1,
        label = paste0("n = ", scales::comma(total))),
    inherit.aes = FALSE,
    vjust = -0.25, size = 3.6
  )+
  facet_wrap(~ pais, ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Proporción de casos por criterio de confirmación",
    x = "Año epidemiológico",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    strip.text   = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    plot.margin    = margin(t = 10, r = 8, b = 10, l = 8)
  )

ggsave(
  "salida/articulo_1/Proporciones_por_criterio.tiff",
  graf_prop_criterio, width = 12, height = 12, dpi = 300
)


# ============================
# Tablas S1–S4 para el artículo
# ============================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(flextable)
library(officer)
library(binom)
library(janitor)
library(scales)

# Carpeta de salida
dir.create("salida/articulo_1/tablas", showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# Utilidades
# ----------------------------

# País desde 'tipo_base'
dengue_pais <- dengue_expandido 

# Función para IC 95% binomial (Wilson)
ci_wilson <- function(x, n) {
  ifelse(n > 0,
         binom::binom.wilson(x, n)[, c("lower","upper")],
         cbind(lower = NA_real_, upper = NA_real_))
}

# Estilo flextable genérico
ft_estilo <- function(ft) {
  ft %>%
    theme_vanilla() %>%
    autofit() %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2:flextable::ncol_keys(ft), align = "center", part = "all") %>%
    bold(part = "header")
}

# Guardado opcional a imagen
guardar_png <- function(ft, ruta_png, width = 10, height = 6, res = 300){
  # Nota: save_as_image solo funciona para flextable
  try(flextable::save_as_image(ft, path = ruta_png), silent = TRUE)
}

# ============================
# Tabla S1. Proporción de confirmados por país × año (con IC95%)
# ============================
# Base S1 (conteos y proporción)
S1_base <- dengue_pais %>%
  count(pais, `año epidemiológico`, `clasificación epidemiológica`) %>%
  pivot_wider(
    names_from  = `clasificación epidemiológica`,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    total = confirmado + descartado + sospechoso,
    prop_confirmado = if_else(total > 0, confirmado/total, NA_real_)
  )

# IC 95% Wilson vectorizado
ci <- binom::binom.wilson(S1_base$confirmado, S1_base$total)

# Armar tabla final
S1 <- S1_base %>%
  bind_cols(ci[, c("lower","upper")]) %>%   # agrega columnas lower/upper
  mutate(
    `Proporción confirmado` = scales::percent(prop_confirmado, accuracy = 0.1),
    `IC95%` = paste0(
      scales::percent(lower, accuracy = 0.1), " – ",
      scales::percent(upper, accuracy = 0.1)
    )
  ) %>%
  select(
    País = pais,
    `Año epidemiológico` = `año epidemiológico`,
    `Total casos` = total,
    `Confirmados (n)` = confirmado,
    `Proporción confirmado`,
    `IC95%`
  ) %>%
  arrange(País, `Año epidemiológico`)
# Tabla flextable

ft_S1 <- flextable(S1) %>%
  set_caption("Tabla S1. Proporción de casos confirmados por país y año epidemiológico (IC95%).") %>%
  ft_estilo()

# ============================
# Tabla S2. Confirmados por laboratorio vs. nexo (país × año)
# ============================

S2 <- dengue_pais %>%
  filter(`clasificación epidemiológica` == "confirmado") %>%
  mutate(método = if_else(tolower(criterio) %in% c("laboratorio","lab","laboratorial"),
                          "Confirmado por laboratorio", "Confirmado por nexo")) %>%
  count(pais, `año epidemiológico`, método) %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(total_confirmados = sum(n),
         prop = n/total_confirmados) %>%
  ungroup() %>%
  pivot_wider(
    names_from  = método,
    values_from = c(n, prop),
    values_fill = 0
  ) %>%
  mutate(across(starts_with("prop_"), ~ percent(.x, accuracy = 0.1))) %>%
  arrange(pais, `año epidemiológico`)

# Renombrado amigable
S2 <- S2 %>%
  rename(
    País = pais,
    `Año epidemiológico` = `año epidemiológico`,
    `Confirmados totales (n)` = total_confirmados,
    `Confirmados por laboratorio (n)` = `n_Confirmado por laboratorio`,
    `Confirmados por nexo (n)` = `n_Confirmado por nexo`,
    `Confirmados por laboratorio (%)` = `prop_Confirmado por laboratorio`,
    `Confirmados por nexo (%)` = `prop_Confirmado por nexo`
  )

ft_S2 <- flextable(S2) %>%
  set_caption("Tabla S2. Confirmados por laboratorio vs. por nexo, por país y año epidemiológico.") %>%
  ft_estilo()

# ============================
# Tabla S3. Distribución de pruebas diagnósticas por país × año
#          (entre registros con test conocido; incluye % sin dato)
# ============================

S3_base <- dengue_pais %>%
  mutate(test_cat = case_when(
    str_detect(tolower(test), "ns1") ~ "Antígeno NS1",
    str_detect(tolower(test), "pcr") ~ "RT-PCR",
    str_detect(tolower(test), "igm|serol") ~ "Serología",
    is.na(test) ~ NA_character_,
    TRUE ~ test
  ))

S3 <- S3_base %>%
  count(pais, `año epidemiológico`, test_cat, name = "n") %>%
  mutate(test_cat = replace_na(test_cat, "Sin dato de test")) %>%
  group_by(pais, `año epidemiológico`) %>%
  mutate(total = sum(n), prop = n/total) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(pais, `año epidemiológico`, total),
    names_from = test_cat,
    values_from = c(n, prop),
    values_fill = 0
  ) %>%
  mutate(across(starts_with("prop_"), ~ percent(.x, accuracy = 0.1))) %>%
  arrange(pais, `año epidemiológico`)

# Renombres legibles (si no existen, se ignora con any_of)
S3 <- S3 %>%
  rename(
    País = pais,
    `Año epidemiológico` = `año epidemiológico`,
    `Total (c/ y s/ test)` = total,
    `NS1 (n)`      = any_of("n_Antígeno NS1"),
    `RT-PCR (n)`   = any_of("n_RT-PCR"),
    `Serología (n)`= any_of("n_Serología"),
    `Sin dato (n)` = any_of("n_Sin dato de test"),
    `NS1 (%)`      = any_of("prop_Antígeno NS1"),
    `RT-PCR (%)`   = any_of("prop_RT-PCR"),
    `Serología (%)`= any_of("prop_Serología"),
    `Sin dato (%)` = any_of("prop_Sin dato de test")
  )

ft_S3 <- flextable(S3) %>%
  set_caption("Tabla S3. Distribución de pruebas diagnósticas por país y año (incluye porcentaje sin dato de test).") %>%
  ft_estilo()

# ============================
# Tabla S4. Porcentaje de datos faltantes por variable y país
# ============================

vars_eval <- c("sexo", "rango etario", "criterio", "test", "clasificación epidemiológica")

S4 <- dengue_pais %>%
  # homogeneizar nombres para evaluar NA sobre las columnas originales con acentos
  rename(
    `sexo` = sexo,
    `rango etario` = `rango etario`,
    `criterio` = criterio,
    `test` = test,
    `clasificación epidemiológica` = `clasificación epidemiológica`
  ) %>%
  select(pais, all_of(vars_eval)) %>%
  mutate(across(all_of(vars_eval), ~ ifelse(is.na(.), 1, 0))) %>%
  group_by(pais) %>%
  summarise(across(all_of(vars_eval), ~ mean(.x)*100, .names = "{.col} (%)"), .groups = "drop") %>%
  arrange(pais)

ft_S4 <- flextable(S4) %>%
  set_caption("Tabla S4. Porcentaje de datos faltantes por variable y país.") %>%
  ft_estilo()

# ============================
# Exportar a DOCX (todas las tablas)
# ============================

doc <- read_docx() %>%
  body_add_par("Material suplementario - Tablas", style = "heading 1") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_S1) %>% body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_S2) %>% body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_S3) %>% body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_S4)

print(doc, target = "salida/articulo_1/tablas/Suplemento_Tablas_S1_S4.docx")

# (Opcional) Exportar cada tabla como imagen PNG
guardar_png(ft_S1, "salida/articulo_1/tablas/Tabla_S1.png")
guardar_png(ft_S2, "salida/articulo_1/tablas/Tabla_S2.png")
guardar_png(ft_S3, "salida/articulo_1/tablas/Tabla_S3.png")
guardar_png(ft_S4, "salida/articulo_1/tablas/Tabla_S4.png")
