# =============================
# Table 1 por Municipio – Script
# =============================

source("scripts/03_surveillance/01_quality.R")

pacman::p_load(
  dplyr, tidyr, ggplot2, gtsummary,  webshot, flextable, tableone, rstatix,
   kableExtra,officer)

# Directorio de trabajo
# Ejecutar desde la raíz del proyecto / Run from the project root.
vars6 <- c( "Atención","Laboratorio", "Cierre",
            "Completitud", "Consistencia")
Var_SLAMVI_factor <- c( "Consistencia")

dengue_attr_base<-dengue_attr_base%>%
  rename(
    "Atención"= Retraso_Not_cln,
    "Laboratorio" = Retraso_Lab_cln,
    "Cierre" = Temporalidad_cln,
    "Completitud"= Completitud_redu) 



Tabla_SLAMVI_1<-CreateTableOne(data=dengue_attr_base, vars=vars6, 
                               factorVars =Var_SLAMVI_factor,
                               strata= "Municipio")

Tabla_SLAMVI_1 <- print(
  Tabla_SLAMVI_1,
  showAllLevels = TRUE,
  missing = TRUE,
  format = list(big.mark = ","),
  padColnames = TRUE,
  testContinuous = "kruskal.test",
  testCategorical = "chisq")

Tabla_SLAMVI_1
# 2. Convertir a flextable y exportar 
Tabla_SLAMVI_docx <- flextable::qflextable(as.data.frame(Tabla_SLAMVI_1)) %>%
  flextable::set_caption("Resumen de datos de calidad de las bases. Dengue. Triple Frontera. 2009–2023")

save_as_docx(
  Tabla_SLAMVI_docx,
  path = "salida/articulo_2/Tabla_SLAMVI_1.docx"
)

# 2) Render kable
tab_html <- Tabla_SLAMVI_1 |>
  kbl(caption = "Resumen de datos de calidad de las bases. Dengue. Triple Frontera. 2009–2023") |>
  kable_classic_2(full_width = FALSE)
tab_html
# 3) Guardar como PNG (imagen)
save_kable(tab_html, file = "salida/articulo_2/Tabla_SLAMVI_1.png", zoom = 2)  # zoom ↑ para mejor resolución


###Analisis por temporada-------------

# Preparo la base
data <- base_periodos %>%
  mutate(
    # Periodo como factor con 4 niveles (
    Periodo = str_to_upper(Periodo),
    Periodo = fct_relevel(factor(Periodo),
                          "PRE","COVID","INTER","CHIKV"),
    
    # Mes epidemico binario legible
    MES_Epi = case_when(
      MES_Epi =="1" ~ "Sí",
      TRUE ~ "No"
    ),
    MES_Epi = factor(MES_Epi, levels = c("No","Sí")),
    
    # Municipio como factor
    Municipio = factor(Municipio),
    # Aseguramos numéricas
    Atención = as.numeric(Retraso_Not_cln),
    Laboratorio  = as.numeric(Retraso_Lab_cln),
    Consistencia = factor(Consistencia, levels = c("No","Sí")),
    Completitud = fct_relevel( factor(Completitud_redu_cat),
    "baja","regular",   "buena", "excelente"  )) %>%
  select(Municipio, Periodo, MES_Epi, Atención, Laboratorio, Consistencia, Completitud)



vars_cont <- c("Atención","Laboratorio")
vars_cat  <- c("Consistencia","Completitud")


#Funciones
# Chi-cuadrado con simulación (robusto ante celdas chicas o muchas categorías)
chisq_sim <- function(data, variable, by, ...) {
  tb <- table(data[[variable]], data[[by]])
  broom::tidy(chisq.test(tb, simulate.p.value = TRUE, B = 50000))
}

# Post-hoc de proporciones por pares, con BH, por cada NIVEL de la variable categórica
posthoc_prop <- function(df, fac_var, by_var){
  # df: data.frame
  # fac_var: nombre de la variable categórica (string)
  # by_var:  nombre del grupo (string), p.ej. "Municipio" o "Periodo"
  
  stopifnot(fac_var %in% names(df), by_var %in% names(df))
  
  d0 <- df %>%
    dplyr::select(fac = .data[[fac_var]], grp = .data[[by_var]]) %>%
    dplyr::filter(!is.na(fac), !is.na(grp)) %>%
    dplyr::mutate(fac = as.factor(fac), grp = as.factor(grp))
  
  # si hay menos de 2 grupos, no hay comparación
  if (nlevels(d0$grp) < 2L) return(dplyr::tibble())
  
  # recorrer cada nivel de la categórica como “éxito”
  purrr::map_dfr(levels(d0$fac), function(lv){
    tmp <- d0 %>% dplyr::mutate(hit = as.integer(fac == lv)) %>%
      dplyr::count(grp, hit) %>%
      tidyr::pivot_wider(names_from = hit, values_from = n, values_fill = 0)
    
    # éxitos y totales por grupo
    successes <- tmp$`1`
    totals    <- tmp$`0` + tmp$`1`
    
    # nombrar los vectores para que la salida tenga etiquetas correctas
    names(successes) <- as.character(tmp$grp)
    names(totals)    <- as.character(tmp$grp)
    
    # si algún total es 0, salteamos
    if (any(totals == 0)) return(dplyr::tibble())
    
    pw <- pairwise.prop.test(x = successes, n = totals, p.adjust.method = "BH")
    
    as.data.frame(as.table(pw$p.value)) %>%
      dplyr::filter(!is.na(Freq)) %>%
      dplyr::transmute(
        categoria = lv,
        group1 = as.character(Var1),
        group2 = as.character(Var2),
        q = Freq
      )
  })
}
tbl_desc_con_p <- function(data_in, by_var, titulo){
  gtsummary::tbl_summary(
    data = data_in %>%
      dplyr::select(dplyr::all_of(c(vars_cont, vars_cat)), {{by_var}}),
    by   = {{by_var}},
    type = list(
      gtsummary::all_continuous() ~ "continuous2",
      dplyr::all_of(vars_cat)     ~ "categorical"
    ),
    statistic = list(
      gtsummary::all_continuous() ~ c("{mean} ({sd})", "{median} [{p25}, {p75}]"),
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
    add_overall(last = TRUE) %>%
    add_p(test = list(
      # continuas: Kruskal si hay >2 grupos; Wilcoxon si hay 2
      gtsummary::all_continuous() ~ function(data, variable, by, ...){
        g <- data[[by]]
        if(nlevels(factor(g)) > 2) "kruskal.test" else "wilcox.test"
      },
      # categóricas: Chi2 simulado (robusto)
      dplyr::all_of(vars_cat)     ~ chisq_sim
    )) %>%
    add_q(method = "BH") %>%
    modify_caption(titulo)
}

# Post-hoc de Dunn para las continuas: helper
run_dunn <- function(data_in, var, by_sym){
  df0 <- data_in %>% dplyr::select(x = all_of(var), g = {{by_sym}}) %>% filter(!is.na(x), !is.na(g))
  if(nlevels(factor(df0$g)) <= 2) return(NULL)
  rstatix::dunn_test(df0, x ~ g, p.adjust.method = "BH") %>%
    mutate(variable = var) %>%
    select(variable, group1, group2, p.adj, p.adj.signif)
}

tbl_desc_con_p <- function(data_muni, by_var, titulo){
  # calcular aquí el test para continuas según # de grupos
  g_vec <- dplyr::pull(data_muni, {{by_var}})
  nlev  <- nlevels(factor(g_vec))
  test_cont <- if (nlev > 2) "kruskal.test" else "wilcox.test"
  
  gtsummary::tbl_summary(
    data = data_muni %>%
      dplyr::select(dplyr::all_of(c(vars_cont, vars_cat)), {{by_var}}),
    by   = {{by_var}},
    type = list(
      gtsummary::all_continuous() ~ "continuous2",
      dplyr::all_of(vars_cat)     ~ "categorical"
    ),
    statistic = list(
      gtsummary::all_continuous()  ~ c("{mean} ({sd})", "{median} [{p25}, {p75}]"),
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
    gtsummary::add_overall(last = TRUE) %>%
    gtsummary::add_p(test = list(
      gtsummary::all_continuous() ~ test_cont,   # ← ahora sí, un string válido
      dplyr::all_of(vars_cat)     ~ chisq_sim
    )) %>%
    gtsummary::add_q(method = "BH") %>%
    gtsummary::modify_caption(titulo)
}




###Analisis
# Tabla principal por Municipio (todas las observaciones)------------
tbl_muni <- tbl_desc_con_p(data, Municipio, titulo = "**Comparación entre Municipios**")

# Dunn por variable continua (Municipio)
dunn_muni <- map_dfr(vars_cont, ~ run_dunn(data, .x, rlang::sym("Municipio"))) %>%
  arrange(variable, p.adj)

# Post-hoc proporciones por categóricas (Municipio)
posthoc_cat_muni <- map_dfr(vars_cat, ~ posthoc_prop(data, .x, by_var = "Municipio")) %>%
  arrange(categoria, q)

# Exportar a Word
doc1 <- read_docx() %>%
  body_add_par("ANÁLISIS 1: Municipios entre sí", style = "heading 1") %>%
  body_add_flextable(as_flex_table(tbl_muni)) %>%
  body_add_par("Post-hoc Dunn (continuas) por Municipio", style = "heading 2") %>%
  body_add_flextable(flextable(dunn_muni)) %>%
  body_add_par("Post-hoc proporciones (categóricas) por Municipio", style = "heading 2") %>%
  body_add_flextable(flextable(posthoc_cat_muni))

print(doc1, target = "salida/articulo_2/Analisis1_Municipios.docx")



#Hago VPP y tasa de positividad por año

variables <- c( "vpp_pct", "positividad_lab")
Tabla_muni_VPP<-CreateTableOne(data=resumen_indicadores_municipio, vars=variables, 
                 strata= "Municipio")

Tabla_muni_VPP <- print(
  Tabla_muni_VPP,
  showAllLevels = TRUE,
  missing = TRUE,
  format = list(big.mark = ","),
  padColnames = TRUE,
  testContinuous = "kruskal.test",
  testCategorical = "chisq")

# 2. Convertir a flextable y exportar 
Tabla_muni_VPP_docx <- flextable::qflextable(as.data.frame(Tabla_muni_VPP)) %>%
  flextable::set_caption("Resumen de datos de calidad de las bases. Dengue. Triple Frontera. 2009–2023")

save_as_docx(
  Tabla_muni_VPP_docx,
  path = "salida/articulo_2/Tabla_Muni_VPP.docx"
)

# 2) Render kable
Tabla_muni_VPP |>
  kbl(caption = "Resumen de datos de calidad de las bases. Dengue. Triple Frontera. 2009–2023") |>
  kable_classic_2(full_width = FALSE)

resumen_indicadores_municipio%>% dunn_test(positividad_lab~  Municipio , p.adjust.method = "BH") 



# Chi-cuadrado con simulación (para gtsummary en las categóricas)
chisq_sim <- function(data, variable, by, ...) {
  tb <- table(data[[variable]], data[[by]])
  broom::tidy(chisq.test(tb, simulate.p.value = TRUE, B = 50000))
}

# Tabla resumida por Periodo (por municipio)
tbl_desc_con_p <- function(data_muni, titulo){
  gtsummary::tbl_summary(
    data = data_muni %>%
      dplyr::select(dplyr::all_of(c(vars_cont, vars_cat)), Periodo),
    by   = Periodo,
    type = list(
      gtsummary::all_continuous() ~ "continuous2",
      dplyr::all_of(vars_cat)     ~ "categorical"
    ),
    statistic = list(
      gtsummary::all_continuous()  ~ c("{mean} ({sd})", "{median} [{p25}, {p75}]"),
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
    add_overall(last = TRUE) %>%
    add_p(test = list(
      gtsummary::all_continuous() ~ "kruskal.test",
      dplyr::all_of(vars_cat)     ~ chisq_sim
    )) %>%
    add_q(method = "BH") %>%
    modify_caption(titulo)
}

# Dunn para continuas (Periodo dentro de municipio)
run_dunn <- function(data_in, var){
  df0 <- data_in %>% select(x = all_of(var), g = Periodo) %>% filter(!is.na(x), !is.na(g))
  if (nlevels(factor(df0$g)) <= 2) return(tibble())
  rstatix::dunn_test(df0, x ~ g, p.adjust.method = "BH") %>%
    transmute(variable = var, group1, group2, q = p.adj, signif = p.adj.signif) %>%
    arrange(q)
}

# Post-hoc proporciones para categóricas (Periodo dentro de municipio)
posthoc_prop <- function(df, fac_var){
  d0 <- df %>%
    select(fac = .data[[fac_var]], grp = Periodo) %>%
    filter(!is.na(fac), !is.na(grp)) %>%
    mutate(fac = as.factor(fac), grp = as.factor(grp))
  if (nlevels(d0$grp) < 2L) return(tibble())
  
  map_dfr(levels(d0$fac), function(lv){
    tmp <- d0 %>% mutate(hit = as.integer(fac == lv)) %>%
      count(grp, hit) %>% pivot_wider(names_from = hit, values_from = n, values_fill = 0)
    successes <- tmp$`1`; totals <- tmp$`0` + tmp$`1`
    names(successes) <- as.character(tmp$grp); names(totals) <- names(successes)
    if (any(totals == 0)) return(tibble())
    pw <- pairwise.prop.test(x = successes, n = totals, p.adjust.method = "BH")
    as.data.frame(as.table(pw$p.value)) %>%
      filter(!is.na(Freq)) %>%
      transmute(variable = fac_var, categoria = lv, group1 = as.character(Var1), group2 = as.character(Var2), q = Freq)
  }) %>% arrange(variable, categoria, q)
}

munis <- levels(resumen_indicadores_municipio$Municipio)

doc <- read_docx() %>%
  body_add_par("ANÁLISIS: Periodos dentro de cada Municipio", style = "heading 1")

for (m in munis){
  sub <- dat %>% filter(Municipio == m)
  
  # Tabla descriptiva por Periodo
  tabla <- tbl_desc_con_p(sub, titulo = paste0("**", m, "** — Periodo (PRE / INTER / COVID / CHIKV)"))
  
  # Post-hoc Dunn (continuas)
  dunn_tbl <- map_dfr(vars_cont, ~ run_dunn(sub, .x)) %>%
    filter(nrow(.) == 0 | q < 1)  # deja vacía si no hay 3+ grupos
  
  # Post-hoc proporciones (categóricas) — nos quedamos con los pares significativos
  postc_tbl <- map_dfr(vars_cat, ~ posthoc_prop(sub, .x)) %>%
    filter(q < 0.05)
  
  # Añadir al doc
  doc <- doc %>%
    body_add_par(m, style = "heading 2") %>%
    body_add_flextable(as_flex_table(tabla)) %>%
    body_add_par("Post-hoc Dunn (continuas) — pares y q (BH)", style = "heading 3") %>%
    body_add_flextable(
      if (nrow(dunn_tbl)) flextable(dunn_tbl) else flextable(tibble(info = "Sin comparaciones Dunn aplicables o sin diferencias."))
    ) %>%
    body_add_par("Post-hoc proporciones (categóricas) — pares con q < 0.05 (BH)", style = "heading 3") %>%
    body_add_flextable(
      if (nrow(postc_tbl)) flextable(postc_tbl) else flextable(tibble(info = "Sin pares significativos en categóricas."))
    ) %>%
    body_add_par("")
}

print(doc, target = "Periodos_por_Municipio_posthoc.docx")



############ Indicadores por individual------------------
resumen_indicadores_municipio %>%
  filter(!is.na(positividad_lab), !is.na(Municipio)) %>%
  mutate(Municipio = as.factor(Municipio)) %>%
  kruskal_test(positividad_lab ~ Municipio)

# Dunn post-hoc (BH)
dunn_muni_2 <- resumen_indicadores_municipio %>%
  filter(!is.na(positividad_lab), !is.na(Municipio)) %>%
  mutate(Municipio = as.factor(Municipio)) %>%
  dunn_test(positividad_lab ~ Municipio, p.adjust.method = "BH") %>%
  arrange(p.adj)

dunn_muni_3 <- data %>%
  filter(!is.na(Atención), !is.na(Municipio)) %>%
  mutate(Municipio = as.factor(Municipio)) %>%
  dunn_test(Atención ~ Municipio, p.adjust.method = "BH") %>%
  arrange(p.adj)

dunn_muni_4 <- data %>%
  filter(!is.na(Laboratorio), !is.na(Municipio)) %>%
  mutate(Municipio = as.factor(Municipio)) %>%
  dunn_test(Laboratorio ~ Municipio, p.adjust.method = "BH") %>%
  arrange(p.adj)

############ Indicadores Periodo------------


###Hago tabla periodo
base_periodos <- base_periodos %>%
  mutate(
    Periodo = as.factor(Periodo),
    Periodo = fct_relevel(factor(Periodo),
                          "PRE","COVID","INTER","CHIKV"),
    Municipio = as.factor(Municipio),
    MES_Epi = as.factor(MES_Epi),
    Cierre = as.numeric(Temporalidad_cln),
    Diagnostico  = as.numeric(Retraso_Lab_cln),
    Atención  = as.numeric(Retraso_Not_cln),
    Completitud =  case_when  (
      Completitud_redu_cat == "excelente" ~ "Si",
      TRUE ~ "No"),
    Consistencia = as.factor(Consistencia)
  )

# 2) Control de tabla: estadísticas y tests
ctrl <- tableby.control(
  test = TRUE, total = TRUE,
  numeric.stats = c("meansd"),
  cat.stats     = c("countpct"),
  numeric.test  = "kwt",       # Kruskal–Wallis
  cat.test      = "chisq",     # Chi-cuadrado
  simulate.p    = TRUE,# usa simulación para cat.test (robusto)
  digits = 1,          # redondeo general
  digits.stats = 1,    # redondeo de estadísticas
  digits.p = 3,        # p-values (opcional)
  digits.chisq = 3     # tests (opcional)
  )

# 3) Tabla: Periodo (~ variables), estratificada por Municipio
T1.periodo <- tableby(
  Periodo ~ Diagnostico + Atención +
    Completitud + Consistencia,
  data   = base_periodos,
  strata = Municipio,
  control = ctrl
)

summary(T1.periodo, text=TRUE)

#Salida
arsenal::write2word(T1.periodo,
                      file = "salida/articulo_2/T2_periodo_por_municipio.doc",
                      title = "Tabla 1. Comparación por Periodo (estratificada por Municipio)",
                    keep.md = TRUE, quiet = TRUE    )

###Hago tabla Cierre
base_periodos_IGU <- base_periodos %>% 
  filter(pais == "brasil") 

# 2) Control de tabla: estadísticas y tests
ctrl <- tableby.control(
  test = TRUE, total = TRUE,
  numeric.stats = c("meansd"),
  cat.stats     = c("countpct"),
  numeric.test  = "kwt",       # Kruskal–Wallis
  cat.test      = "chisq",     # Chi-cuadrado
  simulate.p    = TRUE,         # usa simulación para cat.test (robusto)
  digits = 1L,
  digits.count = 0L,
  digits.pct = 1L,
  digits.p = 1L,,
  
  
)

# 3) Tabla: Periodo (~ variables), estratificada por Municipio
T1.periodo.cierre <- tableby(
  Periodo ~ Cierre ,
  data   = base_periodos_IGU,
  strata = Municipio,
  control = ctrl
)

summary(T1.periodo.cierre, text=TRUE)

#Salida
arsenal::write2word(T1.periodo.cierre,
                    file = "salida/articulo_2/T2_periodo_por_municipio_crierre.doc",
                    title = "Tabla 1. Comparación por Periodo (estratificada por Municipio)",
                    keep.md = TRUE, quiet = TRUE)

###Hago tabla VPP 
# 3) Tabla: Periodo (~ variables), estratificada por Municipio
T1.periodo_vpp <- tableby(
  Periodo ~ vpp_pct + positividad_lab ,
  data   = resumen_indicadores_municipio_periodos,
  strata = Municipio,
  control = ctrl_sin_test
)

summary(T1.periodo_vpp, text=TRUE)

ctrl_sin_test

#Salida
arsenal::write2word(T1.periodo_vpp,
                    file = "salida/articulo_2/T2_periodo_por_municipio_crierre.doc",
                    title = "Tabla 1. Comparación por Periodo (estratificada por Municipio)",
                    keep.md = TRUE, quiet = TRUE    )


### Hago Dunn

# --- Dunn por Periodo dentro de cada Municipio ---
#Atención
dunn_Atencion <- base_periodos %>%
  filter(!is.na(Atención), !is.na(Periodo), !is.na(Municipio)) %>%
  group_by(Municipio) %>%
  filter(n_distinct(Periodo) >= 3) %>%             # asegura que haya ≥3 periodos
  dunn_test(Atención ~ Periodo, p.adjust.method = "BH") %>%
  arrange(Municipio, p.adj) %>%
  ungroup()

write.csv(dunn_Atencion, "salida/articulo_2/dunn_Atencion_por_Municipio.csv", row.names = FALSE)

#Diagnostico
dunn_Diagnostico<- base_periodos %>%
  filter(!is.na(Diagnostico), !is.na(Periodo), !is.na(Municipio)) %>%
  group_by(Municipio) %>%
  filter(n_distinct(Periodo) >= 3) %>%             # asegura que haya ≥3 periodos
  dunn_test(Diagnostico ~ Periodo, p.adjust.method = "BH") %>%
  arrange(Municipio, p.adj) %>%
  ungroup()

# guardar CSV
write.csv2(dunn_Diagnostico, "salida/articulo_2/dunn_Diagnostico_por_Municipio.csv", row.names = FALSE)


#Cierre
#Diagnostico
dunn_Cierre<- base_periodos %>% filter(pais == "brasil") %>%
  filter(!is.na(Cierre), !is.na(Periodo), !is.na(Municipio)) %>%
  group_by(Municipio) %>%
  filter(n_distinct(Periodo) >= 3) %>%             # asegura que haya ≥3 periodos
  dunn_test(Cierre ~ Periodo, p.adjust.method = "BH") %>%
  arrange(Municipio, p.adj) %>%
  ungroup()

# guardar CSV
write.csv(dunn_Cierre, "salida/articulo_2/dunn_Cierre_por_Municipio.csv", row.names = FALSE)

ggplot(base_periodos %>% filter(!is.na(Atención), !is.na(Periodo), !is.na(Municipio)),
       aes(x = Periodo, y = Atención, fill = Periodo)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.7) +
  geom_jitter(aes(color = Periodo),
              width = 0.15, size = 1.5, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~Municipio, ncol = 3, scales = "free_y") +
 # cale_fill_manual(values = col_periodo) +
# scale_color_manual(values = col_periodo) +
  labs(
    title = "Retraso en Atención por Periodo y Municipio",
    x = "Periodo", y = "Días de retraso (Atención)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"))
    
    
### Hago por MESe-------------------------
base_MESe <- base_periodos %>%
  mutate(
    MES_Epi = case_when( is.na (MES_Epi) ~ "No",
                         MES_Epi == 1 ~ "Si"),
    Municipio = as.factor(Municipio),
    MES_Epi = as.factor(MES_Epi),
    Cierre = as.numeric(Temporalidad_cln),
    Diagnostico  = as.numeric(Retraso_Lab_cln),
    Atención  = as.numeric(Retraso_Not_cln),
    Completitud =  case_when  (
      Completitud_redu_cat == "excelente" ~ "Si",
      TRUE ~ "No"),
    Consistencia = as.factor(Consistencia)
  )


# 3) Tabla: Periodo (~ variables), estratificada por Municipio
T1.MESe <- tableby(
  MES_Epi ~ Diagnostico + Atención +
    Completitud + Consistencia,
  data   = base_MESe,
  strata = Municipio,
  control = ctrl
)

summary(T1.MESe, text=TRUE)

#Salida
arsenal::write2word(T1.MESe,
                    file = "salida/articulo_2/T2_Mes_EPI_por_municipio.doc",
                    title = "Tabla 1. Comparación por Mes Epi (estratificada por Municipio)",
                    keep.md = TRUE, quiet = TRUE   )

###Hago tabla Cierre
base_MESe_IGU <- base_MESe %>% 
  filter(pais == "brasil") 

# 2) Control de tabla: estadísticas y tests
ctrl <- tableby.control(
  test = TRUE, total = TRUE,
  numeric.stats = c("meansd"),
  cat.stats     = c("countpct"),
  numeric.test  = "kwt",       
  cat.test      = "chisq",     # Chi-cuadrado
  simulate.p    = TRUE)

# 3) Tabla: Periodo (~ variables), estratificada por Municipio
T1.periodo.cierre <- tableby(
  MES_Epi ~ Cierre ,
  data   = base_MESe_IGU,
  strata = Municipio,
  control = ctrl
)

summary(T1.periodo.cierre, text=TRUE)

#Salida
arsenal::write2word(T1.periodo.cierre,
                    file = "salida/articulo_2/T2_Mes_Epi_por_municipio_cierre.doc",
                    title = "Tabla 1. Comparación por Periodo (estratificada por Municipio)",
                    keep.md = TRUE, quiet = TRUE    )

###Hago tabla VPP 
resumen_indicadores_municipio_periodos <- resumen_indicadores_municipio_periodos %>% mutate(
  MES_Epi = case_when( is.na (MES_Epi) ~ "No",
                       MES_Epi == 1 ~ "Si") )
# 3) Tabla: Periodo (~ variables), estratificada por Municipio
T1.periodo_vpp <- tableby(
  MES_Epi ~ vpp_pct + positividad_lab ,
  data   = resumen_indicadores_municipio_periodos,
  strata = Municipio,
  control = ctrl
)

summary(T1.periodo_vpp, text=TRUE)

#Salida
arsenal::write2word(T1.periodo_vpp,
                    file = "salida/articulo_2/T2_Mes_Epi_por_municipio_vpp.doc",
                    title = "Tabla 1. Comparación por Periodo (estratificada por Municipio)",
                    keep.md = TRUE, quiet = TRUE    )


