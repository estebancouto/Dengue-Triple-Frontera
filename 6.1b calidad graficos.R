source("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/source/script/6.calidad.R")

# Librerias----
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2, tableone, 
  RColorBrewer, scales, gt, data.table, gridExtra, cowplot,
  R0, tsibble, kableExtra, gtsummary, gt, ggpubr)

# Directorio de trabajo
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")

# Grafico 1 Serie temporal-----
# 0) Rectángulos de periodos (con leyenda)
periodos <- tribble(
  ~periodo, ~xmin,                 ~xmax,
  "PRE",    as.Date("2009-10-01"), as.Date("2020-02-29"),
  "COVID",  as.Date("2020-03-01"), as.Date("2021-09-30"),
  "INTER",  as.Date("2021-10-01"), as.Date("2022-09-30"),
  "CHIKV",  as.Date("2022-10-01"), as.Date("2023-09-30")
)

# 1) Filtro previo (necesario para filter_index)
tsibble_CS <- tsibble_Base_Mensual_Final %>%
  index_by(fecha) %>%
  group_by(Municipio, Tipo) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup() %>%
  as_tsibble(index = fecha, key = c(Municipio, Tipo))

tsibble_CS_filt <- tsibble_CS %>%
  filter_index("2009 Oct" ~ "2023 Sep")

# 2) Etiquetas de países para facet
pais_labels <- c(ar = "Argentina", br = "Brasil", py = "Paraguay")

# 3) Gráfico
graf1 <- ggplot() +
  # bandas por periodo (con leyenda)
  geom_rect(
    data = periodos,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = periodo),
    alpha = 0.2, color = "gray50"
  ) +
  # serie (usar as.Date(fecha) porque el índice es yearmonth)
  geom_line(
    data = tsibble_CS_filt,
    aes(x = as.Date(fecha), y = valor),
    color = "black", linewidth = 1
  ) +
  labs(
    title = "Grafico 1. Casos sospechosos de dengue y periodos",
    subtitle = "Triple Frontera 2009-2023",
    y = "Casos",
    x = NULL,
    caption = "Elaboración propia con datos de SINAN, SNVS y DVGS"
  ) +
  ggthemes::theme_few() +
  theme(
    plot.background   = element_rect(fill = "gray95"),
    panel.background  = element_rect(fill = "gray85"),
    legend.background = element_rect(
      fill = "gray85", linewidth = 1, linetype = "solid", color = "gray50"
    ),
    legend.margin     = margin(6, 6, 6, 6),
    plot.title        = element_text(face = "bold", size = 16),
    legend.position   = "right",
    plot.subtitle     = element_text(face = "bold", size = 12),
    plot.caption      = element_text(face = "bold", size = 8),
    legend.key.width  = unit(1, "cm")
  ) +
  scale_fill_manual(
    values = c(
      "PRE"   = "#B3CDE3",
      "COVID" = "#CCEBC5",
      "INTER" = "#FBB4AE",
      "CHIKV" = "#DECBE4"
    ),
    breaks = c("PRE", "COVID", "INTER", "CHIKV")
  ) +
  guides(fill = guide_legend(title = "Periodo", title.position = "top")) +
  facet_wrap(
    ~Municipio, ncol = 1, strip.position = "top",
    scales = "free_y"
  )


ggsave(
  filename = "salida/articulo_2/Graf1.tiff",
  plot     = graf1,
  width    = 12, height = 12, dpi = 300, units = "in", device = "tiff"
)




# Visualizo outliers----
vars <- c("Retraso_Not", "Retraso_Lab", "Temporalidad")

    
p_all <- ggplot(Den_Calidad_Todos, aes(x = variable, y = vars, fill = Periodo)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.15, position = position_dodge(width = 0.9)) +
  coord_flip() +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(
    title = "Distribución de indicadores (global)",
    x = NULL, y = NULL, fill = "Periodo"
  ) +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank())


#  Boxplots por municipio 
summary(dengue_attr_base$Retraso_Not_outlier)
summary(dengue_attr_base$Retraso_Lab_outlier)
summary(dengue_attr_base$Temporalidad_outlier)

dengue_long <- dengue_attr_base %>%
  select(Municipio,  Retraso_Not_cln, Retraso_Lab_cln, Temporalidad_cln) %>%
  pivot_longer(cols = -Municipio,
               names_to = "Variable",
               values_to = "Dias") %>% filter(!is.na(Dias))

facet_labels <- c(
  "Retraso_Not_cln"  = "Oportunidad atención (días)",
  "Retraso_Lab_cln"  = "Oportunidad de toma de muestra (días)",
  "Temporalidad_cln" = "Cierre de caso 
  (días)"
)

# Boxplots por municipio, con nombres de facet personalizados
outlier1<-ggplot(dengue_long, aes(x = Municipio, y = Dias, fill = as.factor(Variable))) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Variable,
             scales = "free_x",
             labeller = as_labeller(facet_labels)) +
  coord_flip() +
  labs(
    title = "Distribución de retrasos por municipio",
    x = NULL, y = "Días"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
outlier1

ggsave(
  filename = "salida/articulo_2/outlier1_box.tiff",
  plot = outlier1,
  width = 10, height = 6, dpi = 300
)

outlier2<-ggplot(dengue_long, aes(x = Dias, color = Variable)) +
  geom_density(linewidth = 0.8, adjust = 1.2) +
  facet_wrap(~ Municipio, scales = "free_y") +
  scale_color_manual(
    values = c("#1b9e77", "#d95f02", "#7570b3"),
    labels = facet_labels
  ) +  coord_cartesian(xlim = c(0, 20)) + 
  labs(
    title = "Distribución de retrasos (líneas de densidad) por municipio",
    x = "Días",
    y = "Densidad",
    color = "Variable"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

outlier2

ggsave(
  filename = "salida/articulo_2/outlier2_lineas.tiff",
  plot = outlier2,
  width = 10, height = 6, dpi = 300
)

#Histograma de completitud

outlier3<-ggplot(dengue_attr_base, aes(x = Completitud_redu, fill = Completitud_redu_cat)) +
  geom_bar(color = "black", alpha = 0.8) +
  facet_wrap(~ Municipio, scales = "free_y") +
  labs(
    title = "Distribución de categorías de completitud por municipio",
    x = "Categoría de completitud reducida",
    y = "Frecuencia",
    fill = "Completitud"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "salida/articulo_2/outlier3_completitud.tiff",
  plot = outlier3,
  width = 10, height = 6, dpi = 300
)


# Atributos por año (Epi) y país----
# Metas operativas 
meta_not   <- 7   # días FIS→Notificación
meta_lab   <- 14  # días FIS→Laboratorio
meta_cierre<- 60  # días FIS→Cierre

# ---------- CON TODOS (Den_Calidad_Todos
# Por País x Año epidemiológico
resumen_pais_Todos <- Den_Calidad_Todos %>%
  group_by(year_e,YEAR_E, Pais) %>%
  summarise(
    n = n(),
    med_Retraso_Not  = median(Retraso_Not, na.rm = TRUE),
    p75_Retraso_Not  = quantile(Retraso_Not, 0.75, na.rm = TRUE, type = 2),
    pct_Not_en_meta  = mean(Retraso_Not <= meta_not, na.rm = TRUE) * 100,
    
    med_Retraso_Lab  = median(Retraso_Lab, na.rm = TRUE),
    p75_Retraso_Lab  = quantile(Retraso_Lab, 0.75, na.rm = TRUE, type = 2),
    pct_Lab_en_meta  = mean(Retraso_Lab <= meta_lab, na.rm = TRUE) * 100,
    
    med_Temporalidad = median(Temporalidad, na.rm = TRUE),
    p75_Temporalidad = quantile(Temporalidad, 0.75, na.rm = TRUE, type = 2),
    pct_Cierre_en_meta = mean(Temporalidad <= meta_cierre, na.rm = TRUE) * 100,
    
    med_Completitud_redu = median(Completitud_redu, na.rm = TRUE),
    p75_Completitud_redu = quantile(Completitud_redu, 0.75, na.rm = TRUE, type = 2)
      ) %>%
  arrange(Pais, year_e)

# Por País x Municipio x Año epidemiológico (detalle municipal)
resumen_muni_Todos <- Den_Calidad_Todos %>%
  group_by(year_e,, YEAR_E, Pais, Municipio) %>%
  summarise(
    n = n(),
    med_Retraso_Not  = mean(Retraso_Not, na.rm = TRUE),
    pct_Not_en_meta  = mean(Retraso_Not <= meta_not, na.rm = TRUE) * 100,
    med_Retraso_Lab  = mean(Retraso_Lab, na.rm = TRUE),
    pct_Lab_en_meta  = mean(Retraso_Lab <= meta_lab, na.rm = TRUE) * 100,
    med_Temporalidad = mean(Temporalidad, na.rm = TRUE),
    pct_Cierre_en_meta = mean(Temporalidad <= meta_cierre, na.rm = TRUE) * 100,
    med_Completitud_redu  = mean(Completitud_redu, na.rm = TRUE)
  ) %>%
  arrange(Pais, Municipio, YEAR_E) 

# ---------- SIN DESCARTADOS (Den_Calidad_CP
resumen_pais_CP <- Den_Calidad_CP %>%
  group_by(YEAR_E, Pais) %>%
  summarise(
    n = n(),
    med_Retraso_Not  = median(Retraso_Not, na.rm = TRUE),
    p75_Retraso_Not  = quantile(Retraso_Not, 0.75, na.rm = TRUE, type = 2),
    pct_Not_en_meta  = mean(Retraso_Not <= meta_not, na.rm = TRUE) * 100,
    
    med_Retraso_Lab  = median(Retraso_Lab, na.rm = TRUE),
    p75_Retraso_Lab  = quantile(Retraso_Lab, 0.75, na.rm = TRUE, type = 2),
    pct_Lab_en_meta  = mean(Retraso_Lab <= meta_lab, na.rm = TRUE) * 100,
    
    med_Temporalidad = median(Temporalidad, na.rm = TRUE),
    p75_Temporalidad = quantile(Temporalidad, 0.75, na.rm = TRUE, type = 2),
    pct_Cierre_en_meta = mean(Temporalidad <= meta_cierre, na.rm = TRUE) * 100,
    
    med_Completitud_redu = median(Completitud_redu, na.rm = TRUE),
    p75_Completitud_redu = quantile(Completitud_redu, 0.75, na.rm = TRUE, type = 2)) %>%
  arrange(Pais, YEAR_E)

resumen_muni_CP <- Den_Calidad_CP %>%
  group_by(YEAR_E, Pais, Municipio) %>%
  summarise(
    n = n(),
    med_Retraso_Not  = median(Retraso_Not, na.rm = TRUE),
    pct_Not_en_meta  = mean(Retraso_Not <= meta_not, na.rm = TRUE) * 100,
    med_Retraso_Lab  = median(Retraso_Lab, na.rm = TRUE),
    pct_Lab_en_meta  = mean(Retraso_Lab <= meta_lab, na.rm = TRUE) * 100,
    med_Temporalidad = median(Temporalidad, na.rm = TRUE),
    pct_Cierre_en_meta = mean(Temporalidad <= meta_cierre, na.rm = TRUE) * 100,
    med_Completitud_redu  = median(Completitud_redu, na.rm = TRUE)) %>%
  arrange(Pais, Municipio, YEAR_E)

# ---------- Exportación 
write.csv(resumen_pais_Todos,  "salida/articulo_2/resumen_pais_Todos.csv",  row.names = FALSE)
write.csv(resumen_muni_Todos,  "salida/articulo_2/resumen_muni_Todos.csv",  row.names = FALSE)
write.csv(resumen_pais_CP,     "salida/articulo_2/resumen_pais_CP.csv",     row.names = FALSE)
write.csv(resumen_muni_CP,     "salida/articulo_2/resumen_muni_CP.csv",     row.names = FALSE)




# --- Tabla S1 Resumen por año epideimiologico × municipio (Todos)----

# Metas
meta_not    <- 7
meta_lab    <- 14
meta_cierre <- 60

# Resumen por año epidemiológico × municipio (MEDIA + p25–p75)
# --- FLAGS (uno por métrica)
resumen_epi_muni <- Den_Calidad_Todos %>%
  group_by(YEAR_E, Municipio) %>%
  summarise(
    n = n(),
    # Notificación
    mean_Retraso_Not = suppressWarnings(mean(Retraso_Not, na.rm = TRUE)),
    p25_Retraso_Not  = suppressWarnings(quantile(Retraso_Not, 0.25, na.rm = TRUE, type = 2)),
    p75_Retraso_Not  = suppressWarnings(quantile(Retraso_Not, 0.75, na.rm = TRUE, type = 2)),
    pct_Not_en_meta  = mean(Retraso_Not <= meta_not, na.rm = TRUE) * 100,
    # Laboratorio
    mean_Retraso_Lab = suppressWarnings(mean(Retraso_Lab, na.rm = TRUE)),
    p25_Retraso_Lab  = suppressWarnings(quantile(Retraso_Lab, 0.25, na.rm = TRUE, type = 2)),
    p75_Retraso_Lab  = suppressWarnings(quantile(Retraso_Lab, 0.75, na.rm = TRUE, type = 2)),
    pct_Lab_en_meta  = mean(Retraso_Lab <= meta_lab, na.rm = TRUE) * 100,
    # Cierre
    mean_Temporalidad = suppressWarnings(mean(Temporalidad, na.rm = TRUE)),
    p25_Temporalidad  = suppressWarnings(quantile(Temporalidad, 0.25, na.rm = TRUE, type = 2)),
    p75_Temporalidad  = suppressWarnings(quantile(Temporalidad, 0.75, na.rm = TRUE, type = 2)),
    pct_Cierre_en_meta= mean(Temporalidad <= meta_cierre, na.rm = TRUE) * 100,
    # Completitud reducida
    mean_Completitud_red = suppressWarnings(mean(Completitud_redu, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Municipio, YEAR_E) %>%
  mutate(
    across(c(mean_Retraso_Not, p25_Retraso_Not, p75_Retraso_Not,
             mean_Retraso_Lab, p25_Retraso_Lab, p75_Retraso_Lab,
             mean_Temporalidad, p25_Temporalidad, p75_Temporalidad,
             mean_Completitud_red), ~round(., 1)),
    across(c(pct_Not_en_meta, pct_Lab_en_meta, pct_Cierre_en_meta), ~round(., 1)),
    Not_col    = sprintf("%.1f (%.1f–%.1f)", mean_Retraso_Not, p25_Retraso_Not, p75_Retraso_Not),
    Lab_col    = sprintf("%.1f (%.1f–%.1f)", mean_Retraso_Lab, p25_Retraso_Lab, p75_Retraso_Lab),
    Cierre_col = sprintf("%.1f (%.1f–%.1f)", mean_Temporalidad, p25_Temporalidad, p75_Temporalidad)
  ) %>%
  group_by(Municipio) %>%
  mutate(
    # n
    flag_max_n     = n  == max(n,  na.rm = TRUE),
    flag_min_n     = n  == min(n,  na.rm = TRUE),
    # Notificación (lower = mejor, upper = peor)
    flag_best_not  = mean_Retraso_Not == min(mean_Retraso_Not, na.rm = TRUE),
    flag_worst_not = mean_Retraso_Not == max(mean_Retraso_Not, na.rm = TRUE),
    # Laboratorio
    flag_best_lab  = mean_Retraso_Lab == min(mean_Retraso_Lab, na.rm = TRUE),
    flag_worst_lab = mean_Retraso_Lab == max(mean_Retraso_Lab, na.rm = TRUE),
    # Cierre
    flag_best_cie  = mean_Temporalidad == min(mean_Temporalidad, na.rm = TRUE),
    flag_worst_cie = mean_Temporalidad == max(mean_Temporalidad, na.rm = TRUE),
    # Completitud reducida (lower/upper según quieras resaltar; acá: lower=mejor -> cursiva)
    flag_best_comp  = mean_Completitud_red == min(mean_Completitud_red, na.rm = TRUE),
    flag_worst_comp = mean_Completitud_red == max(mean_Completitud_red, na.rm = TRUE)
  ) %>%
  ungroup()


# Guardar UNA IMAGEN POR MUNICIPIO
munis <- unique(resumen_epi_muni$Municipio)

for (m in munis) {
  dfm <- resumen_epi_muni %>% filter(Municipio == m)
  
  # Tabla GT para el municipio m
  tab_m <- dfm %>%
    select(
      YEAR_E, n,
      Not_col, pct_Not_en_meta,
      Lab_col, pct_Lab_en_meta,
      Cierre_col, pct_Cierre_en_meta,
      mean_Completitud_red,
      flag_max_n, flag_min_n,
      flag_best_not,  flag_worst_not,
      flag_best_lab,  flag_worst_lab,
      flag_best_cie,  flag_worst_cie,
      flag_best_comp, flag_worst_comp
    ) %>%
    gt() %>%
    tab_header(
      title    = md(paste0("**Resumen por año epidemiológico — ", m, "**")),
      subtitle = md(paste0(
        "Base: Total de notificaciones. Media (p25–p75). Metas — Not ≤ ", meta_not,
        " d; Lab ≤ ", meta_lab, " d; Cierre ≤ ", meta_cierre, " d."
      ))
    ) %>%
    cols_label(
      YEAR_E         = "Año",
      n                   = "n",
      Not_col             = "Oportunidad Atención\nMedia (p25–p75, d)",
      pct_Not_en_meta     = "% Not ≤ meta",
      Lab_col             = "Oportunidad Laboratorio\nMedia (p25–p75, d)",
      pct_Lab_en_meta     = "% Lab ≤ meta",
      Cierre_col          = "Oportunidad Cierre\nMedia (p25–p75, d)",
      pct_Cierre_en_meta  = "% Cierre ≤ meta",
      mean_Completitud_red= "Completitud\nmedia (%)"
    ) %>%
    fmt_number(columns = c(n), decimals = 0) %>%
    fmt_number(columns = c(pct_Not_en_meta, pct_Lab_en_meta, pct_Cierre_en_meta, mean_Completitud_red),
               decimals = 1) %>%
    # --- Estilos: upper en NEGRITA ---
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = n,        rows = flag_max_n)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Not_col,  rows = flag_worst_not)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Lab_col,  rows = flag_worst_lab)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Cierre_col, rows = flag_worst_cie)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = mean_Completitud_red, rows = flag_worst_comp)) %>%
    # --- Estilos: lower en CURSIVA ---
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(columns = n,       rows = flag_min_n)) %>%
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(columns = Not_col, rows = flag_best_not)) %>%
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(columns = Lab_col, rows = flag_best_lab)) %>%
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(columns = Cierre_col, rows = flag_best_cie)) %>%
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(columns = mean_Completitud_red, rows = flag_best_comp)) %>%
    # Ocultar banderas
    cols_hide(columns = c(flag_max_n, flag_min_n,
                          flag_best_not, flag_worst_not,
                          flag_best_lab, flag_worst_lab,
                          flag_best_cie, flag_worst_cie,
                          flag_best_comp, flag_worst_comp)) %>%
    tab_source_note(md("Elaboración propia con datos de SINAN, SNVS y DGVS.")) %>%
    tab_options(table.font.size = px(12), data_row.padding = px(2))
  
  
  # Nombre de archivo seguro
  safe_name <- str_replace_all(tolower(m), "[^a-z0-9]+", "_")
  gtsave(
    tab_m,
    filename = file.path("salida/articulo_2", paste0("tabla_epiyear_", safe_name, ".png")),
    vwidth = 1600, vheight = 0
  )
}



# --- Tabla S2 Resumen VPP y consistencia por año epideimiologico × municipio (Todos)----
for (m in munis) {
  dfm <- resumen_indicadores %>% filter(Municipio == m)
  
  tab_m <- dfm %>%
    select(
      YEAR_E, n,
      pct_consistencia,
      confirmados, vpp_pct,
      lab_pos, lab_neg, pos_testeados, positividad_lab
    ) %>%
    gt() %>%
    tab_header(
      title    = md(paste0("**Calidad y desempeño — ", m, "**")),
      subtitle = md("Año epidemiológico. Consistencia (%), VPP = confirmados/total (%), Positividad de laboratorio = positivos/(positivos+negativos) (%).")
    ) %>%
    cols_label(
      YEAR_E      = "Año epi",
      n                = "n",
      pct_consistencia = "% Consistencia",
      confirmados      = "Confirmados",
      vpp_pct          = "VPP (%)",
      lab_pos          = "Lab (+)",
      lab_neg          = "Lab (–)",
      pos_testeados    = "Testeados",
      positividad_lab  = "Positividad lab (%)"
    ) %>%
    fmt_number(columns = c(n, confirmados, lab_pos, lab_neg, pos_testeados), decimals = 0) %>%
    fmt_number(columns = c(pct_consistencia, vpp_pct, positividad_lab), decimals = 1) %>%
    tab_source_note(md("Elaboración propia con datos de SINAN, SNVS y DGVS.")) %>%
    tab_options(table.font.size = px(12), data_row.padding = px(2))
  
  safe_name <- stringr::str_replace_all(tolower(m), "[^a-z0-9]+", "_")
  gtsave(
    tab_m,
    filename = file.path("salida/articulo_2", paste0("tabla_indicadores_", safe_name, ".png")),
    vwidth = 1300, vheight = 0
  )
}

#---- Mes epidemiologico----



heatmap_data$MESe <- factor(
  heatmap_data$MESe,
  c( "Oct", "Nov", "Dic", "Ene", "Feb", "Mar", "Abr", "May", "Jun",
    "Jul", "Ago", "Sep")
)

# Plot tipo heatmap
p1 <- ggplot(heatmap_data, aes(x = MESe, y = factor(YEAR_E), fill = MES_Epi)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "gray90", "1" = "firebrick"), guide = "none") +
  labs(title = "Mapa de calor de meses epidémicos por año en la Triple Frontera
       (en rojo meses consecutivos con más de un 20% de positividad)",
       x = "Mes",
       y = "Año epidemiologico",
       fill = "Epidémico") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

p1

ggsave(
  filename = "salida/articulo_2/heatmap_meses_epidemicos.tiff",
  plot = p1,
  width = 10, height = 6, dpi = 300
)

##GRaficos de comparación temporal ---------------
dat <- resumen_periodo %>%
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
    Atención = as.numeric(mean_Retraso_Not),
    Laboratorio  = as.numeric(med_Retraso_Lab),
    VPP          = as.numeric(vpp_pct),
    Consistencia = as.numeric(pct_consistencia),
    Completitud      = as.numeric(pct_completitud)
  ) 

# Lista de variables a describir/analizar
vars_cont <- c("Atención","Laboratorio","VPP","Consistencia","Completitud")
vars_cont <- vars_cont[vars_cont %in% names(dat)] 
# Paleta de colores personalizada (para mantener consistencia)
col_periodo <- c("PRE"   = "#B3CDE3",
                 "COVID" = "#CCEBC5",
                 "INTER" = "#FBB4AE",
                 "CHIKV" = "#DECBE4")

# Loop para generar un gráfico por variable
plots <- lapply(vars_cont, function(vars_cont) {
  ggplot(dat, aes(x = Periodo, y = .data[[vars_cont]], fill = Periodo)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.7) +
    geom_jitter(aes(color = Periodo),
                width = 0.15, size = 1.5, alpha = 0.6, show.legend = FALSE) +
    scale_fill_manual(values = col_periodo) +
    scale_color_manual(values = col_periodo) +
    facet_wrap(~Municipio, ncol = 3, scales = "free_y") +
    # stat_compare_means(method = "kruskal.test",
    #                    label.y = max(dat[[vars_cont]], na.rm = TRUE) * 1.05,
    #                    size = 3.2) +
    labs(
      title = paste("Distribución de", vars_cont, "por periodo y municipio"),
      x = "Periodo",
      y = vars_cont,
      fill = "Periodo"
    ) +
    # theme_pubr(base_size = 12, legend = "right") +
    theme(
      strip.background = element_rect(fill = "grey90", color = "grey60"),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
})

plots

### Con Dunn----

# 1. Preparar datos en formato largo
dat_long <- dat %>%
  pivot_longer(cols = all_of(vars_cont), 
               names_to = "Variable", 
               values_to = "Valor")

# 2. Calcular Test de Dunn
stat_test <- dat_long %>%
  group_by(Municipio, Variable) %>%
  dunn_test(Valor ~ Periodo, p.adjust.method = "BH") %>%
  filter(p.adj < 0.05) 

# 3. Calcular posiciones Y dinámicas (importante para escalas libres)
# Calculamos máximos por faceta para saber dónde poner las barras
max_vals <- dat_long %>%
  group_by(Municipio, Variable) %>%
  summarise(
    max_y = max(Valor, na.rm = TRUE),
    rng = max(Valor, na.rm = TRUE) - min(Valor, na.rm = TRUE),
    .groups = "drop"
  )

stat_test_plot <- stat_test %>%
  inner_join(max_vals, by = c("Municipio", "Variable")) %>%
  group_by(Municipio, Variable) %>%
  arrange(p.adj) %>% # Ordenar para apilar barras si hay varias
  mutate(
    group1 = as.character(group1),
    group2 = as.character(group2),
    step = 0.1 * rng,
    y.position = max_y + step + (step * 0.8 * (row_number() - 1))
  ) %>%
  ungroup() %>%
  mutate( p.adj.signif ="*")

# 4. Generar el Gráfico
# Si stat_test_plot está vacío (sin diferencias significativas), ggplot dará error
# así que ponemos un condicional simple o generamos el plot base primero.

p <- ggplot(dat_long, aes(x = Periodo, y = Valor, fill = Periodo)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width = 0.7) +
  geom_jitter(aes(color = Periodo), width = 0.15, size = 1.0, alpha = 0.6) +
  
  scale_fill_manual(values = col_periodo) +
  scale_color_manual(values = col_periodo) +
  
  facet_grid(Variable ~ Municipio, scales = "free_y") +
  
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "grey80", fill = NA),
        ) +
  labs(y = NULL, x = NULL)

# Solo añadimos la capa de estadísticas si hay diferencias significativas
if(nrow(stat_test_plot) > 0){
  p <- p + stat_pvalue_manual(
    stat_test_plot, 
    label = "p.adj.signif", 
    tip.length = 0.01,
    hide.ns = TRUE,
    inherit.aes = FALSE 
  )
}

p

ggsave(
  filename = "salida/articulo_2/plot_final.tiff",
  plot     = p,
  width    = 12, height = 12, dpi = 300, units = "in", device = "tiff"
)

##########Hago grafico con VPP##

# 1) Armar fecha yearmonth
ts_cs <- resumen_indicadores_municipio %>%
  mutate(
    # separar año inicial y final del YEAR_E tipo "2009-2010"
    year_ini = as.integer(substr(as.character(YEAR_E), 1, 4)),
    year_fin = as.integer(substr(as.character(YEAR_E), 6, 9)),
    # regla: hasta mes 9 es el segundo año, luego el primero
    year = if_else(mes <= 9, year_fin, year_ini),
    fecha = yearmonth(make_date(year, mes, 1))
  ) %>%
  as_tsibble(index = fecha, key = Municipio)

# 2) Factor de escala para “subir” el VPP a la escala de los casos
escala <- max(ts_cs$n, na.rm = TRUE) / 100  # 100 porque el vpp_pct está en %

graf2 <- ggplot() +
  # bandas por periodo
  geom_rect(
    data = periodos,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = periodo),
    alpha = 0.2, color = "gray50"
  ) +
  # serie de CASOS (eje primario)
  geom_line(
    data = ts_cs,
    aes(x = as.Date(fecha), y = n, color = "Casos sospechosos"),
    linewidth = 0.8
  ) +
  # serie de VPP, escalada al eje primario
  geom_line(
    data = ts_cs,
    aes(x = as.Date(fecha), y = vpp_pct * escala, color = "VPP mensual"),
    linewidth = 0.8, linetype = "dashed"
  ) +
  scale_y_continuous(
    name = "Casos sospechosos de dengue",
    sec.axis = sec_axis(~ . / escala, name = "VPP mensual (%)")
  ) +
  scale_fill_manual(
    values = c(
      "PRE"   = "#B3CDE3",
      "COVID" = "#CCEBC5",
      "INTER" = "#FBB4AE",
      "CHIKV" = "#DECBE4"
    ),
    breaks = c("PRE", "COVID", "INTER", "CHIKV")
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Casos sospechosos" = "black",
      "VPP mensual"       = "red"
    )
  ) +
  labs(
    title    = "Casos sospechosos de dengue y VPP mensual",
    subtitle = "Triple Frontera, 2009–2023",
    x        = NULL,
    caption  = "Elaboración propia con datos de SINAN, SNVS y DGVS"
  ) +
  ggthemes::theme_few() +
  theme(
    plot.background   = element_rect(fill = "gray95"),
    panel.background  = element_rect(fill = "gray85"),
    legend.background = element_rect(
      fill = "gray85", linewidth = 1, linetype = "solid", color = "gray50"
    ),
    legend.position   = "right",
    legend.margin     = margin(6, 6, 6, 6),
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(face = "bold", size = 12),
    plot.caption      = element_text(face = "bold", size = 8),
    legend.key.width  = unit(1, "cm")
  ) +
  guides(
    fill  = guide_legend(title = "Periodo", title.position = "top"),
    color = guide_legend(title = NULL,     title.position = "top")
  ) +
  facet_wrap(
    ~ Municipio,
    ncol   = 1,
    scales = "free_y",
    strip.position = "top"
  )


graf2

ggsave(
  filename = "salida/articulo_2/Graf1.tiff",
  plot     = graf2,
  width    = 12, height = 12, dpi = 300, units = "in", device = "tiff"
)
