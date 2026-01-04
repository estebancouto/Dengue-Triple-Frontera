####Uno todas las bases de datos###
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")

#Paquetes
pacman::p_load(
  fpp3, writexl, stringr
)

#Cargo bases ----
dengue_se_total<-read.csv2( "source/csv/dengue_se_art_4.csv")
distritos <- c(
  "Puerto Iguazú" = "Puerto Iguazú (Arg.)",
  "Foz do Iguaçu" = "Foz do Iguaçu (Bra.)",
  "Cdad. del Este"= "Cdad. del Este (Par.)",
  "Hernandarias" = "Hernandarias (Par.)",
  "Minga Guazú"  = "Minga Guazú (Par.)",
  "Pte. Franco"  = "Pte. Franco (Par.)"
)
dengue_se_total<-dengue_se_total %>%
  mutate(
    distrito = recode(distrito, !!!distritos),
    distrito = factor(distrito,
                      levels = unname(distritos))  # orden según el vector
  )


dengue_rt_total<-read.csv("source/csv/tabataba/Base_Rt_municipios_semanal_CP.csv")

dengue_rt_total2 <- dengue_rt_total %>%
  select(-X) %>%   # elimino índice inútil
  mutate(
    pais = recode(
      pais,
      "ar" = "argentina"  )
  ) %>%
  rename(distrito = dist)   # mismo nombre que en dengue_se_total

# 2) Unir por pais + distrito + yw
dengue_merged <- dengue_se_total %>%
  left_join(
    dengue_rt_total2 %>%
      select( distrito, yw, Rt, Rtlow, Rtupp, rt_sostenido, epiyear_lab, incidencia),
    by = c( "distrito", "yw")
  )  %>%   mutate (yw=yearweek(yw))



tabataba_ar<-read.csv( "salida/articulo_4/Indicadores/Epi_features_Arg.csv")
tabataba_br<-read.csv( "salida/articulo_4/Indicadores/Epi_features_BR.csv")
tabataba_mg<-read.csv( "salida/articulo_4/Indicadores/Epi_features_PY_MG.csv")
tabataba_her<-read.csv( "salida/articulo_4/Indicadores/Epi_features_PY_HER.csv")
tabataba_pf<-read.csv( "salida/articulo_4/Indicadores/Epi_features_PY_PF.csv")
tabataba_cde<-read.csv( "salida/articulo_4/Indicadores/Epi_features_PY_CDE.csv")

#----Hago Tsibble ----
#Hago Tsibble
ini <- yearweek("2009 W40")
fin <- yearweek("2023 W39")
idx <- seq(ini, fin, by = 1)

# 1) Claves esperadas (todos los distritos)
keys <- dengue_merged %>% distinct(pais, distrito)

dengue_ts <- expand_grid(keys, yw = idx) %>%
  left_join(dengue_merged, by = c("pais","distrito","yw")) %>% 
  as_tsibble(
    key = c(pais, distrito),
    index = yw  ) %>%
  fill_gaps() %>%
  mutate(
    across(
      where(is.numeric),
      ~ replace_na(., 0)
    )
  )   %>% 
  filter (yw >= yearweek("2009 W40"), !is.na(pais)) %>% select(-c(YEAR_SE, se)) %>%
filter(!grepl(" W53$", as.character(yw)))

#---Hago Tabataba agrupada----
tabataba_all_long <- bind_rows(
  "Puerto Iguazú"  = tabataba_ar,
  "Foz do Iguaçu"  = tabataba_br,
  "Minga Guazú"    = tabataba_mg,
  "Hernandarias"   = tabataba_her,
  "Pte. Franco"    = tabataba_pf,
  "Cdad. del Este" = tabataba_cde,
  .id              = "distrito"   # <--- acá ya sale con ese nombre
)  %>%
  mutate(
    distrito = recode(distrito, !!!distritos),
    distrito = factor(distrito,
                      levels = unname(distritos))  # orden según el vector
  )

#--- Hago flechas para graficos con fechas----
flechas <- tabataba_all_long %>%
  select(distrito, ANIO_EPI_LAB, Comienzo_SE, Pico_SE, DcFin) %>%
  pivot_longer(
    cols      = c(Comienzo_SE, Pico_SE, DcFin),
    names_to  = "tipo",
    values_to = "se_lab"
  ) %>%
  filter(!is.na(se_lab)) %>%
  separate(se_lab, into = c("SE", "YEAR"), sep = " - ", convert = TRUE) %>%
  mutate(
    yw   = yearweek(paste0(YEAR, " W", sprintf("%02d", SE))),
    tipo = dplyr::recode(
      tipo,
      Comienzo_SE = "inicio",
      Pico_SE     = "pico",
      DcFin       = "fin"
    )
    
  )

#Agrego las fechas que no estan por ser año previo
extra_inicios <- tibble::tribble(
  ~distrito,               ~SE, ~YEAR,
  "Pte. Franco (Par.)",    38L, 2013L,  # 38-2013**
  "Cdad. del Este (Par.)", 37L, 2013L,  # 37-2013**
  "Minga Guazú (Par.)",    35L, 2015L,  # 35-2015**
  "Pte. Franco (Par.)",    39L, 2015L,  # 39-2015**
  "Cdad. del Este (Par.)", 32L, 2015L,  # 32-2015**
  "Foz do Iguaçu (Bra.)",  30L, 2019L,  # 30-2019**
  "Cdad. del Este (Par.)", 37L, 2020L,  # 37-2020**
  "Minga Guazú (Par.)",    38L, 2021L   # 38-2021**
) %>%
  mutate(
    ANIO_EPI_LAB = if_else(
      SE >= 30,
      paste0(YEAR, "-", YEAR + 1),
      paste0(YEAR - 1, "-", YEAR)
    ),
    tipo = "inicio",
    yw   = yearweek(paste0(YEAR, " W", sprintf("%02d", SE)))
  ) %>%
  select(distrito, ANIO_EPI_LAB, tipo, SE, YEAR, yw)

flechas <- flechas %>%
  bind_rows(extra_inicios) %>%
  arrange(distrito, ANIO_EPI_LAB, yw) %>%
  filter(!(YEAR == 2021 & SE == 21)) %>%
  mutate(
    distrito = recode(distrito, !!!distritos),
    distrito = factor(distrito,
                      levels = unname(distritos)),
    # lo paso a factor para controlar orden en la leyenda
    tipo = factor(tipo, levels = c("inicio", "pico", "fin"))
  )

# Genero tabla de sombras para Rt sostenido ----
sombras_rt <- dengue_ts %>%
  as_tibble() %>%                          # salir del tsibble
  arrange(pais, distrito, yw) %>%
  group_by(pais, distrito, epiyear_lab) %>%
  mutate(
    flag   = replace_na(rt_sostenido, FALSE),                 # TRUE cuando Rt_sostenido
    cambio = flag != lag(flag, default = first(flag)),        # cambios TRUE/FALSE
    bloque = cumsum(cambio)                                   # numera bloques consecutivos
  ) %>%
  filter(flag) %>%                                            # solo semanas con Rt_sostenido == TRUE
  group_by(pais, distrito, epiyear_lab, bloque) %>%
  summarise(
    xmin  = yearweek(min(yw)),
    xmax  = yearweek(max(yw)),
    n_sem = n(),                                              # duración del bloque
    .groups = "drop"
  )


epiyears <- sort(unique(sombras_rt$epiyear_lab))
col_distrito <- c(
  "Puerto Iguazú (Arg.)"  = "#56B4E9", # celeste
  "Foz do Iguaçu (Bra.)"  = "#009E73", # verde
  "Cdad. del Este (Par.)" = "#F8766D", # rosa/rojo
  "Hernandarias (Par.)"   = "#D55E00", # naranja rojizo
  "Minga Guazú (Par.)"   = "#E7298A", # fucsia
  "Pte. Franco (Par.)"    = "#B2182B"  # rojo más oscuro
)



#---2009-2011  ----


## Bases por periodo ----
dengue_09_11 <- dengue_ts %>%
  filter_index("2009 W40" ~ "2011 W39") %>% 
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  ) 

sombras_rt_09_11 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2009-2010", "2010-2011")) %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_09_11$distrito))
  )

flechas_09_11 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2009-2010", "2010-2011")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

#Elijo eje 
brks <- seq(min(dengue_09_11$yw), max(dengue_09_11$yw), by = 1)
brks <- brks[isoweek(as.Date(brks)) %in% c(40)]
brks <- brks[isoweek(as.Date(brks)) != 53]

###Grafico 2009-2011 ----
Graf_2009<-ggplot(dengue_09_11, aes(x = yw, y = incidencia)) +
  
  # Sombras: Rt sostenido por distrito, sin leyenda
  geom_rect(
    data  = sombras_rt_09_11,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito          # fill = distrito
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  
  # Curva de incidencia
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
  # Puntos sobre el eje X, coloreados por tipo (NO usan fill)
  geom_point(
    data = flechas_09_11,
    aes(
      x      = yw,
      y      = -5,
      colour = tipo            # sólo colour
    ),
    inherit.aes = FALSE,
    shape  = 17,               # triángulo sólido que usa colour
    size   = 2.5
  ) +
  
  facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  # Colores de CIUDADES (sombras)
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
  
  # Colores de TIPOS (flechas)
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",  # verde
      pico   = "#377eb8",  # azul
      fin    = "#e41a1c"   # rojo
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2009-2011",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana epidemiológica - Año",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_yearweek(
    breaks = brks,
    labels = function(x) paste0("W", sprintf("%02d", isoweek(as.Date(x))), "-", year(as.Date(x)))
  )+ 
  scale_x_yearweek(date_breaks="20 weeks", date_labels = "%U-%Y") 

Graf_2009

ggsave(Graf_2009,
  filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2009_2011.png",
  width    = 18,
  height   = 24,
  units    = "cm",
  dpi      = 300
)

#--- Tabataba 2009-2011 ----
tabataba_2009_2011 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB  %in% c("2009-2010", "2010-2011"))%>% arrange(ANIO_EPI_LAB)


#---2011-2014  ----


##Bases por periodo ----
dengue_11_13 <- dengue_ts %>%
  filter_index("2011 W40" ~ "2014 W39")%>%
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  )

sombras_rt_11_13 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2011-2012", "2012-2013", "2013-2014")) %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_11_13$distrito))
  )

flechas_11_13 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2011-2012", "2012-2013", "2013-2014")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

###Grafico 2011-2013 ----
Graf_2011<-ggplot(dengue_11_13, aes(x = yw, y = incidencia)) +
   geom_rect(
    data  = sombras_rt_11_13,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito          
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
    geom_point(
    data = flechas_11_13,
    aes(
      x      = yw,
      y      = -5,
      colour = tipo            
    ),
    inherit.aes = FALSE,
    shape  = 17,               
    size   = 2.5
  ) +
   facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  # Colores de CIUDADES (sombras)
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
    # Colores de TIPOS (flechas)
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",  # verde
      pico   = "#377eb8",  # azul
      fin    = "#e41a1c"   # rojo
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2011-2014",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana epidemiológica - Año",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )+ 
  scale_x_yearweek(date_breaks="20 weeks", date_labels = "%U-%Y") 

Graf_2011

ggsave(Graf_2011,
       filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2011_2013.png",
       width    = 18,
       height   = 24,
       units    = "cm",
       dpi      = 300
)

#--- Tabataba 2011-2014 ----
tabataba_2011_2013 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB  %in% c("2011-2012", "2012-2013", "2013-2014"))%>% arrange(ANIO_EPI_LAB)


#---2014-2016 ----


##Base por periodo ----
dengue_14_16 <- dengue_ts %>%
  filter_index("2014 W40" ~ "2016 W39")%>%
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  )

sombras_rt_14_16 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2014-2015", "2015-2016")) %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_14_16$distrito))
  )

flechas_14_16 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2014-2015", "2015-2016")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

#Elijo eje 
brks <- seq(min(dengue_14_16$yw), max(dengue_14_16$yw), by = 1)
brks <- brks[isoweek(as.Date(brks)) %in% c(40)]
brks <- brks[isoweek(as.Date(brks)) != 53]



###Grafico 2014-2016 ----
Graf_2014<-ggplot(dengue_14_16, aes(x = yw, y = incidencia)) +
  
  # Sombras: Rt sostenido por distrito, sin leyenda
  geom_rect(
    data  = sombras_rt_14_16,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito          # fill = distrito
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  
  # Curva de incidencia
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
  # Puntos sobre el eje X, coloreados por tipo (NO usan fill)
  geom_point(
    data = flechas_14_16,
    aes(
      x      = yw,
      y      = -5,
      colour = tipo            # sólo colour
    ),
    inherit.aes = FALSE,
    shape  = 17,               # triángulo sólido que usa colour
    size   = 2.5
  ) +
  
  facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  # Colores de CIUDADES (sombras)
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
  
  # Colores de TIPOS (flechas)
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",  # verde
      pico   = "#377eb8",  # azul
      fin    = "#e41a1c"   # rojo
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2014-2016 ",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + 
  scale_x_yearweek(date_breaks="20 weeks", date_labels = "%U-%Y") 
  

Graf_2014

ggsave(Graf_2014,
       filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2014_2016.png",
       width    = 18,
       height   = 24,
       units    = "cm",
       dpi      = 300
)

#--- Tabataba 2014-2016 ----
tabataba_2014_2016 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB  %in% c("2014-2015", "2015-2016"))%>% arrange(ANIO_EPI_LAB)

#--- 2016-2018 ----

## 1) Bases por periodo ----
dengue_16_18 <- dengue_ts %>%
  filter_index("2016 W40" ~ "2018 W39") %>%   # 2016-2017 y 2017-2018
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  )

sombras_rt_16_18 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2016-2017", "2017-2018")) %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_16_18$distrito))
  )

flechas_16_18 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2016-2017", "2017-2018")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

### Gráfico 2016-2018 ----
Graf_2016_2018 <- ggplot(dengue_16_18, aes(x = yw, y = incidencia)) +
  
  geom_rect(
    data  = sombras_rt_16_18,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
  geom_point(
    data = flechas_16_18,
    aes(
      x      = yw,
      y      = -1,
      colour = tipo
    ),
    inherit.aes = FALSE,
    shape  = 17,
    size   = 2.5
  ) +
  
  facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
  
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",
      pico   = "#377eb8",
      fin    = "#e41a1c"
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2016–2018",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana epidemiológica - Año",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )+ 
  scale_x_yearweek(date_breaks="20 weeks", date_labels = "%U-%Y") 

Graf_2016_2018

ggsave(
  filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2016_2018.png",
  plot     = Graf_2016_2018,
  width    = 18,
  height   = 24,
  units    = "cm",
  dpi      = 300
)

#--- Tabataba 2016-2018 ----
tabataba_2016_2018 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB %in% c("2016-2017", "2017-2018")) %>%
  arrange(ANIO_EPI_LAB)

#--- 2018-2021 ----

## 1) Bases por periodo ----
dengue_18_21 <- dengue_ts %>%
  filter_index("2018 W40" ~ "2021 W39") %>%   # 2018-2019, 2019-2020 y 2020-2021
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  )

sombras_rt_18_21 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2018-2019", "2019-2020", "2020-2021")) %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_18_21$distrito))
  )

flechas_18_21 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2018-2019", "2019-2020", "2020-2021")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

### Gráfico 2018-2021 ----
Graf_2018_2021 <- ggplot(dengue_18_21, aes(x = yw, y = incidencia)) +
  
  geom_rect(
    data  = sombras_rt_18_21,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
  geom_point(
    data = flechas_18_21,
    aes(
      x      = yw,
      y      = -5,
      colour = tipo
    ),
    inherit.aes = FALSE,
    shape  = 17,
    size   = 2.5
  ) +
  
  facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
  
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",
      pico   = "#377eb8",
      fin    = "#e41a1c"
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2018–2021",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana epidemiológica - Año",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + 
  scale_x_yearweek(date_breaks="20 weeks", date_labels = "%U-%Y") 

Graf_2018_2021

ggsave(
  filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2018_2021.png",
  plot     = Graf_2018_2021,
  width    = 18,
  height   = 24,
  units    = "cm",
  dpi      = 300
)

#--- Tabataba 2018-2021 ----
tabataba_2018_2021 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB %in% c("2018-2019", "2019-2020", "2020-2021")) %>%
  arrange(ANIO_EPI_LAB)

#--- 2018-2021 ----

## 1) Bases por periodo ----
dengue_18_21 <- dengue_ts %>%
  filter_index("2018 W40" ~ "2021 W39") %>%   # 2018-2019, 2019-2020 y 2020-2021
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  )

sombras_rt_18_21 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2018-2019", "2019-2020", "2020-2021")) %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_18_21$distrito))
  )

flechas_18_21 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2018-2019", "2019-2020", "2020-2021")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

### Gráfico 2018-2021 ----
Graf_2018_2021 <- ggplot(dengue_18_21, aes(x = yw, y = incidencia)) +
  
  geom_rect(
    data  = sombras_rt_18_21,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
  geom_point(
    data = flechas_18_21,
    aes(
      x      = yw,
      y      = -5,
      colour = tipo
    ),
    inherit.aes = FALSE,
    shape  = 17,
    size   = 2.5
  ) +
  
  facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
  
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",
      pico   = "#377eb8",
      fin    = "#e41a1c"
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2018–2021",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana epidemiológica - Año",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

Graf_2018_2021

ggsave(
  filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2018_2021.png",
  plot     = Graf_2018_2021,
  width    = 18,
  height   = 24,
  units    = "cm",
  dpi      = 300
)

#--- Tabataba 2018-2021 ----
tabataba_2018_2021 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB %in% c("2018-2019", "2019-2020", "2020-2021")) %>%
  arrange(ANIO_EPI_LAB)
#--- 2021-2023 ----

## 1) Bases por periodo ----
dengue_21_23 <- dengue_ts %>%
  filter_index("2021 W40" ~ "2023 W39") %>%   # 2021-2022 y 2022-2023
  mutate(
    distrito = factor(distrito, levels = unique(distrito))
  )

sombras_rt_21_23 <- sombras_rt %>%
  filter(epiyear_lab %in% c("2021-2022","2022-2023"))  %>%
  mutate(
    distrito = factor(distrito, levels = unique(dengue_21_23$distrito))
  )

flechas_21_23 <- flechas %>% 
  filter(ANIO_EPI_LAB %in% c("2021-2022","2022-2023")) %>%
  rename(epiyear_lab = ANIO_EPI_LAB)

### Gráfico 2021-2023 ----
Graf_2021_2023 <- ggplot(dengue_21_23, aes(x = yw, y = incidencia)) +
  
  geom_rect(
    data  = sombras_rt_21_23,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0, ymax = Inf,
      fill = distrito
    ),
    inherit.aes = FALSE,
    alpha  = 0.4,
    colour = NA
  ) +
  
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  
  geom_point(
    data = flechas_21_23,
    aes(
      x      = yw,
      y      = -5,
      colour = tipo
    ),
    inherit.aes = FALSE,
    shape  = 17,
    size   = 2.5
  ) +
  
  facet_wrap(
    ~ distrito,
    ncol   = 1,
    scales = "fixed"
  ) +
  
  scale_fill_manual(
    values = col_distrito,
    guide  = "none"
  ) +
  
  scale_colour_manual(
    values = c(
      inicio = "#1b9e77",
      pico   = "#377eb8",
      fin    = "#e41a1c"
    ),
    name = "Marca"
  ) +
  
  labs(
    title    = "Incidencia semanal, Rt y fechas clave 2021–2023",
    subtitle = "Sombras: Rt sostenido > 1; marcas sobre el eje X: inicio, pico y fin del periodo",
    x        = "Semana epidemiológica - Año",
    y        = "Incidencia (por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )+ 
  scale_x_yearweek(date_breaks="20 weeks", date_labels = "%U-%Y") 

Graf_2021_2023

ggsave(
  filename = "salida/articulo_4/Figuras/Figura_Rt_sostenido_2021_2023.png",
  plot     = Graf_2021_2023,   # CORREGIDO
  width    = 18,
  height   = 24,
  units    = "cm",
  dpi      = 300
)

#--- Tabataba 2021-2023 ----
tabataba_2021_2023 <- tabataba_all_long %>%
  filter(ANIO_EPI_LAB %in% c("2021-2022","2022-2023"))  %>%
  arrange(ANIO_EPI_LAB)



#--- Guardo todas las tablas Tabataba juntas ----
write_xlsx(
  list(
    `2009_2011` = tabataba_2009_2011,
    `2011_2013` = tabataba_2011_2013,
    `2014_2016` = tabataba_2014_2016,
    `2016_2018` = tabataba_2016_2018,
    `2018_2021` = tabataba_2018_2021,
    `2021_2023` = tabataba_2021_2023
  ),
  path = "salida/articulo_4/Indicadores/Epi_features_todos_años.xlsx"
)


#--- Grafico serotipos ----- 


dengue_total<-read.csv2("source/csv/armo/dengue_total.csv")

df_long <- dengue_total %>%
  filter(!is.na(serotipo),
         pais %in% c("argentina", "brasil", "paraguay")) %>%
  
  # 2) Limpiamos etiquetas de país y serotipo
  mutate(
    pais = case_when(
      pais == "paraguay" ~ "Paraguay",
      pais == "brasil"   ~ "Brasil",
      pais == "argentina"~ "Argentina",
      TRUE               ~ NA_character_
    ),
    serotipo = str_to_upper(str_trim(serotipo)),
    serotipo = factor(
      serotipo,
      levels = c("DEN1", "DEN2", "DEN3", "DEN4")
    )
  ) %>%
    group_by(year, pais, serotipo) %>%
  summarise(total = n(), .groups = "drop")

all_years <- sort(unique(dengue_total$year))

Serotipos<- ggplot(df_long, aes(x = year, y = pais,
                    color = pais, size = total)) +
  geom_point(alpha = 0.8,
             position = position_dodge(width = 0.6)) +
  facet_wrap(~ serotipo, ncol = 1) +
  scale_color_manual(values = c(
    "Paraguay"  = "#FF6666",
    "Brasil"    = "#66CC66",
    "Argentina" = "#6699FF"
  )) +
  scale_size_continuous(range = c(2, 8)) +
  scale_x_continuous(breaks = all_years) +
  labs(
    title    = "Figura 2. Serotipo de dengue",
    subtitle = "Triple frontera, 2009–2023",
    x        = "Año",
    y        = "",
    color    = "País",
    size     = "Casos"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text      = element_text(face = "bold", size = 12),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    panel.spacing.y = unit(0.8, "lines")
  )

Serotipos

ggsave( filename = "salida/articulo_4/Figuras/Serotipos.png",
        plot     = Serotipos,  
        width    = 18,
        height   = 24,
        units    = "cm",
        dpi      = 300
)
