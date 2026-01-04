###Calculo Rt de Alto Parana Paraguay por periodos con  15 o más semanas consecutivas sin tener 5 semanas sin casos
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")

GTmax <- 5
nsim  <- 1000

# Listas para acumular resultados de cada período
res_dyn_list <- list()

Base_se_TxInc<- Base_se_TxInc %>%   mutate(POP_AP = Pop_py_HERNANDARIAS + Pop_py_CDE + Pop_py_MG + Pop_py_PF)


#=============================
# PERÍODO 1: 2009 SE>=46 a 2011 SE<=25
#=============================
Base_Rt_ap.1 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2009 & SE >= 46) |(YEAR == 2010)| (YEAR == 2011 & SE <= 28))

Base_Rt_ap.1$t <- seq_len(nrow(Base_Rt_ap.1))
Tmax <- nrow(Base_Rt_ap.1)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.1,
                         span = min(0.3, 10/Tmax)
                         ))

gt <- mcmapply(
  evalGenTimeDist, 1:Tmax,
  MoreArgs = list(
    a = c(16, 4.3, 1, 1),
    b = c(1/2.69821, 1/0.4623722, 1, 1),
    serT = temp.lo, tt = Base_Rt_ap.1$t, GT.max = GTmax
  )
)

n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax)
gt.lo[1:n, ] <- gt
gt.lo <- t(gt.lo)

mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.1$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)

Rt.lo <- est.R.Temp(
  Base_Rt_ap.1$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
  date = Base_Rt_ap.1$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim
)

lT <- length(Rt.lo$R)

df_dyn_01 <- data.frame(
  R = Rt.lo$R,
  ymin = Rt.lo$conf.int[1],
  ymax = Rt.lo$conf.int[2],
  t = Rt.lo$date[1:lT],
  incid = Rt.lo$epid$incid,
  dt.inicio = 1:lT,
  type = "temperature-dependent",
  period = "P01"
)

df_stat_01 <- data.frame(
  R = TD$R,
  ymin = TD$conf.int[1],
  ymax = TD$conf.int[2],
  t = Rt.lo$date[1:lT],
  incid = Rt.lo$epid$incid,
  dt.inicio = 1:lT,
  type = "temperature-independent",
  period = "P01"
)

Base_Rt_ap_01 <- Base_Rt_ap.1 %>%
  dplyr::left_join(
    df_dyn_01 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P01"
  )

# Guardo directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_01

#=============================
# PERÍODO 2: 2011 SE>=45 a 2012 SE<=27
#=============================
Base_Rt_ap.2 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2011 & SE >= 45) | (YEAR == 2012 & SE <= 30))

Base_Rt_ap.2$t <- seq_len(nrow(Base_Rt_ap.2))
Tmax <- nrow(Base_Rt_ap.2)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.2,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(
  evalGenTimeDist, 1:Tmax,
  MoreArgs = list(
    a = c(16, 4.3, 1, 1),
    b = c(1/2.69821, 1/0.4623722, 1, 1),
    serT = temp.lo, tt = Base_Rt_ap.2$t, GT.max = GTmax
  )
)
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)

mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.2$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ap.2$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ap.2$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)

df_dyn_02 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P02")
df_stat_02 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P02")

Base_Rt_ap_02 <- Base_Rt_ap.2 %>%
  dplyr::left_join(
    df_dyn_02 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P02"
  )

# Guardo directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_02

#=============================
# PERÍODO 3: 2012 SE>=36 a 2013 SE<=52
#=============================
Base_Rt_ap.3 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2012 & SE >= 47) | (YEAR == 2013)| (YEAR  == 2014 & SE <= 3))
Base_Rt_ap.3$t <- seq_len(nrow(Base_Rt_ap.3))
Tmax <- nrow(Base_Rt_ap.3)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.3,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ap.3$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.3$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ap.3$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ap.3$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_03 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P03")
df_stat_03 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P03")
Base_Rt_ap_03 <- Base_Rt_ap.3 %>%
  dplyr::left_join(
    df_dyn_03 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P03"
  )

# Guardo directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_03

#=============================
# PERÍODO 4: 2014 SE>=11 a 2014 SE<=22
#=============================
Base_Rt_ap.4<- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2014) & (SE >= 11 & SE <=25)) 
Base_Rt_ap.4$t <- seq_len(nrow(Base_Rt_ap.4))
Tmax <- nrow(Base_Rt_ap.4)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.4,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ap.4$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.4$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ap.4$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ap.4$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_04 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P04")
df_stat_04 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P04")
Base_Rt_ap_04 <- Base_Rt_ap.4%>%
  dplyr::left_join(
    df_dyn_04 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P04"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_04

#=============================
# PERÍODO 5: 2015 SE>=1 a 2021 SE<=52
#=============================
Base_Rt_ap.5 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR >= 2015 & YEAR <= 2021) | (YEAR == 2022 & SE <= 3))
Base_Rt_ap.5$t <- seq_len(nrow(Base_Rt_ap.5))
Tmax <- nrow(Base_Rt_ap.5)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.5,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ap.5$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.5$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ap.5$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ap.5$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_05 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P05")
df_stat_05 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P05")
Base_Rt_ap_05 <- Base_Rt_ap.5 %>%
  dplyr::left_join(
    df_dyn_05 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P05"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_05
#=============================
# PERÍODO 6: 2022 SE>=9 a 2022 SE<=31
#=============================
Base_Rt_ap.6 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2022) & (SE >= 9 & SE <=34)) 
Base_Rt_ap.6$t <- seq_len(nrow(Base_Rt_ap.6))
Tmax <- nrow(Base_Rt_ap.6)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.6,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ap.6$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.6$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ap.6$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ap.6$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_06 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P06")
df_stat_06 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P06")
Base_Rt_ap_06 <- Base_Rt_ap.6 %>%
  dplyr::left_join(
    df_dyn_06 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P06"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_06

#=============================
# PERÍODO 7: 2022 SE>=40 a 2023 SE<=39
#=============================
Base_Rt_ap.7 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_py_AP_Total_CP, POP_AP, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2022 & SE >= 40) | (YEAR == 2023 & SE <= 43))
Base_Rt_ap.7$t <- seq_len(nrow(Base_Rt_ap.7))
Tmax <- nrow(Base_Rt_ap.7)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ap.7,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ap.7$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ap.7$Den_py_AP_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ap.7$Den_py_AP_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ap.7$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_07 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P07")
df_stat_07 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P07")
Base_Rt_ap_07 <- Base_Rt_ap.7 %>%
  dplyr::left_join(
    df_dyn_07 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_py_AP_Total_Rt     = R,
    Den_py_AP_Total_Rtlow  = lower,
    Den_py_AP_Total_Rtupp  = upper,
    period = "P07"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ap_07



#---------------------------------------------------------------------
# UNIR TODOS LOS RESULTADOS Y DEFINIR NIVELES DEL PERÍODO
#---------------------------------------------------------------------
Base_Rt_ap_total  <- dplyr::bind_rows(res_dyn_list)

# Periodo como factor con niveles ordenados P01..P13
Base_Rt_ap_total$period <- factor(Base_Rt_ap_total$period,
                               levels = sprintf("P%02d", 1:13),
                               ordered = TRUE)

# Contar niveles (cuántos períodos hay en el total)
n_periodos <- nlevels(Base_Rt_ap_total$period)
n_periodos
# o bien: length(unique(df_result_all$period))

#---------------------------------------------------------------------
# GRAFICAR
#---------------------------------------------------------------------
Base_Rt_ap_total <- Base_Rt_ap_total %>%
      mutate(
      iso_week = sprintf("%04d-W%02d-1", YEAR, SE),    
      fecha = ISOweek::ISOweek2date(iso_week)
    )
  

ggplot(
  Base_Rt_ap_total,
  aes(
    x = fecha,
    y = Den_py_AP_Total_Rt,
    ymin = Den_py_AP_Total_Rtlow,
    ymax = Den_py_AP_Total_Rtupp
  )
) +
  geom_ribbon(fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = 2, color = "red") +
  facet_wrap(~ period, scales = "free_x", ncol = 4) +
    coord_cartesian(ylim = c(0, 5)) +
  labs(
    x = "Fecha (inicio de la semana)",
    y = "Rt (con intervalos de confianza)",
    title = "Rt estimado por año y semana epidemiológica"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90")
  )
# Guardar la figura
ggsave("salida/articulo_4/Rt/Rt_por_periodo_AP.png", width = 10, height = 8, dpi = 300)
