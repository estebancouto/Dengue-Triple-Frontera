###Calculo Rt de Argentina por periodos con  15 o más semanas consecutivas sin tener 5 semanas sin casos
# Ejecutar desde la raíz del proyecto / Run from the project root.
GTmax <- 5
nsim  <- 1000

# Listas para acumular resultados de cada período
res_dyn_list <- list()

#=============================
# PERÍODO 1: 2009 SE>=45 a 2010 SE<=8
#=============================
Base_Rt_ar.1 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2009 & SE >= 45) | (YEAR == 2010 & SE <= 11))

Base_Rt_ar.1$t <- seq_len(nrow(Base_Rt_ar.1))
Tmax <- nrow(Base_Rt_ar.1)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.1,
                         span = min(0.3, 10/Tmax)
                         ))

gt <- mcmapply(
  evalGenTimeDist, 1:Tmax,
  MoreArgs = list(
    a = c(16, 4.3, 1, 1),
    b = c(1/2.69821, 1/0.4623722, 1, 1),
    serT = temp.lo, tt = Base_Rt_ar.1$t, GT.max = GTmax
  )
)

n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax)
gt.lo[1:n, ] <- gt
gt.lo <- t(gt.lo)

mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.1$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)

Rt.lo <- est.R.Temp(
  Base_Rt_ar.1$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
  date = Base_Rt_ar.1$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim
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

Base_Rt_ar_01 <- Base_Rt_ar.1 %>%
  dplyr::left_join(
    df_dyn_01 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P01"
  )

# Guardo directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_01

#=============================
# PERÍODO 2: 2010 SE>=51 a 2011 SE<=17
#=============================
Base_Rt_ar.2 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2010 & SE >= 51) | (YEAR == 2011 & SE <= 20))

Base_Rt_ar.2$t <- seq_len(nrow(Base_Rt_ar.2))
Tmax <- nrow(Base_Rt_ar.2)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.2,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(
  evalGenTimeDist, 1:Tmax,
  MoreArgs = list(
    a = c(16, 4.3, 1, 1),
    b = c(1/2.69821, 1/0.4623722, 1, 1),
    serT = temp.lo, tt = Base_Rt_ar.2$t, GT.max = GTmax
  )
)
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)

mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.2$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.2$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.2$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)

df_dyn_02 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P02")
df_stat_02 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P02")

Base_Rt_ar_02 <- Base_Rt_ar.2 %>%
  dplyr::left_join(
    df_dyn_02 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P02"
  )

# Guardo directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_02

#=============================
# PERÍODO 3: 2011 SE>=36 a 2012 SE<=5
#=============================
Base_Rt_ar.3 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2011 & SE >= 36) | (YEAR == 2012 & SE <= 8))
Base_Rt_ar.3$t <- seq_len(nrow(Base_Rt_ar.3))
Tmax <- nrow(Base_Rt_ar.3)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.3,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.3$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.3$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.3$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.3$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_03 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P03")
df_stat_03 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P03")
Base_Rt_ar_03 <- Base_Rt_ar.3 %>%
  dplyr::left_join(
    df_dyn_03 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P03"
  )

# Guardo directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_03

#=============================
# PERÍODO 4: 2012 SE>=50 a 2013 SE<=24
#=============================
Base_Rt_ar.4<- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2012 & SE >= 50) | (YEAR == 2013 & SE <= 27))
Base_Rt_ar.4$t <- seq_len(nrow(Base_Rt_ar.4))
Tmax <- nrow(Base_Rt_ar.4)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.4,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.4$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.4$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.4$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.4$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_04 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P04")
df_stat_04 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P04")
Base_Rt_ar_04 <- Base_Rt_ar.4%>%
  dplyr::left_join(
    df_dyn_04 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P04"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_04

#=============================
# PERÍODO 5: 2013 SE>=45 a 2014 SE<=8
#=============================
Base_Rt_ar.5 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2013 & SE >= 45) | (YEAR == 2014 & SE <= 11))
Base_Rt_ar.5$t <- seq_len(nrow(Base_Rt_ar.5))
Tmax <- nrow(Base_Rt_ar.5)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.5,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.5$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.5$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.5$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.5$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_05 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P05")
df_stat_05 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P05")
Base_Rt_ar_05 <- Base_Rt_ar.5 %>%
  dplyr::left_join(
    df_dyn_05 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P05"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_05
#=============================
# PERÍODO 6: 2014 SE>=50 a 2015 SE<=14
#=============================
Base_Rt_ar.6 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2014 & SE >= 50) | (YEAR == 2015 & SE <= 17))
Base_Rt_ar.6$t <- seq_len(nrow(Base_Rt_ar.6))
Tmax <- nrow(Base_Rt_ar.6)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.6,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.6$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.6$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.6$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.6$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_06 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P06")
df_stat_06 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P06")
Base_Rt_ar_06 <- Base_Rt_ar.6 %>%
  dplyr::left_join(
    df_dyn_06 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P06"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_06

#=============================
# PERÍODO 7: 2015 SE>=47 a 2016 SE<=13
#=============================
Base_Rt_ar.7 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2015 & SE >= 47) | (YEAR == 2016 & SE <= 16))
Base_Rt_ar.7$t <- seq_len(nrow(Base_Rt_ar.7))
Tmax <- nrow(Base_Rt_ar.7)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.7,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.7$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.7$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.7$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.7$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_07 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P07")
df_stat_07 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P07")
Base_Rt_ar_07 <- Base_Rt_ar.7 %>%
  dplyr::left_join(
    df_dyn_07 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P07"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_07

#=============================
# PERÍODO 8: 2016 SE>=43 a 2016 SE<=52
#=============================
Base_Rt_ar.1 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2016 & SE >= 43) |(YEAR == 2017 & SE <= 2))
Base_Rt_ar.1$t <- seq_len(nrow(Base_Rt_ar.1))
Tmax <- nrow(Base_Rt_ar.1)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.1,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.1$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.1$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.1$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.1$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_08 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P08")
df_stat_08 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P08")
Base_Rt_ar_08 <- Base_Rt_ar.1 %>%
  dplyr::left_join(
    df_dyn_08 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P08"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_08
#=============================
# PERÍODO 9: 2019 SE>=3 a 2019 SE<=20
#=============================
Base_Rt_ar.9 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter(YEAR == 2019 & SE >= 3 & SE <= 23)
Base_Rt_ar.9$t <- seq_len(nrow(Base_Rt_ar.9))
Tmax <- nrow(Base_Rt_ar.9)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.9,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.9$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.9$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.9$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.9$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_09 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P09")
df_stat_09 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P09")
Base_Rt_ar_09 <- Base_Rt_ar.9 %>%
  dplyr::left_join(
    df_dyn_09 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P09"
    
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_09
#=============================
# PERÍODO 10: 2019 SE>=36 a 2020 SE<=26
#=============================
Base_Rt_ar.10 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2019 & SE >= 36) | (YEAR == 2020 & SE <= 29))
Base_Rt_ar.10$t <- seq_len(nrow(Base_Rt_ar.10))
Tmax <- nrow(Base_Rt_ar.10)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.10,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.10$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.10$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.10$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.10$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_10 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P10")
df_stat_10 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P10")
Base_Rt_ar_10 <- Base_Rt_ar.10 %>%
  dplyr::left_join(
    df_dyn_10 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P10"
    
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_10

#=============================
# PERÍODO 11: 2021 SE>=6 a 2021 SE<=18
#=============================
Base_Rt_ar.11 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter(YEAR == 2021 & SE >= 6 & SE <= 21)
Base_Rt_ar.11$t <- seq_len(nrow(Base_Rt_ar.11))
Tmax <- nrow(Base_Rt_ar.11)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.11,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.11$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.11$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.11$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.11$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_11 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P11")
df_stat_11 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P11")
Base_Rt_ar_11 <- Base_Rt_ar.11 %>%
  dplyr::left_join(
    df_dyn_11 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P11"
      )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_11

#=============================
# PERÍODO 12: 2022 SE>=39 a 2022 SE<=46
#=============================
Base_Rt_ar.12 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter(YEAR == 2022 & SE >= 39 & SE <= 52)
Base_Rt_ar.12$t <- seq_len(nrow(Base_Rt_ar.12))
Tmax <- nrow(Base_Rt_ar.12)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.12,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.12$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.12$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.12$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.12$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_12 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P12")
df_stat_12 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P12")

Base_Rt_ar_12 <- Base_Rt_ar.12 %>%
  dplyr::left_join(
    df_dyn_12 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P12"
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_12

#=============================
# PERÍODO 13: 2023 SE>=6 a 2023 SE<=29
#=============================
Base_Rt_ar.13 <- Base_se_TxInc %>%
  select(YEAR, SE, Den_ar_Total_CP, Pop_ar_IGR, Clima_ar_Tempmedia) %>%
  filter(YEAR == 2023 & SE >= 6 & SE <= 32)
Base_Rt_ar.13$t <- seq_len(nrow(Base_Rt_ar.13))
Tmax <- nrow(Base_Rt_ar.13)
temp.lo <- predict(loess(Clima_ar_Tempmedia ~ t,
                         data = Base_Rt_ar.13,
                         span = min(0.3, 10/Tmax)
))

gt <- mcmapply(evalGenTimeDist, 1:Tmax,
               MoreArgs = list(a = c(16, 4.3, 1, 1),
                               b = c(1/2.69821, 1/0.4623722, 1, 1),
                               serT = temp.lo, tt = Base_Rt_ar.13$t, GT.max = GTmax))
n <- GTmax + 1
gt.lo <- matrix(0, ncol = Tmax, nrow = Tmax); gt.lo[1:n, ] <- gt; gt.lo <- t(gt.lo)
mGT <- R0::generation.time("gamma", c(3, 1))
end_TD <- min(500, Tmax)
TD <- R0::est.R0.TD(Base_Rt_ar.13$Den_ar_Total_CP, mGT, begin = 1, end = end_TD, nsim = nsim, correct = FALSE)
Rt.lo <- est.R.Temp(Base_Rt_ar.13$Den_ar_Total_CP[1:Tmax], mGT, GTTemp = gt.lo,
                    date = Base_Rt_ar.13$t[1:Tmax], begin = 1, end = Tmax, correct = FALSE, nsim = nsim)
lT <- length(Rt.lo$R)
df_dyn_13 <- data.frame(R = Rt.lo$R, ymin = Rt.lo$conf.int[1], ymax = Rt.lo$conf.int[2],
                        t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                        dt.inicio = 1:lT, type = "temperature-dependent", period = "P13")
df_stat_13 <- data.frame(R = TD$R, ymin = TD$conf.int[1], ymax = TD$conf.int[2],
                         t = Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                         dt.inicio = 1:lT, type = "temperature-independent", period = "P13")
Base_Rt_ar_13 <- Base_Rt_ar.13 %>%
  dplyr::left_join(
    df_dyn_13 %>% dplyr::select(t, R, lower, upper),
    by = "t"
  ) %>%
  dplyr::transmute(
    YEAR, SE,
    Den_ar_IGR_Total_Rt     = R,
    Den_ar_IGR_Total_Rtlow  = lower,
    Den_ar_IGR_Total_Rtupp  = upper,
    period = "P13"
    
  )

# Guardás directamente el resultado combinado:
res_dyn_list[[length(res_dyn_list) + 1]] <- Base_Rt_ar_13


#---------------------------------------------------------------------
# UNIR TODOS LOS RESULTADOS Y DEFINIR NIVELES DEL PERÍODO
#---------------------------------------------------------------------
Base_Rt_ar_total  <- dplyr::bind_rows(res_dyn_list)

# Periodo como factor con niveles ordenados P01..P13
Base_Rt_ar_total$period <- factor(Base_Rt_ar_total$period,
                               levels = sprintf("P%02d", 1:13),
                               ordered = TRUE)

# Contar niveles (cuántos períodos hay en el total)
n_periodos <- nlevels(Base_Rt_ar_total$period)
n_periodos
# o bien: length(unique(df_result_all$period))

#---------------------------------------------------------------------
# GRAFICAR
#---------------------------------------------------------------------
ggplot(
  Base_Rt_ar_total,
  aes(
    x = SE,  # eje horizontal: semana epidemiológica
    y = Den_ar_IGR_Total_Rt,
    ymin = Den_ar_IGR_Total_Rtlow,
    ymax = Den_ar_IGR_Total_Rtupp
  )
) +
  geom_ribbon(fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = 2, color = "red") +
  facet_wrap(~ period, scales = "free_x", ncol = 4) +
  coord_cartesian(ylim = c(0, 5)) +
  labs(
    x = "Semana epidemiológica (SE)",
    y = "Rt (con intervalos de confianza)",
    title = "Rt estimado por período"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
# Guardar la figura
ggsave("salida/articulo_4/Rt/Rt_por_periodo_Arg.png", width = 10, height = 8, dpi = 300)
