####Uno todas las bases de datos###
# Ejecutar desde la raíz del proyecto / Run from the project root.
#Uso scripts de Codeco 2017
source("R/external/EstRtGT_v4.R")
source("R/external/sumgamma_v2.R")


#Paquetes----
pacman::p_load(
  tidyverse, lubridate, read.dbc, readxl, purrr, tibble,
  janitor, flextable, arsenal, webshot2, tableone, 
  RColorBrewer, scales, gt, data.table, gridExtra, cowplot,
  R0,  parallel, ISOweek, colorspaces
)


#Función para periodos de Rt 0-----
encontrar_secuencias2 <- function(df,
                                  year_col,
                                  casos_col,
                                  week_col = NULL,
                                  min_tail = 10,
                                  start_index = 1,
                                  all_sequences = FALSE,
                                  return_periods = FALSE) {
  
  stopifnot(
    is.data.frame(df),
    year_col %in% names(df),
    casos_col %in% names(df),
    is.null(week_col) || week_col %in% names(df),
    is.numeric(min_tail), min_tail >= 1,
    is.numeric(start_index), start_index >= 1
  )
  
  year  <- df[[year_col]]
  casos <- df[[casos_col]]
  n     <- length(casos)
  
  if (n < 6 || start_index > n - 5) return(NULL)
  
  # Vector lógico (usar & vectorizado, NO &&)
  is_zero <- !is.na(casos) & casos == 0
  
  hay_quintuple_cero_en <- function(a, b) {
    if (b - a + 1 < 5) return(FALSE)
    for (j in a:(b - 4)) {
      if (all(is_zero[j:(j + 4)])) return(TRUE)
    }
    FALSE
  }
  
  resultados <- list()
  periodos <- list()
  
  i <- start_index
  while (i <= n - 5) {
    if (all(is_zero[i:(i + 4)]) && (!is.na(casos[i + 5]) && casos[i + 5] != 0)) {
      k <- i + 5
      
      if ((n - k + 1) >= min_tail) {
        window_end <- k + min_tail - 1
        
        if (!hay_quintuple_cero_en(k, window_end)) {
          fin <- n
          if (n >= k + 4) {
            for (j in k:(n - 4)) {
              if (all(is_zero[j:(j + 4)])) {
                fin <- j - 1
                break
              }
            }
          }
          
          start_extra <- if (!is.null(week_col)) df[[week_col]][k] else NA
          end_extra   <- if (!is.null(week_col)) df[[week_col]][fin] else NA
          
          resultados[[length(resultados) + 1]] <- data.frame(
            start_index = k,
            end_index   = fin,
            start_year  = year[k],
            end_year    = year[fin],
            start_casos = casos[k],
            end_casos   = casos[fin],
            start_extra = start_extra,
            end_extra   = end_extra,
            stringsAsFactors = FALSE
          )
          
          if (return_periods) {
            cols <- if (is.null(week_col)) c(year_col, casos_col) else c(year_col, casos_col, week_col)
            periodos[[length(periodos) + 1]] <- df[k:fin, cols, drop = FALSE]
          }
          
          if (!all_sequences) {
            if (return_periods) {
              return(list(summary = do.call(rbind, resultados),
                          periods = periodos))
            } else {
              return(do.call(rbind, resultados))
            }
          }
          
          i <- fin
        }
      }
    }
    i <- i + 1
  }
  
  if (length(resultados) == 0) return(NULL)
  
  res_df <- do.call(rbind, resultados)
  if (return_periods) {
    list(summary = res_df, periods = periodos)
  } else {
    res_df
  }
}
#-----------------------------------------------------------------------------

#Cargo bases ----
dengue_ar_agrupados<-read.csv2("source/csv/armo/dengue_ar_agrupados.csv")
dengue_ar_total<-read.csv2("source/csv/armo/dengue_ar_total.csv")
den_py_total<-read.csv2("source/csv/armo/dengue_py_total.csv")
den_br_total<-read.csv2("source/csv/armo/dengue_br_total.csv")
dengue_total<-read.csv2("source/csv/armo/dengue_total.csv")


#Acomodo bases
#Filtro por caso probable
dengue_ar_total<-dengue_ar_total%>%
  mutate(
    across(c(fecha,  fis, fecha_lab), as.Date),
    across(where(is.character), as.factor)
  )  %>%
  filter(year_e > 2008, year_e < 2023)  %>%
  group_by( year_e,year, se, mes, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
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
  group_by( year_e,year, se, mes, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
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
  group_by( year_e,year, se, mes, sexo, re, criterio, classi_fin, sistema, pais, distrito, serotipo, test, resultado,resul_validado, tipo_base) %>%
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
  select(year_e,year, se, mes, re, casos, criterio, sistema, pais, distrito, classi_fin, tipo_base)


dengue_completo_ar <- bind_rows(
  dengue_ar_total,
  ar_long
)

#-----------------------------------------------------------------------------
  #Base rT
#------------------------------------------------------------------------------

###SE####
#####Dengue
# Argentina
dengue_ar_se <- dengue_completo_ar %>% 
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se, distrito, pais) %>%
  summarise(
    Den_ar_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_ar_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados      = lab_pos + lab_neg,  # sólo positivos+negativos; otros niveles quedan fuera
    positividad_lab_ar = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_)
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_ar_Total_CP, Den_ar_Total_CC, positividad_lab_ar)

# Paraguay
dengue_py_se <- den_py_total %>%
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se,  pais) %>%
  summarise(
    Den_py_AP_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_py_AP_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados       = lab_pos + lab_neg,
    positividad_lab_py_AP  = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_),
    distrito = "Alto Paraná"
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_py_AP_Total_CP, Den_py_AP_Total_CC, positividad_lab_py_AP )

# Brasil
dengue_br_se <- den_br_total %>%
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se, distrito, pais) %>%
  summarise(
    Den_br_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_br_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados       = lab_pos + lab_neg,
    positividad_lab_br  = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_)
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_br_Total_CP, Den_br_Total_CC, positividad_lab_br)
# CIUDAD DEL ESTE
dengue_py_se_CDE <- den_py_total %>%
  filter(distrito == "CIUDAD DEL ESTE") %>%
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se, distrito, pais) %>%
  summarise(
    Den_py_CDE_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_py_CDE_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados      = lab_pos + lab_neg,
    positividad_lab_py_CDE = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_)
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_py_CDE_Total_CP, Den_py_CDE_Total_CC, positividad_lab_py_CDE)


# PRESIDENTE FRANCO
dengue_py_se_PF <- den_py_total %>%
  filter(distrito == "PRESIDENTE FRANCO") %>%
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se, distrito, pais) %>%
  summarise(
    Den_py_PF_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_py_PF_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados      = lab_pos + lab_neg,
    positividad_lab_py_PF = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_)
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_py_PF_Total_CP, Den_py_PF_Total_CC, positividad_lab_py_PF)


# HERNANDARIAS
dengue_py_se_HER <- den_py_total %>%
  filter(distrito == "HERNANDARIAS") %>%
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se, distrito, pais) %>%
  summarise(
    Den_py_HER_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_py_HER_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados      = lab_pos + lab_neg,
    positividad_lab_py_HER = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_)
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_py_HER_Total_CP, Den_py_HER_Total_CC, positividad_lab_py_HER)


# MINGA GUAZU
dengue_py_se_MG <- den_py_total %>%
  filter(distrito == "MINGA GUAZU") %>%
  mutate(YEAR_SE =  as.integer(if_else(se == 1 & mes == 12, year + 1, year))) %>% 
  group_by(YEAR_SE, se, distrito, pais) %>%
  summarise(
    Den_py_MG_Total_CP = sum(casos [classi_fin != "descartado"], na.rm = TRUE),
    Den_py_MG_Total_CC = sum(if_else(classi_fin == "confirmado", casos, 0, missing = 0), na.rm = TRUE),
    lab_pos         = sum(if_else(resul_validado == "positivo", casos, 0, missing = 0), na.rm = TRUE),
    lab_neg         = sum(if_else(resul_validado == "negativo", casos, 0, missing = 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pos_testeados      = lab_pos + lab_neg,
    positividad_lab_py_MG = if_else(pos_testeados > 0, (lab_pos / pos_testeados) * 100, NA_real_)
  ) %>%
  rename(SE = se) %>%
  select(YEAR_SE, SE, Den_py_MG_Total_CP, Den_py_MG_Total_CC, positividad_lab_py_MG)



#####Clima####
clima_ar_se<-read.csv2("source/csv/clima/clima_ar_se.csv") %>% mutate(YEAR_SE = as.integer(YEAR_SE))


#####Población####
pop_ar<-read.csv2("source/csv/pop/pop_ar.csv") 
pop_br<-read.csv2("source/csv/pop/pop_br.csv") 
pop_py<-read.csv2("source/csv/pop/pop_py.csv")

###Uno datos
Listado_se<-list(dengue_ar_se,dengue_py_se,dengue_br_se,dengue_py_se_CDE,
                 dengue_py_se_PF,
                 dengue_py_se_HER,
                 dengue_py_se_MG, clima_ar_se)


Base_semanal <- reduce(Listado_se, full_join, by = c("YEAR_SE", "SE")) %>%
  arrange(YEAR_SE, SE) %>%
  select(!starts_with("X")) 

Poblacion <- reduce( list(pop_br, pop_ar, pop_py), full_join, by = "YEAR")%>%
  arrange(YEAR) %>%
 rename (YEAR_SE = YEAR) %>% 
  select(!starts_with("X"))

Base_se_TxInc <- left_join(Base_semanal, Poblacion, by = "YEAR_SE")  %>%
  mutate(Den_ar_IGR_incidencia = replace_na(Den_ar_Total_CP/Pop_ar_IGR*100000, 0), 
         Den_br_IGU_incidencia = replace_na(Den_br_Total_CP/Pop_br_IGU*100000, 0),
         Den_py_AP_incidencia = replace_na(Den_py_AP_Total_CP/(Pop_py_CDE+Pop_py_PF+Pop_py_HERNANDARIAS+Pop_py_MG)*100000, 0),
         Den_py_CDE_incidencia = replace_na(Den_py_CDE_Total_CP/Pop_py_CDE*100000, 0),
         Den_py_PF_incidencia = replace_na(Den_py_PF_Total_CP/Pop_py_PF*100000, 0),
         Den_py_HER_incidencia = replace_na(Den_py_HER_Total_CP/Pop_py_HERNANDARIAS*100000, 0),
         Den_py_MG_incidencia = replace_na(Den_py_MG_Total_CP/Pop_py_MG*100000, 0)
         ) 


Base_se_TxInc[is.na(Base_se_TxInc)] <- 0         



######Calculo rT con Probables#######
#Elimino semana 53
Base_se_TxInc <- Base_se_TxInc %>% filter(SE != 53) %>%  rename (YEAR = YEAR_SE)  


### IGU ####-------
Base_Rt_br<-Base_se_TxInc%>%
  select(YEAR,SE,Den_br_Total_CP,Pop_br_IGU, Clima_ar_Tempmedia) %>%
  filter((YEAR == 2009 & SE >= 40) | (YEAR >= 2010 & YEAR <= 2022) | (YEAR == 2023 & SE <= 39))

Base_Rt_br[is.na(Base_Rt_br)] <- 0


Base_Rt_br$t <- 1:nrow(Base_Rt_br)
par(mfrow=c(2,1),mar=c(4,4,3,1))
plot(1:10)
plot(Base_Rt_br$t, Base_Rt_br$Clima_ar_Tempmedia, main="temperature", ylab="temperature (C)" , xlab="week")
temp.lo=predict(loess(Base_Rt_br$Clima_ar_Tempmedia~Base_Rt_br$t, span=0.05)) # we smooth the temperature (optional)
lines(temp.lo)

plot(Base_Rt_br$t, Base_Rt_br$Den_br_Total_CP, type="l",xlab="week",ylab="cases")

Tmax <- length(Base_Rt_br$Den_br_Total_CP)
GTmax = 5 # maximum generation time (weeks)

gt <- mcmapply(evalGenTimeDist, 1:Tmax ,MoreArgs=list(a=c(16, 4.3, 1, 1),
                                                      b=c(1/2.69821, 1/0.4623722, 1, 1),
                                                      serT=temp.lo, tt=Base_Rt_br$t, GT.max = GTmax)) 

n = GTmax + 1
par(mfrow=c(1,1))
contour(gt[1:n,1:Tmax], x=1:n,y=1:Tmax, ylab="time", xlab="generation time (week)",
        main="")

mGT<-R0::generation.time("gamma", c(3, 1))

TD <- R0::est.R0.TD(Base_Rt_br$Den_br_Total_CP, mGT, begin=1, end=728, nsim=1000, correct=FALSE)

nsim=1000 


gt.lo<- matrix(0, ncol = Tmax, nrow=Tmax)
gt.lo[1:n, ] <- gt
gt.lo <- t(gt.lo)

Rt.lo <- est.R.Temp(Base_Rt_br$Den_br_Total_CP[1:Tmax], mGT, GTTemp=gt.lo, date=Base_Rt_br$t[1:Tmax], begin=1, end=Tmax, correct = FALSE, nsim=nsim)

#  ggplot

lT <- length(Rt.lo$R)
df.result.lo_br <- data.frame(R = Rt.lo$R, ymin=Rt.lo$conf.int[1], 
                              ymax=Rt.lo$conf.int[2], t=Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                              dt.inicio =1:lT,                        
                              type="temperature-dependent")

rangeTD <- c(1, Tmax)

df.static_br <- data.frame(R= TD$R, ymin=TD$conf.int[1],
                           ymax=TD$conf.int[2], t=Rt.lo$date[1:lT],
                           incid = Rt.lo$epid$incid, dt.inicio =1:lT, 
                           type="temperature-independent")

df.result.lo <- rbind(df.result.lo_br, df.static_br)

ggplot(data=df.result.lo, aes(x=dt.inicio, y=R, ymin=lower, ymax=upper)) + ylim(0,5.0) + 
  geom_ribbon() + geom_line() + geom_line(aes(y=1), linetype=2) + facet_grid(type ~ . ) 

### IGR####----------
### No puedo sacarla porque tiene muchos 0####
#### Hago periodos cortos####
arg <- encontrar_secuencias2(
  Base_se_TxInc,
  year_col = "YEAR",
  casos_col = "Den_ar_Total_CP",
  week_col = "SE",
  min_tail = 10,
  all_sequences = T,
  return_periods = F
)

arg #periodos

write.csv2(arg, "salida/articulo_4/Rt/periodos_Rt_Argentina.csv", row.names = F)

source("scripts/02_transmission/rt/01_argentina.R")
### AP####--------
#Corto los de PY- AP
py_ap <- encontrar_secuencias2(
  Base_se_TxInc,
  year_col = "YEAR",
  casos_col = "Den_py_AP_Total_CP",
  week_col = "SE",
  min_tail = 15,
  all_sequences = T,
  return_periods = F
)

py_ap #periodos
write.csv2(py_ap, "salida/articulo_4/Rt/periodos_Rt_Alto_Parana.csv", row.names = F)

### CDE####--------
#Corto los de PY- AP
py_cde <- encontrar_secuencias2(
  Base_se_TxInc,
  year_col = "YEAR",
  casos_col = "Den_py_CDE_Total_CP",
  week_col = "SE",
  min_tail = 15,
  all_sequences = T,
  return_periods = F
)

py_cde #periodos
write.csv2(py_cde, "salida/articulo_4/Rt/periodos_Rt_CDE.csv", row.names = F)

### MG####--------
#Corto los de PY- AP
py_mg<- encontrar_secuencias2(
  Base_se_TxInc,
  year_col = "YEAR",
  casos_col = "Den_py_MG_Total_CP",
  week_col = "SE",
  min_tail = 15,
  all_sequences = T,
  return_periods = F
)

py_mg #periodos
write.csv2(py_mg, "salida/articulo_4/Rt/periodos_Rt_MG.csv", row.names = F)

### PF####--------
#Corto los de PY- AP
py_pf<- encontrar_secuencias2(
  Base_se_TxInc,
  year_col = "YEAR",
  casos_col = "Den_py_PF_Total_CP",
  week_col = "SE",
  min_tail = 15,
  all_sequences = T,
  return_periods = F
)

py_pf #periodos
write.csv2(py_pf, "salida/articulo_4/Rt/periodos_Rt_PF.csv", row.names = F)

### HER####--------
#Corto los de PY- AP
py_her<- encontrar_secuencias2(
  Base_se_TxInc,,
  year_col = "YEAR",
  casos_col = "Den_py_HER_Total_CP",
  week_col = "SE",
  min_tail = 15,
  all_sequences = T,
  return_periods = F
)

py_her #periodos
write.csv2(py_her, "salida/articulo_4/Rt/periodos_Rt_HER.csv", row.names = F)

source("scripts/02_transmission/rt/01_argentina.R")
source("scripts/02_transmission/rt/02_alto_parana.R")
source("scripts/02_transmission/rt/03_ciudad_del_este.R")
source("scripts/02_transmission/rt/04_minga_guazu.R")
source("scripts/02_transmission/rt/05_presidente_franco.R")
source("scripts/02_transmission/rt/06_hernandarias.R")


###Uno las bases####-------


### UNIFICACIÓN DE Rt - BRASIL ####
Base_Rt.1 <- merge(Base_Rt_br, df.result.lo_br, by = "t") %>%
  mutate(Den_br_IGU_Total_Rt = R,
         Den_br_IGU_Total_Rtlow = lower,
         Den_br_IGU_Total_Rtupp = upper) %>%
  select(YEAR, SE, Den_br_IGU_Total_Rt, Den_br_IGU_Total_Rtlow, Den_br_IGU_Total_Rtupp)

Base_Rt_ap_total

### BASE FINAL UNIFICADA ###
Base_Rt_total <- list(Base_Rt.1,
                      Base_Rt_HER_total,
                      Base_Rt_PF_total,
                      Base_Rt_ap_total,
                      Base_Rt_CDE_total,
                      Base_Rt_MG_total,
                      Base_Rt_ar_total) %>%
  reduce(full_join, by = c("YEAR", "SE")) %>%
  arrange(YEAR, SE) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))  
  )


####Armo la base por SE##########
Base_se_Rt<-merge(Base_Rt_total, Base_se_TxInc, by = c("YEAR", "SE"))

write.csv2(Base_se_Rt,"source/csv/tabataba/Base_SE_Final_CP.csv")



##########Calculo Rt de la TF################ -------------

Base_Rt_TF<-Base_se_TxInc%>%
  select(YEAR,SE,Den_br_Total_CP,
         Den_ar_Total_CP,
         Den_py_AP_Total_CP,
         Pop_br_IGU,
         Pop_ar_IGR,
         Pop_py_HERNANDARIAS,
         Pop_py_CDE,
         Pop_py_MG,
         Pop_py_PF,
         Clima_ar_Tempmedia) %>%
  mutate(Den_TF_Total_CP= Den_br_Total_CP+Den_ar_Total_CP+Den_py_AP_Total_CP,
         Pop_TF= Pop_br_IGU+ Pop_ar_IGR+ Pop_py_HERNANDARIAS+ Pop_py_CDE+ Pop_py_MG+ Pop_py_PF) %>%
  filter((YEAR == 2009 & SE >= 40) | (YEAR >= 2010 & YEAR <= 2022) | (YEAR == 2023 & SE <= 40)) %>%  
  filter (SE != 53)

Base_Rt_TF[is.na(Base_Rt_TF)] <- 0



Base_Rt_TF$t <- 1:nrow(Base_Rt_TF) 
par(mfrow=c(2,1),mar=c(4,4,3,1))
plot(1:10)
plot(Base_Rt_TF$t, Base_Rt_TF$Clima_ar_Tempmedia, main="temperature", ylab="temperature (C)" , xlab="week")
temp.lo=predict(loess(Base_Rt_TF$Clima_ar_Tempmedia~Base_Rt_TF$t, span=0.05)) # we smooth the temperature (optional)
lines(temp.lo)

plot(Base_Rt_TF$t, Base_Rt_TF$Den_TF_Total_CP, type="l",xlab="week",ylab="cases")

Tmax <- length(Base_Rt_TF$Den_TF_Total_CP)
GTmax = 5 # maximum generation time (weeks)

gt <- mcmapply(evalGenTimeDist, 1:Tmax ,MoreArgs=list(a=c(16, 4.3, 1, 1),
                                                      b=c(1/2.69821, 1/0.4623722, 1, 1),
                                                      serT=temp.lo, tt=Base_Rt_TF$t, GT.max = GTmax)) 

n = GTmax + 1
par(mfrow=c(1,1))
contour(gt[1:n,1:Tmax], x=1:n,y=1:Tmax, ylab="time", xlab="generation time (week)",
        main="")

mGT<-R0::generation.time("gamma", c(3, 1))
TD <- R0::est.R0.TD(Base_Rt_TF$Den_TF_Total_CP, mGT, begin=1, end= 729, nsim=1000, correct=FALSE)

nsim=1000 


gt.lo<- matrix(0, ncol = Tmax, nrow=Tmax)
gt.lo[1:n, ] <- gt
gt.lo <- t(gt.lo)

Rt.lo <- est.R.Temp(Base_Rt_TF$Den_TF_Total_CP[1:Tmax], mGT, GTTemp=gt.lo, date=Base_Rt_TF$t[1:Tmax], begin=1, end=Tmax, correct = FALSE, nsim=nsim)

#  ggplot

lT <- length(Rt.lo$R)
df.result.lo_TF <- data.frame(R = Rt.lo$R, ymin=Rt.lo$conf.int[1], 
                              ymax=Rt.lo$conf.int[2], t=Rt.lo$date[1:lT], incid = Rt.lo$epid$incid,
                              dt.inicio =1:lT,                        
                              type="temperature-dependent")

rangeTD <- c(1, Tmax)

df.static_TF <- data.frame(R= TD$R, ymin=TD$conf.int[1],
                           ymax=TD$conf.int[2], t=Rt.lo$date[1:lT],
                           incid = Rt.lo$epid$incid, dt.inicio =1:lT, 
                           type="temperature-independent")

df.result.lo <- rbind(df.result.lo_TF, df.static_TF)

ggplot(data=df.result.lo, aes(x=dt.inicio, y=R, ymin=lower, ymax=upper)) + ylim(0,5.0) + 
  geom_ribbon() + geom_line() + geom_line(aes(y=1), linetype=2) + facet_grid(type ~ . ) 

#Identifico Periodos de inicio de TFote####
df.result.lo_TF <- df.result.lo_TF %>%
  mutate(
    ano_epi = 2009 + ((row_number() - 1) %/% 52)
  )

df.result.lo_TF <- df.result.lo_TF %>%
  mutate(
    semana_epi = ((row_number() - 1) %% 52) + 1
  )

# Marcar si cumple condición de Rt sostenido
df.result.lo_TF <- df.result.lo_TF %>%
  mutate(
    rt_sostenido = R > 1 & lower > 1 & upper > 1
  )

# Función para contar rachas consecutivas
contar_rachas <- function(vec) {
  rle_vec <- rle(vec)
  lengths <- rle_vec$lengths
  values <- rle_vec$values
  idx <- rep(NA, length(vec))
  contador <- 0
  for (i in seq_along(lengths)) {
    if (values[i]) {
      contador <- contador + 1
      idx[sum(lengths[seq_len(i - 1)]) + 1: lengths[i]] <- contador
    }
  }
  return(idx)
}

# Aplicar por año epidemiológico
df.result.lo_TF <- df.result.lo_TF %>%
  group_by(ano_epi) %>%
  mutate(
    grupo = contar_rachas(rt_sostenido)
  ) %>%
  ungroup()

# Identificar duración de cada grupo
duraciones <- df.result.lo_TF %>%
  filter(!is.na(grupo)) %>%
  count(ano_epi, grupo, name = "duracion")

# Unir de nuevo
df.result.lo_TF <- df.result.lo_TF %>%
  left_join(duraciones, by = c("ano_epi", "grupo"))

# Filtrar los eventos de Rt sostenido de al menos 3 y 5 semanas
rt_3sem <- df.result.lo_TF %>%
  filter(rt_sostenido, duracion >= 3)

rt_5sem <- df.result.lo_TF %>%
  filter(rt_sostenido, duracion >= 5)

# Opcional: mostrar por año y semanas donde ocurre
rt_3_resumen <- rt_3sem %>%
  group_by(ano_epi, grupo) %>%
  summarise(
    semanas = list(dt.inicio),
    duracion = first(duracion)
  )

rt_5_resumen <- rt_5sem %>%
  group_by(ano_epi, grupo) %>%
  summarise(
    semanas = list(dt.inicio),
    duracion = first(duracion)
  )

Base_Rt_TF<- merge(Base_Rt_TF,df.result.lo_TF, by="t")

write.csv2(Base_Rt_TF,"source/csv/tabataba/Base_TF_Final_CP.csv")

####Grafico####

Base_Rt_TF <- Base_Rt_TF %>%
  mutate(
    ANIO_EPI = if_else(SE >= 40, YEAR, YEAR - 1),
    ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1),
    rt_sostenido = R > 1 & lower > 1 & upper > 1,
    SE_epi = if_else(SE >= 40, SE - 39, SE + 13)  # Re-etiquetado de SE para año epidemiológico
  ) %>% filter(ANIO_EPI < 2023)  # Filtrar hasta el año epidemiológico 2022-2023

# Gráfico con facetas por año epidemiológico
p <- ggplot(Base_Rt_TF, aes(x = SE_epi)) +
  geom_rect(
    data = Base_Rt_TF %>% dplyr::filter(rt_sostenido),
    aes(
      xmin = SE_epi - 0.5, xmax = SE_epi + 0.5,
      fill = ANIO_EPI_LAB, group = ANIO_EPI_LAB
    ),
    ymin = -Inf, ymax = Inf,
    alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_line(aes(y = R), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ ANIO_EPI_LAB, scales = "fixed", ncol = 3) +
  scale_x_continuous(
    limits = c(1, 52),
    breaks = seq(1, 52, by = 8),
    labels = function(x) {
      out <- (x + 40) %% 52   # desplaza para que 1 -> 40
      out[out == 0] <- 52     # corrige los 0 del módulo
      out
    }
  ) +
  labs(
    title = "Rt semanal por año epidemiológico (inicio en SE 40)",
    subtitle = "Sombreado: períodos con Rt > 1 sostenido (IC95% > 1)",
    x = "Semana epidemiológica (calendario)",
    y = "Rt",
    caption = "Bandas: IC95%. Línea punteada: Rt = 1."
  ) +
  # Fondo blanco explícito
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = "grey80")
  )

p

ggsave(
  "salida/articulo_4/Rt_TF_semanal_2009_2023.png",
  plot = p,
  width = 15, height = 20, units = "cm", dpi = 300,
  bg = "white"   # asegura fondo blanco en el archivo
)

#Grafico 2
Base_Rt_TF_long <- Base_Rt_TF %>%
  arrange(YEAR, SE) %>%             # orden cronológico
  mutate(
    week_index = row_number()       # índice de semana "largo" continuo
  )

# Factor de escala para poder ver Rt y casos en el mismo gráfico
max_R   <- max(Base_Rt_TF_long$R, na.rm = TRUE)
max_den <- max(Base_Rt_TF_long$Den_TF_Total_CP, na.rm = TRUE)

escala <- max_den / max_R   # cuántos casos "equivalen" a 1 unidad de Rt

# Gráfico "largo" con Rt + Den_TF_Total_CP y sombreados
p_long <- ggplot(Base_Rt_TF_long, aes(x = week_index)) +
  # sombreado donde Rt > 1 sostenido
  geom_rect(
    data = Base_Rt_TF_long %>% filter(rt_sostenido),
    aes(
      xmin = week_index - 0.5,
      xmax = week_index + 0.5,
      fill = ANIO_EPI_LAB,
      group = ANIO_EPI_LAB
    ),
    ymin = -Inf, ymax = Inf,
    alpha = 0.2,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +

  # Den_TF_Total_CP (re-escalado, eje secundario)
  geom_line(
    aes(y = Den_TF_Total_CP / escala),
    color = "red",
    linewidth = 0.5
  ) +
  
  scale_y_continuous(
    name = "Rt",
    sec.axis = sec_axis(
      ~ . * escala,
      name = "Casos de dengue (Den_TF_Total_CP)"
    )
  ) +
  # Eje X: marcar los años en la semana 1 de cada año
  scale_x_continuous(
    breaks = Base_Rt_TF_long$week_index[Base_Rt_TF_long$SE == 1],
    labels = Base_Rt_TF_long$YEAR[Base_Rt_TF_long$SE == 1]
  ) +
  labs(
    title    = "Rt y casos de dengue en la Triple Frontera (serie semanal continua)",
    subtitle = "Sombreado: periodos con Rt > 1 sostenido (IC95% > 1)",
    x        = "Semana (marca en SE 1 de cada año)",
    caption  = "Línea azul: Rt (IC95%). Línea roja: Den_TF_Total_CP (eje secundario)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

p_long

ggsave(
  "salida/articulo_4/Rt_Den_TF_serie_larga_2009_2022_v2.png",
  plot   = p_long,
  width  = 18,
  height = 10,
  units  = "cm",
  dpi    = 300,
  bg     = "white"
)


#Gráfico 3


Base_Rt_TF <- Base_Rt_TF %>%
  mutate(
    ANIO_EPI = if_else(SE >= 40, YEAR, YEAR - 1),
    ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1),
    rt_sostenido = R > 1 & lower > 1 & upper > 1,
    SE_epi = if_else(SE >= 40, SE - 39, SE + 13)  # Re-etiquetado de SE para año epidemiológico
  ) %>% 
  filter(ANIO_EPI < 2023)  # Filtrar hasta el año epidemiológico 2022-2023

## Factor de escala global para superponer CP
max_R   <- max(Base_Rt_TF$R, na.rm = TRUE)
max_den <- max(Base_Rt_TF$Den_TF_Total_CP, na.rm = TRUE)

escala <- max_den / max_R   # cuántos CP equivalen a 1 unidad de Rt

# Gráfico con facetas por año epidemiológico + CP
p <- ggplot(Base_Rt_TF, aes(x = SE_epi)) +
  geom_rect(
    data = Base_Rt_TF %>% dplyr::filter(rt_sostenido),
    aes(
      xmin = SE_epi - 0.5, xmax = SE_epi + 0.5,
      fill = "grey70", group = ANIO_EPI_LAB
    ),
    ymin = -Inf, ymax = Inf,
    alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE
  ) +
  # Rt (eje primario)
  geom_line(aes(y = R), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  
  # CP reescalados (eje secundario)
  geom_line(
    aes(y = Den_TF_Total_CP / escala),
    color = "red",
    linewidth = 0.5
  ) +
  
  facet_wrap(~ ANIO_EPI_LAB, scales = "fixed", ncol = 3) +
  scale_x_continuous(
    limits = c(1, 52),
    breaks = seq(1, 52, by = 8),
    labels = function(x) {
      out <- (x + 40) %% 52   # desplaza para que 1 -> 40
      out[out == 0] <- 52     # corrige los 0 del módulo
      out
    }
  ) +
  scale_y_continuous(
    name = "Rt",
    sec.axis = sec_axis(
      ~ . * escala,
      name = "Casos de dengue"
    )
  ) +
  labs(
    title = "Rt semanal por año epidemiológico (inicio en SE 40)",
    subtitle = "Sombreado: períodos con Rt > 1 sostenido (IC95% > 1). Línea roja: casos (CP).",
    x = "Semana epidemiológica",
    caption = "Bandas: IC95%. Línea punteada: Rt = 1."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = "grey80")
  )

p

ggsave(
  "salida/articulo_4/Rt_TF_semanal_2009_2023_con_CP.png",
  plot = p,
  width = 15, height = 20, units = "cm", dpi = 300,
  bg = "white"
)

#Tabla resumen---------
## Año epidemiológico y TF total
Base_se_Rt_epi <- Base_se_Rt  %>%
  mutate(
    ANIO_EPI = if_else(SE >= 40, YEAR, YEAR - 1),
    ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1),
    SE_epi = if_else(SE >= 40, SE - 39, SE + 13)  # Re-etiquetado de SE para año epidemiológico
  ) %>% filter(ANIO_EPI < 2023)  # Filtrar hasta el año epidemiológico 2022-2023

## Poblaciones 2022
pop_2022 <- Base_se_Rt %>%
  filter(YEAR == 2022) %>%
  summarise(
    Pop_ar_IGR_2022 = unique(Pop_ar_IGR),
    Pop_br_IGU_2022 = unique(Pop_br_IGU),
    Pop_py_CDE_2022 = unique(Pop_py_CDE),
    Pop_py_PF_2022  = unique(Pop_py_PF),
    Pop_py_HER_2022 = unique(Pop_py_HERNANDARIAS),
    Pop_py_MG_2022  = unique(Pop_py_MG),
    POP_AP_2022     = unique(POP_AP)
  )

Pop_TF_2022 <- with(pop_2022,
                    Pop_ar_IGR_2022 + Pop_br_IGU_2022 + POP_AP_2022
)

fmt_pop <- function(x)
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)

## Agregar variables de Triple Frontera
Base_se_Rt_epi <- Base_se_Rt_epi %>%
  mutate(
    Den_TF_Total_CP = Den_ar_Total_CP + Den_br_Total_CP + Den_py_AP_Total_CP,
    Den_TF_Total_CC = Den_ar_Total_CC + Den_br_Total_CC + Den_py_AP_Total_CC,
    Den_TF_incidencia = (Den_TF_Total_CP / Pop_TF_2022) * 10000,
    positividad_lab_TF = rowMeans(
      cbind(positividad_lab_ar, positividad_lab_br, positividad_lab_py_AP),
      na.rm = TRUE
    )
  )


### acomodo tabla
dat_PI <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Puerto Iguazú (Arg.). Población: ",
                      fmt_pop(pop_2022$Pop_ar_IGR_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_ar_Total_CP,
    cc       = Den_ar_Total_CC,
    inc      = Den_ar_IGR_incidencia,
    pos      = positividad_lab_ar,
    temp_med = Clima_ar_Tempmedia,
    temp_max = Clima_ar_Tempmax,
    temp_min = Clima_ar_Tempmin
  )

## Foz do Iguaçu
dat_IGU <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Foz do Iguaçu (Bra.). Población: ",
                      fmt_pop(pop_2022$Pop_br_IGU_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_br_Total_CP,
    cc       = Den_br_Total_CC,
    inc      = Den_br_IGU_incidencia,
    pos      = positividad_lab_br,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## Alto Paraná total
dat_AP <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Alto Paraná (Par.). Población: ",
                      fmt_pop(pop_2022$POP_AP_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_py_AP_Total_CP,
    cc       = Den_py_AP_Total_CC,
    inc      = Den_py_AP_incidencia,
    pos      = positividad_lab_py_AP,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## CDE
dat_CDE <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Ciudad del Este (Par.). Población: ",
                      fmt_pop(pop_2022$Pop_py_CDE_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_py_CDE_Total_CP,
    cc       = Den_py_CDE_Total_CC,
    inc      = Den_py_CDE_incidencia,
    pos      = positividad_lab_py_CDE,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## Presidente Franco
dat_PF <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Presidente Franco (Par.). Población: ",
                      fmt_pop(pop_2022$Pop_py_PF_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_py_PF_Total_CP,
    cc       = Den_py_PF_Total_CC,
    inc      = Den_py_PF_incidencia,
    pos      = positividad_lab_py_PF,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## Hernandarias
dat_HER <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Hernandarias (Par.). Población: ",
                      fmt_pop(pop_2022$Pop_py_HER_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_py_HER_Total_CP,
    cc       = Den_py_HER_Total_CC,
    inc      = Den_py_HER_incidencia,
    pos      = positividad_lab_py_HER,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## Minga Guazú
dat_MG <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Minga Guazú (Par.). Población: ",
                      fmt_pop(pop_2022$Pop_py_MG_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp       = Den_py_MG_Total_CP,
    cc       = Den_py_MG_Total_CC,
    inc      = Den_py_MG_incidencia,
    pos      = positividad_lab_py_MG,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## Triple Frontera total
dat_TF <- Base_se_Rt_epi %>%
  transmute(
    region   = paste0("Triple Frontera (AR-BR-PY). Población: ",
                      fmt_pop(Pop_TF_2022),
                      " (Año 2022)"),
    ANIO_EPI,
    cp  = Den_TF_Total_CP,
    cc       = Den_TF_Total_CC,
    inc      = Den_TF_incidencia,
    pos      = positividad_lab_TF,
    temp_med = NA_real_,
    temp_max = NA_real_,
    temp_min = NA_real_
  )

## Un solo data frame
Base_TF_all <- bind_rows(
  dat_PI, dat_IGU, dat_AP, dat_CDE,
  dat_PF, dat_HER, dat_MG, dat_TF
)

Base_TF_all <- Base_TF_all %>%
  mutate(region = factor(region),
         ANIO_EPI_LAB = paste0(ANIO_EPI, "-", ANIO_EPI + 1),
         ANIO_EPI_LAB= factor(ANIO_EPI_LAB),
        ANIO_EPI_LAB = droplevels(ANIO_EPI_LAB)   # limpia niveles vacíos globales
)

tab_all <- tableby(
  ANIO_EPI_LAB ~ cp + cc + inc + pos + temp_med + temp_max + temp_min,
  data   = Base_TF_all,
  strata = region,
  control = tableby.control(
    test          = FALSE,              # <<< esto apaga los contrasts
    total         = TRUE,               # columna "Total"
    numeric.stats = c("sum", "mean"),
    digits        = 2
  )
)



labs_vars <- c(
  cp       = "Casos probables (media por SE)",
  cc       = "Casos confirmados (media por SE)",
  inc      = "Incidencia acumulada (media por SE)",
  pos      = "Positividad (media por SE)",
  temp_med = "Clima -Temperatura media (°C)",
  temp_max = "Clima -Temperatura maxima (°C) (media por SE)",
  temp_min = "Clima -Temperatura minima (°C) (media por SE)"
)

sum_all <- summary(
  tab_all,
  text              = TRUE,
  overall           = TRUE,          # columna "Total"
  labelTranslations = labs_vars
)

df_all <- as.data.frame(sum_all)
View(df_all)

write.csv(
  x    = sum_all,
  file = "salida/articulo_4/tabla_resumen_TF_tableby.csv", 
  fileEncoding = "Latin1"
)

###Grafico de toda la serie por municipio--------
# 1) Rt en formato largo

Base_SE_Final_CP <- Base_se_Rt |>
  mutate(
    yw = tsibble::yearweek(sprintf("%d W%02d", YEAR, SE))
  )

rt_long <- Base_SE_Final_CP |>
  select(
    YEAR, SE, yw,
    starts_with("Den_") & ends_with("Rt") |
      starts_with("Den_") & ends_with("Rtlow") |
      starts_with("Den_") & ends_with("Rtupp")
  ) |>
  pivot_longer(
    cols = starts_with("Den_"),
    names_to   = c("pref", "pais", "dist", "tipo", "medida"),
    names_sep  = "_",
    values_to  = "valor"
  ) |>
  # nos quedamos solo con "Total"
  filter(tipo == "Total") |>
  select(-pref, -tipo) |>
  # pasamos Rt, Rtlow, Rtupp a columnas
  pivot_wider(
    names_from  = medida,
    values_from = valor
  )

inc_long <- Base_SE_Final_CP |>
  select(
    YEAR, SE, yw,
    ends_with("_incidencia")
  ) |>
  pivot_longer(
    cols = starts_with("Den_"),
    names_to  = c("pref", "pais", "dist", "var"),
    names_sep = "_",
    values_to = "incidencia"
  ) |>
  select(-pref, -var)

# unir Rt e incidencia
datos_ts <- rt_long |>
  left_join(
    inc_long,
    by = c("YEAR", "SE", "yw", "pais", "dist")
  ) |>
  as_tsibble(
    index = yw,
    key   = c(pais, dist)
  ) %>% filter (dist != "AP") %>%
  mutate(
    rt_sostenido = Rt > 1 & Rtlow > 1 & Rtupp > 1
  ) %>% arrange( pais)

#Grafico

#Nombres
municipios <- c(
  IGR = "Puerto Iguazú (Arg.)",
  IGU = "Foz do Iguaçu (Bra.)",
  CDE = "Cdad. del Este (Par.)",
  HER = "Hernandarias (Par.)",
  MG  = "Minga Guazú (Par.)",
  PF  = "Pte. Franco (Par.)"
)

#Año epi
datos_ts <- datos_ts |>
  mutate(
    epiyear_ini = if_else(SE >= 40, YEAR, YEAR - 1L),
    epiyear_lab = paste0(epiyear_ini, "-", epiyear_ini + 1)
  )  %>% mutate(
    dist = dplyr::recode(dist, !!!municipios))  %>%
  mutate(
    dist = factor(dist,
                  levels = c(
                    "Puerto Iguazú (Arg.)",
                    "Foz do Iguaçu (Bra.)",
                    "Cdad. del Este (Par.)",
                    "Hernandarias (Par.)",
                    "Minga Guazú (Par.)",
                    "Pte. Franco (Par.)"
                  ))
  )


#Sombras


sombras_rt <- datos_ts |>
  as_tibble() |>
  arrange(pais, dist, yw) |>
  group_by(pais, dist, epiyear_lab) |>
  mutate(
    flag   = replace_na(rt_sostenido, FALSE),
    cambio = flag != lag(flag, default = first(flag)),
    bloque = cumsum(cambio)
  ) |>
  filter(flag) |>
  group_by(pais, dist, epiyear_lab, bloque) |>
  summarise(
    xmin = min(yw),
    xmax = max(yw),
    .groups = "drop"
  )

#Colores
col_epiyear <- c(
  "2009-2010" = "#f4b4b4",
  "2010-2011" = "#f2d3a2",
  "2011-2012" = "#f7f0a6",
  "2012-2013" = "#d7e7a8",
  "2013-2014" = "#c3e1bf",
  "2014-2015" = "#b7e3e8",
  "2015-2016" = "#bed5f7",
  "2016-2017" = "#cfbdf6",
  "2017-2018" = "#f4bdf1",
  "2018-2019" = "#f8c8da",
  "2019-2020" = "#f3c1c1",
  "2020-2021" = "#ead0aa",
  "2021-2022" = "#e5e7a7",
  "2022-2023" = "#cbe3a9"
)



p2 <- ggplot(datos_ts, aes(x = yw, y = incidencia)) +
  
  # SOMBRAS Rt_sostenido con misma paleta que el gráfico de Rt
  geom_rect(
    data  = sombras_rt,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "gray80"),
    inherit.aes = FALSE,
    alpha  = 0.70,
    colour = NA
  ) +
  
  # curva de incidencia
  geom_line(linewidth = 0.8) +
  
  # semana 40 (inicio año epi)
  geom_vline(
    data = datos_ts |> filter(SE == 40),
    aes(xintercept = yw),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.2,
    colour = "red"
  ) +
  
  facet_wrap(~ dist, scales = "free_y", ncol = 1) +  
  
  scale_fill_manual(values = col_epiyear) +
  guides(fill = "none") +
  
  labs(
    title    = "Incidencia semanal y períodos con Rt sostenido > 1",
    subtitle = "Sombras coloreadas por temporadao; líneas rojas: semana 40",
    x        = "Semana (yearweek)",
    y        = "Incidencia (casos por 100.000 hab)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    strip.background   = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

p2

ggsave(
  "salida/articulo_4/Rt_muni_casos_semanal_2009_2023.png",
  plot = p2,
  width = 20, height = 18, units = "cm", dpi = 300,
  bg = "white"   # asegura fondo blanco en el archivo
)

ggsave(
  "salida/articulo_4/Rt_muni_casos_semanal_2009_2023_ancha.png",
  plot = p2,
  width = 25, height = 18, units = "cm", dpi = 300,
  bg = "white"   # asegura fondo blanco en el archivo
)

write.csv(datos_ts,"source/csv/tabataba/Base_Rt_municipios_semanal_CP.csv")


###Grafico 1 articulo--------




# 2) Preparar datos Rt + incidencia 

Base_SE_Final_CP <- Base_se_Rt |>
  mutate(
    yw = tsibble::yearweek(sprintf("%d W%02d", YEAR, SE))
  )

rt_long <- Base_SE_Final_CP |>
  select(
    YEAR, SE, yw,
    starts_with("Den_") & ends_with("Rt") |
      starts_with("Den_") & ends_with("Rtlow") |
      starts_with("Den_") & ends_with("Rtupp")
  ) |>
  pivot_longer(
    cols = starts_with("Den_"),
    names_to = c("pref", "pais", "dist", "tipo", "medida"),
    names_sep = "_",
    values_to = "valor"
  ) |>
  filter(tipo == "Total") |>
  select(-pref, -tipo) |>
  pivot_wider(
    names_from = medida,
    values_from = valor
  )

inc_long <- Base_SE_Final_CP |>
  select(
    YEAR, SE, yw,
    ends_with("_incidencia")
  ) |>
  pivot_longer(
    cols = starts_with("Den_"),
    names_to = c("pref", "pais", "dist", "var"),
    names_sep = "_",
    values_to = "incidencia"
  ) |>
  select(-pref, -var)

datos_ts <- rt_long |>
  left_join(
    inc_long,
    by = c("YEAR", "SE", "yw", "pais", "dist")
  ) |>
  as_tsibble(
    index = yw,
    key = c(pais, dist)
  ) |>
  filter(dist != "AP") |>
  mutate(
    rt_sostenido = Rt > 1 & Rtlow > 1 & Rtupp > 1,
    epiyear_ini = if_else(SE >= 40, YEAR, YEAR - 1L),
    temporada = paste0(epiyear_ini, "-", epiyear_ini + 1)
  ) |>
  arrange(pais)


# 3) Nombres de municipios 
municipios <- c(
  IGR = "Puerto Iguazú (Arg.)",
  IGU = "Foz do Iguaçu (Bra.)",
  CDE = "Cdad. del Este (Par.)",
  HER = "Hernandarias (Par.)",
  MG = "Minga Guazú (Par.)",
  PF = "Pte. Franco (Par.)"
)

datos_ts <- datos_ts |>
  mutate(
    dist = recode(dist, !!!municipios),
    dist = factor(
      dist,
      levels = c(
        "Puerto Iguazú (Arg.)",
        "Foz do Iguaçu (Bra.)",
        "Cdad. del Este (Par.)",
        "Hernandarias (Par.)",
        "Minga Guazú (Par.)",
        "Pte. Franco (Par.)"
      )
    )
  )


# 4) Sombras Rt sostenido + colores de df_plot 

col_sombras <- df_plot |>
  distinct(pais, temporada, nivel_sombra, color_sombra) |>
  mutate(
    pais = case_when(
      pais %in% c("argentina", "Argentina", "ar", "AR") ~ "ar",
      pais %in% c("brasil", "Brasil", "br", "BR") ~ "br",
      pais %in% c("paraguay", "Paraguay", "py", "PY") ~ "py",
      TRUE ~ pais
    )
  )

sombras_rt <- datos_ts |>
  as_tibble() |>
  arrange(pais, dist, yw) |>
  group_by(pais, dist, temporada) |>
  mutate(
    flag = replace_na(rt_sostenido, FALSE),
    cambio = flag != lag(flag, default = first(flag)),
    bloque = cumsum(cambio)
  ) |>
  filter(flag) |>
  group_by(pais, dist, temporada, bloque) |>
  summarise(
    xmin = min(yw),
    xmax = max(yw),
    .groups = "drop"
  ) |>
  left_join(
    col_sombras,
    by = c("pais", "temporada")
  )

sombras_rt <- read_csv(
  "salida/articulo_4/sombras_rt.csv",
  show_col_types = FALSE
) |>
  mutate(
    xmin = tsibble::yearweek(xmin),
    xmax = tsibble::yearweek(xmax)
  )

# 5) Factor para superponer Rt sobre incidencia 

factor_rt <- max(datos_ts$incidencia, na.rm = TRUE) / 5


# 6) Gráfico


p2 <- ggplot(datos_ts, aes(x = yw)) +
  geom_rect(
    data = sombras_rt,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = nivel_agrupado
    ),
    inherit.aes = FALSE,
    alpha = 0.35,
    colour = NA
  ) +
  geom_ribbon(
    aes(
      ymin = Rtlow * factor_rt,
      ymax = Rtupp * factor_rt
    ),
    fill = "#4A90E2",
    alpha = 0.12,
    colour = NA,
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = Rt * factor_rt, colour = "Rt"),
    linewidth = 0.45
  ) +
  geom_line(
    aes(y = incidencia, colour = "Incidencia"),
    linewidth = 0.8
  ) +
  geom_vline(
    data = datos_ts |> filter(SE == 40),
    aes(xintercept = yw, colour = "Semana 40"),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.25
  ) +
  facet_wrap(~dist, scales = "fixed", ncol = 1) +
  scale_fill_manual(
    name = "Serotipo predominante",
    values = col_sombra_final,
    drop = FALSE
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Incidencia" = "black",
      "Rt" = "#0057B8",
      "Semana 40" = "red"
    )
  ) +
  scale_y_continuous(
    name = "Incidencia",
    limits = c(0, max_inc),
    sec.axis = sec_axis(
      ~ . / factor_rt,
      name = "Rt",
      breaks = 1:5
    )
  ) +
  labs(
    title = "Incidencia semanal, Rt y períodos con Rt sostenido > 1",
    subtitle = "Sombras: período de transmisión sostenida; banda azul tenue: IC 95%",
    x = "Semana epidemiológica"
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE),
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = "grey80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal"
  )

p2

ggsave(
  "salida/articulo_4/Incidencia_Rt_serotipos_alta_calidad2.png",
  plot = p2,
  width = 16,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)

###Grafico con y libre

datos_plot_free <- datos_ts |>
  group_by(dist) |>
  mutate(
    max_inc_dist = max(incidencia, na.rm = TRUE),
    factor_rt_dist = max_inc_dist / 5,
    Rt_plot = pmin(Rt, 5) * factor_rt_dist,
    Rtlow_plot = pmax(Rtlow, 1) * factor_rt_dist,
    Rtupp_plot = pmin(Rtupp, 5) * factor_rt_dist
  ) |>
  ungroup()


  p2_free <- ggplot(datos_plot_free, aes(x = yw)) +
  geom_rect(
    data = sombras_rt,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = nivel_agrupado
    ),
    inherit.aes = FALSE,
    alpha = 0.35,
    colour = NA
  ) +
  geom_ribbon(
    aes(
      ymin = Rtlow_plot,
      ymax = Rtupp_plot
    ),
    fill = "#4A90E2",
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(
    aes(y = Rt_plot, colour = "Rt"),
    linewidth = 0.45
  ) +
  geom_line(
    aes(y = incidencia, colour = "Incidencia"),
    linewidth = 0.8
  ) +
  geom_vline(
    data = datos_plot_free |> filter(SE == 40),
    aes(xintercept = yw, colour = "Semana 40"),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.25
  ) +
  facet_wrap(~dist, scales = "free_y", ncol = 1) +
  scale_fill_manual(
    name = "Serotipo predominante",
    values = col_sombra_final,
    drop = FALSE
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Incidencia" = "black",
      "Rt" = "#0057B8",
      "Semana 40" = "red"
    )
  ) +
  scale_y_continuous(
    name = "Incidencia"
  ) +
  labs(
    title = "Incidencia semanal, Rt y períodos con Rt sostenido > 1",
    subtitle = "Eje Y libre; Rt reescalado por municipio de 1 a 5",
    x = "Semana epidemiológica"
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE),
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "white", colour = "grey80")
  )

p2_free