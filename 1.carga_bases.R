#Manipulación inicial de bases de datos epidemiologicos

#Librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, read.dbc, readxl, purrr) 

#Directorio de trabajo
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")
#Brasil ----
##Cargar bases de datos
##Las bases de SINAN están en formato DBC (Compact DataBase), un formato propio de Brasil.
BR2009<-read.dbc("source/SINAN/DENGBR09.dbc")
BR2010<-read.dbc("source/SINAN/DENGBR10.dbc")
BR2011<-read.dbc("source/SINAN/DENGBR11.dbc")
BR2012<-read.dbc("source/SINAN/DENGBR12.dbc")
BR2013<-read.dbc("source/SINAN/DENGBR13.dbc")
BR2014<-read.dbc("source/SINAN/DENGBR14.dbc")
BR2015<-read.dbc("source/SINAN/DENGBR15.dbc")
BR2016<-read.dbc("source/SINAN/DENGBR16.dbc")
BR2017<-read.dbc("source/SINAN/DENGBR17.dbc")
BR2018<-read.dbc("source/SINAN/DENGBR18.dbc")
BR2019<-read.dbc("source/SINAN/DENGBR19.dbc")
BR2020<-read.dbc("source/SINAN/DENGBR20.dbc")
BR2021<-read.dbc("source/SINAN/DENGBR21.dbc")
BR2022<-read.dbc("source/SINAN/DENGBR22.dbc")
BR2023<-read.dbc("source/SINAN/DENGBR23.dbc")


##Seleccionar variables de casos vinculados a Foz do Iguaçu
####Notificación: ID_MUNICIP = 410830 (Foz do Iguaçu) 
#Residencia: ID_MN_RESI = 410830 (Foz do Iguaçu)

data2020=BR2020 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2009=BR2009 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2010=BR2010 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2011=BR2011 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2012=BR2012 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2013=BR2013 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2014=BR2014 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2015=BR2015 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2016=BR2016 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2017=BR2017 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2018=BR2018 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2019=BR2019 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2021=BR2021 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2022=BR2022 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")
data2023=BR2023 %>% filter(ID_MUNICIP== "410830"| ID_MN_RESI== "410830")


#Uno años 2009-2013 y 2014-2020
BR_Den_09_13 <- merge(data2009,data2010,all=T)
BR_Den_09_13 <- merge(BR_Den_09_13,data2011,all=T)
BR_Den_09_13 <- merge(BR_Den_09_13,data2012,all=T)
BR_Den_09_13 <- merge(BR_Den_09_13,data2013,all=T)

BR_Den_14_20 <- merge(data2014,data2015,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2016,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2017,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2018,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2019,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2020,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2021,all=T)
BR_Den_14_20 <- merge(BR_Den_14_20,data2022,all=T)
BR_Den_14_23 <- merge(BR_Den_14_20,data2023,all=T)

# Guardar bases

write.csv2(BR_Den_09_13 ,"source/csv/Dengue_Foz_09_13_Crudo.csv")
write.csv2(BR_Den_14_23 ,"source/csv/Dengue_Foz_14_23_Crudo.csv")

# Argentina ----

# 1. Cargar bases 
AR_SNVS1 <- read.csv2("source/TAD/sivilasnvs1.csv", fileEncoding = "UTF-8") 
AR_C2 <- read.csv2("source/TAD/SNVS1_RAMIREZ_C2.csv", fileEncoding = "latin1") 
AR_Agrupados <- read.csv2("source/TAD/SNVS1_Agrupado_V2.csv", fileEncoding = "latin1")
AR_SNVS2 <- read.csv2("source/TAD/SNVS2_RAMIREZ.csv", fileEncoding = "latin1") 

# 2. Quitar columnas X 
AR_C2 <- AR_C2[ , !names(AR_C2) %in% c("X")]
AR_Agrupados <- AR_Agrupados[ , !names(AR_Agrupados) %in% c("X")]
AR_SNVS2 <- AR_SNVS2[ , !names(AR_SNVS2) %in% c("X")]

# 3. Filtrar Iguazú 
AR_SNVS1 <- AR_SNVS1 %>%
  filter(id_localidad == "3283" | id_localidad_carga == "3283")

AR_C2 <- AR_C2 %>%
  filter(Codigo_localidad_Carga == "3283") %>%
  filter(Anio_Consulta >= 2009 & Anio_Consulta <= 2023)

AR_SNVS2 <- AR_SNVS2 %>%
  filter(ID_LOC_INDEC_RESIDENCIA == "54063030" | ID_LOC_INDEC_CARGA == "54063030")

# SNVS1 -----------------------------------------------------------------------

# 4. Limpiar duplicados por claves
AR_SNVS1 <- AR_SNVS1 %>%
  distinct(Id_sivila_Estudio_Individual, Id_sivila_datos_estudio, Fecha,
           Fecha_Inicio_Sintomas, id_patologia, Fecha_Carga, id_provincia_carga,
           nombre_provincia_carga, Codigo_Prov_INDEC, id_departamento_carga,
           nombre_departamento_carga, Codigo_Partido_INDEC, id_localidad_carga,
           nombre_localidad_carga, Codigo_Loc_INDEC, id_sivila_grupo_evento,
           nombre_grupo_evento, id_etiologia, Nombre_Etiologia, Patologia,
           Prueba_Resultado, Muestras, Tipo_Muestra, Pruebas, Sexo,
           .keep_all = TRUE)

# 5. Fechas
AR_SNVS1[AR_SNVS1 == "1/1/1900"] <- NA
AR_SNVS1[AR_SNVS1 == ""] <- NA

AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    Fecha = as.Date(Fecha, format = "%d/%m/%Y"),
    FIS = as.Date(Fecha_Inicio_Sintomas, format = "%d/%m/%Y"),
    Fecha_Carga = as.Date(Fecha_Carga,format =  "%d/%m/%Y"),
    Fecha_Lab = as.Date(Fecha_Toma_Muestra,format = "%d/%m/%Y"), 
    FN = as.Date(FECHA_NACIMIENTO, format = "%d/%m/%Y")
  )

# 6. Normalizar Test y Resultado
AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    Test = case_when(
      Pruebas %in% c("Detección Antígeno NS1", "NS1", "Test rápido Dengue NS1") ~ "NS1",
      Pruebas %in% c("RT-PCR en tiempo real", "RT-PCR", "PCR", "Serotipificación") ~ "PCR",
      Pruebas %in% c("Ac Ig M comercial", "MAC-ELISA IgM", "ELISA IgM") ~ "IgM",
      Pruebas %in% c("Test rápido Dengue IgG", "IgG", "Neutralización") ~ "IgG",
      TRUE ~ NA_character_
    ),
    Resultado = case_when(
      Prueba_Resultado %in% c("Negativo", "No Reactivo") ~ "Negativo",
      Prueba_Resultado %in% c("Positivo", "Presencia", "Reactivo") ~ "Positivo",
      TRUE ~ NA_character_
    )
  )

# 7. Eliminar duplicados según regla original
AR_SNVS1 <- AR_SNVS1 %>%
  mutate(
    Prioridad = case_when(
      Resultado == "Negativo" & Test == "IgM" & difftime(Fecha_Lab, FIS, units = "days") >= 7 ~ 7,
      Resultado == "Positivo" & Test == "PCR" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 6,
      Resultado == "Positivo" & Test == "NS1" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 5,
      Resultado == "Positivo" & Test == "IgM" & difftime(Fecha_Lab, FIS, units = "days") > 5 ~ 4,
      Resultado == "Negativo" & Test == "PCR" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 3,
      Resultado == "Negativo" & Test == "NS1" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 2,
      TRUE ~ 1
    )
  ) %>%
  group_by(Id_sivila_Estudio_Individual) %>%
  filter(Prioridad == max(Prioridad, na.rm = TRUE)) %>%
  slice_min(Fecha_Lab, with_ties = FALSE) %>%
  ungroup()

# SNVS2 -----------------------------------------------------------------------

AR_SNVS2 <- AR_SNVS2 %>%
  mutate(
    Test = case_when(
      DETERMINACION %in% c("Antígeno NS1") ~ "NS1",
      DETERMINACION %in% c("Genoma viral", "RT-qPCR") ~ "PCR",
      DETERMINACION %in% c("IgM DENV", "MAC-ELISA IgM", "ELISA IgM comercial") ~ "IgM",
      TRUE ~ NA_character_
    ),
    Resultado = case_when(
      RESULTADO %in% c("DEN-1", "DEN-2", "DEN-4", "Positivo") ~ "Positivo",
      RESULTADO %in% c("Negativo", "No detectable") ~ "Negativo",
      TRUE ~ NA_character_
    ),
    Fecha = as.Date(FECHA_CONSULTA, "%d/%m/%Y"),
    FIS = as.Date(FIS, "%d/%m/%Y"),
    Fecha_Carga = as.Date(FECHA_APERTURA, "%d/%m/%Y"),
    Fecha_Lab = as.Date(FTM, "%d/%m/%Y")
  ) %>%
  mutate(
    Prioridad = case_when(
      Resultado == "Negativo" & Test == "IgM" & difftime(Fecha_Lab, FIS, units = "days") >= 7 ~ 7,
      Resultado == "Positivo" & Test == "PCR" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 6,
      Resultado == "Positivo" & Test == "NS1" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 5,
      Resultado == "Positivo" & Test == "IgM" & difftime(Fecha_Lab, FIS, units = "days") > 5 ~ 4,
      Resultado == "Negativo" & Test == "PCR" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 3,
      Resultado == "Negativo" & Test == "NS1" & difftime(Fecha_Lab, FIS, units = "days") <= 7 & difftime(Fecha_Lab, FIS, units = "days") >= 0 ~ 2,
      TRUE ~ 1
    )
  ) %>%
  group_by(IDEVENTOCASO) %>%
  filter(Prioridad == max(Prioridad, na.rm = TRUE)) %>%
  slice_min(Fecha_Lab, with_ties = FALSE) %>%
  ungroup()

# C2 --------------------------------------------------------------------------

#Fechas
AR_C2[AR_C2 == "1900-01-01"] <- NA
AR_C2[AR_C2 == ""] <- NA
AR_C2[AR_C2 == "1/1/1900"] <- NA
AR_C2[AR_C2 == "1900"] <- NA

AR_C2 <- AR_C2 %>%
  mutate(
    Fecha_consulta  = as.Date(as.numeric(Fecha_consulta), origin = "1899-12-30"),
    Fecha_Contagio = as.Date(as.numeric(Fecha_Contagio), origin = "1899-12-30"),
    Fecha_Inicio_Sintomas = as.Date(Fecha_Inicio_Sintomas, format= "%d/%m/%Y")
  )

AR_C2 <- AR_C2 %>%
  arrange(Fecha_Inicio_Sintomas)%>%
  group_by(codigo_paciente) %>%
  slice_min(order_by = Fecha_Inicio_Sintomas, with_ties = FALSE) %>%
  ungroup()

# Guardar bases

write.csv2(AR_SNVS1 ,"source/csv/Dengue_Ar_SNVS1_Crudo.csv")
write.csv2(AR_C2 ,"source/csv/Dengue_Ar_C2_Crudo.csv")
write.csv2(AR_SNVS2 ,"source/csv/Dengue_Ar_SNVS2_Crudo.csv")

# Paraguay ----
# Cargar base
#Leo base de datos recibidas hasta 2020. Las bases están en Excel.


Den_py_2009<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2009.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2010<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2010.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2011<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2011.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2012<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2012.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2013<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2013.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2014<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2014.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2015<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2015.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2016<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2016.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2017<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2017.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2018<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2018.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2019<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2019.xlsx", col_types = c("text","date", "text", "text", "text", "text", "date", "date", "text", "text","text"))
Den_py_2020<-read_xlsx("source/Paraguay/BASE ARBOVIROSIS 2020.xlsx", col_types = c("text","text", "text", "text", "text", "text", "text", "text", "text", "text","text"))
Den_py_2020<-Den_py_2020%>%  mutate(across(starts_with("FECHA"), \(x) as.Date(x, format = "%d/%m/%Y")))

Den_py_2020<-Den_py_2020%>%  mutate(across(starts_with("FECHA"), \(x) as.Date(x, format = "%d/%m/%Y")))

list_Den_py<- list(Den_py_2009,Den_py_2010,Den_py_2011,Den_py_2012,Den_py_2013,Den_py_2014,Den_py_2015,Den_py_2016,Den_py_2017,Den_py_2018,Den_py_2019,Den_py_2020)
Den_py_total<- reduce(list_Den_py, full_join,by= intersect(names(list_Den_py),names(list_Den_py))) 


#Leo base de datos recibidas hasta 20-23 Las bases están en Excel.

Den_py_nuevo<-read_xlsx("source/Paraguay/ALTO PARANA - 2021 a 2023 - ARBOVIROSIS.xlsx",
                        col_types = c("numeric","text", "text", "text", "text", "text", "text", "text", "text", "text","text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

write.csv2(Den_py_total ,"source/csv/dengue_py_crudo_09_20.csv")
write.csv2(Den_py_nuevo ,"source/csv/dengue_py_crudo_21_23.csv")
