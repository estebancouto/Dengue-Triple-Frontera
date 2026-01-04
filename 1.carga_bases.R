#Manipulaci?n inicial de bases de datos epidemiologicos

#Librer?as
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, read.dbc, readxl, purrr) 

#Directorio de trabajo
setwd("C:/Users/pc/Google Drive/INMET/Vigifronteiras/Tesis/analisis/")
#Brasil ----
##Cargar bases de datos
##Las bases de SINAN est?n en formato DBC (Compact DataBase), un formato propio de Brasil.
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


##Seleccionar variables de casos vinculados a Foz do Igua?u
####Notificaci?n: ID_MUNICIP = 410830 (Foz do Igua?u) 
#Residencia: ID_MN_RESI = 410830 (Foz do Igua?u)

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


#Uno a?os 2009-2013 y 2014-2020
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


# Paraguay ----
# Cargar base
#Leo base de datos recibidas hasta 2020. Las bases est?n en Excel.


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


#Leo base de datos recibidas hasta 20-23 Las bases est?n en Excel.

Den_py_nuevo<-read_xlsx("source/Paraguay/ALTO PARANA - 2021 a 2023 - ARBOVIROSIS.xlsx",
                        col_types = c("numeric","text", "text", "text", "text", "text", "text", "text", "text", "text","text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

write.csv2(Den_py_total ,"source/csv/dengue_py_crudo_09_20.csv")
write.csv2(Den_py_nuevo ,"source/csv/dengue_py_crudo_21_23.csv")


#Argentina----
#cargo bases

AR_Agrupados_2<-read.csv2("C:/Users/pc/Google Drive/INMET/Vigifronteiras/DATOS/Argentina/Dengue/TAD/TAD original/SNVS1_Agrupado_V2.csv", fileEncoding = "latin1")

AR_SNVS_2<-read.csv2("C:/Users/pc/Google Drive/INMET/Vigifronteiras/DATOS/Argentina/Dengue/TAD/Alejandro/SNVS2_RAMIREZ.csv", fileEncoding = "latin1") %>%
  filter(ID_LOC_INDEC_CARGA %in% c("54063000","54063030","54063090")|
           ID_LOC_INDEC_RESIDENCIA %in% c("54063000", "54063030", "54063090")) %>%
  filter(EVENTO== "Dengue")

AR_SNVS_1<-read.csv2("C:/Users/pc/Google Drive/INMET/Vigifronteiras/DATOS/Argentina/Dengue/TAD/Alejandro/sivilasnvs1.csv", fileEncoding = "latin1")  %>%
  filter(id_localidad =="3283" | id_localidad_carga=="3283" )

####Corregir esto que se rompió