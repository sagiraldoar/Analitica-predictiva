wd="C:/Users/sga96/OneDrive/Accidentalidad-Medellin"
setwd(wd)

df_2014=read.csv("./Datasets/Accidentalidad_georreferenciada_2014.csv",strip.white=TRUE,
                 encoding = "UTF-8")
df_2015=read.csv("./Datasets/Accidentalidad_georreferenciada_2015.csv",strip.white=TRUE,
                 encoding = "UTF-8")
df_2016=read.csv("./Datasets/Accidentalidad_georreferenciada_2016.csv",strip.white=TRUE,
                 encoding = "UTF-8")
df_2017=read.csv("./Datasets/Accidentalidad_georreferenciada_2017.csv",strip.white=TRUE,
                 encoding = "UTF-8")
df_2018=read.csv("./Datasets/Accidentalidad_georreferenciada_2018.csv",strip.white=TRUE,
                 encoding = "UTF-8")

df_inicial = rbind(df_2014,df_2015,df_2016,df_2017,df_2018)

library(tidyr)
df <- separate(data = df_inicial, col = FECHA, into = c("FECHA", "Sobrante"), sep = " ")
df$FECHA <- as.Date(df$FECHA, format = "%Y/%m/%d")

# Se limpian los datos y de la columna CLASE y se agrupan en 3 categorías
df$CLASE[df$CLASE=='Choque'] <- "Choque" 
df$CLASE[df$CLASE=='Volcamiento'] <- "Otro" 
df$CLASE[df$CLASE=='Atropello'] <- "Atropello" 
df$CLASE[df$CLASE=='Caída de Ocupante'] <- "Otro" 
df$CLASE[df$CLASE=='Caida Ocupante'] <- "Otro" 
df$CLASE[df$CLASE=='Caida de Ocupante'] <- "Otro" 
df$CLASE[df$CLASE=='Caída Ocupante'] <- "Otro" 
df$CLASE[df$CLASE=='Choque y Atropello'] <- "Choque" 
df$CLASE[df$CLASE=='otro'] <- "Otro"
df$CLASE[df$CLASE=='Incendio'] <- "Otro"
df$CLASE[df$CLASE==""] <- "Otro"


df$GRAVEDAD[df$GRAVEDAD=="MUERTO"] <- "PERSONA AFECTADA"
df$GRAVEDAD[df$GRAVEDAD=="HERIDO"] <- "PERSONA AFECTADA"


#Se agrupa por clase y por día
library(dplyr,warn.conflicts = FALSE)
df <-  df %>%
  group_by(FECHA,PERIODO,MES,DIA,DIA_NOMBRE,GRAVEDAD) %>%
  summarize(NRO_ACCIDENTES=n())

#se crea la variable año de referencia
df$ANIO_REFERENCIA <- df$PERIODO -2014

#Se agrega variable semana
df$SEMANA <- format(df$FECHA,"%V")

# Se agregan  variable de FESTIVO al modelo
festivos=read.csv("./Datasets/Festivos.csv")
festivos$FECHA <- as.Date(festivos$FECHA, format = "%Y-%m-%d")
df <- merge(x = df, y = festivos, by = "FECHA", all.x = TRUE)

# Se agrega variable LABORAL al modelo
laborales=c('LUNES','MARTES','MIÉRCOLES','JUEVES','VIERNES')
df$LABORAL=ifelse(df$DIA_NOMBRE %in% laborales,1,0)
df$LABORAL=ifelse(df$FESTIVO == 1,0,df$LABORAL)

#Se agregan fechas especiales
df_fechas_especiales=read.csv("./Datasets/Fechas especiales.csv",strip.white=TRUE,
                              encoding = "UTF-8")
df_fechas_especiales$FECHA <- as.Date(df_fechas_especiales$FECHA, format = "%Y-%m-%d")
df <- merge(x = df, y = df_fechas_especiales, by = "FECHA", all.x = TRUE)
df$MES <- sprintf("%02d", df$MES)
df$DIA <- sprintf("%02d", df$DIA)


df_material <- df[df$GRAVEDAD== "SOLO DAÑOS",]

library("writexl")
#write_xlsx(df_material,"df_solo_danios_diario.xlsx")

library("readxl")
 df_material <-  read_excel("df_solo_danios_diario.xlsx")
 df_material_diario <- df_material[,c("FECHA","DIA_NOMBRE","FESTIVO","FECHA_ESPECIAL","NRO_ACCIDENTES")]
 write.csv(df_material_diario,"df_solo_danios_diario.csv",row.names=FALSE,fileEncoding= "UTF-8")
 
 df_material_semanal <- df_material %>%
   group_by(PERIODO,MES,MES_NOMBRE,SEMANA) %>%
   summarize(NRO_ACCIDENTES=sum(NRO_ACCIDENTES))
 write.csv(df_material_semanal,"df_solo_danios_semanal.csv",row.names=FALSE)
 
 df_material_mensual <- df_material %>%
   group_by(PERIODO,MES,MES_NOMBRE) %>%
   summarize(NRO_ACCIDENTES=sum(NRO_ACCIDENTES))
 write.csv(df_material_mensual,"df_solo_danios_mensual.csv",row.names=FALSE)
 

 
  df_personas <- df[df$GRAVEDAD== "PERSONA AFECTADA",]
 
#write_xlsx(df_personas,"df_personas_diario.xlsx")

 df_personas <-  read_excel("df_personas_diario.xlsx")
 df_personas_diario <- df_personas[,c("FECHA","DIA_NOMBRE","FESTIVO","FECHA_ESPECIAL","NRO_ACCIDENTES")]
  write.csv(df_personas_diario,"df_personas_diario.csv",row.names=FALSE)
  
 df_personas_semanal <- df_personas %>%
   group_by(PERIODO,MES,MES_NOMBRE,SEMANA) %>%
   summarize(NRO_ACCIDENTES=sum(NRO_ACCIDENTES))
 write.csv(df_personas_semanal,"df_personas_semanal.csv",row.names=FALSE)
 
 df_personas_mensual <- df_personas %>%
   group_by(PERIODO,MES,MES_NOMBRE) %>%
   summarize(NRO_ACCIDENTES=sum(NRO_ACCIDENTES))
 write.csv(df_personas_mensual,"df_personas_mensual.csv",row.names=FALSE)
