
wd="C:/Users/sga96/OneDrive/Accidentalidad-Medellin"
setwd(wd)

FECHA <- seq(as.Date("2018/01/01"), by = "day", length.out = 2195)
df <-data.frame(FECHA)
df$PERIODO <- format(df$FECHA,"%Y")
df$MES <- as.numeric(format(df$FECHA,"%m"))
df$MES <- sprintf("%02d", df$MES)
df$SEMANA <- format(df$FECHA,"%V")
df$DIA <- as.numeric(format(df$FECHA,"%d"))
df$DIA <- sprintf("%02d", df$DIA)
df$DIA_NOMBRE <- weekdays(as.Date(df$FECHA))
df$DIA_NOMBRE <- toupper(df$DIA_NOMBRE)
festivos=read.csv("./Datasets/Festivos.csv")
festivos$FECHA <- as.Date(festivos$FECHA, format = "%Y-%m-%d")
df <- merge(x = df, y = festivos, by = "FECHA", all.x = TRUE)

laborales=c('LUNES','MARTES','MIÉRCOLES','JUEVES','VIERNES')
df$LABORAL=ifelse(df$DIA_NOMBRE %in% laborales,1,0)


#Se agregan fechas especiales
df_fechas_especiales=read.csv("./Datasets/Fechas especiales.csv",strip.white=TRUE,
                              encoding = "UTF-8")
df_fechas_especiales$FECHA <- as.Date(df_fechas_especiales$FECHA, format = "%Y-%m-%d")
df <- merge(x = df, y = df_fechas_especiales, by = "FECHA", all.x = TRUE)
df[is.na(df)] <- 0

library("writexl")
#write_xlsx(df,"df_fechas_prediccion.xlsx")

library("readxl")
df_fechas_prediccion <-  read_excel("df_fechas_prediccion.xlsx")
write.csv(df_fechas_prediccion,"df_fechas_prediccion.csv",fileEncoding = 'UTF-8',row.names=FALSE)

library(fastDummies)

df$ANIO_REFERENCIA <- as.numeric(df$PERIODO)-2014
df_modelo <- dummy_cols(df, select_columns = c('MES','SEMANA','DIA','DIA_NOMBRE'))



write.csv(df_modelo,"df_prediccion.csv",fileEncoding = 'UTF-8',row.names=FALSE)

