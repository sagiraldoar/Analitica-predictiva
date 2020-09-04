#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(leaflet)
library(rgdal)
library(dplyr)
library(fastDummies)

barrios_med1 <- readOGR('barrios_med_1/barrios_med1.shp', layer = "barrios_med1", encoding = "UTF-8", use_iconv=TRUE)  
colores = as.character(ifelse(barrios_med1@data$cluster==1, "green",
                              ifelse(barrios_med1@data$cluster==2, "red",
                                     ifelse(barrios_med1@data$cluster==3, "yellow", "black"))))
barrios <- barrios_med1@data$NOMBRE

grupos <- barrios_med1@data$cluster[!is.na(barrios_med1@data$cluster)]

df_group <- read.csv("barrios_model.csv",encoding = "UTF-8")
names(df_group) <- toupper(names(df_group))
names(df_group)[names(df_group)=="NOMBRE"]   <- "BARRIO"
names(df_group)[names(df_group)=="SOLO_DANOS"]   <- "SOLO DANOS"
df_group <- df_group[,-1]

df_group1 <- subset(df_group,select = c(BARRIO, CLUSTER)) 

promedio_grupos <- as.data.frame(df_group %>% group_by(CLUSTER) %>% summarise_at(vars(-c(BARRIO)), ~round(mean(.))))


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Analitica predictiva",
                    tabPanel("Historico",
                             sidebarPanel(
                                 tags$h3("Fechas:"),
                                 dateRangeInput("fecha", "Fecha:", start="2014-01-01",
                                                end="2015-12-31",min="2014-01-01",
                                                max="2018-12-31",language="es"),
                                 selectInput("choose",label="Temporalidad",
                                             choices = c("Diario","Semanal","Mensual")),
                                 selectInput("choose2",label="Tipo",
                                             choices = c("Personas involucradas","Solo material")),
                                 actionButton("submitbutton", "Ingresar", 
                                              class = "btn btn-primary"),width=2
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 h1("Accidentalidad Medellin",align="center"),
                                 fluidRow(
                                     column(6,
                                            
                                            
                                            DT::dataTableOutput("data")),

                                    column(3,

                                            
                                             h4("Distribucion accidentes",align="center"),
                                             plotlyOutput('box')),
                                     column(3,

                                                      h4("Periodo con mas accidentes",align="center"),
                                                      plotlyOutput('barh')),
                                            
                                    
                                     ),#fluidrow,
                                 
                                    h4("Evolucion accidentalidad",align="center"),
                                    plotlyOutput('line')
                                            

                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("Predicciones",
                             sidebarPanel(
                                 tags$h3("Fechas:"),
                                 dateRangeInput("fecha2", "Fecha:", start="2018-01-01",
                                                end="2019-12-31",min="2018-01-01",
                                                max="2023-12-31",language="es"),
                                 selectInput("choose3",label="Temporalidad",
                                             choices = c("Diario","Semanal","Mensual")),
                                 selectInput("choose4",label="Tipo",
                                             choices = c("Solo material","Personas involucradas")),
                                 actionButton("submitbutton2", "Ingresar", 
                                              class = "btn btn-primary"),width=2
                             ),# sidebarPanel,
                             mainPanel(
                                 h1("Accidentalidad Medellin",align="center"),
                                 fluidRow(
                                     column(6,
                                            
                                            
                                            DT::dataTableOutput("pred_dia")),
                                     
                                     column(3,
                                            
                                            
                                            h4("Distribucion accidentes",align="center"),
                                            plotlyOutput('box2')
                                            ),
                                     column(3,
                                            
                                            h4("Periodo con mas accidentes",align="center"),
                                            plotlyOutput('barh2')
                                            ),
                                     
                                     
                                 ),#fluidrow,
                                 
                                 h4("Evolucion accidentalidad",align="center"),
                                 plotlyOutput('line2')
                                 
                                 
                                 
                             ) # mainPanel
                             ),#tabpanel 
                    tabPanel("Agrupamiento",
                             sidebarPanel(
                                 tags$h3("Filtros:"),
                                 selectInput("choose5",label="Elige el barrio que quieres analizar",
                                             choices = barrios),
                                 selectInput("choose6",label="Elige el grupo que quieres analizar",
                                             choices = grupos),
                                 h4("Barrios por grupo"),
                                 tags$h6("Escoge previamente el grupo de barrios"),
                                 DT::dataTableOutput("grupos_display"),width=2
                             ), # sidebarPanel
                             mainPanel(
                                 h2("Accidentalidad en Medellin"),
                                 leafletOutput("mymap"),
                                 p("Rojo: Accidentalidad alta - Cluster 2, Amarillo: Accidentalidad media - Cluster 3, Amarillo: Accidentalidad baja - Cluster 1. Las zonas azules son lugares que no tuvieron registros en la informacion analizada."),
                                 br(),
                                 h4("Caracteristicas de accidentalidad - Barrio seleccionado"),
                                 p("Cantidad de accidentes por tipo (atropello, choque, otro), consecuencia (herido, muerto, solo danos), y tipo de dia (laboral o no laboral)."),
                                 DT::dataTableOutput("barrio_display"),
                                 br(),
                                 h4("Promedio de accidentalidad por grupo"),
                                 p("Caracteristicas de accidentalidad por grupo de barrios."),
                                 DT::dataTableOutput("prom_grupos_display"),
                             )
                             )
                    
                ) # navbarPage
) # fluidPage


server <-  function(input, output, session) {
    
    
    df = reactive({
        df_group = subset(df_group,BARRIO == input$choose5)
        return(df_group)})
    
    df1 = reactive({
        df_group = subset(df_group1,CLUSTER == input$choose6)
        return(df_group)})
    
    df2 = reactive({promedio_grupos})
    
    
    output$mymap <- renderLeaflet({
        m=leaflet(barrios_med1)
        m=addPolygons(m,popup=barrios)
        m=addTiles(m)
        m=addPolygons(m,popup=barrios,color=colores)
    })
    
    output$barrio_display <- renderDataTable({datatable(df(),rownames = FALSE, options = list(searching = FALSE, lengthMenu = FALSE, paging = FALSE, scrollX = TRUE))})
    output$prom_grupos_display <- renderDataTable({datatable(df2(),rownames = FALSE, options = list(searching = FALSE, lengthMenu = FALSE, paging = FALSE, scrollX = TRUE))})
    output$grupos_display <- renderDataTable({datatable(df1(),rownames = FALSE)})

    data_display <- eventReactive(input$submitbutton,{
        if(input$choose2=="Solo material"){
            df_dia <- read.csv('df_solo_danios_diario.csv',encoding = "UTF-8")
            df_semana <- read.csv('df_solo_danios_semanal.csv',encoding = "UTF-8",colClasses = c("numeric","character","character","character","numeric") )
            df_mes <- read.csv('df_solo_danios_mensual.csv',encoding = "UTF-8",colClasses = c("numeric","character","character","numeric"))
            df_dia$FECHA <- as.Date(df_dia$FECHA,format = "%Y-%m-%d")
        }else{
            df_dia <- read.csv('df_personas_diario.csv',encoding = "UTF-8")
            df_semana <- read.csv('df_personas_semanal.csv',encoding = "UTF-8",colClasses = c("numeric","character","character","character","numeric") )
            df_mes <- read.csv('df_personas_mensual.csv',encoding = "UTF-8",colClasses = c("numeric","character","character","numeric"))
            df_dia$FECHA <- as.Date(df_dia$FECHA,format = "%Y-%m-%d")  
        }
        if(input$choose=="Diario"){
            df <- df_dia[df_dia$FECHA>= as.Date(input$fecha[1],format = "%Y-%m-%d") & df_dia$FECHA<=as.Date(input$fecha[2],format = "%Y-%m-%d"), ]
            df$auxiliar <- df$FECHA
            colnames(df) <- c("Fecha","Dia","Festivo","Dia_especial","Accidentes","auxiliar")
            df
        }else{
            if(input$choose=="Semanal"){
            anio_inicial <- format(as.Date(input$fecha[1]),"%Y")
            semana_inicial <- format(as.Date(input$fecha[1]),"%V")
            semana_inicial <- as.numeric(paste(anio_inicial,semana_inicial,sep=""))
            anio_final <- format(as.Date(input$fecha[2]),"%Y")
            semana_final <- format(as.Date(input$fecha[2]),"%V")
            semana_final <- as.numeric(paste(anio_final,semana_final,sep=""))
            df_semana$AUXILIAR <- as.numeric(paste(df_semana$PERIODO,df_semana$SEMANA,sep=""))
            df <- df_semana[df_semana$AUXILIAR >=semana_inicial & df_semana$AUXILIAR <=semana_final,]
            df$AUXILIAR <- paste(df$PERIODO,df$SEMANA,sep="-")
            colnames(df) <- c("Periodo","Mes","Mes_nombre","Semana","Accidentes","auxiliar")
            df
            }else{
                    anio_inicial <- format(as.Date(input$fecha[1]),"%Y")
                    mes_inicial <- format(as.Date(input$fecha[1]),"%m")
                    mes_inicial <- as.numeric(paste(anio_inicial,mes_inicial,sep=""))
                    anio_final <- format(as.Date(input$fecha[2]),"%Y")
                    mes_final <- format(as.Date(input$fecha[2]),"%m")
                    mes_final <- as.numeric(paste(anio_final,mes_final,sep=""))
                    df_mes$AUXILIAR <- as.numeric(paste(df_mes$PERIODO,df_mes$MES,sep=""))
                    df <- df_mes[df_mes$AUXILIAR >=mes_inicial & df_mes$AUXILIAR <=mes_final,]
                    df$AUXILIAR <- paste(df$PERIODO,df$MES,sep="-")
                    colnames(df) <- c("Periodo","Mes","Mes_nombre","Accidentes","auxiliar")
                    df
        }
        }
        #df

    },ignoreNULL = FALSE)

    data_pred_dia <- eventReactive(input$submitbutton2,{
       
        df_pred <- read.csv('df_prediccion.csv',encoding = "UTF-8")
        df_pred$MES <- sprintf("%02d", as.numeric(df_pred$MES))
        df_pred$SEMANA <- sprintf("%02d", as.numeric(df_pred$SEMANA))
        df_fechas <- read.csv('df_fechas_prediccion.csv',encoding = "UTF-8")
        df_fechas$MES <- sprintf("%02d",as.numeric( df_pred$MES))
        df_fechas$SEMANA <- sprintf("%02d", as.numeric(df_fechas$SEMANA))

        if(input$choose3=="Diario"){
        df_fechas <- df_fechas[df_fechas$FECHA>= as.Date(input$fecha2[1],format = "%Y-%m-%d") & df_fechas$FECHA<=as.Date(
            input$fecha2[2],format = "%Y-%m-%d"), ]   
        df_pred <- df_pred[df_pred$FECHA>= as.Date(input$fecha2[1],format = "%Y-%m-%d") & df_pred$FECHA<=as.Date(
                                                                input$fecha2[2],format = "%Y-%m-%d"), ] 
        
        if(input$choose4=="Solo material"){
            #modelo <- readRDS("personas_diario.rds") 
            #preds <- predict("personas_diario.rds",df_pred)
            #df_fechas$Prediccion <- preds
            df_fechas$Prediccion <- 20  
            df_fechas
        }else{
            modelo <- readRDS("modelo_diario_personas.rds") 
            preds <- predict(modelo,df_pred)
            df_fechas$Prediccion <- round(exp(preds),0)
            df_fechas
 
        }
        
       
        df_fechas$auxiliar=df_fechas$FECHA
        df_fechas <- df_fechas[,c('FECHA','DIA_NOMBRE','FESTIVO','FECHA_ESPECIAL','Prediccion','auxiliar')]
        colnames(df_fechas) <- c('Fecha','Dia','Festivo','Fecha_especial','Prediccion','auxiliar')
        df_fechas
        
        }else{
            if(input$choose3=="Semanal"){
                df_pred<-  df_pred %>%
                    group_by(PERIODO,SEMANA) %>%
                    summarize(Total_laborales=sum(LABORAL),Total_festivos=sum(FESTIVO))

                
                df_pred <- dummy_cols(df_pred, select_columns = c('SEMANA'))
                
                df_fechas<-  df_fechas%>%
                    group_by(PERIODO,SEMANA) %>%
                    summarize()
                
                anio_inicial <- format(as.Date(input$fecha2[1]),"%Y")
                semana_inicial <- format(as.Date(input$fecha2[1]),"%V")
                semana_inicial <- as.numeric(paste(anio_inicial,semana_inicial,sep=""))
                anio_final <- format(as.Date(input$fecha2[2]),"%Y")
                semana_final <- format(as.Date(input$fecha2[2]),"%V")
                semana_final <- as.numeric(paste(anio_final,semana_final,sep=""))
                
                df_pred$AUXILIAR <- as.numeric(paste(df_pred$PERIODO,df_pred$SEMANA,sep=""))
                df_pred<- df_pred[df_pred$AUXILIAR >=semana_inicial & df_pred$AUXILIAR <=semana_final,]
                df_fechas$AUXILIAR <- as.numeric(paste(df_fechas$PERIODO,df_fechas$SEMANA,sep=""))
                df_fechas<- df_fechas[df_fechas$AUXILIAR >=semana_inicial & df_fechas$AUXILIAR <=semana_final,]
                
                if(input$choose4=="Solo material"){
                #modelo <- readRDS("solo_danios_semanal.rds") 
                #preds <- predict("solo_danios_semanal.rds",df_pred)
                #df_fechas$Prediccion <- preds
                df_fechas$Prediccion <- 50   
                }else{
                    modelo <- readRDS("modelo_semanal_personas.rds") 
                    df_pred$Total_laborales <- scale(df_pred$Total_laborales)
                    df_pred$Total_festivos <- scale(df_pred$Total_festivos)
                    preds <- predict(modelo,df_pred)
                    df_fechas$Prediccion <- round(exp(preds),0)
                      
                }
                df_fechas$AUXILIAR <- NULL
                df_fechas$auxiliar <- paste(df_fechas$PERIODO,df_fechas$SEMANA,sep="-")
                #colnames(df) <- c("Periodo","Mes","Mes_nombre","Semana","Accidentes","auxiliar")
                df_fechas
                
                

            }else{
                df_pred<-  df_pred %>%
                    group_by(PERIODO,MES) %>%
                    summarize(Total_laborales=sum(LABORAL),Total_festivos=sum(FESTIVO))
                df_pred <- dummy_cols(df_pred, select_columns = c('MES'))
                
                df_fechas<-  df_fechas%>%
                    group_by(PERIODO,MES,MES_NOMBRE) %>%
                    summarize()
                
                anio_inicial <- format(as.Date(input$fecha2[1]),"%Y")
                mes_inicial <- format(as.Date(input$fecha2[1]),"%m")
                mes_inicial <- as.numeric(paste(anio_inicial,mes_inicial,sep=""))
                anio_final <- format(as.Date(input$fecha2[2]),"%Y")
                mes_final <- format(as.Date(input$fecha2[2]),"%m")
                mes_final <- as.numeric(paste(anio_final,mes_final,sep=""))
                
                df_pred$AUXILIAR <- as.numeric(paste(df_pred$PERIODO,df_pred$MES,sep=""))
                df_pred<- df_pred[df_pred$AUXILIAR >=mes_inicial & df_pred$AUXILIAR <=mes_final,]
                df_fechas$AUXILIAR <- as.numeric(paste(df_fechas$PERIODO,df_fechas$MES,sep=""))
                df_fechas<- df_fechas[df_fechas$AUXILIAR >=mes_inicial & df_fechas$AUXILIAR <=mes_final,]
                
                if(input$choose4=="Solo material"){
                    #modelo <- readRDS("solo_danios_mensual.rds") 
                    #preds <- predict("solo_danios_mensual.rds",df_pred)
                    #df_fechas$Prediccion <- preds
                    df_fechas$Prediccion <- 100   
                }else{
                    modelo <- readRDS("modelo_mensual_personas.rds") 
                    df_pred$Total_laborales <- scale(df_pred$Total_laborales)
                    df_pred$Total_festivos <- scale(df_pred$Total_festivos)
                    preds <- predict(modelo,df_pred)
                    df_fechas$Prediccion <- round(exp(preds),0)  
                }
                df_fechas$AUXILIAR <- NULL
                df_fechas$auxiliar <- paste(df_fechas$PERIODO,df_fechas$MES,sep="-")
                #colnames(df) <- c("Periodo","Mes","Mes_nombre","Semana","Accidentes","auxiliar")
                df_fechas
                
                
                  }
            }   

        
    },ignoreNULL = FALSE)
    output$data <- renderDataTable({
        datatable(data_display()[,-ncol(data_display())],rownames = FALSE, 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  options = list(
                      # dom = 't',
                      # deferRender = TRUE,
                      searching = TRUE,
                      autoWidth = TRUE,
                      info=FALSE,
                      # scrollCollapse = TRUE,
                      rownames = TRUE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "350px",
                      fixedHeader = TRUE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                          leftColumns = 0,
                          heightMatch = 'none'
                      )
                  )
        )  %>% formatStyle(colnames(data_display())[1:ncol(data_display())-1], `text-align` = 'center')

       
    })

    output$pred_dia <- renderDataTable({
        datatable(data_pred_dia()[,-ncol(data_pred_dia())],rownames = FALSE, 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  options = list(
                      # dom = 't',
                      # deferRender = TRUE,
                      searching = TRUE,
                      autoWidth = TRUE,
                      info=FALSE,
                      # scrollCollapse = TRUE,
                      rownames = TRUE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "350px",
                      fixedHeader = TRUE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                          leftColumns = 0,
                          heightMatch = 'none'
                      )
                  )
        )  %>% formatStyle(colnames(data_pred_dia())[1:ncol(data_pred_dia())-1], `text-align` = 'center')
        
        
    })

    output$plot <- renderPlotly({
        plot1 <- plot_ly(
            x = as.factor(data_display()$auxiliar),
            y = data_display()$Accidentes, 
            type = 'bar')
        plot1 <- plot1 %>% layout(autosize = F, width = 1400, height = 350)
    })  
    output$box <- renderPlotly({
        box1 <- plot_ly(
            y = data_display()$Accidentes,
            showlegend = FALSE,
            hoverinfo = 'y',
            type = 'box',
            colors=c("green"))
        box1 <- box1 %>% layout(autosize = F, width = 300, height = 410) %>% layout(xaxis = list(showticklabels = F))
    })  
    
    output$barh <- renderPlotly({
        plot1 <- plot_ly(
            x = (head(data_display()[order(-data_display()$Accidentes),],n=5))$Accidentes,
            y = as.factor((head(data_display()[order(-data_display()$Accidentes),],n=5))$auxiliar), 
            type = 'bar',
            orientation='h')
        plot1 <- plot1 %>% layout(autosize = F, width = 400, height = 430
                                  ,    yaxis = list(
                                      categoryorder = "array",
                                      categoryarray = rev(as.factor((head(data_display()[order(-data_display()$Accidentes),],n=5))$auxiliar))
                                      )
                                  )

    })   
    output$line <- renderPlotly({
    fig <- plot_ly(x=as.factor(data_display()$auxiliar),y=data_display()$Accidentes,
                   type="scatter",
                   mode="lines",
                   fill='tonexty',
                   )
                   
   # fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
})
    output$plot2 <- renderPlotly({
        plot1 <- plot_ly(
            x = as.factor(data_pred_dia()$auxiliar),
            y = data_pred_dia()$Prediccion, 
            type = 'bar')
        plot1 <- plot1 %>% layout(autosize = F, width = 1400, height = 350)
    })  
    output$box2 <- renderPlotly({
        box1 <- plot_ly(
            y = data_pred_dia()$Prediccion,
            showlegend = FALSE,
            hoverinfo = 'y',
            type = 'box')
        box1 <- box1 %>% layout(autosize = F, width = 300, height = 410) %>% layout(xaxis = list(showticklabels = F))
    })  
    
    output$barh2 <- renderPlotly({
        plot1 <- plot_ly(
            x = (head(data_pred_dia()[order(-data_pred_dia()$Prediccion),],n=5))$Prediccion,
            y = as.factor((head(data_pred_dia()[order(-data_pred_dia()$Prediccion),],n=5))$auxiliar), 
            type = 'bar',
            orientation='h')
        plot1 <- plot1 %>% layout(autosize = F, width = 400, height = 430
                                  ,    yaxis = list(
                                      categoryorder = "array",
                                      categoryarray = rev(as.factor((head(data_pred_dia()[order(-data_pred_dia()$Prediccion),],n=5))$auxiliar))
                                  )
        )
        
    })   
    output$line2 <- renderPlotly({
        fig <- plot_ly(x=as.factor(data_pred_dia()$auxiliar),y=data_pred_dia()$Prediccion,
                       type="scatter",
                       mode="lines",
                       fill='tonexty')
        
        # fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
    })
}


 
 shinyApp(ui = ui, server = server)
