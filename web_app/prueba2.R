library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(DT)
library(plotly)
library(dplyr)

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Analitica predictiva",
                  tabPanel("Agrupamiento",
                           sidebarPanel(
                             tags$h3("Filtros:"),
                             selectInput("choose",label="Elige el barrio que quieres analizar",
                                         choices = barrios),
                             selectInput("choose1",label="Elige el grupo que quieres analizar",
                                         choices = grupos),
                             h4("Barrios por grupo"),
                             tags$h6("Escoge previamente el grupo de barrios"),
                             DT::dataTableOutput("grupos_display")
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
                ),
                
)

server <- function(input, output, session) {
  
  barrios_med1 <- readOGR('C:/Users/Cristian Restrepo/Desktop/web_app/barrios_med_1/barrios_med1.shp', layer = "barrios_med1", encoding = "UTF-8", use_iconv=TRUE)  
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

  df = reactive({
    df_group = subset(df_group,BARRIO == input$choose)
    return(df_group)})
  
  df1 = reactive({
    df_group = subset(df_group1,CLUSTER == input$choose1)
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
}

shinyApp(ui, server)