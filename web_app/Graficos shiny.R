#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
       
        fluidRow(
            mainPanel(
            plotOutput("plotmes")),
            
       
            mainPanel(
                plotOutput("plotsemana")),

         
                mainPanel(
                    plotOutput("plotdia"))

        )
     )
            )

# Define server logic required to draw a histogram
server <- function(input, output) {
    dfM=read.csv("df_modelo.csv")
    output$plotmes <- renderPlot({
        
        ggplot(dfM, aes(x=dfM$PERIODO, y=dfM$NRO_ACCIDENTES)) + 
            geom_bar(stat = "identity")
        
        
    }) #finplotmes
   
    output$plotsemana <- renderPlot({

        ggplot(dfM, aes(x=dfM$MES, y=NRO_ACCIDENTES)) +
            geom_bar(stat = "identity")
    })#finplotsemana

    output$plotdia <- renderPlot({

        ggplot(dfM, aes(x=dfM$SEMANA, y=NRO_ACCIDENTES)) +
            geom_bar(stat = "identity")
    })#finplotdia
    # # 
    print(colnames(dfM))
 

    
   
} #finServer

# Run the application 
shinyApp(ui = ui, server = server)
