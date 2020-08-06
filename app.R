#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rsconnect)

source("DataGatheringFunc.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Grade Visualizer"),
    
    # Sidebar with different widgets depending on the selected tab
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = "input.tabselected==1",
                             h2("Player Visuals"),
                             textInput("name", label = h3("Player Name"), value = "Lonzo Ball"),
                             
                             textInput("season", label = h3("Season"), value = "2018-19"),
                             textInput("clarity", label = h3("Clarity(For Heat Map)"), value = 20),
                             radioButtons("Restricted", label = h3("Restricted Area"), 
                                                choices = list("Yes" = TRUE, "No" = FALSE),
                                                selected = FALSE),
                             radioButtons("period", label = h3("Quarter"),
                                          choices = list("First" = 1, "Second" = 2, "Third" = 3, "Fourth" = 4, "All" = 5), 
                                          selected = 1)),
            conditionalPanel(condition = "input.tabselected==2",
                             h2("Team Data"),
                             textInput("teamName", label = h3("Team Name"), value = "LAL"),
                             radioButtons("Filter", label = h3("Extra Filter"), choices = list("Under 35% Shooting from 3" = "3PT under 35", "Over 40% Shooting from 3"="3PT over 40",
                                                                                               "Wins" = "W", "Loss" = "L", "None" = "None")),
                             textInput("seasonT", label = h3("Season"), value = "2018-19"),
                             textInput("clarityT", label = h3("Clarity(For Heat Map"), value = 20),
                             radioButtons("RestrictedT", label = h3("Restricted Area"), 
                                                choices = list("Yes" = TRUE, "No" = FALSE),
                                                selected = FALSE),
                             radioButtons("periodT", label = h3("Quarter"),
                                          choices = list("First" = 1, "Second" = 2, "Third" = 3, "Fourth" = 4, "All" = 5), 
                                          selected = 1))
            
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Player Data", value = 1,
                                  plotOutput("PlayerdistPlot"),
                                 DT::dataTableOutput("PlayerdistData",width='600px'),
                                  plotOutput("PlayerheatMap"),
                                  plotOutput("PlayerppsPlot")
                                  
                                 ),
                        tabPanel("Team Data", value = 2,
                                 h5("Summary Statistics"),
                                 plotOutput("teamHeatMap"),
                                 plotOutput("facet")
                                 ),
                        
                        id = "tabselected"
            )
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$PlayerheatMap <- renderPlot({
        
        playerHeatMap(input$name,input$season,as.numeric(input$clarity),as.logical(input$Restricted),as.numeric(input$period))
        
    })
    
    output$PlayerdistPlot <- renderPlot({
        
        playerDistributionPlot(input$name, input$season,input$filter)
        
    })
    output$PlayerppsPlot <- renderPlot({
        
        pps(input$name,input$season,as.numeric(input$clarity),as.numeric(input$period))
        
        
    })
    output$PlayerdistData <- DT::renderDataTable(playerDistributionData(input$name,input$season),options=list(scrollX=T))
    
    output$facet <- renderPlot({
        
        facetTest(input$teamName,input$seasonT,input$Filter)
        
        
    })
    output$teamHeatMap <- renderPlot({
        
        teamHeatMap(input$teamName,input$seasonT, as.numeric(input$clarityT),as.logical(input$RestrictedT),as.numeric(input$periodT),input$Filter)
        
        
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
