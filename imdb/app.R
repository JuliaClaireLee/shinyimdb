#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(RMySQL)
library(etl)
library(leaflet)
library(RSQLite)
library(ceeds)
library(macleish)
library(lubridate)
library(shinydashboard)
library(shiny)
library(highcharter)
library(timetk)
library(kableExtra)
library(plotly)
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
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$choose_dataset <- renderUI({
        selectInput("dataset2", "Data set", as.list(data_sets))
    })
    datasetInput2 <- reactive({
        switch(input$dataset2,
               "WhatelyMet" = daily_whately2,
               "OrchardMet" = daily_orchard2)
    })
    output$choose_columns <- renderUI({
        # If missing input, return to avoid error later in function
        if (is.null(input$dataset2))
            return()
        
        # Get the data set with the appropriate name
        data1 <-  datasetInput2()
        colnames <- names(data1)
        
        # Create the checkboxes and select them all by default
        checkboxGroupInput("columns", "Choose columns", 
                           choices  = colnames,
                           selected = colnames)
    })
    output$data_table <- DT::renderDataTable({
        if(is.null(input$dataset))
            return()
        if (is.null(input$columns) || !(input$columns %in% names(datasetInput2())))
            return()
        
        # Keep the selected columns
        data1 <-  datasetInput2()[, input$columns, drop = FALSE]
        data1<-data1 %>%
            filter(the_date >= ymd(as.character(input$start)), 
                   the_date <=ymd(as.character(input$end)))
        
        data1
    })
    
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste('Weather', '.csv', sep='') },
        content = function(file) {
            write.csv( datasetInput2()[, input$columns, drop = FALSE]%>%
                           filter(the_date >= ymd(as.character(input$start)), 
                                  the_date <=ymd(as.character(input$end)))
                       , file)
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
