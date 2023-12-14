


##################################################################################
# Define UI
library(shiny)
library(ggplot2)
library(latticeExtra)
library(tidyverse)
library(tidyr)
library(dplyr)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(

      selectInput(inputId = "state", label = "Select State", choices = unique(pollandasthma$state)),

      selectInput(inputId = "pollutant", label = "Select Pollutant", choices = c( "Ozone", "PM10", "NO2"))
    ),
    mainPanel(

      plotOutput("line_plot")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    pollandasthma %>%
      filter(state == input$state)
  })
  
  output$line_plot <- renderPlot({
    y_variable <- switch(
      input$pollutant,
      Ozone = "Ozone",
      PM10 = "PM10",
      NO2 = "NO2"
    )
    
    obj1 <- xyplot(filtered_data()[[y_variable]] ~ Year | factor(state), filtered_data(), type = c("l", "g"), col='darkgreen' , ylab = "Pollutant Level", auto.key = list(space = "right", title = 'Pollutant'))
    obj2 <- xyplot(filtered_data()$Prevalencepercent ~ Year | factor(state), filtered_data(), type = c("l", "g"), col='black', ylab = "Prevalence Percent", auto.key = list(space = "right", title = "Asthma Prevalence"))
    combined_plot <- doubleYScale(obj2, obj1, add.ylab2 = TRUE, use.style=FALSE)
    
    print(combined_plot)
    
  })
}


shinyApp(ui = ui, server = server)

library(rsconnect)
deployApp()
