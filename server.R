#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("ui.R")
library(plotly)

shinyServer(function(input, output) {
  
  time_data <- reactive({
    youtube_data <- youtube_data %>% 
      filter(day_of_week == input$days)
    
  })
  
   
  output$time_plot <- renderPlotly({
    p <- ggplot(data = time_data()) +
      geom_bar(aes(x = hour_publish), fill = "red") +
      scale_y_continuous(limits = c(0, 750))
    
    p <- ggplotly(p)
    p
    
  })
  
  output$to_trending <- renderPlotly({
    p <- ggplot(data = time_data()) + 
      
      geom_smooth(aes(x = hour_publish, y = days_to_trending), color = "red", se = FALSE, method = "loess") +
      scale_y_continuous(limit=c(0,80),oob = "squish") +
    
      labs(
        x = "Hour Published",
        y = "Days Until Video Reached Trending"
      )
    
    p <- ggplotly(p, tooltip = "y")
    p
  })
  
})
