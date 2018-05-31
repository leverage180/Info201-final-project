#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
youtube_data <- read.csv("data/USvideos.csv", stringsAsFactors = FALSE)

youtube_data <- youtube_data %>% 
  mutate("date_publish" = if_else(as.numeric(substring(publish_time, 12, 13)) < 7, as.Date(publish_time) - 1, as.Date(publish_time)))

youtube_data <- youtube_data %>% 
  mutate("hour_publish" = if_else(as.numeric(substring(publish_time, 12, 13)) < 7, 
                                    24 - (7 - as.numeric(substring(publish_time, 12, 13))), 
                                    as.numeric(substring(publish_time, 12, 13)) - 7))

youtube_data <- youtube_data %>% 
  mutate("day_of_week" = weekdays(date_publish))

youtube_data <- youtube_data %>% 
  mutate("days_to_trending" = as.Date(gsub(".", "-", trending_date, fixed = TRUE), "%y-%d-%m") - as.Date(publish_time))


shinyUI(fluidPage(
  
  titlePanel("Time v Trending"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("days",
                  "Day of the Week",
                  choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

    ),
    
    mainPanel(
       plotlyOutput("time_plot"),
       plotlyOutput("to_trending")
    )
  )
))
