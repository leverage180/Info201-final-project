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
library(plotly)
library(tidyr)
library(tidytext)

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
  theme = "style.css",
  
  titlePanel("YouTube TRENDING"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Welcome",
                p("On this site, users will see breakdown of videos from YouTube's Trending Page. We wish to explore whether a pattern exists amongst the videos
                  that climb their way onto one of YouTube's most promiment tabs, if not the most promiment. We pulled the data from Kaggle. The dataset was produced
                  and updated by a user named 'Mitchell J'. The dataset is called 'Trending YouTube Video Statistics. We are using version 83 of the USVideos.")          
              ),
              tabPanel("Time",
                       sidebarLayout(
                         sidebarPanel(
                           
                           selectInput("days",
                                       "Day of the Week",
                                       choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
                         ),
                         
                         mainPanel(
                           h2("Time v Trending"),
                           plotlyOutput("time_plot"),
                           plotlyOutput("to_trending")
                         )
              )  
        ),
        tabPanel("Titles",
          sidebarLayout(
            sidebarPanel(
              selectInput("choice",  label = strong("Word Searches"), 
                          choices = list("Default" = "default",
                                         "Positive" = "positive",
                                         "Negative" = "negative",
                                         "All" = "all"))
            ),
            mainPanel(
              tabsetPanel(type = "tabs",
                          tabPanel("Breakdown",
                                   h2("What are the most common words used in YouTube videos on the Trending page?"),
                                   p("Titles are used to briefly describe and label a video. With the boom in video production and content, however,
                                     it becomes hard to stand apart. In this section, we will explore what words are commonly found in these videos
                                     to uncover any correlations."),
                                   p("After exploring the data, users may find that many large studios basically control the Trending Page. They seem to push
                                     many official trailers and music videos that dominate the Trending page. So, it seems as if the key to being famous on YouTube 
                                     as of the moment is to be immenseely powerful or famous already. A plethora of words are related to a 'bougie'
                                     lifestyle as well.")
                                   ),
                          tabPanel("Word Chart",
                                   plotOutput('wordBar', click = "my_click"),
                                   verbatimTextOutput('wordText'),
                                   p(htmlOutput("description"))
                          )
                                   )
                          )
          )  
        )
  )
))
