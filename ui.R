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
library(leaflet)
library(misc3d)
library(png)
library(raster)
library(sp)
library(viridis)
library(plot3D)

# Reads data
project_data <-
  read.csv('youtube-new/USvideos.csv', stringsAsFactors = F)
project_data$trending_date <-
  gsub("[.]", "/", project_data$trending_date)
id_data <-
  read.csv('youtube-new/category_id.csv', stringsAsFactors = F)
# Names & Joins
colnames(id_data) <- c("category_id", "category_name")
# Coerces int
id_data$category_id <- as.integer(id_data$category_id)
joined_data <- left_join(project_data, id_data)

youtube_data <-
  read.csv("data/USvideos.csv", stringsAsFactors = FALSE)

youtube_data <- youtube_data %>%
  mutate("date_publish" = if_else(
    as.numeric(substring(publish_time, 12, 13)) < 7,
    as.Date(publish_time) - 1,
    as.Date(publish_time)
  ))

youtube_data <- youtube_data %>%
  mutate("hour_publish" = if_else(
    as.numeric(substring(publish_time, 12, 13)) < 7,
    24 - (7 - as.numeric(substring(publish_time, 12, 13))),
    as.numeric(substring(publish_time, 12, 13)) - 7
  ))

youtube_data <- youtube_data %>%
  mutate("day_of_week" = weekdays(date_publish))

youtube_data <- youtube_data %>%
  mutate("days_to_trending" = as.Date(gsub(".", "-", trending_date, fixed = TRUE), "%y-%d-%m") - as.Date(publish_time))


shinyUI(
  fluidPage(
    theme = "style.css",
    
    h1("YouTube TRENDING"),
    
    tabsetPanel(
      type = "tabs",
      tabPanel("Welcome",
               tabsetPanel(
                 tabPanel(
                   "Credit",
                   p(
                     "On this site, users will see breakdown of videos from YouTube's Trending Page. We wish to explore whether a pattern exists amongst the videos
                     that climb their way onto one of YouTube's most promiment tabs, if not the most promiment. We pulled the data from Kaggle. The dataset was produced
                     and updated by a user named 'Mitchell J'. The dataset is called 'Trending YouTube Video Statistics. We are using version 83 of the USVideos and other
                     countries."
                   )
                   ),
                 tabPanel(
                   "Thesis",
                   p(
                     "For this project, we will present visualizations and explanation that seek to answer the question if there are certain common attributes amongst videos
                     which appear on Trending. If there is no common or all the traits are proportional in appearance, such as every word has a similar chance of appearing, then
                     we would lean towards rejecting the notion that only certain types of videos appear regularly on the YouTube Trending. As users will see, there does appear to be
                     some behaviors that indicate that there are patterns available for creators to use to appear on YouTube's Trending page. Thus, for this project, we want to find
                     evidence that supports the idea that there are certain types of videos that appear on the Trending Page, signaling that they hold advantages over other videos on
                     YouTube."
                   ),
                   p(htmlOutput("trend_link"))
                   ),
                 tabPanel(
                   "Group Information",
                   h3("/0 [Divide By Zero]"),
                   h4("Anh-Minh Nguyen"),
                   h4("Connor Carlson"),
                   h4("Thomas Harding"),
                   h4("Alec Bergen"),
                   h5("Date Produced: 5/30/2018")
                 )
                 )),
      tabPanel("Time",
               sidebarLayout(
                 sidebarPanel(selectInput(
                   "days",
                   "Day of the Week",
                   choices = c(
                     "Sunday",
                     "Monday",
                     "Tuesday",
                     "Wednesday",
                     "Thursday",
                     "Friday",
                     "Saturday"
                   )
                 )),
                 
                 mainPanel(
                   h2("Time vs Trending"),
                   plotlyOutput("time_plot"),
                   plotlyOutput("to_trending"),
                   htmlOutput("time_desc")
                 )
               )),
      tabPanel("Titles",
               sidebarLayout(
                 sidebarPanel(
                   radioButtons(
                     "choice",
                     label = strong("Word Searches"),
                     choices = list(
                       "Default" = "default",
                       "Positive" = "positive",
                       "Negative" = "negative",
                       "All" = "all"
                     )
                   ),
                   htmlOutput('choice_def')
                 ),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Breakdown",
                     h2(
                       "What are the most common words used in YouTube videos on the Trending page?"
                     ),
                     p(
                       "Titles are used to briefly describe and label a video. With the boom in video production and content, however,
                       it becomes hard to stand apart. In this section, we will explore what words are commonly found in these videos
                       to uncover any correlations."
                     ),
                     p(
                       "After exploring the data, users may find that many large studios basically control the Trending Page. They seem to push
                       many official trailers and music videos that dominate the Trending page. So, it seems as if the key to being famous on YouTube
                       as of the moment is to be immenseely powerful or famous already. A plethora of words are related to a 'bougie'
                       lifestyle as well."
                     )
                     ),
                   tabPanel(
                     "Word Chart",
                     plotOutput('wordBar', click = "my_click"),
                     verbatimTextOutput('wordText'),
                     p(htmlOutput("description"))
                   )
                     ))
                 )),
      
      tabPanel("Composition",
               fluidPage(
                 titlePanel("Composition of Trending Videos by Category"),
                 sidebarLayout(
                   sidebarPanel(
                     dateInput(
                       'date',
                       label = paste('Date input (2017/11/14 - 2018/03/30):'),
                       value = "2017-11-14",
                       format = "yyyy-mm-dd"
                     )
                   ),
                   mainPanel(
                     plotlyOutput("graph"),
                     dataTableOutput("table"),
                     htmlOutput('description_alec')
                     
                   )
                 )
               ))
                   )
  )
  
  
  
  )
