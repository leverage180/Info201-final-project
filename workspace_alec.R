library(shiny)
library(dplyr)
library(plot3D)
library(RColorBrewer)
library(plotly)
library(viridis)
library(leaflet)

# Reads data
project_data <- read.csv('youtube-new/USvideos.csv', stringsAsFactors = F)
project_data$trending_date <- gsub("[.]", "/", project_data$trending_date)
id_data <- read.csv('youtube-new/category_id.csv', stringsAsFactors = F)
# Names & Joins
colnames(id_data) <- c("category_id","category_name")
# Coerces int
id_data$category_id <- as.integer(id_data$category_id)
joined_data <- left_join(project_data, id_data)


min_date <- as.Date(min(project_data$trending_date),format = "%y/%m/%d")
max_date <- as.Date(max(project_data$trending_date),format = "%y/%m/%d")

ui <- fluidPage(
  titlePanel("Dates and date ranges"),
    
    dateRangeInput('date_selection',
                   label = paste('Date range input 2: range is limited,',
                                 'dd/mm/yy, language: fr, week starts on day 1 (Monday),',
                                 'separator is "-", start view is year'),
                   start = min_date, end = max_date,
                   min = min_date, max = max_date,
                   separator = " - ", format = "yy/mm/dd",
                   startview = 'year', language = 'en'
    ),
  textOutput("date1")
  )





server <- function(input, output, session) {
  
  # input$date and others are Date objects. When outputting
  # text, we need to convert to character; otherwise it will
  # print an integer rather than a date.

  
  output$date1 <- renderText({
          date_vector <- as.vector(gsub("-", "/", as.character(input$date_selection, collapse = " ")))
          date_vector[1]
          input$date_selection
    
  })
}

shinyApp(ui, server)
