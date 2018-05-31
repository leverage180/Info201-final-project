library(shiny)
library(dplyr)
library(plot3D)
library(RColorBrewer)
library(plotly)
library(viridis)
library(leaflet)

# Functions
swap_date <- function(input_date) {
  year <- substr(input_date,1,4)
  month <- substr(input_date,6,7)
  day <-  substr(input_date,9,10)
  return_date <- paste0(year, "/", day, "/", month)
  return_date
}
# Reads data
project_data <- read.csv('youtube-new/USvideos.csv', stringsAsFactors = F)
project_data$trending_date <- gsub("[.]", "/", project_data$trending_date)
id_data <- read.csv('youtube-new/category_id.csv', stringsAsFactors = F)
# Names & Joins
colnames(id_data) <- c("category_id","category_name")
# Coerces int
id_data$category_id <- as.integer(id_data$category_id)
joined_data <- left_join(project_data, id_data)

ui <- fluidPage(
  titlePanel("Dates and date ranges"),
  sidebarLayout(
  sidebarPanel(
  dateInput('date',
            label = 'Date input (2017/11/14 - 2018/03/30):',
            value = "2017-11-14",
            format = "yyyy-mm-dd"
  )),
  mainPanel(
  plotlyOutput("graph"),
  tableOutput("table")
)
)
)
server <- function(input, output) {
  # Main Graph
  output$graph <- renderPlotly({
    
    for_fun <- joined_data  %>% mutate(count = 1) %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10))%>%
      group_by(category_name) %>% summarize(total = sum(count,na.rm = T))
    
    # Sums category totals for specific date
    q_data <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  summarize(
        likes = sum(likes, na.rm =T),
        dislikes = sum(dislikes, na.rm=T),
        comment_count = sum(comment_count,na.rm=T),
        views = sum(as.numeric(views),na.rm=T)
      )
    n_occurance <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  mutate(count = 1) %>%  summarize(total = sum(count))
    
    arranged_data <- joined_data %>% arrange(-likes) %>% mutate(ranking = rownames(joined_data)) %>%
      select(category_name, ranking)
    
    # Joins data sets
    scaled_data <- left_join(q_data, n_occurance)
    
    arranged_data <- scaled_data %>% arrange(-likes) %>% mutate(ranking = rownames(scaled_data)) %>%
      select(category_name, ranking)
    
    scaled_data <- left_join(scaled_data, arranged_data)

    x <- scaled_data$views / scaled_data$total
    y <- scaled_data$likes / scaled_data$total
    z <- scaled_data$comment_count / scaled_data$total
    id <- scaled_data$ranking
    
    p <- plot_ly(scaled_data, x = ~x, y = ~y, text = ~category_name, type = 'scatter', size= ~comment_count, mode = 'markers',
                 color = ~category_name, opacity = .5,
                 marker = list(sizemode = 'diameter')) %>%
      layout(title = paste('Ratio of likes to dislikes by category.', '\n', 'Size scaled for comment count.', '\n'),
             xaxis = list(title ="Views", showgrid = FALSE),
             yaxis = list(title = "Likes", showgrid = FALSE),
             showlegend = FALSE)
    p
  })
  output$table<- renderTable({
    
    for_fun <- joined_data  %>% mutate(count = 1) %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10))%>%
      group_by(category_name) %>% summarize(total = sum(count,na.rm = T))
    
    # Sums category totals for specific date
    q_data <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  summarize(
        likes = sum(likes, na.rm =T),
        dislikes = sum(dislikes, na.rm=T),
        comment_count = sum(comment_count,na.rm=T),
        views = sum(as.numeric(views),na.rm=T)
      )
    n_occurance <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  mutate(count = 1) %>%  summarize(total = sum(count))
    
    arranged_data <- joined_data %>% arrange(-likes) %>% mutate(ranking = rownames(joined_data)) %>%
      select(category_name, ranking)
    
    # Joins data sets
    scaled_data <- left_join(q_data, n_occurance)
    
    arranged_data <- scaled_data %>% arrange(-likes) %>% mutate(ranking = rownames(scaled_data)) %>%
      select(category_name, ranking)
    
    scaled_data <- left_join(scaled_data, arranged_data)
    scaled_data
  })
}

shinyApp(ui, server)

