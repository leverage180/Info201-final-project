library(shiny)
library(dplyr)
library(plotly)

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
  titlePanel("Composition of Trending Videos by Category"),
  sidebarLayout(
  sidebarPanel(
  dateInput('date',
            label = paste('Date input (2017/11/14 - 2018/03/30):'),
            value = "2017-11-14",
            format = "yyyy-mm-dd"
  )),
  mainPanel(
  plotlyOutput("graph"),
  dataTableOutput("table"),
  htmlOutput('description')
  
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
    # Counts the number of occurances of each group
    n_occurance <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  mutate(count = 1) %>%  summarize(total = sum(count))
    # # Arranges data by likes and constructs datafrane of rankings by likes
    # arranged_data <- joined_data %>% arrange(-likes) %>% mutate(ranking = rownames(joined_data)) %>%
    #   select(category_name, ranking)
    
    # Joins data sets
    scaled_data <- left_join(q_data, n_occurance)
    # Arranges data by likes and constructs datafrane of rankings by likes
    arranged_data <- scaled_data %>% arrange(-likes) %>% mutate(ranking = rownames(scaled_data)) %>%
      select(category_name, ranking)
    # Joins data sets
    scaled_data <- left_join(scaled_data, arranged_data)
    # Creates ratio
    x <- scaled_data$views / scaled_data$total
    y <- scaled_data$likes / scaled_data$total
    z <- scaled_data$comment_count / scaled_data$total
    id <- scaled_data$ranking
    # Construct and return plotly graph
    p <- plot_ly(scaled_data, x = ~x, y = ~y, text = ~category_name, type = 'scatter', size= ~comment_count, mode = 'markers',
                 color = ~category_name, opacity = .5,
                 marker = list(sizemode = 'diameter')) %>%
      layout(title = paste('Ratio of likes to dislikes by category.', '\n', 'Size scaled for comment count.', '\n'),
             xaxis = list(title ="Views", showgrid = FALSE),
             yaxis = list(title = "Likes", showgrid = FALSE),
             showlegend = FALSE)
    p
  })
  # Same code as above, but as a data table
  output$table<- renderDataTable({
    
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
    
    scaled_data <- left_join(q_data, n_occurance)
    
    arranged_data <- scaled_data %>% arrange(-likes) %>% mutate(ranking = rownames(scaled_data)) %>%
      select(category_name, ranking)
    
    scaled_data <- left_join(scaled_data, arranged_data)
    scaled_data
  })
  output$description <- renderUI({
    code_break <- "<br><br>"
    str1 <- "The graphs visualizes the number of likes per view as a scatter plot. The size of the dot represents
    the numbers of views that the video reached when the video became a trending video on YouTube. The functionality
    of this application allows for analysis of likes/view scaled by comment count for each day (2017/11/14 - 2018/03/30)"
    str2 <- "This graph gives the user the understanding of the make-up of a trending video. This sort of analysis could
    be used to gain insight into what it takes to make their video become a trending video in their marketed category. While each category's
    ratio varies per day of the week, there are general trends that emerge for each video, and the graph has a rough positive correlation."
    HTML(paste(code_break, str1, str2, sep="<br><br>"))
  })
}

shinyApp(ui, server)

