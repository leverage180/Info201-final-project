# install.packages("leaflet")
# install.packages("plot3D")
# install.packages("RColorBrewer")

library(dplyr)
library(plot3D)
library(RColorBrewer)
library(plotly)
library(viridis)
library(leaflet)

# Reads data
project_data <- read.csv('youtube-new/USvideos.csv', stringsAsFactors = F)
id_data <- read.csv('youtube-new/category_id.csv', stringsAsFactors = F)
# Names & Joins
colnames(id_data) <- c("category_id","category_name")
# Coerces int
id_data$category_id <- as.integer(id_data$category_id)
joined_data <- left_join(project_data, id_data)
# Filters data & runs mean of data
# p_data <- joined_data %>% filter(trending_date == "17.15.11") %>%  
#   group_by(category_name) %>%  summarize(
#     likes = median(likes),
#     dislikes = median(dislikes),
#     comment_count = median(comment_count),
#     views = median(views)
#   )

# Specifies date
select_date <- "17.15.11"
# Counts number of by category for date
for_fun <- joined_data  %>% mutate(count = 1) %>% filter(trending_date == select_date) %>%  
  group_by(category_name) %>% summarize(total = sum(count,na.rm = T))

# Sums category totals for specific date
q_data <- joined_data %>% filter(trending_date == select_date) %>%  
  group_by(category_name) %>%  summarize(
    likes = sum(likes, na.rm =T),
    dislikes = sum(dislikes, na.rm=T),
    comment_count = sum(comment_count,na.rm=T),
    views = sum(views,na.rm=T)
  )

n_occurance <- q_data <- joined_data %>% filter(trending_date == select_date) %>%  
  group_by(category_name) %>%  mutate(count = 1) %>%  summarize(total = sum(count))
scaled_data <- left_join(scaled_data, n_occurance)
scaled_data %>% str
arranged_data <- q_data %>% arrange(-likes) %>% mutate(ranking = rownames(arranged_data)) %>% 
  select(category_name, ranking)

# Joins data sets
scaled_data <- left_join(for_fun, q_data)
scaled_data <- left_join(scaled_data, arranged_data)
x <- scaled_data$views / scaled_data$total
y <- scaled_data$likes / scaled_data$total
z <- scaled_data$comment_count / scaled_data$total
id <- as.integer(scaled_data$ranking)

scaled_data %>% View

# x <- p_data$views
# y <- p_data$likes
# z <- p_data$comment_count
# # id <- p_data$category_name
# id <- p_data$dislikes


p <- plot_ly(scaled_data, x = ~x, y = ~y, text = ~category_name, type = 'scatter', size= ~total, mode = 'markers', 
             color = ~category_name, colors = 'Paired', opacity = .5,
             marker = list(sizemode = 'diameter')) %>%
  layout(title = paste('Ratio of likes to dislikes by category.', '\n', 'Size scaled for comment count.', '\n'),
         xaxis = list(title ="Views", showgrid = FALSE),
         yaxis = list(title = "Likes", showgrid = FALSE),
         colorscale='Viridis',
         showlegend = FALSE)
  
          