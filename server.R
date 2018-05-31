#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


source("ui.R")

library(shiny)
library(plotly)

# Reads data
project_data <- read.csv('youtube-new/USvideos.csv', stringsAsFactors = F)
project_data$trending_date <- gsub("[.]", "/", project_data$trending_date)
id_data <- read.csv('youtube-new/category_id.csv', stringsAsFactors = F)
# Names & Joins
colnames(id_data) <- c("category_id","category_name")
# Coerces int
id_data$category_id <- as.integer(id_data$category_id)
joined_data <- left_join(project_data, id_data)

swap_date <- function(input_date) {
  year <- substr(input_date,1,4)
  month <- substr(input_date,6,7)
  day <-  substr(input_date,9,10)
  return_date <- paste0(year, "/", day, "/", month)
  return_date
}

############################################################################
################### Anh-Minh ###############################################
############################################################################

# Visit https://github.com/anhm1n/supahprojects to find more code
# Did not include for aesthetics purpose

# Create data frames from the cleaning we did above
# Write and read to save time
df_u <- read.csv("WordListFromTitles.csv", stringsAsFactors = F, header = T)
df_t <- read.csv("TitleList.csv", stringsAsFactors = F)

# Create a vector of clean titles to use to find sample
list_title <- as.vector(df_t$list_title)

list_title_word <- str_split(list_title, " ")

title_sampler <- function(x) {
  index_match <- rep(NA, length(list_title_word))
  for(i in 1:length(list_title_word)) {
    index_match[i] <- sum(list_title_word[[i]] == x) >= 1
  }
  index_match
} 

# Make a bing dictionary to join and find negative or postive words
bing_sentiments <- get_sentiments("bing")
bing_sentiments$word <- toupper(bing_sentiments$word) 

df_bing <- inner_join(bing_sentiments, df_u, by = "word")

plain_words <- c("THE", "THEIR", "THEY", "THEYRE", "YOUR", "YOU" , "A", "AN", "IS", "ISNT", "WILL",
                 "WONT", "DID", "DIDNT", "HAVE", "HAD", "WHEN", "WHERE", "HOW", "WHAT", "wHY", 
                 "HAVENT", "NOT", "SHOULD", "WOULD", "COULD", "BE", "BEING", "GET", "HADNT", "WE",
                 "THIS", "THERE", "IN", "MY", "TO", "AS", "I", "-", "ING", "IN", "FROM", "AT", "HE", "SHE",
                 "AND", "ON", "IT", "FOR", "OF", "WITH")

# Removes all plain words
df_c <- df_u[!(df_u$word %in% plain_words),]

#############################################################################################################

shinyServer(function(input, output) {
  
  ############################################################################
  ###################### CONNOR's WORK #######################################
  ############################################################################
  
  # Calculates a datafram of the average days until video reaches trending by hour and by day chosen
  ave_day_trend <- reactive({
    average_days_to_trending <- youtube_data %>% 
      filter(day_of_week == input$days) %>% 
      select(hour_publish, days_to_trending) %>% 
      group_by(hour_publish) %>% 
      summarise("average_day" = mean(days_to_trending))
    average_days_to_trending
  })
  
  # filters the youtube data by day chosen
  time_data <- reactive({
    youtube_data <- youtube_data %>% 
      filter(day_of_week == input$days)
    
  })
  
  # creates a plot based on the day chosen, with time of day on the x axis and the amount of videos in trending in the y axis
  output$time_plot <- renderPlotly({
    p <- ggplot(data = time_data()) +
      geom_bar(aes(x = hour_publish), fill = "red") +
      scale_y_continuous(limits = c(0, 750)) +
      labs(
        title = "Amount of Videos V. Time Published",
        x = "Hour Published",
        y = "Amount of Videos in Trending"
      )
    p <- ggplotly(p)
    p
    
  })
  
  # creates a plot based on the day chosen, with the time published on the x axis and the average amount of days it took to reach trending on the y axis 
  output$to_trending <- renderPlotly({
    p <- plot_ly(data = ave_day_trend(), x = ~hour_publish, y = ~average_day, type = 'scatter', mode = 'lines', color = "Red") %>% 
      layout(
        xaxis = list(title = "Hour Published"),
        yaxis = list(title = "Average Days Until Trending")
      )
    p
  })
  
  output$time_desc <- renderUI({
    
    HTML(paste0("These graphs show the effect on the time of day published and the day of the week published of a video on trending. As we found out from the 
                graphs the time published that has the most videos on trending is <strong>Tuesday at hour 9</strong>. Statstically this would be the best time to post a video.
                We also compared this to the amount of days it took for the video to reach trending. We found an inverse relationship between the amount of videos
                in trending and the amount of days it took for the video to get there. Therefore there someone is more likely to get into trending if it takes them
                less time to get there."))
  })

 ################################################################ 
######################### ANH-MINH'S WORK #######################
  
# Determines which data frame to use  
  decipher <- reactive({
    if(input$choice == "positive" | input$choice == "negative") {
      df_main <- df_bing %>% 
        filter(sentiment == input$choice)
    } else if (input$choice == "default") {
      df_main <- df_c
    } else {
      df_main <- df_u
    }
    df_main <- top_n(df_main, n = 16, wt = number)
    df_main
  })
 
# Factorizes word to have interactivity available   
  factorize <- reactive({
    df_main <- decipher()
    df_main$word <- factor(decipher()$word)
    df_main
  })
  
# Create plot of word count  
  output$wordBar <- renderPlot({
    ggplot(data = decipher()) +
      geom_bar(aes(x = word, y = number), stat = "identity", fill = "red") +
      theme(axis.text = element_text(size = 10)) +
      coord_flip() +
      labs(title = paste("TOP", toupper(input$choice)), x = "Word", y = "Number")
  })

  # Creates a random sample of 3 titles for user to click on  
  output$wordText <- renderPrint({
    if (is.null(input$my_click$y)) {
      return("")
    } else {
      lvls <- levels(factorize()$word)
      string <- lvls[round(input$my_click$y)]
      
      df_samp <- df_t[title_sampler(as.character(string)), ]
      t_samp <- df_samp[sample(length(df_samp), size = 3)] %>% 
        str_replace_all("\\s+", " ")
      
      cat(paste0(string, "\nNumber = ", subset(decipher(), word == string)[, "number"], 
                 "\n", t_samp[1],
                 "\n", t_samp[2],
                 "\n", t_samp[3]))
    }
  })
  
  # Text description of data
  output$description <- renderPrint({
    if(input$choice == "default") {
      HTML(paste0("As we observe the most common default words in YouTube's trending page, we notice
                  that many of the words are words used in popular movie and tv show trailers. It seems
                  that the YouTube Trending page leans towards promotion of studio created content instead 
                  of individual creator based content in this case. Some words such as <b>ME, MAKEUP, </b>and<b> DAY </b> indicate
                  that YouTubers who are promiment enough by name can make it onto YouTube. It also
                  indicates a diverse audience, and that people could be genuinely interested in the 
                  lives of promiment YouTubers. It seems that music videos are very popular by the appearance 
                  of <b>AUDIO, MUSIC, VIDEO</b> and <b>FT</b>, demonstrating YouTube's ability to promote
                  music, but primarily for corporate size figures. Lastly, words like <b>NEW</b> and <b>2018</b>
                  show some evidence that YouTube's audience prefers new and/or updated content."))  
    } else if(input$choice == "positive") {
      HTML(paste0("We can probably guess with strong confidence that <b>TRUMP</b> is not a reference to
                  \'winning\' but the US President Trump. It seems that due to his controversial nature, it
                  can generate quite a number of views to reach Trending consistenly. <b>MARVEL</b> is another
                  one that probably means the studio Marvel. A few other words like <b>HONEST</b> and <b>HOT</b>
                  seem to be dominated by big channels that started on YouTube, Honest Trailers and First We Feast.
                  Most of the other words like <b>BEST</b> and <b>LIKE</b> seem to be informative videos that catch
                  a viewer's attention by showing viewers the best extremes of life."))  
    } else if(input$choice == "negative") {
      HTML(paste0("Words like <b>EXPENSIVE</b> and <b>FAKE</b> seem to be common because of their appeal to YouTube's 
                  general audience. With the appearance of <b>DEATH</b> and <b>DEAD</b>, it seems that stuff that people
                  don't regularly experience fascinates them. Many of these words like <b>BREAK</b> and <b>SUCK</b>
                  are not being used with a negative connotation. There still appears to be channels like CinemaSins
                  and Vanity fair, that dominate thus bringing up the words <b>WRONG</b> and <b>VANITY</b>. Music videos
                  also introduce a lots of words that make the top negative list because it seems everyone likes a good 
                  sad song once in a while."))
    } else {
      HTML(paste0("This chart just shows the top words in general. As we can note, many of them are used for grammatical purposes,
                  and we assume not to be important enought to consider."))
    }
  })
  
  # Links to YouTube's Trending page
  output$trend_link <- renderPrint({
    HTML(paste0("<a href = https://www.youtube.com/feed/trending> Here </a>is a link to YouTube's Trending page."))
  })
  
  # Prints definition of title selection
  output$choice_def <- renderPrint({
    if (input$choice == "default") {
      HTML(paste0("<b>Default</b><br> This selection omits any of the transition words like 'with' or 'the'. "))
    } else if (input$choice == "positive") {
      HTML(paste0("<b>Positive</b><br> This selection uses any word determined by the Bing dictionary to have a positive
                  sentiment."))
    } else if (input$choice == "negative") {
      HTML(paste0("<b>Negative</b><br> This selection uses any word determined by the Bing dictionary to have a negative sentiment."))
    } else {
      HTML(paste0("<b>All</b><br> All the words in titles found from YouTube's Trending page are considered."))
    }
  })
  ############################################################################
  ################################# ALECS' WORK #############################
  ############################################################################

  # Main Graph
  output$graph <- renderPlotly({
    
    for_fun <- joined_data  %>% mutate(count = 1) %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10))%>%
      group_by(category_name) %>% summarize(total = sum(count))
    
    # Sums category totals for specific date
    q_data <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  summarize(
        likes = sum(likes),
        dislikes = sum(dislikes),
        comment_count = sum(comment_count),
        views = sum(views)
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
      group_by(category_name) %>% summarize(total = sum(count))
    
    # Sums category totals for specific date
    q_data <- joined_data %>% filter(trending_date == substr(swap_date(as.character(input$date)), 3, 10)) %>%
      group_by(category_name) %>%  summarize(
        likes = sum(likes),
        dislikes = sum(dislikes),
        comment_count = sum(comment_count),
        views = sum(views)
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
  output$description_alec <- renderUI({
    code_break <- "<br><br>"
    str1 <- "The graphs visualizes the number of likes per view as a scatter plot. The size of the dot represents
    the numbers of views that the video reached when the video became a trending video on YouTube. The functionality
    of this application allows for analysis of likes/view scaled by comment count for each day (2017/11/14 - 2018/03/30)"
    str2 <- "This graph gives the user the understanding of the make-up of a trending video. This sort of analysis could
    be used to gain insight into what it takes to make their video become a trending video in their marketed category. While each category's
    ratio varies per day of the week, there are general trends that emerge for each video, and the graph has a rough positive correlation."
    HTML(paste(code_break, str1, str2, sep="<br><br>"))
  })
})