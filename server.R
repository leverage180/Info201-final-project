#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
#                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
#                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
#                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', '�?'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
#                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
# 
#  youtube <- read.csv("USVideos.csv", stringsAsFactors = F)
# 
#  possessive <- function(x) {
#    phrase <- x
#    if (str_detect(phrase, "\\$")) {
#      phrase <- "$"
#    }
#    str_replace_all(phrase, "\\'s", "") %>%
#      str_replace_all("\\'S", "")
# }
# 
#  bland <- function(x) {
#    if (str_detect(x, "ð")) {
#      title_clean <- iconv(x, "UTF-8", "ASCII", sub = "")
#    } else {
#      title <-  iconv(x, "UTF-8", "UTF-8", sub = "")
#      title_clean <- chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), title) %>%
#        iconv("UTF-8", "ASCII", sub = "")
#    }
#    list <- unlist(str_split(title_clean, " "))
#    paste(lapply(list, possessive), collapse = " ") %>%
#      str_replace_all("[\\'.]", "") %>%
#      str_replace_all("[^[A-Za-z0-9;_-]\\$]", " ") %>%
#      toupper()
#  }
# 
#  list_title <- unlist(lapply(youtube$title, bland))
# 
#  title_df <- data.frame(list_title)
# 
#  write.csv(title_df, "TitleList.csv", row.names = F)

# ###########################################
# ###########################################
# ###########################################

# list_all <- str_split(list_title, " ") %>%
#   unlist()
# 
# list_unique <- unique(list_all)
# 
# df_u <- data.frame(list_unique, stringsAsFactors = F)
# 
# colnames(df_u)[1] <- "word"
# 
# count_strings <- function(x) {
#   sum(list_all == x)
# }
# 
# df_u <- mutate(df_u, number = lapply(word, count_strings))
# 
# df_u <- filter(df_u, word != "")
# 
# x <- vapply(df_u$number, length, 1L)
# df_u <- df_u[rep(rownames(df_u), x), ]
# df_u$number <- unlist(df_u$number, use.names = FALSE)
# 
# write.csv(df_u, file = "WordListFromTitles.csv", row.names = F)
##########################################################################
##########################################################################
##########################################################################
source("ui.R")

library(shiny)
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
      labs(
        x = "Hour Published",
        y = "Days Until Video Reached Trending"
      )
    
    p <- ggplotly(p, tooltip = "y")
    p
  })
  
  df_u <- read.csv("WordListFromTitles.csv", stringsAsFactors = F, header = T)
  df_t <- read.csv("TitleList.csv", stringsAsFactors = F)
  
  list_title <- as.vector(df_t$list_title)
  
  list_title_word <- str_split(list_title, " ")
  
  title_sampler <- function(x) {
    index_match <- rep(NA, length(list_title_word))
    for(i in 1:length(list_title_word)) {
      index_match[i] <- sum(list_title_word[[i]] == x) >= 1
    }
    index_match
  } 
  
  bing_sentiments <- get_sentiments("bing")
  bing_sentiments$word <- toupper(bing_sentiments$word) 
  
  df_bing <- inner_join(bing_sentiments, df_u, by = "word")
  
  plain_words <- c("THE", "THEIR", "THEY", "THEYRE", "YOUR", "YOU" , "A", "AN", "IS", "ISNT", "WILL",
                   "WONT", "DID", "DIDNT", "HAVE", "HAD", "WHEN", "WHERE", "HOW", "WHAT", "wHY", 
                   "HAVENT", "NOT", "SHOULD", "WOULD", "COULD", "BE", "BEING", "GET", "HADNT", "WE",
                   "THIS", "THERE", "IN", "MY", "TO", "AS", "I", "-", "ING", "IN", "FROM", "AT", "HE", "SHE",
                   "AND", "ON", "IT", "FOR", "OF", "WITH")
  
  df_c <- df_u[!(df_u$word %in% plain_words),]
  
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
  
  factorize <- reactive({
    df_main <- decipher()
    df_main$word <- factor(decipher()$word)
    df_main
  })
  
  
  output$wordBar <- renderPlot({
    ggplot(data = decipher()) +
      geom_bar(aes(x = word, y = number), stat = "identity", fill = "red") +
      theme(axis.text = element_text(size = 10)) +
      coord_flip() +
      labs(title = paste("TOP", toupper(input$choice)), x = "Word", y = "Number")
  })
  
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
})
