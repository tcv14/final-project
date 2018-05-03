# load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(twitteR)
library(markovchain)

data <- suppressMessages(read_csv("count.de.en.csv"))
data1 <- suppressMessages(read_csv("news.bing_count.csv"))
data2 <- suppressMessages(read_csv("news.posneg_count.csv"))
data3 <- suppressMessages(read_csv("tweet_count.en.csv"))
data4 <- suppressMessages(read_csv("tweet_count.de.csv"))
data5 <- suppressMessages(read_csv("tweet.bing_count.csv"))
data6 <- suppressMessages(read_csv("tweet.posneg_count.csv"))
consumerKey <- "aJNOJTkWVtVeHpg6j1aClFIDr"
consumerSecret <- "cXoHnFrryqmfjS0uB13x66HhODPhfFO5gRQWCE5KBMI5wbeyGm"
api_key <- "aJNOJTkWVtVeHpg6j1aClFIDr"
api_secret <- "cXoHnFrryqmfjS0uB13x66HhODPhfFO5gRQWCE5KBMI5wbeyGm"
access_token <- "975190164925485056-kcPzJmWbOKe8K2XSPiRolaHj6mdKjAp"
access_token_secret <- "JCTKEegnNogKQxTyxndhrfVQip6EPrjHZzJXOEVcnTU2E"
my_oauth <- setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
load("mcfit.RData")
load("mcfit_de.RData")

ui <- dashboardPage(
  dashboardHeader(title = "English and German Text Analysis", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("News Articles", 
               tabName = "news_articles", 
               icon = icon("list-alt", lib = "glyphicon"),
               menuSubItem("Overview", tabName = "overview_na"),
               menuSubItem("Sentiment Analysis", tabName = "sen_analysis_na")),
      menuItem("Twitter", 
               tabName = "twitter", 
               icon = icon("twitter", lib = "font-awesome"),
               menuSubItem("Overview", tabName = "overview_tw"),
               menuSubItem("Sentiment Analysis", tabName = "sen_analysis_tw")),
      menuItem("For Fun!", 
               tabName = "for_fun", 
               icon = icon("gift", lib = "glyphicon"),
               menuSubItem("Real-Time Data Generator", tabName = "realtimedata"),
               menuSubItem("Text Generator", tabName = "textgen"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row
    tabItems(
      # First tab content
      tabItem(tabName = "overview_na",
              h2("Overview"),
              fluidRow(
                # Create a box for the slider
                box(title = "Select Top Number of Words to Display:",
                    sliderInput("slider1", "Words:", min = 1, max = 30,
                                value = 5), width = 12),
                # plot of count comparison
                box(
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                       style = "text-align: center;"
                                     )), 
                  plotOutput("plot1"), width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "sen_analysis_na",
              h2("Sentiment Analysis"),
              fluidRow(
                # Create a box for the slider
                box(title = "Select Top Number of Words to Display:",
                    sliderInput("slider2", "Words:", min = 1, max = 30,
                                value = 5), width = 12),
                # plot of bing positive and negative sentiments
                box(
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                       style = "text-align: center;"
                                   )),
                  plotOutput("plot2", height = 1000), width = 12)
              )
      ),
      # Third tab content
      tabItem(tabName = "overview_tw",
              h2("Overview"),
              fluidRow(
                # Create a box for the slider
                box(title = "Select Top Number of Words to Display:",
                    sliderInput("slider3", "Words:", min = 1, max = 30,
                                value = 5), width = 12),
                # plot of count comparison 
                box(
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                       style = "text-align: center;"
                                   )),
                  plotOutput("plot3"), width = 12)
              )
      ),
      # Fourth tab content
      tabItem(tabName = "sen_analysis_tw",
              h2("Sentiment Analysis"),
              fluidRow(
                # Create a box for the slider
                box(title = "Select Top Number of Words to Display:",
                    sliderInput("slider4", "Words:", min = 1, max = 30,
                                value = 5), width = 12),
                # plot of bing positive and negative sentiments
                box(
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                       style = "text-align: center;"
                                   )),
                  plotOutput("plot4", height = 1000), width = 12)
              )
      ),
      # Fifth tab content
      tabItem(tabName = "realtimedata",
              h2("Real-Time Data Generator"),
              fluidRow(
                # create buttons to select language
                box(radioButtons("radio1", label = h3("Select Language:"),
                             choices = list("English" = 1, "German" = 2), selected = 1), width = 6),
                # create a slider to select how many data points to gather
                box(title = "Select Number of Tweets to Gather:",
                    sliderInput("slider5", "Tweets:", min = 1, max = 5,
                                value = 1), width = 6),
                # create a table to print out text and time stamp 
                box(h3("Data"), tableOutput("table1"), width = 12)
              )
      ),
      # Sixth tab content
      tabItem(tabName = "textgen",
              h2("Markov Chain Text Generator"),
              fluidRow(
                # create buttons to select language
                box(radioButtons("radio2", label = h3("Select Language:"),
                                 choices = list("English" = 1, "German" = 2), selected = 1), width = 6),
                # create a slider to select how many words to generate
                box(title = "Select Number of Words to Generate:",
                    sliderInput("slider6", "Words:", min = 1, max = 100,
                                value = 1), width = 6),
                # create a table to print out text and time stamp 
                box(h3("Text"), tableOutput("table2"), width = 12)
              )
      )
    )
  )
)


server <- function(input, output) {
  
  # Plots of count for english and german news articles
  output$plot1 <- renderPlot({
    data <- data %>% group_by(language) %>% top_n(input$slider1, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    print(ggplot(data, aes(word, n, fill = language)) + geom_col(show.legend = FALSE) + 
            facet_wrap(~language, scales = "free_y") + labs(y = "n", x = NULL) + coord_flip())
  })
  
  # Plots of count for positive and negative sentiment for english and german news articles
  output$plot2 <- renderPlot({
    data1 <- data1 %>% group_by(sentiment) %>% top_n(input$slider2, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    data2 <- data2 %>% group_by(sentiment) %>% top_n(input$slider2, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    
    gridExtra::grid.arrange(
      ggplot(data1, aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + 
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Set3") + 
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("English"),
      ggplot(data2, aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + 
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Set2") +
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("German"), 
      nrow = 2)
  })
  
  # Plots of count for english and german tweets
  output$plot3 <- renderPlot({
    data3 <- data3 %>% top_n(input$slider3, n) %>% mutate(word = reorder(word, n))
    data4 <- data4 %>% top_n(input$slider3, n) %>% mutate(word = reorder(word, n))
    gridExtra::grid.arrange(
      ggplot(data3, aes(word, n)) + geom_col(fill = "royalblue3") + xlab(NULL) + 
        coord_flip() + ggtitle("English"),
      ggplot(data4, aes(word, n)) + geom_col(fill = "palevioletred4") + xlab(NULL) + 
        coord_flip() + ggtitle("German"),
      nrow = 1)
  })
  
  # Plots of count for positive and negative sentiment for english and german tweets
  output$plot4 <- renderPlot({
    data5 <- data5 %>% group_by(sentiment) %>% top_n(input$slider4, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    data6 <- data6 %>% group_by(sentiment) %>% top_n(input$slider4, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    gridExtra::grid.arrange(
      ggplot(data5, aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + 
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Accent") + 
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("English"),
      ggplot(data6, aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Set1") +
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("German"),
      nrow = 2)
  })
  
  output$table1 <- renderTable({
      if (input$radio1 == 1) {
        rt_en <- searchTwitter('#Facebook + Zuckerberg -filter:retweets', n=input$slider5, lang="en")
        rt_en <- twListToDF(rt_en)
        rt_en <- rt_en %>% select(text, created)
        rt_en[,2] <- as.character(rt_en[,2])
        rt_en }
      else {
        rt_de <- searchTwitter('#Facebook + Zuckerberg -filter:retweets', n=input$slider5, lang="de")
        rt_de <- twListToDF(rt_de)
        rt_de <- rt_de %>% select(text, created)
        rt_de[,2] <- as.character(rt_de[,2])
        rt_de }
    })
  
  output$table2 <- renderText({
    if (input$radio2 == 1) {
      markovchainSequence(n = input$slider6, markovchain = mcfit)
    }
    else {
      markovchainSequence(n = input$slider6, markovchain = mcfit_de)
    }
  })
}

shinyApp(ui, server)
