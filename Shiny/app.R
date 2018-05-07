# load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(markovchain)

data <- suppressMessages(read_csv("count.de.en.csv"))
data1 <- suppressMessages(read_csv("news.bing_count.csv"))
data2 <- suppressMessages(read_csv("news.posneg_count.csv"))
data3 <- suppressMessages(read_csv("tweet_count.en.csv"))
data4 <- suppressMessages(read_csv("tweet_count.de.csv"))
data5 <- suppressMessages(read_csv("tweet.bing_count.csv"))
data6 <- suppressMessages(read_csv("tweet.posneg_count.csv"))
load("mcfit.RData")
load("mcfit_de.RData")

ui <- dashboardPage(
  dashboardHeader(title = "English and German Text Analysis", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("News Articles", 
               tabName = "news_articles", 
               icon = icon("list-alt", lib = "glyphicon"),
               menuSubItem("Top Word Counts", tabName = "overview_na"),
               menuSubItem("Sentiment Analysis", tabName = "sen_analysis_na")),
      menuItem("Twitter", 
               tabName = "twitter", 
               icon = icon("twitter", lib = "font-awesome"),
               menuSubItem("Top Word Counts", tabName = "overview_tw"),
               menuSubItem("Sentiment Analysis", tabName = "sen_analysis_tw")),
      menuItem("Application of n-grams", 
               tabName = "for_fun", 
               icon = icon("menu-hamburger", lib = "glyphicon"),
               menuSubItem("Markov Chain Text Generator", tabName = "textgen"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row
    tabItems(
      # First tab content
      tabItem(tabName = "overview_na",
              h2("Top Word Counts"),
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
                  plotOutput("plot1"), width = 12),
                box(tableOutput("text1"), width = 12)
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
                  plotOutput("plot2", height = 1000), width = 12),
                box(tableOutput("text2"), width = 12),
                box(tableOutput("text3"), width = 12)
              )
      ),
      # Third tab content
      tabItem(tabName = "overview_tw",
              h2("Top Word Counts"),
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
                  plotOutput("plot3"), width = 12),
                box(tableOutput("text4"), width = 12)
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
                  plotOutput("plot4", height = 1000), width = 12),
                box(tableOutput("text5"), width = 12),
                box(tableOutput("text6"), width = 12)
              )
      ),
      # Fifth tab content
      tabItem(tabName = "textgen",
              h2("Markov Chain Text Generator"),
              fluidRow(
                # create buttons to select language
                box(radioButtons("radio1", label = h3("Select Language:"),
                                 choices = list("English" = 1, "German" = 2), selected = 1), width = 6),
                # create a slider to select how many words to generate
                box(title = "Select Number of Words to Generate:",
                    sliderInput("slider5", "Words:", min = 1, max = 100,
                                value = 1), width = 6),
                # create a table to print out text and time stamp 
                box(h3("Text"), tableOutput("table1"), width = 12),
                box(tableOutput("text7"), width = 12)
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
  
  # English translation of top german news article word counts
  output$text1 <- renderText({
    paste('English Translation:', 
          "user, data, boss, company, millions, information, congress, 
          data scandal, question, app, mistake, year, company, senator, network, responsibility, affected, 
          become, should, scandal, already, EU, hearing, trump, platform, donald, 
          user data, internet, privacy, people.")
  })
  
  # Plots of count for positive and negative sentiment for english and german news articles
  output$plot2 <- renderPlot({
    data1 <- data1 %>% group_by(sentiment) %>% top_n(input$slider2, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    data2 <- data2 %>% group_by(sentiment) %>% top_n(input$slider2, n) %>% ungroup() %>% mutate(word = reorder(word, n))
    
    gridExtra::grid.arrange(
      ggplot(data1, aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + 
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Set2") + 
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("English"),
      ggplot(data2, aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + 
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Set2") +
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("German"), 
      nrow = 2)
  })
  
  # English translation of top german negative word counts
  output$text2 <- renderText({
    paste('English Translation for negative sentiments:', 
          "mistake, scandal, end, criticism, missuse, sorrow, propoganda, crisis, not allowed, damage, short,
          hard, adjust, delete, critics, controversy, exacerbate, outrage, affaire, offend, fail,
          avoid, lose, banish, heavy, manipulation, deletion, slow, close, small, hate, threaten.")
    
  })
  
  # English translation of top german positive word counts
  output$text3 <- renderText({
    paste('English Translation for positive sentiments:',
          "responsibility, knowledge, protect, better, responsible, exactly, easy, solution, safety,
          obtain, well-known, consent, connect, aimed, explain, especially, goal, way, trust, promise,
          style, rights, optimistic, new, possibility, possible, massive, solve, intelligence, to make better,
          ready, correct.")
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
  
  # English translation of top german tweet word conts
  output$text4 <- renderText({
    paste('English Translation:',
          "data, hearing, congress, europe, signed, boss, question, senate, senator, data scandal,
          users, questioning, EU, year, parliament, live, internet, data protection, appearance,
          billions, topic, apologize, asked, world, online, advertisement, a lot, responsibility,
          user, pay.")
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
        facet_wrap(~sentiment, scales = "free_y") + scale_fill_brewer(palette = "Accent") +
        labs(y = "n", x = NULL) + coord_flip() + ggtitle("German"),
      nrow = 2)
  })
  
  # English translation for top german negative word counts
  output$text5 <- renderText({
    paste('English Translation for negative sentiments:', 
          "scandal, mistake, naive, at fault, censor, end, doubt, delete, farce, betray, incredibility,
          propoganda, criticism, crazy, vague, bad, short, hard, shattered, madness, ignorance, fault,
          nervous, deprivation, lie, mad, negligence, disappointed, outrage, restrict, vicious.")
  })
  
  # English translation for top german positive word counts
  output$text6 <- renderText({
    paste('English Translation for positive sentiments:', 
          "responsibility, easy, explain, better, knowledge, love, right, protect, intelligence, exactly,
          safety, earned, rights, laugh, clear, important, fast, possibility, believe, asked, free, interested,
          to make better, well-known, worth, understand, surprised, to be happy, firm, best.")
  })
  
  # generating Markov Chain text for each language and selected length
  output$table1 <- renderText({
    if (input$radio1 == 1) {
      markovchainSequence(n = input$slider5, markovchain = mcfit)
    }
    else {
      markovchainSequence(n = input$slider5, markovchain = mcfit_de)
    }
  })
  
  # note of caution about text generated
  output$text7 <- renderText({
    paste("Please note, this is not actual text from any news article or tweet gathered throughout this study;
          this is purely for demonstration purposes and is used to illustrate an application of n-grams.")
  })
}

shinyApp(ui, server)
