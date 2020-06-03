#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

importPackage <- function(){
    library(twitteR)
    library(SnowballC)
    library(devtools)
    library(Rstem)
    library(tm)
    library(NLP)
    library(SentimentAnalysis)
    library(plyr)
    library(ggplot2)
    library(RColorBrewer)
    library(wordcloud)
    library(sentiment)
    library(dplyr)
}

#import package
importPackage()

authenticateApi <- function(){
  #persiapan untuk mendapat akses ke twitter
  consumer_key <- "isi consumer key milik mu"
  consumer_secret <- "isi consumer key milik mu"
  access_token <- "isi consumer key milik mu"
  access_secret <- "isi consumer key milik mu"
  
  #get data from twitter
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

#run authenticate api so that we have access to get data from twitter
authenticateApi();

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sentiment analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #textInput("cari", "Masukkan topik atau hastag yang akan cari", "jokowi"),
          #sliderInput(
           # "jumlah",
            #"Jumlah data yang akan diambil",
            #min = 100,
            #max = 1000,
            #value = 100),
          #submitButton(text="getText")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Plot Klasifikasi Emosi", plotOutput("emotionPlot")), #plot klasifikasi by emotion
                       tabPanel("Plot KlasifikasiPolaritas", plotOutput("polarityPlot")), #plot klasifikasi by polarity
                       tabPanel("Hasil Analisis", DT::dataTableOutput('hasil')), #data hasil analisis
                       tabPanel("Word Cloud", plotOutput("wordcloud")), #word cloud
                       tabPanel("Data", DT::dataTableOutput('data')) # data sebelum analisis
                       )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  #sentimen <- reactive({
                               ### phase 1 prepare data from get until cleaning
    #print(input$cari)
    #print(input$jumlah)
    tweet <- searchTwitter("corona", n = 300, lang="en", retryOnRateLimit = 10e3)
    #tweet_text <- sapply(tweet, function(x) x$getText)
    maiden_tweet <- twListToDF(tweet)
    tweet_text <- data.frame(maiden_tweet['text'])
    tweet_text <- tweet_text$text
    #tweet = read.csv('~/Desktop/Proyek_Data_Science/CoronavirusUSA_13_4_to_29_4-2020_Clean_Data3.csv')
    #twet_text = tweet$tweet_text
    
    #save file
    #write.csv(tweet_text,'~/Desktop/Proyek_Data_Science/tweet.csv', row.names = FALSE)
    
    #cleaning data
    # remove retweet entities
    tweet_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_text)
    # remove at people
    tweet_text = gsub("@\\w+", "", tweet_text)
    # remove punctuation
    tweet_text = gsub("[[:punct:]]", "", tweet_text)
    # remove numbers
    tweet_text = gsub("[[:digit:]]", "", tweet_text)
    # remove html links
    tweet_text = gsub("http\\w+", "", tweet_text)
    # remove unnecessary spaces
    tweet_text = gsub("[ \t]{2,}", "", tweet_text)
    tweet_text = gsub("^\\s+|\\s+$", "", tweet_text)
    tweet_text = gsub("note", "", tweet_text)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
        # create missing value
        y = NA
        # tryCatch error
        try_error = tryCatch(tolower(x), error=function(e) e)
        # if not an error
        if (!inherits(try_error, "error"))
            y = tolower(x)
        # result
        return(y)
    }
    # lower case using try.error with sapply 
    tweet_text = sapply(tweet_text, try.error)
    # remove NAs in some_txt
    tweet_text = tweet_text[!is.na(tweet_text)]
    names(tweet_text) = NULL
    #write.csv(tweet_text,'~/Desktop/Proyek_Data_Science/tweet.csv', row.names = FALSE)
    
    #remove english common stopwords with tm_map
    #tweet_text <- tm_map(tweet_text, removeWords, stopwords("en"))
    #remove extra white space
    #tweet_text <- tm_map(tweet_text, stripWhitespace)
    
    
    #### phase 2, classify emotion (sadness, joy, angry, ) and classify polarity (positif, negatif)
    #classify emotion
    text_emotion = classify_emotion(tweet_text, algorithm="bayes", prior = 1.0)
    #get emotion best fit
    emotion = text_emotion[,7]
    #subtitute NA's by Unknown
    emotion[is.na(emotion)] = "unknown"
    
    #check polarity
    text_polarity = classify_polarity(tweet_text, algorithm = "bayes")
    #get polarity best fit
    polarity = text_polarity[,4]
    
    #### phase 3, prepare new data frame with the result of analysis sentiment
    # save classify and text to data frame
    result = data.frame(text = tweet_text, emotion = emotion, polarity = polarity, stringsAsFactors = FALSE)
    #sort data frame
    result = within(result, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing = TRUE))))
    
    #make term-document matrix
    #firstly we must convert data into corpus
    corpus <- Corpus(VectorSource(tweet_text))
    dtm <- TermDocumentMatrix(corpus)
    matrix <- as.matrix(dtm)
    sort_by_freq <- sort(rowSums(matrix),decreasing = TRUE)
    word_cloud <- data.frame(word = names(sort_by_freq), freq=sort_by_freq)  
    
    #return(list(
    #  "data" = tweet,
    #  "tweet" = tweet_text,
    #  "emotion" = emotion,
    #  "polarity" = polarity,
    #  "word_cloud" = word_cloud
   # ))
  #})
  output$emotionPlot <- renderPlot({
    #plot by emotions
    ggplot(result, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      ggtitle("Sentiment Analysis of Spider-Man: Far From Home by emotion")
  })
  
  output$polarityPlot <- renderPlot({
    ggplot(result, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette = "RdGy") +
      ggtitle("sentiment analysis of spider-man: far from home by polarity") +
      labs(x="polarity",y="number of tweets")
  })
  
  output$hasil = DT::renderDataTable({
    DT::datatable(result, list(lengthChange = FALSE))
  })
    
  output$wordcloud <- renderPlot({
    wordcloud(words = word_cloud$word, freq = word_cloud$freq, min.freq = 1,
              max.words = 200, random.order = FALSE, rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  })
  
  output$data = DT::renderDataTable({
    DT::datatable(maiden_tweet, list(lengthChange = FALSE))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
