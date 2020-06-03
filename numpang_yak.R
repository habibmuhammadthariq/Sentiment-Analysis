#### phase 3, prepare new data frame with the result of analysis sentiment
# save classify and text to data frame
result = data.frame(text = tweet_text, emotion = emotion, polarity = polarity, stringsAsFactors = FALSE)
#sort data frame
result = within(result, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing = TRUE))))

#make term-document matrix
dtm <- TermDocumentMatrix(tweet_text)
matrix <- as.matrix(dtm)
sort_by_freq <- sort(rowSums(matrix),decreasing = TRUE)
word_cloud <- data.frame(word = names(sort_by_freq), freq=sort_by_freq)  


## the last phase, visualize the result
#plot by emotions
output$emotionPlot <- renderPlot({
  ggplot(result, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    labs(x="emotion categories", y="number of tweets") +
    ggtitle("Sentiment Analysis of Spider-Man: Far From Home by emotion")    
})


#plot by polarity
ggplot(result, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette = "RdGy") +
  ggtitle("sentiment analysis of spider-man: far from home by polarity") +
  labs(x="polarity",y="number of tweets")

#generate word cloud
set.seed(1234)
wordcloud(words = word_cloud$word, freq = word_cloud$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))



output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white')
})