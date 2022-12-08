#Sentiment analysis project that extracts data from Twitter to get an idea of whether the general sentiment of a topic is positive, negative or neutral

# #INSTALLS NECESSARY PACKAGES
install.packages(c('hms','lubridate','tidytext','tm','wordcloud','igraph'));
install.packages(c('glue','networkD3','rtweet','plyr','stringr','ggplot2'));
install.packages(c('ggeasy','plotly','dplyr','magrittr','tidyverse','emoji','sjmisc'));


#Run each library to make sure they're loaded into R 
library(hms);library(lubridate);library(tidytext);library(tm);library(wordcloud);library(igraph);library(glue);library(networkD3);
library(rtweet);library(plyr);library(stringr);library(ggplot2);library(ggeasy);library(plotly);library(dplyr);library(magrittr);
library(tidyverse);library(emoji);library(sjmisc)

## IMPORTANT TO SET WORKING DIRECTORY FOR WHEN THE PROGRAM TRIES TO WRITE OR IMPORT DATA TO/FROM THE DIRECTORY
getwd()
setwd("~/ECO365 Final project")
getwd()
#########################################################################################################################################################

#FUNCTIONS

#Function to clean tweets and strips them to a dataframe with a single word and its count in each row
cleaned.text = function(text){
  
  words <- tweets %>%
    mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
           text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
           text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
    unnest_tokens(word, text, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]"),
           !str_detect(word, "^#"),         
           !str_detect(word, "@\\S+")) %>%
       count(word, sort = TRUE)
  
return(words)
}

#Function algorithm that takes tweets,removes any unnecessary letters, links and numbers, separates each word out and assigns them sentiment values
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we are giving vector of sentences as input. 
  # plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub() function:
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Call garbage collector to help system memory
gc()
#########################################################################################################################################################
#########################################################################################################################################################

#VARIABLE FOR SEARCH TERM, recommended use is '# ' or string literal " "
trends <- get_trends("United States")

#extracts the trending tweet with the highest volume
maxVol <- trends%>%
  pull(tweet_volume) %>% 
  max(.,na.rm = TRUE)
#extracts the Trending phrase or hashtag and stores into a variable
Lookup <- trends %>% 
  filter(tweet_volume == maxVol) %>% 
  pull(trend)

#Extract up to 18000 tweets, ****NOTE LIMITATION IS 18,000 TWEETS EVERY 15 MINUTES AT CURRENT VERSION
tweets <- search_tweets(Lookup, n = 9000,include_rts = FALSE, type = "recent", lang = "en",fileEncoding = "UTF-8")

#pulls more tweets on same look up from prior to last tweet pulled from previous call USEFUL IF PREVIOUS CALL EXCEEDED THE LIMITATION
tweets2 <- search_tweets(Lookup, n = 6000,include_rts = FALSE, type = "recent", max_id = tweets, lang = "en", fileEncoding = "UTF-8", retryonratelimit = TRUE)

#Joins the three lists together
tweets_join <- full_join(tweets,tweets2)

#creates a file in working repository for easy to access storage
write_csv(tweets_join, file = "file.csv")

#Cleans text to single word and counts frequency
strippedTweets <-cleaned.text(tweets_join)


#Call garbage collector to help system memory
gc()
#########################################################################################################################################################
#########################################################################################################################################################

 #Creating a time-series graph that shows tweets over time based on creation date of tweets extracted

  #turning list into Data frame
  tweets.df <- as.data.frame(tweets_join)
  
  #adding a new column to round the time of creation to the nearest hour for easier aggregation
  tweets.df %<>%
    mutate(created = created_at %>% str_remove_all(pattern = '\\+0000') %>% 
             parse_date_time(orders = '%y-%m-%d %H%M%S'))
  
  tweets.df%<>%
        mutate(Created_At_Round = created %>% round(units = 'hours') %>% as.POSIXct())
  
  #finding the earliest post's creation time for the start of the chart
  tweets.df %>% 
    pull(created) %>% min()
  
  #finding the most recent creation time for the end of the chart
  tweets.df %>% 
    pull(created) %>% max()
  
  #Creating an object to store the linegraph and its variables in 
  lineGraphed <- tweets.df %>%
    count(Created_At_Round) %>%
    ggplot(aes(x = Created_At_Round, y = n)) +
    theme_light() + 
    geom_line() +
    labs(x= "Date", y = NULL)
   # geom_smooth(method = lm, se = FALSE, color = "gray", size = 1)
  #transforms ggplot line graph into interactive line graph
  ggplotly(lineGraphed, tooltip = "all", dynamicTicks = TRUE)%>%
    #Since the layout() function only has a title (and not subtitle) argument, we have to use some HTML to format our text to get it just right.
    layout(title = paste(
      '<b> </b> ',paste("Tweets Per Hour", paste("for Keyword", Lookup)),
      '<br><sup>',
      paste0(format(min(tweets.df$created_at), "%d %B %Y"), " to ", format(max(tweets.df$created_at),"%d %B %Y")),
      '</i><br>',
      '</sup>'
    ),
  margin = list(t = 75)
  )
  
  #Call garbage collector to help system memory
  gc()
#########################################################################################################################################################
#########################################################################################################################################################  
 
  
  #Intakes lexicon of positive and negative words for comparison in following code lines
  
   #imports positive and negative words 
  positive = scan('posSentiment.txt', what = 'character', comment.char = ';')
  negative = scan('negSentiment.txt', what = 'character', comment.char = ';')
  # add your list of words below as you wish if missing in above read lists
  pos.words = c(positive,'upgrade','Congrats','prizes','prize','thanks','thnx',
                'Grt','gr8','plz','trending','recovering','brainstorm','leader', 'congrats', 'congratulations')
  neg.words = c(negative,'wtf','epicfail','Fight','fighting', 'arrest', 'shit', 'corruption','corrupt', 'bullshit', 'conspiracy' )
  
  
#########################################################################################################################################################  
#########################################################################################################################################################  
  
  
  #variable that holds the column 'full_text' (we dont technically need this if we pass tweet.df$full_text through the function)
  test <- tweets.df$full_text
  
  #new variable to determine the sentiment score based on how many positive or negative words are in the text
  analysis <- score.sentiment(test, pos.words, neg.words)
  
  # sentiment score frequency table displays data in a table
  table(analysis$score)

  #Barchart to display frequency by sentiment
  neutral <- length(which(analysis$score == 0))
  positive <- length(which(analysis$score > 0))
  negative <- length(which(analysis$score < 0))
  Sentiment <- c("Negative","Neutral","Positive")
  Count <- c(negative,neutral,positive)
  output <- data.frame(Sentiment,Count)
  output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
  sentimentFreq <- ggplot(output, aes(x=Sentiment,y=Count))+
    geom_bar(stat = "identity", aes(fill = Sentiment))+
    ggtitle("Barchart of Sentiment type of Extracted tweets")
  ggplotly(sentimentFreq,tooltip = "all", dynamicTicks = TRUE)
  
  #Call garbage collector to help system memory
  gc()
#########################################################################################################################################################  
#########################################################################################################################################################    
  
  #Sets up color for sentiment
  play<-analysis
  play$Color <- replace(play$Color, play$score <0, Sentiment[1] )
  play$Color <- replace(play$Color, play$score >0, Sentiment[3] )
  play$Color <- replace(play$Color, play$score==0, Sentiment[2] )
  
  #histogram plot into object
  practice <- play %>%
    ggplot(aes(x = score, fill = Color)) + 
    geom_histogram( binwidth = 1)+ 
    scale_x_continuous()+
    ylab("Frequency") + 
    xlab("Sentiment score") +
    ggtitle("Distribution of Sentiment scores") +
    ggeasy::easy_center_title()
  #create interactive plot using histogram object
  ggplotly(practice, tooltip = "all", dynamicTicks = TRUE)
  
  
  #Call garbage collector to help system memory
  gc()
#########################################################################################################################################################
#########################################################################################################################################################    

  
  #Creates a word cloud of 100 words sized by frequency
  #adds positive and negative logical columns to help identify words
  
 strippedTweets$PosSentiment <- ifelse(match(strippedTweets$word, pos.words, nomatch = NA),"Blue",F)
 strippedTweets$NegSentiment <- ifelse(match(strippedTweets$word, neg.words, nomatch = NA),"Red",F)
 strippedTweets$NeuSentiment <- ifelse((is.na(strippedTweets$PosSentiment) & is.na(strippedTweets$NegSentiment)),"Green" ,NA)
  
 strippedTweets <-  strippedTweets%>%
   mutate(Color = coalesce(PosSentiment, NegSentiment, NeuSentiment))%>%
   select(-PosSentiment, -NegSentiment,-NeuSentiment)
  
  words<- strippedTweets %>% 
    with(wordcloud(word, n, scale = c(2.2,.8), random.order = F, max.words = 200, color =brewer.pal(8, "Dark2"), random.color = TRUE))
  
  #Call garbage collector to help system memory
  gc()
#########################################################################################################################################################  
#########################################################################################################################################################

  
  #extracts emojis and creates a frequency chart for the top 15
emoji<-  tweets %>%
    mutate(emoji = emoji_extract_all(text)) %>%
    unnest(cols = c(emoji)) %>%
    count(emoji, sort = TRUE) %>%
    top_n(15)
  
  emo <-ggplot(emoji, aes(x=reorder(emoji, n), y=n)) + 
    geom_bar(stat="identity") +
    xlab("Terms") + 
    ylab("Count") + 
    theme_dark() +
    coord_flip() +
    theme(axis.text=element_text(size=10)) +
    ggtitle('Most common Emoji frequency plot') + geom_text(aes(label = n), colour = "white")+
    ggeasy::easy_center_title() 
  
  ggplotly(emo, tooltip = "all")

  #Call garbage collector to help system memory
  gc()
#########################################################################################################################################################  
#########################################################################################################################################################
 
  #Creates a horizontal frequency chart of top twenty words
  
  #adds positive and negative logical value columns to help identify word sentiment (checks for exact match against a pre-loaded lexicon file)
  strippedTweets$PosSentiment <- ifelse(match(strippedTweets$word, pos.words, nomatch = FALSE),T,F)
  strippedTweets$NegSentiment <- ifelse(match(strippedTweets$word, neg.words, nomatch = FALSE),T,F)
  
  #Grabs top 10 positive words from tweets
  PosTweets <- strippedTweets%>%
    filter(PosSentiment == TRUE)%>%
    top_n(10,n)
  
  #Grabs top 10 negative words from tweets
  NegTweets <- strippedTweets%>%
    filter(NegSentiment == TRUE)%>%
    top_n(10,n)%>%
    #turns count values negative so the barchart has inverted variables 
    mutate(n = n*-1)
  
  #join the two tables for top twenty positive and negative tweets
  topTwenty <- full_join(PosTweets, NegTweets)
  
  #add a column which specifices color to positive and negative values (Sentiment vector assigns color to logical sentiment value)
  npgraph <- topTwenty%>%
    mutate(Color = ifelse((topTwenty$n > 0), Sentiment[3], Sentiment[1]))
  
  #Builds sorted vertical bar chart (currently has a positive and negative y axis)
  PosNeg<- ggplot(npgraph, aes(x=reorder(word, n), y=n, fill = Color) ) +  
    geom_bar(stat = "identity") +
    #this turns the labels on the y axis by adding the abs function to labels (however the bars label is still negative)
    scale_y_continuous(labels = abs)+
    xlab("Terms") + 
    ylab("Count") + 
    coord_flip() +
    theme_dark() +
    ggtitle('Top Ten Positive and Negative Words') + 
    #The bars change to positive because the abs function makes all values of n positive
    geom_text(aes(label = abs(n)), colour = "white") +
    ggeasy::easy_center_title()
  
  #creates an interactive chart with ggplot if you pass ggplot object as a parameter
  ggplotly(PosNeg)
  
  #Call garbage collector to help system memory
  gc()
#########################################################################################################################################################
#########################################################################################################################################################
    
    
#Creates chart to display Most popular hashtags associated with the original search term 
  pullHashtag <- tweets %>% 
    unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
    filter(str_detect(hashtag, "^#"),
           hashtag != paste("#",Lookup)) %>%
    count(hashtag, sort = TRUE) %>%
    top_n(11)
  
  hashtag<- ggplot(pullHashtag, aes(x=reorder(hashtag, n), y=n)) + 
    geom_bar(stat="identity") +
    xlab("Terms") + 
    ylab("Count") + 
    coord_flip() +
    theme_dark() +
    theme(axis.text=element_text(size=10)) +
    ggtitle('Most common hashtag frequency plot') +
    ggeasy::easy_center_title() + geom_text(aes(label = n), colour = "white")
  ggplotly(hashtag, tooltip = "all")
  
  #Call garbage collector to help system memory
  gc()
  
#########################################################################################################################################################
#########################################################################################################################################################
  
  