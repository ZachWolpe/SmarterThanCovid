

#__________________________________ Required libraries ______________________________________
library(rtweet)
library(dplyr)
library(tidytext)
library(tidyr)
library(NLP)
library(syuzhet)
library(tm)
library(SnowballC)
library(stringi)
library(syuzhet)
library(ggplot2)
library(wordcloud)
library(stringr)
library(reshape2)

#__________________________________ Twitter Authentication ______________________________________
# sign up for twitter api, we used a free account.
get_token()
api_key <- "your api key"
api_secret_key <- "your secret key"

## authenticate
token <- create_token(
  app = "QuantaLabsSentiment",
  consumer_key = api_key,
  consumer_secret = api_secret_key)



# get local & global trends
sa_trends <- get_trends(woeid='South Africa', exclude=FALSE, token=token, parse=TRUE)


# get tweets per trend
tweets_per_trend = c()

for (i in (1:dim(sa_trends)[1])) {
  tweets = search_tweets(sa_trends$trend[i], token=token, n=150)
  tweets$trend = sa_trends$trend[i]
  tweets$tweet_volume = sa_trends$tweet_volume[i]
  tweets_per_trend <- rbind(tweets_per_trend,tweets)
}


# download lexicon
nrc_lexicon = get_sentiments("nrc")
afinn_lexicon = get_sentiments("afinn")
bing_lexicon = get_sentiments("bing")

#_________________________________ Clean Text _________________________________
clean_tweets_text <- function(dataset, plot=T) {
  "Return: clean word tokens ranked by frequency of appearance"
  
  
  
  dataset <- dataset %>% unnest_tokens(word, text) %>% select(word)
  dataset <- dataset %>% anti_join(stop_words, by=c("word", "word"))
  dataset <- dataset %>% count(word, sort=TRUE) 
  dataset <- dataset[dataset$word != 'https',]
  
  dataset <- dataset[is.na(as.numeric(dataset$word)),]
  dataset <- dataset %>% mutate(word=reorder(word,n))
  if (plot) {
    # Most Used Words
    plt <- dataset[5:15,] %>%
      ggplot(aes(word,n,fill=word)) +
      ggtitle("Most Common Words on Twitter SA") + 
      geom_col() +
      theme_minimal() +
      theme(legend.position = 'None',
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      coord_flip()
    
    print(plt)
  }
  
  return(dataset)
  
}
#_________________________________ Clean Text _________________________________



clean_tweets_text(tweets_per_trend)







### Bing: Polarized Sentiment Analysis
# Compute the sentiment of each word in a polarized fashion (_'positive'_ or _'negative'_)
# ________________________________________________ Clean Data ________________________________________________
clean = clean_tweets_text(tweets_per_trend, T)


# __________________________________________ compute_bing_sentiment __________________________________________
compute_bing_sentiment <- function(word_list, lexicon=bing_lexicon, show_plots=FALSE, word_size=30, printt=F) {
  "Compute the Corpus Sentiment for a given Lexicon"
  
  # get bing sentiment
  word_list <- word_list %>% inner_join(lexicon, by=c('word','word')) %>% select(word, n, sentiment) 
  
  
  # compute for visualization
  word_list$pole = ifelse(word_list$sentiment == 'positive', 1, -1)
  word_list$score = word_list$pole * word_list$n
  
  
  # visualize
  if (show_plots) {
    net_sent <- ggplot(word_list, aes(word, score, fill=pole)) + geom_col() + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = 'none')
    print(net_sent)
    
    plt <- pie(x=c(mean(word_list$sentiment=="negative"),mean(word_list$sentiment=="positive")),
               labels=c("negative",'positive'),
               col=c("darkblue", "lightblue"))
    print(plt)
    
    # Visualize Most Common Positive vs Negative Words
    plt <- word_list %>% group_by(sentiment) %>%
      top_n(n=10, wt=n) %>% mutate(word=reorder(word,n)) %>%
      ggplot(aes(reorder(word,n), n, fill=sentiment)) +
      geom_col(show.legend=FALSE) +
      facet_wrap(~sentiment, scales="free_y") +
      labs(y = "Contribution to Sentiment",
           x = NULL) +
      coord_flip()
    print(plt)
    
    
    # generate wordcloud by sentiment colours  
    group = c(word_list$sentiment)
    basecolours = c('darkgreen','darkred')
    colourlist = basecolours[match(group,unique(group))]
    
    plt <- wordcloud(words=word_list$word, freq=word_list$n, 
                     colors=colourlist, 
                     ordered.colors=T,
                     max.words=word_size)
    print(plt)
    
    # Structured sentiment word cloud
    plt <- word_list %>% acast(word ~ sentiment, value.var="n", fill=0) %>% 
      comparison.cloud(colors=c("darkred",'darkgreen'), 
                       max.words=word_size)
    print(plt)
    
  }
  
  if (printt) {
    print(paste("Proportion of POSITIVE sentiment words: ", mean(word_list$sentiment=="positive")))
    print(paste("Proportion of NEGATIVE sentiment words: ", mean(word_list$sentiment=="negative")))
    print(paste("Proportion of NEUTRAL sentiment words: ", mean(word_list$sentiment=="neutral")))
  }
  
  return(word_list)
}
bing_sent <- compute_bing_sentiment(clean, bing_lexicon, TRUE)
head(bing_sent)







### nrc: Diversified Sentiment Analysis
# Compute the sentiment of each word in a diversified fashion, sentiment words include:

# ______________________________________________________ Compute Diversified Sentiment ______________________________________________________
diversified_sentiment_analysis <- function(word_list, nrc_lexicon, show_plots=TRUE) {
  "Compute, plot & return the diversified sentiment"
  
  
  # compute diversified sentiment - compute the number of appearances of each sentiment type
  diversed_sentiment <- unique(word_list %>% inner_join(nrc_lexicon) %>% 
                                 group_by(sentiment) %>% mutate(sum_n = sum(n)) %>% select(sum_n))
  # compute proportion
  diversed_sentiment$proportion = diversed_sentiment$sum_n / sum(diversed_sentiment$sum_n)
  
  # compute ratio
  diversed_sentiment$ratio = diversed_sentiment$proportion / max(diversed_sentiment$proportion)
  
  
  # order nrc by 2 criteria: circular position (CLOCK)
  rank <- data.frame(sentiment=c('positive','joy','surprise',"sadness",'disgust','negative','anger','fear','anticipation','trust'),
                     position=c(0,1,2,3,4,5,6,7,8,9),
                     sent=c(1,1,0,-1,-1,-1,-1,-1,0,1)) 
  rank <- inner_join(diversed_sentiment, rank, by="sentiment")
  # order
  rank <- rank[order(rank$position),]
  
  if (show_plots==T){
    # col plot
    plt <- ggplot(rank, aes(x=reorder(sentiment, position), y=proportion, fill=sent)) + geom_col() + ggtitle("Diversified Sentiment")
    print(plt)
    
    # coordinate plot
    plt <- ggplot(rank, aes(x=reorder(sentiment, position), y=ratio, fill=(sent))) + 
      geom_bar(stat='identity', show.legend=F) +
      coord_polar() + 
      ggtitle("Sentiment of Tweets in SA")+ 
      xlab("") + ylab("") + 
      theme(legend.position = "None",
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour='#DCDCDC'),
            panel.grid.minor  = element_line(colour='#DCDCDC'))  
    print(plt) 
  }
  return(rank)
}
#  Call Function 
diversified_sentiment_analysis(clean, nrc_lexicon)






# net sentiment of tweets containing Covid
hashtag = 'covid'

dataset <- search_tweets(hashtag, n=1000, include_rts = FALSE, token=token)


clean_ = clean_tweets_text(dataset, T)
data_ = diversified_sentiment_analysis(clean_, nrc_lexicon)

gg <- ggplot(data=data_,aes(x=sentiment,y=sum_n)) + 
  geom_bar(aes(fill=sentiment),stat ="identity") + 
  xlab("Sentiments") + ylab("Scores") +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("Sentiment of Covid Tweets")
plot(gg)

  

# _____________________________________________________ Time Series _____________________________________________________

## search tweets mentioning WhenCoronaVirusIsOver South Africa
cov_over <- search_tweets("#WhenCoronaVirusIsOver", geocode='-30.55948,22.9375,1000mi', n=9000, include_rts = FALSE, token=token)

## search tweets mentioning #CoronaVirusSouthAfrica
covsa <- search_tweets("#CoronaVirusSouthAfrica", n=9000, include_rts=FALSE, token=token)

cov_over$query <- "#WhenCoronaVirusIsOver"
covsa$query <- "#CoronaVirusSouthAfrica"

df <- rbind(covsa, cov_over)


# ______________ Sentiment _____________

df$text_plain <- plain_tweets(df$text)
sa <- syuzhet::get_nrc_sentiment(df$text_plain)
df <- cbind(df, sa)

## create function for aggregating date-time vectors
round_time <- function(x, interval = 60) {
  ## round off to lowest value
  rounded <- floor(as.numeric(x) / interval) * interval
  ## center so value is interval mid-point
  rounded <- rounded + round(interval * .5, 0)
  ## return to date-time
  as.POSIXct(rounded, origin = "1970-01-01")
}

## use pipe (%>%) operator for linear syntax
long_emotion_ts <- df %>%
  ## select variables (columns) of interest
  dplyr::select(created_at, query, anger:positive) %>%
  ## convert created_at variable to desired interval
  ## here I chose 6 hour intervals (3 * 60 seconds * 60 mins = 3 hours)
  mutate(created_at = round_time(created_at, 3 * 60 * 60)) %>%
  ## transform data to long form
  tidyr::gather(sentiment, score, -created_at, -query) %>%
  ## group by time, query, and sentiment
  group_by(created_at, query, sentiment) %>%
  ## get mean for each grouping
  summarize(score = mean(score, na.rm = TRUE),
            n = n()) %>%
  ungroup()

long_emotion_ts

library(extrafont)
font_import()
loadfonts(device = "win")

# break scale for better vis
long_emotion_ts[long_emotion_ts$query=='#WhenCoronaVirusIsOver',]$score = 
  long_emotion_ts[long_emotion_ts$query=='#WhenCoronaVirusIsOver',]$score/4
  

long_emotion_ts %>%
  ggplot(aes(x = created_at, y = score, fill=query)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~ sentiment, scale = "free_y", nrow = 2) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        axis.text = element_text(size = 9),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Sentiment analysis of Twitter statuses over time",
       subtitle = "Tweets aggregated by hour on topics of #CoronaVirusSouthAfrica and #WhenCoronaVirusIsOver") +
  scale_x_datetime(date_breaks = "24 hours", date_labels = "%b %d")







