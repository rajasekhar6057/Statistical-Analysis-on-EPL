###################################################################################
#Step-1 
#installing required packages
install.packages(c("plyr", "tidyverse", "cluster", "ggplot2", "plotly", "xlsx", "dplyr", "foreign", "nnet", "twitteR"))
###################################################################################

###################################################################################
#Step-2
#loading libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(cluster)
library(ggplot2)
library(plotly)
library(xlsx)
library(foreign)
library(nnet)
library(twitteR)
###################################################################################

###################################################################################
#Step-3
#reading in dataset
referee <- read.csv("data/referee_updated.csv", stringsAsFactors = FALSE)
match <- read.xlsx("data/ProjectMatchleveldata_R.xlsx", sheetIndex = 1)
league <- read.xlsx("data/ProjectLeagueleveldata.xlsx", sheetIndex = 1)
###################################################################################

###################################################################################
#Questions
#Q1.	Factors predicting final result of the match Multi nominal Logistic Regression
match$FTR <- relevel(match$FTR, ref="H")
test <- multinom(FTR ~ HTR + HS + AS + HC + AC + HST + AST + HF + AF + HY + AY + HR + AR, family=binomial(link="logit"), data = match)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1))*2
p
p.df <- data.frame(p)
View(p)

###################################################################################
#Q2.	Clustering the referees into Harsh referee and 
#Lenient referee based on the Fouls committed by the teams and Booking Points 
#given by the referee in the matches over the seasons K means Cluster analysis
#k-means clustering
k2 <- kmeans(referee[,2:5], centers = 2, nstart = 25)
k2

result <- cbind(referee$Referee, data.frame(k2$cluster))
colnames(result) <- c("referee", "cluster")
View(result)

viz <- result %>%
  group_by(cluster) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(cluster = reorder(cluster,Count))

plot_ly(viz, x = ~cluster, y = ~Count, type = 'bar',
        marker = list(color = 'rgb(232,171,102)',width = 1.5)) %>%
  layout(title = "Lenient Vs Harsh Referee",
         xaxis = list(title = "Cluster"),
         yaxis = list(title = "Count"))

###################################################################################
#Q3.	Does the type of Referee (clustered in the above task) play a role in 
#predicting the Final match result? Multi nominal Logistic Regression
referee <- cbind(referee, result$cluster)
referee.test <- multinom(Referee ~ away_bp + home_bp + avg_af + avg_hf,data = referee)
summary(referee.test)

a <- summary(referee.test)$coefficients/summary(referee.test)$standard.errors
a

b <- (1 - pnorm(abs(a), 0, 1))*2
b
b.df <- data.frame(b)
View(b)

###################################################################################
#Q4.	Impact of Bet365 on match result Table Analysis and Linear Regression
#Table Analysis
b365 <- data.frame(table(match$B365RESULT))
ftr <- data.frame(table(match$FTR))

tbl = table(match$B365RESULT, match$FTR)
tbl

plot(tbl)
#table analysis
chisq.test(tbl)


#Linear Regression
linearMod <- lm(FTGD ~ HTGD + B365RESULT + HTGD*B365RESULT, data = match)
print(linearMod)
summary(linearMod)
###Betting odds are impacting the match result.

###################################################################################
#Q5.	Does a team that spends more money on its players end up qualifying for 
#Champions League i.e., being in the top 4 teams list on the league table? 
#Correlation and Bar Graph

#viewing the column names and changing any wierd column names
names(league)
colnames(league)[17] <- "Spendings"
#correlation
c <- cor(league$Position, league$Spendings)
c
###################################################################################

#6.	Sentiment Analysis on Top 3 teams of EPL from Twitter official pages

#grabbing data from Twitter
#please setup a twitter app and get authentication keys
consumerKey <- "******"
consumerSecret <- "************"
accessToken <- 	"****-****"
accessSecret <- "****"

setup_twitter_oauth(consumer_key = consumerKey, 
                    consumer_secret = consumerSecret, 
                    access_token = accessToken, 
                    access_secret = accessSecret)

#SearchTwitter
#n=5000 gives us 5000 tweets
ManU.Data <- searchTwitter("Manchester United", n=5000,  lang='en')
ManCity.Data <- searchTwitter("Manchester City", n=5000,  lang='en')
Liverpool.Data <- searchTwitter("Liverpool", n=5000,  lang='en')


#Conversion to data frame
ManU.Data.df <- twListToDF(ManU.Data)
ManCity.Data.df <- twListToDF(ManCity.Data)
Liverpool.Data.df <- twListToDF(Liverpool.Data)

#We can either search from twitter or use an existing data file
ManU.Data.df <- read_csv('dataset/Manchester_United.csv')
ManCity.Data.df <- read_csv('dataset/Manchester_City.csv')
Liverpool.Data.df <- read_csv('dataset/Liverpool.csv')

#GetText from the data frame for sentiment analysis
ManU.Data.df <- as.data.frame(ManU.Data.df$text)
colnames(ManU.Data.df)[1]<-"text"

ManCity.Data.df <- as.data.frame(ManCity.Data.df$text)
colnames(ManCity.Data.df)[1]<-"text"

Liverpool.Data.df <- as.data.frame(Liverpool.Data.df$text)
colnames(Liverpool.Data.df)[1]<-"text"

#positive and negative words for calculating sentiment
pos <- readLines("data/positive_words.txt")
neg <- readLines("data/negative_words.txt")


#sentiment analysis function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    #removing special characters
                    sentence <- gsub("@\\w+", "", sentence)
                    sentence <- gsub("#\\w+", '', sentence)
                    sentence <- gsub("RT\\w+", "", sentence)
                    sentence <- gsub("http.*", "", sentence)
                    # remove punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence <- gsub('\\d+', '', sentence)
                    
                    #convert to lower
                    sentence <- tolower(sentence)
                    
                    # split sentence into words with str_split (stringr package)
                    word.list <- str_split(sentence, "\\s+")
                    words <- unlist(word.list)
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos)
                    neg.matches <- match(words, neg)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}

#performing sentiment analysis on 3 team's tweets
#converting text to utf8
ManU_sentiment  <- sapply(ManU.Data.df ,function(row) iconv(row, "latin1", "ASCII", sub=""))
ManCity_sentiment  <- sapply(ManCity.Data.df ,function(row) iconv(row, "latin1", "ASCII", sub=""))
Liverpool_sentiment  <- sapply(Liverpool.Data.df ,function(row) iconv(row, "latin1", "ASCII", sub=""))

#sentiment score
ManU.sentiment_score <- score.sentiment(ManU_sentiment, pos, neg, .progress='text')
ManCity.sentiment_score <- score.sentiment(ManCity_sentiment, pos, neg, .progress='text')
Liverpool.sentiment_score <- score.sentiment(Liverpool_sentiment, pos, neg, .progress='text')

#summary of sentiment score
summary(ManU.sentiment_score)
summary(ManCity.sentiment_score)
summary(Liverpool.sentiment_score)

#Convert sentiment scores from numeric to character to enable the gsub function 
ManU.sentiment_score$sentiment <- as.character(ManU.sentiment_score$score)
ManCity.sentiment_score$sentiment <- as.character(ManCity.sentiment_score$score)
Liverpool.sentiment_score$sentiment <- as.character(Liverpool.sentiment_score$score)

#After looking at the summary(ManU.sentiment_Score$sentiment) decide on a threshold for the sentiment labels
ManU.sentiment_score$sentiment <- gsub("^0$", "Neutral", ManU.sentiment_score$sentiment)
ManU.sentiment_score$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", ManU.sentiment_score$sentiment)
ManU.sentiment_score$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$", "Very Positive", ManU.sentiment_score$sentiment)
ManU.sentiment_score$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", ManU.sentiment_score$sentiment)
ManU.sentiment_score$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", ManU.sentiment_score$sentiment)

#After looking at the summary(ManCity.sentiment_Score$sentiment) decide on a threshold for the sentiment labels
ManCity.sentiment_score$sentiment <- gsub("^0$", "Neutral", ManCity.sentiment_score$sentiment)
ManCity.sentiment_score$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", ManCity.sentiment_score$sentiment)
ManCity.sentiment_score$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$", "Very Positive", ManCity.sentiment_score$sentiment)
ManCity.sentiment_score$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", ManCity.sentiment_score$sentiment)
ManCity.sentiment_score$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", ManCity.sentiment_score$sentiment)

#After looking at the summary(Liverpool.sentiment_Score$sentiment) decide on a threshold for the sentiment labels
Liverpool.sentiment_score$sentiment <- gsub("^0$", "Neutral", Liverpool.sentiment_score$sentiment)
Liverpool.sentiment_score$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", Liverpool.sentiment_score$sentiment)
Liverpool.sentiment_score$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$", "Very Positive", Liverpool.sentiment_score$sentiment)
Liverpool.sentiment_score$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", Liverpool.sentiment_score$sentiment)
Liverpool.sentiment_score$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", Liverpool.sentiment_score$sentiment)

View(ManU.sentiment_score)

#Convert score_chr to factor for visualizations
ManU.sentiment_score$sentiment <- as.factor(ManU.sentiment_score$sentiment)
ManCity.sentiment_score$sentiment <- as.factor(ManCity.sentiment_score$sentiment)
Liverpool.sentiment_score$sentiment <- as.factor(Liverpool.sentiment_score$sentiment)

#plot to show number of negative, positive and neutral comments of Manchester United Fans
plot1 <- ggplot(ManU.sentiment_score, aes(x=sentiment))+geom_bar()
plot1

#plot to show number of negative, positive and neutral comments of Manchester United Fans
plot2 <- ggplot(ManCity.sentiment_score, aes(x=sentiment))+geom_bar()
plot2

#plot to show number of negative, positive and neutral comments of Manchester United Fans
plot3 <- ggplot(Liverpool.sentiment_score, aes(x=sentiment))+geom_bar()
plot3
###################################################################################
#END OF ANALYSIS
###################################################################################