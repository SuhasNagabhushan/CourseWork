library(rtweet)
library(ggplot2)
library(syuzhet)
library(tidytext)
library(wordcloud)
library(tm)

Companytdata <- get_timeline("@nytimes", n=1000)
write_as_csv(Companytdata,"Companytdata.csv")
#As Shown in the class, the graph is plotted.
Companytdata %>%
  ts_plot(by="mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of New York Times Twitter statuses",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


Companytdata$text <-  gsub("https\\S*", "", Companytdata$text)
Companytdata$text <-  gsub("@\\S*", "", Companytdata$text) 
Companytdata$text  <-  gsub("amp", "", Companytdata$text) 
Companytdata$text  <-  gsub("[\r\n]", "", Companytdata$text)
Companytdata$text  <-  gsub("[[:punct:]]", "", Companytdata$text)

finaltweet <- Companytdata %>% select(text) %>% unnest_tokens(word, text)
finaltweet <- finaltweet %>% anti_join(stop_words)

# bar chart - most frequent words found in the tweets
finaltweet %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Number of times the word is used",
       x = "Unique words",
       title = "Most frequent words found in the tweets of The New York Times",
       subtitle = "The words removed from the list")

# Keeping retweets
times_rts <- Companytdata[Companytdata$is_retweet==TRUE,]
# Keeping replies
times_rply<- subset(Companytdata, !is.na(Companytdata$reply_to_status_id))
datatweet <- data.frame(
  category=c("Retweets", "Replies"),
  count=c(113, 143)
)

# Adding columns 
datatweet$fraction = datatweet$count / sum(datatweet$count)
datatweet$percentage = datatweet$count / sum(datatweet$count) * 100
datatweet$ymax = cumsum(datatweet$fraction)
datatweet$ymin = c(0, head(datatweet$ymax, n=-1))
# legend creation
Type_of_Tweet <- paste(datatweet$category, datatweet$percentage, "%")
ggplot(datatweet, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# Converting tweets to ASCII to remove characters
finaltweet <- iconv(finaltweet, from="UTF-8", to="ASCII", sub="")
# removing retweets
finaltweet <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",finaltweet)
# removing mentions
finaltweet <-gsub("@\\w+","",finaltweet)
ew_sentiment<-get_nrc_sentiment((finaltweet))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

set.seed(1234)
wordcloud(Companytdata$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

nytwt <- Companytdata %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
nytwt <- subset(nytwt, count > 5)
datatwt <- data.frame(
  category=nytwt$source,
  count=nytwt$count
)
datatwt$fraction = datatwt$count / sum(datatwt$count)
datatwt$percentage = datatwt$count / sum(datatwt$count) * 100
datatwt$ymax = cumsum(datatwt$fraction)
datatwt$ymin = c(0, head(datatwt$ymax, n=-1))
Source <- paste(datatwt$category, datatwt$percentage, "%")
ggplot(datatwt, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

Companytweet <- read_csv('Companytdata.csv')

senti <- get_sentiment(Companytweet$text,method="afinn")

plot(senti,type="l")

senti2 <- get_sentiment(Companytweet$text,method="syuzhet")
plot(senti2,type="l")

plot(Companytweet$created_at,senti2)

senti3 <- get_sentiment(Companytweet$text,method="nrc")
plot(senti3,type="l")
plot(Companytweet$created_at,senti3)

nrc_data <- get_nrc_sentiment(Companytweet$text)
barplot(
  sort(colSums(prop.table(nrc_data))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)


fulldata <- cbind(Companytweet,senti,senti2,senti3,nrc_data)
write_csv(Companytweet,'fullcompany.csv')
