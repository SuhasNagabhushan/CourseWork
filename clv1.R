library(dplyr)
library (ggplot2)
clvdata <-read.csv(file.choose()) # load the clvdataset.csv
RFM<-group_by (clvdata, CustomerID) %>% summarise( Monetary= sum(Revenue), Frequency = n(), Recency = 1/(as.numeric (difftime(format(Sys.time(), "%Y-%m-%d"), last(Date),units = c("days")) )))
normalize <-function(x) {return (((x -min(x)) / (max(x) - min(x))) * 5)}
dfNorm <-as.data.frame(lapply(RFM[2:4], normalize))
wss <-(nrow(dfNorm)-1)*sum(apply(dfNorm,2,var))
for (i in 2:15) wss[i] <-sum(kmeans(dfNorm,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
mydataCluster <-kmeans(dfNorm, 5, nstart = 20)
mydataCluster$cluster<-as.factor(mydataCluster$cluster)
ggplot(dfNorm, aes(Recency, Monetary, color = mydataCluster$cluster)) + geom_point()
                                 