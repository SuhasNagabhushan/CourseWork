pacman::p_load(dplyr, tidyr, stringr, data.table, sentimentr, ggplot2)

reviews_all = read.csv(file.choose(), stringsAsFactors = F)

reviews_df <- reviews_all %>% mutate(id=row_number())
str(reviews_all)

nrow(lexicon::hash_sentiment_jockers_rinker)
replace_in_lexicon <-  tribble(~x,~y,"switch",0,"nintendo",0,"red",0,"amazeballs",.75)
review_lexicon <-lexicon::hash_sentiment_jockers_rinker %>% filter(!x %in% replace_in_lexicon$x) %>% setDT() %>% setkey("x")
sent_df <- reviews_df%>% get_sentences() %>% sentiment_by (by= c('id','author','date','stars','review_format'), polarity_dt = review_lexicon)


ggplot(sent_df, aes(x = stars, y = ave_sentiment, color = factor(stars), group = stars)) +geom_boxplot() +geom_hline(yintercept=0, linetype="dashed", color = "red") +geom_text(aes(5.2, -0.05, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red") +guides(color = guide_legend(title="Star Rating")) +ylab("Average Sentiment") +xlab("Review Star Rating") +ggtitle("Sentiment of Amazon Reviews, by Star Rating") 
