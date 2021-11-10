library(dplyr)
library(reshape2)
library(ggplot2)
orders <-read.csv (file.choose()) # load the file cacclvdataset.csv 
cac <-read.csv (file.choose()) # load the file cac.csv
gr.margin <-read.csv(file.choose()) # load the file grossmargin.csv
orders$orderdate <-as.Date(orders$orderdate, format = "%m/%d/%Y")
max(orders$orderdate)
today <-as.Date('2012-04-11', format='%Y-%m-%d')
orders <-merge(orders, gr.margin, by='product') #merge orders & grossmargin
clv <-orders %>% group_by(clientId) %>% summarise(clv=sum(grossmarg)) %>% ungroup()
orders <-dcast(orders, orderId + clientId + gender + orderdate ~ product, value.var='product', fun.aggregate=length)
orders <-orders %>%group_by(clientId) %>%mutate(frequency=n(),recency=as.numeric(today-orderdate)) %>% filter(orderdate==max(orderdate)) %>%filter(orderId==max(orderId)) %>%ungroup()
orders.segm <-orders %>%mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',ifelse(between(frequency, 2, 2), '2',ifelse(between(frequency, 3, 3), '3',ifelse(between(frequency, 4, 4), '4',ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%mutate(segm.rec=ifelse(between(recency, 0, 6), '0-6 days',ifelse(between(recency, 7, 13), '7-13 days',ifelse(between(recency, 14, 19), '14-19 days',ifelse(between(recency, 20, 45), '20-45 days',ifelse(between(recency, 46, 80), '46-80 days', '>80 days'))))))
# creating last cart feature, ignore errors here if you have missing SKU
mutate(cart=paste(ifelse(a!=0, 'a', ''),ifelse(b!=0, 'b', ''),ifelse(c!=0, 'c', ''), sep='')) %>%arrange(clientId)
# defining order of boundaries
orders.segm$segm.freq <-factor(orders.segm$segm.freq, levels=c('>5','5','4', '3', '2', '1'))
orders.segm$segm.rec <-factor(orders.segm$segm.rec, levels=c('>80 days', '46-80 days', '20-45 days', '14-19 days', '7-13 days', '0-6 days'))
orders.segm <-merge(orders.segm, cac, by='clientId')
orders.segm <-merge(orders.segm, clv, by='clientId')
lcg.clv <-orders.segm %>%group_by(segm.rec, segm.freq) %>%summarise(quantity=n(),cac=sum(cac),clv=sum(clv)) %>%ungroup() %>%mutate(cac1=round(cac/quantity, 2),clv1=round(clv/quantity, 2))
lcg.clv <-melt(lcg.clv, id.vars=c('segm.rec', 'segm.freq', 'quantity')) 
ggplot(lcg.clv[lcg.clv$variable %in% c('clv', 'cac'), ],aes(x=variable, y=value, fill=variable)) +theme_bw() +theme(panel.grid = element_blank())+geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +geom_text(aes(y=value, label=value), size=4) +facet_grid(segm.freq ~ segm.rec) +ggtitle("LifeCycle Grids -CLV vs CAC (total)")
ggplot(lcg.clv[lcg.clv$variable %in% c('clv1', 'cac1'), ],aes(x=variable, y=value, fill=variable)) +theme_bw() +theme(panel.grid = element_blank())+geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +geom_text(aes(y=value, label=value), size=4) +facet_grid(segm.freq ~ segm.rec) +ggtitle("LifeCycle Grids -CLV vs CAC (average)")

