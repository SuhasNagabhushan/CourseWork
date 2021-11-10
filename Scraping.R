if(!"pacman" %in% installed.packages()[,"Package"])install.packages("pacman")

pacman::p_load(rvest, dplyr, tidyr, stringr, DT, RCurl, XML, purrr)

prod_code<- "B005BPZCUC"

url <- paste0("https://www.amazon.com/Hutzler-354RD-Pepper-Saver-Red/dp/", prod_code)
doc <- read_html(url)
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n","",.) %>% trimws()

scrape_amazon <- function(url, throttle = 0){
  if(!"pacman" %in% installed.packages()[,"Package"])install.packages("pacman")
  pacman::p_load(rvest, dplyr, tidyr, stringr, DT, RCurl, XML, purrr)
  
  sec = 0
  if(throttle < 0) warning ("throttle was less than 0: set to 0")
  if(throttle > 0) sec=max(0, throttle + runif(1,-1,1))
  doc<- read_html(url)
  title <- doc%>%html_nodes("#cm_cr-review_list .a-color-base") %>% html_text() %>% gsub("\n","", .) %>% trimws()
  author <- doc%>%html_nodes("#cm_cr-review_list .a-profile-name") %>% html_text()
  date <- doc%>%html_nodes("#cm_cr-review_list .review-date") %>% html_text() %>% gsub(".*on","",.)
  review_format <- doc  %>% html_nodes(".review-format-strip") %>% html_text()
  stars <- doc%>%html_nodes("#cm_cr-reviews_list .review-rating") %>% html_text() %>% str_extract("\\d") %>% as.numeric()
  #comments <- doc%>%html_nodes("#cm_cr-review_list .review-text")%>% html_text() %>%gsub("\n","",.) %>% trimws()
  df <- data.frame(author, title, date, stars, review_format, stringsAsFactors = F)
    return(df)
}
urltesting <- "http://www.amazon.com/product-reviews/B005BPZCUC/?pageNumber=1"
reviewtesting <- scrape_amazon(urltesting)
str(reviewtesting)

pages <- 90
reviews_all <- NULL
for(page_num in 1:pages){
  url<-paste0("https://www.amazon.com/product-reviews/", prod_code,"/?pageNumber=",page_num)
  reviews <- scrape_amazon(url, throttle=3)
  reviews_all <- rbind(reviews_all, cbind(prod,reviews))
}

str(reviews_all)
write.csv(reviews_all,file="B07VGRJDFY_Reviews.csv")
