library(RSelenium)
library(xml2)
library(dplyr)
library(rvest)
library(httr)
library(sentimentr)
library(ggplot2)

#start up phantom js session
wdman::phantomjs()  #just need to run once to start RSelenium server
rD <- rsDriver(browser = 'phantomjs')
remDr <- rD$client
remDr$open()

baseUrl <- 'http://www.cnn.com'
searchTerm <- 'Puerto Rico Hurricane'
searchTime <- 'Past month'


#go to cnn front page
remDr$navigate(baseUrl)
remDr$screenshot(display = TRUE)

#click on search button
searchButton <- remDr$findElement(using = 'css', value = '.search-input__button')
searchButton$clickElement()
remDr$screenshot(display = TRUE)

#enter search text
searchField <- remDr$findElement(using = 'css', value = '.search-input__text')
searchField$sendKeysToElement(list(searchTerm))
remDr$screenshot(display = TRUE)

#submit search
submit <- remDr$findElement(using = 'css', value = '.search-input__button')
submit$clickElement()
remDr$screenshot(display = TRUE)

# click "Stories" for articles only
stories <- remDr$findElement(using = 'css', value = '.collectionList > ul:nth-child(1) > li:nth-child(2) > label:nth-child(2)')
stories$clickElement()
remDr$screenshot(display = TRUE)


########CNN only has by Date or by Relevance#####
#---select past week only--- 
#find the date range selector
##dateRange <- remDr$findElement(using = 'css', value = '.date-range-selector')
##dateRange$clickElement()
#extract the options
##opts <- dateRange$selectTag()
#find the option index number that matches what we want
##optNum <- which(opts$text == searchTime)
# select the correct date range option
##opts$elements[[optNum]]$clickElement()



#scrape the article metadata
articles <- remDr$findElements(using = 'css', value = '.cnn-search__result.cnn-search__result--article')
pb <- txtProgressBar(max = length(articles), style = 3)
articleList <- list()
for(i in 1:length(articles)){
  article <- read_html(articles[[i]]$getElementAttribute("outerHTML")[[1]]) 
  title <- article %>%
    html_node('.cnn-search__result-headline') %>%
    html_text()
  link <- article %>%
    html_node('a') %>%
    html_attr('href') %>%
    paste(baseUrl, ., sep = '')
  date <- article %>%
    html_node('.cnn-search__result-publish-date') %>%
    html_text() 
  date <- gsub("\n","", date)
  date <- gsub(" ","",date)
   
  dfTemp <- data.frame(title = title, link = link, date = date, stringsAsFactors = FALSE)
  articleList[[i]] <- dfTemp
  setTxtProgressBar(pb, i)
}
articleDF <- do.call('rbind', articleList)
#articleDF_all <- data.frame()  #do this once
articleDF_all <- rbind(articleDF_all, articleDF)

## Click Next Button and repeat metadata scrape
submit <- remDr$findElement(using = 'css', value = 'div.pagination-arrow:nth-child(3)')
submit$clickElement()
remDr$screenshot(display = TRUE)



# clean URL
#test <- articleDF_all
articleDF_all$link <- substring(articleDF_all$link, 19)



#scrape article text (using rvest - more reliable and faster)
library(httr)
articleDF_all$text <- NA
articleDF_all$author <- NA
articleDF_all$location <- NA
pb <- txtProgressBar(max = nrow(articleDF_all), style = 3)
for(i in 1:nrow(articleDF_all)){
  article <- GET(articleDF_all$link[i])
  
  articleDF_all$text[i] <- article %>%
    read_html() %>%
    html_node('.pg-rail-tall__body') %>%
    html_text()
  
  articleDF_all$author[i] <- article %>%
    read_html() %>%
    html_node('.metadata__byline__author') %>%
    html_text() %>%
    gsub('By ', '', .)
  
  articleDF_all$location[i] <- article %>%
    read_html() %>%
    html_node('.el-editorial-source') %>%
    html_text() %>%
    gsub('(.*?),.*', '\\1', .)
  
  setTxtProgressBar(pb, i)
}



#remove bad entries in location
articleDF$location[grep('[A-Z]+[a-z]+', articleDF$location)] <- NA


# filter out money.cnn.com articles (website formats are different)
articleDF_all <- filter(articleDF_all, text != "NA")



# sentiment metric
library(sentimentr)
sent <- sentiment_by(articleDF_all$text)
articleDF_all$sentiment <- sent$ave_sentiment * 100


#frequently mentioned words
library(qdap)
freq <- freq_terms(articleDF_all$text, top = 20, at.least = 4, stopwords = Top200Words) 



# plot sentiment over time (by day)
articleDF_all$date <- mdy(articleDF_all$date)

articleDF_all <- filter(articleDF_all, date > "2017-09-20")

daily_sent <- summarize(group_by(articleDF_all, date), count = n(), avgSent = mean(sentiment))

ggplot(daily_sent)+
  geom_bar(aes(x = date, y = avgSent), stat="identity") +
  #geom_line(aes(x = day, y = count))
  scale_x_date(date_breaks = "days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ylab("Average Sentiment Score") +
  theme(axis.title.x = element_blank())+
  ggtitle("Average Sentiment of CNN Articles \n About Hurricane Maria in Puerto Rico")


ggplot(articleDF_all, aes(x = date, y = sentiment))+
  geom_point()+
  geom_hline(yintercept = 0, size =1, color = "blue")+
  scale_x_date(date_breaks = "days")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ggtitle("Sentiment by Day of CNN Articles \n About Hurrican Maria in Puerto Rico")+
  theme(axis.title.x = element_blank())+
  ylab("Sentiment Score")

ggplot(daily_sent)+
  geom_bar(aes(x = date, y = count), stat="identity") +
  scale_x_date(date_breaks = "days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ylab("# of Articles") +
  theme(axis.title.x = element_blank())+
  ggtitle("Number of Daily CNN Articles \n About Hurricane Maria in Puerto Rico")
  
