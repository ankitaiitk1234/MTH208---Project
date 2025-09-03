library(rvest)
library(tidyverse)


html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-shows-and-movies-to-binge-watch-now/")

# extracting the each webseries page link
website <- html %>%  html_elements(".article_movie_title a") %>% html_attr("href")
website <- paste0("https:", website)

WebSeries <- html %>%  
  html_elements(".article_movie_title a") %>%
  html_text()


num_webseries <- length(website)
Revs <- numeric(length = length(website))
Tomatometer <- numeric(length = length(website))
Popcornmeter <- numeric(length = length(website))
AudienceRevs <- numeric(length = length(website))
Genre <- numeric(length = length(website))
Seasons <- numeric(length = length(website))
ReleaseDate <- numeric(length = length(website))


for(i in 1:num_webseries)
{
  html_webseries <- read_html(website[i])
  Revs[i] <- html_webseries %>% html_elements("rt-link[slot = criticsReviews]")%>% html_text()
  AudienceRevs[i] <- html_webseries %>% html_elements("rt-link[slot = audienceReviews]")%>% html_text()
  Tomatometer[i] <- html_webseries %>% html_elements("rt-button[slot = criticsScore]")%>% html_text()
  Popcornmeter[i] <- html_webseries %>% html_elements("rt-button[slot = audienceScore]")%>% html_text()
  Genre[i] <- html_webseries %>% html_elements("rt-text[slot = genre]") %>% html_text()
  Seasons[i] <- html_webseries %>% html_elements("rt-text[slot = seasonsCount]") %>% html_text()
  ReleaseDate[i] <- html_webseries %>% html_elements("rt-text[slot = releaseDate]") %>% html_text()
}


# Cleaning the data obtain above
for(i in 1:100)
{
  # Reviews
  find_num <- as.numeric(strsplit(Revs[i], " ")[[1]])
  Revs[i] <- find_num[!is.na(find_num)]
  
  # Audience Reviews
  get_one <- strsplit(AudienceRevs[i], "\\+")[[1]][1]
  
  #  removing the `+`
  then <- tail(strsplit(get_one, " ")[[1]], 1)
  then <- paste0(strsplit(then, ",")[[1]], collapse = "")
  AudienceRevs[i] <- as.numeric(then)
  
  # Removing %
  Tomatometer[i] <- substr(Tomatometer[i], start = 1, stop = nchar(Tomatometer[i]) - 1)
  Popcornmeter[i] <- substr(Popcornmeter[i], start = 1, stop = nchar(Popcornmeter[i]) - 1)
  
  # count of seasons
  find_num2 <- as.numeric(strsplit(Seasons[i], " ")[[1]])
  Seasons[i] <- find_num2[!is.na(find_num2)]
}

# converting the data into data frame
data <- data.frame(WebSeries,Genre,Revs, AudienceRevs, Tomatometer, Popcornmeter,Seasons,ReleaseDate)
data

#converting into csv file 
write.csv(data, "data.csv", row.names = FALSE)
