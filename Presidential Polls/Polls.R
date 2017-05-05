library(dplyr)
library(tidyr)
library(arules)
library(stringr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(scales)

# calculate how many observations are made for three prediciton models catogorized by FiveThirtyEight.com
#http://projects.fivethirtyeight.com/2016-election-forecast/national-polls/
#Polls-plus forecast -->  What polls, the economy and historical data tell us about Nov. 8
#Polls-only forecast -->  What polls alone tell us about Nov. 8
#Now-cast            -->  Who would win the election if it were held today
data.polls <- read.csv('presidential_polls.csv',sep=",",as.is = TRUE,stringsAsFactors = FALSE)
grades<- unique(data.polls$grade)

data.polls <- data.polls %>%
  mutate(
    forecastdate = as.Date(forecastdate, "%m/%d/%y"),
    startdate = as.Date(startdate, "%m/%d/%y"),
    enddate  = as.Date(enddate, "%m/%d/%y"),
    createddate  = as.Date(createddate, "%m/%d/%y"),
    timestamp  = as.POSIXct(strptime(timestamp, "%m/%d/%y %H:%M")),
    grade = ordered(grade, levels = grades))
