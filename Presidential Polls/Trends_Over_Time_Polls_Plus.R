library(dplyr)
library(tidyr)
library(arules)
library(stringr)
library(lubridate)
library(plotly)
source("Polls.R")
source("Filters.R")
#Create a data.drame for Clinton with RAW & ADJ Polls
  ClintonOnly <-  PollsPlus.Filter%>%
  select(poll_wt,samplesize,enddate,rawpoll_clinton,adjpoll_clinton)%>%
  mutate(choice = c("Clinton"))
 
  ClintonOnly<-setNames(ClintonOnly,c("poll_wt","samplesize","enddate","raw_Poll","adj_Poll","choice"))

  #Create a data.drame for Trump with RAW & ADJ Polls
  
  TrumpOnly<-PollsPlus.Filter %>%
  select(poll_wt,samplesize,enddate,rawpoll_trump,adjpoll_trump)%>%
  mutate(choice = c("Trump"))
  
  TrumpOnly<-setNames(TrumpOnly,c("poll_wt","samplesize","enddate","raw_Poll","adj_Poll","choice"))
  
#Create a data.drame for Johnson with RAW & ADJ Polls
  
    JohnsonOnly<- PollsPlus.Filter %>%
    select(poll_wt,samplesize,enddate,rawpoll_johnson,adjpoll_johnson)%>%
    mutate(choice = c("Johnson"))
    
    JohnsonOnly<-setNames(JohnsonOnly,c("poll_wt","samplesize","enddate","raw_Poll","adj_Poll","choice"))
    
#Bind all the columns as rows from Clinton,Trump,Johnson and create a new data frame
    Combine.IndivData<- data.frame()
     Combine.IndivData<-bind_rows(rbind(ClintonOnly,TrumpOnly,JohnsonOnly),Combine.IndivData)
     
#Calculate Cumulative mean & weighted mean for RAW POLL and append the columns to the new Combine.IndivData dataframe
        Combine.RAWIndivData<- Combine.IndivData %>%
                              filter(poll_wt>0)%>%
                              mutate(dayssince = as.numeric(lubridate::today() - enddate  ))%>%
                              mutate(wt = poll_wt * sqrt(samplesize) / dayssince) %>%
                              mutate(votewt = wt*raw_Poll) %>%
                              group_by(choice) %>%
                              arrange(choice, -dayssince) %>%
                              mutate(cum.mean.wt = cumsum(votewt) / cumsum(wt)) %>%
                              mutate(cum.mean = cummean(raw_Poll)) 

        
ggplot2::ggplot(subset(Combine.RAWIndivData,( enddate > ymd("2015-11-01"))),   
ggplot2::aes(y=cum.mean, x=enddate, color=choice)) +
ggplot2::geom_point()+
ggplot2::geom_line() + 
ggplot2::geom_point(ggplot2::aes(size=wt))+
ggplot2::labs(title = "Cumulative Mean Vote Percentage by RAW_POLL \n",  
                        y = "Cumulative Percent Vote by POLLS-PLUS type", 
                        x = "Poll Date", 
                        color = "Candidate", 
                        size="Calculated Weight")
         
#Calculate Cumulative mean & weighted mean for ADJ POLL and append the columns to the new Combine.
#IndivData dataframe
Combine.ADJIndivData<- Combine.IndivData %>%
                       filter(poll_wt>0) %>%
                       mutate(dayssince = as.numeric(lubridate::today() - enddate  ))%>%
                       mutate(ADJwt = poll_wt * sqrt(samplesize) / dayssince) %>%
                       mutate(votewt = ADJwt*adj_Poll) %>%
                       group_by(choice) %>%
                       arrange(choice, -dayssince) %>%
                       mutate(cum.mean.wt = cumsum(votewt) / cumsum(ADJwt)) %>%
                       mutate(cum.mean = cummean(adj_Poll)) 

ggplot2::ggplot(subset(Combine.ADJIndivData,( enddate > ymd("2015-11-01"))), 
ggplot2::aes(y=cum.mean, x=enddate, color=choice)) +
ggplot2::geom_point()+
ggplot2::geom_line() +
ggplot2::geom_point(ggplot2::aes(size=ADJwt)) +
ggplot2::labs(title = "Cumulative Mean Vote Percentage by ADJUSTED POLL\n",  
                y = "Cumulative Percent Vote by POLLS-PLUS type", 
                x = "Poll Date", 
                color = "Candidate", 
                size="Calculated Weight")

      