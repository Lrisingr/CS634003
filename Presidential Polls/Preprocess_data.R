library(ROCR)
library(e1071) 
library(rminer)
library(SDMTools)

data.polls[is.na(data.polls)]<- 0
colnames(data.polls)[colnames(data.polls) == 'population'] <- 'pop_type'

ADJPoll.df<- subset(data.polls,select=c(adjpoll_trump,adjpoll_clinton, adjpoll_johnson,adjpoll_mcmullin))
RAWPoll.df<- subset(data.polls,select=c(rawpoll_trump,rawpoll_clinton, rawpoll_johnson,rawpoll_mcmullin))

#Check who is the winner based on the Adjusted poll 
checkWinner <- function(df) {
  nameWinner<- colnames(df)[max.col(df, ties.method = 'first')]
  return(sub(".*_", "", nameWinner))}


data.polls <- data.polls %>% 
            mutate(Winner = checkWinner(ADJPoll.df))

data.polls<- subset(data.polls,Winner!="mcmullin")

#if clinton wins then 1 , else 0
Loser<- ifelse(data.polls$Winner=="clinton",1,0)

data.polls<- data.polls %>%
              mutate(Loser)

numeric.grades<- seq(10,0,-1)
grades_tbl<- cbind(grades,numeric.grades)
colnames(grades_tbl)<- c("grade","numeric.grades")

PollsOnly.Filter <-  filter(data.polls, type == "polls-only")
NowCast.Filter <-  filter(data.polls, type == "now-cast")
PollsPlus.Filter <-  filter(data.polls, type == "polls-plus")

statesWinner<- subset(PollsOnly.Filter,select=c(state,Winner,Loser))
df<-PollsOnly.Filter
df<- merge(x=df,y=grades_tbl,by="grade",all.x = TRUE, all.y = FALSE)
df$numeric.grades<- as.numeric(df$numeric.grades)

#create spread weight for the as a definition for classification
spread = df$adjpoll_clinton-df$adjpoll_trump
spread_weight = spread*df$poll_wt
spread_weight_grade = spread_weight*as.numeric(df$numeric.grades) #unused in models
df = cbind(df,spread,spread_weight,spread_weight_grade)
