source("Polls.R")
#Calculate the Average polling for all the Contendors on Obs.Models type of polls
#Avg for Clinton on Polls-Only

PollsOnly.Filter <-  filter(data.polls, type == "polls-only")

#Compare the raw values with Adjusted values for Clinton
PO.RawClinton<-mean(PollsOnly.Filter$rawpoll_clinton)
PO.AdjClinton<-mean(PollsOnly.Filter$adjpoll_clinton)
#Avg for Trump on Polls-Only
PO.RawTrump<-mean(PollsOnly.Filter$rawpoll_trump)
PO.AdjTrump<-mean(PollsOnly.Filter$adjpoll_trump)
#Avg for Johnson on Polls-Only
PO.RawJohn<-mean(PollsOnly.Filter$rawpoll_johnson)
PO.AdjJohn<-mean(PollsOnly.Filter$adjpoll_johnson)
#Avg for Mcmillan on Polls-Only
PO.RawMc<-mean(PollsOnly.Filter$rawpoll_mcmullin)
PO.AdjMc<-mean(PollsOnly.Filter$adjpoll_mcmullin)



#Calculate the Average polling for all the Contendors on Obs.Models type of polls
#Avg for Clinton on Now-Cast

NowCast.Filter <-  filter(data.polls, type == "now-cast")


#Compare the raw values with Adjusted values for Clinton
NC.RawClinton<-mean(NowCast.Filter$rawpoll_clinton)
NC.AdjClinton<-mean(NowCast.Filter$adjpoll_clinton)
#Avg for Trump on NowCast
NC.RawTrump<-mean(NowCast.Filter$rawpoll_trump)
NC.AdjTrump<-mean(NowCast.Filter$adjpoll_trump)
#Avg for Johnson on NowCast
NC.RawJohn<-mean(NowCast.Filter$rawpoll_johnson)
NC.AdjJohn<-mean(NowCast.Filter$adjpoll_johnson)
#Avg for Mcmillan on NowCast
NC.RawMc<-mean(NowCast.Filter$rawpoll_mcmullin)
NC.AdjMc<-mean(NowCast.Filter$adjpoll_mcmullin)




#Calculate the Average polling for all the Contendors on Obs.Models type of polls
#Avg for Clinton on Polls-Plus

PollsPlus.Filter <-  filter(data.polls, type == "polls-plus")

PP.RawClinton<-mean(PollsPlus.Filter$rawpoll_clinton)
PP.AdjClinton<-mean(PollsPlus.Filter$adjpoll_clinton)
#Avg for Trump on Polls-Plus
PP.RawTrump<-mean(PollsPlus.Filter$rawpoll_trump)
PP.AdjTrump<-mean(PollsPlus.Filter$adjpoll_trump)
#Avg for Johnson on Polls-Plus
PP.RawJohn<-mean(PollsPlus.Filter$rawpoll_johnson)
PP.AdjJohn<-mean(PollsPlus.Filter$adjpoll_johnson)
#Avg for Mcmillan on Polls-Plus
PP.RawMc<-mean(PollsPlus.Filter$rawpoll_mcmullin)
PP.AdjMc<-mean(PollsPlus.Filter$adjpoll_mcmullin)







#Calculate Weighted Mean & Mean for the Polls-Only type Filter 
PollsOnlyData <- PollsOnly.Filter %>%
  filter(poll_wt > 0) %>%
  #Days passed from Today to EndDATE in the PollsOnly.Filter
  mutate(dayssince = as.numeric(lubridate::today() - enddate)) %>%
  #calculate the weights for each row 
  mutate(wt = poll_wt * sqrt(samplesize) / dayssince)%>%
  mutate(clintonWT = wt*rawpoll_clinton)%>%
  mutate(trumpWT = wt*rawpoll_trump)%>%
  mutate(johnsonWT = wt*rawpoll_johnson)%>%
  mutate(mcMullinWT = wt*rawpoll_mcmullin)%>%
  mutate(Cum.WeightedMean.Clinton = cumsum(clintonWT) / cumsum(wt)) %>%
  mutate(Cum.WeightedMean.Trump = cumsum(trumpWT) / cumsum(wt)) %>%
  mutate(Cum.WeightedMean.Johnson = cumsum(johnsonWT) / cumsum(wt)) %>%
  mutate(Cum.WeightedMean.McMullin = cumsum(mcMullinWT) / cumsum(wt)) %>%
  mutate(Cum.Mean.Clinton = cummean(rawpoll_clinton))%>%
  mutate(Cum.Mean.Trump = cummean(rawpoll_trump))%>%
  mutate(Cum.Mean.Johnson = cummean(rawpoll_johnson))%>%
  mutate(cum.mean.McMullin = cummean(rawpoll_mcmullin))
  











