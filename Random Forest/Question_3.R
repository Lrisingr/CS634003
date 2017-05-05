library(arules)
library(arulesViz)
trans_3<- read.transactions(file="forests.txt",format=c("basket"),rm.duplicates = FALSE)
basket<- read_baskets()
param_3<- list(support=0.6,target="frequent itemsets",minlen=5,maxlen=206)
Question_3= apriori(trans_3,param_3)
Question_3.df<- as(Question_3,"data.frame")
library(stringr) 
char.Question_3 <- as.character(Question_3.df[,1])
char.Question_3 <- sort( str_replace_all( char.Question_3, "[{}]", "" ) )
forests<-list()
for(i in 1:length(char.Question_3)) {
  Ps <- sapply(strsplit(char.Question_3[i], ","), "[", 1)
  forests[[i]] <- Ps
}
setNames(forests, paste0("Forest_ID", 1:length(char.Question_3)))
unique(forests)









