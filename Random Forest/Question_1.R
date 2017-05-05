library(arules)
library(arulesViz)
library(Matrix)
rm(list = ls())
try(dev.off(),silent = T)

#Load transactions from Database with rm.Duplicates = FALSE so as to check all the repeating patterns too
trans_1<- read.transactions(file="forests.txt",format=c("basket"),rm.duplicates = FALSE)
#find all the rules with minimum support threshold of 60%
Question_1 = apriori(trans_1,list(support=0.6))
#retrieve only the patterns after removing duplicates
View(inspect(head(unique(Question_1@lhs),20)))




