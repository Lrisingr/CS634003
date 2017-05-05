library(arules)
transactions<- read.transactions(file="forests.txt",format=c("basket"),rm.duplicates = FALSE)
param<- list(support=0.6,target="maximally frequent itemsets")
rules= apriori(transactions,param)
itemsets.df <- as(rules, "data.frame")
#Order itemset by Support
frequentItemsets <- itemsets.df[with(itemsets.df, order(-support,items)),]
#Find Itemsets with least support i.e of the Maximally frequent itemsets,find itemsets with less frequency/support
leastCount<- min(frequentItemsets$support)
#Find itemsets of  Support = leastCount
leastFrequentItemsets<-subset(frequentItemsets,frequentItemsets$support==leastCount)
#plot maximally frequent itemsets with support
plot(frequentItemsets$items,frequentItemsets$support)
#plot maximally frequest Itemsets with least Support
plot(leastFrequentItemsets$items,leastFrequentItemsets$support)


