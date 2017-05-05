library(arules)
library(arulesViz)
library(Matrix)

#read the file to get the Trasactions from
trans_2<- read.transactions(file="forests.txt",format=c("basket"))
#require all the max-frequent plant species with a support threshold of 60%
param_2<- list(support=0.6,target="maximally frequent itemsets",maxlen=206)
#Apply apriori algorithm with required parameters.
Question_2 = apriori(trans_2,param_2)
#convert into a data.frame to display the results
max_freq_species.df <- as(Question_2,"data.frame")

View(max_freq_species.df)