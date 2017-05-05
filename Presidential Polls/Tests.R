source("5Folds.R")

ACCUR_LogiR_nBayes = t.test(accur[,1],accur[,2],conf.level=0.95,paired=TRUE)
PRECISION_LOGIR_NBAYES = t.test(precis[,1],precis[,2],conf.level=0.95,paired=TRUE)

ACC_SVM_Bayes<- t.test(accur[,2],accur[,3],conf.level=0.95,paired=TRUE)
PRECISION_SVM_Bayes<- t.test(precis[,2],precis[,3],conf.level=0.95,paired=TRUE)


ACCUR_LOGIR_SVM<-t.test(accur[,1],accur[,3],conf.level=0.95,paired=TRUE)
PRECIS_LOGIR_SVM<-t.test(precis[,1],precis[,3],conf.level=0.95,paired=TRUE)


