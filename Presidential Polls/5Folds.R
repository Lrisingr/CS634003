try(dev.off(),silent=TRUE) # clear all plots
source("Preprocess_data.R")
pdf(file='Graphs.pdf',width=10,height=7.5)
df<- subset(df,select=c(-cycle,-branch,-matchup,-forecastdate,-url))

df = df[sample(nrow(df)),] 
folds = cut(seq(1,nrow(df)),breaks=5,labels=FALSE) 

#store all the value of accuracy and precision in the form of a matrix to differentiate and
#display it to the user

accur = matrix(data=NA,nrow=5,ncol=3)
precis = matrix(data=NA,nrow=5,ncol=3)
cutoff = 0.50

#Perform 5 fold cross validation

for(i in 1:5){
  #Segment your data by fold using the which() function 
  indicesTest = which(folds==i,arr.ind=TRUE)
  dfTest = df[indicesTest, ]
  dfTraining = df[-indicesTest, ]

  #Naive Bayes
  
  nBayes = naiveBayes(Loser~spread_weight+grade,data=dfTraining)
  nBayes.prob<- predict(nBayes,dfTest,type='raw')[,'1']
  nBayes.labels<- dfTest$Loser
  nBayes.prediction<- prediction(nBayes.prob,nBayes.labels)
  
  nBayes.performance<-performance(nBayes.prediction,"tpr","fpr")
  nBayes.auc<-performance(nBayes.prediction,"auc")
  nBayes.prec<- performance(nBayes.prediction,"prec")
  nBayes.acc<- performance(nBayes.prediction,"acc")
  
  #create a logistic regression model based on the class label and dfTraining
  #glm() uses an iterative re-weighted least squares algorithm. 
  #The algorithm hit the maximum number of allowed iterations before signalling convergence. 
  #The default, documented in ?glm.control is 25. 
  #You pass control parameters as a list in the glm call: pass maxit=50
  
  Logi.Regr<-glm(Loser~spread_weight,data=dfTraining,family=binomial("logit"))
  #predicting the test data
  Logi.Regr.probs<-predict(Logi.Regr, dfTest, type = "response")
  Logi.Regr.prediction<-prediction(Logi.Regr.probs, dfTest$Loser)
  #define what the labels are 
  Logi.Regr.labels<-dfTest$Loser
  #roc analysis for test data
  Logi.Regr.performance<-performance(Logi.Regr.prediction,"tpr","fpr")
  Logi.Regr.auc<-performance(Logi.Regr.prediction,"auc")
  Logi.Regr.prec<- performance(Logi.Regr.prediction,"prec")
  Logi.reg.acc<- performance(Logi.Regr.prediction,"acc")
  
  #Model- 3 SVM
  Model.SVM <- svm(Loser~spread_weight+grade,data=dfTraining)
  SVM.Prob<-predict(Model.SVM, dfTest,type="response")
  SVM.prediction<- ROCR::prediction(SVM.Prob,dfTest$Loser)
  
  SVM.performance<-performance(SVM.prediction,"tpr","fpr")
  SVM.auc<-performance(SVM.prediction,"auc")
  SVM.prec<- performance(SVM.prediction,"prec")
  SVM.acc<- performance(SVM.prediction,"acc")
  
  
  #graphs
  #calculate TPR vs FPR performance
  windows()
  plot(Logi.Regr.performance,main=paste('ROC: Fold ',i,sep=''),xaxs='i',yaxs='i',asp=1,col="red",lwd=2)
  lines(SVM.performance@x.values[[1]],nBayes.performance@y.values[[1]],add=TRUE,col="green",lwd=2)
  lines(nBayes.performance@x.values[[1]],nBayes.performance@y.values[[1]],add=TRUE,col="blue",lwd=2)
  plot_range<-range(0,0.5,0.5,0.5,0.5)
  legend(0.5, plot_range[2], c("logistic regression","svm","NBayes"), cex=0.8,
         col=c("red","green","blue"), pch=21:22, lty=1:2)
  
  #Calculate Precision
  windows()
  par(pty="m")
  plot(Logi.Regr.prec,main=paste('Precision: Fold ',i,sep=''),ylim=c(0.4,1),col="red",lwd=2)
  lines(SVM.prec@x.values[[1]],SVM.prec@y.values[[1]],add=TRUE,col="green",lwd=2)
  lines(nBayes.prec@x.values[[1]],nBayes.prec@y.values[[1]],col='blue')
  plot_range<-range(0,0.5,0.5,0.5,0.5)
  legend(0.5, plot_range[2], c("logistic regression","svm","NBayes"), cex=0.8,
         col=c("red","green","blue"), pch=21:22, lty=1:2)
  
  #Calcuate Accuracy
  windows()
  plot(Logi.reg.acc,main=paste('Accuracy: Fold ',i,sep=''),ylim=c(0.4,1),col="red",lwd=2)
  lines(SVM.acc@x.values[[1]],SVM.acc@y.values[[1]],add=TRUE,col="green",lwd=2)
  lines(nBayes.acc@x.values[[1]],nBayes.acc@y.values[[1]],col='blue')
  abline(v=0.5,lty=2)
  plot_range<-range(0,0.5,0.5,0.5,0.5)
  legend(0.5, plot_range[2], c("logistic regression","svm","NBayes"), cex=0.8,
         col=c("red","green","blue"), pch=21:22, lty=1:2)
  
  accur[i,1] = Logi.reg.acc@y.values[[1]][max(which(Logi.reg.acc@x.values[[1]]>=cutoff))]
  accur[i,2] = nBayes.acc@y.values[[1]][max(which(nBayes.acc@x.values[[1]]>=cutoff))]
  accur[i,3] = SVM.acc@y.values[[1]][max(which(SVM.acc@x.values[[1]]>=cutoff))]
  
  precis[i,1] = Logi.Regr.prec@y.values[[1]][max(which(Logi.Regr.prec@x.values[[1]]>=cutoff))]
  precis[i,2] = nBayes.prec@y.values[[1]][max(which(nBayes.prec@x.values[[1]]>=cutoff))]
  precis[i,3] =  SVM.prec@y.values[[1]][max(which(SVM.prec@x.values[[1]]>=cutoff))]
}


