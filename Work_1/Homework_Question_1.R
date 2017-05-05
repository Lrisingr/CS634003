amazon<- read.csv(file="Amazon.csv",header=TRUE,sep=",")
  Microsoft<- subset(amazon,amazon$manufacturer=="microsoft")
  hist(Microsoft$price)

  #Display custom input of outliers

  DisplayOutliers <- function(dataset, var)
  {
    name_variable <- eval(substitute(var),eval(dataset))
    na1 <- sum(is.na(name_variable))
    m1 <- mean(name_variable, na.rm = T)
    par(mfrow=c(2, 2), oma=c(0,0,3,0)) #set multiple graphs on the same page with 2 rows  & 2 columns.
    plot(name_variable, main="With outliers",pch="*", col="red", cex=2)
    outlier <- boxplot.stats(name_variable)$out
    outliers_mean <- mean(outlier)
    name_variable <- ifelse(name_variable %in% outlier, NA, name_variable)
    plot(name_variable, main="Without outliers",pch="*", col="red", cex=2)
    title("Outlier prices Data", outer=TRUE)
    na2 <- sum(is.na(name_variable))
    cat("Outliers identified:", na2 - na1,"\n")
    outliers<-boxplot.stats(Microsoft$price)$out
    # plot(Microsoft$price, xlim=c(0, 100), ylim=c(0, 11318), main="With Outliers", xlab="Observed Values", ylab="Price", pch="*", col="red", cex=2)
    cat("Mean of the outlier prices:", round(outliers_mean, 2),"$", "\n")
    m2 <- mean(name_variable, na.rm = T)
    cat("Mean prices with outlier:", round(m1, 2),"$","\n")
    cat("Mean prices with outliers removed:", round(m2, 2),"$", "\n")
  }

  DisplayOutliers(amazon, Microsoft$price)
