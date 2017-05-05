#read the source from the CSV files
  Google<-read.csv(file="GoogleProducts.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
  Amazon<-read.csv(file="Amazon.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

  #Replaces all matches of a string, if the parameter is a string vector, returns a string vector
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  # take shortest string,  apply Levenshtein distance over the largest one to take the minimum

  adist.custom <- function (String_1, String_2, sliding = TRUE)
  {
    s.String_1<-strsplit(trim(String_1), split=' ')
    s.String_2<-strsplit(trim(String_2), split=' ')
    s.String_2<-trim(unlist(s.String_2))
    s.String_1<-trim(unlist(s.String_1))

    #return which string is shorter/longer
    if (length(s.String_2)>=length(s.String_1))
    {
      Short.string<-  s.String_1
      Long.string<-s.String_2
    } else {
      Short.string <- s.String_2
      Long.string<-s.String_1
    }
    # sliding
    return.LevDistance<-0
    if (sliding == TRUE)
    {
      min<-99999
      s1<-trim(paste(Short.string,collapse = ' '))

      #trim the long string over the data in the short string

      for (k in 1:(length(Long.string)-length(Short.string)))
      {
        s2<-trim(paste(Long.string[k:(length(Short.string)+(k-1))],collapse = ' '))
        #check if there is a partial match comparing both the strings
        ads<-adist(s1,s2,partial = TRUE, ignore.case = TRUE)
        min <- ifelse(ads<min,ads,min)
      }
      #return the distance between two strings
      return.LevDistance<-min
    } else {

      #string start matching
      s1<-trim(paste(Short.string,collapse = ' '))
      s2<-trim(paste(Long.string[1:length(Short.string)],collapse = ' '))
      return.LevDistance<-adist(s1,s2,partial = TRUE, ignore.case = TRUE)
    }
    return (return.LevDistance)
  }
  #set the size of the vector/dataMatrix to the length of columns in Amazon,Google vectors
  MatrixDataSize<-matrix(NA, ncol = length(Amazon$title),nrow = length(Google$name))
  #Loop over both the vectors and set the Title,Name,Description to Lower alphabet
  for(i in 1:length(Amazon$title))
  {
    for(j in 1:length(Google$name))
    {
      MatrixDataSize[j,i]<-adist.custom(tolower(Amazon[i,]$title),tolower(Google[j,]$name))
    }
  }

  min.name.custom<-apply(MatrixDataSize, 1, min)

  match.google.amazon<-NULL

  #Write whatever the strings that matched to Match.google.amazon vector with size of MatrixDataSize
  for(i in 1:nrow(MatrixDataSize))
  {
    s2.i<-match(min.name.custom[i],MatrixDataSize[i,])
    s1.i<-i
    match.google.amazon<-rbind(data.frame(s2.i=s2.i,
                                          s1.i=s1.i,
                                          Amazon_Title=Amazon[s2.i,]$title,
                                          Google_Name=Google[s1.i,]$name,
                                          adist=min.name.custom[i]),match.google.amazon)
  }


  writeToCSV<- write.csv(match.google.amazon,file="MyDataMatrix.csv")

  
