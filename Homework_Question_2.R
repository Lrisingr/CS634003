Google<- read.csv(file="GoogleProducts.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
  gameRelatedInfo = read.csv(file="gaming.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

  desc <- Google[,c(3)]
  GTitle <- Google[,c(2)]
  gaming= subset(gameRelatedInfo,grepl("gaming",gameRelatedInfo$GAMING))
  matches<- unique(grep(paste(gaming,collapse = "|"),desc,value = TRUE))
  #Gives whether given string in the gaming subset matches with the subset in the Google Object
  descBoolean=sapply(gaming,
                     function(y)
                       sapply(desc,
                              function(x)
                                any(grep(y,x,value = TRUE)))
  )

  GTitleBoolean = sapply(gaming,
                         function(y)
                           sapply(GTitle,
                                  function(x)
                                    any(grepl(y,x)))
  )

  test1 = sapply(gaming, function(y) {
    sapply(GTitle, function(x) {
      any(grepl(y,x, fixed = TRUE))
    })
  })

  #retrieve all the values that are true in the Google subset
  #Question:why does it give me TRUE in the name field , when i'm seaching only in the description subset
