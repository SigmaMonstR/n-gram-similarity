
vecgrams <- function(vec){
  # A function to convert a vector into a series of n-grams.
  # Creates n-grams for lengths i to k
  #
  # Args:
  #       vec: a vector of strings
  #
  # Returns:
  #       a list with nested lists
  #
  library(tm)
  library(SnowballC)
  
  master <- data.frame()
  
  for(row in 1:length(vec)){
    
    #Parse sentences in row
    print(row)
    
    sentences <- unlist(strsplit(tolower(gsub("[^[:alnum:][:space:]\\.]", "",vec[row])), "\\. "))
    

    #Parse words in sentence
    for(sent in 1:length(sentences)){
      print(paste("Sentence: ", sent))
      out <- strsplit(tolower(sentences[sent]), "[[:punct:][:space:]]")
      
      print(sent)
      
      #Build Uni-grams to Tri-grams
      t1 <- trimws(unlist(out))
      t1 <- t1[t1!="" & nchar(t1)>1]
      t1 <- stemDocument(t1)
      
      if(length(t1)==1){
        t2 <- t1
      } else if(length(t1)==2){
        t01 <- t1
        t02 <- paste(t1[1:(length(t1)-1)], t1[2:(length(t1))])
        t2 <- c(t01, t02)
      } else if(length(t1)>2){
        t01 <- t1
        t02 <- paste(t1[1:(length(t1)-1)], t1[2:(length(t1))])
        t03 <- paste(t1[1:(length(t1)-2)], t1[2:(length(t1)-1)], t1[3:(length(t1))])
        t2 <- c(t01, t02, t03)
      }
      
      #Create skip grams
      skips <- expand.grid(t1, t1)
      skips[,1] <- as.character(skips[,1])
      skips[,2] <- as.character(skips[,2])
      tskip <- as.vector(paste(skips[,1], skips[,2]))
      
      t2 <- c(t2, tskip)
      
      master <- rbind(master, data.frame(rec = row, sentence = sent, grams = t2))
    }
    
  }
  
  
  return(master)
}

#Example
  test <- c("Four score and seven years ago. Howdy doody. What's going on here","where is star.", "what?", "agreeing completing weighing")
  vecgrams(test)
