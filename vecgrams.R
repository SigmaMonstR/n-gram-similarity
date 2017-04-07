
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
  out <- strsplit(tolower(vec), "[[:punct:][:space:]]")
  
  for(row in 1:length(out)){
    print(row)
    t1 <- unlist(out[row])
    t1 <- t1[t1!="" & nchar(t1)>0]
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
   
    master <- rbind(master, data.frame(rec = row,  grams = t2))
  }
  
  
  return(master)
}

#Example
  test <- c("Four score and seven years ago","where is star?", "what?", "agreeing completing weighing")
  vecgrams(test)
