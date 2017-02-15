
vecgrams <- function(vec, i, k){
  # A function to convert a vector into a series of n-grams.
  # Creates n-grams for lengths i to k
  #
  # Args:
  #       vec: a vector of strings
  #       i: minimum n-grams
  #       k: maximum n-grams
  #
  # Returns:
  #       a list with nested lists
  #
  out <- strsplit(tolower(vec), "[[:punct:][:space:]]")
  
  for(row in 1:length(out)){
    t1 <- unlist(out[row])
    t1 <- t1[t1!="" & nchar(t1)>0]
    t2 <- c()
    for(gram in i:k){
      for(s in 1:length(t1)){
        if(s-gram + 1 >0){
          t2 <- c(t2, paste(t1[(s-gram+1):s], collapse = " "))
        }
      }
    }
    out[[row]] <- t2
  }
  
  return(out)
}

#Example
#  test <- c("Four score and seven years ago","where is star?", "what?")
#  vecgrams(test,1, 5)
