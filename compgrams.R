
compgrams <- function(grams, comp){
  # A function to provide recommended matches for each gram
  # Creates n-grams for lengths i to k
  #
  # Args:
  #       grams: a vecgrams formatted data frame (see vecgrams.R)
  #       comp: vector of names to match against
  #
  # Returns:
  #       a data frame with top matches per 
  #
  grams.list <- unique(grams[, 2])
  master <- data.frame()
  for(k in grams.list){
    print(k)
    temp <- grams[ grams[,2] == k, "grams"]
    temp <- na.omit(temp)
    counter <- c()
    for(i in temp){
      counter <- c(counter, grep(i, comp))
    }
    if(length(counter)>0){
      res <- aggregate(counter, by = list(counter), FUN = length)
      res <- res[order(-res$x),]
      res <- res[1:5,]
      res[,1] <- comp[res[,1]]
      res <- na.omit(res)
      res$id <- k
      res <- res[,c(3,1,2)]
      colnames(res) <- c("vec","comp.match","score")
      res$lev <- NA
      for(j in 1:nrow(res)){
        res$lev[j] <- adist(res$vec[j], res$comp.match[j])
      }
      master <- rbind(master, res)
    }
}
    
  return(master)
}

#Example
  #test <- c("Four score and seven years ago","where is star?", "what?")
  #comp <- c("star", "dog","score", "score boards have stars on them seven year")
  #vecgrams(test,1, 5)
 
