
kmatch <- function(y1, y2, n = 3, replacement = F){
  # Produces nearest neighbor propensity score match from two dataframes
  # Input dataframes should be organized as follows: ID var in column 1, propensity score in column 2
  #
  # Args:
  #       y1: left hand dataframe
  #       y2: right hand dataframe
  #       n: number of matches
  #       replacement: draw with replacement
  #
  # Returns:
  #       a data frame with matches and scores
  #
  master <- data.frame()
  y1 <- na.omit(y1)
  y2 <- na.omit(y2)
  for(i in 1:nrow(y1)){
    ref <- y1[i,2]
    y3 <- rank(abs(y2[,2] - ref), ties.method = "random")
    pos <- y2[match(seq(1,n,1), y3),]
    if(nrow(y2) > n){
      master <- rbind(master, data.frame(ref = ref, ref.org = y1[i, 1], match.val = pos[,2], match.org = pos[,1]))
      if(replacement == F){
        y2 <- y2[!(y2[,1] %in% pos[,1]),]
      }
    }
  }
  return(master)
}
