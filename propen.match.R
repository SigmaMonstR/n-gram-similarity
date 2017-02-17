
kmatch <- function(y1, y2, n = 3, replacement = F){
  master <- data.frame()
  y1 <- na.omit(y1)
  y2 <- na.omit(y2)
  for(i in 1:length(y1)){
    print(i)
    ref <- y1[i]
    y3 <- rank(abs(y2 - ref), ties.method = "random")
    pos <- y2[match(seq(1,n,1), y3)]
    if(length(y2) > n){
      master <- rbind(master, data.frame(ref = ref, match.val = pos))
      if(replacement == F){
        y2 <- y2[!(y2 %in% pos)]
      }
    }
  }
  return(master)
}
