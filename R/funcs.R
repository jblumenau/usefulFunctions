trim <- function(s) gsub("^[[:space:]]+|[[:space:]]+$","",s)
word.count <- function(str1) sapply(gregexpr("\\W+", str1), length) + 1


block.bootstrap <- function(block){
  n.blocks <- length(unique(block))
  block.table <- table(block)
  resampled.blocks <- sample(unique(block),size=n.blocks,replace=TRUE)
  n.resamp <- sum(block.table[resampled.blocks],na.rm=T)
  resample <- rep(NA,n.resamp)
  counter <- 0
  unique.resampled.blocks <- unique(resampled.blocks)
  table.resampled.blocks <- table(resampled.blocks)
  
  resample <- unlist(sapply(1:length(unique.resampled.blocks), function(x) rep(which(block== unique.resampled.blocks[x]), table.resampled.blocks[x])))
  return(resample)
}	


timer.func <- function(func){
  start <- proc.time()
  func
  return(proc.time() - start)
}