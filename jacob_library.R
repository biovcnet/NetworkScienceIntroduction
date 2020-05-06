## Helper functions
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

reorder_cor_and_p <- function(cormat, pmat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  pmat <- pmat[hc$order, hc$order]
  list(r = cormat, p = pmat)
}

firstVar <- function(Var1, Var2){
  ifelse(Var1 < Var2, Var1, Var2)
}

secondVar <- function(Var1, Var2){
  ifelse(Var1 < Var2, Var2, Var1)
}