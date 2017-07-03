

roi <- function(df,n) {
  
  library(pacman)
  pacman::p_load(PUBAD)
  
  # extract variables
  X <- as.matrix(df)

  # find Euclidian distances
  d1 <- as.matrix(dist(X, method = "euclidean"))
  diag(d1) <- Inf

  # find region of influence nearest neighbors (n)
  ROI_NN <- matrix(NA, nrow=nrow(X), ncol=n)
  for (i in 1:ncol(d1)){
    ROI_NN[i,] <- order(d1[,i])[1:n]
  }
  
  # return nearest neighbors
  return(ROI_NN)
  
}

# example
# nn <- roi(data.full$STAID,10)
