## This scripts clusters study subjects in the space spanned by top
## Principal Components computed in the last script
library(mclust)
pcCluster <- function(df, n_component) {
  GMM <- Mclust(df, G = n_component)
  muHat <- GMM$parameters$mean
  varList <- GMM$parameters$variance$sigmasq
  class <- GMM$classification
  
  return(
    list(
      "muHat" = muHat,
      "varList" = varList,
      "class" = class
    )
  )
}
