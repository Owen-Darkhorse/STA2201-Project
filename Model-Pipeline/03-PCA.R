## This script aims to compute principal components from off diagonal matrix of 
## PLV off-diagonal elements
## @ df: it is the matrix of off-diagonal PLV elements in dataframe format
## @ y: factor, a vector of class labels, same length as df
## @ impExpl: a float between 0 an 1, minimum proportion of variance should be 
## explained by a PC
## @ plotting: boolean, whether to plot 
## Returns: a list containing the followings
## components: A data.frame of PCs, with columns as the PC's index
## Z : A transformed df by loading vectors
## prop_var_explained: A number, actual cumulative variance explained
## Side Effect: create a biplot


library(ggplot2)

PCA <- function(df, y=NULL, minVar, plotting) {
  pcaResult <- prcomp(df, center = T, scale = T)
  impExpl = pcaResult$sdev^2 / sum(pcaResult$sdev^2) #Proportions of variance explained
  min_components = which.max(impExpl > minVar)
  components = pcaResult$rotation[, 1:min_components]
  df_hat = as.matrix(df) %*% components %*% t(components)
  
  Z = as.matrix(df) %*% pcaResult$rotation[,1:2]
  scoreFrame = data.frame(Z)
  scoreFrame = cbind(scoreFrame, y)
  colnames(scoreFrame) = c("PC1", "PC2", "y")
  
  if (plotting) {
    ggplot(data = scoreFrame) + 
      geom_point(aes(x = PC1, y = PC2, color = factor(y), shape = factor(y)))+
      labs(x = paste("PC 1:", round(impExpl[1]*100), "%"),
           y = paste("PC 2:", round(impExpl[2]*100, 2), "%"),
           title = "Biplot of the Top 2 PCs")
  }
  
  return(
    list(
      "n_components" = min_components,
      "V" = data.frame(components),
      "Z" = scoreFrame,
      "prop_var_explained" = round(sum(impExpl[impExpl > minVar]), 2)
       )
  )
}
