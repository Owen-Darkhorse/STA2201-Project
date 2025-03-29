## This script is the main function that executes modelling 
## pipeline script in order

## Alpha Band filtering here

## PLV computing here

## Simulate some data
n_ad = 36
n_cn = 29
n_ftd = 23
p = 19 * 18 / 2
K <- 3

alzMat = matrix(rnorm(n_ad * p, -5, 1),
                nrow = n_ad, ncol = p, byrow = T)
cnMat = matrix(rnorm(n_cn * p, -1, 2),
                nrow = n_cn, ncol = p, byrow = T)
ftdMat = matrix(rnorm(n_ftd * p, 5, 3),
                nrow = n_ftd, ncol = p, byrow = T)

df = data.frame(rbind(alzMat, cnMat, ftdMat))
rownames(df) = paste0("sub-",seq(1,88))
y = c(rep("AD", 36),  rep("CN", 29), rep("FTD", 23))

## PCA on PLV off-diagonal elements
setwd("C:/Users/86139/Desktop/PARA Note System/Projects/STA2201/STA2201-Project/Model-Pipeline")
source("03-PCA.R")
pcaResult <- PCA(df, y,  0.03, T)

## Clustering score vectors
source("04-Clustering.R")
debug(pcCluster)
clusterResult <- pcCluster(pcaResult$Z[,1:pcaResult$n_components], K)

## Visualize Clustering Results
source("05-visualizeCluster.R")
gmmplot(pcaResult$Z, clusterResult$class, K, clusterResult$muHat, clusterResult$varList)

## Statistical Test here


## Supervised Learning here

