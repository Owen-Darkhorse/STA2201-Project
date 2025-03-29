## This scripts Aims to Visualize Clusters

ggplot <- function(df, Z, K, centroids, varList) {
  col = c("forestgreen", "orange", "steelblue", "purple", "yellow", "salmon")
  c = pchisq(0.8, 2)
  phase <- seq(0,2*pi, 0.01)
  
  circle <- data.frame("h" = cos(phase) * sqrt(c), 
                       "v" = sin(phase) * sqrt(c))
  plt <- ggplot() + coord_equal()
  
  for (k in 1:K) {
    df = data.frame(df, alpha = Z[,k])
    mu = M[,k]
    
    center = data.frame(longtitude = mu[1], latitude = mu[2])
    Sigma = S[[k]]
    ED = eigen(Sigma)
    ellipse = t(ED$vectors %*% diag(sqrt(ED$values)) %*% t(circle) + mu)
    colnames(ellipse) = c("PC1", "PC2")
    
    plt = plt + geom_point(df, mapping = aes(h, v, alpha = alpha), color = col[k]) +
      geom_point(data = center, mapping = aes(h, v), color = col[k], shape = 4, size = 4)+
      geom_path(data = ellipse, mapping = aes(h, v), color = col[k])
    
  }
  print(plt)
  
}