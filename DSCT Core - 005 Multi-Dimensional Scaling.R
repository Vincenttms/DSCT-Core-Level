# Multi-Dimensional Scaling Example in R
# Using cereal data

library(MASS)
library(ggfortify)

data(UScereal)
head(UScereal)

# first need to convert the data into a distance matrix
# euclidean distances between the rows
d <- dist(UScereal[,1:10], method = "euclidean", diag=TRUE)

# Apply classical MDS 
fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
fit # view results

# visualise in 2D using labels
autoplot(fit, shape = FALSE, label.colour = 'blue', label.size = 3)

# visualise using ggplot
ggplot(as.data.frame(fit$points), aes(fit$points[,1], -fit$points[,2], label = rownames(fit$points))) +
  geom_text(check_overlap = TRUE, size=3) +
  #xlab('Log10(V1)') + ylab('Log10(V2)') +
  #scale_x_continuous(breaks = NULL, trans='log2') +
  #scale_y_continuous(breaks = NULL, trans='log2')
  xlab('V1') + ylab('V2')


# visualise in 3D
isoFit <- isoMDS(d, k=3) # k is the number of dim
isoFit # view results

library(rgl)
plot3d(isoFit$points, size=5)


require(igraph)
library(igraph)
g <- graph.full(nrow(fit$points))
V(g)$label <- rownames(fit$points)
layout <- layout.mds(g, dist = as.matrix(d))
plot(g,
     layout = layout,
     vertex.size = 4,
     vertex.label.font=1,
     vertex.label.family='sans',
     vertex.label.cex=0.75,
     vertex.label.dist=-0.5)
