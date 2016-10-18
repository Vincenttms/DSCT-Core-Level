# Multi-Dimensional Scaling Example in R
# Using cereal data

library(MASS)
library(ggfortify)

data(UScereal)
head(UScereal)

# first need to convert the data into a distance matrix
# euclidean distances between the rows
< ... enter your code ... >
  
# Apply classical MDS 
< ... enter your code ... >
fit # view results

# visualise in 2D using labels
< ... enter your code ... >
  
# visualise using ggplot
< ... enter your code ... >
  

# visualise in 3D
< ... enter your code ... >
isoFit # view results


library(rgl)
< ... enter your code ... >
  

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
