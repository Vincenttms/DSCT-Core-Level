# Principle Component Analysis Example in R
# Using iris data

library(ggfortify)

data(iris)
head(iris, 3)

# trick to assign a colour to each species so that easier for visualisation later
values <- c("red", "green", "blue")
iris$colour <- values[iris$Species]


# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE.
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE,
                 retx=TRUE) 

# summary of the PCA
summary(ir.pca) 

# plot the scree plot
screeplot(ir.pca, type="lines")


# plot the principle components to visualise the data in 2D space
# including the eigenvectors
autoplot(ir.pca, data = iris, colour = "Species", cex=3,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


# We can also plot the principle components in 3D space using the first
# 3 loadings - we tag each species with a colour and then use it to visualise
library(rgl)
plot3d(ir.pca$x[,1:3], col = iris$colour, size=5)


autoplot(kmeans(ir.pca$x[,1:3],3), ir.pca$x[,1:3])
