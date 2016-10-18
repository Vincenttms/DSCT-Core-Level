# Principle Component Analysis Example in R
# Using iris data

library(ggfortify)

data(iris)
head(iris, 3)

# trick to assign a colour to each species so that easier for visualisation later
values <- c("red", "green", "blue")
iris$colour <- values[iris$Species]


# log transform 
< ... enter your code ... >

  
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE.
< ... enter your code ... >

    
# summary of the PCA
summary(ir.pca)


# plot the scree plot
< ... enter your code ... >
  

# plot the principle components to visualise the data in 2D space
# including the eigenvectors
< ... enter your code ... >
  

# We can also plot the principle components in 3D space using the first
# 3 loadings - we tag each species with a colour and then use it to visualise
< ... enter your code ... >
  

autoplot(kmeans(ir.pca$x[,1:3],3), ir.pca$x[,1:3])
