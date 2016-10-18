###################################################################
# Data Science Competency Training
# Core - 002 Correlation Analysis
#
# Author: Ian Lo, 2016
# Organisation: CrimsonLogic Pte Ltd
###################################################################
#
# The following code details the use of correlation to deteremine the
# relationship between two variables from the mtcars dataset


# load required libraries
library(ggplot2)

# plot relationship between mpg and hp from the mtcars dataset
ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + geom_smooth(method = "lm")

# test the correlations
cor.test(mtcars$mpg, mtcars$hp)

# using the correlation function in the Hmisc package
library(Hmisc)
rcorr(as.matrix(data.frame(mtcars$mpg, mtcars$hp)), type = "pearson")
