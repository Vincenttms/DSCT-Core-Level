###################################################################
# Data Science Competency Training
# Core - 002 Significance Testing
#
# Author: Ian Lo, 2016
# Organisation: CrimsonLogic Pte Ltd
###################################################################
#
# The following code details the use of t-tests and f-test to see if
# the means from the 2 groups are the same or different


# load required libraries
require(pastecs)

# create the dataset
sales <-
  data.frame(
    A = c(39, 41, 40, 40, 41, 39, 39, 41, 43),
    B = c(42, 43, 43, 44, 44, 42, 42, 44, 45)
  )

# assign specific columns from the sales dataframe into 2 separate lists
salesA <- sales$A
salesB <- sales$B

# determine mean and standard deviation statistics for code A saless
meanASales <- mean(sales$A)
sdASales <- sd(sales$A)

# computation of the standard error of the mean for code A saless
semASales <- sdASales / sqrt(length(sales$A))

# calculate the 95% confidence intervals of the mean
ciASales <- c(meanASales - 1.96 * semASales, meanASales + 1.96 * semASales)

# print out results
sprintf(
  "Group A sales: Mean = %f, Standard Deviation = %f, Standard Error = %f, CI = %f, %f",
  meanASales,
  sdASales,
  semASales,
  ciASales[1],
  ciASales[2]
)



# easier way to get descriptive statistics
stat.desc(sales$A)



# statistics for code B saless
meanBSales <- mean(sales$B)
sdBSales <- sd(sales$B)
#computation of the standard error of the mean for code A saless
semBSales <- sdBSales / sqrt(length(sales$B))
#95% confidence intervals of the mean
ciBSales <- c(meanBSales - 1.96 * semBSales, meanBSales + 1.96 * semBSales)
# print out results
sprintf(
  "Group B sales: Mean = %f, Standard Deviation = %f, Standard Error = %f, CI = %f, %f",
  meanBSales,
  sdBSales,
  semBSales,
  ciBSales[1],
  ciBSales[2]
)

# easier way to get descriptive statistics
stat.desc(sales$B)



# run box plot to view distribution
library(reshape2)
library(ggplot2)
salesData <- melt(sales)
ggplot(salesData, aes(x = factor(salesData$variable), y = salesData$value)) + geom_boxplot()



# checking homoskedasticity (homogeneity of variances),
# p value of the F test should be > 0.05 meaning that there is no difference in variances
var.test(sales$A, sales$B)

# running independent 2 samples t-test
t.test(sales$A, sales$B) 
