###################################################################
# Data Science Competency Training
# Core - 002 Significance Testing
#
# Author: Ian Lo, 2016
# Organisation: CrimsonLogic Pte Ltd
###################################################################
#
# The following code details the use of correlation to deteremine the
# relationship between two variables from the mtcars dataset


# load required libraries
require(pastecs)

# create the dataset
runtime <-
  data.frame(
    A = c(39.6, 40.2, 40.9, 40.9, 41.4, 39.8, 39.4, 41.8, 43.6),
    B = c(42.3, 43.4, 43.9, 44.0, 44.4, 42.8, 42.1, 44.8, 45.5)
  )

# assign specific columns from the runtime dataframe into 2 separate lists
runtimeA <- runtime$A
runtimeB <- runtime$B

# determine mean and standard deviation statistics for code A runtimes
meanARun <- mean(runtime$A)
sdARun <- sd(runtime$A)

# computation of the standard error of the mean for code A runtimes
semARun <- sdARun / sqrt(length(runtime$A))

# calculate the 95% confidence intervals of the mean
ciARun <- c(meanARun - 1.96 * semARun, meanARun + 1.96 * semARun)

# print out results
sprintf(
  "Group A Runtime: Mean = %f, Standard Deviation = %f, Standard Error = %f, CI = %f, %f",
  meanARun,
  sdARun,
  semARun,
  ciARun[1],
  ciARun[2]
)



# easier way to get descriptive statistics
stat.desc(runtime$A)



# statistics for code B runtimes
meanBRun <- mean(runtime$B)
sdBRun <- sd(runtime$B)
#computation of the standard error of the mean for code A runtimes
semBRun <- sdBRun / sqrt(length(runtime$B))
#95% confidence intervals of the mean
ciBRun <- c(meanBRun - 1.96 * semBRun, meanBRun + 1.96 * semBRun)
# print out results
sprintf(
  "Group B Runtime: Mean = %f, Standard Deviation = %f, Standard Error = %f, CI = %f, %f",
  meanBRun,
  sdBRun,
  semBRun,
  ciBRun[1],
  ciBRun[2]
)

# easier way to get descriptive statistics
stat.desc(runtime$B)



# run box plot to view distribution
library(reshape2)
library(ggplot2)
runtimeData <- melt(runtime)
ggplot(runtimeData, aes(x = factor(runtimeData$variable), y = runtimeData$value)) + geom_boxplot()



# checking homoskedasticity (homogeneity of variances),
# p value of the F test should be > 0.05 meaning that there is no difference in variances
var.test(runtime$A, runtime$B)

# running independent 2 samples t-test
t.test(runtime$A, runtime$B) 
