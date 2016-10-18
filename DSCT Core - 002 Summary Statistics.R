
# mean of mpg
mean(mtcars$mpg)

# median of mpg
median(mtcars$mpg)

# mode of mpg - Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(mtcars$mpg)

# summary statistics incld. range(min/max)
summary(mtcars$mpg)

# standard deviation
sd(mtcars$mpg) 

# Quantiles
quantile(mtcars$mpg)

# Interquartile range
IQR(mtcars$mpg)


# estimating the population mean from sample mean
sample <- c(25, 20, 15, 5, 30, 7, 5, 10, 12, 40, 30, 30, 10, 25, 10, 20, 10, 10, 25, 5)
mean(sample)
sd(sample)

sort(sample)

sample2 <- c(25, 20, 15, 5, 30, 7, 5, 10, 12, 40, 30, 30, 10)
mean(sample2)
sd(sample2)

sample3 <- c(12, 40, 30, 30, 10, 25, 10, 20, 10, 10, 25, 5)
mean(sample3)
sd(sample3)


# calculating the standard error of the mean
stderrmean <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

stderrmean(sample)
stderrmean(sample2)
stderrmean(sample3)


# calulating sample2 confidence interval
error <- qnorm(0.975)*sd(sample2)/sqrt(length(sample2))
sample2left <- mean(sample2) -error
sample2right <- mean(sample2) +error

# calulating sample3 confidence interval
error <- qnorm(0.975)*sd(sample3)/sqrt(length(sample3))
sample3left <- mean(sample3) -error
sample3right <- mean(sample3) +error


# box plot
library(ggplot2)
shelf <- data.frame(group=c(''), value=c(39.6, 40.2, 40.9, 40.9, 41.4, 39.8, 39.4, 41.8, 43.6))
boxplot(shelf)
ggplot(shelf, aes_string(x='group',y='value')) + geom_boxplot()



