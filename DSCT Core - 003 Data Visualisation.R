# We will be using the mtcars dataset

# create a table count of frequency for each category
counts_cylinders <- table(mtcars$cyl)
counts_cylinders

# visualise using a bar plot
barplot(counts_cylinders,
        main="Count of records by Cylinders",
        xlab="Cylnder Count",
        ylab="Frequency")


# histogram based on the variable mpg
hist(mtcars$mpg)


# histogram based on sepecific bins
hist(mtcars$mpg,
     breaks=10,
     xlab="Miles per Gallon",
     main="Histogram with 10 bins")


# boxplot of mpg
boxplot(mtcars$mpg, main="Boxplot of Miles/Gallon")

# boxplot of mpg by cylinders
boxplot(mtcars$mpg ~ factor(mtcars$cyl), main="Boxplot of Miles/Gallon for Different Cylinders")


# scatterplot of mpg vs weight
plot(mtcars$wt,
     mtcars$mpg,
     main="Simple Scatter Plot of MPG vs Weight",
     xlab="Car Weight (lbs/100)",
     ylab="Miles Per Gallon")

# scatter plot that includes trend line
abline(lm(mpg ~ wt, data=mtcars))

