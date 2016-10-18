# load ggplot2 package
library(ggplot2)

# base histogram
hist(mtcars$mpg)

# ggplot example
ggplot(mtcars, aes(x = mpg)) + geom_histogram(colour = "darkgreen",
                                              fill = "blue",
                                              binwidth = 5)


# Scatter plot of mpg vs displacement categorised by transmission
par(mar=c(4,4,.1,.1))
plot(mpg ~ disp,
     data=subset(mtcars, am == "0"),
     xlim=c(50,450),
     ylim=c(5,40))
points(mpg ~ disp, col="red",
       data=subset(mtcars, am == "1"))
legend(350, 40,
       c("Auto", "Manual"), title="Transmission",
       col=c("black", "red"),
       pch=c(1, 1))


# Using ggplot2 to plot the same scatter plot of mpg vs displacement categorised by transmission
ggplot(mtcars, aes(x=disp,y=mpg,color=factor(am))) + geom_point()



# AES example with accleration as the aestheic colour for the points
p <- ggplot(data = mtcars, aes(x = wt, mpg)) + geom_point(aes(color = qsec))
p

# AES example with cylincer as the aestheic colour for the points
p <- ggplot(data = mtcars, aes(x = wt, mpg)) + geom_point(aes(color = factor(cyl)))
p

# geom_xx example
set.seed(1523)  # This makes the result or sampling reproducible
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
pd0 <- ggplot(data = dsmall, aes(carat, price))
# geom_smooth(method = "lm"). The gray region is one standard error
pd1 <- pd0 + geom_point() + geom_smooth(method = "lm")
pd1



# stat example
ggplot(mtcars, aes(x=mpg)) + geom_bar()
ggplot(mtcars, aes(x=mpg)) + geom_bar(stat='bin')
ggplot(mtcars, aes(x=mpg)) + geom_bar(stat='bin', binwidth=4)


# position adjustment examples
ggplot(mtcars, aes(x=factor(cyl), fill=factor(am))) + geom_bar(position='stack')
ggplot(mtcars, aes(x=factor(cyl), fill=factor(am))) + geom_bar(position='fill')
ggplot(mtcars, aes(x=factor(cyl), fill=factor(am))) + geom_bar(position='dodge')
