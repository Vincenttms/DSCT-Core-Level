# R Script for Data Cleaning & Preparation

# require libraries
require(dplyr)
require(gplots)
require(RColorBrewer)
require(biganalytics) # clustering and kmeans on a large dataset requires this package to handle large datasets

# load libraries
library(dplyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(biganalytics)

# set working directory
#setwd("D:\\Projects\\Data Science Competency\\Curriculum\\00 Core\\src\\")
setwd("/Users/ianlo/Work/Projects/Data Science Competency/Curriculum/00 Core/src/")


# read csv data file
bkgs <- read.csv2("ypedia/Sample.csv", header = TRUE, sep=",",
                  stringsAsFactors = TRUE)


# convert date_time from string to date format in R as another variable
bkgs$srch_date <- as.POSIXct(bkgs$date_time, format = "%Y-%m-%d")

# convert date_time from string to datetime format in R
bkgs$date_time <- as.POSIXct(bkgs$date_time, format = "%Y-%m-%d %H:%M:%S")

# convert srch_ci and srch_co from string to date
bkgs$srch_ci <- as.POSIXct(bkgs$srch_ci, format = "%Y-%m-%d")
bkgs$srch_co <- as.POSIXct(bkgs$srch_co, format = "%Y-%m-%d")


# looking at summary statistics for the entire data set
summary(bkgs)


# What are the data issues?
# =====================================
# user_location_country = 0
# user_location_region = 0
# user_location_city = 0

# Missing orig_destination_distance
nrow(bkgs[bkgs$orig_destination_distance == "",])

# srch_ci = NAs
bkgs[is.na(bkgs$srch_ci),]

# srch_co = NAs
bkgs[is.na(bkgs$srch_ci),]

# srch_adults_cnt AND srch_children_cnt are both 0


# hotel_continent = 0
# hotel_country = 0
# hotel_market = 0
# hotel_cluster = 0
# Are there any records where the srch_co is < srch_ci?
# Are there any records where the srch_ci < srch_date(date_time)



# Data Cleaning
# =============
# Are there any records where the srch_co is < srch_ci?
bkgs %>% filter(srch_co < srch_ci)

# Are there any records where the srch_ci < srch_date(date_time)
bkgs %>% filter(srch_ci < srch_date)

# Exclude these data from analysis (take note of the boolean logic)
bkgs <- bkgs %>% filter( !((srch_co < srch_ci) | (srch_ci < srch_date)) )



# Data Transformation
# =============
# convert date_time from string to date format in R as another variable
# called srch_date (removing the timestamp)
bkgs$srch_date <- as.POSIXct(bkgs$date_time, format = "%Y-%m-%d")

# Add a new feature called srch_mth to capture month only
# note as.Date does not support fractional dates
bkgs$srch_mth <- strftime(bkgs$srch_date, "%Y-%m")


# Already added srch_date and srch_mth from previous steps. Now we add more
# derived features
bkgsNew <- bkgs

# Create duration (srch_co - srch_ci) feature
bkgsNew$duration <- as.integer(bkgsNew$srch_co - bkgsNew$srch_ci)/60/60/24

# Create days_in_adv (srch_ci - srch_date) feature
bkgsNew$days_in_adv <- as.integer(bkgsNew$srch_ci - bkgsNew$srch_date)/60/60/24



# Illustrating the impact of normalisation
bkgsCl <- dplyr::select(bkgsNew,
                        -(X:date_time),
                        -(orig_destination_distance:user_id),
                        -(srch_ci:srch_co),
                        -is_booking,
                        -cnt,
                        -(srch_date:srch_mth))



# set seed to ensure results are reproduciable
set.seed(12345)

# IMPORTNAT to scale first before doing cluster analysis
bkgsCl <- as.data.frame(scale(bkgsCl))


# Using Factor approach
library(nFactors)
ev <- eigen(cor(bkgsCl)) # get eigenvalues
ap <- parallel(subject=nrow(bkgsCl),var=ncol(bkgsCl),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS, legend = FALSE) 

# from the plotnScree graph we can determine the no. of factors to extract
fit <- factanal(bkgsCl, factors=7, rotation="varimax", scores='regression')
fit$scores



# Using Principle Component Analysis
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
bkgsCl.pca <- prcomp(bkgsCl,
                     center = TRUE,
                     scale. = TRUE,
                     retx=TRUE) 

# summary of the PCA
summary(bkgsCl.pca)

# plot the scree plot
screeplot(bkgsCl.pca, type="lines")

# Show the various factor scores
bkgsCl.pca$x





# Examples of applying factorisation / PCA on visualisation
# ==========================================================
detach(package:nFactors, unload=TRUE)
# for standard k-means in R
library(ggfortify)
#km <- kmeans(bkgsCl, centers = 3, iter.max = 25, nstart = 5, algorithm="Hartigan-Wong")
km <- kmeans(bkgsCl, centers = 3, iter.max = 25, nstart = 5, algorithm="MacQueen")
autoplot(km, data = bkgsCl)



# K-means Clustering using BigMemory
library(bigmemory)
library(biganalytics)

# calculate the principle component first
fit <- principal(bkgsCl, nfactors=3, rotate="varimax", scores=TRUE)

# before doing k-means, we need to scale the data so that no one variable
# has an oversized loading
km <- bigkmeans(as.big.matrix(scale(bkgsCl)), 3, iter.max = 25, nstart = 5, dist = "euclid")
bkgsCl$kmCluster=factor(km$cluster)
centers=as.data.frame(km$centers)

ggplot(bkgsCl, aes(x=fit$scores[,1], y=fit$scores[,2], color=bkgsCl$kmCluster)) +
  geom_point()


# We can also plot the principle components in 3D space using the first
# 3 loadings - we tag each species with a colour and then use it to visualise
library(rgl)
values <- c("red", "green", "blue")
bkgsCl$colour <- values[bkgsCl$kmCluster]
plot3d(fit$scores[,1:3], col = bkgsCl$colour, size=5)
