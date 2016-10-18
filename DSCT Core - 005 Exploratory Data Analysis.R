# R Script for Exploratory Data Analysis

# require libraries
require(pastecs)
require(dplyr)
require(gplots)
require(RColorBrewer)
require(moments)  # package for skewness function
require(ggdendro) # package for easy creation of dendrograms for ggplot
require(biganalytics) # clustering and kmeans on a large dataset requires this package to handle large datasets

# load libraries
library(pastecs)
library(dplyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(moments)
library(ggdendro)
library(biganalytics)
library(cluster)

# set working directory
setwd("D:\\Projects\\Data Science Competency\\Curriculum\\00 Core\\src\\")


# read csv data file
bkgs <- read.csv2("ypedia\\Sample.csv", header = TRUE, sep=",",
                  stringsAsFactors = TRUE)


# convert date_time from string to date format in R as another variable
bkgs$srch_date <- as.POSIXct(bkgs$date_time, format = "%Y-%m-%d")

# convert date_time from string to datetime format in R
bkgs$date_time <- as.POSIXct(bkgs$date_time, format = "%Y-%m-%d %H:%M:%S")

# convert srch_ci and srch_co from string to date
bkgs$srch_ci <- as.POSIXct(bkgs$srch_ci, format = "%Y-%m-%d")
bkgs$srch_co <- as.POSIXct(bkgs$srch_co, format = "%Y-%m-%d")




# Data Exploration

# Summary of the Data
# view a summary of the raw data
  < ... enter your code ... >

  
# looking at summary statistics for the entire data set
  < ... enter your code ... >
  
  
# looking at summary statistics for srch_adult_cnt and srch_children_cnt
  < ... enter your code ... >
  
  
# using the pastecs package to generate descriptive statistics
  < ... enter your code ... >
  
  
# using moments package to show kurtosis and skewness
# kurtosis is a measure of the peakedness of the data distribution.
# Negative kurtosis would indicates a flat data distribution, which is said to be platykurtic.
# Positive kurtosis would indicates a peaked distribution, which is said to be leptokurtic
  < ... enter your code ... >

  
# Intuitively, the skewness is a measure of symmetry.
# As a rule, negative skewness indicates that the mean of the data values is less than the median,
# and the data distribution is left-skewed.
# Positive skewness would indicates that the mean of the data values is larger than the median,
# and the data distribution is right-skewed. 
  < ... enter your code ... >
  

# no. of distinct country, region and city the users are from
  < ... enter your code ... >
  
  
# how many records have srch_adults_cnt == 0? What is the percentage?
# In US minors can travel alone
  < ... enter your code ... >
  
  
# Method 1:
# any searches with BOTH srch_adults_cnt and srch_children_cnt == 0?
# may be erronous data
  < ... enter your code ... >
  
  
# Method 2:
# An alternate way of presenting if there are  any searches with BOTH srch_adults_cnt
# and srch_children_cnt == 0? may be erronous data
  < ... enter your code ... >
  
  
# Method 3:
# tabulate to see of there are any records with both adults and children == 0
  < ... enter your code ... >
  
  
# see the specific statistics for the search count for adults and children
# (include removing 0 records)
  < ... enter your code ... >
  
  
# find the unique counts for each column in the data (except X and date_time)
  < ... enter your code ... >
  



##################################################################################
# Exploring the shape of the data
##################################################################################
  

# bar plot of booking transactions by continent
  < ... enter your code ... >
  


# histogram plot of srch_adults_cnt overlayed with the density plot
  < ... enter your code ... >
  


# histogram plot of srch_adults_cnt overlayed with the density plot
  < ... enter your code ... >
  


# view the histogram of the following attributes
# this code uses reshape2 to transform the data for easier rendering using histogram
library(reshape2)
d <- melt(bkgs[, c('channel',
                   'is_booking',
                   'is_mobile',
                   'orig_destination_distance',
                   'srch_rm_cnt',
                   'srch_adults_cnt',
                   'srch_children_cnt')],
          id.vars = NULL)
ggplot(d, aes(x = as.numeric(value))) +
  geom_histogram(aes(fill=variable), alpha=0.7, position = "identity") +
  facet_wrap(~ variable, scales = "free")
remove(d)




# distribution of the no. of booking attempts - need to find for each customer
# how many bookings made, group by the no.of bookings and then count the no. of customers
# within each group
  < ... enter your code ... >
  


# distribution of the booking rate
  < ... enter your code ... >


# Add a new feature called srch_mth to capture month only
# note as.Date does not support fractional dates
bkgs$srch_mth <- strftime(bkgs$srch_date, "%Y-%m")



# distribution of the no. of searches by day
  < ... enter your code ... >
  


# strong correlation at the day level between searches and bookings
  < ... enter your code ... >


  
# distribution of the no. of searches by year - month
bkgs %>%
  group_by(s = as.factor(as.character(srch_mth))) %>%
  summarise(srch_cnt = n(), bkg_cnt = sum(is_booking)) %>%
  melt(.) %>%
  ggplot(aes(x=s, y=value)) + 
  geom_histogram(aes(fill=variable), binwidth = 1.0, stat = "identity", alpha = 0.7) +
  labs(title="Distribution of Searches by Date") +
  labs(x="Date", y="No. of Search Attempts") +
  facet_wrap(~ variable, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# strong correlation at the month level between searches and bookings
bkgs %>%
  group_by(s = as.factor(as.character(srch_mth))) %>%
  summarise(srch_cnt = n(), bkg_cnt = sum(is_booking)) %>%
  select(srch_cnt, bkg_cnt) %>%
  corr(.)




# using scatter plots to identify possible outliers
labels <- c("0" = "Continent 0", "1" = "Continent 1", 
            "2" = "Continent 2", "3" = "Continent 3",
            "4" = "Continent 4", "5" = "Continent 5",
            "6" = "Continent 6")
  < ... enter your code ... >
  


# Spinning 3d Scatterplot
  < ... enter your code ... >
  



# determine the correlation between all the variables (except non-numeric / date variables)
cor_mat <- data.matrix(cor(dplyr::select(bkgs,
                                         #                                  -(X:site_name),
                                         -(X:date_time),
                                         -(orig_destination_distance:user_id),
                                         -(srch_ci:srch_co),
                                         -(srch_date:srch_mth))))
# round up all the values in the correlation matrix
cor_mat <- apply(cor_mat, 1, function(x) {round(x, digits=3)})


# set the colour pallet
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(-1,-0.01,length=100), # for red
               seq(0,0.8,length=100), # for yellow
               seq(0.81,1,length=100)) # for green

# creates a 13 x 8 inch image
png("./heatmaps_in_r.png", # create PNG for the heat map        
    width = 13*300,        # 13 x 300 pixels
    height = 8*300,        # 8 x 300 pixels
    res = 300,             # 300 pixels per inch
    pointsize = 10)        # smaller font size

# plot heatmap
heatmap.2(cor_mat,
          cellnote = cor_mat,   # same data set for cell labels
          main = "Correlation Matrix", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Rowv=TRUE)            # turn off column clustering

dev.off()               # close the PNG device





# Data Validation

# Are there any records where the srch_co is < srch_ci?
  < ... enter your code ... >

  
# Are there any records where the srch_ci < srch_date(date_time)
  < ... enter your code ... >

  
# Exclude these data from analysis (take note of the boolean logic)
  < ... enter your code ... >
  

############################################################################
# Feature Engineering
############################################################################
# Already added srch_date and srch_mth from previous steps. Now we add more
# derived features

# Create duration (srch_co - srch_ci) feature
  < ... enter your code ... >
  
# Create days_in_adv (srch_ci - srch_date) feature
  < ... enter your code ... >
  
# By adding new features, new outliers can be identified using a scatterplot
  < ... enter your code ... >
  

# Looking at the mean booking rate by segments and compare with the overall
# booking mean

catg_list <- c(
  "site_name",
  "posa_continent",
  "user_location_country",
  "user_location_region",
  "user_location_city",
  "channel",
  "srch_destination_id",
  "srch_destination_type_id",
  "hotel_continent",
  "hotel_country",
  "hotel_market",
  "hotel_cluster"
)


#
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# Create multiple plots to visually compare mean booking rate across all the
# different possible "segments"
tableMeans <- function(x, y, overall_mean)
{
  z <- x %>%
    group_by_(s = y) %>%
    summarise(mean = mean(is_booking)) %>%
    ggplot(aes(x=s, y=mean)) +
      geom_histogram(aes(fill=mean), stat = "identity", alpha = 0.7) +
    #      scale_fill_continuous(low="light blue", high="dark blue", breaks=overall_mean) +
      scale_fill_gradient2(low="red", midpoint = overall_mean, mid = "white") + 
      labs(x=y)
  
  return(z)
}

plots <- lapply(catg_list, tableMeans, x=bkgsNew, mean(bkgsNew$is_booking))

# display the first 3 plots in a matrix layout
multiplot(plotlist = plots[1:3],
          layout = matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))
          





#####################################################################
# Some EDA Clustering Techniques
#####################################################################

bkgsCl <- dplyr::select(bkgsNew,
                        -(X:date_time),
                        -(orig_destination_distance:user_id),
                        -(srch_ci:srch_co),
                        -cnt,
                        -(srch_date:srch_mth))


# Hierarichal Clustering
# if we use the whole 100k records, the calculation will take too long
# We can use the createDataPartition function in the caret package to do sampling
# based on the class distribution of the is_booking field
library(caret)
library(Rclusterpp)

set.seed(12345)
index <- createDataPartition(bkgsCl$is_booking, p = .001,
                                  list = FALSE,
                                  times = 1)

bkgsHCSamp <- bkgsCl[index]
h <- Rclusterpp.hclust(bkgsHCSamp, method="average", distance="euclidean") 
plot(h)




# set seed to ensure results are reproduciable
set.seed(12345)

# IMPORTNAT to scale first before doing cluster analysis
bkgsCl <- as.data.frame(scale(bkgsCl))


# using standard k-means in R (scaled)
library(ggfortify)
km <- kmeans(bkgsCl, 3, iter.max = 25, nstart = 5, algorithm="Hartigan-Wong")
autoplot(km, data = bkgsCl)


# K-means Clustering using BigMemory
library(bigmemory)
library(biganalytics)

# calculate the principle component first
library(psych)
fit <- principal(bkgsCl, nfactors=3, rotate="varimax", scores=TRUE)


# before doing k-means, we need to scale the data so that no one variable
# has an oversized loading
  < ... enter your code ... >

  
# We can also plot the principle components in 3D space using the first
# 3 loadings - we tag each species with a colour and then use it to visualise
  < ... enter your code ... >
  