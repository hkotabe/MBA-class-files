rm(list=ls())
graphics.off()

###############################################
### Perceptual Mapping: Car Example Dataset ###
###############################################

# Load "car" example dataset
#install.packages("plfm") # If you do not have this installed you need to "un-comment" it to install this package before loading it with library()
library(plfm)
data(car)
str(car)

# Frequency ratings across all car models per perception attribute
car$freq1

# Produce perceptual map
library(anacor)
ca <- anacor(car$freq1)
plot(ca, plot.dim=c(1,2), conf=NULL, main="GSB Car Perceptions Map", xlab="Economy <--> Luxury", ylab="City/Stylish <--> Outdoor/Versatile")


###########################################
### Perceptual Mapping: Car GSB Dataset ###
###########################################

rm(list=ls())
graphics.off()

# First, set working directory to source file location (Session -> Set Working Directoy -> Source File Location)

# Quick data check
dat <- read.csv("https://raw.githubusercontent.com/hkotabe/MBA-class-files/master/car-survey-pm.csv") 
dat <- subset(dat, id>201700) # only 2017 Marketing Research students
str(dat)
head(dat)
cor(dat[,-c(1:2)], use="pairwise.complete.obs")

# Aggregate data by Brand taking the mean of all participant ratings
dat.agg <- aggregate(dat[,-c(1,2)], by=list(Brand=dat$Brand), FUN=mean, na.rm=TRUE)
dat.agg2 <- dat.agg[,-1]
rownames(dat.agg2) <- dat.agg[,1]

# Transpose data frame so Brands are rows and Attributes are columns
dat.agg2 <- as.data.frame(t(dat.agg2))

# Produce perceptual map
#install.packages("anacor") # If you do not have this installed you need to "un-comment" it to install this package before loading it with library()
library(anacor)
ca <- anacor(dat.agg2)
ca
plot(ca, plot.dim=c(1,2), conf=NULL, main="GSB Car Perceptions Map", 
     xlab="Fuel Efficient/Green <--> Prestige/Luxury", ylab="Performance <--> Safety ???")