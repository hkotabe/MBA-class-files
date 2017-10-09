###########################################
### Perceptual Mapping: Example Dataset ###
###########################################

rm(list=ls())
graphics.off()

# Load Brand example dataset and brief data check
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)
str(brand.ratings)
summary(brand.ratings)

# Scale brand ratings so that Means = 0 and SDs = 1
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
apply(brand.sc[,-ncol(brand.sc)], 2, mean)
apply(brand.sc[,-ncol(brand.sc)], 2, sd)

# Create visual correlation plot to see which attributes are associated
#install.packages("corrplot") # Need to uncomment this if "corrplot" is not installed yet
library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order="hclust")
graphics.off()

# Aggregate data by taking mean attribute values across brands
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1]
brand.mean

# Create heatmap visually depicting the mean attribute values across brands
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
          col=brewer.pal(9, "GnBu"), trace="none", key=T, dend="none",
          main="\n\n\n\n\nBrand attributes")

# Principal component analysis to reduce attribute dimensions
brand.mu.pc <- prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)

# Create the perceptual map!
biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5, 1))

# Suppose you want to move in the direction of Brand C 
brand.mean["c", ] - brand.mean["e", ]

# Suppose you want to fill the competitive gap between Brands B, C, F, and G
colMeans(brand.mean[c("b", "c", "f", "g"), ]) - brand.mean["e", ]


###############################################
### Perceptual Mapping: SKK GSB Car Dataset ###
###############################################

rm(list=ls())
graphics.off()

# Load Brand example dataset and brief data check
brand.ratings <- read.csv("https://raw.githubusercontent.com/hkotabe/MBA-class-files/master/car-survey-pm.csv")
brand.ratings <- subset(brand.ratings, id>201700) # only 2017 Marketing Research students
head(brand.ratings)
str(brand.ratings)
summary(brand.ratings)

# Scale brand ratings so that Means = 0 and SDs = 1
brand.sc <- brand.ratings
brand.sc[, -c(1,2)] <- scale(brand.ratings[, -c(1,2)])
apply(brand.sc[,-c(1,2)], 2, mean, na.rm=T)
apply(brand.sc[,-c(1,2)], 2, sd, na.rm=T)

# Create visual correlation plot to see which attributes are associated
#install.packages("corrplot") # Need to uncomment this if "corrplot" is not installed yet
library(corrplot)
corrplot(cor(brand.sc[, -c(1,2)], use="pairwise.complete.obs"), order="hclust")
graphics.off()

# Aggregate data by taking mean attribute values across brands
brand.mean <- aggregate(. ~ Brand, data=brand.sc[, -1], mean)
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1]
brand.mean

# Create heatmap visually depicting the mean attribute values across brands
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
          col=brewer.pal(9, "GnBu"), trace="none", key=T, dend="none",
          main="\n\n\n\n\nBrand attributes")

# Principal component analysis to reduce attribute dimensions
brand.mu.pc <- prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)

# Create the perceptual map!
biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5, 1))

# Suppose you work at Hyundai and you want to want to move in the direction of the ideal "next car" for SKK MBA students
brand.mean["Hyundai", ] - brand.mean["Next car", ]





