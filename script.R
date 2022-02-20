####### DATA LOADING ######
############################
library(purrr)
library(tidyr)
library(ggplot2)
library(plyr)
library(factoextra)
library(fpc)
library(dbscan)

wineDataOrg = read.csv("wine.csv", header = FALSE)
#rename column names for better readability
names(wineDataOrg)[1] <- 'Type'
names(wineDataOrg)[2] <- 'Alcohol'
names(wineDataOrg)[3] <- 'Malic acid'
names(wineDataOrg)[4] <- 'Ash'
names(wineDataOrg)[5] <- 'Alcalinity of ash'
names(wineDataOrg)[6] <- 'Magnesium'
names(wineDataOrg)[7] <- 'Total phenols'
names(wineDataOrg)[8] <- 'Flavanoids'
names(wineDataOrg)[9] <- 'Nonflavanoid phenols'
names(wineDataOrg)[10] <- 'Proanthocyanins'
names(wineDataOrg)[11] <- 'Color intensity'
names(wineDataOrg)[12] <- 'Hue'
names(wineDataOrg)[13] <- 'OD280/OD315 of diluted wines'
names(wineDataOrg)[14] <- 'Proline'

wineData <- wineDataOrg

#check number of rows and number of columns
nrow(wineData)
ncol(wineData) 

#now have a look at original data
summary(wineData)
plot(wineData)
wineData %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


####### DATA CLEANING ######
############################

#remove rows if data is null
wineDataCleaned = wineData[rowSums(is.na(wineData)) == 0,]

#remove duplicate rows if available
unique(wineDataCleaned)

#remove outliers
#first check if any outliers available using boxplot
boxplot(wineDataCleaned)

#when plottng the box plot you can see there are outliers in 2,3,4,5,9,10,11 columns, let's remove them
#wineDataOutlierCleaned <- wineDataCleaned
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,2] %in% boxplot.stats(wineDataOutlierCleaned[,2])$out),]
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,3] %in% boxplot.stats(wineDataOutlierCleaned[,3])$out),]
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,4] %in% boxplot.stats(wineDataOutlierCleaned[,4])$out),]
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,5] %in% boxplot.stats(wineDataOutlierCleaned[,5])$out),]
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,9] %in% boxplot.stats(wineDataOutlierCleaned[,9])$out),]
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,10] %in% boxplot.stats(wineDataOutlierCleaned[,10])$out),]
#wineDataOutlierCleaned <- wineDataOutlierCleaned[-which(wineDataOutlierCleaned[,11] %in% boxplot.stats(wineDataOutlierCleaned[,11])$out),]
#boxplot(wineDataOutlierCleaned)

#find absolute value of z-score for each value in each column
z_scores <- as.data.frame(sapply(wineDataCleaned, function(wineDataCleaned)
  (abs(wineDataCleaned-mean(wineDataCleaned))/sd(wineDataCleaned))))
head(z_scores)
#only keep rows in dataframe with all z-scores less than absolute value of 3
wineDataOutlierCleaned <- wineDataCleaned[!rowSums(z_scores > 3), ]
dim(wineDataOutlierCleaned)


####### REMOVE FIRST COLUMN ######
############################
wineDataOutlierCleanedBeforeScaling <- wineDataOutlierCleaned
#remove first column before scaling
wineDataOutlierCleaned$Type <- NULL

# Scaling the dataset
wineDataCleanedAndScaled <- scale(wineDataOutlierCleaned[-1])

# plot the data again
boxplot(wineDataCleanedAndScaled)


#K-means clustering
km <- kmeans(wineDataCleanedAndScaled, 3)
km
plot(wineDataCleanedAndScaled, col=km$cluster+1, main="K-means")
table(wineDataOutlierCleanedBeforeScaling$Type, km$cluster)

# do heirarchicle clustering
# use euclidean distance method for clustering
dis = dist(wineDataCleanedAndScaled, method="euclidean") 
hcwineAve = hclust(dis, method="ave") #Group average as similarity measure
par(mfcol=c(1,1))
plot(hcwineAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcwineAve, k=3, border="green")

# Cut the tree down since distance is too much
hcwineAveCut = cutree(hcwineAve, 3)
hcwineAveCut #List cluster membership

table(wineDataOutlierCleanedBeforeScaling$Type, hcwineAveCut)


#do db scan clustering
set.seed(123456789)  # Setting seed
kNNdistplot(wineDataCleanedAndScaled, k = 30)
abline(h=4.35, col = "red", lty=2)

Dbscan_cl <- dbscan(wineDataOutlierCleanedBeforeScaling, eps = 4.35, MinPts = 30)
Dbscan_cl
Dbscan_cl$cluster

plot(wineDataCleanedAndScaled, col=Dbscan_cl$cluster+1, main="DBSCAN")



