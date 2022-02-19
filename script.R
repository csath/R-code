#load data
wineData = read.csv("wine.csv", header = FALSE)
wineNames = read.csv("wine-names.csv", header = FALSE)

#check number of rows and number of columns
nrow(wineData)
ncol(wineData) 

#remove first column
head(wineData)
#V1 is the first column
wineData$V1 <- NULL 
#verify if the first column is removed
head(wineData)

#rename column names for better readability
names(wineData)[1] <- 'Alcohol'
names(wineData)[2] <- 'Malic acid'
names(wineData)[3] <- 'Ash'
names(wineData)[4] <- 'Alcalinity of ash'
names(wineData)[5] <- 'Magnesium'
names(wineData)[6] <- 'Total phenols'
names(wineData)[7] <- 'Flavanoids'
names(wineData)[8] <- 'Nonflavanoid phenols'
names(wineData)[9] <- 'Proanthocyanins'
names(wineData)[10] <- 'Color intensity'
names(wineData)[11] <- 'Hue'
names(wineData)[12] <- 'OD280/OD315 of diluted wines'
names(wineData)[13] <- 'Proline'

#remove rows if data is null
wineDataCleaned = wineData[rowSums(is.na(wineData)) == 0,]
nrow(wineData)
nrow(wineDataCleaned)
#there are no missing values

#check max values and remove is anything seesm suspisious
max(wineDataCleaned[1])
max(wineDataCleaned[2])
max(wineDataCleaned[3])
max(wineDataCleaned[4])
max(wineDataCleaned[5])
max(wineDataCleaned[6])
max(wineDataCleaned[7])
max(wineDataCleaned[8])
max(wineDataCleaned[9])
max(wineDataCleaned[10])
max(wineDataCleaned[11])
max(wineDataCleaned[12])
max(wineDataCleaned[13])

#max hue and proline value seems to be invalid, so remove them
wineDataCleaned = wineDataCleaned[wineDataCleaned$Hue < 1.71,]
wineDataCleaned = wineDataCleaned[wineDataCleaned$Proline < 1680,]
nrow(wineDataCleaned)

#Check the data summary
summary(wineData)
#plot different digrams to visualize data
plot(wineData)

plot(wineDataCleaned[,2:4])


kc = kmeans(wineDataCleaned[,2:4], 2)
kc

par(mfrow=c(1, 2))
plot(wineDataCleaned[,2:3], col=kc$cluster)
points(kc$centers[,1:2], col=1:2, pch=8, cex=2)
plot(wineDataCleaned[,3:4], col=kc$cluster)
points(kc$centers[,2:3], col=1:2, pch=8, cex=2)

par(mfrow=c(1, 2))
plot(wineDataCleaned[1], type="o", col="blue")
hist(wineDataCleaned[1], xlim=c(0,7), breaks=10, col="green")
table(wineDataCleaned[1]) 



# use euclidean distance method for clustering
dis = dist(wineDataCleaned[2:4], method="euclidean") 
hcwineAve = hclust(dis, method="ave") #Group average as similarity measure
par(mfcol=c(1,2))
plot(hcwineAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcwineAve, k=2, border="green")

# Cut the tree down since distance is too much
hcwineAveCut = cutree(hcwineAve, 2)
hcwineAveCut #List cluster membership

#iris data set 
dis = dist(iris[1:4], method="euclidean")
hcIrisAve = hclust(dis, method="ave")
plot(hcIrisAve, hang=-1, labels=iris$Species, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcIrisAve, k=3, border="blue")

hcIrisAveCut = cutree(hcIrisAve, k=3)
table(iris$Species, hcIrisAveCut)

par(mfcol=c(1,2))
plot(iris[,1:2], col=hcIrisAveCut, cex.main = 0.75)
plot(iris[,3:4], col=hcIrisAveCut, cex.main = 0.75)

# use wards method for clustering
hcIrisWard = hclust(dis, method="ward.D")
plot(hcIrisWard, hang=-1, labels=iris$Species, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcIrisWard, k=3, border="red") 


hcIrisWardCut = cutree(hcIrisWard, k=3)
table(iris$Species, hcIrisWardCut)

par(mfcol=c(1,2))
plot(iris[,1:2], col=hcIrisWardCut, cex.main = 0.75)
plot(iris[,3:4], col=hcIrisWardCut, cex.main = 0.75)
