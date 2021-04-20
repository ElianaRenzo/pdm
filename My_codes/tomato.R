library(RSMLM)
setwd('/Users/Eliana/Documents/PDM/Codes/My_codes')

data <- read.csv('simulated_data.csv')
# search radius
tomatoR <- 18
# persistence threshold
tomatoThresh <- 5

detectionList <- data
#data.xy <- subset(data, select=c("V1", "V2"))

coords <- as.matrix(detectionList[, c('x', 'y')])

# cluster data using ToMATo
labels <- clusterTomato(coords, tomatoR, tomatoThresh)
#write.csv(labels, paste("tomato", name, ".csv", sep=""))

plotClusterScatter(coords, labels)





