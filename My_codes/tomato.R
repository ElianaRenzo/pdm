library(RSMLM)
setwd('/Users/Eliana/Documents/PDM/Codes/My_codes')

data <- read.csv('standard_zoom_for_tomato.csv')
# search radius
tomatoR <- 15
# persistence threshold
tomatoThresh <- 6

detectionList <- data
#data.xy <- subset(data, select=c("V1", "V2"))

coords <- as.matrix(detectionList[, c('x', 'y')])

# cluster data using ToMATo
labels <- clusterTomato(coords, tomatoR, tomatoThresh)
#write.csv(labels, paste("tomato", name, ".csv", sep=""))

# Tomato result
plotClusterScatter(coords, labels)

# GT
plotClusterScatter(coords, detectionList$labels_1)

numClustersTom <- sum(unique(labels) > 0)
numClustersGT <- sum(unique(detectionList$labels_1) > 0)

print(paste('ToMATo found', numClustersTom, 'clusters in the dataset. The ground truth number is', numClustersGT))

tomatoDiag <- tomatoDiagram(coords, tomatoR)
plotTomatoDiagram(tomatoDiag, tomatoThresh)

name <- "_test_saving_results"

#write.csv(labels, paste("tomato", name, ".csv", sep=""))





