library(RSMLM)
setwd('/Users/Eliana/Documents/PDM/Codes/My_codes/Data')

folder_name <- 'different_densities/'
ID <- '4'


# search radius
tomatoR <- 31 # Selon juliette, mettre plutot un peu plus que 30 (triche)
# persistence threshold
tomatoThresh <- 6


for (i in 0:29) {
  data <- read.csv(paste(folder_name, 'simulated_SMLM_', ID, '_', i, '.csv', sep = ""))
  
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
  
  filename <- paste("tomato_results/tomato_result_", ID, "_", i , ".csv", sep = "")
  
  write.csv(labels, filename)
}





