library(RSMLM)

setwd('/Users/Eliana/Documents/PDM/Codes/My_codes/Deviations_from_standard/')


folder_name <- 'different_sizes/'
ID <- '5'


# search radius
tomatoR <- 35 # Selon juliette, mettre plutot un peu plus que 30 (triche)
# persistence threshold
tomatoThresh <- 5

NumberOfSimulations <- 30



for (i in 0:(NumberOfSimulations - 1)) {
  
  filename <- paste('simulated_SMLM_', ID, '_', i, sep = "")
  print(filename)
  data <- read.csv(paste('Data/', folder_name, filename, '.csv', sep = ""))
  
  detectionList <- data
  
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
  
  filename <- paste("Tomato_analysis/tomato_result_", ID, "_", i , ".csv", sep = "")
  
  write.csv(labels, filename)
}





