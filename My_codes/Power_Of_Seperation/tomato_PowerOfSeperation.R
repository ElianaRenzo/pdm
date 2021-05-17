library(RSMLM)
setwd('/Users/Eliana/Documents/PDM/Codes/My_codes/Power_Of_Seperation')


folder_name <- 'DataForTomato/'
NumberOfRuns <- 30
distances <- c(60, 70, 75, 80, 85, 90, 95, 100, 105, 110, 120, 150, 200)

# search radius
tomatoR <- 40
# persistence threshold
tomatoThresh <- 6


for (dist in distances) {
  for (run in 0: (NumberOfRuns - 1)) {
    print(run)
    name = paste("sim_", run, "_dist_", dist, sep = "")
    data <- read.csv(paste(folder_name, name, ".csv", sep=""), header = TRUE)
  
    
    print("document telecharge")
  
    detectionList <- data
    #data.xy <- subset(data, select=c("V1", "V2"))
      
    coords <- as.matrix(detectionList[, c('x', 'y')]) # CORRIGER ICI CA MARCHE PAS JE COMPRENDS PAS POURQUOI
      
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
      
    filename <- paste("tomato_results/tomato_sim_", run, "_dist_", dist, ".csv", sep = "")
      
    write.csv(labels, filename)
  }
}

