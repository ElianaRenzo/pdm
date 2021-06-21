library("dbscan")
library("deldir")
library("igraph")
library("geometry")
library("ggplot2")
library("randomcoloR")
library("RSMLM")

clusterVoronoi <- function(coords, threshold, densityChoice) {
  
  numDimensions <- dim(coords)[2]
  if (numDimensions != 2) {
    stop('Coordinates should be 2D')
  } 
  if (sum(duplicatedxy(coords[, 1], coords[, 2])) > 0) {
    stop("Coords list should not contain duplicated points")
  } 
  
  numDetections <- dim(coords)[1]
  
  # compute veronoi tesselation using deldir library
  vtess <- deldir(coords[, 1], coords[, 2])
  
  # retrieve area of Voronoi tiles
  tileAreas <- vtess$summary$dir.area
  
  
  # create igraph object corresponding to the Delaunay triangulation 
  g <- make_empty_graph(n = numDetections, directed = FALSE)
  g <- add_edges(g, c(rbind(vtess$delsgs$ind1, vtess$delsgs$ind2)))
  
  # to hold density estimate
  density <- rep(0, numDetections)
  
  # calculate neighbours for each detection in the graph
  neigh <- adjacent_vertices(g, seq(1, numDetections, by = 1))
  
  # use first rank tile area for density calculation
  if (densityChoice == 1) {
    for (i in 1 : numDetections) {
      density[i] <- (1 + length(neigh[[i]])) / (tileAreas[i] + sum(tileAreas[neigh[[i]]]))
    }
    # use tile area for density calculation
  } else if (densityChoice == 0) {
    density <- 1 / tileAreas
  }
  # delete tiles with area larger than specified maxiumum
  tileIndices <-  density > threshold
  g <- delete_vertices(g, which(!tileIndices))
  
  # compute the connected components of the graph
  ccs <- components(g)
  # extract cluster indices as vector with zero value representing no cluster assignment
  clusterIndices <- rep(0, numDetections)
  clusterIndices[which(tileIndices)] <- ccs$membership
  
  return(clusterIndices)
  
}


library(RSMLM)
setwd('/Users/Eliana/Documents/PDM/Codes/My_codes/Power_Of_Seperation')


folder_name <- 'DataForTomato/'
NumberOfRuns <- 30


# density factor - threshold for object 
density_factor <- 0.5

default_area <- (30+30) * (30+ 105 + 30)

distances <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105)



for (dist in distances) {
  for (run in 0: (NumberOfRuns - 1)) {
    print(run)
    name = paste("dist_", dist, "_sim_", run, sep = "")
    data <- read.csv(paste(folder_name, name, ".csv", sep=""), header = TRUE)
    
    detectionList <- data
    coords <- as.matrix(detectionList[, c('x', 'y')])
    
    N <- dim(coords)[1]
    
    #area <- (max(coords[,1])- min(coords[,1])) * (max(coords[,2])- min(coords[,2]))
    mean_density <- N/default_area
  
    res <- clusterVoronoi(coords, density_factor * mean_density, 1)
    
    plotClusterScatter(coords, res)
  
    filename <- paste("voronoi_analysis/denstiy_factor_05/voronoi_dist_", dist, "_sim_", run, ".csv", sep = "")
  
    write.csv(res, filename)
  }
}






