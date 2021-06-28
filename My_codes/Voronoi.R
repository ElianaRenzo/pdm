library("dbscan")
library("deldir")
library("igraph")
library("geometry")
library("ggplot2")
library("randomcoloR")
library("RSMLM")

#' 2D Voronoi tesselation based clustering
#' 
#' A Voronoi tesselation is contructed from the detection coordinates. Detection density is defined as the inverse of tile area, or mean first rank tile area.
#' Detections with density below a specified threshold are removed from the dual Delaunay triangulation and the connected components of this graph are used to define clusters. 
#' Implementation is for 2D data only. \cr \cr
#' Levet, Florian, et al. "SR-Tesseler: a method to segment and quantify localization-based super-resolution microscopy data." Nature methods 12.11 (2015): 1065. \cr
#' Andronov, Leonid, et al. "ClusterViSu, a method for clustering of protein complexes by Voronoi tessellation in super-resolution microscopy." Scientific reports 6 (2016): 24084.
#' 
#' @param coords A matrix containing coordinates of the detections.
#' @param threshold Threshold for density.
#' @param densityChoice Choice of density calculation method. 0: inverse tile area, 1: inverse mean first rank tile area
#' @return Vector containing the cluster indices for each detection, a cluster index of zero refers to noise
#'
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


setwd('/Users/Eliana/Documents/PDM/Codes/My_codes/Deviations_from_standard/')

folder_name <- 'non_uniform_noise/'
ID <- 6

NumberOfSimulations <- 30


density_factor <- 2

for (i in 0:(NumberOfSimulations - 1)) {

  filename <- paste('simulated_SMLM_', ID, '_', i, sep = "")
  print(filename)
  data <- read.csv(paste('Data/', folder_name, filename, '.csv', sep = ""))
    
    detectionList <- data
    
    cat('simulated_SMLM_', ID, '_',  i)
    
    coords <- as.matrix(detectionList[, c('x', 'y')])
    N <- dim(coords)[1]
    
    area <- (max(coords[,1])- min(coords[,1])) * (max(coords[,2])- min(coords[,2]))
    mean_density <- N/area
    res <- clusterVoronoi(coords, density_factor * mean_density, 1)
    
    plotClusterScatter(coords, res)
    
    filename <- paste("SRT_analysis/voronoi_result_", ID, "_", i , ".csv", sep = "")
    
    write.csv(res, filename)
}
plotClusterScatter(coords, res)



folder_name = 'different_sizes/'

# Tests pour les faire 1 à la fois. 
data <- read.csv(paste('Data/', folder_name, 'simulated_SMLM_', 5, '_', 0, '.csv', sep = ""))

detectionList <- data

coords <- as.matrix(detectionList[, c('x', 'y')])
N <- dim(coords)[1]

area <- (max(coords[,1])- min(coords[,1])) * (max(coords[,2])- min(coords[,2]))
mean_density <- N/area
res <- clusterVoronoi(coords, 2*mean_density, 1)

plotClusterScatter(coords, res)

filename <- paste("SRT_analysis/voronoi_result_", ID, "_", i , ".csv", sep = "")

write.csv(res, filename)












