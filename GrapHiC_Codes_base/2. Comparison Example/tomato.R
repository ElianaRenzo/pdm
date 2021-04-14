library(RSMLM)
setwd('/Users/Eliana/Documents/PDM/Codes/GrapHiC_Codes_base/2.\ Comparison\ Example/Data')

 

for (r_seed in 0:49) {
  for (i in 0:13) {
    name = paste("data", r_seed, "_", i, sep = "")
    data <- read.csv(paste(name, ".csv", sep=""), header = FALSE)
    # search radius
    tomatoR <- 8
    # persistence threshold
    tomatoThresh <- 5
    # cluster data using ToMATo
    labels <- clusterTomato(data, tomatoR, tomatoThresh)
    write.csv(labels, paste("tomato", name, ".csv", sep=""))
  }
}




