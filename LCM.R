library(VarSelLCM)
library(cluster)
library(aricode)
library(rlist)
library(sfsmisc)

LCM_process_varied_set <- function(ds, plot_characteristics, clusters) {
  if(missing(clusters)) {
    clusters <- list(6,6,6)
  }
  k <- 1
  meanAcrossFrames <- list()
  datasetMean <- list()
  boxplots <- list()
  for (d_idx in 1:length(ds))
  {
    d_cols = ds[[d_idx]][[2]]
    frames <- ds[[d_idx]][[1]]
    i <- 1
    meanByFrames <- list()
    
    for (frame_idx in 1:length(frames))
    {
      df = data.frame(frames[[frame_idx]])
      frame_data <- subset(df, select = -c(y))
      num_clusters <- clusters[[d_idx]]
      result <- VarSelCluster(frame_data, num_clusters, nbcores=6, vbleSelec=FALSE, crit.varsel="BIC")
      #result <- VarSelCluster(frame_data, 6, nbcores=6)
      frameARI <-ARI(fitted(result), df$y)
      meanByFrames[frame_idx] <- frameARI
    }
    
    frameMean <- mean(unlist(meanByFrames))
    meanAcrossFrames[d_idx] <- frameMean
    boxplots[[d_idx]] = meanByFrames
  }
  mean1 <- mean(unlist(boxplots[1]))
  mean2 <- mean(unlist(boxplots[2]))
  mean3 <- mean(unlist(boxplots[3]))
  current_mean <- mean(mean1, mean2, mean3)
  names <- plot_characteristics$names
  ylab <- plot_characteristics$ylab
  xlab <- plot_characteristics$xlab
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main="LCM Results")
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}
means <- list()
lcm_start = Sys.time()
for(idx in 1:DS_COUNT) {
  current_mean <- LCM_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]])
  list[idx] <- current_mean
  
}

lcm_time <- (Sys.time() - kamila_start)
lcm_mean <- mean(unlist(means))
