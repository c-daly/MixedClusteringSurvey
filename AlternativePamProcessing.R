library(cluster)
library(aricode)
library(rlist)
library(sfsmisc)

PAM_process_varied_set <- function(ds, plot_characteristics, clusters) {
  if(missing(clusters)) {
    clusters <- list(3,3,3)
  }
  k <- 1
  meanAcrossFrames <- list()
  datasetMean <- list()
  boxplots <- list()
  # iterate over all datasets (N_VARIES, CONT_VARIES, etc)
  #ds <- D_CONTVARIES
  # Each major dataset has three variations within it
  # each with 1000 sub-datasets
  for (d_idx in 1:length(ds))
  {
    d_cols = ds[[d_idx]][[2]]
    frames <- ds[[d_idx]][[1]]
    i <- 1
    meanByFrames <- list()
    
    # each of the sub-datasets is itself
    # a dataframe
    for (frame_idx in 1:length(frames))
    {
      df = data.frame(frames[[frame_idx]])
      frame_data <- subset(df, select = -c(y))
      num_clusters <- clusters[[d_idx]]
      dist <- daisy(frame_data, metric = "gower")
      result <- pam(dist, k = num_clusters, cluster.only = FALSE)
      frameARI <-ARI(result$clustering, df$y)
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
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main="PAM (GOWER) Results")
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}

pam_means <- list()
pam_start = Sys.time()
for(idx in 1:DS_COUNT) {
  current_mean <- PAM_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]])
  pam_means[idx] <- current_mean
}

pam_time <- (Sys.time() - pam_start)
pam_overall_mean <- mean(unlist(pam_means))
