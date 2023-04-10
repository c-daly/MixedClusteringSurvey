#library(cluster)
#library(aricode)
#library(rlist)
#library(sfsmisc)
set.seed(1680) # for reproducibility

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

best_ARI <- 0
best_pam_results <- list()

best_frame_tsne <- list()
best_tsne <- list()
PAM_process_varied_set <- function(ds, plot_characteristics, clusters, ds_idx) {
  if(missing(clusters)) {
    clusters <- list(3,3,3)
  }
  k <- 1
  meanAcrossFrames <- list()
  datasetMean <- list()
  boxplots <- list()
  best_frame_result <- list()
  # iterate over all datasets (N_VARIES, CONT_VARIES, etc)
  #ds <- D_CONTVARIES
  # Each major dataset has three variations within it
  # each with 1000 sub-datasets
  for (d_idx in 1:length(ds))
  {
    best_ARI <- 0
    d_cols = ds[[d_idx]][[2]]
    frames <- ds[[d_idx]][[1]]
    i <- 1
    meanByFrames <- list()
    
    # each of the sub-datasets is itself
    # a dataframe
    for (frame_idx in 1:length(frames))
    {
      best_frame_tsne <- list()
      df = data.frame(frames[[frame_idx]])
      frame_data <- subset(df, select = -c(y))
      num_clusters <- clusters[[d_idx]]
      dist <- daisy(frame_data, metric = "gower")
      result <- pam(dist, k = num_clusters, cluster.only = FALSE, diss = TRUE)
      frameARI <-ARI(result$clustering, df$y)
      if(frameARI > best_ARI) {
        best_frame_result <- result    
        best_ARI <- frameARI
      }
      meanByFrames[frame_idx] <- frameARI
    }
    tsne_obj <- Rtsne(dist, is_distance = TRUE)
    tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X","Y")) %>%
    mutate(cluster = factor(result$clustering)) 
    best_frame_tsne <- tsne_data

    best_pam_plots[[ds_idx]] <- data.frame(best_frame_tsne)
    frameMean <- mean(unlist(meanByFrames))
    meanAcrossFrames[d_idx] <- frameMean
    boxplots[[d_idx]] = meanByFrames
  }
  mean1 <- mean(unlist(boxplots[1]))
  mean2 <- mean(unlist(boxplots[2]))
  mean3 <- mean(unlist(boxplots[3]))
  current_mean <- mean(c(mean1, mean2, mean3))
  names <- plot_characteristics$names
  ylab <- plot_characteristics$ylab
  xlab <- plot_characteristics$xlab
  title <- paste("Pam Results (Gower): ", ds_idx)
  g <- ggplot(mapping = aes(x = X, y = Y), best_pam_plots[[ds_idx]]) + geom_point(aes(color = cluster)) + ggtitle(title)
  print(g)
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main="PAM (GOWER) Results")
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}

pam_means <- list()
pam_start = Sys.time()

for(idx in 1:DS_COUNT) {
  current_mean <- PAM_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]], idx)
  pam_means[idx] <- current_mean
}

pam_time <- (Sys.time() - pam_start)
pam_overall_mean <- mean(unlist(pam_means))
barplot(unlist(pam_means),names=c(1,2,3,4,5,6,7), ylim=c(0,1), main="PAM mean ARI by Test")