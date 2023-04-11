library(cluster)
library(aricode)
library(rlist)
library(sfsmisc)
library(clustMixType)

KProto_process_varied_set <- function(ds, plot_characteristics, clusters, ds_idx) {
  best_ARI <- 0
  best_frame_result <- list()
  best_frame_tsne <- list()
  
  if(missing(clusters)) {
    clusters <- list(6, 6, 6)
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
      num_clust <- clusters[[d_idx]]
      result <- kproto(frame_data, num_clust)
      frameARI <-ARI(result$cluster, df$y)
      meanByFrames[frame_idx] <- frameARI
      if(frameARI > best_ARI) {
        best_ARI <- frameARI
        best_frame_result <- result
        tsne_obj <- Rtsne(frame_data, is_distance = FALSE)
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("X","Y")) %>%
          mutate(cluster = factor(result$cluster))
        best_frame_tsne <- tsne_data
        best_kproto_plots[[ds_idx]] <- data.frame(best_frame_tsne)
      }
    }
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
  title <- paste("K-prototype Results: ", ds_idx)
  g <- ggplot(mapping = aes(x = X, y = Y), best_kproto_plots[[ds_idx]]) + geom_point(aes(color = cluster)) + ggtitle(title)
  print(g)
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main=title)
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}
 
kproto_means <- list()
kproto_start = Sys.time()
for(idx in 1:DS_COUNT) {
  current_mean <- KProto_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]], idx)
  kproto_means[idx] <- current_mean 
}

kproto_time <- (Sys.time() - kproto_start)
kproto_overall_mean <- mean(unlist(kproto_means))
barplot(unlist(kproto_means),names=c(1,2,3,4,5,6,7),ylim=c(0,1), xlab="Test number", ylab="Mean ARI", main="K-Prototypes mean ARI by Test")