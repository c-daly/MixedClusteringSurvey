library(cluster)
library(clustMD)
library(aricode)
library(rlist)
library(sfsmisc)
library(Rmixmod)

mixmod_process_varied_set <- function(ds, plot_characteristics, clusters, ds_idx) {
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
      result <- mixmodCluster(frame_data, num_clust)
      frameARI <-ARI(result@bestResult@partition, df$y)
      meanByFrames[frame_idx] <- frameARI
      if(frameARI > best_ARI) {
        best_ARI <- frameARI
        best_frame_result <- result
        tsne_obj <- Rtsne(frame_data, is_distance = FALSE)
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("X","Y")) %>%
          mutate(cluster = factor(result@bestResult@partition))
        best_frame_tsne <- tsne_data
        best_mixmod_plots[[ds_idx]] <- data.frame(best_frame_tsne)
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
  title <- paste("Mixmod Results: ", ds_idx)
  g <- ggplot(mapping = aes(x = X, y = Y), best_mixmod_plots[[ds_idx]]) + geom_point(aes(color = cluster)) + ggtitle(title)
  print(g)
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main=title)
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}

mixmod_means <- list()
mixmod_start = Sys.time()
for(idx in 1:DS_COUNT) {
  mixmod_means[idx] <- mixmod_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]], idx)
}


mixmod_time <- (Sys.time() - mixmod_start)
mixmod_mean <- mean(unlist(mixmod_means))
barplot(unlist(mixmod_means),names=c(1,2,3,4,5,6,7), ylim=c(0,1), xlab="Test number", ylab="Mean ARI", main="Mixmod mean ARI by Test")
