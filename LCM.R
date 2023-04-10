library(VarSelLCM)
library(cluster)
#library(aricode)
library(rlist)
library(sfsmisc)

LCM_process_varied_set <- function(ds, plot_characteristics, clusters, ds_idx) {
  if(missing(clusters)) {
    clusters <- list(6,6,6)
  }
  best_ARI <- 0
  best_frame_result <- list()
  best_frame_tsne <- list()
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
      result <- VarSelCluster(frame_data, num_clusters, nbcores=10, vbleSelec=FALSE, crit.varsel="BIC")
      #result <- VarSelCluster(frame_data, 6, nbcores=6)
      fresult <- fitted(result)
      frameARI <-ARI(fresult, df$y)
      meanByFrames[frame_idx] <- frameARI
      if(frameARI > best_ARI) {
        best_ARI <- frameARI
        best_frame_result <- result
        tsne_obj <- Rtsne(frame_data, is_distance = FALSE)
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("X","Y")) %>%
          mutate(cluster = factor(fresult))
        best_frame_tsne <- tsne_data
        best_lcm_plots[[ds_idx]] <- data.frame(best_frame_tsne)
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
  title <- paste("LCM Results: ", ds_idx)
  g <- ggplot(mapping = aes(x = X, y = Y), best_lcm_plots[[ds_idx]]) + geom_point(aes(color = cluster)) + ggtitle(title)
  print(g)
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main=title)
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}
lcm_means <- list()
lcm_start = Sys.time()
for(idx in 1:DS_COUNT) {
  current_mean <- LCM_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]], idx)
  lcm_means[idx] <- current_mean
  
}

lcm_time <- (Sys.time() - lcm_start)
lcm_overall_mean <- mean(unlist(lcm_means))
barplot(unlist(lcm_means),names=c(1,2,3,4,5,6,7),ylim=c(0,1), main="LCM mean ARI by Test")

