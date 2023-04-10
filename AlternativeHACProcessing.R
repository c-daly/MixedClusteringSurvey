library(cluster)
library(aricode)
library(rlist)
library(sfsmisc)

HAC_process_varied_set <- function(ds, plot_characteristics, clusters, ds_idx) {
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
      y <- subset(df, select = c(y))
 
      clust_num <- clusters[[d_idx]]
      dist <- daisy(frame_data, metric = "gower")
      result <- hclust(dist, method="ward.D2")
      cut_result <- cutree(result, clust_num)
      frameARI <-ARI(cut_result, df$y)
      if(frameARI > best_ARI) {
        best_ARI <- frameARI
        best_frame_result <- cut_result
        tsne_obj <- Rtsne(dist, is_distance = TRUE)
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("X","Y")) %>%
          mutate(cluster = factor(cut_result))
        best_frame_tsne <- tsne_data
        best_hac_plots[[ds_idx]] <- data.frame(best_frame_tsne)
      }
      meanByFrames[frame_idx] <- frameARI
    }
    frameMean <- mean(unlist(meanByFrames))
    meanAcrossFrames[d_idx] <- frameMean
    boxplots[[d_idx]] <- meanByFrames
  }
  mean1 <- mean(unlist(boxplots[1]))
  mean2 <- mean(unlist(boxplots[2]))
  mean3 <- mean(unlist(boxplots[3]))
  current_mean <- mean(c(mean1, mean2, mean3))
  names <- plot_characteristics$names
  ylab <- plot_characteristics$ylab
  xlab <- plot_characteristics$xlab
  title <- paste("HClust Results (Gower/Ward.D2): ", ds_idx)
  g <- ggplot(mapping = aes(x = X, y = Y), best_hac_plots[[ds_idx]]) + geom_point(aes(color = cluster)) + ggtitle(title)
  print(g)
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main="HAC Results")
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}

#HAC_process_varied_set(D_NVARIES, nvaries_plot_characteristics)
#HAC_process_varied_set(D_CONTVARIES, contvaries_plot_characteristics)
#HAC_process_varied_set(D_KVARIES, kvaries_plot_characteristics, clusters=list(2,6,10))
#HAC_process_varied_set(D_CAT_REL_VARIES, cat_rel_varies_plot_characteristics)
#HAC_process_varied_set(D_CONT_REL_VARIES, cont_rel_varies_plot_characteristics)
#HAC_process_varied_set(D_NUM_CONT_VARIES_CAT4, contvaries_plot_characteristics_cat4)
#HAC_process_varied_set(D_NUM_CONT_VARIES_CAT8, contvaries_plot_characteristics_cat8)

hac_means <- list()
hac_start = Sys.time()
for(idx in 1:DS_COUNT) {
  current_mean <- HAC_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]], idx)
  hac_means[idx] <- current_mean
}
hac_time <- (Sys.time() - hac_start)
hac_overall_mean <- mean(unlist(hac_means))
barplot(unlist(hac_means),names=c(1,2,3,4,5,6,7), ylim=c(0,1), main="HAC mean ARI by Test")
