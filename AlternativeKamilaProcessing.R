library(kamila)
library(aricode)
library(rlist)
library(sfsmisc)
kamila_process_varied_set <- function(ds, plot_characteristics, clusters, ds_idx) {
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
      cont_columns <- d_cols[[1]]
      cat_columns <- d_cols[[2]]
      cont_col.num <- which(colnames(frame_data) %in% cont_columns)
      cat_col.num <- which(colnames(frame_data) %in% cat_columns)
      
      cat_df <- as.data.frame(frame_data[, cat_col.num])
      cont_df <- as.data.frame(frame_data[, cont_col.num])
      numClust <- clusters[[d_idx]] 
      kamRes <- kamila(cont_df,
                       cat_df,
                       numClust = numClust,
                       numInit = 15)
      frameARI <- ARI(kamRes$finalMemb, df$y)
      meanByFrames[frame_idx] <- frameARI
      if(frameARI > best_ARI) {
        best_ARI <- frameARI
        best_frame_result <- kamRes 
        tsne_obj <- Rtsne(frame_data, is_distance = FALSE)
        tsne_data <- tsne_obj$Y %>%
          data.frame() %>%
          setNames(c("X","Y")) %>%
          mutate(cluster = factor(kamRes$finalMemb))
        best_frame_tsne <- tsne_data
        best_kamila_plots[[ds_idx]] <- data.frame(best_frame_tsne)
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
  print(current_mean)
  names <- plot_characteristics$names
  ylab <- plot_characteristics$ylab
  xlab <- plot_characteristics$xlab
  title <- paste("Kamila Results: ", ds_idx)
  g <- ggplot(mapping = aes(x = X, y = Y), best_kamila_plots[[ds_idx]]) + geom_point(aes(color = cluster)) + ggtitle(title)
  print(g)
  boxplot(unlist(boxplots[1]), unlist(boxplots[2]), unlist(boxplots[3]), names=names, ylab=ylab, xlab=xlab, main="Kamila Results")
  points(c(mean1, mean2, mean3), pch=20) 
  return(current_mean)
}

kamila_means <- list()
kamila_start = Sys.time()
for(idx in 1:DS_COUNT) {
  current_mean <- kamila_process_varied_set(data_collection[[DS_IDX]][[idx]], data_collection[[PC_IDX]][[idx]], data_collection[[CLUSTER_IDX]][[idx]], idx)
  kamila_means[idx] <- current_mean
}
kamila_time <- (Sys.time() - kamila_start)
kamila_overall_mean <- mean(unlist(kamila_means))
barplot(unlist(kamila_means),names=c(1,2,3,4,5,6,7), ylim=c(0,1), xlab="Test number", ylab="Mean ARI", main="Kamila mean ARI by Test")
