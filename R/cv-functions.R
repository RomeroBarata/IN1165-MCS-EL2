createStratifiedPartition <- function(y, folds = 10){
  if (is.data.frame(y)) y <- unlist(y, use.names = FALSE)
  classes_dist <- table(y)
  
  partition <- vector(mode = "numeric", length = length(y))
  for(i in seq_along(classes_dist)){
    if (length(folds) == 1){
      max_sample <- ceiling(classes_dist[i] / folds) * folds
      folds_idx <- rep_len(1:folds, length.out = max_sample)
    } else{
      offset <- classes_dist[i] %% 10
      max_sample <- classes_dist[i] - offset + 10
      folds_idx <- rep_len(rep.int(seq_along(folds), times = folds),
                           length.out = max_sample)
    }
    class_partition <- sample(folds_idx)[1:classes_dist[i]]
    class_id <- names(classes_dist)[i]
    partition[y == class_id] <- class_partition
  }
  partition
}

cvTrain <- function(data, method, method_args = list(), 
                    folds, repeats, cores, seed = NULL, ...){
  if (!is.null(seed)) set.seed(seed)
  partitions <- replicate(repeats, 
                          createStratifiedPartition(data[[ncol(data)]], folds), 
                          simplify = FALSE)
  
  results <- parallel::mcMap(train, 
                             data = list(data), 
                             method = list(method), 
                             method_args = list(method_args), 
                             partition = partitions, 
                             cores = list(cores), 
                             mc.cores = cores)
}

train <- function(data, method, method_args = list(), partition, cores, ...){
  folds <- length(unique(partition))
  for (i in seq_len(folds)){
    training <- data[partition != i, ]
    model <- do.call(method, c(list(data = training), method_args))
    
    testing <- data[partition == i, -ncol(data)]
    predictions <- predict(model, testing)
    
    y <- unlist(data[partition == i, ncol(data)], use.names = FALSE)
    
    # 
    maj_vote <- lapply(predictions, majorityVote)
    acc <- lapply(maj_vote, function(x) mean(x == y))
    
    # Assemble results
    if (i == 1){
      results <- rbind(c(Original = acc[[1]], Pruned = acc[[2]]))
    } else{
      results <- rbind(results, c(Original = acc[[1]], Pruned = acc[[2]]))
    }
  }
  results
}