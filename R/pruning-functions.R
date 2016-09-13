EPIC <- function(ensemble, data, p){
  x <- subset(data, select = -Class)
  y <- data$Class
  num_examples <- nrow(data)
  labels <- levels(y)
  lookup <- structure(seq_along(labels), names = labels)
  
  predictions <- predict(ensemble, x)
  
  # 
  V <- t(apply(predictions, 1, function(x) table(x)[labels]))
  V[is.na(V)] <- 0
  dimnames(V) <- list(1:num_examples, labels)
  
  # 
  ensemble_predictions <- apply(V, 1, function(x) labels[which.max(x)])
  
  # 
  alpha_f <- function(j){
    correct_predictions <- predictions[, j] == y
    belongs_minority <- predictions[, j] != ensemble_predictions
    ifelse(correct_predictions & belongs_minority, 1, 0)
  }
  alpha <- vapply(seq_along(ensemble), alpha_f, numeric(num_examples))
  
  #
  beta_f <- function(j){
    correct_predictions <- predictions[, j] == y
    belongs_majority <- predictions[, j] == ensemble_predictions
    ifelse(correct_predictions & belongs_majority, 1, 0)
  }
  beta <- vapply(seq_along(ensemble), beta_f, numeric(num_examples))
  
  # 
  theta_f <- function(j){
    wrong_predictions <- predictions[, j] != y
    ifelse(wrong_predictions, 1, 0)
  }
  theta <- vapply(seq_along(ensemble), theta_f, numeric(num_examples))
  
  # 
  v_max <- apply(V, 1, max)
  v_sec <- apply(V, 1, function(x) sort(x, decreasing = TRUE)[2])
  v_correct <- vapply(seq_len(num_examples), 
                      function(i) V[i, lookup[y[i]]], 
                      numeric(1))
  
  # 
  IC <- function(j){
    v_current <- vapply(seq_len(num_examples), 
                        function(i) V[i, lookup[predictions[i, j]]], 
                        numeric(1))
    c_1 <- alpha[, j] * (2 * v_max - v_current)
    c_2 <- beta[, j] * v_sec
    c_3 <- theta[, j] * (v_correct - v_current - v_max)
    sum(c_1 + c_2 + c_3)
  }
  contributions <- vapply(seq_along(ensemble), IC, numeric(1))
  
  # 
  idx <- round((p / 100) * length(ensemble))
  ensemble[order(contributions, decreasing = TRUE)][1:idx]
}