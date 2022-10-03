
greedy_knapsack <- function(x, W){
  
  input_check <- all(x > 0) && is.data.frame(x) && ncol(x) == 2 && W > 0 && is.numeric(W)
  stopifnot(input_check)
  
  result_list <- list()
  result_value <- 0
  result_elements <- integer()
  s2_weight <- 0
  s2_value <- 0
  
  n <- nrow(x)
  
  x <- cbind(x, vpw = x[,"v"] / x[, "w"], N = c(1:n))
  x <- x[order(x[, "vpw"], decreasing = TRUE),]
  # add Value Per Weight "vpm" and num of items "N", sort decresingly by Value Per Weight
  
  result_value <- 0
  result_weight <- 0
  for (i in 1:n) {
    if (result_weight + x[i, "w"] <= W) {
      #current weight + item weight <= max weight, then put it in
      result_weight <- result_weight + x[i, "w"]
      result_value <- result_value + x[i, "v"]
      result_elements <- append(result_elements, x[i, "N"])
    } else {
      s2_weight <- x[i, "w"]
      s2_value <- x[i, "v"]
      break
      # containing the first item that did not fit
    }
  }
  
  if (result_value < s2_value) {
    result_weight <- s2_weight
    result_value <- s2_value
    result_elements <- c(i)
  }
  # choose s2 if with greater value
  
  result_value <- round(result_value)
  result_list <- list(value = result_value, elements = result_elements)
  
  return(result_list)
  
  
}