#' Brute force search
#'
#' going through all possible alternatives and return the maximum value found, but faster.
#' 
#' @param x a data.frame with two variables v and w
#' @param W the knapsack size, a positive number
#' 
#' @return the maximum knapsack value and which elements (rows in the data.frame)
#' 
#' @examples
#' brute_force_knapsack_optimize(x = knapsack_objects[1:8,], W = 3500)
#' 
#' @export


brute_force_knapsack_optimize <- function(x, W){
  # W is the knapsack size
  
  result_list <- list()
  
  input_check <- all(x > 0) && is.data.frame(x) && ncol(x) == 2 && W > 0 && is.numeric(W)
  stopifnot(input_check)
  
  n <- nrow(x)
  # numbers of all elements
  result_value <- 0
  result_elements <- integer()
  
  for (i in 1:2^n - 1) {
    elements <- which(intToBits(i) == 1)
    #elements to be tested
    weight <- sum(.subset2(x, 1)[elements])
    value <- sum(.subset2(x, 2)[elements])
    # .subset2  >> [[]] > []
    # This is to avoid expensive unclassing when applying the default method to an object. 
    if ( weight < W ) {
      # elements can be contained by W-size knapsack
      if ( value > result_value ) {
        result_value <- round(value)
        result_elements <- elements
      }
    }
  }
  
  result_list <- list(value = result_value, elements = result_elements)
  
  return(result_list)
}


