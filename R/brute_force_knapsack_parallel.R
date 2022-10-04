#' Brute force search
#'
#' going through all possible alternatives and return the maximum value found, with a choice of parallel. 
#' 
#' @param x a data.frame with two variables v and w
#' @param W the knapsack size, a positive number
#' @param parallel FALSE for non-parallel, TRUE for parallel,default F
#' 
#' @return the maximum knapsack value and which elements (rows in the data.frame)
#' 
#' @samples
#' brute_force_knapsack_parallel(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE)
#' 
#' @export
#' @import parallel

brute_force_knapsack_parallel <- function(x, W, parallel = FALSE){
  # W is the knapsack size
  
  result_list <- list()
  
  input_check <- all(x > 0) && is.data.frame(x) && ncol(x) == 2 && W > 0 && is.numeric(W)
  stopifnot(input_check)
  
  n <- nrow(x)
  # numbers of all elements
  result_value <- 0
  result_elements <- integer()
  
  if (parallel == TRUE) {
    
    c <- c(1:2^n - 1)
    funapply <- function(i){
      elements <- which(intToBits(i) == 1)
      weight <- sum(.subset2(x, 1)[elements])
      value <- sum(.subset2(x, 2)[elements])
      out <- data.frame(w = weight, v = value, n = i)
      # w | v | n 
      return(out)
    }
    # initiate
    corenum <- detectCores() 
    cl <- makeCluster(getOption("cl.cores", corenum))
    # start parallel
    outapply<- as.data.frame(t(parSapply(cl, c, funapply)))
    
    stopCluster(cl)
    # stop parallel
    
    outapply<- subset(outapply, w < W)
    result_par <- subset(outapply, v == max(unlist(outapply$v)))
    
    result_elements <- which(intToBits(result_par$n) == 1)
    result_value <- round(as.numeric(result_par$v))
    
  } else{
    # NOT parallel
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
  }
  
  result_list <- list(value = result_value, elements = result_elements)
  
  return(result_list)
}



