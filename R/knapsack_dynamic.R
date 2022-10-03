




knapsack_dynamic <- function(x, W){
  
  input_check <- all(x > 0) && is.data.frame(x) && ncol(x) == 2 && W > 0 && is.numeric(W)
  stopifnot(input_check)
  
  result_list <- list()
  result_value <- 0
  result_elements <- integer()
  
  n <- nrow(x)
  
  arr_dyn <- array(0, c(n, W))
  # initiate array of results with 0
  
  # initiate first row as a stupid compensation for not having row 0
  if (x[1, "w"] < W) {
    arr_dyn[1, c(x[1,"w"]:W)] <- x[1,"v"]
  }
  
  # find max v
  for (i in 2:n) {
    # i: number of elements
    for (j in 1:W) {
      # j: weights of sackpack
      if (j < x[i, "w"]) {
        # x[i, "w"] : weights of i'th item
        # i'th item cant be put into j weight, >[i-1] items in j weight
        arr_dyn[i, j] <- arr_dyn[i - 1, j]
      }
      else {
        # consider i'th item can be in
        # >[i-1] items in [j - w(i)] weight, v' + v(i)
        arr_dyn[i, j] <- max(arr_dyn[i - 1, j], arr_dyn[i - 1, j - x[i, "w"]] + x[i, "v"])
      }
    }
  }
  result_value <- arr_dyn[n, W]
  # find max v end
  
  # find elements for max v
  j <- W
  for (i in n:2) {
    if (arr_dyn[i, j] > arr_dyn[i - 1, j]) {
      result_elements <- append(result_elements, i)
      j <- j - x[i, "w"]
      # check if arr_dyn[i, j] "jumped" from arr_dyn[i - 1, j]
      # if so , store this element(i) and deduct the w[i] from j
    }
    else {
      # doing nothing, keep j, let i--
    }
  }
  if (arr_dyn[1, j] > 0) {
    result_elements <- append(result_elements, 1)
  }
  # for row 0
  
  result_value <- round(result_value)
  result_elements <- sort(result_elements)
  result_list <- list(value = result_value, elements = result_elements)
  
  return(result_list)
}

