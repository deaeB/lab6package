#'data for knappack
#'
#'The data we will use is generated with {set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")}. To create larger datasets, just set n to a larger number.
#'
#'@format a data.frame with 2000 obs of 2 variables
#'
#' \describe{
#'   \item{w}{weight of the item}
#'   \item{v}{value of the item}
#'   }
#'   
#'@source https://www.ida.liu.se/~732A94/info/courseinfo.en.shtml lab6 2022


"knapsack_objects"


if (0) {
  RNGversion(min(as.character(getRversion()),"3.5.3"))
  ##old sampler used for backward compatibility
  ## suppressWarnings() can be used so that the above warning is not displayed
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 2000
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
}
