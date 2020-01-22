#' Mean vector function
#'
#' The function takes the input and constructs a vector composed of each sample mean
#'
#' @param X
#'
#' @return A vector of sample means
#' @export
#'
#' @examples X = 1:30, meanvector(X)
meanvector = function(X){
  mx = colMeans(X)
  p = length(X)

  sm = matrix(mx, p, 1, FALSE)
  list(sm = sm)
}


