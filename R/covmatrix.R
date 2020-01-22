#' Sample variances and covariances function
#'
#' The function constructs a matrix of variances and covariances by using the input.
#'
#' @param X
#'
#' @return A matrix of variances and covariances
#' @export
#'
#' @examples X = matrix(1:9, 3, 3), covmat(X)
covmat = function(X){
  mx = colMeans(X)
  p = length(X)
  n = length(X[,1])

  Sn = matrix(NA, p, p)

  for(i in 1:p){
    for(k in 1:p){
      Sn[i,k] = sum((X[,i]-mx[i])*(X[,k]-mx[k]))/n
    }
  }

  list(Sn = Sn)
}
