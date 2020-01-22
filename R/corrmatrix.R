#' Sample correlation function
#'
#' This function returns a matrix of sample correlations by using the input.
#'
#' @param X
#'
#' @return A matrix of sample correlations
#' @export
#'
#' @examples X = matrix(1:9, 3, 3), corrmat(X)
corrmat = function(X){
  mx = colMeans(X)
  p = length(X)
  n = length(X[,1])

  Sn = matrix(NA, p, p)
  R = matrix(NA, p, p)

  for(i in 1:p){
    for(k in 1:p){
      Sn[i,k] = sum((X[,i]-mx[i])*(X[,k]-mx[k]))/n
      R[i,k] = sum((X[,i]-mx[i])*(X[,k]-mx[k]))/(sqrt(sum((X[,i]-mx[i])^2))*sqrt(sum((X[,k]-mx[k])^2)))
    }
  }

  list(R = R)
}
