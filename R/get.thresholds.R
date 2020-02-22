#' Get xsi item parameters from TAM object
#'
#' @param tamobj A TAM-object
#'
#' @return Returns a matrix of xsi parameters
#' @export
#'
#' @examples
#' data(DHP)
#' tamobj<-digram.estimate(DHP)
#' get.thresholds(tamobj)
get.thresholds<-function(tamobj) {
  A<-tamobj$A
  thresholds<--apply(data.frame(n=2:dim(A)[2]),1,function(x) A[,x,] %*% tamobj$xsi$xsi)
  error<--apply(data.frame(n=2:dim(A)[2]),1,function(x) A[,x,] %*% tamobj$xsi$se.xsi)
  return (data.frame(xsi=thresholds,se.xsi=error,row.names = row.names(A)))
}

