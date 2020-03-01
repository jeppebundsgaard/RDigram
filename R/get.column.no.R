#' Get column number of a variable in the recoded data
#'
#' @param do A DIGRAM object
#' @param variable.name.num The number, label or name of the variable to search for
#'
#' @return Returns the column.number
#' @export
#'
#' @examples
#' data(DHP)
#' get.column.no(DHP,"dhp36")
get.column.no<-function(do,variable.name.num) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(variable.name.num)) stop("You need to provide a number or name of the variable")
  variable.num<-if(class(variable.name.num)=="numeric") variable.name.num else which(sapply(do$variables,function(x) x[["variable.name"]]==variable.name.num || x["variable.label"]==variable.name.num))
  if(length(variable.num)==0) {
    # Let's try column names
    variable.num<-which(sapply(colnames(do$recoded),function(x) grepl(x,variable.name.num)))
  }
  if(length(variable.num)==0) stop(paste(variable.name.num,"is not a variable name or label in",do$project))
  do$variables[[variable.num]]$column.number
}
