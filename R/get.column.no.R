#' Get column number(s) of a variable in the recoded data
#'
#' @param do A DIGRAM object
#' @param variable.name.num The number, label or name of the variable(s) to search for
#'
#' @return Returns the column.number(s)
#' @export
#'
#' @examples
#' data(DHP)
#' get.column.no(DHP,"dhp36")
get.column.no<-function(do,variable.name.num) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(variable.name.num)) stop("You need to provide a number or name of the variable")
  variable.nums<-c()
  for(i in variable.name.num) {
    variable.num<-if(class(i)%in%c("integer","numeric")) i else {
      if(grepl("\"",i)) {i<-gsub("\"","",i)
      } else if(length(i)==1) i<-unlist(strsplit(i,"; ?|, ?| "))
      which(sapply(do$variables,function(x) x[["variable.name"]] %in% i || x["variable.label"] %in% i))
    }
    if(length(variable.num)==0) {
      # Let's try column names
      variable.num<-which(sapply(colnames(do$recoded),function(x) grepl(x,i)))
    }
    if(length(variable.num)==0) stop(paste(i,"is not a variable name or label in",do$project))
    #do$variables[[variable.num]]$column.number
    variable.nums<-c(variable.nums,variable.num)
  }
  variable.nums
}
