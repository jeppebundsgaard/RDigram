#' @title Update variable
#' @name variable.update
#' @description Update variable in DIGRAM Object.
#' @usage variable.update(do=NULL,variable=NULL,variable.name=NULL,variable.label=NULL,category.names=c(),variable.type=c("ordinal","nominal"),minimum=NULL,maximum=NULL,cutpoints=c())
#' @param do A digram.object
#' @param variable.to.update The number or name of the variable that should be updated
#' @param variable.name New name for the variable (string)
#' @param variable.label New label for the variable (one or more uppercase letters)
#' @param variable.type Type of variable ("ordinal" or "nominal")
#' @param minimum Smallest value of the variable (lower values are coded NA)
#' @param maximum Largest value of the variable (lower values are coded NA)
#' @param cutpoints Cutpoints used to recode the variable
#' Category 1: minimum <= values <= cutpoint(1)
#' Category 2: cutpoint(1) < values <= cutpoint(2)
#' ...
#' Category N: cutpoint(n) < values <= maximum
#' @export
#' @return Returns a DIGRAM object with an updated variable
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' DHP<-variable.update(do=DHP,variable.to.update="dhp36",cutpoints=c(2,3))
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
variable.update<-function(do=NULL,variable.to.update=NULL,variable.name=NULL,variable.label=NULL,category.names=NULL,variable.type=c("nominal","ordinal"),minimum=NULL,maximum=NULL,cutpoints=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(variable.to.update)) stop("You need to provide a number or name of the variable to update")
  variable.type<-match.arg(variable.type)
  #data<-as.data.frame(data)
  variable.num<-if(class(variable.to.update)=="numeric") variable.to.update else which(sapply(do$variables,function(x) x[["variable.name"]]==variable.to.update))
  variable<-do$variables[[variable.num]]
  do$variables[[variable.num]]<-list(variable.name=ifelse(is.null(variable.name),variable$variable.name,variable.name),
       variable.label=ifelse(is.null(variable.label),variable$variable.label,variable.label),
       column.number=variable$column.number,
       ncat=variable$ncat,
       category.names=if(is.null(category.names)) variable$category.names else category.names,
       variable.type=ifelse(is.null(variable.type),variable$variable.type,variable.type),
       minimum=ifelse(is.null(minimum),variable$minimum,minimum),
       maximum=ifelse(is.null(maximum),variable$maximum,maximum),
       cutpoints=if(is.null(cutpoints)) variable$cutpoints else cutpoints)
  colnames(do$data)[variable.num]<-do$variables[[variable.num]]$variable.name
  do$recoded<-digram.recode(do$data,do$variables,do$filter.conditions)
  do
}
#' Get column number of a variable in the recoded data
#'
#' @param do A DIGRAM object
#' @param variable.name.num The number or name of the variable to search for
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
  variable.num<-if(class(variable.name.num)=="numeric") variable.name.num else which(sapply(do$variables,function(x) x[["variable.name"]]==variable.name.num))
  do$variables[[variable.num]]$column.number
}
