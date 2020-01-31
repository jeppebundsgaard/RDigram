#' @title Update variable
#' @name update.variable
#' @description Update variable in DIGRAM Object.
#' @usage update.variable(do=NULL,variable=NULL,variable.name=NULL,variable.label=NULL,category.names=c(),variable.type=c("ordinal","nominal"),minimum=NULL,maximum=NULL,cutpoints=c())
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
#' DHP<-update.variable(do=DHP,variable=2,cutpoints=c(2,3))
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
update.variable<-function(do=NULL,variable.num=NULL,variable.name=NULL,variable.label=NULL,category.names=NULL,variable.type=NULL,minimum=NULL,maximum=NULL,cutpoints=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(variable.num)) stop("You need to provide a variable number")
  #data<-as.data.frame(data)
  variable.num<-if(class(variable.to.update)=="numeric") variable.to.update else which(sapply(do$variables,function(x) x[["variable.name"]]==variable.to.update))
  variable<-do$variables[[variable.num]]
  do$variables[[variable.num]]<-list(variable.name=ifelse(is.null(variable.name),variable$variable.name,variable.name),
       variable.label=ifelse(is.null(variable.label),variable$variable.label,variable.label),
       ncat=ifelse(is.null(ncat),variable$ncat,ncat),
       variable.type=ifelse(is.null(variable.type),variable$variable.type,variable.type),
       category.names=if(is.null(category.names)) variable$category.names else category.names,
       minimum=ifelse(is.null(minimum),variable$minimum,minimum),
       maximum=ifelse(is.null(maximum),variable$maximum,maximum),
       cutpoints=if(is.null(cutpoints)) variable$cutpoints else cutpoints)
  colnames(do$data)[variable.num]<-do$variables[[variable.num]]$variable.name
  do$recoded<-digram.recode(do$data,do$variables,do$filter.conditions)
  do
}
