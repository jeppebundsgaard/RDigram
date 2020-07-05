#' @title Update variable
#' @name variable.update
#' @description Update one or more variable(s) in a DIGRAM Object.
#' @param do A digram.object
#' @param variable.to.update The number or name of the variable(s) which should be updated
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
#' @details A set of variables can be updated by providing a vector of variable names/numbers to update and ordered lists whith their new properties. For variable.type, cutpoints, minimum and maximum you can provide one property to give all variables.
#' @return Returns a DIGRAM object with (an) updated variable(s)
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' data(DHP)
#' DHP<-variable.update(do=DHP,variable.to.update="dhp36",cutpoints=c(2,3))
#' DHP<-variable.update(do=DHP,variable.to.update=c(1,3),variable.label=list("32","36"))
#'
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
variable.update<-function(do=NULL,variable.to.update=NULL,variable.name=NULL,variable.label=NULL,category.names=NULL,variable.type=NULL,minimum=NULL,maximum=NULL,cutpoints=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(variable.to.update)) stop("You need to provide a number or name of the variable to update")
  #data<-as.data.frame(data)
  if(!is.null(cutpoints) && !inherits(cutpoints,"list")) cutpoints<-rep(x = list(cutpoints),times=length(variable.to.update))
  if(!is.null(variable.type) && !inherits(variable.type,"list")) variable.type<-rep(x = list(variable.type),times=length(variable.to.update))
  if(!is.null(minimum) && !inherits(minimum,"list")) minimum<-rep(x = list(minimum),times=length(variable.to.update))
  if(!is.null(maximum) && !inherits(maximum,"list")) maximum<-rep(x = list(maximum),times=length(variable.to.update))

  for(i in 1:length(variable.to.update)) {
    variable.num<-if(inherits(variable.to.update[i],"integer")) variable.to.update[i] else which(sapply(do$variables,function(x) x[["variable.name"]]==variable.to.update[i]))
    variable<-do$variables[[variable.num]]
    if(!is.null(variable.type[[i]])) match.arg(variable.type[[i]],c("nominal","ordinal"))
    do$variables[[variable.num]]<-list(variable.name=ifelse(is.null(variable.name[i]),variable$variable.name,variable.name[i]),
         variable.label=ifelse(is.null(variable.label[[i]]),variable$variable.label,variable.label[[i]]),
         column.number=variable$column.number,
         ncat=variable$ncat,
         category.names=if(is.null(category.names[[i]])) variable$category.names else category.names[[i]],
         variable.type=ifelse(is.null(variable.type[[i]]),variable$variable.type,variable.type[[i]]),
         minimum=ifelse(is.null(minimum[[i]]),variable$minimum,minimum[[i]]),
         maximum=ifelse(is.null(maximum[[i]]),variable$maximum,maximum[[i]]),
         cutpoints=if(is.null(cutpoints[[i]])) variable$cutpoints else cutpoints[[i]])
    colnames(do$data)[variable.num]<-do$variables[[variable.num]]$variable.name
  }
  do$recoded<-digram.recode(do$data,do$variables,do$filter.conditions)
  do

}


#' @title Delete variable
#' @name variable.delete
#' @description Delete one or more variable(s) in a DIGRAM Object.
#' @param do A digram.object
#' @param variable.to.delete The number or name of the variable(s) which should be deleted
#' @export
#' @details A set of variables can be deleted by providing a vector of variable names/numbers.
#' @return Returns a DIGRAM object with (a) deleted variable(s)
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' data(DHP)
#' DHP<-variable.delete(do=DHP,variable.to.delete="dhp36")
#'
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
#'
variable.delete<-function(do=NULL,variable.to.delete=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(variable.to.delete)) stop("You need to provide a number or name of the variable to delete")
  variable.to.delete<-get.column.no(do,variable.to.delete)
  var.names<-get.variable.names(do,variable.to.delete)
  do$variables<-do$variables[-variable.to.delete]
  # Delete from testlets and splits
  if(!is.null(do$testlets)) do$testlets<-lapply(do$testlets,function(x) {x$testlet<-x$testlet[!(names(x$testlet) %in% var.names)]; x})
  if(!is.null(do$splits)) do$splits<-do$splits[apply(do$splits,1,function(x) {length(intersect(x,variable.to.delete))==0}),]
  # Update recursive structure
  for(i in variable.to.delete[order(variable.to.delete)]) {
    do$recursive.structure[do$recursive.structure>=i]<-do$recursive.structure[do$recursive.structure>=i]-1
  }
  do$recoded<-digram.recode(do$data,do$variables,do$filter.conditions)
  do
}
