#' @title Detect Local dependency
#' @name local.dependency
#' @description One-line function description
#' @details Multiline function description (indent after the first line)
#' @param do an object of class \code{digram.object}
#' @export
#' @return list of local dependencies
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples Examples using the function
#' @note Any notes about the operation of the function
#' @aliases Additional aliases through which users can find documentation
#' @references References concerning the methodology employed by function
library("DescTools")
library("iarm")
local.dependency<-function(do=NULL,recoded=NULL){
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    recoded<-do$recoded
    items<-lapply(do$variables,function(x) x$variable.label)
  } else items<-colnames(recoded)
  num.items<-ncol(recoded)
  for(i in 1:num.items-1) {
    for(j in (i+1):num.items) {
      tab<-table(recoded[,2:3])
      lr<-GoodmanKruskalGamma(tab,conf.level = 0.95)
      lb<-partgam_LD(recoded[,1:6],p.adj = "BH")
    }
  }
  BH <- p.adjust(pvals, "BH")
}
