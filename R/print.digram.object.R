#' @title Print DIGRAM object
#' @name print.digram.object
#' @description Prints information about a DIGRAM object
#' @details Prints
#' @param do a DIGRAM object
#' @export
#' @return Returns nothing
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples print(DHP)
#' @note Any notes about the operation of the function
#' @references References concerning the methodology employed by function
print.digram.object <- function(do=NULL) {
  if(!class(do)=="digram.object") stop("You need to provide a digram.object")
  lsep<-"###############################"
  cat(lsep,"\n# Project: ",do$project,"\n",lsep,sep="")
  cat("\nDescription:\n",do$comments,"",sep = "")
  cat(lsep,"\n",ncol(do$data)," variables in the dataset",sep="")
  cat("\n",length(do$variables)," project variables:",sep="")
  for(x in do$variables) {if(!is.null(x)) cat("\n",x$variable.label," ",x$variable.name,"\t",x$ncat,x$variable.type,"categories")}
  # Recursive blocks
  cat("\n\n")
  first<-1
  for(i in 1:do$recursive.blocks) {
    if(i!=1) cat("<-")
    for(j in first:do$recursive.structure[i]) cat(do$variables[[j]]$variable.label)
    first<-do$recursive.structure[i]+1

  }
  cat("\n\n",nrow(do$data)," cases in the dataset",sep="")

}
#print(do)
