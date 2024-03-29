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
  lsep<-ifelse(knitr::is_latex_output() || knitr::is_html_output(),"-","###############################")
  newline<-ifelse(knitr::is_latex_output() || knitr::is_html_output(),"\n\n","\n")
  cat(lsep,newline,"## ",do$project,newline,lsep,sep="")
  if(class(do$comments)=="character") cat(newline,"Description:",newline,do$comments,newline,lsep,sep = "")
  cat(newline,ncol(do$data)," variables in the dataset",sep="")
  cat(newline,length(do$variables)," project variables:",sep="")

  print(knitr::kable(x = t(sapply(do$variables,function(x) c(x$variable.label,x$variable.name,paste(x$ncat,x$variable.type,"categories")))),col.names = c("Label","Variable","Categories"),booktabs=T,longtable=T,format = "markdown"))
  #for(x in do$variables) {if(!is.null(x)) cat(newline,x$variable.label," ",x$variable.name,"\t",x$ncat,x$variable.type,"categories")}
  # Recursive blocks
  cat("\n\n")
  first<-1
  for(i in 1:do$recursive.blocks) {
    if(i!=1) cat(" <- ")
    for(j in first:do$recursive.structure[i]) cat(ifelse(nchar(do$variables[[j]]$variable.label)>1 && j>first," ",""),do$variables[[j]]$variable.label,sep="")
    first<-do$recursive.structure[i]+1

  }
  cat("\n\n",nrow(do$data)," cases in the dataset",sep="")
  if(!is.null(do$testlets)) {
    cat("\n\nTestlets:\n\n *",
        paste(
          lapply(do$testlets,function(x)
            paste0(x$label,": ",x$name,"\n    - ",
                   paste(sapply(x$testlet,function(y)
                     do$variables[[y]]["variable.name"]),
                     collapse = " + ")
                   )
            ),
            collapse = "\n\n * ")
    )
  }
  if(!is.null(do$splits)) {
    cat("\n\nSplit items:\n *",paste(apply(do$split,1,function(x) paste(do$variables[[x[["var"]]]]$variable.name,"by",do$variables[[x[["exo"]]]]$variable.name)),collapse = "\n * "))
  }
  if(length(do$commandsrun)>0) cat("\n\nCommands run on original digram object:\n\n ",paste(do$commandsrun,collapse = "\n\n"))
}
#print(do)
