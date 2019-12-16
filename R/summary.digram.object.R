#' @title Summarize DIGRAM object
#' @name summary.digram.object
#' @description Summarizes information about a DIGRAM object
#' @param do a DIGRAM object
#' @export
#' @return Returns nothing
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples summary(DHP)
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
summary.digram.object <- function(do=NULL) {
  # if(!inherits(do,"digram.object")) stop("You need to provide a digram.object")
  # lsep<-"###############################"
  # cat(lsep,"\n# Project: ",do$project,"\n",lsep,sep="")
  # cat("\nDescription:\n",do$comments,"",sep = "")
  # cat(lsep,"\n",ncol(do$data)," variables in the dataset",sep="")
  # cat("\n",length(do$variables)," project variables:",sep="")
  # for(x in do$variables) {if(!is.null(x)) cat("\n",x$variable.label," ",x$variable.name,"\t",x$ncat,x$variable.type,"categories")}
  # # Recursive blocks
  # cat("\n\n")
  # first<-1
  # for(i in 1:do$recursive.blocks) {
  #   if(i!=1) cat("<-")
  #   for(j in first:do$recursive.structure[i]) cat(do$variables[[j]]$variable.label)
  #   first<-do$recursive.structure[i]+1
  #
  # }
  # cat("\n\n",nrow(do$data)," cases in the dataset",sep="")
  print(do)
  score.information(do = do,items = 1:do$recursive.structure[1],accept.na = sum(apply(do$recoded[1:do$recursive.structure[1]],1,sum),na.rm = T)==0)
}
#print(do)
