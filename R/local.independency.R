#' @title Detect local dependency
#' @name local.independency
#' @description Investigate items for local independence
#' @usage local.independency(do=NULL,recoded=NULL,items=1:do$recursive.structure[1],exo=(do$recursive.structure[1]+1):do$recursive.structure[2])
#' @param do an object of class \code{digram.object}
#' @param recoded A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param exo A vector of columns from the recoded data to include as exogenous variables in the analysis *or* a character vector of variable labels
#' @param digits Number of digits in table
#' @param verbose Print results
#' @export
#' @details
#' Second step in item screening: Analysis of DIF and local dependency
#' \describe{
#' \item{C2}{Y i ⊥X j |S for all i = 1 . . . k and j = 1 ...m}
#' \item{C4}{Y a ⊥Y b |R a and Y a ⊥Y b |R b}
#' }
#' @return Returns a list of local dependencies
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' local.independency(DHP)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
local.independency<-function(do=NULL,recoded=NULL,items=NULL,exo=NULL,p.adj= c("BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none"),digits=2,verbose=T){
  p.adj <- match.arg(p.adj)
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    recoded<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
    if(is.null(exo)) exo<-if(ncol(recoded)>do$recursive.structure[1]) do$recursive.structure[1]:ncol(recoded) else c()
      #if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else if(ncol(recoded)>do$recursive.structure[1]) do$recursive.structure[1]:ncol(recoded) else c()
  } else {
    if(is.null(items)) items<-colnames(recoded)
    if(is.null(exo)) exo<-c()
  }
  selected<-na.omit(recoded[,items])
  sums<-apply(selected,2,sum,na.rm=T)
  if(!all(sums>0)) {
    removecols<-sums==0
    warning(paste("Some items had no variation. The following items have been removed:",colnames(selected)[removecols]))
    selected<-selected[,!removecols]
  }
  if(sum(complete.cases(selected))==0) stop("There are no complete cases in the dataset. Complete cases are needed for calculation of partial gamma.")
  sink("/dev/null")
  orig.result<-iarm::partgam_LD(selected,p.adj = p.adj)
  sink()
  result<-orig.result
  missing.item1<-colnames(selected)[!colnames(selected) %in% unique(result$Item1)]
  missing.item2<-colnames(selected)[!colnames(selected) %in% unique(result$Item2)]
  result[nrow(result)+1,]<-c(missing.item1,missing.item2,rep(NA,6))
  colnames(result)[5]<-"p.adj"
  molten<-reshape2::melt(data = result[,-6],id.vars=c("Item1","Item2"),na.rm=F)
  # tonum<-!molten$variable %in% c("sig")
  molten$value<-as.numeric(molten$value)
  dep.matrix<-reshape2::acast(data = molten,formula = Item1~Item2~variable,drop = F)

  num.col<-ncol(dep.matrix)
  order.cols<-order(apply(dep.matrix[,,"gamma"],2,function(x) sum(!is.na(x))),decreasing = F)
  order.rows<-order(apply(dep.matrix[,,"gamma"],1,function(x) sum(!is.na(x))),decreasing = T)

  is.dependant<-which(orig.result$p.adj<0.05)
  if(length(is.dependant)>0) {
    rels<-c()
    for(i in is.dependant) {
      rels=c(rels,paste(orig.result$Item1[i],"and",orig.result$Item2[i]))
    }
    warning(paste0("\nLocal dependence between\n",paste(rels,collapse = "\n")))

  }
  item.labels<-get.labels(do,colnames(gammas))
  if(verbose)
    print.corr.matrix(corr.matrix=dep.matrix[order.rows,order.cols,"gamma"],pvals = dep.matrix[order.rows,order.cols,"p.adj"],cnames = item.labels[order.cols],rnames=paste(item.labels[order.rows],rownames(dep.matrix)[order.rows],sep = ": "),digits = digits)

  nsign<-which(apply(dep.matrix[,,"p.adj"],1:2,function(x) ifelse(x>0.05,T,F)))
  gammas<-dep.matrix[,,"gamma"]
  gammas[nsign]<-NA
  rownames(gammas)<-item.labels
  plotmat(A = round(gammas,2),
              pos = num.col, curve = 0.7, lwd = 1,shadow.size = 0,
              arr.len = 0.3, arr.width = 0.15, my = 0.2,
              box.size = 0.03, arr.type = "curved", dtext = 0.25,
              main = "Significant partial gamma coefficients between items")
  #print.corr.matrix(corr.matrix=matrix(dep.matrix[,,"gamma"]),ncol = num.col,byrow = T),pvals = matrix(as.numeric(dep.matrix[,,"p.adj"]),ncol = num.col,byrow = T),cnames = colnames(dep.matrix))
  invisible(orig.result)
}
#local.independency(do=proces.do,items = grep(paste0("^",m), colnames(proces.do$recoded)))
