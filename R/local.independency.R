#' @title Detect local dependency
#' @name local.independency
#' @description Investigate items for local independence
#' @usage local.independency(do=NULL,resp=NULL,items=1:do$recursive.structure[1],digits=2,verbose=T)
#' @param do an object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param p.adj the kind of multiple p-value testing adjustment to be used (one of "BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none").
#' @param digits Number of digits in table
#' @param only.significant Only list fit values significantly different from 1
#' @param verbose Print results
#' @export
#' @details
#' Second step in item screening: Analysis of DIF and local dependency
#' \describe{
#' \item{C2}{Y i ⊥X j |S for all i = 1 . . . k and j = 1 ...m}
#' \item{C4}{Y a ⊥Y b |R a and Y a ⊥Y b |R b}
#' }
#' #' Use item.DIF() for detection of Differential Item Functioning
#' @return Returns a list of local dependencies
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @seealso \code{\link{partgam_LD}}
#' @examples
#' local.independency(DHP)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
local.independency<-function(do=NULL,resp=NULL,items=NULL,p.adj= c("BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none"),digits=2,only.significant=F,verbose=T){
  p.adj <- match.arg(p.adj)
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
      #if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else if(ncol(recoded)>do$recursive.structure[1]) do$recursive.structure[1]:ncol(recoded) else c()
  } else {
    if(is.null(items)) items<-colnames(resp)
  }
  if(inherits(items,"character")) items<-match(items,item.names)
  item.names<-get.variable.names(do,items)
  item.labels<-get.labels(do,items)
  header<-header.format("Test of local independence")

  selected<-na.omit(resp[,items])
  sums<-apply(selected,2,sum,na.rm=T)
  if(!all(sums>0)) {
    removecols<-sums==0
    warning(paste("Some items had no variation. The following items have been removed:",item.names[removecols]))
    item.names<-item.names[!removecols]
    item.labels<-item.labels[!removecols]
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
  molten<-reshape2::melt(data = result[,-6],id.vars=c("Item1","Item2"),na.rm=T)
  # tonum<-!molten$variable %in% c("sig")
  molten$value<-as.numeric(molten$value)
  dep.matrix<-reshape2::acast(data = molten,formula = Item1~Item2~variable,drop = F)
  num.col<-ncol(dep.matrix)

  is.dependant<-which(orig.result$p.adj<0.05)
  if(length(is.dependant)>0) {
    rels<-c()
    for(i in is.dependant) {
      rels=c(rels,paste(orig.result$Item1[i],"and",orig.result$Item2[i]))
    }
    warning(paste0("\nLocal dependence between\n",paste(rels,collapse = "\n")))
    if(knitr::is_html_output() || knitr::is_latex_output()) cat("\n\n### Local dependence\n\n",paste(rels,collapse = "\n\n"))
  }
  siggammas<-gammas<-as.numeric(dep.matrix[,,"gamma"])
  nsign<-if(only.significant) which(apply(dep.matrix[,,"p.adj"],1:2,function(x) ifelse(x<0.05,T,F))) else c()
  siggammas[nsign]<-NA

  order.cols<-order(apply(dep.matrix[,,"gamma"],2,function(x) sum(is.na(x))-sum(is.nan(x))),decreasing = T)
  order.rows<-order(apply(dep.matrix[,,"gamma"],1,function(x) sum(is.na(x))-sum(is.nan(x))),decreasing = F)
  order.names<-match(rownames(dep.matrix[,,"gamma"])[order.rows],colnames(selected))
  order.labels<-match(colnames(dep.matrix[,,"gamma"])[order.cols],colnames(selected))
  #item.labels<-as.factor(get.labels(do,items = items))
  #item.labels<-get.labels(do,colnames(siggammas))
  #rownames(siggammas)<-item.labels

  if(verbose)
    print.corr.matrix(corr.matrix=dep.matrix[order.rows,order.cols,"gamma"],pvals = dep.matrix[order.rows,order.cols,"p.adj"],cnames = item.labels[order.labels],rnames=paste(item.labels[order.names],item.names[order.names],sep = ": "),digits = digits) #rownames(dep.matrix) erstattet af item.names
  #print.corr.matrix(corr.matrix=dep.matrix[order.rows,order.cols,"gamma"],pvals = dep.matrix[order.rows,order.cols,"p.adj"],cnames = item.labels[order.cols],rnames=paste(item.labels[order.rows],item.names[order.rows],sep = ": "),digits = digits) #rownames(dep.matrix) erstattet af item.names

  # Draw graph
  nitem<-length(items)
  #item.labels<-as.factor(get.labels(do,items = items))
  froms<-as.numeric(factor(result$Item1,levels = colnames(selected)))
  tos<-as.numeric(factor(result$Item2,levels = colnames(selected)))

  sigval<-(result$sig!=" " & !is.na(result$gamma))

  nodes<-DiagrammeR::create_node_df(n=nitem,label=item.labels,fillcolor="ivory")
  edges<-DiagrammeR::create_edge_df(from = froms,to=tos,rel="DIF",label=ifelse(sigval,round(as.numeric(result$gamma),2)," "),color=rgb(.8,.8,0,ifelse(sigval,abs(car::recode(as.numeric(result$gamma),"NaN=1")),0)))
  LD.graph<-DiagrammeR::create_graph()%>%DiagrammeR::add_node_df(nodes)%>%DiagrammeR::add_edge_df(edges)
  if(knitr::is_latex_output() || knitr::is_html_output()) {
    file_name<-paste0("LD_",do$project,".png")
    DiagrammeR::export_graph(graph = LD.graph,file_name = file_name,file_type = "png")
    # print(knitr::include_graphics(path = file_name))
  } else print(DiagrammeR::render_graph(graph = LD.graph,layout = "kk" ))

  # diagram::plotmat(A = round(gammas,2),
  #             pos = num.col, curve = 0.7, lwd = 1,shadow.size = 0,
  #             arr.len = 0.3, arr.width = 0.15, my = 0.2,
  #             box.size = 0.03, arr.type = "curved", dtext = 0.25,
  #             main = "Significant partial gamma coefficients between items")
  invisible(orig.result)
}
#local.independency(do=proces.do,items = grep(paste0("^",m), colnames(proces.do$recoded)))
