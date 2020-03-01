#' @title Detect local dependence
#' @name local.independence
#' @description Investigate items for local independence
#' @usage local.independence(do=NULL,resp=NULL,items=1:do$recursive.structure[1],digits=2,verbose=T)
#' @param do an object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param p.adj the kind of multiple p-value testing adjustment to be used (one of "BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none").
#' @param digits Number of digits in table
#' @param max.name.length Maximum length of item names (to be printed in tables)
#' @param only.significant Only list fit values significantly different from 1
#' @param verbose Print results
#' @export
#' @details
#' Second step in item screening: Analysis of DIF and local dependence
#' \describe{
#' \item{C4}{\eqn{Y_a \bot Y_b \mid R_a}{Y_a ⊥ Y_b | R_a} and \eqn{Y_a \bot Y_b \mid R_b}{Y_a ⊥ Y_b | R_b}}
#' Conditional independence of A and B given C is denoted as \eqn{A \bot B \mid C}{A ⊥ B | C}.
#' }
#' Use [item.DIF()] for detection of Differential Item Functioning
#'
#'If you want to use this function in R Markdown or Bookdown, you need to use xelatex as latex engine, and you need to force dev to use cairo_pdf or png.
#' Add this in you yaml header:
#'
#' \code{output:\cr
#'   pdf_document: \cr
#'     latex_engine: xelatex}
#'
#' Add this in your setup chunck:
#'
#' \code{knitr::opts_chunk$set(echo = TRUE, dev = "cairo_pdf", dpi = 300)}
#' @return Returns a list of local dependencies
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @seealso [partgam_LD()], [item.DIF()]
#' @examples
#' local.independence(DHP)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
local.independence<-function(do=NULL,resp=NULL,items=NULL,p.adj= c("BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none"),digits=2,max.name.length=30,only.significant=F,verbose=T){
  p.adj <- match.arg(p.adj)
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
      #if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else if(ncol(recoded)>do$recursive.structure[1]) do$recursive.structure[1]:ncol(recoded) else c()
  } else {
    if(is.null(items)) items<-colnames(resp)
  }
  item.names<-get.variable.names(do,items)
  if(inherits(items,"character")) items<-match(items,item.names)
  item.labels<-get.labels(do,items)
  header<-header.format("Test of local independence")
  # Combine items with LD
  environment(collapse.testlets) <- environment()
  collapse.testlets()

  item.names<-item.names.shorten(item.names,max.name.length)
  selected<-na.omit(resp[,items])
  sums<-apply(selected,2,sum,na.rm=T)
  if(!all(sums>0)) {
    removecols<-sums==0
    items<-items[!removecols]
    warning(paste("Some items had no variation. The following items have been removed:",paste(item.names[removecols],collapse = ", ")))
    item.names<-item.names[!removecols]
    item.labels<-item.labels[!removecols]
    selected<-selected[,!removecols]
  }
  if(sum(complete.cases(selected))==0) stop("There are no complete cases in the dataset. Complete cases are needed for calculation of partial gamma.")
  if(ncol(selected)<=2) {
    warning("More that two items are needed for analysis of local independency.")
  } else {
    colnames(selected)<-item.names
    sink("/dev/null")
    orig.result<-iarm::partgam_LD(selected,p.adj = p.adj)
    sink()
    result<-orig.result
    missing.item1<-colnames(selected)[!colnames(selected) %in% unique(result$Item1)]
    missing.item2<-colnames(selected)[!colnames(selected) %in% unique(result$Item2)]
    result[nrow(result)+1,]<-c(missing.item1,missing.item2,rep(" ",6))
    colnames(result)[5]<-"p.adj"
    molten<-reshape2::melt(data = result[,-6],id.vars=c("Item1","Item2"),na.rm=T)
    # tonum<-!molten$variable %in% c("sig")
    molten$value<-as.numeric(molten$value)
    dep.matrix<-reshape2::acast(data = molten,formula = Item1~Item2~variable,drop = F)
    num.col<-ncol(dep.matrix)

    is.dependant<-which(orig.result$p.adj<0.05)
    if(length(is.dependant)>0) {
      item1<-unique(orig.result$Item1[is.dependant])
      rels<-paste(item1,"and",sapply(item1,function(x) paste(orig.result$Item2[!is.na(orig.result$p.adj) & orig.result$p.adj<0.05 & orig.result$Item1 == x],collapse = ", ")))
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


    dograph<-as_tbl_graph(do,items=items,LD=orig.result[orig.result[,5]<0.05,1:3])
    p<-
      ggraph::ggraph(dograph,layout="fr")+
      ggraph::geom_edge_link(mapping=aes(label=ifelse(!is.na(gamma),round(abs(gamma),digits),""),alpha=ifelse(!is.na(gamma),abs(gamma),.4),color=is.na(gamma)),
                     angle_calc="along",label_dodge=unit(.25,"cm"),end_cap = ggraph::square(.5, 'cm'),start_cap = ggraph::square(.5, 'cm'),arrow = arrow(angle=10,length=unit(.2,"cm")))+
      ggraph::geom_node_label(mapping = aes(label=label,color=type)) +
      theme_void()+
      theme(legend.position = "none")+
      ggraph::scale_edge_color_brewer(palette = "Set1" ,limits=c(FALSE,TRUE))+
      scale_color_brewer(palette = "Set2")# ,limits=c(FALSE,TRUE))

   if(knitr::is_latex_output() || knitr::is_html_output()) {
     print(p)
    } else print(p)

    invisible(orig.result)
  }
}
#local.independence(do=proces.do,items = grep(paste0("^",m), colnames(proces.do$recoded)))
