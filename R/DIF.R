#' @title Detect Differential Item Functioning
#' @name item.DIF
#' @description Detect Differential Item Functioning (DIF)
#' @usage item.DIF(do=NULL,resp=NULL,items=1:do$recursive.structure[1],exo=(do$recursive.structure[1]+1):do$recursive.structure[2],verbose=T)
#' @param do an object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param exo A vector of columns from the recoded data to include as exogenous variables in the analysis *or* a character vector of variable labels
#' @param p.adj the kind of multiple p-value testing adjustment to be used (one of "BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none").
#' @param digits Number of digits in table
#' @param max.name.length Maximum length of item names (to be printed in tables)
#' @param only.significant Only list fit values significantly different from 1
#' @param verbose Print results
#' @export
#' @details
#' Second step in item screening: Analysis of DIF and local dependency
#' \describe{
#' \item{C2}{\eqn{Y_i \perp X_j \mid S}{Y_i ⊥ X_j | S} for all \eqn{i = 1} \eqn{\ldots}{...} \eqn{k} and \eqn{j = 1} \eqn{\ldots}{...} \eqn{m}}
#' \item{C4}{\eqn{Y_a \perp Y_b \mid R_a}{Y_a ⊥ Y_b |R_a} and \eqn{Y_a \perp Y_b \mid R_b}{Y_a ⊥ Y_b | R_b}}
#' Conditional independence of A and B given C is denoted as \eqn{A \perp B \mid C}{A ⊥ B | C}.
#' }
#' Use [local.independence()] to detect local dependency
#'
#' If you want to use this function in R Markdown or Bookdown, you need to use xelatex as latex engine, and you need to force dev to use cairo_pdf or png.
#' Add this in you yaml header:
#'
#' \code{output:\cr
#'   pdf_document: \cr
#'     latex_engine: xelatex}
#'
#' Add this in your setup chunck:
#'
#' \code{knitr::opts_chunk$set(echo = TRUE, dev = "cairo_pdf", dpi = 300)}
#' @return Returns a list of DIF-information
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @seealso [partgam_LD()], [local.independence()]
#' @examples
#' item.DIF(DHP)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
item.DIF<-function(do=NULL,resp=NULL,items=NULL,exo=NULL,p.adj=c("BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none"),max.name.length=30,digits=2,only.significant=F,verbose=T){
  p.adj <- match.arg(p.adj)
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
    if(is.null(exo)) exo<-if(ncol(resp)>do$recursive.structure[1]) (do$recursive.structure[1]+1):ncol(resp) else NULL
    #if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else if(ncol(recoded)>do$recursive.structure[1]) do$recursive.structure[1]:ncol(recoded) else c()
  } else {
    if(is.null(items)) items<-colnames(resp)
  }

  item.names<-get.variable.names(do,items)
  if(inherits(items,"character")) items<-match(items,item.names)
  item.labels<-get.labels(do,items)

  # Combine items with LD
  environment(collapse.testlets) <- environment()
  collapse.testlets()

  exo.names<-get.variable.names(do,exo)
  if(inherits(exo,"character")) exo<-match(exo,exo.names)
  exo.labels<-get.labels(do,exo)

  header<-header.format("Test of Differential Item Functioning")

  no.na<-apply(resp[,c(items,exo)],1,function(x) all(!is.na(x)))
  selected<-resp[no.na,items]
  if(sum(complete.cases(selected))==0) stop("There are no complete cases in the dataset. Complete cases are needed for calculation of partial gamma.")
  removeitems<-setdiff(colnames(resp[,items]),colnames(selected))
  if(length(removeitems)>0) {
    removecols<-removeitems%in%colnames(resp[,items])
    warning(paste("Some items had no variation. The following items have been removed:",item.names[removecols]))
    item.names<-item.names[-removecols]
    item.labels<-item.labels[-removecols]
  }
  item.names<-item.names.shorten(item.names,max.name.length)
  colnames(selected)<-item.names

  exoselected<-resp[no.na,exo]
  removeexo<-setdiff(colnames(resp[,exo]),colnames(exoselected))
  if(length(removeexo)>0) {
    removecols<-removeexo%in%colnames(resp[,exo])
    warning(paste("Some exogenous variables had no variation. The following variables have been removed:",exo.names[removecols]))
    exo.names<-exo.names[-removecols]
    exo.labels<-exo.labels[-removecols]
  }
  if(ncol(exoselected)==0) stop("No exogeneous variable given. Can't do DIF analysis without.")
  # Make dummies out of nominal variables
  exos<-make.exo.dummies(do,exo,exoselected,exo.names,exo.labels)
  exoselected<-exos$exoselected
  exo.labels<-exos$exo.labels
  exo.names<-item.names.shorten(exos$exo.names,max.name.length)

  exo<-exos$exo
  # Remove exos with no cases
  exoselected<-as.data.frame(exoselected[,apply(exoselected,2,sum)>0])
  sink("/dev/null")
  result<-iarm::partgam_DIF(dat.items = selected,dat.exo = exoselected,p.adj = p.adj)
  sink()
  colnames(result)[5]<-"p.adj"
  molten<-reshape2::melt(data = result[,-6],id.vars=c("Item","Var"),na.rm=F)
  # tonum<-!molten$variable %in% c("sig")
  molten$value<-as.numeric(molten$value)
  DIF.matrix<-reshape2::acast(data = molten,formula = Item~Var~variable,drop = F)

  num.col<-ncol(DIF.matrix)
  #  order.cols<-order(apply(DIF.matrix[,,"gamma"],2,function(x) sum(is.finite(x)||is.nan(x))),decreasing = F)
  #  order.rows<-order(apply(DIF.matrix[,,"gamma"],1,function(x) sum(is.finite(x)||is.nan(x))),decreasing = T)

  has.DIF<-which(result$p.adj<0.05)
  if(length(has.DIF)>0) {
    item<-unique(result$Item[has.DIF])
    rels<-paste(item,"and",sapply(item,function(x) paste(result$Var[!is.na(result$p.adj) & result$p.adj<0.05 & result$Item == x],collapse = ", ")))

    warning(paste0("\nDifferential Item Functioning between\n",paste(rels,collapse = "\n")))
    if(knitr::is_html_output() || knitr::is_latex_output()) cat("\n\n### Differential Item Functioning\n\n",paste(rels,collapse = "\n\n"))
  }
  gammas<-DIF.matrix[,,"gamma"]
  nsign<-which(apply(DIF.matrix[,,"p.adj"],1:2,function(x) ifelse(x<0.05,T,F)))
  gammas[nsign]<-NA

  if(verbose)
    print.corr.matrix(corr.matrix=DIF.matrix[,,"gamma"],pvals = DIF.matrix[,,"p.adj"],cnames = colnames(DIF.matrix[,,"gamma"]),rnames=rownames(DIF.matrix[,,"gamma"]),digits = digits)
  #print.corr.matrix(corr.matrix=DIF.matrix[order.rows,order.cols,"gamma"],pvals = DIF.matrix[order.rows,order.cols,"p.adj"],cnames = exo.names[order.cols],rnames=item.names[order.rows],digits = digits)

  # Draw graph
  dograph<-as_tbl_graph(do,items=items,exo=exo,DIF=result[result[,5]<0.05,1:3])
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

}
make.exo.dummies<-function(do,exo,exoselected,exo.names,exo.labels=NULL) {
  # Make dummies out of nominal variables
  for(i in exo) {
    if(do$variables[[i]]$variable.type=="nominal") {
      exonum<-i-do$recursive.structure[1]
      dummies<-psych::dummy.code(exoselected[,exonum])
      colnames(dummies)<-paste(do$variables[[i]]$variable.name,colnames(dummies),sep = ", ")
      exoselected<-cbind(exoselected[,-exonum],dummies)
      exo.names<-c(exo.names[-exonum],colnames(dummies))
      if(!is.null(exo.labels)) exo.labels<-c(exo.labels[-exonum],paste0(exo.labels[exonum],1:ncol(dummies)))
      colnames(exoselected)<-exo.names
    }
  }
  exo<-do$recursive.structure[1]+(1:ncol(exoselected))
  list(exoselected=exoselected,exo=exo,exo.names=exo.names,exo.labels=exo.labels)
}
