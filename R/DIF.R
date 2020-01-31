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
#' @param only.significant Only list fit values significantly different from 1
#' @param verbose Print results
#' @export
#' @details
#' Second step in item screening: Analysis of DIF and local dependency
#' \describe{
#' \item{C2}{Y i ⊥X j |S for all i = 1 . . . k and j = 1 ...m}
#' \item{C4}{Y a ⊥Y b |R a and Y a ⊥Y b |R b}
#' }
#' Use local.dependency() to detect local dependency
#' @return Returns a list of DIF-information
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @seealso \code{\link{partgam_DIF}}
#' @examples
#' item.DIF(DHP)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
item.DIF<-function(do=NULL,resp=NULL,items=NULL,exo=NULL,p.adj=c("BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none"),digits=2,only.significant=F,verbose=T){
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
  item.labels<-get.labels(do,items)
  exo.names<-get.variable.names(do,exo)
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
  exo.names<-exos$exo.names
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
    rels<-c()
    for(i in has.DIF) {
      rels=c(rels,paste(result$Item[i],"and",result$Var[i]))
    }
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
  nitem<-length(item.labels)
  nexo<-length(exo.labels)
  froms<-as.numeric(factor(result$Var,levels = colnames(exoselected)))+nitem
  tos<-as.numeric(factor(result$Item,levels = colnames(selected)))

  item.nodes<-DiagrammeR::create_node_df(n=nitem,label=item.labels,fillcolor="ivory")
  exo.nodes<-DiagrammeR::create_node_df(n=nexo,label=exo.labels,fillcolor="snow")
  nodes<-rbind(item.nodes,exo.nodes)
  edges<-DiagrammeR::create_edge_df(from = froms,to=tos,rel="DIF",label=ifelse(result$sig==" "," ",round(result$gamma,2)),color=rgb(.8,.8,0,ifelse(result$sig==" ",0, car::recode(abs(result$gamma),"NaN=1"))))
  DIF.graph<-DiagrammeR::create_graph()%>%DiagrammeR::add_node_df(nodes)%>%DiagrammeR::add_edge_df(edges)
  if(knitr::is_latex_output() || knitr::is_html_output()) {
    file_name<-paste0("DIF_",do$project,".png")
    DiagrammeR::export_graph(graph = DIF.graph,file_name = file_name,file_type = "png")
    # print(knitr::include_graphics(path = file_name))
  } else print(DiagrammeR::render_graph(graph = DIF.graph,layout = "kk" ))

}
#item.DIF(do=proces.do,items = grep(paste0("^",m), colnames(proces.do$recoded)))
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
