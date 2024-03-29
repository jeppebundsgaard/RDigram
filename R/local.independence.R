#' @title Detect local dependence
#' @name local.independence
#' @description Investigate items for local independence
#' @usage local.independence(do=NULL,resp=NULL,items=1:do$recursive.structure[1],digits=2,verbose=T)
#' @param do an object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param p.adj the kind of multiple p-value testing adjustment to be used (one of "holm", "BH","hochberg", "hommel", "bonferroni", "BY", "none"), see [p.adjust()].
#' @param digits Number of digits in table
#' @param max.name.length Maximum length of item names (to be printed in tables)
#' @param only.significant Only list fit values significantly different from 1
#' @param use.names Use item names instead of item labels as node labels
#' @param summarize.testlets If true, don't collapse testlets, but summarize number of local dependent item pairs (both ways) in each testlet. Asterisk (*) indicates one or more negative gamme correlations.
#' @param verbose Print results
#' @param extra.verbose Print warnings in PDF and HTML-output
#' @param list.LD List item pairs being locally dependent
#' @param draw.graph Draw graph of Local Dependencies
#' @param saved.result To avoid repeated calculation, you can provide a saved version of the analysis (returned from local.independence())
#' @param caption Caption for the LD table (in Rmarkdown)
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
#' Add this in your setup chunk:
#'
#' \code{knitr::opts_chunk$set(echo = TRUE, dev = "cairo_pdf", dpi = 300)}
#' @return Returns a list of local dependencies
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @seealso [partgam_LD()], [item.DIF()]
#' @examples
#' local.independence(DHP)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
local.independence<-function(do=NULL,resp=NULL,items=NULL,p.adj= c("holm","BH","hochberg", "hommel", "bonferroni", "BY", "none"),digits=2,max.name.length=30,only.significant=F,use.names=F,summarize.testlets=F,verbose=T,extra.verbose=F,draw.graph=T,list.LD=T,saved.result=NULL,caption=paste(ifelse(summarize.testlets,"Summary of",""),"Local dependence of",ifelse(is.null(do),"items",do$project))){
  p.adj <- match.arg(p.adj)
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
      #if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else if(ncol(recoded)>do$recursive.structure[1]) do$recursive.structure[1]:ncol(recoded) else c()
  } else {
    if(is.null(items)) items<-colnames(resp)
  }
  items<-get.column.no(do,items)
  item.names<-get.variable.names(do,items)
  #if(inherits(items,"character")) items<-match(items,item.names)
  item.labels<-if(use.names) item.names else get.labels(do,items)
  header<-header.format("Test of local independence")
  if(!summarize.testlets) {
    # Combine items with LD
    environment(collapse.testlets) <- environment()
    collapse.testlets()
  }
  item.names<-item.names.shorten(item.names,max.name.length)
  selected<-na.omit(resp[,items])
  sums<-apply(selected,2,sum,na.rm=T)
  if(!all(sums>0)) {
    removecols<-sums==0
    items<-items[!removecols]
    RDigram.warning(paste("Some items had no variation. The following items have been removed:",paste(item.names[removecols],collapse = ", ")),extra.verbose=extra.verbose)
    item.names<-item.names[!removecols]
    item.labels<-item.labels[!removecols]
    selected<-selected[,!removecols]
  }
  if(sum(complete.cases(selected))==0) stop("There are no complete cases in the dataset. Complete cases are needed for calculation of partial gamma.")
  if(ncol(selected)<=2) {
    warning("More that two items are needed for analysis of local independency.")
  } else {
    colnames(selected)<-item.names
    result<-if(is.null(saved.result)) partgam_LD(selected,p.adj = p.adj, verbose = F) else saved.result
    orig.result<-result
    colnames(result)[grep("padj",colnames(result))]<-"p.adj"
    molten<-reshape2::melt(data = result[,-7],id.vars=c("Item1","Item2"),na.rm=T)

    molten$value<-as.numeric(molten$value)
    dep.matrix<-reshape2::acast(data = molten,formula = Item1~Item2~variable,drop = F)

    num.col<-ncol(dep.matrix)

    is.dependant<-which(result$p.adj<0.05)
    if(list.LD && length(is.dependant)>0) {
      item1<-unique(result$Item1[is.dependant])
      rels<-paste(item1,"and",sapply(item1,function(x) paste(result$Item2[!is.na(result$p.adj) & result$p.adj<0.05 & result$Item1 == x],collapse = ", ")))
      warning(paste0("\nLocal dependence between\n",paste(rels,collapse = "\n")))
      if(knitr::is_html_output() || knitr::is_latex_output()) cat("\n\n### Local dependence\n\n",paste(rels,collapse = "\n\n"))
    }
    # siggammas<-gammas<-as.numeric(dep.matrix[,,"gamma"])
    # nsign<-if(only.significant) which(apply(dep.matrix[,,"p.adj"],1:2,function(x) ifelse(x<0.05,T,F))) else c()
    # siggammas[nsign]<-NA

    order.names<-match(item.names,rownames(dep.matrix[,,"gamma"]))

    if(summarize.testlets && !is.null(do$testlets)) {
      num.sign<-(dep.matrix[order.names,order.names,"p.adj"]<.05)*1
      neg.sign<-(dep.matrix[order.names,order.names,"p.adj"]<.05 & dep.matrix[order.names,order.names,"gamma"]<0)
      remove.cols<-c()
      item.names.sign<-item.names
      item.labels.sign<-item.labels
      for(tlist in do$testlets){
        testlet<-tlist$testlet
        testlet<-(which(items %in% testlet))
        if(length(testlet)>0) {
          num.sign<-cbind(num.sign,rowSums(num.sign[,testlet],na.rm = T))
          num.sign<-rbind(num.sign,colSums(num.sign[testlet,],na.rm = T))
          neg.sign<-cbind(neg.sign,apply(neg.sign[,testlet],1,any,na.rm = T))
          neg.sign<-rbind(neg.sign,apply(neg.sign[testlet,],2,any,na.rm = T))

          colnames(num.sign)[ncol(num.sign)]<-rownames(num.sign)[nrow(num.sign)]<-tlist$name
          item.names.sign<-c(item.names.sign,tlist$name)
          item.labels.sign<-c(item.labels.sign,tlist$label)
          remove.cols<-c(remove.cols,testlet)
        }
      }
      neg.sign<-neg.sign[-remove.cols,-remove.cols]
      num.sign<-num.sign[-remove.cols,-remove.cols]
      for(i in 1:ncol(num.sign))
        for(j in 1:nrow(num.sign))
          if(!is.na(num.sign[j,i])) num.sign[j,i]<-paste(num.sign[j,i],ifelse(neg.sign[j,i],"*",""))
      item.labels.sign<-item.labels.sign[-remove.cols]
      item.names.sign<-item.names.sign[-remove.cols]
      p<-knitr::kable(x = num.sign,row.names = T,booktabs=T,longtable=ncol(num.sign)<=8,col.names = if(use.names) item.labels.sign else paste(item.labels.sign,item.names.sign,sep = ": "),align = "r",format=ifelse(knitr::is_html_output(),"html",ifelse(knitr::is_latex_output(),"latex","markdown")),caption = caption)
      if(ncol(num.sign)>3 && knitr::is_latex_output()) {
        p <-p %>%
          kable_styling(latex_options = c("scale_down")) %>% landscape()
      }
      print(p)
    } else if(verbose)
      print.corr.matrix(corr.matrix=dep.matrix[order.names,order.names,"gamma"],pvals = dep.matrix[order.names,order.names,"p.adj"],cnames = item.labels,rnames=if(use.names) item.labels else paste(item.labels,item.names,sep = ": "),digits = digits,caption=caption)

    # Draw graph
    if(draw.graph){
      # Remove NaN's
      result<-result[!is.nan(result$gamma),]
      dograph<-as_tbl_graph(do,items=items,LD=result[result$p.adj<0.05,1:3],summarize.testlets = summarize.testlets)
      p<-
        ggraph::ggraph(dograph,layout="fr")+
        ggraph::geom_edge_link(mapping=aes(label=ifelse(!is.na(gamma),round(gamma,digits),""),alpha=ifelse(!is.na(gamma),abs(gamma),.4),color=is.na(gamma)),
                       angle_calc="along",label_dodge=unit(.25,"cm"),end_cap = ggraph::square(.5, 'cm'),start_cap = ggraph::square(.5, 'cm'),arrow = arrow(angle=10,length=unit(.2,"cm")))+
        ggraph::geom_node_label(mapping = aes(label=if(use.names) name else label,color=type)) +
        theme_void()+
        theme(legend.position = "none")+
        ggraph::scale_edge_color_brewer(palette = "Set1" ,limits=c(FALSE,TRUE))+
        scale_color_brewer(palette = "Set2")# ,limits=c(FALSE,TRUE))

       print(p)
    }
    invisible(orig.result)
  }
}
#local.independence(do=proces.do,items = grep(paste0("^",m), colnames(proces.do$recoded)))


# Hacking iarm
partgam_LD<-function (dat.items, p.adj = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "none"), verbose=T)
{
  padj <- match.arg(p.adj)
  dat.items<-dat.items[complete.cases(dat.items),]
  score <- rowSums(dat.items)
  k <- ncol(dat.items)
  result<-setNames(data.frame(matrix(ncol = 9,nrow=0)),c("Item1", "Item2","gamma", "se", "pvalue","padj", "sig", "lower","upper"))
  for(i in 1:k) {
    for(j in 1:k) {
      if(i!=j) {
        rest <- score - dat.items[,j]
        mm <- iarm::partgam(dat.items[,i], dat.items[,j], rest)
        result[nrow(result)+1,]<-c(names(dat.items)[j],names(dat.items)[i],mm[dim(mm)[1], 1:2],NA,NA,NA,mm[dim(mm)[1], 3:4])
      }
    }
  }
  result$pvalue <- apply(result[,3:4],1,function(x) ifelse(x[1] > 0, 2 * (1 - pnorm(x[1]/x[2])), 2 * (pnorm(x[1]/x[2]))))
  result$padj<-p.adjust(result$pvalue, method = padj)#, n = (k * (k - 1))) #/2
  result$sig<-symnum(result$padj, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c(" ***", " **", " *", " .", " "))
  names(result)[6] <- paste("padj", padj, sep = ".")

  if(verbose) {
      print(cbind(result[, 1:2], round(result[,3:ifelse(padj == "none",5,6)], digits = 4), sig = result[, 7], round(result[,8:9], digits = 4)))
      cat("\n")
    }
  invisible(result)
}
