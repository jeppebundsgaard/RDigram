#' Not exported
header.format<-function(t="",margin=0) {
  if(knitr::is_latex_output() || knitr::is_html_output()) {
    cat("\n\n",rep("#",4-margin)," ",t,sep="")
  } else {
    tw<-stringr::str_length(t)
    cat("\n+",rep("-",tw+2),"+",rep(paste("\n|",stringr::str_pad(" ",tw),"|",collapse=""),margin),"\n| ",t," |\n",rep(paste("|",stringr::str_pad(" ",tw),"|\n",collapse=""),margin),"+",rep("-",tw+2),"+\n",sep="")
  }
  t
}
print.corr.matrix<-function(corr.matrix=NULL,pvals=NULL,cnames=NULL,rnames=NULL,verbose=T,digits=2) {
  library(kableExtra)
  symp <- symnum(pvals, corr = F,
                 cutpoints = c(0,  .001,.01,.05, .1, 1),
                 symbols = c("***","**","*","."," "),na = F,abbr.colnames = F)
  corr.matrix.print<-gsub("NA","",matrix(paste(round(corr.matrix,digits = digits), symp),nrow = length(if(is.null(rnames)) cnames else rnames)))
  corr.matrix.print<-data.frame(corrs=corr.matrix.print)
  #colnames(corr.matrix.print)<-cnames
  if(length(dim(corr.matrix))==2) {
    kablecnames<-cnames
    if(!is.null(rnames)) rownames(corr.matrix.print)<-rnames else rownames(corr.matrix.print)<-cnames
  } else {
    kablecnames<-c("Rest")
    rownames(corr.matrix.print)<-cnames
  }
  #print(kablecnames)
  p<-knitr::kable(x = corr.matrix.print,col.names = as.character(kablecnames),row.names = T,booktabs=T,longtable=ncol(corr.matrix.print)<=8,format=ifelse(knitr::is_html_output(),"html",ifelse(knitr::is_latex_output(),"latex","markdown")))
  if(ncol(corr.matrix.print)>3 && knitr::is_latex_output()) {
    p <-p %>%
      kable_styling(latex_options = c("scale_down")) %>% landscape()
  }
  if(verbose){
    #cat("\n")
    print(p)
    }
  invisible(p)
}
#' Get variable labels
#'
#' @param do A digram.object
#' @param items The variable.numbers of the items or exogeneous variables to provide the labels for
#'
#' @return Returns the variable.labels of the items/exogeneous variables .
#' @export
#'
#' @examples
#' get.labels(DHP,1:2)
get.labels<-function(do,items=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  l<-sapply(do$variables,function(x) x$variable.label)
  if(!is.null(items)) {
    if(max(items)>length(l)) { # We have testlet items
      l<-c(l,sapply(do$testlets,function(x) x$label))
    }
    l<-l[items]
  }
  l
}

#' Get variable names
#'
#' @param do A digram.object
#' @param items The variable.numbers of the items or exogeneous variables to provide the names for
#'
#' @return Returns the variable.names of the items/exogeneous variables .
#' @export
#'
#' @examples
#' get.variable.names(DHP,1:2)
get.variable.names<-function(do,items=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  n<-sapply(do$variables,function(x) x$variable.name)
  if(!is.null(items)) {
    if(max(items)>length(n)) { # We have testlet items
      n<-c(n,sapply(do$testlets,function(x) x$name))
    }
    n<-n[items]
  }
  n
}
item.names.shorten<-function(item.names,max.name.length) {
  item.names<-sapply(item.names, function(x)
    ifelse(nchar(x)>max.name.length,
           paste0(substr(x,start = 1,stop = floor(max.name.length/2)-2),"...",substr(x,start = nchar(x)-floor(max.name.length/2)+2,stop = nchar(x))),
           x)
  )
  dups<-which(duplicated(item.names))
  numdup<-0
  while(length(dups)>0) {
    numdup<-numdup+1
    item.names[dups]<-paste0(apply(array(item.names[dups]),1,substr,start=1,stop=max.name.length-numdup),paste(rep("x",numdup),collapse = ""))
    dups<-which(duplicated(item.names))
  }
  item.names
}
