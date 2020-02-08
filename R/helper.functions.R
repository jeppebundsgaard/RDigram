#' Not exported
header.format<-function(t="",margin=0) {
  if(knitr::is_latex_output() || knitr::is_html_output()) {
    cat("\n",rep("#",4-margin)," ",t,sep="")
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
  if(ncol(corr.matrix.print)>6 && knitr::is_latex_output()) {
    p <-p %>%
      kable_styling(latex_options = c("scale_down")) %>% landscape()
  }
  if(verbose){
    #cat("\n")
    print(p)
    }
  invisible(p)
}
get.labels<-function(do,items=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  l<-sapply(do$variables,function(x) x$variable.label)
  if(!is.null(items)) l<-l[items]
  l

  # if(is.null(items)) items<-colnames(do$recoded) else {
  #   if(is.numeric(items)) items<-colnames(do$recoded)[items]
  # }
  # if(inherits(items,"character")) items<-which(items %in% colnames(recoded))
  # sapply(do$variables,function(x) x$variable.label)

  #all.vars<-t(sapply(do$variables,function(x) c(x$variable.name,x$variable.label)))
  #apply(matrix(items),1,function(x) all.vars[all.vars[,1]==x,2])
}
get.variable.names<-function(do,items=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  n<-sapply(do$variables,function(x) x$variable.name)
  if(!is.null(items)) n<-n[items]
  n
}
