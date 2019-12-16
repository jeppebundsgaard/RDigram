#' @title Item correlations
#' @name item.correlations
#' @description Calculate item correlations, item-rest correlations and correlations between items and exogenous variables
#' @usage item.correlations(do=NULL,recoded=NULL,items=1:do$recursive.structure[1],exo=(do$recursive.structure[1]+1):do$recursive.structure[2])
#' @param do an object of class \code{digram.object}
#' @param recoded A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param exo A vector of columns from the recoded data to include as exogenous variables in the analysis *or* a character vector of variable labels
#' @param accept.na A boolean. Include cases with missing values in responses
#' @param verbose Print results
#' @export
#' @details
#' First step in item screening: Analysis of consistency (Positive correlations)
#' \describe{
#' \item{M1}{Yi and Yj are positively correlated for all pairs of items}
#' \item{M2}{Ya is positively monotonically related to the rest-score Ra and all subscores SB where Ya is not an element in B}
#' \item{M3}{If X is positively related to Θ, then X will also be positively related to S, to all subscores, SA , and all item responses Yi}
#' }
#' @return Returns a list of correlations
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' library(iarm)
#' do<-digram.object(project = "desc2",data = desc2,variables = c(5:14,2:4,1),recursive.structure = c(10,13))
#' item.correlations(do)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
item.correlations<-function(do=NULL,recoded=NULL,items=NULL,exo=NULL,accept.na=F,verbose=T){
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    recoded<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
    if(is.null(exo)) exo<-if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else c()
  } else {
    if(is.null(items)) items<-colnames(recoded)
    if(is.null(exo)) exo<-c()
  }
  selected<-if(accept.na) recoded[,items] else na.omit(recoded[,items])
  if(nrow(selected)==0) stop("No cases without NA's. Try setting accept.na to TRUE")
  selectedexo<-na.omit(recoded[,c(items,exo)])
  num.items<-length(items)
  num.exo<-length(exo)

  header<-header.format("Item-item correlations")
  pval.matrix<-corr.items<-matrix(rep(NA,(num.items^2)),nrow = num.items)
  for(i in 1:(num.items)) {
    for(j in (i):num.items) {
      tab<-table(selected[,c(i,j)])
      acor<-MESS::gkgamma(tab,conf.level = 0.95)
      corr.items[i,j]<-acor$estimate
      pval.matrix[i,j]<-acor$p.value
    }
  }
  BH.items <- matrix(p.adjust(pval.matrix, "BH"),nrow=num.items)
  items.print<-print.corr.matrix(corr.items,BH.items,colnames(selected),verbose = verbose)
  neg.corr<-which(corr.items<0)
  no.corr<-which(BH.items>0.05)
  if(length(neg.corr)>0) {
    rels<-c()
    for(i in neg.corr) {
      r<-((i-1) %% num.items)+1
      rels<-c(rels,paste(colnames(selected)[r],"and",colnames(selected)[ceiling(i/num.items)]))
    }
    warning(paste0(header,"\nNegative correlation between\n",paste(rels,collapse = "\n")))
  }
  if(length(no.corr)>0) {
    rels<-c()
    for(i in no.corr) {
      r<-((i-1) %% num.items)+1
      rels=c(rels,paste(colnames(selected)[r],"and",colnames(selected)[ceiling(i/num.items)]))
    }
    warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")))

  } else if(length(neg.corr)==0) {
    cat("\nAll correlations are significantly positive.")
  }
  #####
  header<-header.format("Item-rest correlations")
  pval.rest<-corr.rest<-rep(NA,num.items)
  for(i in 1:(num.items)) {
      item<-selected[,i]
      rest<-apply(selected[,-i],1,sum,na.rm=T)
      tab<-table(item,rest)
      acor<-MESS::gkgamma(tab,conf.level = 0.95)
      corr.rest[i]<-acor$estimate
      pval.rest[i]<-acor$p.value
  }
  BH.rest <- p.adjust(pval.rest, "BH")
  rest.print<-print.corr.matrix(corr.rest,BH.rest,colnames(selected),verbose = verbose)
  neg.rest<-which(corr.rest<0)
  no.rest<-which(BH.rest>0.05)
  if(length(neg.rest)>0) {
    rels<-c()
    for(i in neg.rest) {
      r<-((i-1) %% num.items)+1
      rels<-c(rels,paste(colnames(selected)[r],"and",colnames(selected)[ceiling(i/num.items)]))
    }
    warning(paste0(header,"\nNegative correlation between\n",paste(rels,collapse = "\n")))
  }
  if(length(no.rest)>0) {
    rels<-c()
    for(i in no.rest) {
      r<-((i-1) %% num.items)+1
      rels=c(rels,paste(colnames(selected)[r],"and",colnames(selected)[ceiling(i/num.items)]))
    }
    warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")))

  } else if(length(neg.rest)==0) {
    cat("\nAll correlations are significantly positive.")
  }
  #####
  if(num.exo>0) {
    header<-header.format("Correlations between items and exogeneous variables")
    pval.exo<-corr.exo<-matrix(rep(NA,(num.items*num.exo)),nrow = num.items)
    for(i in 1:num.items) {
      for(j in 1:num.exo) {
        tab<-table(selectedexo[,c(i,num.items+j)])
        acor<-MESS::gkgamma(tab,conf.level = 0.95)
        corr.exo[i,j]<-acor$estimate
        pval.exo[i,j]<-acor$p.value
      }
    }
    BH.exo <- matrix(p.adjust(pval.exo, "BH"),nrow = num.items)
    exo.print<-print.corr.matrix(corr.exo,BH.exo,colnames(selectedexo[,exo]),colnames(selectedexo[,items]),verbose = verbose)
    # stargazer::stargazer(corr.exo,na.print = "",type = "text",rownames = T,colnames = T)
    neg.exo<-which(corr.exo<0)
    pos.exo<-which(corr.exo>0)
    no.exo<-which(BH.exo>0.05)
    if(length(neg.exo)>0 && length(pos.exo)>0) {
      for(j in 1:ceiling(max(pos.exo,neg.exo)/num.items)) {
        relsneg<-c()
        relspos<-c()
        for(i in neg.exo[neg.exo>num.items*(j-1) & neg.exo<=num.items*j]) {
          r<-((i-1) %% num.items)+1
          relsneg<-c(relsneg,paste(colnames(selectedexo)[r],"and",colnames(selectedexo)[ceiling(i/num.items)+num.items]))
        }
        for(i in pos.exo[pos.exo>num.items*(j-1) & pos.exo<=num.items*j]) {
          r<-((i-1) %% num.items)+1
          relspos<-c(relspos,paste(colnames(selectedexo)[r],"and",colnames(selectedexo)[ceiling(i/num.items)+num.items]))
        }
        if(!is.null(relsneg) && !is.null(relspos))
          warning(paste0(header,"\nNegative correlation between\n",paste(relsneg,collapse = "\n"),"\nBut positive correlation between\n",paste(relspos,collapse = "\n")))
      }
    }
    if(length(no.exo)>0) {
      rels<-c()
      for(i in no.exo) {
        r<-((i-1) %% num.items)+1
        rels=c(rels,paste(colnames(selectedexo)[r],"and",colnames(selectedexo)[ceiling(i/num.items)+num.items]))
      }
      warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")))

    } else if(length(neg.exo)==0) {
      cat("\nAll correlations are significantly positive.")
    }
  }
  invisible(list(corr.items=corr.items,BH.items=BH.items,items.print=items.print,
      corr.rest=corr.rest,BH.rest=BH.rest,rest.print=rest.print,
      corr.exo=corr.exo,BH.exo=BH.exo,exo.print=exo.print))
}
print.corr.matrix<-function(corr.matrix=NULL,pvals=NULL,cnames=NULL,rnames=NULL,verbose=T) {
  symp <- symnum(pvals, corr = F,
                 cutpoints = c(0,  .001,.01,.05, .1, 1),
                 symbols = c("***","**","*","."," "),na = F)
  corr.matrix.print<-gsub("NA","",matrix(paste(format(corr.matrix,nsmall = 2), symp),nrow = length(if(is.null(rnames)) cnames else rnames)))
  corr.matrix.print<-data.frame(corrs=corr.matrix.print)
  if(length(dim(corr.matrix))==2) {
    kablecnames<-cnames
    if(!is.null(rnames)) rownames(corr.matrix.print)<-rnames else rownames(corr.matrix.print)<-cnames
  } else {
    kablecnames<-c("Rest")
    rownames(corr.matrix.print)<-cnames
  }
  p<-knitr::kable(x = corr.matrix.print,col.names = kablecnames,row.names = T)
  if(verbose)
    print(p)
  p
}
