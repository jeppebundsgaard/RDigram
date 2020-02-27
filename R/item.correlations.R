#' @title Item correlations
#' @name item.correlations
#' @description Calculate item correlations, item-rest correlations and correlations between items and exogenous variables
#' @usage item.correlations(do=NULL,resp=NULL,items=1:do$recursive.structure[1],exo=(do$recursive.structure[1]+1):do$recursive.structure[2])
#' @param do an object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param exo A vector of columns from the recoded data to include as exogenous variables in the analysis *or* a character vector of variable labels
#' @param max.name.length Maximum length of item names (to be printed in tables)
#' @param accept.na A boolean. Include cases with missing values in responses
#' @param verbose Print results
#' @export
#' @details
#' First step in item screening: Analysis of consistency (Positive correlations)
#' \describe{
#' \item{M1}{\eqn{Y_i} and \eqn{Y_j} are positively correlated for all pairs of items}
#' \item{M2}{\eqn{Y_a} is positively monotonically related to the rest-score \eqn{R_a} and all subscores \eqn{S_B} where \eqn{Y_a} \eqn{\not\in}{is not an element in} B}
#' \item{M3}{If \eqn{X} is positively related to \eqn{\theta}, then \eqn{X} will also be positively related to \eqn{S}, to all subscores, \eqn{S_A}, and all item responses \eqn{Y_i}}
#' }
#' @return Returns a list of correlations
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' library(iarm)
#' do<-digram.object(project = "desc2",data = desc2,variables = c(5:14,2:4,1),recursive.structure = c(10,13))
#' item.correlations(do)
#' @references
#' Kreiner, S. & Christensen, K.B. (2011). Item Screening in Graphical Loglinear Rasch Models. *Psychometrika*, vol. 76, no. 2, pp. 228-256. DOI: 10.1007/s11336-9203-Y
item.correlations<-function(do=NULL,resp=NULL,items=NULL,exo=NULL,max.name.length=30,accept.na=F,verbose=T){
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
    if(is.null(exo)) exo<-if(ncol(resp)>do$recursive.structure[1]) (do$recursive.structure[1]+1):ncol(resp) else NULL
    #if(is.null(exo)) exo<-if(length(do$recursive.structure)>1) (do$recursive.structure[1]+1):do$recursive.structure[2] else c()
  } else {
    if(is.null(items)) items<-colnames(resp)
    if(is.null(exo)) exo<-c()
  }
  selected<-if(accept.na) resp[,items] else na.omit(resp[,items])
  if(nrow(selected)==0) stop("No cases without NA's. Try setting accept.na to TRUE")
  exona<-!is.na(resp[,c(items,exo)])
  selecteditemsexo<-resp[exona,items]
  exoselected<-resp[exona,exo]

  item.labels<-get.labels(do,items)
  item.names<-get.variable.names(do,items)
  item.names<-sapply(item.names, function(x) ifelse(nchar(x)>max.name.length,paste(substr(x,start = 1,stop = max.name.length),"..."),x))

  exo.names<-get.variable.names(do,exo)
  exo.names<-sapply(exo.names, function(x) ifelse(nchar(x)>max.name.length,paste(substr(x,start = 1,stop = max.name.length),"..."),x))

  exos<-make.exo.dummies(do,exo,exoselected,exo.names)
  exoselected<-exos$exoselected
  exo.names<-exos$exo.names
  exo<-exos$exo
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


  items.print<-print.corr.matrix(corr.matrix = corr.items,pvals = BH.items,cnames = item.labels,rnames=paste(item.labels,item.names,sep = ": "),verbose = verbose)
  neg.corr<-which(corr.items<0)
  no.corr<-which(BH.items>0.05)
  if(length(neg.corr)>0) {
    rels<-c()
    for(i in neg.corr) {
      r<-((i-1) %% num.items)+1
      rels<-c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
    }
    warning(paste0(header,"\nNegative correlation between\n",paste(rels,collapse = "\n")))
  }
  if(length(no.corr)>0) {
    rels<-c()
    for(i in no.corr) {
      r<-((i-1) %% num.items)+1
      rels=c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
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
  rest.print<-print.corr.matrix(corr.rest,BH.rest,item.names,verbose = verbose)
  neg.rest<-which(corr.rest<0)
  no.rest<-which(BH.rest>0.05)
  if(length(neg.rest)>0) {
    rels<-c()
    for(i in neg.rest) {
      r<-((i-1) %% num.items)+1
      rels<-c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
    }
    warning(paste0(header,"\nNegative correlation between\n",paste(rels,collapse = "\n")))
  }
  if(length(no.rest)>0) {
    rels<-c()
    for(i in no.rest) {
      r<-((i-1) %% num.items)+1
      rels=c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
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
        tab<-table(selecteditemsexo[,i],exoselected[,j])
        acor<-MESS::gkgamma(tab,conf.level = 0.95)
        corr.exo[i,j]<-acor$estimate
        pval.exo[i,j]<-acor$p.value
      }
    }
    BH.exo <- matrix(p.adjust(pval.exo, "BH"),nrow = num.items)
    exo.print<-print.corr.matrix(corr.exo,BH.exo,cnames= exo.names,rnames=item.names,verbose = verbose)
    # stargazer::stargazer(corr.exo,na.print = "",type = "text",rownames = T,colnames = T)
    neg.exo<-which(corr.exo<0)
    pos.exo<-which(corr.exo>0)
    no.exo<-which(BH.exo>0.05)
    if(length(neg.exo)>0 && length(pos.exo)>0) {
      # Go through columns in corr - i.e. exos
      for(j in 1:ceiling(max(pos.exo,neg.exo)/num.items)) {
        relsneg<-c()
        relspos<-c()
        # Go through rows in corr - i.e. items
        for(i in neg.exo[neg.exo>num.items*(j-1) & neg.exo<=num.items*j]) {
          r<-((i-1) %% num.items)+1
          relsneg<-c(relsneg,paste(item.names[r],"and",exo.names[ceiling(i/num.items)]))
        }
        for(i in pos.exo[pos.exo>num.items*(j-1) & pos.exo<=num.items*j]) {
          r<-((i-1) %% num.items)+1
          relspos<-c(relspos,paste(item.names[r],"and",exo.names[ceiling(i/num.items)]))
        }
        if(!is.null(relsneg) && !is.null(relspos))
          warning(paste0(header,"\nNegative correlation between\n",paste(relsneg,collapse = "\n"),"\nBut positive correlation between\n",paste(relspos,collapse = "\n")))
      }
    }
    if(length(no.exo)>0) {
      rels<-c()
      for(i in no.exo) {
        r<-((i-1) %% num.items)+1
        rels=c(rels,paste(item.names[r],"and",exo.names[ceiling(i/num.items)]))
      }
      warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")))

    } else if(length(neg.exo)==0) {
      cat("\nAll correlations are significantly positive.")
    }
  } else corr.exo<-BH.exo<-exo.print<-c()
  invisible(list(corr.items=corr.items,BH.items=BH.items,items.print=items.print,
      corr.rest=corr.rest,BH.rest=BH.rest,rest.print=rest.print,
      corr.exo=corr.exo,BH.exo=BH.exo,exo.print=exo.print))
}
