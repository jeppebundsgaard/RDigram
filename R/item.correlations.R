#' @title Item correlations
#' @name item.correlations
#' @description Calculate item correlations, item-rest correlations and correlations between items and exogenous variables
#' @usage item.correlations(do=NULL,resp=NULL,items=1:do$recursive.structure[1],exo=(do$recursive.structure[1]+1):do$recursive.structure[2])
#' @param do an object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include as items in the analysis *or* a character vector of variable labels
#' @param do.testlets Bolean. If TRUE, testlets are combined to superitems.
#' @param do.split Bolean. If TRUE, items coded as split are split.
#' @param exo A vector of columns from the recoded data to include as exogenous variables in the analysis *or* a character vector of variable labels
#' @param max.name.length Maximum length of item names (to be printed in tables)
#' @param accept.na A boolean. Include cases with missing values in responses
#' @param verbose Print results
#' @param extra.verbose Print warnings in PDF and HTML-output
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
item.correlations<-function(do=NULL,resp=NULL,items=NULL,do.testlets=T,do.split=T,exo=NULL,max.name.length=30,accept.na=F,verbose=T,extra.verbose=F,
                            caption.items=paste("Item correlations for",ifelse(is.null(do),"items",do$project)),
                            caption.rest=paste("Item-rest correlations for",ifelse(is.null(do),"items",do$project)),
                            caption.exo=paste("Correlations between items and exogenous variables for",ifelse(is.null(do),"items",do$project))
                            ){
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
  items<-get.column.no(do,items)
  item.labels<-get.labels(do,items)
  item.names<-get.variable.names(do,items)

  all.olditems<-c()
  if(do.testlets && !is.null(do$testlets)) {
    for(testlet in do$testlets){
      olditems<-which(items %in% testlet$testlet)
      if(length(olditems)>0) {
        newitem<-ncol(resp)+1
        items<-c(items,newitem)
        all.olditems<-c(all.olditems,olditems)
        # Recode
        resp[,newitem]<-apply(resp[,testlet$testlet],1,sum)#,na.rm=T)
        # Combine names and item.labels
        item.names<-c(item.names,testlet$name)
        colnames(resp)[newitem]<-testlet$name
        item.labels<-c(item.labels,testlet$label)
      }
    }
  }

  if(do.split && !is.null(do$split)) {
    if(!accept.na) {
      warning("Set accept.na to TRUE if you want to get information on split items.")
    } else {
      for (i in 1:nrow(do$split)) {
        splits<-do$split[i,]
        olditem<-as.numeric(splits[1])
        if(length(olditem %in% items)>0) {
          exoitem<-as.numeric(splits[2])
          exocat<-do$variables[[exoitem]]$category.names
          ncat<-do$variables[[exoitem]]$ncat
          newitems<-ncol(resp)+1:ncat
          items<-c(items,newitems)
          all.olditems<-c(all.olditems,olditem)
          # Split
          nas<-rep(NA,ncat)
          resp[,newitems]<-sapply(1:nrow(resp),function(i) {
            newscores<-nas
            if(resp[i,exoitem] %in% exocat[,2]) newscores[resp[i,exoitem]]<-resp[i,olditem]
            newscores
          })

          # Combine names and item.labels
          olditemno<-which(items %in% olditem)
          newnames<-paste0(colnames(resp)[olditem],"_",exocat[,2])
          colnames(resp)[newitems]<-newnames
          item.names<-c(item.names,newnames)
          item.labels<-c(item.labels,paste0(item.labels[olditemno],"_",do$variables[[exoitem]]$variable.label,1:ncat))
        }
      }
    }
  }

  # Remove old.items used in testlet and split
  if(length(all.olditems)>0) {
    item.labels<-item.labels[-all.olditems]
    item.names<-item.names[-all.olditems]
    items<-items[-all.olditems]
  }


  selected<-if(accept.na) resp[,items] else na.omit(resp[,items])
  if(nrow(selected)==0) stop("No cases without NA's. Try setting accept.na to TRUE")

  item.names<-item.names.shorten(item.names,max.name.length)

  num.items<-length(items)

  header<-header.format("Item-item correlations")
  pval.matrix<-corr.items<-matrix(rep(NA,(num.items^2)),nrow = num.items)
  for(i in 1:(num.items)) {
    for(j in (i):num.items) {
      tab<-table(selected[,c(i,j)])
      acor<-MESS::gkgamma(tab,conf.level = 0.95)
      acor$estimate[is.nan(acor$estimate)]<-NA
      corr.items[i,j]<-acor$estimate
      pval.matrix[i,j]<-acor$p.value
    }
  }
  BH.items <- matrix(p.adjust(pval.matrix, "BH"),nrow=num.items)


  items.print<-print.corr.matrix(corr.matrix = corr.items,pvals = BH.items,cnames = item.labels,rnames=paste(item.labels,item.names,sep = ": "),verbose = verbose,caption=caption.items)
  neg.corr<-which(corr.items<0)
  no.corr<-which(BH.items>0.05)
  if(length(neg.corr)>0) {
    rels<-c()
    for(i in neg.corr) {
      r<-((i-1) %% num.items)+1
      rels<-c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
    }
    RDigram.warning(paste0(header,"\nNegative correlation between\n",paste(rels,collapse = "\n")),extra.verbose=extra.verbose)
  }
  if(length(no.corr)>0) {
    rels<-c()
    for(i in no.corr) {
      r<-((i-1) %% num.items)+1
      rels=c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
    }
    RDigram.warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")),extra.verbose=extra.verbose)

  } else if(length(neg.corr)==0) {
    cat("\nAll correlations are significantly positive.")
  }
  #####
  header<-header.format("Item-rest correlations")
  pval.rest<-corr.rest<-rep(NA,num.items)
  for(i in 1:(num.items)) {
      item<-selected[,i]
      rest<-apply(array(selected[,-i]),1,sum,na.rm=T)
      tab<-table(item,rest)
      acor<-MESS::gkgamma(tab,conf.level = 0.95)
      corr.rest[i]<-acor$estimate
      pval.rest[i]<-acor$p.value
  }
  BH.rest <- p.adjust(pval.rest, "BH")
  rest.print<-print.corr.matrix(corr.rest,BH.rest,item.names,verbose = verbose,caption=caption.rest)
  neg.rest<-which(corr.rest<0)
  no.rest<-which(BH.rest>0.05)
  if(length(neg.rest)>0) {
    rels<-c()
    for(i in neg.rest) {
      r<-((i-1) %% num.items)+1
      rels<-c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
    }
    RDigram.warning(paste0(header,"\nNegative correlation between\n",paste(rels,collapse = "\n")),extra.verbose=extra.verbose)
  }
  if(length(no.rest)>0) {
    rels<-c()
    for(i in no.rest) {
      r<-((i-1) %% num.items)+1
      rels=c(rels,paste(item.names[r],"and",item.names[ceiling(i/num.items)]))
    }
    RDigram.warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")),extra.verbose=extra.verbose)

  } else if(length(neg.rest)==0) {
    cat("\nAll correlations are significantly positive.")
  }
  #####
  if(length(exo)>0) {
    exona<-!is.na(resp[,c(items,exo)])
    selecteditemsexo<-resp[exona,items]
    exoselected<-resp[exona,exo]
    exo.names<-get.variable.names(do,exo)
    exo.names<-item.names.shorten(exo.names,max.name.length)

    exos<-make.exo.dummies(do,exo,exoselected,exo.names)
    exoselected<-exos$exoselected
    exo.names<-exos$exo.names
    exo<-exos$exo
    num.exo<-length(exo)

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
    exo.print<-print.corr.matrix(corr.exo,BH.exo,cnames= exo.names,rnames=item.names,verbose = verbose,caption=caption.exo)
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
          RDigram.warning(paste0(header,"\nNegative correlation between\n",paste(relsneg,collapse = "\n"),"\nBut positive correlation between\n",paste(relspos,collapse = "\n")),extra.verbose=extra.verbose)
      }
    }
    if(length(no.exo)>0) {
      rels<-c()
      for(i in no.exo) {
        r<-((i-1) %% num.items)+1
        rels=c(rels,paste(item.names[r],"and",exo.names[ceiling(i/num.items)]))
      }
      RDigram.warning(paste0(header,"\nNo significant correlation between\n",paste(rels,collapse = "\n")),extra.verbose=extra.verbose)

    } else if(length(neg.exo)==0) {
      cat("\nAll correlations are significantly positive.")
    }
  } else corr.exo<-BH.exo<-exo.print<-c()
  invisible(list(corr.items=corr.items,BH.items=BH.items,items.print=items.print,
      corr.rest=corr.rest,BH.rest=BH.rest,rest.print=rest.print,
      corr.exo=corr.exo,BH.exo=BH.exo,exo.print=exo.print))
}
