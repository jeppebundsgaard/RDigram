#' Get item parameters and standard errors as a data.frame from a TAM or eRm object
#'
#' @param obj A TAM or eRm-object
#' @param type For TAM-objects, return Andersen or Conquest ("IRT") parameters.
#'
#' @return Returns a matrix of item parameters and standard errors, one row for each item
#' @export
#' @seealso [item.params.convert()]
#' @details Andersen parameters are given as easiness parameters as is the convention. Difficulty can be calculated as the negated version of the parameters.
#' @examples
#' data(DHP)
#' tamobj<-digram.estimate(DHP)
#' get.item.params(tamobj)
get.item.params<-function(obj,type=c("andersen","conquest"),include.se=F) {
  type=match.arg(type)
  switch(which(c("tam.mml","eRm") %in% class(obj)),
    "1"={#"tam.mml"
      # A<-obj$A
      # thresholds<--apply(data.frame(n=2:dim(A)[2]),1,function(x) A[,x,] %*% obj$xsi$xsi)
      # item.params<-data.frame(xsi=thresholds,se.xsi=error,row.names = row.names(A))
      if(type=="conquest") {
        item.params<-obj$item_irt[,grepl(pattern = "beta|tau",x = colnames(obj$item_irt))]
        if(!is.data.frame(item.params)) item.params<-as.data.frame(item.params)
        rownames(item.params)<-obj$item_irt$item
        attributes(item.params)$par.type<-"conquest"
        colnames(item.params)<-if(ncol(item.params)==1) c("beta") else c("beta",paste0("tau.",1:(ncol(item.params)-1)))
      }
      else {
        # Notice: Andersen parametres are easiness parameters
        item.params<--obj$item[,grepl(pattern = "AXsi",x = colnames(obj$item))]
        if(!is.data.frame(item.params)) item.params<-as.data.frame(item.params)
        attributes(item.params)$par.type<-"andersen"
        colnames(item.params)<-paste0("cat.",1:ncol(item.params))
      }
      if(include.se) {
        A<-obj$A
        error<--apply(data.frame(n=2:dim(A)[2]),1,function(x) A[,x,] %*% obj$xsi$se.xsi)
        if(!is.data.frame(error)) error<-as.data.frame(error)
        colnames(error)<-paste0("se.cat.",1:ncol(error))
        item.params<-cbind(item.params,error)
      }
      item.params
    },
    "2"={#"eRm"
      library(reshape2)
      m<-melt(obj$betapar)
      m<-cbind(m,matrix(unlist( strsplit(sub("^beta ","",names(obj$betapar)),split = "\\.c")),ncol=2,byrow = T))
      colnames(m)<-c("beta","item","category")
      m$category<-strtoi(m$category)
      beta<-dcast(data = melt(m,id.vars = c("item","category"))[,c(1,2,4)],item~category )
      item.params<-beta[,-1]
      if(include.se) {
        m<-melt(obj$se.beta)
        m<-cbind(m,matrix(unlist( strsplit(sub("beta ","",names(obj$betapar)),split = "\\.c")),ncol=2,byrow = T))
        colnames(m)<-c("beta","item","category")
        m$category<-strtoi(m$category)
        error<-dcast(data = melt(m,id.vars = c("item","category"))[,c(1,2,4)],item~category )[,-1]
        if(!is.data.frame(error)) error<-as.data.frame(error)
        colnames(error)<-paste0("se.cat.",colnames(error))
        if(!is.data.frame(item.params)) item.params<-as.data.frame(item.params)
        item.params<-cbind(item.params,error)
      }
      orig.order<-match(unique(sub("^beta (.*).c[0-9]+$","\\1",names(obj$betapar))),beta[,1])
      item.params<-if(!is.data.frame(item.params)) data.frame(beta=item.params[orig.order]) else item.params[orig.order,]
      row.names(item.params)<-beta[orig.order,1]
      attributes(item.params)$par.type<-"andersen"
      item.params
    }
  )
}

