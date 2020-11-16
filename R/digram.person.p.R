#' Calculate probability of correct answer to item categories
#'
#' @param mod A TAM or eRm model
#' @param item.params Item params in PCM parametrization in a matrix or data.frame with categories in columns and persons in rows. If left out, item.params is taken from mod.
#' @param thetas A vector of person parameters. If left out, thetas is calculated from the model. It takes time on large datasets.
#' @param ns A vecor of the same length as thetas. Number of persons having each theta. If left out, it is counted from mod or set to 1.
#'
#' @return Returns a list of unique thetas, consisting of Theta, Npers (number of persons having this theta) and a data.frame of probabilities of getting points in the category.
#' @export
#'
#' @examples
#' mod<-digram.estimate(DHP)
#' p<-digram.person.p(mod)
digram.person.p<-function(mod,item.params=NULL,thetas=NULL,ns=NULL) {
  if(is.null(thetas)) {
    if(inherits(mod,"tam.mml")) {
      wle<-TAM::tam.wle(mod)
      allthetas<-wle$theta
    } else if(inherits(mod,"eRm")) {
      pp<-eRm::person.parameter(mod)
      allthetas<-pp$theta.table$`Person Parameter`
    } else stop(paste("Don't know how to create person parameters from object of class",class(mod)[1]))
    thetas<-unique(allthetas)
    ns<-sapply(thetas,function(t) sum(allthetas==t,na.rm = T))
    thetas<-thetas[order(thetas)]
  }
  if(is.null(ns)) ns<-rep(1,length(thetas))
  if(is.null(item.params))
    item.params<-item.params.convert(mod,to="pcm")
  numerators<-lapply(thetas,function(theta) {
    theta=t(apply(array(item.params),1,function(i) {
      c(1,sapply(1:length(i),function(c) ifelse(is.na(i[c]),NA,exp(c*theta-sum(i[1:c])))))
    }))
  })
  denominators<-lapply(numerators,function(l) apply(l,1,sum,na.rm = T))
  p<-lapply(1:length(numerators),function(x) numerators[[x]]/denominators[[x]])
  p<-lapply(1:length(p),function(i) {
    pp<-p[[i]]
    colnames(pp)<-paste0("cat.",0:(ncol(pp)-1))
    list(Theta=thetas[i],Npers=ns[i],p=pp)
  })
  p
}
