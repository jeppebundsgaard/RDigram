# Tau
tau.to.beta<-function(item.params) {

}
tau.to.andersen<-function(item.params) {

}
tau.to.multiplicative<-function(item.params) {

}
tau.to.xsi<-function(item.params) {

}
# Beta
beta.to.tau<-function(item.params) {

}
beta.to.andersen<-function(item.params) {

}
beta.to.multiplicative<-function(item.params) {

}
beta.to.xsi<-function(item.params) {

}
#Andersen
andersen.to.tau<-function(item.params) {

}
andersen.to.beta<-function(item.params) {

}
andersen.to.multiplicative<-function(item.params) {

}
andersen.to.xsi<-function(item.params) {

}
# multiplicative
multiplicative.to.tau<-function(item.params) {

}
multiplicative.to.beta<-function(item.params) {

}
multiplicative.to.andersen<-function(item.params) {

}

multiplicative.to.xsi<-function(item.params) {
  # Remove the 1's
  item.params<-item.params[,-1]
  # Get the xsi's
  t(apply(item.params,1,delta.to.xsi.item))
}

# xsi
xsi.to.tau<-function(item.params=NULL,tamobj=NULL) {
  if(!is.null(tamobj)) {
    item.params.and.se<-get.thresholds(tamobj = tamobj)
    item.params<-item.params.and.se[,1:(ncol(item.params.and.se)/2)]
  }

}
xsi.to.beta<-function(item.params=NULL,tamobj=NULL) {
  if(!is.null(tamobj)) {
    item.params.and.se<-get.thresholds(tamobj = tamobj)
    item.params<-item.params.and.se[,1:(ncol(item.params.and.se)/2)]
  }

}
xsi.to.andersen<-function(item.params=NULL,tamobj=NULL) {
  if(!is.null(tamobj)) {
    item.params.and.se<-get.thresholds(tamobj = tamobj)
    item.params<-item.params.and.se[,1:(ncol(item.params.and.se)/2)]
  }
  # Returns a matrix of deltas for all items
  # Set the beta of score 0 to 0
  # Get the sigmas
  t(apply(item.params,1,xsi.to.sigma.item))
}
#' Multiplicative item parameters calculated from xsi parameters (so-called Conquest parametrization)
#'
#' @param item.params a matrix of xsi item parameters (using the Conquest parametrisation). Items in rows, threshold values in columns
#'
#' @return Returns the deltas in the power series parametrization (e.g. from DIGRAM) of the Rasch Model from the xsi parameters (e.g. from Conquest and TAM).
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples
#' item.params<-matrix(c(1,.5,1,1,1,2,1,4),nrow=4)
#' xsi.to.multiplicative(item.params)
xsi.to.multiplicative<-function(item.params=NULL,tamobj=NULL,standardize=c("none","items","cases")) {
  standardize<-match.arg(standardize)
  print(standardize)
  if(!is.null(tamobj)) {
    item.params.and.se<-get.thresholds(tamobj = tamobj)
    item.params<-item.params.and.se[,1:(ncol(item.params.and.se)/2)]
  }
  # Returns a matrix of deltas for all items
  # Set the beta of score 0 to 0
  # item.params<-cbind(data.frame(xsi.0=rep(0,nrow(item.params))),item.params)
  # Get the deltas
  unstandardized<-t(apply(item.params,1,xsi.to.delta.item))
  switch(standardize,
          "items"={
            #In case of no LD and DIF, standidardize by setting sum of thresholds to 0
            sumofall<-sum(unstandardized)
            correction<-sumofall/sum(!apply(unstandardized,1:2,is.na))
            standardized<-unstandardized-correction},
          "cases"=standardized<-unstandardized, # No idea how to do this correctly
          "none"=standardized<-unstandardized
         )
  cbind(
    data.frame(xsi.0=rep(1,nrow(item.params))),
    standardized
    )
}

#' Delta of the multiplicative parametrization calculated from xsi parameter(Conquest parametrization)
#'
#' @param single.item.params a vector of item parameters (thresholds) (using the Conquest parametrisation)
#'
#' @return Returns delta
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.

#' @examples
#' single.item.params<-c(1,1,0.5)
#' xsi.to.delta.item(single.item.params=single.item.params)
#' xsi.to.delta.item(single.item.params=single.item.params)
xsi.to.delta.item<-function(single.item.params) {
  #sigma<--sapply(1:length(single.item.params),function(i) sum(single.item.params[1:i]))
  # Returns deltas for this item
  #sapply(sigma,exp)
  sigma.to.delta(xsi.to.sigma.item(single.item.params))
}
xsi.to.sigma.item<-function(single.item.params) {
  single.item.params<-single.item.params[!is.na(single.item.params)]
  mean.param<-mean(as.numeric(single.item.params))
  sapply(1:length(single.item.params),function(i) mean.param+single.item.params[i])
}
sigma.to.delta<-function(sigma) {
# Returns deltas for this item
  sapply(sigma,exp)
}
delta.to.xsi.item<-function(single.item.params) {
  sigma<-sapply(single.item.params,log)
  -sapply(1:length(sigma),function(i) sigma[i]-ifelse(i==1,0,sigma[i-1]))
}
# Interpreting Ordered Partition Model Parameters from ConQuest. Nathaniel J. S. Brown. October 2004
#andersen.to.xsi.item<-function(single.item.params) {
#   item.levels<-length(single.item.params)
#   item.param<-1/item.levels*sum(single.item.params)
#   single.item.params-item.param
# }

#' Convert item parameters from one parametrization to another
#'
#' @param item.params a matrix of item parameters (items in rows, thresholds in columns)
#' @param from,to Type of item parameters. One of pcm (RUMM2030), log.item.score (?), multiplicative (DIGRAM or RDigram, xsi (Conquest or TAM))
#'
#' @return Returns item parameters in the parametrization specified in *to*
#' @export
#'
#' @examples
#' item.params<-matrix(c(0,1,2,3,1,2,3,4),nrow=4)
#' item.param.convert(item.params=item.params,from="xsi",to="multiplicative")
item.param.convert<-function(item.params=c(),from=c("pcm","log.item.score","multiplicative","xsi"),to=c("pcm","log.item.score","multiplicative","xsi")) {
  if(class(item.params)=="numeric") item.params<-matrix(item.params,nrow=1)
  if(!inherits(item.params,"matrix")) item.params<-as.matrix(item.params)
  from<-match.arg(from)
  to<-match.arg(to)
  if(from==to) return(item.params)
  switch(from,
         "xsi"=switch(to,
                      "multiplicative"=xsi.to.multiplicative(item.params = item.params),
                      stop("This conversion is not implemented yet, sorry!")
         ),
         "multiplicative"=switch(to,
                      "xsi"=multiplicative.to.xsi(item.params = item.params),
                      stop("This conversion is not implemented yet, sorry!")
         ),
         stop("This conversion is not implemented yet, sorry!")
  )
}
