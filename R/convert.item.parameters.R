
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
xsi.to.multiplicative<-function(item.params) {
  # Returns a matrix of deltas for all items
  # Set the beta of score 0 to 0
  item.params<-cbind(data.frame(xsi.0=rep(0,nrow(item.params))),item.params)
  # Get the deltas
  t(apply(item.params,1,delta.item))
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
#' delta.item(single.item.params=single.item.params)
delta.item<-function(single.item.params) {
  sigma<--sapply(1:length(single.item.params),function(i) sum(single.item.params[1:i]))
  # Returns deltas for this item
  sapply(sigma,exp)
}

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
         stop("This conversion is not implemented yet, sorry!")
  )
}
