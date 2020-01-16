#' Person fit based on response pattern
#'
#' @param do a DIGRAM object
#' @param resp a matix of responses (if no DIGRAM object is supplied)
#' @param items a vector of items to use
#' @param item.params a matrix of item parameters (using the PCM parametrisation). Items in rows, threshold values in columns
#'
#' @return Returns a list of results for each respondent, consisting of response pattern probability and the sum of probabilities of patterns with lower probability (montecarlo probability)
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples person.fit.pattern(do=do, item.params=item.params)
person.fit.pattern<-function(do=NULL,resp=NULL,items=NULL,item.params=matrix()) {
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
  } else {
    if(is.null(items)) items<-1:ncol(resp)
  }
  resp<-resp[,items]
  if(class(item.params)!="matrix") item.params<-as.matrix(item.params)
  respondent<-0
  apply(resp,1,function(xs) {
    respondent<<-respondent+1
    nona<-!is.na(xs)
    if(sum(nona)==0) {
      warning("A respondent didn't have any responses")
    } else {
      xs<-xs[nona]
      xs.item.params<-item.params[which(nona),]
      print(paste("Respondent number",respondent,". Number of items",length(xs)))
      ds<-delta.pattern(xs,xs.item.params)
      g<-gamma.pattern(xs,xs.item.params)
      # Probability of actual response pattern
      prob.actual<-prod(ds,na.rm = T)/g$gamma
      # All pattern probabilities
      prob.all<-apply(apply(g$patterns,1,delta.pattern,item.params=xs.item.params),2,prod,na.rm=T)/g$gamma
      # Sum of all pattern probabilities smaller than ds
      montecarlo<-sum(prob.all[prob.all<=prob.actual])-prob.actual
      list(prob.actual=prob.actual,montecarlo=montecarlo)
    }
  })
}
#' deltas calculated from betas of the PCM of items in a given response pattern
#'
#' @param xs the response pattern (x1 ... xk)
#' @param item.params a matrix of item parameters (using the PCM parametrisation). Items in rows, threshold values in columns
#'
#' @return Returns the deltas in the power series parametrization of the Rasch Model from betas from the Partial Credit Model.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples delta.pattern(xs,item.params)
delta.pattern<-function(xs,item.params) {
  if(class(item.params)!="matrix") item.params<-matrix(item.params,ncol=1)
  # Returns a matrix of deltas for all items in this response pattern
  sapply(1:length(xs),function(item) delta.item(xs[item],item.params[item,]))
}
#' delta calculated from beta of the PCM
#'
#' @param x response (score on the item)
#' @param single.item.params a vector of item parameters (thresholds) (using the PCM parametrisation)
#'
#' @return Returns delta
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.

#' @examples delta.igem(x,single.item.params)
delta.item<-function(x,single.item.params) {
  sigma.pattern<-ifelse(x>0,-sum(single.item.params[1:x]),0)
  # Returns a delta for this person, this item
  exp(sigma.pattern)
}
#' Gamma of a given pattern
#'
#' @param xs A response pattern (scores on a series of items)
#' @param item.params a matrix of item parameters (using the PCM parametrisation). Items in rows, threshold values in columns
#'
#' @return Returns gamma.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples gamma.pattern(xs,item.params)
gamma.pattern<-function(xs,item.params) {
  if(class(item.params)!="matrix") item.params<-matrix(item.params,ncol=1)
  # Get the maximum score of the items in the response pattern xs
  maxscores<-apply(as.matrix(item.params[!is.na(xs),],ncol=ncol(item.params)),1,function(y) sum(!is.na(y)))
  # Total score R of the response
  R<-sum(xs,na.rm = T)
  # Create all response patterns that result in total score R
  patterns<-all.patterns(maxscores = maxscores,target = R)
  # Calculate the sum of the products of delta.item for these patterns and Returns a gamma for this total score (response)
  it<-0
  gamma<-sum(
    apply(patterns,1,
      function(y) {
        it<<-it+1;
        if(it>1000) {it<--0;cat(".")}
        prod(sapply(
            1:length(y),
            function(item) { delta.item(single.item.params = item.params[item,],x=y[item])}
          )
        )
      }
    )
  )
  cat("\n")
  list(gamma=gamma,patterns=patterns)
}
#' Create all possible response patterns
#'
#' @param maxscores vector of max scores
#' @param target target total score
#'
#' @return Returns a matrix of all possible patterns. If target is set, only patterns with at total score of target is returned
#' @details This function quickly gets very resource intensive. On a 8 core, 16 GB computer, 25 items are too heavy.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples all.patterns(maxscores,target=5)
all.patterns<-function(maxscores=c(),target=NULL) {
  p<-internal.patterns(maxscores = maxscores,target = target)
  cat("\n")
  if(!is.null(target)) p<-p[apply(p,1,sum)==target,]
}
internal.patterns<-function(maxscores=c(),pattern=matrix(data=0,ncol = length(maxscores)),i=1,target=NULL) {
  cat("+")
  sums<-apply(pattern,1,sum)
  copy<-matrix(rep(pattern[if(is.null(target)) 1:nrow(pattern) else (sums<target & sums+sum(maxscores[i:length(maxscores)])>=target),],each=maxscores[i]),ncol=length(maxscores))
  copy[,i]<-rep(1:maxscores[i],times=nrow(copy)/maxscores[i])
  if(!is.null(target)) copy<-copy[apply(copy,1,sum)<=target,]
  pattern<-rbind(pattern,copy)
  if(i<length(maxscores))
    pattern<-internal.patterns(maxscores = maxscores,pattern = pattern,i = i+1,target = target)
  pattern
}
