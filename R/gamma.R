
#' Gammas of item.parameters given total score using patterns - inefficient
#'
#' @param item.params a matrix of item parameters (using the PCM parametrisation). Items in rows, threshold values in columns
#' @param R The total score for which to calculate the gamma parameter
#' @param param.type Type of item parameters given. One of pcm (RUMM2030), log.item.score (?), multiplicative (DIGRAM or RDigram, xsi (Conquest or TAM))
#'
#' @return Returns gamma for the given total score.
#' @details Always use gamma.matrix instead of gamma.pattern. It is far more efficient.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples
#' item.params<-matrix(c(1,.5,1,1,1,2,1,4),nrow=4)
#' gamma.pattern(item.params,"multiplicative",3)
gamma.pattern<-function(item.params,param.type=c("pcm","log.item.score","multiplicative","xsi"),R=0) {
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
  # Get the maximum score of the items in the response pattern xs
  maxscores<-apply(item.params,1,function(y) sum(!is.na(y))-1)
  #maxscores<-apply(as.matrix(item.params[!is.na(xs),],ncol=ncol(item.params)),1,function(y) sum(!is.na(y)))
  # Create all response patterns that result in total score R
  patterns<-all.patterns(maxscores = maxscores,target = R)
  #print(patterns)
  # Calculate the sum of the products of delta.item for these patterns and Returns a gamma for this total score (response)
  it<-0
  gamma<-sum(
    apply(patterns,1,
          function(y) {
            it<<-it+1;
            #if(it>1000) {it<--0;cat(".")}
            prod(unlist(sapply(
              1:length(y),
              function(item) { item.params[item,y[item]+1] }
            )),na.rm = T
            )
          }
    )
  )
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
#' @examples
#' maxscores<-c(1,2,3,1,2,3)
#' all.patterns(maxscores,target=5)
all.patterns<-function(maxscores=c(),target=NULL) {
  p<-internal.patterns(maxscores = maxscores,target = target)
  #cat("\n")
  if(!is.null(target)) p<-p[apply(p,1,sum)==target,]
  if(is.null(nrow(p))) p<-matrix(p,nrow = 1)
  p
}
internal.patterns<-function(maxscores=c(),pattern=matrix(data=0,ncol = length(maxscores)),i=1,target=NULL) {
  #if(verbose) cat("+")
  sums<-apply(pattern,1,sum)
  copy<-matrix(rep(pattern[if(is.null(target)) 1:nrow(pattern) else (sums<target & sums+sum(maxscores[i:length(maxscores)])>=target),],each=maxscores[i]),ncol=length(maxscores))
  copy[,i]<-rep(1:maxscores[i],times=nrow(copy)/maxscores[i])
  if(!is.null(target)) copy<-copy[apply(copy,1,sum)<=target,]
  pattern<-rbind(pattern,copy)
  if(i<length(maxscores))
    pattern<-internal.patterns(maxscores = maxscores,pattern = pattern,i = i+1,target = target)
  pattern
}
#' Gammas of item.parameters given total score using matrix calculations
#'
#' @param item.params a matrix of item parameters (using the PCM parametrisation). Items in rows, threshold values in columns
#' @param R The total score for which to calculate the gamma parameter (set to NULL to get gammas for all possible total scores)
#' @param param.type Type of item parameters given. One of pcm (RUMM2030), log.item.score (?), multiplicative (DIGRAM or RDigram, xsi (Conquest or TAM))
#'
#' @return Returns gamma for the given total score.
#' @details Always use gamma.matrix instead of gamma.pattern. It is far more efficient.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples
#' item.params<-matrix(c(1,.5,1,1,1,2,1,4),nrow=4)
#' gamma.matrix(item.params,"multiplicative",3)
gamma.matrix<-function(item.params=NULL,param.type=c("pcm","log.item.score","multiplicative","xsi"),R=NULL) {
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
  # Initializing by setting gamma to the first item parameters
  gamma<-item.params[1,!is.na(item.params[1,])]
  if(nrow(item.params)>1) {
    for(i in 2:nrow(item.params)) {
      # The outer product of gamma & the item:
      outer.product<-outer(gamma,item.params[i,!is.na(item.params[i,])])
      # Sum of diagonals - Thanks to https://stackoverflow.com/questions/27935555/get-all-diagonal-vectors-from-matrix/27935808#27935808
      # Create an indicator for all diagonals in the matrix
      d <- row(outer.product) + col(outer.product)-2
      # Use split to group on these values
      s<-split(outer.product, d)
      # Take the sum of the diagonals
      gamma<-sapply(s, sum,na.rm=T)
    }
  }
  if(is.null(R)) gamma else gamma[R+1]
}



# item.params<-as.matrix(read.csv2("~/Hentet/item.csv"))
# gamma.matrix(item.params=item.params,param.type="multiplicative")
#' Draw a plausible response based on item parameters
#'
#' @param item.params a matrix of item parameters (items in rows, thresholds in columns)
#' @param param.type Type of item parameters. One of pcm (RUMM2030), log.item.score (?), multiplicative (DIGRAM or RDigram, xsi (Conquest or TAM))
#' @param R Total score of the response
#'
#' @return Returns a plausible response pattern
#' @export
#'
#' @examples
#' item.params<-matrix(c(1,.5,1,1,1,2,1,4),nrow=4)
#' draw.plausible.response(item.params=item.params,param.type="multiplicative",R=2)
draw.plausible.response<-function(item.params,param.type=c("pcm","log.item.score","multiplicative","xsi"),R=0) {
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
  # For each item, draw a response, based on item.parameters, conditioned on the total score R
  # Prob(Xi=xi|R=r)=deltaixi*gammar-xi*(A\Xi)/gammar (eq. B.9)
  nitem<-nrow(item.params)
  x<-rep(0,nitem)
  for(i in 1:(nitem-1)) {
    thres<-!is.na(item.params[i,])
    nthres<-sum(thres)
    p<-rep(NA,nthres)
    g<-gamma.matrix(item.params = item.params[i:nitem,],param.type = "multiplicative",R = R-sum(x))
    for(j in 2:nthres) {
      # p-value
      p[j]<-(item.params[i,j]*gamma.matrix(item.params = item.params[(i+1):nitem,],param.type = "multiplicative",R=R-sum(x)-(j-1)))/g
    }
    p[1]<-max(0,1-sum(p,na.rm = T))
    xs<-sample(0:(nthres-1),size = 1,prob = p)
    x[i]<-xs
    if(sum(x)==R) break
  }
  x[nitem]<-R-sum(x)
  x
}
