
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
  if(nrow(item.params)==0) return(1)
  # Initializing by setting gamma to the first item parameters - this is also a special case.
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

#' Draw plausible responses based on item parameters
#'
#' @param item.params a matrix of item parameters (items in rows, thresholds in columns)
#' @param param.type Type of item parameters. One of pcm (RUMM2030), log.item.score (?), multiplicative (DIGRAM or RDigram, xsi (Conquest or TAM))
#' @param R Total score of the response
#' @param num.responses The number of response patterns to return
#'
#' @return Returns a matrix of plausible response patterns
#' @export
#'
#' @examples
#' item.params<-matrix(c(1,.5,1,1,1,2,1,4),nrow=4)
#' draw.plausible.response(item.params=item.params,param.type="multiplicative",R=2,num.responses=10)
draw.plausible.response<-function(item.params,param.type=c("pcm","log.item.score","multiplicative","xsi"),R=0,num.responses=1) {
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
 #  Needed? Helpful? Maybe...
  # item.params<-item.params[,apply(item.params,2,function(x) any(!is.na(x)))]

  #item.params<-apply(item.params,1:2,function(x) ifelse(is.na(x),0,x))
  # For each item, draw a response, based on item.parameters, conditioned on the total score R
  # Prob(Xi=xi|R=r)=deltaixi*gammar-xi*(A\Xi)/gammar (eq. B.9)
  # print(item.params)
  nitem<-nrow(item.params)
  # Get all possible gs from 1 to nitem-1
  gs<-matrix(rep(0,(nitem)*(R+1)),nrow=nitem)
  for(i in 1:(nitem)) {
    gs[i,]<-gamma.matrix(item.params = item.params[i:nitem,],param.type = "multiplicative")[1:(R+1)]
  }
  # Reuse gammas
  gms<-matrix(rep(NA,(nitem-1)*(R-1)),nrow=nitem-1)
#  print(gs)
  x<-matrix(rep(0,nitem*num.responses),nrow=num.responses)
  for(k in 1:num.responses) {
    for(i in 1:(nitem-1)) {
      ncat<-sum(item.params[i,]>0,na.rm = T)
      p<-rep(0,ncat)
      g<-gs[i,R+1-sum(x[k,])]
      for(j in 2:ncat) {
        # p-value
        r<-R-sum(x[k,])-(j-1)
        if(r>0) {
          if(!is.na(gms[i,r])) {gm<-gms[i,r]}
          else {
            gm<-gamma.matrix(item.params = item.params[(i+1):nitem,],param.type = "multiplicative",R=r)
            gms[i,r]<-gm
          }
          p[j]<-item.params[i,j]*gm/g #
        }
      }
      p[1]<-max(0,1-sum(p,na.rm = T))
      p[is.na(p)]<-0
      x[k,i]<-sample(ncat,size = 1,prob = p)-1
      while(sum(x[k,])>R) {x[k,i]<-x[k,i]-1}
      if(sum(x[k,])==R) break
    }
    x[k,nitem]<-R-sum(x[k,])
    if(x[k,nitem]>sum(item.params[nitem,]>0,na.rm = T)) print("WARNING!!!")
  }
  x
}
conditional.item.prob<-function(item=1,item.params,param.type=c("pcm","log.item.score","multiplicative","xsi"),R=0) {
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
  item.param<-item.params[item,]
  ncat<-sum(item.param>0,na.rm = T)
  sapply(1:min(R,ncat),function(x) item.param[x]*gamma.matrix(item.params = item.params[-item,],param.type = "multiplicative",R = R-x))/gamma.matrix(item.params = item.params,param.type = "multiplicative",R = R)
}
conditional.cumul.prob<-function(item=1,item.params,param.type=c("pcm","log.item.score","multiplicative","xsi"),Ri=0,R=0) {
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
  cond.cumul.prob<-function(x) {(gamma.matrix(item.params = item.params[1:item,],param.type = "multiplicative",R = x)*gamma.matrix(item.params = item.params[-(1:item),],param.type = "multiplicative",R = R-x))/gamma.matrix(item.params = item.params,param.type = "multiplicative",R = R)}
  prob<-cond.cumul.prob(Ri)
  p<-sum(sapply(0:Ri,cond.cumul.prob),na.rm = T)
  q<-sum(sapply(Ri:R,cond.cumul.prob),na.rm = T)
  list(conditional.cumul.prob=prob,p=p,q=q)
}
