#' Person fit based on response pattern
#'
#' @param do a DIGRAM object
#' @param resp a matix of responses (if no DIGRAM object is supplied)
#' @param items a vector of items to use
#' @param item.params a matrix of item parameters. Items in rows, thresholds in columns
#' @param param.type Type of item parameters given. One of pcm (RUMM2030), log.item.score (?), multiplicative (DIGRAM or RDigram, xsi (Conquest or TAM))
#' @param num.montecarlo the number of iterations if the calculation of alternative patterns should be done using a montecarlo solution. 0 to do all patterns.
#' @param verbose set to TRUE if you want to follow the progression
#' @return Returns a list of results for each respondent, consisting of response pattern probability and the p-value of getting this pattern or a pattern of lower probability.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Jeppe Bundsgaard & Svend Kreiner (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#' @examples
#' data(DHP)
#' person.fit.pattern(do=DHP, item.params=item.params)
person.fit.pattern<-function(do=NULL,resp=NULL,items=NULL,item.params=matrix(),param.type=c("pcm","log.item.score","multiplicative","xsi"),num.montecarlo=0,verbose=T) {
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    if(is.null(items)) items<-1:do$recursive.structure[1]
  } else {
    if(is.null(items)) items<-1:ncol(resp)
  }
  resp<-resp[,items]
  item.params<-item.param.convert(item.params = item.params,from = param.type,to = "multiplicative")
  respondent<-0
  res<-t(apply(resp,1,function(xs) {
    respondent<<-respondent+1
    nona<-!is.na(xs)
    if(sum(nona)==0) {
      warning("A respondent didn't have any responses")
    } else {
      xs<-xs[nona]
      R<-sum(xs)
      resp.item.params<-item.params[nona,]
      if(verbose) cat(paste0("\nRespondent number: ",respondent,". Number of items: ",length(xs),". Total score: ",R,"\n"))
      # Gamma calculation
      if(num.montecarlo>0) {
        g<-gamma.matrix(item.params = resp.item.params,param.type = "multiplicative", R = R)
        #print(R)
        # Probability of actual response pattern
        conditional.probability<-prod(xs.item.params(xs,resp.item.params),na.rm = T)/g
        #print(conditional.probability)
        is.smaller<-0
        for(i in 1:num.montecarlo) {
          pattern<-draw.plausible.response(item.params = resp.item.params,param.type = "multiplicative",6)
          #print(pattern)
          # Probability of response pattern
          draw.probalility<-prod(xs.item.params(pattern,resp.item.params),na.rm = T)/g
          #print(draw.probalility)
          is.smaller<-is.smaller+as.numeric(draw.probalility<conditional.probability)
          #print(is.smaller)
        }
        p.val<-is.smaller/num.montecarlo
      } else {
        gp<-gamma.pattern(item.params = resp.item.params,param.type = "multiplicative", R = R)
        g<-gp$gamma
        # All pattern probabilities
        prob.all<-apply(apply(gp$patterns,1,xs.item.params,item.params=resp.item.params),2,prod,na.rm=T)/g
        # Probability of actual response pattern
        conditional.probability<-prod(xs.item.params(xs,resp.item.params),na.rm = T)/g

        # Sum of all pattern probabilities smaller than ds
        p.val<-sum(prob.all[prob.all<=conditional.probability])-conditional.probability

      }
      r<-c(conditional.probability=conditional.probability,p.val=p.val)
      if(verbose) print(r)
    }
  }))
  res
}
xs.item.params<-function(xs,item.params) {
  sapply(
    1:length(xs),
    function(item) { item.params[item,xs[item]+1] }
  )
}
