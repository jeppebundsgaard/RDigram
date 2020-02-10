#' Estimate RDigram object using TAM
#'
#' @param do A digram.object
#' @param items The items to include in the analysis
#'
#' @return Returns a TAM result object
#' @export
#' @seealso tam.mml(), tam.fa()
#' @references
#' Wang, W.-C., & Wilson, M. (2005). The Rasch Testlet Model. *Applied Psychological Measurement*, 29(2), 126–149. https://doi.org/10.1177/0146621604271053
#' Rijmen, F. (2009). *Three multidimensional models for testlet-based tests: Formal relations and an empirical comparison*. ETS Research Report Series, 2009(2), i–13. https://doi.org/10.1002/j.2333-8504.2009.tb02194.x

#' @examples
#' data(DHP)
#' do<-DHP
#' mod1<-digram.estimate(do)
#' summary(mod1)
#' do2<-code.LD(do,"ef")
#' mod2<-digram.estimate(do2)
#' summary(mod2)
#' mod1$deviance
#' mod2$deviance
#' mod1$deviance-mod2$deviance
digram.estimate<-function(do,items=NULL) {
  if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
  resp<-do$recoded
  if(is.null(items)) items<-1:do$recursive.structure[1]
  if(inherits(items,"character")) items<-match(items,item.names)
  item.names<-get.variable.names(do,items)
  item.labels<-get.labels(do,items)
  selected<-resp[,items]
  LDs<-rep(NA, length(items))
  if(!is.null(do$LD)) {
    for(i in 1:length(do$LD)) {
      LDs[do$LD[[i]]]<-i
    }
    mod<-tam.fa(resp=selected,irtmodel = "bifactor1",dims=LDs)
  } else {
    mod<-tam.mml(resp=selected)
  }
  mod
}
