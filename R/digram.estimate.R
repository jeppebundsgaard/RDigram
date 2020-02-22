#' Estimate RDigram object using TAM
#'
#' @param do A digram.object
#' @param items The items to include in the analysis
#' @param constraint Constraint on "cases" or "items"
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
digram.estimate<-function(do,items=NULL,constraint = "cases",use.package=c("TAM","eRm"),tam.control=list(),verbose=T,...) {
  use.package<-match.arg(use.package)
  tam.control$progress <- verbose
  if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
  resp<-do$recoded
  if(is.null(items)) items<-1:do$recursive.structure[1]
  if(inherits(items,"character")) items<-match(items,item.names)
  item.names<-get.variable.names(do,items)
  item.labels<-get.labels(do,items)
  if(!is.null(do$DIF)) {
    for (i in 1:nrow(do$DIF)) {
      DIFs<-do$DIF[i,]
      exoitem<-as.numeric(DIFs[2])
      exocat<-do$variables[[exoitem]]$category.names
      ncat<-do$variables[[exoitem]]$ncat
      newitems<-ncol(resp)+1:ncat
      items<-c(items,newitems)
      olditem<-as.numeric(DIFs[1])
      # Split
      nas<-rep(NA,ncat)
      resp[,newitems]<-sapply(1:nrow(resp),function(i) {
        newscores<-nas
        if(resp[i,exoitem] %in% exocat[,2]) newscores[resp[i,exoitem]]<-resp[i,olditem]
        newscores
      })

      # Combine names and labels
      olditemno<-which(items %in% olditem)
      newnames<-paste0(item.names[olditemno],"_",exocat[,2])
      item.names<-c(item.names,newnames)
      colnames(resp)[newitems]<-newnames
      item.labels<-c(item.labels,paste0(item.labels[olditemno],"_",do$variables[[exoitem]]$variable.label,1:ncat))
      item.names<-item.names[-olditemno]
      item.labels<-item.labels[-olditemno]
      # Remove item-nums
      items<-c(items[-olditemno])
    }
  }
  selected<-resp[,items]
  LDs<-rep(NA, length(items))
  if(!is.null(do$LD)) {
    mod<-switch (use.package,
      "TAM"={
          for(i in 1:length(do$LD)) {
            LDs[do$LD[[i]]]<-i
          }
          TAM::tam.fa(resp=selected,irtmodel = "bifactor1",dims=LDs,control=tam.control, constraint = constraint)
        },
      "eRm"={
        for(LDs in do$LD){
          newitem<-ncol(resp)+1
          items<-c(items,newitem)
          olditems<-which(items %in% LDs)
          # Recode
          resp[,newitem]<-apply(resp[,LDs],1,sum,na.rm=T)
          # Combine names and labels
          newname<-paste(item.names[olditems],collapse = "+")
          item.names<-c(item.names,newname)
          colnames(resp)[newitem]<-newname
          item.labels<-c(item.labels,paste(item.labels[olditems],collapse = "+"))
          item.names<-item.names[-olditems]
          item.labels<-item.labels[-olditems]
          # Remove item-nums
          items<-c(items[-olditems])
          selected<-resp[,items]
          naonly<-apply(selected,1,function(x) sum(!is.na(x))<2)
          selected<-selected[!naonly,]
        }
        eRm::PCM(X = selected)
      }
    )
  } else {
    mod<-switch (use.package,
                 "TAM"={
                   TAM::tam.mml(resp=selected,constraint = constraint, control=tam.control,...)
                 },
                 "eRm"={
                   naonly<-apply(selected,1,function(x) sum(!is.na(x))<2)
                   selected<-selected[!naonly,]
                   eRm::PCM(X = selected)
                 }
    )
  }
  if(use.package=="eRm") mod$naonly<-naonly # Use this to reintroduce cases without thetas...
  mod
}
