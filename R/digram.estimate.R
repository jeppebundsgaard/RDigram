#' Estimate RDigram object using TAM
#'
#' @param do A digram.object
#' @param items The items to include in the analysis
#' @param groups Names or column numbers of exogenous variables to use for grouping in TAM. If more names are given, all combinations of values are calculated and used as grouping variables.
#' @param ncases Number of cases to sample for the estimation (0 uses all cases)
#' @param constraint Constraint on "cases" or "items"
#' @param use.package Which R package to use for the estimation. TAM and eRm are implemented.
#' @param collapse.testlets Testlets are estimated using a bifactorial model in TAM and a data matrix in eRm. Setting collapse.testlets to TRUE calculates super-items instead and estimate a normal polytomous model.
#' @param init.model In TAM, the model that was output from an earlier estimation can be used to set sensible init-values for the estimation.
#' @param tam.control Use this to set control parameters in TAM estimation.
#' @param sum0 Set to TRUE if you want eRm to sum the parameters to 0. If FALSE the first parameter is set to 0.
#' @param verbose Set to TRUE to get information about the estimation progress.
#'
#' @return Returns a TAM result object
#' @details
#' Uses either the package TAM or eRm to estimate the model.
#' If items have been coded as testlets, a bifactorial model is used in TAM ([tam.fa()]). Otherwise [tam.mml()] is used for estimation.
#' In eRm, testlets are managed by creating an interaction parameter between the testlet items. In this case [LPCM()] is used for estimation. This is also the case, if groups are provided. Otherwise [PCM()] is used for estimation.
#' @export
#' @seealso [tam.mml()], [tam.fa()], [PCM()], [LPCM()]
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
digram.estimate<-function(do,items=NULL,groups=NULL,ncases=0,constraint = "cases",use.package=c("TAM","eRm"),collapse.testlets=F,init.model=NULL,tam.control=list(),sum0=T,verbose=T,...) {
  use.package<-match.arg(use.package)
  if(use.package=="TAM") tam.control$progress <- verbose
  if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
  resp<-do$recoded
  if(ncases>0) resp<-resp[sample(1:nrow(resp),ncases),]
  if(is.null(items)) items<-1:do$recursive.structure[1]
  items<-get.column.no(do,items)

  item.names<-get.variable.names(do,items)
  #if(inherits(items,"character")) items<-match(items,item.names)
  item.labels<-get.labels(do,items)
  if(!is.null(do$split)) {
    for (i in 1:nrow(do$split)) {
      splits<-do$split[i,]
      olditem<-as.numeric(splits[1])
      if(length(olditem %in% items)>0) {
        exoitem<-as.numeric(splits[2])
        exocat<-do$variables[[exoitem]]$category.names
        ncat<-do$variables[[exoitem]]$ncat
        newitems<-ncol(resp)+1:ncat
        items<-c(items,newitems)
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
  }
  if(!is.null(init.model)) {
    xsi.inits=matrix(c(1:nrow(init.model$xsi),init.model$xsi$xsi),ncol = 2)
    variance.inits=init.model$variance
  }  else {xsi.inits<-variance.inits<-NULL}
  selected<-resp[,items]
  if(!is.null(groups)) {
    groupitems<-get.column.no(do,groups)
    #cats<-sapply(groupitems,function(i) {do$variables[[i]]$category.names[do$variables[[i]]$category.names$Category %in% c(do$variables[[i]]$cutpoints,do$variables[[i]]$maximum),"Name"][do$recoded[,i]+1]})
    cats<-sapply(groupitems,function(i) {do$variables[[i]]$category.names$Name[do$recoded[,i]+1]})
    group<-do.call("paste",c(as.data.frame(cats),sep="_"))
    group[apply(cats,1,function(x) any(is.na(x)))]<-"NA"
    if(any(table(group)<2)) stop(paste("There is only one member of group(s):",paste(names(table(group))[table(group)<2],collapse = ", ")))

  } else group<-NULL

  testlets<-rep(NA, length(items))
  if(!is.null(do$testlets)) {
    if(collapse.testlets) {
      for(testlet in do$testlets){
        olditems<-which(items %in% testlet$testlet)
        if(length(olditems)>0) {
          newitem<-ncol(resp)+1
          items<-c(items,newitem)
          # Recode
          resp[,newitem]<-apply(resp[,testlet$testlet],1,sum)#,na.rm=T)
          # Combine names and labels
          newname<-testlet$name
          item.names<-c(item.names,newname)
          colnames(resp)[newitem]<-newname
          item.labels<-c(item.labels,testlet$label)
          item.names<-item.names[-olditems]
          item.labels<-item.labels[-olditems]
          # Remove item-nums
          items<-c(items[-olditems])
          selected<-resp[,items]
          if(use.package!="TAM") {
            naonly<-apply(selected,1,function(x) sum(!is.na(x))<2)
            selected<-selected[!naonly,]
          }
        }
      }
    }
    mod<-switch (use.package,
      "TAM"={
          if(collapse.testlets) {
            TAM::tam.mml(resp=selected,group = group,xsi.inits = xsi.inits,variance.inits = variance.inits,constraint = constraint, control=tam.control,...)
          } else {
            for(i in 1:length(do$testlets)) {
              testlets[do$testlets[[i]]$testlet]<-i
            }
            TAM::tam.fa(resp=selected,irtmodel = "bifactor1",dims=testlets,xsi.inits = xsi.inits,variance.inits = variance.inits,control=tam.control)#, constraint = constraint)
          }
        },
      "eRm"={
          if(collapse.testlets) {
            if(!is.null(group)){
              group<-as.numeric(as.factor(group))
              eRm::LPCM(X=selected,groupvec = group,sum0 = sum0)
            } else
              eRm::PCM(X = selected,sum0=sum0)
          } else {
            W<-build_digram_W(do,items,item.labels,sum0 = sum0)
            # Remove item-nums
            #items<-c(items[-olditems])
            #selected<-resp[,items]
            naonly<-apply(selected,1,function(x) sum(!is.na(x))<2)
            selected<-selected[!naonly,]
            if(is.null(group)) group<-1
            eRm::LPCM(X = selected,W = W,groupvec = group,sum0=sum0)
          }
      }
    )
  } else {
    mod<-switch (use.package,
                 "TAM"={
                   TAM::tam.mml(resp=selected,group = group,xsi.inits = xsi.inits,variance.inits = variance.inits,constraint = constraint, control=tam.control,...)
                 },
                 "eRm"={
                   naonly<-apply(selected,1,function(x) sum(!is.na(x))<2)
                   selected<-selected[!naonly,]
                   eRm::PCM(X = selected,sum0=sum0)
                 }
    )
  }
  if(use.package=="eRm") mod$naonly<-naonly # Use this to reintroduce cases without thetas...
  mod
}
build_digram_W<-function(do,items,item.labels,sum0=T) {
  itemcols<-sapply(items,function(x) do$variables[[x]]$ncat)-1
  nitemcols<-sum(itemcols)
  nitems<-length(items)
  W<-matrix(rep(0,(nitemcols-1)*nitemcols),nrow = nitemcols)
  colnames(W)<-paste("eta",1:(nitemcols-1))
  rownames(W)<-paste("beta",sapply(1:nitems,function(x) paste0(item.labels[x],".c",1:itemcols[x])))
  W[1,]<-ifelse(sum0,-1,0)
  W[2:nitemcols,]<-c(rep(c(1,rep(0,nitemcols-1)),nitemcols-2),1)
  for(testlet in do$testlets){
    newitem<-max(items)+1 # or ncol(resp)+1
    items<-c(items,newitem)
    olditems<-which(items %in% testlet$testlet)
    newitemcols<-1#max(itemcols[olditems])
    # Combine names and labels
    newname<-paste(item.labels[olditems],collapse = "+")
    new.W<-matrix(rep(0,nitemcols*newitemcols),ncol = newitemcols)
    #new.W.rows<-matrix(rep(0,((nitemcols-1)+newitemcols)*newitemcols),nrow = newitemcols)
    colnames(new.W)<-paste("eta",(ncol(W)+1):(ncol(W)+newitemcols))
    #rownames(new.W.rows)<-paste("beta",paste0(newname,".c",1:newitemcols))
    #new.W[1,]<-ifelse(sum0,-1,0)
    for(i in 1:length(testlet$testlet)) {
      f<-sum(itemcols[1:(olditems[i]-1)])+1
      #A column for each category: new.W[f:(f+itemcols[olditems[i]]-1),]<-c(rep(c(1,rep(0,itemcols[olditems[i]])),itemcols[olditems[i]]-1),1)
      # A column for testlet. Values: 1:ncat
      new.W[f:(f+itemcols[olditems[i]]-1),]<-rep(1,itemcols[olditems[i]])

    }
    W<-cbind(W,new.W)
    #W<-rbind(W,new.W.rows)
  }
  W
}
