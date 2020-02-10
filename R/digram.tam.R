#' @references
#' Wang, W.-C., & Wilson, M. (2005). The Rasch Testlet Model. *Applied Psychological Measurement*, 29(2), 126–149. https://doi.org/10.1177/0146621604271053
#' Brandt, S. (2008). Estimation of a Rasch model including subdimensions. In *IERI Monograph Series Volume 1. Issues and Methodologies in Large-Scale Assessments*. http://www.ierinstitute.org/dissemination-area.html
#' Bao, H., Gotwals, A. W., & Mislevy, R. J. (2006). *Assessing local item dependence in building explanation tasks* (PADI Technical Report 14). Menlo Park, CA: SRI International.

digram.tam<-function(do,items=NULL,modeltype="PCM2",...) {
  if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
  resp<-do$recoded
  # REMPOVE!!!
  items<-NULL
  if(is.null(items)) items<-1:do$recursive.structure[1]
  if(inherits(items,"character")) items<-match(items,item.names)
  item.names<-get.variable.names(do,items)
  item.labels<-get.labels(do,items)
  # The scoring matrix contains information on Local Dependent items
  if(!is.null(do$LD)) {
    for(LDs in do$LD){
      newitem<-ncol(resp)+1
      items<-c(items,newitem)
      olditems<-which(items %in% LDs)
      # Recode
      resp[,newitem]<-apply(resp[,LDs],1,sum)
      newname<-paste(item.names[olditems],collapse = "+")
      item.names<-c(item.names,newname)
      colnames(resp)[newitem]<-newname
      item.labels<-c(item.labels,paste(item.labels[olditems],collapse = "+"))
    }
  }
  selected<-resp[,items]#na.omit()
  maxscores<-apply(selected,2,max,na.rm=T)
  mats<-designMatrices(resp = selected)
  B<-mats$B
  A<-mats$A
  if(!is.null(do$LD)) {
    newitem<-length(items)-length(do$LD)
    for(LDs in do$LD){
      newitem<-newitem+1
      oldA3<-dim(A)[3]
      # The super item should have all possible response patterns of the ingoing items
      maxscoresLD<-maxscores[LDs]
      patterns<-all.patterns(maxscores=maxscoresLD)
      patterns<-patterns[do.call(order, as.data.frame(patterns)),]
      # TODO: Name it to the items...
      # testlet<-"testlet"
      npattern<-nrow(patterns)
      newcols<-(npattern-ncol(B))
      if(newcols>0) newcolnames<-paste0("Cat",ncol(B):(ncol(B)+newcols-1))
      dimension<-1
      # TODO: Expand to cope with multidimensions...
      # Add to scoring matrix
      if(newcols>0) {
        B1<-matrix(rep(0,newcols*nrow(B)))
        dim(B1)<-c(nrow(B),newcols,1)
        dimnames(B1)[[2]]=newcolnames
        B<-Abind(B,B1,along=2)
      }
      LDpatterns<-matrix(c(apply(patterns,1,sum)))
      B[newitem,,dimension]<-LDpatterns
      #dim(B1)<-c(1,ncol(B),1)
      #dimnames(B1)[[1]] = list(testlet)
      #B<-Abind(B,B1,along = 1)
      # Create the desing matrix for the super item
      # Add new item to all existing
      #A<-Abind(A,matrix(rep(0,dim(A)[2]*dim(A)[3]),nrow=dim(A)[2]),along = 1)
      #dimnames(A)[[1]][dim(A)[1]]<-paste0("Item",ifelse(dim(A)[1]<10,"0",""),dim(A)[1])
      # Add super item categories
      # A<-
      numAcols<-prod(maxscoresLD+1)-1
      newAcols<-numAcols-dim(A)[[2]]+1
      print(numAcols)
      if(newcols>0) {
        # Add rows in A for all patterns
        A1<-matrix(rep(NA,(newcols)*dim(A)[1]*dim(A)[3]))
        dim(A1)<-c(dim(A)[1],(newcols),dim(A)[3])
        dimnames(A1)[[2]]<-paste0("Category",(npattern-(newcols)):(npattern-1))
        A<-Abind(A,A1,along = 2)
      }
      if(newAcols>0) {
        # Add super item x categories
        A2<-matrix(rep(0,dim(A)[1]*dim(A)[2]*(newAcols)))
        dim(A2)<-c(dim(A)[1],dim(A)[2],(newAcols))
        dimnames(A2)[[3]]<-paste0(item.names[newitem],"_Cat",(numAcols-newAcols+1):numAcols)
        A<-Abind(A,A2,along = 3)
        A[newitem,,]<-rep(0,dim(A)[2]*dim(A)[3])
      }
      # Go through patterns and add to design matrix
      for(i in 1:npattern) {
        x<-patterns[i,]
        print(x)
        a<-c()
        for(j in 1:length(x)) {
          a1<-if(j<length(x) && x[j]>0) rep(0,maxscoresLD[(j+1)]*x[j]) else c()
          a2<-c(rep(-1,x[j]))
          a3<-if(j>1) c(rep(0,maxscoresLD[j-1]-x[j])) else c()
          a<-c(a,a1,a2,a3)
        }
        a0<-rep(0,npattern-length(a)-1)
        a<-c(0,a,a0)
        print(a)
        print(dim(A)[3]-npattern+i)
        A[newitem,,dim(A)[3]-numAcols+i-1]<-a
      }
    }
  }
  w<-tam.mml(resp = selected,irtmodel = modeltype,A = A,B=B)
  wo<-tam.mml(resp = selected)
}
