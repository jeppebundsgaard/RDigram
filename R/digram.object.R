#' @title Create DIGRAM Object
#' @name digram.object
#' @description Create a digram.object.
#' @usage digram.object(project=NULL,data=data.frame(),variables=colnames(data),filter.conditions=data.frame(variable.number=numeric(0),min=numeric(0),max=numeric(0)),recursive.structure=c(),comments="")
#' @param project The name of the DIGRAM project
#' @param data The data.frame or matrix with the data
#' @param variables A vector of column names or numbers from the dataset to include in the recoded dataset (the order matters in the recoded data) *or* a list of variables each element in the form list(variable.name="",variable.label="",ncat=0,category.names=c(),variable.type=c("nominal","ordinal"),minimum=0,maximum=1,cutpoints=c())
#' @param filter.conditions A data.frame with three columns: variable.number, max, and min. Only cases, for which all values of filter variables belong to the intervals defined by the corresponding minimum and maximum values (both included), will be used in the analysis.
#' @param recursive.structure A vector of cutpoints to define the recursive blocks
#' @param comments A string
#' @export
#' @return Returns a digram.object
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples
#' library(iarm)
#' do<-digram.object(project = "desc2",data = desc2,variables = c(5:14,2:4,1),recursive.structure = c(10,13))
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
digram.object<-function(project=NULL,data=data.frame(),variables=NULL,filter.conditions=NULL,recursive.structure=NULL,comments="") {
  if(is.null(project)) stop("You need to provide a project name")
  #data<-as.data.frame(data)
  if(!inherits(variables,"list")) {
    vars<-if(is.null(variables)) colnames(data) else if(inherits(variables,c("numeric","integer"))) colnames(data)[variables] else variables
    num.vars<-length(vars)
    variables<-list()
    for(i in 1:num.vars) {
      iminus<-i-1
      numletters<-which(i<=(26^(1:5)))[1]
      variable.label<-""
      k<-iminus
      for(j in numletters:1) {r<-(k%%26);variable.label<-paste0(LETTERS[r+ifelse(numletters<2||j>1,1,0)],variable.label); k<-floor(k/26)}
      variable.name<-vars[i]
      column.number<-which(colnames(data)%in%vars[i])
      # Test for integer/float/character...
      categories<-na.omit(unique(data[,vars[i]]))
      categories<-categories[order(categories)]
      ncat<-length(categories)
      variable.type<-ifelse(inherits(categories,"numeric") | inherits(categories,"integer"),"ordinal","nominal")
      minimum<-if(variable.type=="ordinal") min(categories) else 0
      maximum<-if(variable.type=="ordinal") max(categories) else ncat-1
      # Record 2: <Minimum> <Cutpoint(1)>...<Last cutpoint> ... <Maximum>
      cutpoints<-if(variable.type=="ordinal") categories[order(categories)] else 0:(ncat-1)
      cutpoints<-cutpoints[-ncat]
      # Categories
      if(ncat<1) stop(paste("Not enough categories in",variable.name))
      category.names<-data.frame(Category=0:(ncat-1),Name=categories)
      if(class(data[,vars[i]])!="numeric") {
        r2<-paste(apply(category.names,1,function(x) {
          paste0("'",x["Name"],"'=",x["Category"])
        }),collapse = ";")
        data[,vars[i]]<-car::recode(data[,vars[i]],r2,as.numeric = T)
      }

      variables[[i]]<-list(variable.name=variable.name,variable.label=variable.label,column.number=column.number,ncat=ncat,category.names=category.names,variable.type=variable.type,minimum=minimum,maximum=maximum,cutpoints=cutpoints)
    }
  }
  recoded<-digram.recode(data,variables,filter.conditions)
  # The number of recursive blocks should appear on a separate record.
  if(is.null(recursive.structure)) recursive.structure<-length(variables)
  recursive.blocks<-length(recursive.structure)
  do<-list(project=project,data=data,recoded=recoded,variables=variables,filter.conditions=filter.conditions,recursive.blocks=recursive.blocks,recursive.structure=recursive.structure,comments=comments)
  class(do)<-"digram.object"
  do
}
digram.recode<-function(data,variables,filter.conditions=NULL) {
  # Recode data
  as.data.frame(sapply(variables,function(x) {
    cutpoints2<-matrix(c(x$minimum,x$cutpoints+.000001,x$cutpoints,x$maximum),ncol = 2,byrow = F)
    froms<-apply(cutpoints2,1,paste,collapse=":")
    tos<-0:(length(x$cutpoints))
    recodestr<-paste(apply(matrix(c(froms,tos),ncol=2,byrow = F),1,paste,collapse="="),collapse=";")
    column.number<-x$column.number
    datacol<-data[,column.number]
    if(!is.null(filter.conditions)) {
      minmax<-filter.conditions[filter.conditions$variable.number==column.number,2:3]
      if(nrow(minmax)>0) {
        minval<-minmax[1]
        maxval<-minmax[2]
        datacol<-car::recode(datacol,paste0("lo:",(minval-.000001),"=NA;",(maxval+.000001),":hi=NA"))
      }
    }
    recoded<-data.frame("col"=car::recode(datacol,recodestr))
    colnames(recoded)<-x$variable.name
    return(recoded)
  }))
}
# a<-digram.recode(DHP$data,DHP$variables)
# all(a==DHP$recoded)

#' Code items as local dependant
#'
#' @param do A digram.object
#' @param LD String. The item pairs that are local dependent. Give as a list of comma separated variable numbers, variable labels or variable names
#' @param append Logical. Append new LD variables to the existing ones.
#'
#' @return Returns a digram.object with the revised LD-data.frame.
#' @export
#' @details Local dependence is often caused by items sharing a common stimulus. This is called testlets or item bundles (Wang & Wilson 2006. Coding for Local Dependence is the same as identifying a testlet or an item bundle.
#' @references
#' Wang, W.-C., & Wilson, M. (2005). The Rasch Testlet Model. *Applied Psychological Measurement*, 29(2), 126–149. https://doi.org/10.1177/0146621604271053
#' @examples
#' data(DHP)
#' do<-code.LD(do=DHP,LD=c("ab,dhp36 dhp37,5 6"))
code.LD<-function(do,LD,append=F) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(LD)) stop("You need to provide a list of variables which are local dependent")
  pairs<-strsplit(x = LD, split =" *, *")[[1]]
  LDs<-lapply(pairs,function(x) {
      pair<-strsplit(x," +")[[1]]
      if(length(pair)<2) pair<-strsplit(pair,"")[[1]]
      pair<-sapply(pair,function(x) ifelse(grepl("^[0-9]$",x),as.numeric(x),x))
      sapply(pair,get.column.no,do=do)
  })
  # rownames(do$LD)<-NULL
  # colnames(do$LD)<-paste("var",1:ncol(do$LD))
  if(append && !is.null(do$LD)) {
    do$LD<-append(do$LD,LDs)
  } else do$LD<-LDs

  do
}
#' Code items as having DIF
#'
#' @param do A digram.object
#' @param DIF.var String. The variables having DIF. A comma separated list of variable numbers, labels or names.
#' @param DIF.exo String. The variables causing DIF. A comma separated list of exogenous variable numbers, labels or names.
#' @param append Logical.
#' @details If more variables and exogenous variables are given, all possible combinations of these are set to exibit DIF.
#' @return
#' @export
#'
#' @examples
#' data(DHP)
#' do<-code.DIF(DHP,"a,b","under60")
code.DIF<-function(do,DIF.var,DIF.exo,append=F) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(DIF.var)) stop("You need to provide one or more variables which have DIF")
  if(is.null(DIF.exo)) stop("You need to provide one or more exogenous variables which cause DIF")

  if(append && !is.null(do$DIF)) DIFs<-do$DIF else DIFs<-c()
  vars<-strsplit(x = DIF.var, split =" *, *")[[1]]
  var.nums<-sapply(vars,get.column.no,do=do)
  exos<-strsplit(x = DIF.exo, split =" *, *")[[1]]
  exo.nums<-sapply(exos,get.column.no,do=do)
  do$DIF<-rbind(DIFs,expand.grid(var=var.nums,exo=exo.nums))
  do
}
