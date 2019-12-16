#' @title Create DIGRAM Object
#' @name digram.object
#' @description Create a digram.object.
#' @usage digram.object(project=NULL,data=data.frame(),variables=colnames(data),filter.conditions=list(),recursive.structure=c(),comments="")
#' @param project The name of the DIGRAM project
#' @param data The data.frame or matrix with the data
#' @param variables A vector of column names or numbers from the dataset to include in the recoded dataset (the order matters in the recoded data) *or* a list of variables each element in the form list(variable.name="",variable.label="",ncat=0,category.names=c(),variable.type=c("nominal","ordinal"),minimum=0,maximum=1,cutpoints=c())
#' @param filter.conditions A list where names are variable numbers and values are \code{c(max,min)}. Only cases, for which all values of filter variables belong to the intervals defined by the corresponding minimum and maximum values (both included), will be used in the analysis.
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
digram.object<-function(project=NULL,data=data.frame(),variables=NULL,filter.conditions=list(),recursive.structure=NULL,comments="") {
  if(is.null(project)) stop("You need to provide a project name")
  #data<-as.data.frame(data)
  if(!inherits(variables,"list")) {
    vars<-if(is.null(variables)) colnames(data) else if(inherits(variables,"numeric")) colnames(data)[variables] else variables
    num.vars<-length(vars)
    variables<-list()
    for(i in 1:num.vars) {
      numletters<-which(i<=(26^(1:5)-1))[1]
      variable.label<-""
      k<-i-1
      for(j in numletters:1) {r<-(k%%26);variable.label<-paste0(LETTERS[r+1],variable.label); k<-floor(k/26)}
      variable.name<-vars[i]
      # Test for integer/float/character...
      categories<-na.omit(unique(data[,vars[i]]))
      ncat<-length(categories)
      variable.type<-ifelse(inherits(categories,"numeric") | inherits(categories,"integer"),"ordinal","nominal")
      minimum<-if(variable.type=="ordinal") min(categories) else 0
      maximum<-if(variable.type=="ordinal") max(categories) else ncat-1
      # Record 2: <Minimum> <Cutpoint(1)>...<Last cutpoint> ... <Maximum>
      cutpoints<-if(variable.type=="ordinal") categories[order(categories)] else 0:(ncat-1)
      cutpoints<-cutpoints[-ncat]
      # Categories from .CAT-file
      category.names<-data.frame(Category=0:(ncat-1),Name=categories[order(categories)])

      variables[[i]]<-list(variable.name=variable.name,variable.label=variable.label,ncat=ncat,category.names=category.names,variable.type=variable.type,minimum=minimum,maximum=maximum,cutpoints=cutpoints)
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
    column.number<-which(colnames(data)==x$variable.name)
    datacol<-data[,column.number]
    if(!is.null(filter.conditions)) {
      if(length(filter.conditions)>=column.number) {
        if(length(filter.conditions[[column.number]])>0) {
          minval<-filter.conditions[[column.number]][1]
          maxval<-filter.conditions[[column.number]][2]
          datacol<-recode(datacol,paste0("lo:",(minval-.000001),"=NA;",(maxval+.000001),":hi=NA"))
        }
      }
    }
    recoded<-data.frame("col"=car::recode(datacol,recodestr))
    colnames(recoded)<-x$variable.name
    return(recoded)
  }))
}
# a<-digram.recode(DHP$data,DHP$variables)
# all(a==DHP$recoded)
