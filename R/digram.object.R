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
      if(!(class(data[,vars[i]]) %in% c("integer","numeric"))) {
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
#' Create a tidygraph object from a digram.object
#'
#' @param do A digram.object.
#' @param LD A data.frame with columns for item1, item2 and gamma coefficient. Items can be variable.names, variable.columns or item numbers.
#' @param DIF A data.frame with columns for item, exogenous variable and gamma coefficient. Items and exogenous variables can be variable.names, variable.columns or item numbers.
#' @usage as_tbl_graph(do)
#' @return Returns a tbl_graph
#' @export
#' @seealso [tidygraph::as_tbl_graph()]
#' @examples
#' library(ggraph)
#' library(tidygraph)
#' dograph<-as_tbl_graph(DHP)
#' ggraph(dograph,layout="fr")+geom_edge_link()+geom_node_label(mapping = aes(label=label))
#' ggraph(dograph,layout="fr")+geom_edge_link()+geom_node_label(mapping = aes(label=name))
#'
#' # Show arrows
#' ggraph(dograph,layout="fr")+geom_edge_link(end_cap = square(.5, 'cm'),arrow = arrow(angle=10,length=unit(.2,"cm")))+geom_node_label(mapping = aes(label=label))
#'
#' # A digram.object with a testlet
#' dograph<-as_tbl_graph(code.testlet(DHP,"a b c"))
#' ggraph(dograph,layout="fr")+geom_edge_link(end_cap = square(.5, 'cm'),arrow = arrow(angle=10,length=unit(.2,"cm")))+geom_node_label(mapping = aes(label=label))
#'
#' # Local dependecy and DIF
#' dograph<-as_tbl_graph(DHP,LD=data.frame(item1=c(5,3),item2=c(6,5),gamma=c(.53,-.38)),DIF=data.frame(item=c(4),exo=c(8),gamma=c(.27)))
#' ggraph(dograph,layout="fr")+geom_edge_link(mapping=aes(label=ifelse(!is.na(gamma),abs(gamma),""),alpha=ifelse(!is.na(gamma),gamma,1),color=ifelse(!is.na(gamma),2,1)),angle_calc="along",label_dodge=unit(.25,"cm"),end_cap = square(.5, 'cm'),arrow = arrow(angle=10,length=unit(.2,"cm")))+geom_node_label(mapping = aes(label=label))
as_tbl_graph.digram.object<-function(do,items=NULL,exo.names=NULL,exo.labels=exo.names,LD=NULL,DIF=NULL){
  if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
  resp<-do$recoded
  if(is.null(items)) items<-1:do$recursive.structure[1]
  if(is.null(exo.names)) {
    exo<-if(ncol(resp)>do$recursive.structure[1]) (do$recursive.structure[1]+1):ncol(resp) else NULL
    exo.names<-get.variable.names(do,exo)
    if(inherits(exo,"character")) exo<-match(exo,exo.names)
    exo.labels<-get.labels(do,exo)
  }
  item.names<-get.variable.names(do,items)
  if(inherits(items,"character")) items<-match(items,item.names)
  item.labels<-get.labels(do,items)


  nitems<-length(items)
  nexo<-length(exo.names)
  ntestlets<-0

  if(!is.null(LD) || !is.null(DIF)) {
    environment(collapse.testlets) <- environment()
    collapse.testlets()
    ntestlets<-sum(items>length(do$variables))
  } else if(!is.null(do$testlets)) {
    testlet.edges<-lapply(do$testlets,function(x) {
      testlet<-x$testlet
      nitem<-length(testlet)
      from<-unlist(sapply(1:(nitem-1),function(y) rep(testlet[y]+2,nitem-y))) # We add 2 because we have theta and total score
      to<-unlist(sapply(2:nitem,function(y) testlet[y:nitem]+2)) # Ditto
      c(from,to,to,from)
    })
    testlet.edges<-as.data.frame(matrix(unlist(testlet.edges),ncol=2))
    colnames(testlet.edges)<-c("from","to")
    testlet.edges$gamma<-NA
  }
  if(!is.null(do$split)) {
    warning("Graphing of split items not implemented yet.")
  }
  if(!is.null(LD)) {
    if(nrow(LD)>0){
      if(ncol(LD)!=3) stop("LD needs to have three columns: item1, item2, and gamma")
      colnames(LD)<-c("from","to","gamma")
      maxnl<-max(nchar(LD$from),nchar(LD$to))
      LD$from<-apply(array(LD$from),1,function(x) which(x==item.names.shorten(item.names,maxnl))) + 2  # We add 2 because we have theta and total score
      LD$to<-apply(array(LD$to),1,function(x) which(x==item.names.shorten(item.names,maxnl))) + 2
    }
  }
  if(!is.null(DIF)) {
    if(nrow(DIF)>0){
      if(ncol(DIF)!=3) stop("DIF needs to have three columns: item, exo, and gamma")
      colnames(DIF)<-c("to","from","gamma")
      #maxnl<-max(nchar(apply(array(DIF$from),1,sub,pattern=",.*",replacement="")),nchar(DIF$to))
      maxnl<-max(nchar(DIF$to))
      # DIF$from<-apply(array(DIF$from),1,function(x) which(sub(",.*","",x)==item.names.shorten(exo.names,maxnl))) + 2 + nitems # We add 2 and nitems because we have theta and total score and items before exos
      DIF$to<-apply(array(DIF$to),1,function(x) which(x==item.names.shorten(item.names,maxnl))) + 2
      DIF$from<-apply(array(DIF$from),1,match,exo.names) + 2 + nitems # We add 2 and nitems because we have theta and total score and items before exos
      #DIF$to<-apply(array(DIF$to),1,match,item.names) + 2  # We add 2 because we have theta and total score

    }
  }
  # nitems<-length(items)
  # nexo<-length(exo)
  # c("Theta","Total Score")
  nodes<-rbind(data.frame(name=c("θ","S"),label=c("θ","S"),type=rep("Ability",2),stringsAsFactors = F),#,column.number=c(0,0)
               data.frame(name=item.names,label=item.labels,type=c(rep("Item",nitems-ntestlets),rep("Testlet",ntestlets)))
  )
  if(nexo>0)  nodes<-rbind(nodes, #t(sapply(1:do$recursive.structure[1], function(x) unlist(do$variables[[x]][c("variable.name","variable.label","column.number")]))),
                           data.frame(name=exo.names,label=exo.labels,type=rep("Exo",nexo))#t(sapply((do$recursive.structure[1]+1):do$recursive.structure[2], function(x) unlist(do$variables[[x]][c("variable.name","variable.label","column.number")])))
  )
  edges<-data.frame(
    from=c(1,rep(2,nitems)),
    to=c(2,3:(nitems+2)))
  if(nexo>0) edges<-rbind(edges,data.frame(
    from=(nitems+3):(nitems+nexo+2),
    to=rep(1,nexo)))
  edges$gamma<-NA
  if(exists("testlet.edges")) edges<-rbind(edges,testlet.edges)
  if(exists("LD")) edges<-rbind(edges,LD)
  if(exists("DIF")) edges<-rbind(edges,DIF)

  tbl_graph(nodes=nodes,edges=edges,directed = T)
}

#' Code items as a testlet/local dependant
#'
#' @param do A digram.object
#' @param testlet String. The items that are part of a testlet/are local dependant. Give as a list of comma separated variable numbers, variable labels or variable names. If there is spaces in the variable names, they can be delimited by ".
#' @param names A vector of strings naming the testlets. If names are not given, they are composed of the testlet item names.
#' @param append Logical. Append new testlet variables to the existing ones.
#'
#' @return Returns a digram.object with the revised testlet-data.frame.
#' @export
#' @details Local dependence is often caused by items sharing a common stimulus. This is called testlets or item bundles (Wang & Wilson 2006. Coding for Local Dependence is the same as identifying a testlet or an item bundle.
#' @references
#' Wang, W.-C., & Wilson, M. (2005). The Rasch Testlet Model. *Applied Psychological Measurement*, 29(2), 126–149. https://doi.org/10.1177/0146621604271053
#' @examples
#' data(DHP)
#' do<-code.testlet(do=DHP,testlet=c("ab,dhp36 dhp37,5 6"))
code.testlet<-function(do,testlet=NULL,names=NULL,labels=NULL,append=F) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(testlet)) stop("You need to provide a list of variables which are local dependent")
  testlet.strs<-ifelse(grepl("\"",testlet),strsplit(x = testlet, split ='(?<=")\\s*,\\s*(?=")',perl = T),testlet.strs<-strsplit(x = testlet, split ="\\s*,\\s*"))[[1]]
  no<-0
  testlets<-lapply(testlet.strs,function(x) {
      no<<-no+1
      if(grepl("\"",x)) {
        res<-gregexpr('"[^"]+"\\s*',x)[[1]]
        res<-c(res,nchar(x))
        testlet<-c()
        for(i in 2:length(res)) {
          testlet<-c(testlet,trimws(substr(x,res[i-1],res[i]-1))) #gsub("\"","",)
        }
      } else testlet<-strsplit(x," |\\+")[[1]]
      if(length(testlet)<2) testlet<-strsplit(testlet,"")[[1]]
      testlet<-sapply(testlet,function(x) ifelse(grepl("^[0-9]$",x),as.numeric(x),x))
      testlet<-sapply(testlet,get.column.no,do=do)
      label<-ifelse(is.null(labels[no]),
                   paste(get.labels(do,testlet),collapse = " + "),
                   labels[no])
      if(is.null(names[no])){
        itemnames<-get.variable.names(do,testlet)
        name<-str_overlap(itemnames)
        if(name=="") name<-paste(itemnames,collapse = " + ")
      } else name<-names[no]
      list(testlet=testlet,name=name,label=label)
  })
  if(append && !is.null(do$testlets)) {
    do$testlets<-append(do$testlets,testlets)
  } else do$testlets<-testlets

  do
}
str_overlap<-function(arr,str="") {
  if(length(arr)==0) return(str)
  if(str=="") return(str_overlap(arr[-1],arr[1]))
  str2=arr[1]
  for(i in 1:nchar(str)) {
    if(substr(str,i,i)!=substr(str2,i,i)) {i<-i-1;break}
  }
  return(sub("[-_.:;,+?]\\s*$","",str_overlap(arr[-1],substr(str,1,i))))
}
# Internal function to collapse testlets
collapse.testlets<-function(){
  if(!is.null(do$testlets)) {
    testletno<-0
    for(tlist in do$testlets){
      testletno<-testletno+1
      testlet<-tlist$testlet
      testlet<-(testlet[testlet %in% items])
      newitem<-length(do$variables)+testletno
      resp[,newitem]<<-NA
      if(length(testlet)>0) {
        items<<-c(items,newitem)
        olditems<-which(items %in% testlet)
        # Recode
        resp[,newitem]<<-apply(resp[,testlet],1,sum,na.rm=T)
        # Combine names and labels
        newname<-tlist$name
        item.names<<-c(item.names,newname)
        colnames(resp)[newitem]<<-newname
        item.labels<<-c(item.labels,tlist$label)
        item.names<<-item.names[-olditems]
        item.labels<<-item.labels[-olditems]
        # Remove item-nums
        items<<-c(items[-olditems])
      }
    }
  }
}
#' Code items to be split (as having DIF)
#'
#' @param do A digram.object
#' @param split.var String. The variables to split (having DIF). A comma separated list of variable numbers, labels or names.
#' @param split.on String. The exogenous variables to split on (causing DIF). A comma separated list of exogenous variable numbers, labels or names.
#' @param append Logical.
#' @details If more variables and exogenous variables are given, all possible combinations of these are split.
#' @return
#' @export
#'
#' @examples
#' data(DHP)
#' do<-code.split(DHP,"a,b","under60")
code.split<-function(do,split.var,split.on,append=F) {
  if(!inherits(do,"digram.object")) stop("do needs to be a digram.object")
  if(is.null(split.var)) stop("You need to provide one or more variables to split")
  if(is.null(split.on)) stop("You need to provide one or more exogenous variables to split on")

  if(append && !is.null(do$splits)) splits<-do$splits else splits<-c()
  vars<-strsplit(x = split.var, split =" *, *")[[1]]
  var.nums<-sapply(vars,get.column.no,do=do)
  exos<-strsplit(x = split.on, split =" *, *")[[1]]
  exo.nums<-sapply(exos,get.column.no,do=do)
  do$splits<-rbind(splits,expand.grid(var=var.nums,exo=exo.nums))
  do
}
