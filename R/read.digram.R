#' @title Read DIGRAM Object
#' @name read.digram
#' @description Reads data from DIGRAM files and create a digram.object.
#' @usage read.digram(project=NULL,path="")
#' @param project The name of the DIGRAM project from which to load data
#' @param path Path to the project files
#' @export
#' @return Returns a digram.object
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples do<-read.digram("DHP",path = "DHP")
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
read.digram<-function(project=NULL,path=""){
  if(is.null(project)) stop("You need to provide a project name")
  # Solve casesensitivity issues
  files<-dir(path)
  deffile<-paste0(path,"/",files[grep(pattern = paste0("^",project,"\\.def$"),x = files,ignore.case = T)])
  def<-read.table(deffile,header = F,stringsAsFactors = F,blank.lines.skip = F,na.strings = c(""),fill=T)
  num.cols<-as.numeric(def[1,1])
  is.file.def<-F
  custom.datfile<-paste0(project,".dat")
  filter.conditions<-list()
  if(nrow(def)>1) {
    for(i in 2:nrow(def)) {
      if(substr(x = def[i,1],1,3)=="FIL") {
        is.file.def<-T
      } else {
        if(is.file.def) {
          if(def[i,1]=="D") custom.datfile<-def[i,2]
        } else {
          nextrow<-nrow(minmax)+1
          filter.conditions[[nextrow]]<-as.numeric(def[i,2:3])
        }
      }
    }
  }
  datfile<-paste0(path,"/",files[grep(pattern = paste0("^",gsub(pattern = ".",replacement = "\\.",x = tolower(custom.datfile),fixed = T),"$"),x = files,ignore.case = T)])
  varfile<-paste0(path,"/",files[grep(pattern = paste0("^",project,"\\.var$"),x = files,ignore.case = T)])
  catfile<-paste0(path,"/",files[grep(pattern = paste0("^",project,"\\.cat$"),x = files,ignore.case = T)])
  data<-as.data.frame(scan(file = datfile,multi.line = T,what=as.list(rep(0,num.cols)),blank.lines.skip = T,na.strings = c("-999")),col.names = paste0("V",1:num.cols))
  # Remove possible trailing whitespace in fields
  #if(sum(is.na(data[,ncol(data)]))==nrow(data)) data[,ncol(data)]<-NULL
  ncol <- max(count.fields(varfile))
  vars<-read.table(varfile,header = F,stringsAsFactors = F,col.names = paste0("V",seq_len(ncol)),fill = T,na.strings = "")
  if(file.exists(catfile)) {
    categories<-read.table(catfile,header = F,stringsAsFactors = F,na.strings = "",fill=T)
  } else categories<-data.frame()
  # Structure of var-file is defined on page 24 in the DIGRAM manual
  num.vars<-as.numeric(vars[1,1])
  recoded<-as.data.frame(matrix(rep(NA,num.vars*nrow(data)),ncol = num.vars))
  variable.start<-which(substr(vars[,1],1,3) =="VAR")
  variable.names<-vars[(variable.start+1):(variable.start+num.vars),]
  comments.start<-which(substr(vars[,1],1,3) =="COM")+1
  comments.end<-ifelse(comments.start<variable.start,variable.start-1,nrow(vars))
  comments<-ifelse(comments.end>comments.start,paste(trimws(apply(apply(vars[comments.start:comments.end,],2,recode,recodes = "NA=''"),1,paste,collapse = " ")),collapse = "\n"),"")
  variables<-list()
  for(i in 1:num.vars) {
    # 2 records for each var
    # Record 1: <Label> <Column number> <Number of categories> <Type>
    r1<-as.matrix(vars[i*2,!is.na(vars[i*2,])])
    column.number<-as.numeric(r1[2])
    variable.label<-r1[1]
    variable.name<-variable.names[variable.names[,1]==variable.label,-1]
    variable.name<-trimws(paste(variable.name[!is.na(variable.name)],collapse = " "))
    names(data)[column.number]<-paste(variable.name)
    ncat<-as.numeric(r1[3])
    variable.type<-c("","nominal","ordinal")[as.numeric(r1[4])]
    # Record 2: <Minimum> <Cutpoint(1)>...<Last cutpoint> ... <Maximum>
    r2<-as.numeric(as.matrix(vars[i*2+1,!is.na(vars[i*2+1,])]))
    minimum<-as.numeric(r2[1])
    maximum<-as.numeric(r2[length(r2)])
    cutpoints<-r2[2:(length(r2)-1)]
    # Categories from .CAT-file
    category.names<-categories[categories[,1]==variable.label,2:3]
    colnames(category.names)<-c("Category","Name")
    # Make data into factor with category.names
    #values<-unique(data[,column.number])
    #levels<-unique(recode(var = values,recodes = paste(apply(category.names,1,paste,collapse="="),collapse = ";")))
    #data[,column.number]<-factor(data[,column.number],levels = levels)

    variables[[i]]<-list(variable.name=variable.name,variable.label=variable.label,ncat=ncat,category.names=category.names,variable.type=variable.type,minimum=minimum,maximum=maximum,cutpoints=cutpoints)
    # Recode data
    # cutpoints2<-matrix(c(minimum,cutpoints+1,cutpoints,maximum),ncol = 2,byrow = F)
    # froms<-apply(cutpoints2,1,paste,collapse=":")
    # tos<-0:(length(cutpoints))
    # recodestr<-paste(apply(matrix(c(froms,tos),ncol=2,byrow = F),1,paste,collapse="="),collapse=";")
    # datacol<-data[,column.number]
    # if(length(filter.conditions)>=column.number) {
    #   if(length(filter.conditions[[column.number]])>0) {
    #     minval<-filter.conditions[[column.number]][1]
    #     maxval<-filter.conditions[[column.number]][2]
    #     # Only cases, for which all values of filter variables belong to the intervals defined by the corresponding minimum and maximum values (both included), will be used in the analysis.
    #     datacol<-recode(datacol,paste0("lo:",(minval-.000001),"=NA;",(maxval+.000001),":hi=NA"))
    #   }
    # }
    # recoded[,i]<-recode(datacol,recodestr)
    # colnames(recoded)[i]<-variable.name
  }
  # Recode
  recoded<-digram.recode(data,variables,filter.conditions)
  # The number of recursive blocks should appear on a separate record.
  recursive.blocks<-as.numeric(vars[i*2+2,1])
  recursive.structure<-as.numeric(as.matrix(vars[i*2+3,!is.na(vars[i*2+3,])]))
  do<-list(project=project,data=data,recoded=recoded,variables=variables,filter.conditions=filter.conditions,recursive.blocks=recursive.blocks,recursive.structure=recursive.structure,comments=comments)
  class(do)<-"digram.object"
  do
}
