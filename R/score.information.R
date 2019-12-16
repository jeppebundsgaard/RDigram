#' @title Print score information
#' @name score.information
#' @description Prints information about the recoded data.
#' @usage score.information(do=NULL,recoded=NULL,items=1:length(do$variables),accept.na = F)
#' @param do An object of class \code{digram.object}
#' @param recoded A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include in the analysis *or* a character vector of variable labels
#' @param accept.na A boolean. Include cases with missing values in responses
#' @export
#' @return Returns NULL.
#' Prints Average item scores and score distribution and Score groups for tests of Rasch models
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples score.information(do,items="abcdef")
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
score.information<-function(do=NULL,recoded=NULL,items=1:do$recursive.structure[1],accept.na=F){
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    recoded<-do$recoded
    labels<-sapply(do$variables,function(x)x$variable.label)
  }
  else labels<-letters[1:ncol(recoded)]
  if(class(items)=="character") {
    items<-apply(as.matrix(strsplit(items,"")[[1]]),1,function(x) which(labels %in% x))
  }
  selected<-recoded[,items]
  selectednona<-if(!accept.na) na.omit(selected) else selected
  if(nrow(selectednona)==0) stop("No cases without NA's. Try setting accept.na to TRUE")
  header.format("Variables selected for item analysis")
  cat("\nItems: ",labels[items])

  ########################
  # Item scores
  header.format("Average item scores and score distribution")

  items.scores<-data.frame(Items=character(),n=numeric(),Mean=numeric(),Mean.complete=numeric(),Item.range=character(),stringsAsFactors = F)

  for(i in 1:ncol(selectednona)) {
    name<-colnames(selectednona)[i]
    x<-selectednona[,i]
    items.scores[i,]<-c(paste(labels[i],":",name),sum(!is.na(x)),mean(x,na.rm = T),mean(x[!is.na(x)]),paste(min(x,na.rm = T),"-",max(x,na.rm = T)))
  }
  items.scores[,c(2:4)]<-apply(items.scores[,c(2:4)],2,as.numeric)
  stargazer::stargazer(items.scores,type = "text",summary = F,digit.separator = "",digits = 2,rownames = F)
#  knitr::kable(items.scores,col.names = c("Items","n","Mean","Mean","Item range"),format = "markdown")#%>%
#    kableExtra::add_header_above(header = c(" "=3,"Complete cases"=2))


  ########################
  # Score distribution
  totals<-apply(selectednona,1,sum,na.rm=T)
  cat("\n\nObtainable score range:",min(totals),"-",max(totals))
  scores<-min(totals):max(totals)
  cat("\n\nScore distribution: ",length(totals)," cases")
  score.distribution<-data.frame(Score=numeric(),Count=numeric(),Percent=numeric(),Cumulated=numeric())
  cumulated<-half<-0
  for(i in scores) {
    has.score<-totals==i
    percent<-sum(has.score)/length(totals)*100
    cumulated<-cumulated+percent
    if(cumulated>50 && half==0) half=i
    score.distribution[i+1,]<-c(i,sum(has.score),percent,cumulated)
  }
  stargazer::stargazer(score.distribution,type = "text",summary = F,digit.separator = "",digits = 2,rownames = F)
  cat("\nMean: ",round(mean(totals),2),"\nVariance: ",round(var(totals),2),"\nStandard Deviation: ",round(sd(totals),2),"\nSkewness: ",round(DescTools::Skew(totals),2),"\nMissing: ",nrow(selectednona)-length(totals),"\n\nCronbach's Alpha: ",round(DescTools::CronbachAlpha(selectednona,na.rm = T),3))

  header.format("Score groups for tests of Rasch models")
  cat("\nObserved:\nMinimum score =",min(totals,na.rm = T),"\nMaximum score =",max(totals,na.rm = T),"\n\nScore Group:",nrow(selectednona),"Cases")
  cat("\n-----------------------------------\nScore\tCount\tPercent\tCumulative\n-----------------------------------")
    lower.score<-sum(totals<half,na.rm = T)
    upper.score<-sum(totals>=half,na.rm = T)
    lower.percent<-lower.score/nrow(selectednona)*100
    upper.percent<-upper.score/nrow(selectednona)*100
    cat("\n","0-",half-1,"\t",lower.score,"\t",round(lower.percent,1),"\t",round(lower.percent,1),sep = "")
    cat("\n",half,"-",max(totals,na.rm = T),"\t",upper.score,"\t",round(upper.percent,1),"\t",round(lower.percent+upper.percent,1),sep="")
}
header.format<-function(t="",margin=0) {
  tw<-stringr::str_length(t)
  cat("\n\n+",rep("-",tw+2),"+",rep(paste("\n|",stringr::str_pad(" ",tw),"|",collapse=""),margin),"\n| ",t," |\n",rep(paste("|",stringr::str_pad(" ",tw),"|\n",collapse=""),margin),"+",rep("-",tw+2),"+\n",sep="")
  t
}
#header.format(t,0)
#score.information(do = do,items = "abcdef")
#DHP<-do
#save(DHP,file = "DHP.rda")
