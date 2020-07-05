#' @title Print score information
#' @name score.information
#' @description Prints information about the recoded data.
#' @usage score.information(do=NULL,resp=NULL,items=1:length(do$variables),accept.na = F)
#' @param do An object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include in the analysis *or* a character vector of variable labels
#' @param accept.na A boolean. Include cases with missing values in responses
#' @export
#' @return Returns NULL.
#' Prints Average item scores and score distribution and Score groups for tests of Rasch models
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples score.information(do,items="abcdef")
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
score.information<-function(do=NULL,resp=NULL,items=1:do$recursive.structure[1],accept.na=F,font.size=9){
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    labels<-sapply(do$variables,function(x)x$variable.label)
  }else labels<-letters[1:ncol(resp)]
  items<-get.column.no(do,items)
  #
  # if(class(items)=="character") {
  #   items<-apply(as.matrix(strsplit(items,"")[[1]]),1,function(x) which(labels %in% x))
  # }
  selected<-resp[,items]
  selectednona<-if(!accept.na) na.omit(selected) else selected
  if(nrow(selectednona)==0) stop("No cases without NA's. Try setting accept.na to TRUE")
  header.format("Variables selected for item analysis")
  cat("\nItems: ",labels[items])

  ########################
  # Item scores
  header.format("Average item scores and score distribution")

  items.scores<-data.frame(Items=character(),n=numeric(),Mean=numeric(),n.complete=numeric(),Mean.complete=numeric(),Item.range=character(),stringsAsFactors = F)

  for(i in 1:ncol(selected)) {
    name<-colnames(selected)[i]
    x<-selected[,name]
    y<-selectednona[,name]
    items.scores[i,]<-c(paste(labels[i],":",name),sum(!is.na(x)),mean(x,na.rm = T),sum(!is.na(y)),mean(y),paste(min(x,na.rm = T),"-",max(x,na.rm = T)))
  }
  items.scores[,c(2:5)]<-apply(items.scores[,c(2:5)],2,as.numeric)
  if(knitr::is_latex_output() || knitr::is_html_output()){
    print(knitr::kable(items.scores,col.names = c("Items","n","Mean","n (complete)","Mean (complete)","Item range"),booktabs=T,longtable=T)%>%
            kable_styling(latex_options = c("scale_down")))
    #%>%
#    kableExtra::add_header_above(header = c(" "=3,"Complete cases"=2))
  } else stargazer::stargazer(items.scores,type = "text",summary = F,digit.separator = "",digits = 2,rownames = F)


  #######################
  # Score distribution in categories
  header.format("Score distribution in categories")

  maxall<-max(selectednona,na.rm=T)
  scoredist<-data.frame(t(sapply(1:ncol(selectednona),function(i) {
    sapply(0:maxall,function(j) sum(selectednona[,i]==j))
  })))
  colnames(scoredist)<-paste("Category",0:maxall)
  rownames(scoredist)<-colnames(selectednona)
  if(knitr::is_latex_output() || knitr::is_html_output())
    print(knitr::kable(scoredist,booktabs=T,longtable=T)%>%kable_styling(latex_options = c("scale_down")))
  else
    stargazer::stargazer(scoredist,type = "text",summary = F,rownames = T)

  #######################
  # Mean sumscore in categories
  header.format("Mean sumscore in categories (excluding item)")

  maxall<-max(selectednona,na.rm=T)
  meansum<-data.frame(t(sapply(1:ncol(selectednona),function(i) {
    sapply(0:maxall,function(j) mean(rowSums(selectednona[selectednona[,i]==j,-i],na.rm = T)))
  })))
  colnames(meansum)<-paste("Category",0:maxall)
  rownames(meansum)<-colnames(selectednona)
  descwarn<-apply(meansum,1,function(x) {
      desc<-FALSE
      for(i in 2:(maxall+1)) if(!is.na(x[i])) desc<-desc|(x[i]<=x[i-1])
      desc
    })
  if(any(descwarn)) meansum$Warning<-ifelse(descwarn,"!","")
  if(knitr::is_latex_output() || knitr::is_html_output())
    print(knitr::kable(meansum,booktabs=T,longtable=T)%>%kable_styling(latex_options = c("scale_down")))
  else
    stargazer::stargazer(meansum,type = "text",summary = F,digit.separator = "",digits = 2,rownames = T)


  ########################
  # Score distribution
  totals<-apply(selectednona,1,sum,na.rm=T)
  cat("\n\nObtainable score range:",min(totals),"-",max(totals))
  scores<-min(totals):max(totals)
  header.format(paste("Score distribution:",length(totals),"cases"))
  score.distribution<-data.frame(Score=numeric(),Count=numeric(),Percent=numeric(),Cumulated=numeric())
  cumulated<-half<-0
  for(i in scores) {
    has.score<-totals==i
    percent<-sum(has.score)/length(totals)*100
    cumulated<-cumulated+percent
    if(cumulated>50 && half==0) half=i
    score.distribution[i+1,]<-c(i,sum(has.score),percent,cumulated)
  }
  if(knitr::is_latex_output() || knitr::is_html_output())
    print(knitr::kable(score.distribution,booktabs=T,longtable=T)%>%kable_styling(latex_options = c("scale_down")))
  else
    stargazer::stargazer(score.distribution,type = "text",summary = F,digit.separator = "",digits = 2,rownames = F)


  newline<-ifelse(knitr::is_html_output() || knitr::is_latex_output(),"\n\n","\n")
  cat(newline,"Mean: ",round(mean(totals),2),newline,"Variance: ",round(var(totals),2),newline,"Standard Deviation: ",round(sd(totals),2),newline,"Skewness: ",round(DescTools::Skew(totals),2),newline,"Missing: ",nrow(selectednona)-length(totals),newline,"Cronbach's Alpha: ",round(DescTools::CronbachAlpha(selectednona,na.rm = T),3))

  header.format("Score groups for tests of Rasch models")
  cat(newline,"Observed:",newline,"Minimum score =",min(totals,na.rm = T),newline,"Maximum score =",max(totals,na.rm = T),newline,"Score Group:",nrow(selectednona),"Cases")
  cat(newline,"| Score | Count | Percent | Cumulative |\n|",rep("------:|",4),sep="")
    lower.score<-sum(totals<half,na.rm = T)
    upper.score<-sum(totals>=half,na.rm = T)
    lower.percent<-lower.score/nrow(selectednona)*100
    upper.percent<-upper.score/nrow(selectednona)*100
    cat("\n| ","0-",half-1," | ",lower.score," | ",round(lower.percent,1)," | ",round(lower.percent,1)," |",sep = "")
    cat("\n| ",half,"-",max(totals,na.rm = T)," | ",upper.score," | ",round(upper.percent,1)," | ",round(lower.percent+upper.percent,1)," |",sep="")
}
