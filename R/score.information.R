#' @title Print score information
#' @name score.information
#' @description Prints information about the recoded data.
#' @usage score.information(do=NULL,resp=NULL,items=1:length(do$variables),accept.na = F)
#' @param do An object of class \code{digram.object}
#' @param resp A data.frame or matrix of recoded data (only used if \code{do} is \code{NULL})
#' @param items A vector of columns from the recoded data to include in the analysis *or* a character vector of variable labels
#' @param do.testlets Bolean. If TRUE, testlets are combined to superitems.
#' @param do.split Bolean. If TRUE, items coded as split are split.
#' @param accept.na A boolean. Include cases with missing values in responses
#' @param rownamewidth Width of item names column
#' @export
#' @return Returns an object of items.scores,scoredist,meansum,score.distribution, and score.groups.
#' Prints Average item scores and score distribution and Score groups for tests of Rasch models
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples score.information(do,items="abcdef")
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
score.information<-function(do=NULL,resp=NULL,items=1:do$recursive.structure[1],do.testlets=T,do.split=T,accept.na=F,font.size=9,rownamewidth="15em"){
  if(!is.null(do)) {
    if(!inherits(do,"digram.object")) stop("do needs to be of class digram.object")
    resp<-do$recoded
    item.labels<-sapply(do$variables,function(x)x$variable.label)
  }else item.labels<-letters[1:ncol(resp)]
  items<-get.column.no(do,items)
  all.olditems<-c()
  if(do.testlets && !is.null(do$testlets)) {
    for(testlet in do$testlets){
      olditems<-which(items %in% testlet$testlet)
      if(length(olditems)>0) {
        newitem<-ncol(resp)+1
        items<-c(items,newitem)
        all.olditems<-c(all.olditems,olditems)
        # Recode
        resp[,newitem]<-apply(resp[,testlet$testlet],1,sum)#,na.rm=T)
        # Combine names and item.labels
        colnames(resp)[newitem]<-testlet$name
        item.labels<-c(item.labels,testlet$label)
      }
    }
  }

  if(do.split && !is.null(do$split)) {
    if(!accept.na) {
      warning("Set accept.na to TRUE if you want to get information on split items.")
    } else {
      for (i in 1:nrow(do$split)) {
        splits<-do$split[i,]
        olditem<-as.numeric(splits[1])
        if(length(olditem %in% items)>0) {
          exoitem<-as.numeric(splits[2])
          exocat<-do$variables[[exoitem]]$category.names
          ncat<-do$variables[[exoitem]]$ncat
          newitems<-ncol(resp)+1:ncat
          items<-c(items,newitems)
          all.olditems<-c(all.olditems,olditem)
          # Split
          nas<-rep(NA,ncat)
          resp[,newitems]<-sapply(1:nrow(resp),function(i) {
            newscores<-nas
            if(resp[i,exoitem] %in% exocat[,2]) newscores[resp[i,exoitem]]<-resp[i,olditem]
            newscores
          })

          # Combine names and item.labels
          olditemno<-which(items %in% olditem)
          newnames<-paste0(colnames(resp)[olditem],"_",exocat[,2])
          colnames(resp)[newitems]<-newnames
          item.labels<-c(item.labels,paste0(item.labels[olditemno],"_",do$variables[[exoitem]]$variable.label,1:ncat))
        }
      }
    }
  }

  # Remove old.items used in testlet and split
  if(length(all.olditems)>0) {
    items<-items[-all.olditems]
  }
  selected<-resp[,items]
  selectednona<-if(!accept.na) na.omit(selected) else selected
  if(nrow(selectednona)==0) stop("No cases without NA's. Try setting accept.na to TRUE")
  items.nona<-items
  caption<-header.format(paste(do$project,"Variables selected for item analysis"))
  cat("\nItems: ",item.labels[items])
  item.names<-get.variable.names(do,items)

  ########################
  # Item scores
  caption<-header.format(paste(do$project,"Average item scores and score distribution"))

  items.scores<-data.frame(Items=character(),n=numeric(),Mean=numeric(),n.complete=numeric(),Mean.complete=numeric(),Item.range=character(),stringsAsFactors = F)
  for(i in 1:ncol(selected)) {
    name<-item.names[i]
    x<-selected[,i]
    y<-selectednona[,i]
    items.scores[i,]<-c(paste(item.labels[items][i],":",name),sum(!is.na(x)),mean(x,na.rm = T),sum(!is.na(y)),mean(y),paste(min(x,na.rm = T),"-",max(x,na.rm = T)))
  }
  items.scores[,c(2:5)]<-apply(items.scores[,c(2:5)],2,as.numeric)
  caption<-header.format(paste(do$project,"Item information"))

  if(knitr::is_latex_output() || knitr::is_html_output()){
    print(knitr::kable(items.scores,col.names = c("Items","n","Mean","n","Mean","Item range"),caption = caption,booktabs=T,longtable=T,format.args = list(scientific=F,digits=2))%>%
            kableExtra::kable_styling() %>%
            kableExtra::add_header_above(c(" " = 2, "Complete cases" = 2, " " = 1)) %>%
            column_spec(1, width = rownamewidth))
  } else stargazer::stargazer(items.scores,type = "text",summary = F,digit.separator = "",digits = 2,rownames = F)


  #######################
  # Score distribution in categories
  caption<-header.format(paste(do$project,"Score distribution in categories"))

  maxall<-max(selectednona,na.rm=T)
  scoredist<-data.frame(t(sapply(1:ncol(selectednona),function(i) {
    sapply(0:maxall,function(j) sum(selectednona[,i]==j,na.rm = accept.na))
  })))
  category.names.1<-do$variables[[items[1]]]$category.names$Name
  same.category.names<-all(sapply(do$variables[items],function(x) length(x$category.names$Name)==length(category.names.1) && all(x$category.names$Name==category.names.1)))
  if(same.category.names) {
    if(knitr::is_latex_output() || knitr::is_html_output()) {colnames(scoredist)<-category.names.1} else
    colnames(scoredist)<-paste(0:maxall,paste("(",category.names.1,")",sep = ""),sep = " ")
  } else {
    colnames(scoredist)<-paste(ifelse((knitr::is_latex_output() || knitr::is_html_output()),"","Category "),0:maxall,sep = "")
    warning("Please note that the items do not have the same category names. To see them: lapply(do$variables,function(x) x$category.names)")
  }
  rownames(scoredist)<-paste(item.labels[items.nona],":",get.variable.names(do,items.nona))
  if(knitr::is_latex_output() || knitr::is_html_output())
    print(knitr::kable(scoredist,caption = caption,booktabs=T,longtable=T)%>%
            kableExtra::kable_styling() %>%
            kableExtra::add_header_above(c(" " = 1, "Category" = ncol(scoredist))) %>%
            column_spec(1, width = rownamewidth))
  else
    stargazer::stargazer(scoredist,type = "text",summary = F,rownames = T)

  #######################
  # Mean sumscore in categories
  caption<-header.format(paste(do$project,"Mean sumscore in categories (excluding item)"))

  maxall<-max(selectednona,na.rm=T)
  meansum<-data.frame(t(sapply(1:ncol(selectednona),function(i) {
    sapply(0:maxall,function(j) mean(if(ncol(selectednona)==2) selectednona[selectednona[,i]==j,-i] else
      rowSums(selectednona[selectednona[,i]==j & !is.na(selectednona[,i]) & apply(selectednona[,-i],1,function(x) !all(is.na(x))),-i],na.rm = T)))
  })))
  rownames(meansum)<-paste(item.labels[items.nona],":",get.variable.names(do,items.nona))
  descwarn<-apply(meansum,1,function(x) {
      desc<-FALSE
      for(i in 2:(maxall+1)) if(!is.na(x[i])) desc<-desc|(x[i]<=x[i-1])
      desc
    })
  meansum<-round(meansum,digits = 2)
  colnames(meansum) = paste(ifelse((knitr::is_latex_output() || knitr::is_html_output()),"","Category "),0:maxall,sep = "")
  if(any(descwarn)) meansum$Warning<-ifelse(descwarn,"!","")
  if(knitr::is_latex_output() || knitr::is_html_output())
    print(knitr::kable(meansum,booktabs=T,longtable=T,caption = caption)%>%
            kableExtra::kable_styling() %>%
            kableExtra::add_header_above(c(" " = 1, "Category" = maxall+1)) %>%
            column_spec(1, width = rownamewidth))
  else
    stargazer::stargazer(meansum,type = "text",summary = F,digit.separator = "",digits = 2,rownames = T)


  ########################
  # Score distribution
  totals<-apply(selectednona,1,sum,na.rm=T)
  cat("\n\nObtainable score range:",min(totals),"-",max(totals))
  scores<-min(totals):max(totals)
  caption<-header.format(paste(do$project,"Score distribution:",length(totals),"cases"))
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
    print(knitr::kable(score.distribution,booktabs=T,longtable=T,format.args = list(scientific=F,digits=2),caption=caption)%>%
            kableExtra::kable_styling())
  else
    stargazer::stargazer(score.distribution,type = "text",summary = F,digit.separator = "",digits = 2,rownames = F)


  newline<-ifelse(knitr::is_html_output() || knitr::is_latex_output(),"\n\n","\n")
  cat(newline,"Mean: ",round(mean(totals),2),newline,"Variance: ",round(var(totals),2),newline,"Standard Deviation: ",round(sd(totals),2),newline,"Skewness: ",round(DescTools::Skew(totals),2),newline,"Missing: ",nrow(selectednona)-length(totals),newline,"Cronbach's Alpha: ",round(psych::alpha(selectednona)$total$std.alpha,3))

  caption<-header.format(paste(do$project,"Score groups for tests of Rasch models"))
  cat(newline,"Observed:",newline,"Minimum score =",min(totals,na.rm = T),newline,"Maximum score =",max(totals,na.rm = T),newline,"Score Group:",nrow(selectednona),"Cases")

#  cat(newline,"| Score | Count | Percent | Cumulative |\n|",rep("------:|",4),sep="")
    lower.score<-sum(totals<half,na.rm = T)
    upper.score<-sum(totals>=half,na.rm = T)
    lower.percent<-lower.score/nrow(selectednona)*100
    upper.percent<-upper.score/nrow(selectednona)*100
    Score<-c(paste0("0-",half-1), half)
    Count<-c(lower.score,upper.score)
    Percent<-c(round(lower.percent,1),round(upper.percent,1))
    Cumulative<-c(round(lower.percent,1),round(lower.percent+upper.percent,1))
    score.groups<-data.frame(Score=Score,Count=Count, Percent=Percent, Cumulative=Cumulative)
    # cat("\n| ","0-",half-1," | ",lower.score," | ",round(lower.percent,1)," | ",round(lower.percent,1)," |",sep = "")
    # cat("\n| ",half,"-",max(totals,na.rm = T)," | ",upper.score," | ",round(upper.percent,1)," | ",round(lower.percent+upper.percent,1)," |",sep="")
    if(knitr::is_latex_output() || knitr::is_html_output())
      print(knitr::kable(score.groups,booktabs=T,caption=caption)%>%
              kableExtra::kable_styling())
    else
      stargazer::stargazer(score.groups,type = "text",summary = F,rownames = F)
    invisible(list(items.scores=items.scores,scoredist=scoredist,meansum=meansum,score.distribution=score.distribution,score.groups=score.groups))
}
