#' Draw a Wrightmap
#'
#' @param mod Output from tam.mml or tam.fa.
#' @param do A digram.object. Used to create title. Alternative to dimname
#' @param dimname A string. Used to create title. Alternative to do.
#' @param cols A vector of colors.
#' @param colscheme A regular expression as string. Used to give items of the same type, same colors.
#' @param verbose Boolean. Set to TRUE to get information about progress.
#'
#' @return
#' Returns a list of item difficulties as thresholds and person abillities as thetas
#' @export
#'
#' @examples
#' mod<-digram.estimate(DHP)
#' digram.wrightmap(mod=mod,do=DHP)
digram.wrightmap<-function(mod,do=NULL,dimname=NULL,cols=collist,colscheme="(.*)_.*",verbose = T) {
  if(is.null(dimname) && !is.null(do)) dimname<-do$project
  thresholds<-TAM::tam.threshold(mod)
  thresorder<-order(thresholds[,1])
  thresholds[1:nrow(thresholds),]<-thresholds[thresorder,]
  itemnames<-attributes(thresholds)$dimnames[[1]]
  attributes(thresholds)$dimnames[[1]]<-itemnames[thresorder]

  thresholds.matrix<-as.matrix(thresholds)
  attr(thresholds.matrix,"theta") <- NULL
  attr(thresholds.matrix, "prob.theta") <- NULL
  class(thresholds.matrix) <- "matrix"
  # print(knitr::kable(thresholds.matrix,caption = paste("Dificulties for",dimname),booktabs = TRUE)%>%
  #         kable_styling(latex_options = c("scale_down")))#,col.names = c("Værdi")
  mod$B<-mod$B[,,1]
  dim(mod$B)[3]<-1
  if(!verbose) sink("/dev/null")
  wle<-TAM::tam.mml.wle(mod,progress = F)
  if(!verbose) sink()
  thetas<-wle[,which(grepl("theta",colnames(wle)))[1]]
  itemcolors<-cols[as.numeric(as.factor(gsub(colscheme,"\\1",itemnames[thresorder]))) %% length(cols)]
  a<-WrightMap::wrightMap(thetas = thetas,thresholds = thresholds ,show.thr.lab = FALSE, label.items.srt = 90,label.items.cex=.5,axis.items="",main.title=paste("WrightMap for",dimname),thr.sym.col.fg = itemcolors, thr.sym.col.bg = itemcolors)
  # thres<-get.thresholds(mod)
  # row.names(thres)<-itemnames
  # thres<-thres[thresorder,]
  invisible(list(thresholds=thresholds.matrix,thetas=thetas))
}
pp<-function(rgb,procent) {
  n<-procent/100
  apply(as.matrix(rgb),1,function(x) {round((n*x+(1-n)*255))})
}
h<-function(rgb) {
  ret<-c()
  for(i in 1:(length(rgb)/3)) {
    int<-(i-1)*3+1
    ret<-c(ret,paste0("#",paste0(as.hexmode(rgb[int:(int+2)]),collapse = "")))
  }
  return(ret)
}
# Colors
# Green (primary): Pantone 355UP: https://store.pantone.com/uk/en/colorfinder/index/acfproduct/code/355+UP
pg<-c(  59, 159, 107  )
pg25<-pp(pg,25)
pg50<-pp(pg,50)
pg75<-pp(pg,75)
pg100<-pg
# Blue: Pantone 314UP: https://store.pantone.com/uk/en/colorfinder/index/acfproduct/code/314+UP
pb<-c( 0, 136, 160 )
pb25<-pp(pb,25)
pb50<-pp(pb,50)
pb75<-pp(pb,75)
pb100<-pb
# Red: Pantone 220UP: https://store.pantone.com/uk/en/colorfinder/index/acfproduct/code/220+UP
pr<-c( 179, 81, 120 )
pr25<-pp(pr,25)
pr50<-pp(pr,50)
pr75<-pp(pr,75)
pr100<-pr
# Yellow: Pantone  7404 UP: https://store.pantone.com/uk/en/colorfinder/index/acfproduct/code/7404+UP
py<-c(251, 208, 58)
py25<-pp(py,25)
py50<-pp(py,50)
py75<-pp(py,75)
py100<-py
# Purple: PANTONE 514 UP: https://store.pantone.com/uk/en/colorfinder/index/acfproduct/code/514+UP
pu<-c( 203, 138, 177 )
pu25<-pp(pu,25)
pu50<-pp(pu,50)
pu75<-pp(pu,75)
pu100<-pu
collist<-h(c(pr,pg,pb,py,pu,pr25,pg25,pb25,py25,pu25,pr50,pg50,pb50,py50,pu50,pr75,pg75,pb75,py75,pu75))
