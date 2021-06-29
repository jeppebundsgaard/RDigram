#' Create a proficiency scale table
#'
#' @param mod A tam.mml or eRm object.
#' @param dontplot Boolean. If TRUE, the proficiency table is not plotted.
#' @param nlevels Number of levels in the proficiency table.
#' @param level.width Width levels in logits
#' @param level1.base Lower limit of 1st level in logits
#' @param level.lims A vector of level limits (lower limit, middle levels ..., higher limit) (if you want equal sized levels, use level.width and level1.base)
#' @param color.palette hcl-palette to use as colorscheme. Get all available names: [hcl.pals()]. See five colors from all palettes here: https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/
#' @param font.base.size Size of base text in milimeters. From ggplot-specs: This is unusual, but makes the size of text consistent with the size of lines and points. Typically you specify font size using points (or pt for short), where 1 pt = 0.35mm. ggplot2 provides this conversion factor in the variable .pt, so if you want to draw 12pt text, set size = 12 / .pt.
#' @param level.names A vector of names of levels
#' @param level.descriptions A vector of level descriptions
#' @param item.params A matrix of item parameters (for automatic item descriptions)
#' @param item.descriptions A matrix of item descriptions (if you don't provide them, they will be produced from item.params or mod)
#' @param item.names.include Boolean: Include item names before the description.
#' @param relative.level.rect.heights Boolean: Set the level heights relative to the texts.
#' @param same.level.rect.heights Boolean: Let the level heights be equal no matter the logit intervals.
#' @param level.rect.heights A vector of heights of the levels. Heights are relative to other heights.
#' @param level.rect.width Width of the level descriptions. Width are relative to other widths.
#' @param level.rect.fill See header
#' @param level.rect.color See header
#' @param include.level.interval Boolean. Include interval of the level (in logits)
#' @param level.interval.family See header
#' @param level.interval.size See header
#' @param level.interval.fontface See header
#' @param level.interval.color See header
#' @param level.interval.hjust See header
#' @param level.interval.nudge_x See header
#' @param level.interval.nudge_y See header
#' @param level.name.family See header
#' @param level.name.size See header
#' @param level.name.fontface See header
#' @param level.name.color See header
#' @param level.name.hjust See header
#' @param level.name.nudge_x See header
#' @param level.name.nudge_y See header
#' @param level.name.linelength Length of description in characters before new line.
#' @param level.description.family See header
#' @param level.description.size See header
#' @param level.description.fontface See header
#' @param level.description.color See header
#' @param level.description.nudge_x See header
#' @param level.description.nudge_y See header
#' @param level.description.linelength Length of description in characters before new line.
#' @param level.description.hjust See header
#' @param items.include.categories An integer indicating how many categories to include. Set to NULL if all categories should be included.
#' @param items.rect.width  Width of the item descriptions. Width are relative to other widths.
#' @param items.rect.fill See header
#' @param items.rect.color See header
#' @param item.description.seperator Character(s) to insert between item descriptions (\\n is newline)
#' @param item.description.bullet Character(s) to insert before item descriptions
#' @param items.description.family  See header
#' @param items.description.size See header
#' @param items.description.fontface See header
#' @param item.description.color See header
#' @param items.description.nudge_x See header
#' @param items.description.nudge_y See header
#' @param items.description.linelength Length of description in characters before new line.
#' @param items.description.hjust See header
#' @param ngroups Number of person groups
#' @param person.parameters Person abillities in logits.
#' @param person.middle Boolean. Use person parameter in the middle of the group interval for calculations of person.probabilities (Default is to use person in the bottom of the group)
#' @param person.dist.type Type of person group. At the moment just: "in.levels" or "none"
#' @param person.probability.0 Color of 0 percent probability of correct answer in person groups
#' @param person.probability.100 Color of 100 percent probability of correct answer in person groups
#' @param person.width Width person groups in logits
#' @param person1.base Lower limit of 1st person group in logits
#' @param person.lims A vector of person limits (lower limit, middle levels ..., higher limit) (if you want equal sized levels, use person.width and person1.base)
#' @param person.rect.width Width of person groups. Width are relative to other widths.
#' @param person.rect.color See header
#' @param person.family See header
#' @param person.size See header
#' @param person.fontface See header
#' @param person.color See header
#' @param person.nudge_x See header
#' @param person.nudge_y See header
#' @param person.hjust See header
#' @param header.level Levels header text
#' @param header.item Items header text
#' @param header.persons Person group header texts (vector of a text for each group)
#' @param header.show.percentage.in.group Boolean. Show percentage of persons in each group.
#' @param header.heigth Height of the header in "logits" relative to level limits.
#' @param header.rect.color Color of header border
#' @param header.rect.fill Fill of the header
#' @param header.hjust Justification of the header text
#' @param header.nudge_x Nudging of the header text on the x-axis
#' @param header.nudge_y Nudging of the header text on the y-axis
#' @param header.family Font family of the header text
#' @param header.size Size of the header text
#' @param header.fontface Font face (bold, normal, italic) of the header text
#' @param header.color Font color of the header text
#' @param extra.verbose Print warnings in PDF and HTML-output

#'
#' @return Returns a ggplot object.
#' @export
#'
#' @examples
#' mod<-digram.estimate(DHP)
#' proficiency.scale(mod)
proficiency.scale<-function(mod=NULL,dontplot=F,font.base.size = 10 / .pt,
                            nlevels=4,level.width=1.2,level1.base=-2.4,level.lims=level1.base+(0:nlevels)*level.width,color.palette="BluGrn",
                            level.names=paste("Level",1:nlevels),level.descriptions=paste("Level description",1:nlevels),
                            item.params=NULL,item.descriptions=NULL,item.names.include=F,
                            # Grid
                            grid.color="gray",
                            # Levels
                            relative.level.rect.heights=F,same.level.rect.heights=F,level.rect.heights=level.lims,
                            level.rect.width=1,level.rect.fill=hcl.colors(4,palette=color.palette)[2],level.rect.color="white",
                            include.level.interval=F,level.interval.family="serif",level.interval.size=font.base.size*1.1,level.interval.fontface="bold",level.interval.color="white",level.interval.hjust="right",level.interval.nudge_x=level.rect.width-0.02,level.interval.nudge_y=.1,
                            level.name.family="serif",level.name.size=font.base.size*1.1,level.name.fontface="bold",level.name.color="white",level.name.hjust="left",level.name.nudge_x=.02,level.name.nudge_y=.1,level.name.linelength=30,
                            level.description.family="serif",level.description.size=font.base.size,level.description.fontface="plain",level.description.color="white",level.description.nudge_x=.02,level.description.nudge_y=.2,level.description.linelength=30,level.description.hjust="left",
                            # Items
                            items.include.categories=NULL,
                            items.rect.width=2,items.rect.fill=hcl.colors(4,palette=color.palette)[3],items.rect.color="white",
                            item.description.seperator="\n",item.description.bullet="* ",
                            items.description.family="serif",items.description.size=font.base.size,items.description.fontface="plain",item.description.color="white",items.description.nudge_x=.02,items.description.nudge_y=.1,items.description.linelength=30,items.description.hjust="left",
                            # Persons
                            person.parameters=ifelse(is.null(mod),null,mod$person$EAP),person.middle=F,
                            ngroups=nlevels,person.dist.type=c("in.levels","none"),person.probability.0=hcl.colors(4,palette = color.palette)[4],person.probability.100=hcl.colors(4,palette = color.palette)[1],
                            person.width=level.width,person1.base=level1.base,person.lims=person1.base+(0:ngroups)*person.width,
                            person.rect.width=.2,person.rect.color="white",
                            person.family="serif",person.size=font.base.size,person.fontface="plain",person.color="white",person.nudge_x=person.rect.width/10,person.nudge_y=.1,person.hjust="center",
                            # Headers
                            header.level="Levels",header.item="Items",header.persons=paste("Group",1:ngroups),header.show.percentage.in.group=T,
                            header.heigth=.2,header.rect.color="white",header.rect.fill="lightgray",header.hjust="center",header.nudge_x=0,header.nudge_y=.05,
                            header.family="serif",header.size=font.base.size,header.fontface="bold",header.color="black",
                            extra.verbose=F
) {
  if(is.null(item.params)) item.params=item.params.convert(mod,to="pcm")
  person.dist.type<-match.arg(person.dist.type)
  if(!is.null(color.palette) && !(color.palette %in% hcl.pals())) stop("You need to use a valid name (or NULL) for the color.palette. See hcl.pals().")
  nlevels<-length(level.lims)-1
  ngroups<-length(person.lims)-1
  if(!is.null(items.include.categories)) {
    item.params.include<-item.params[,1:items.include.categories]
    if(!is.null(item.descriptions))
      item.descriptions<-item.descriptions[,1:items.include.categories]
  }
  else item.params.include<-item.params
  if(is.null(item.descriptions)) {
    item.descriptions=item.descriptions.from.params(item.params.include)
  } else {
    if(is.data.frame(item.descriptions))
      item.descriptions<-as.matrix(item.descriptions)#data.frame(lapply(item.descriptions,as.character),stringsAsFactors=FALSE,row.names = rownames(item.descriptions))
    if(item.names.include)
      item.descriptions<-matrix(paste(item.descriptions.from.params(item.params),item.descriptions,sep=": "),ncol = ncol(item.descriptions))
  }
  # Get item thurstonian thresholds
  thresholds<-WrightMap::make.thresholds(item.params)
  if(!is.null(items.include.categories)) {
    thresholds<-thresholds[,1:items.include.categories]
  }  #
  inorder<-order(thresholds,decreasing = T)
  thresholds<-thresholds[inorder]
  item.descriptions<-item.descriptions[inorder]
  # Combine item descriptions in levels
  items.in.level<-linebreak(sapply(1:nlevels,function(l) paste0(item.description.bullet,paste(linebreak(item.descriptions[!is.na(thresholds) & thresholds>=level.lims[l] & thresholds<level.lims[l+1]],len = ifelse(grepl("\n",item.description.seperator),items.description.linelength,0)),collapse = paste0(item.description.seperator,item.description.bullet)))),len = ifelse(grepl("\n",item.description.seperator),0,items.description.linelength))

  # Warn if unused items
  items.out.of.level<-!is.na(thresholds) & (thresholds>=level.lims[nlevels+1] | thresholds<level.lims[1])
  if(length(items.out.of.level)>0) RDigram.warning(paste0("Some items/item categories were outside of the level interval: ",paste0(item.descriptions[items.out.of.level]," (",thresholds[items.out.of.level],")",collapse = ", "),". They are not included in the proficiency scale."),extra.verbose=extra.verbose)

  level.descriptions<-linebreak(level.descriptions,len = level.description.linelength)
  level.names<-linebreak(level.names,len = level.name.linelength)
  # lines in names
  lines.in.names<-rep(NA,nlevels)
  for(i in 1:nlevels) {
    lines.in.names[i]<-sum(unlist(gregexpr(pattern = "[\n]",text = level.names[i]))>-1)+1
  }
  data<-data.frame(level.names,level.descriptions,lines.in.names)
  if(relative.level.rect.heights) {
    totalheight<-level.rect.heights[nlevels+1]-level.rect.heights[1]
    num.lines<-rep(NA,nlevels)
    for(i in 1:nlevels) {
      descr.num.lines<-sum(unlist(gregexpr(pattern = "[\n]",text = paste(level.names[i],level.descriptions[i])))>-1)+2+include.level.interval # Names and description
      items.num.lines<-sum(unlist(gregexpr(pattern = "[\n]",text = items.in.level[i]))>-1)+1 # Items
      num.lines[i]<-max(descr.num.lines,items.num.lines)
    }
    heights<-totalheight*num.lines/sum(num.lines)
    for(i in 1:nlevels) level.rect.heights[i+1]<-level.rect.heights[i]+heights[i]
  }
  if(same.level.rect.heights) {
    totalheight<-level.rect.heights[nlevels+1]-level.rect.heights[1]
    for(i in 1:nlevels) level.rect.heights[i+1]<-level.rect.heights[i]+totalheight/nlevels
  }
  level.rect.left<-0
  level.rect.right<-level.rect.left+level.rect.width
  items.rect.left<-level.rect.right
  items.rect.right<-items.rect.left+items.rect.width

  pl<-
    ggplot2::ggplot(data=data)+
      theme_void()+ #theme(axis.line.x = element_line(colour = grid.color),axis.text.x = element_text(colour = grid.color))+
    # Header
    geom_tile(aes(x = level.rect.left+level.rect.width/2, width = level.rect.width, y = level.rect.heights[nlevels+1]+header.heigth/2, height = header.heigth),
              color = header.rect.color,  fill = header.rect.fill) +
    geom_text(aes(x = level.rect.left+level.rect.width/2, y = level.rect.heights[nlevels+1]+header.heigth,label = header.level),
              family=header.family,
              nudge_x = header.nudge_x,nudge_y = -header.nudge_y,
              hjust = header.hjust, vjust = "top",
              size = header.size, fontface=header.fontface,color=header.color) +
    geom_tile(aes(x = items.rect.left+items.rect.width/2, width = items.rect.width, y = level.rect.heights[nlevels+1]+header.heigth/2, height = header.heigth),
              color = header.rect.color,  fill = header.rect.fill) +
    geom_text(aes(x = items.rect.left+items.rect.width/2, y = level.rect.heights[nlevels+1]+header.heigth,label = header.item),
              family=header.family,
              nudge_x = header.nudge_x,nudge_y = -header.nudge_y,
              hjust = header.hjust, vjust = "top",
              size = header.size, fontface=header.fontface,color=header.color)


    # Level descriptions
    pl<-pl+
      geom_rect(aes(xmin = level.rect.left, xmax = level.rect.right, ymin = level.rect.heights[1:nlevels], ymax = level.rect.heights[2:(nlevels+1)]),
              color = level.rect.color,  fill = level.rect.fill)
    if(include.level.interval) {
      pl<-pl+
        geom_text(aes(x = level.rect.left, y = level.rect.heights[2:(nlevels+1)],
                      label = paste(level.lims[1:nlevels],level.lims[2:(nlevels+1)],sep = " \u2013 ")),
                  family=level.interval.family,
                  nudge_x = level.interval.nudge_x,nudge_y = -level.interval.nudge_y,
                  hjust = level.interval.hjust, vjust = "top",
                  size = level.interval.size, fontface=level.interval.fontface,color=level.interval.color)
    }
    pl<-pl+
      geom_text(aes(x = level.rect.left, y = level.rect.heights[2:(nlevels+1)],
                label = level.names),
                family=level.name.family,
                nudge_x = level.name.nudge_x,nudge_y = -level.name.nudge_y,
                hjust = level.name.hjust, vjust = "top",
                size = level.name.size, fontface=level.name.fontface,color=level.name.color) +
      geom_text(aes(x = level.rect.left , y = level.rect.heights[2:(nlevels+1)],
                label = level.descriptions),
                family=level.description.family,
                nudge_x = level.description.nudge_x,nudge_y = -level.name.nudge_y/2-(level.description.nudge_y-level.name.nudge_y/2)*lines.in.names,
                fontface=level.description.fontface,color=level.description.color,
                hjust = level.description.hjust, vjust = "top",
                size = level.description.size) +
      # Item descriptions
      geom_rect(aes(xmin = items.rect.left, xmax = items.rect.right, ymin = level.rect.heights[1:nlevels], ymax = level.rect.heights[2:(nlevels+1)]),
                color = items.rect.color, fill = items.rect.fill) +
      geom_text(aes(x = items.rect.left , y = level.rect.heights[2:(nlevels+1)],
                    label = items.in.level),
                family=items.description.family,
                nudge_x = items.description.nudge_x,nudge_y = -items.description.nudge_y,
                fontface=items.description.fontface,color=item.description.color,
                hjust = items.description.hjust, vjust = "top",
                size = items.description.size)

  # Correct response probabilities
      if(person.dist.type=="in.levels"){
        # We have nlevels+1 virtual items, and ask which probability ngroups have in relation to these items
        virtual.items<-level.lims
        virtual.thetas<-person.lims[1:ngroups]+if(person.middle) (person.lims[2:(ngroups+1)]-person.lims[1:ngroups])/2 else 0
        virtual.p<-digram.person.p(item.params = virtual.items,thetas = virtual.thetas)

        person.rect.left<--(1:(ngroups)*person.rect.width)
        person.rect.right<-person.rect.left+person.rect.width

        person.nudge_y<-c(-person.nudge_y,rep(0,nlevels-1),person.nudge_y)

        # quasiquotation: "The bang-bang operator !! forces a single object" to be evaluated now
        pl<-pl+scale_fill_gradient(low = person.probability.100,high = person.probability.0)
        if(header.show.percentage.in.group && !is.null(person.parameters)) {
          grps<-table(cut(person.parameters,breaks = person.lims))
          header.persons<-sapply(1:ngroups,function(g) paste0(header.persons[g],"\n(",format(grps[g]/length(person.parameters)*100,digits = 0)," %)"))
        }
        for(g in 1:ngroups) {
          ps<-virtual.p[[g]]$p[,2]
          probs<-round(ps*100)
          lower<-ps[nlevels+1]
          higher<-ps[1]
          interval<-higher-lower
          problims<-paste(probs,"%")
          yby<-(level.lims[nlevels+1]-level.lims[1])/100
          pl<-pl+
            geom_tile(aes(x = person.rect.left[!!g]+person.rect.width/2, width = person.rect.width, y = level.rect.heights[nlevels+1]+header.heigth/2, height = header.heigth),
                      color = header.rect.color,  fill = header.rect.fill) +
            geom_text(aes(x = person.rect.left[!!g]+person.rect.width/2, y = level.rect.heights[nlevels+1]+header.heigth,label = header.persons[!!g]),
                      family=header.family,
                      nudge_x = header.nudge_x,nudge_y = -header.nudge_y,
                      hjust = header.hjust, vjust = "top",
                      size = header.size, fontface=header.fontface,color=header.color) +


            geom_tile(data=data.frame(fill=seq(higher,lower,length.out = 100)),aes(x = person.rect.left[!!g]+person.rect.width/2, width = person.rect.width, y = seq(level.rect.heights[1]+yby/2,level.rect.heights[nlevels+1]-yby/2,length.out = 100), fill = fill)#level.rect.heights[1:ngroups], ymax = level.rect.heights[2:(ngroups+1)]
                    ) +
            geom_rect(aes(xmin = person.rect.left[!!g], xmax = person.rect.right[!!g], ymin = level.rect.heights[1],ymax=level.rect.heights[nlevels+1]),color = person.rect.color,fill=NA)+#,
            geom_text(data=data.frame(1:(nlevels+1)),aes(x = person.rect.left[!!g]+person.rect.width/2 , y = level.rect.heights[1:(nlevels+1)],
                      label = !!problims),
                      family=person.family,
                      nudge_x = person.nudge_x,nudge_y = -person.nudge_y,
                      fontface=person.fontface,
                      hjust = person.hjust,
                      size = person.size,
                      color=person.color) +
            theme(legend.position="none")
        }
      }
  if(!dontplot)
    print(pl)
  invisible(pl)

}
#' Describe items
#'
#' @param item.descriptions A matrix of item descriptions to edit.
#' @param mod An optional object of type tam.mml or eRm from which to extract the item.params.
#' @param item.params An optional matrix of item parameters to be used as a basis for the item.descriptions matrix.
#' @param outdir Where to save the output file.
#' @param file.name Filename of the output csv-file.
#'
#' @return
#' @export
#'
#' @examples
#' #' mod<-digram.estimate(DHP)
#' describe.items(mod)
describe.items<-function(item.descriptions=NULL,mod=NULL,item.params=NULL,outdir=getwd(),file.name="item.descriptions") {
  if(is.null(item.descriptions)) {
    if(is.null(item.params)) item.params=item.params.convert(mod,to="pcm")
    item.descriptions<-item.descriptions.from.params(item.params)
  }
  # item.descriptions<-edit(item.descriptions, factor.mode="character")
  # if(save)
  #   write.csv2(item.descriptions,file = file.name)
  editTable(item.descriptions, outdir=outdir, outfilename=file.name)

}

#' Produce a template matrix of item descriptions from item parameters
#'
#' @param item.params A matrix of item parameters, items in rows and categories in columns. Item-names as rownames.
#' @param mod An optional object of type tam.mml or eRm from which to extract the item.params.
#'
#' @return Returns a matrix of item descriptions.
#' @export
#'
#' @examples
#' mod<-digram.estimate(DHP)
#' item.descriptions.from.params(mod=mod)
item.descriptions.from.params<-function(item.params=NULL,mod=NULL) {
  if(is.null(item.params)) item.params=item.params.convert(mod,to="pcm")
  item.descriptions=as.matrix(item.params)
  item.descriptions[1:nrow(item.descriptions),]<-rownames(item.descriptions)
  if(ncol(item.descriptions)>1) {
    m<-matrix(rep(1:ncol(item.descriptions),nrow(item.descriptions)),ncol=ncol(item.descriptions),byrow = T)
    item.descriptions<-matrix(paste0(item.descriptions,".", m),
                              nrow=nrow(item.descriptions))
    item.descriptions[is.na(item.params)]<-NA
    rownames(item.descriptions)<-rownames(item.params)
    colnames(item.descriptions)<-paste("Category",1:ncol(item.descriptions))
  } else names(item.descriptions)<-names(item.params)
  item.descriptions
}

#' Read item descriptions from file
#'
#' @param mod A TAM model to be used when filtering the item descriptions (see details)
#' @param file.name An optional filename of the item descriptions. Default is 'item.descriptions.csv'.
#'
#' @return Returns item descriptions from file.
#' @details Item descriptions are filtered so descriptions of unused items are left out, and non-existing descriptions are given the item-name and cathegory
#' @export
#'
#' @examples
#' mod<-digram.estimate(DHP)
#' read.item.descriptions(mod=mod)

read.item.descriptions<-function(mod,file.name="item.descriptions.csv") {
  item.names<-item.descriptions.from.params(mod = mod)
  item.descriptions<-read.csv2(file = file.name,row.names = 1,stringsAsFactors = F)
  in1<-ncol(item.names)
  in2<-min(ncol(item.descriptions),in1)

  t(sapply(rownames(item.names),function(x) sapply(x,function(y) {
    z1<-which(rownames(item.names)==x)
    z2<-which(rownames(item.descriptions)==x)
    if(length(z2)==1) {
      r<-item.descriptions[z2,1:in2]
      if(in2<in1) r<-c(r,item.names[z1,(in2+1):in1])
      r[which(is.na(r))]<-item.names[z1,which(is.na(r))]
      r
    } else {
      item.names[z1,]
    }
  })
  ))
}


linebreak<-function(x,len=45) { if(len==0) x else apply(as.matrix(x),1,function(y) paste(strwrap(y,len),collapse="\n")) }

# library(rhandsontable)
# library(shiny)
editTable <- function(DF, outdir, outfilename){
  if(is.data.frame(DF)) {
    DF<-data.frame(lapply(DF,as.character),stringsAsFactors=FALSE,row.names = rownames(DF))
  } else  DF<-as.data.frame(DF,stringsAsFactors = F)
  ui <- shiny::shinyUI(shiny::fluidPage(

    shiny::verticalLayout(
      rhandsontable::rHandsontableOutput("hot"),
      shiny::wellPanel(
        shiny::actionButton("save", "Save")
      )
    )
  ))

  server <- shiny::shinyServer(function(input, output) {

    values <- shiny::reactiveValues()

    ## Handsontable
    shiny::observe({
      if (!is.null(input$hot)) {
        values[["previous"]] <- isolate(values[["DF"]])
        DF = rhandsontable::hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    })

    output$hot <- rhandsontable::renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable::rhandsontable(DF, stretchH = "all")
    })

    ## Save
    shiny::observeEvent(input$save, {
      finalDF <- shiny::isolate(values[["DF"]])
      write.csv2(finalDF, file=file.path(outdir, sprintf("%s.csv", outfilename)))
      #js$closeWindow()
      shiny::stopApp(finalDF)
    })
  })

  ## run app
  finalDF<-shiny::runApp(list(ui=ui, server=server))
  return(invisible(finalDF))
}
