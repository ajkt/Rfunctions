library(likert)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(stringr)
library(extrafont)
# to use specific fonts, the *.ttf needs to be installed / registered with the R package: font_import(paths = "~/.local/share/fonts/",prompt = F)
# system.file("fontmap", "fonttable.csv", package="extrafontdb")
# R can't deal with fonts that have too many different types belonging to same font family - only regular, bold, italic, bold italic 
# -> delete other entries or change font family name of additional font entries, then loadfonts()

options("max.print"=200)

### Single-item Likert Scale Visualization
# 
# Requires:
#  > this.df: a likert class object with ordered factor levels
#  > my.title: a title/label for the Likert item 
#  > output.file: filename (pdf) where image is saved.
# Output: creates a horizontal divergent stacked barchart.
# 
###
singleItemLikert <- function(this.df, my.title, output.file) {
  
  numlevels <- length(this.df$levels)
  
  my.levels <- this.df$levels
  
  this.df <- this.df$results
  
  
  numcenter <- ceiling(numlevels/2)+1
  print(numcenter)
  
  # split middle/neutral answer into two halves (left and right of )
  print(this.df[,numcenter])
  this.df$midvalues <- this.df[,numcenter]/2
  print(this.df$midvalues) 
  
  #grab columns and replciate midvalues
  this.df2 <- cbind(this.df[,1], this.df[,2:(numcenter-1)],
                    this.df$midvalues,this.df$midvalues,this.df[,numcenter:numlevels+1])
  
  colnames(this.df2)<-c("item",my.levels[1:floor(numlevels/2)],"midlow",
                        "midhigh",my.levels[numcenter:numlevels])
  print(this.df2)
  
  #prettify y axis labels:
  #this.df2$item <- itemlabels
  
  
  #adjust for new number of columns, find ramge of values to avoid too much blank space
  numlevels <- numlevels+1
  point1 <- 2
  point2 <- ((numlevels)/2)+1
  point3 <- point2+1
  point4 <- numlevels+1
  mymin <- (ceiling(max(rowSums(this.df2[,point1:point2]))*4)/4)*-100
  mymax <- (ceiling(max(rowSums(this.df2[,point3:point4]))*4)/4)*100
  print(mymin)
  print(mymax)
  
  
  #create palette and have darkgrey (DFDFDF in it twice)
  numlevels<-length(this.df[1,])-1
  temp.rows<-length(this.df2[,1])
  pal<-brewer.pal((numlevels-1),"PuOr")
  pal[ceiling(numlevels/2)]<-"#DFDFDF"
  legend.pal<-pal
  pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
         pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])
  
  
  #melt into long form
  this.df3<-melt(this.df2,id="item")
  print(this.df3)
  
  this.df3$col<-rep(pal,each=temp.rows)
  this.df3$value<-this.df3$value #*100
  this.df3$item<-str_wrap(this.df3$item, width = 40)
  this.df3$item<-factor(this.df3$item, levels = this.df2$item[order(-(this.df2[,5]+this.df2[,6]+this.df2[,7]))])
  highs<-na.omit(this.df3[(length(this.df3[,1])/2)+1:length(this.df3[,1]),])
  lows<-na.omit(this.df3[1:(length(this.df3[,1])/2),])
  lows <- lows[rev(rownames(lows)),]
  print("HIGHS")
  print(highs)
  print("LOWS")
  print(lows)
  lows$col <- factor(lows$col, levels = c("#B35806","#F1A340", "#FEE0B6", "#DFDFDF")) # order factors! otherwise sorted alphabetically oO
  print("LOWS")
  print(lows)
  
  # highs would be : #DFDFDF,  #FEE0B6,  #F1A340,  #B35806
  
  
  my.bar.width <- 0.7    # depends on ggsave width & height!
  border.width <- 0.4
  ggplot() + geom_bar(data=highs, aes(x = item, y=value, fill=col), position="stack", stat="identity", width=my.bar.width, colour="black", size=border.width) +
    geom_bar(data=lows, aes(x = item, y=-value, fill=col), position="stack", stat="identity", width=my.bar.width, colour="black", size=border.width) +
    geom_hline(yintercept = 0, color =c("white")) +
    expand_limits(y=c(-70,70)) +
    scale_y_continuous(name="percent", breaks=seq(-60,60,20), limits=c(-70,70)) + #, limits=c(-100,100) 
    scale_fill_identity("", labels = my.levels, breaks=legend.pal, guide=guide_legend(nrow=1)) +  
    labs(title=my.title, y="",x="") +
    theme(axis.text.y = element_blank(), axis.ticks.y=element_blank())+ # vs. with axis labels on y: axis.title.y=element_text(size=8) + theme(axis.text.y = element_text(hjust=0)) +
    coord_flip() +
    theme(plot.title = element_text(size=10, hjust=0.5)) + 
    theme(legend.position = "bottom", legend.text = element_text(size=8)) +
    theme(text = element_text(family="Linux Libertine O"))
  ggsave(output.file, width=10, height=2)
  
  #embed_fonts("test.png", outfile="test_fonts_embedded.png")
  
}

### Multi-item Likert Scale Visualization
# 
# Requires: 
#  > this.ld: a likert class object with ordered factor levels
#  > my.titles: titles/labels for the Likert items
#  > output.file: filename (pdf) where image is saved.
# Output: creates horizontal divergent stacked barcharts in single plot.
### 
multiItemLikert <- function(this.ld, my.titles, output.file) {
  
  numlevels <- length(this.ld$levels)
  
  my.levels <- this.ld$levels
  
  this.ld <- this.ld$results
  print(this.ld)
  
  numcenter <- ceiling(numlevels/2)+1
  print(numcenter)
  
  # split middle/neutral answer into two halves (left and right of )
  print(this.ld[,numcenter])
  this.ld$midvalues <- this.ld[,numcenter]/2
  print(this.ld$midvalues) 
  
  #grab columns and replciate midvalues
  this.ld2 <- cbind(this.ld[,1], this.ld[,2:(numcenter-1)],
                    this.ld$midvalues,this.ld$midvalues,this.ld[,numcenter:numlevels+1])
  
  colnames(this.ld2)<-c("item",my.levels[1:floor(numlevels/2)],"midlow",
                        "midhigh",my.levels[numcenter:numlevels])
  print(this.ld2)
  
  #prettify y axis labels:
  this.ld2$item <- my.titles
  
  
  #adjust for new number of columns, find ramge of values to avoid too much blank space
  numlevels <- numlevels+1
  point1 <- 2
  point2 <- ((numlevels)/2)+1
  point3 <- point2+1
  point4 <- numlevels+1
  mymin <- (ceiling(max(rowSums(this.ld2[,point1:point2]))*4)/4)*-100
  mymax <- (ceiling(max(rowSums(this.ld2[,point3:point4]))*4)/4)*100
  print(mymin)
  print(mymax)
  
  
  #create palette and have darkgrey (DFDFDF in it twice)
  numlevels<-length(this.ld[1,])-1
  temp.rows<-length(this.ld2[,1])
  pal<-brewer.pal((numlevels-1),"PuOr")
  pal[ceiling(numlevels/2)]<-"#DFDFDF"
  legend.pal<-pal
  pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
         pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])
  
  
  #melt into long form
  this.ld3<-melt(this.ld2,id="item")
  print(this.ld3)
  
  this.ld3$col<-rep(pal,each=temp.rows)
  this.ld3$value<-this.ld3$value #*100
  this.ld3$item<-str_wrap(this.ld3$item, width = 40)
  this.ld3$item<-factor(this.ld3$item, levels = this.ld2$item[order(-(this.ld2[,5]+this.ld2[,6]+this.ld2[,7]))])
  highs<-na.omit(this.ld3[(length(this.ld3[,1])/2)+1:length(this.ld3[,1]),])
  lows<-na.omit(this.ld3[1:(length(this.ld3[,1])/2),])
  lows <- lows[rev(rownames(lows)),]
  print("HIGHS")
  print(highs)
  print("LOWS")
  print(lows)
  lows$col <- factor(lows$col, levels = c("#B35806","#F1A340", "#FEE0B6", "#DFDFDF")) # order factors! otherwise sorted alphabetically oO
  print("LOWS")
  print(lows)
  
  # highs would be : #DFDFDF,  #FEE0B6,  #F1A340,  #B35806
  
  
  my.bar.width <- 0.5    # depends on ggsave width & height!
  border.width <- 0.4
  ggplot() + geom_bar(data=highs, aes(x = item, y=value, fill=col), position="stack", stat="identity", width=my.bar.width, colour="black", size=border.width) +
    geom_bar(data=lows, aes(x = item, y=-value, fill=col), position="stack", stat="identity", width=my.bar.width, colour="black", size=border.width) +
    geom_hline(yintercept = 0, color =c("white")) +
    expand_limits(y=c(-70,70)) +
    scale_y_continuous(name="percent", breaks=seq(-60,60,20), limits=c(-70,70)) + #, limits=c(-100,100) 
    scale_fill_identity("", labels = my.levels, breaks=legend.pal, guide=guide_legend(nrow=1)) +  
    labs(title="", y="",x="") +
    theme(axis.title.y = element_text(size=8), axis.text.y=element_text(hjust=0)) + 
    coord_flip() +
    theme(plot.title = element_text(size=10, hjust=0.5)) + 
    theme(legend.position = "bottom", legend.text = element_text(size=8)) +
    theme(text = element_text(family="Linux Libertine O"))
  ggsave(output.file, width=10, height=4)
  
}


### EXAMPLE SINGLE ITEM LIKERT: 
exd <- read.csv("example-likert.csv", header=TRUE)
exd <- exd$x
exd
levels(exd)


# reorder factor levels
head(exd)
exd = factor(exd, levels(exd)[c(7,2,5,3,4,1,6)])
levels(exd)
head(exd)

# create likert object
lexd <- likert(as.data.frame(exd))
lexd

# call function
singleItemLikert(lexd,  "Background music is an important part of playing my favourite game.", "test.pdf")





### EXAMPLE MULTI ITEM LIKERT:
mexd <- read.csv("example-multi-likert.csv",header=TRUE)
mexd <- mexd[, 2:4]
ncol(mexd)

levels(mexd[,1])
levels(mexd[,2])
levels(mexd[,3])

# if factor levels are missing, for example A1 and A4:
# mexd[,1] <- as.factor(mexd[,1])
# levels(mexd[,1])
# levels(mexd[,1]) <- c(levels(mexd[,1]), "A1","A4")
# levels(mexd[,1])

# if factors need reordering:
# mexd[,1] = factor(mexd[,1], levels(mexd[,1])[c(5,2,4,7,3,1,6)]) 
# levels(mexd[,1])



# then once all factor levels in correct order:

mlexd <- likert(as.data.frame(mexd))
mlexd

# call function
multiItemLikert(mlexd, c("I felt competent when playing.","I enjoyed playing.","I thought the game was easy to control."), "mtest.pdf")

