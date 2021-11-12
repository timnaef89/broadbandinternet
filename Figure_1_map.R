##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##           							
##	Generate Map of ROW Laws by State
##
##	Yphtach Lelkes						
## 	Last Edited: 13/07/15 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

## load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(maps,ggplot2,grid,scales,Rcpp)
devtools::install_github('ggthemr', 'cttobin')
library(ggthemr)
bbindex <- read.csv("bbindex.csv")
bbindex$State <- gsub("_"," " ,bbindex$State)

##############

#load us map data
bbindex$region <- tolower(bbindex$State)
all_states <- map_data("state")
mergedformap <-   merge(all_states,bbindex,by="region")
mergedformap$ROW <- log(mergedformap$Total)
#plot all states with ggplot
p <- ggplot()
f <- p + geom_polygon( data=mergedformap, aes(x=long, y=lat),colour="white", fill="grey10" ,border="white")
ggthemr("chalk")
ggplot(data = mergedformap, aes(x = long, y = lat, fill = ROW, group = group)) +geom_polygon()+xlab("")+ylab("")+theme_bw()+scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+ theme(legend.position = c(.9, .4))+scale_fill_gradientn(colours=gray.colors(10),breaks=c(0,2.5,5),labels=c("Most Restrictive",0.5,"Least Restrictive"),limits=c(0,5),name="ROW \n")+theme(text=element_text(size=20),legend.position = c(.1,.2))+theme(legend.key.height=unit(1.25,"line"),legend.key.size=unit(1,"line"))

ggsave("rowmap.pdf",width=12,height=8) 
