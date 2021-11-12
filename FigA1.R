if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggthemes,ggplot2)

## Script to create graph of ROW by state
bbindex <- read.csv("bbindex.csv")

bbindex$state <- gsub("_"," ",bbindex$State)
bbindex$state <-  factor(bbindex$state,levels=as.character(bbindex$state))

ggplot(na.omit(bbindex),aes(y=state,x=Total))+geom_point()+xlab("ROW Index")+ylab("")+theme_calc()
ggsave("bbindex.pdf",width=6,height=8)

