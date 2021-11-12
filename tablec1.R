rm(list=c(ls()))
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##           							
##	Does broadband affect some countes mre than others?
##	for Hostile Audience, AJPS
##  Yphtach Lelkes						
## 	Last Edited: 15/07/15 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ivpack,texreg)

## rescale items to lie between 0 and 1
zero1 <- function(x, minx=NA, maxx=NA){
  res <- NA
  if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
  if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
  res
}
load("mergedcounty.RData")
names(countyproviders)
countyproviders <- na.omit(countyproviders[,1:16])
who <- lm(providers~as.factor(year)+log(Total)+region*log(Total)+percent_black*log(Total)+percent_white*log(Total)+percent_male*log(Total)+lowed*log(Total)+unemploymentrate*log(Total)+density*log(Total)+log(HHINC)*log(Total),data=na.omit(countyproviders))
whoses <- cluster.robust.se(who,clusterid = droplevels(as.factor(na.omit(countyproviders)$state)))

texreg(l=list(who),,override.se =list(whoses[,2]),override.pval  =list(whoses[,4]),digits = 3, custom.coef.names = c("Intercept","Year: 2008","ROW (logged)","Midwest","South","West","Percent Black","Percent White","Percent Male","Low Education County","Unemployment Rate","Population Density","Median Income (logged)","Midwest x ROW (logged)","South x ROW (logged)","West x ROW (logged)","Percent Black x ROW (logged)","Percent White x ROW (logged)","Percent Male x ROW (logged)","Low Education County x ROW (logged)","Unemployment Rate x ROW (logged)","Population Density x ROW (logged)","Median Income (logged) x ROW (logged)"),leading.zero = F,table = F,file = "who.tex",stars =c(.05))
