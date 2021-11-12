rm(list=c(ls()))
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##           							
##	Table 2  	
##	for Hostile Audience, AJPS
##  Yphtach Lelkes						
## 	Last Edited: 15/07/15 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
if (!require("pacman")) install.packages("pacman")
pacman::p_load(AER, ivpack,texreg)

## rescale items to lie between 0 and 1
zero1 <- function(x, minx=NA, maxx=NA){
  res <- NA
  if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
  if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
  res
}
load("mergeddataset.RData")
nodemos <- merged[,c(1:16,21:22,28)]
yearhet <- ivreg(zero1(infeels-outfeels)~as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(providers)*as.factor(year)|as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(Total)*as.factor(year),data=na.omit(nodemos[,-19]))
yearhetses <-  cluster.robust.se(yearhet,droplevels(as.factor(na.omit(nodemos[,-19])$state)))


interesthet <- ivreg(zero1(infeels-outfeels)~as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(providers)*zero1(interest)|as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(Total)*zero1(interest),data=na.omit(nodemos))

interesthetses <-  cluster.robust.se(interesthet,droplevels(as.factor(na.omit(nodemos)$state)))


texreg(l=list(yearhet,interesthet),override.se =list(yearhetses[,2],interesthetses[,2]),override.pval  =list(yearhetses[,4],interesthetses[,4]),digits = 3,custom.coef.names = rev(c("\\# of Providers (logged) * Political Interest","Political Interest","\\# of Providers (logged) * Year","\\# of Providers (logged)","Median Income (logged)","Population Density","Unemployment Rate","Low Education County","Percent Male","Percent White","Percent Black","West","South","Midwest","Year: 2008","Intercept")),reorder.coef = c(14,16,15,2,13:3,1),,leading.zero = F,table = F,file = "table2.tex",stars =c(.05))


