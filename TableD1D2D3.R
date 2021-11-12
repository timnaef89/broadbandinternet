rm(list=c(ls()))
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##                                              
##     Tables D1 and D2 and D3
##     for Hostile Audience, AJPS
##  Yphtach Lelkes                              
##      Last Edited: 15/07/15 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

## D1
if (!require("pacman")) install.packages("pacman")
pacman::p_load(AER, ivpack,texreg,stringr)

## rescale items to lie between 0 and 1
zero1 <- function(x, minx=NA, maxx=NA){
  res <- NA
  if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
  if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
  res
}
load("mergeddataset.RData")
names(merged)
## Diff specs
merged <- merged[,c(1:16,20:27)]
merged$education <- factor(merged$education,levels=c("HS or Less","Some College","College","More than college","Missing"))
merged$race <- relevel(car::recode(as.numeric(as.factor(merged$race)),"1='White';2='Other';3='Black';else='Other'",as.factor=T),ref='Other')
reducedform1 <- lm(zero1(infeels-outfeels)~log(Total),data=na.omit(merged))
reducedform2 <- lm(zero1(infeels-outfeels)~as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+log(Total),data=na.omit(merged))
reducedform3 <- lm(zero1(infeels-outfeels)~as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(Total),data=na.omit(merged))
reducedformses1 <- cluster.robust.se(reducedform1,droplevels(na.omit(merged)$state))
reducedformses2 <- cluster.robust.se(reducedform2,droplevels(na.omit(merged)$state))
reducedformses3 <- cluster.robust.se(reducedform3,droplevels(na.omit(merged)$state))

texreg(l=list(reducedform1,reducedform2,reducedform3),override.se =list(reducedformses1[,2],reducedformses2[,2],reducedformses3[,2]),override.pval  =list(reducedformses1[,4],reducedformses2[,4],reducedformses3[,4]),reorder.coef = c(2,3:29,1),digits=3,custom.coef.names = c("Intercept","ROW Index (logged)","Year: 2008",'Democrat',"Race: Black","Race: White","Female","Age: 2nd Quartile","Age: 3rd Quartile","Age: 4th Quartile","Age: Missing","Education: Some College","Education: College","Education: More than College","Education: Missing","Income: 2nd Quartile","Income: 3rd Quartile","Income: 4th Quartile","Income: Missing",rev(c("Median Income (logged)","Population Density","Unemployment Rate","Low Education County","Percent Male","Percent White","Percent Black","West","South","Midwest"))),leading.zero = F,file = "diffspecsreducedform.tex",stars =c(.05),table=F)


#################################################
## ## D2
ivest1 <- ivreg(zero1(infeels-outfeels)~log(providers)|log(Total),data=na.omit(merged))
ivest2 <- ivreg(zero1(infeels-outfeels)~as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+log(providers)|as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+log(Total),data=na.omit(merged))
ivest3 <- ivreg(zero1(infeels-outfeels)~log(providers)+as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)|log(Total)+as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC),data=na.omit(merged))
ivest1ses <- cluster.robust.se(ivest1,droplevels(na.omit(merged)$state))
ivest2ses <- cluster.robust.se(ivest2,droplevels(na.omit(merged)$state))
ivest3ses <- cluster.robust.se(ivest3,droplevels(na.omit(merged)$state))


ivest3 <- ivreg(zero1(infeels-outfeels)~log(providers)+as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)|log(Total)+as.factor(year)+pid+race+female+as.factor(agecut)+education+as.factor(incomecut)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC),data=na.omit(merged))

texreg(l=list(ivest1,ivest2,ivest3),override.se =list(ivest1ses[,2],ivest2ses[,2],ivest3ses[,2]),override.pval  =list(ivest1ses[,4],ivest2ses[,4],ivest3ses[,4]),reorder.coef = c(2,3:29,1),digits=3,custom.coef.names = c("Intercept","\\# of Providers","Year: 2008","Democrat","Race: Black","Race: White","Female","Age: 2nd Quartile","Age: 3rd Quartile","Age: 4th Quartile","Age: Missing","Education: Some College","Education: College","Education: More than College","Education: Missing","Income: 2nd Quartile","Income: 3rd Quartile","Income: 4th Quartile","Income: Missing",rev(c("Median Income (logged)","Population Density","Percent Male","Percent College","Percent White","Percent Black","West","South","Midwest","Percent Bush"))),leading.zero = F,file = "diffspecsivests.tex",stars =c(.05),table=F)
###
## D3
load("mergeddataset.RData")
load("mergedcounty.RData")
names(merged)
nodemos <- na.omit(merged[,c(1:17,21:22)])
## First Stage Model
firstage <- lm(providers~as.factor(year)+typography,data=na.omit(countyproviders[,1:19]))
firststageses <- cluster.robust.se(firstage,na.omit(countyproviders[,1:19])$fipscode)
## Reduced Form Model
reducedform <- lm(zero1(infeels-outfeels)~as.factor(year)+typography,data=nodemos)
reducedformses <- cluster.robust.se(reducedform,nodemos$fipscode)
## IV Model
ivests <- ivreg(zero1(infeels-outfeels)~as.factor(year)+log(providers)|as.factor(year)+typography,data=nodemos)
ivestsses <- cluster.robust.se(ivests,nodemos$fipscode)
metro <- rev(str_trim(substring(levels(merged$typography)[-1],4,100),"left"))
texreg(l=list(firstage,reducedform,ivests),override.se =list(firststageses[,2],reducedformses[,2],ivestsses[,2]),override.pval  =list(firststageses[,4],reducedformses[,4],ivestsses[,4]),reorder.coef = c(23:1),digits = 3, custom.coef.names = rev(c("\\# of Providers (logged)",metro,"Year: 2008","Intercept")),custom.model.names = c("First Stage","Reduced Form","IV Estimates"),leading.zero = F,table = F,file = "terraintabs.tex",stars =c(.05))


##
## D4
load("mergeddataset.RData")
load("mergedcounty.RData")
names(merged)
nodemos <- na.omit(merged[,c(1:16,18:22)])
## First Stage Model
firstage <- lm(log(providers)~as.factor(year)+zero1(slope),data=na.omit(nodemos))
firststageses <- cluster.robust.se(firstage,na.omit(nodemos)$fipscode)
## Reduced Form Model
reducedform <- lm(zero1(infeels-outfeels)~as.factor(year)+zero1(slope),data=na.omit(nodemos))
reducedformses <- cluster.robust.se(reducedform,nodemos$fipscode)
## IV Model
ivests <- ivreg(zero1(infeels-outfeels)~as.factor(year)+log(providers)|as.factor(year)+zero1(slope),data=na.omit(nodemos))
ivestsses <- cluster.robust.se(ivests,nodemos$fipscode)
texreg(l=list(firstage,reducedform,ivests),override.se =list(firststageses[,2],reducedformses[,2],ivestsses[,2]),override.pval  =list(firststageses[,4],reducedformses[,4],ivestsses[,4]),reorder.coef = c(4:1),digits = 3, custom.coef.names = rev(c("\\# of Providers (logged)","Average Slope","Year: 2008","Intercept")),custom.model.names = c("First Stage","Reduced Form","IV Estimates"),leading.zero = F,table = F,file = "slopetabs.tex",stars =c(.05) )

## ALL THREE INSTRUMENTS
nodemos <- na.omit(merged[,c(1:24)])
fullmethod <- ivreg(zero1(infeels-outfeels)~log(providers)|log(Total)+typography+zero1(slope),data=na.omit(merged))
cluster.robust.se(fullmethod,droplevels(as.factor(na.omit(merged)$state)))
