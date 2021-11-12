  rm(list=c(ls()))
  ##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
  ##           							
  ##	Table 1:  First-Stage, Reduced Form, and IV Estimates Predicting the effects of Broadband Penetrati##  on on Affective Polarization  	
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
  load("mergedcounty.RData")
  ## Dataset without Demographics
  countyproviders <- countyproviders[,1:16]
    nodemos <- na.omit(merged[,c(1:16,21:22)])
  ## First Stage Model
  firstage <- lm(log(providers)~as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(Total),data=na.omit(countyproviders))
  firststageses <- cluster.robust.se(firstage,droplevels(as.factor(na.omit(countyproviders)$state)))
  firststageses[nrow(firststageses),3]^2
  ## Reduced Form Model
  reducedform <- lm(zero1(infeels-outfeels)~as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+region+log(HHINC)+log(Total),data=na.omit(nodemos))
  reducedformses <- cluster.robust.se(reducedform,droplevels(as.factor(na.omit(nodemos)$state)))
  

    ## IV Model
    ivests <- ivreg(zero1(infeels-outfeels)~as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(providers)|as.factor(year)+region+percent_black+percent_white+percent_male+lowed+unemploymentrate+density+log(HHINC)+log(Total),data=na.omit(nodemos))
    ivestsses <-cluster.robust.se(ivests,droplevels(as.factor(na.omit(nodemos)$state)))

texreg(l=list(firstage,reducedform,ivests),,override.se =list(firststageses[,2],reducedformses[,2],ivestsses[,2]),override.pval  =list(firststageses[,4],reducedformses[,4],ivestsses[,4]),reorder.coef = c(13,14,12:1),digits = 3, custom.coef.names = rev(c("\\# of Providers (logged)","ROW Index (logged)","Median Income (logged)","Population Density","Unemployment Rate","Low Education County","Percent Male","Percent White","Percent Black","West","South","Midwest","Year: 2008","Intercept")),custom.model.names = c("First Stage","Reduced Form","IV Estimates"),leading.zero = F,table = F,stars = .05,file="table1.tex")

