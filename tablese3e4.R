rm(list=c(ls()))
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##           							
##	Pew
##	for Hostile Audience, AJPS
##  Yphtach Lelkes						
## 	Last Edited: 21/07/14 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
if (!require("pacman")) install.packages("pacman")
pacman::p_load( gtools, stargazer,cem,MatchIt,xtable)

## rescale items to lie between 0 and 1
zero1 <- function(x, minx=NA, maxx=NA){
  res <- NA
  if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
  if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
  res
}

pew <- read.csv("CopyOfNov 2004 Post-Election_csv.csv")

formatch <- na.omit((with(pew,data.frame(broadband,blogs,onlinenews,income,age,race,educ1,interest,age1,sex=sex,pid))))

  

matched <- matchit(as.numeric(broadband)-1~age1+income+educ1+interest+race+pid+sex,data = formatch,method="cem")
outb <- glm(onlinenews==1|blogs==1~broadband,match.data(matched),family="binomial",weight=weights)
outc <- glm(onlinenews==1|blogs==1~broadband+age1+income+educ1+interest+race+pid+sex,match.data(matched),family="binomial",weight=weights)

fortabs <- summary(matched)
stargazer(outb,outc,covariate.labels = c("Highspeed Internet","Age: 2nd T","Age: 3rd T","Income, 2nd T","Income, 3rd T","Education: College","Education: Some College","Political Interest","Race: White","PID: Independent","PID: Republican","Female","Constant"),dep.var.labels = "Visits Partisan Sites",out="pewregs.tex",float = F)
f1 <- cbind(fortabs$sum.all[,1:2],fortabs$sum.matched[,1:2])
rownames(f1) <- c("Distance","Age, 1st T","Age, 2nd T","Age, 3rd T","Income 2nd T","Income 3rd T","Education: College","Education: Some college","Interest","Race: White","PID: Independent","PID: Republican","Female")
                  
addtorow <- list()
addtorow$pos <- list(-1)
addtorow$command <- paste0('\\hline Covariate & \\multicolumn{2}{c}{All} & \\multicolumn{2}{c}{Matched}', '\\\\')
print(xtable(f1),add.to.row=addtorow,file="balancepew.tex",floating=F)

