rm(list = c(ls()))
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##     										
##	NES 2012 Broadband vs. Dial-up Partisan Exposure   	
##	Yph Lelkes						
## 	Last Edited: 10/31/14 by YL
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
if (!require("pacman")) install.packages("pacman")
pacman::p_load(gtools, Hmisc,plyr, stargazer,cem,MatchIt,effects,ggthemr,gridExtra,xtable)

load("CopyOfanes_timeseries_2012.sav")
# Prepping for Matching
formatch$interest_following <- as.numeric(droplevels(formatch$interest_following))
formatch$income <- quantcut(as.numeric(formatch$inc_incgroup_pre),q = seq(from=0,to=1,by=.25))
formatch$race <- (droplevels(formatch$dem_raceeth_x))
formatch$age <- quantcut(as.numeric(formatch$dem_age_r_x),q = seq(from=0,to=1,by=.25))

# Matching using CEM
matched <- matchit(as.numeric(broadband)-1~interest_following+income+race+educ+age+gender_respondent_x,data = formatch,method="cem")
# Matched dataset
matched12 <- match.data(matched)
summary(matched)
## Effect of broadband on media choice
conmedia <- glm(conmedia>0~pid2*broadband+interest_following+income+race+educ+age+gender_respondent_x, data=matched12,weight=weights,family="binomial")
libmedia <- glm(libmedia>0~pid2*broadband+interest_following+income+race+educ+age+gender_respondent_x, data=match.data(matched),weight=weights,family="binomial")

# Export effects to data.frame using effects package  
c1 <- (effect(conmedia,term="pid2*broadband"))
l1 <- (effect(libmedia,term="pid2*broadband"))

############################ 
## ggplot                 ##
############################ 
forggplot <- rbind(
  data.frame(y=inv.logit(c1$fit),pid=c1$x[,1],internet=c1$x[,2],upper=inv.logit(c1$upper),lower=inv.logit(c1$lower),media="Probability of Selecting \n Conservative Media"),
  data.frame(y=inv.logit(l1$fit),pid=l1$x[,1],internet=l1$x[,2],upper=inv.logit(l1$upper),lower=inv.logit(l1$lower),media="Probability of Selecting \n Liberal Media")
)


### Code to switch facet_strips
switch_facet_strip <- function(p, switch = c("x", "y")) {
  
  require(gtable)
  rbind_gtable <- gtable:::rbind_gtable
  cbind_gtable <- gtable:::cbind_gtable
  
  if ("y" %in% switch)
    p <- p + theme(strip.text.y = element_text(vjust = 0.5, angle = 90))
  
  g <- ggplotGrob(p)
  
  
  gdim <- as.numeric(g$layout[g$layout$name == "background", c("b", "r")])
  tpos <- g$layout[g$layout$name == "strip-top", "b"][1]
  rpos <- g$layout[g$layout$name == "strip-right", "r"][1]
  new_tpos <- g$layout[g$layout$name == "axis-b", "b"][1] + 1
  new_rpos <- g$layout[g$layout$name == "axis-l", "r"][1] - 1
  
  if ("x" %in% switch) {
    g <- rbind_gtable(
      rbind_gtable(
        gtable_add_rows(
          rbind_gtable(g[1:tpos-1, ] , g[(tpos+1):(new_tpos-1), ], "first"),
          unit(5, units = "mm")),
        g[tpos, ], "first"),
      g[new_tpos:gdim[1], ], "first")
  }
  
  if ("y" %in% switch) {
    g <- cbind_gtable(
      cbind_gtable(
        gtable_add_cols(
          cbind_gtable(g[, 1:new_rpos], g[, rpos], "first"),
          unit(5, units = "mm")),
        g[, (new_rpos+2):rpos-1], "first"),
      g[, (rpos+1):gdim[2]], "first")
  }
  
  grid.newpage()
  grid.draw(g)
}

###
levels(forggplot$pid) <- c("Democrats","Independents","Republicans")
ggthemr("fresh")
p <- ggplot(subset(forggplot,pid!='Independents'),aes(x=internet,y=y))+
  facet_grid(media~pid)+geom_bar(stat="identity")+
  geom_linerange(aes(ymin=lower,ymax=upper,y=y))+
  ylab("")+
  xlab("Type of Connection at Home")

switch_facet_strip(p,"y")

dev.copy2pdf(file="naesbbonmedia.pdf", width = 8, height = 6)

### Regressions
formatch$interest_following <- as.numeric((formatch$interest_following))
formatch$income <- quantcut(as.numeric(formatch$inc_incgroup_pre),q = seq(from=0,to=1,by=.25))
formatch$race <- (droplevels(formatch$dem_raceeth_x))
formatch$age <- quantcut(as.numeric(formatch$dem_age_r_x),q = seq(from=0,to=1,by=.25))
formatch$educ <- factor(formatch$educ,levels=c("HS or Less","Some College","College","More than College"))

conmedia1 <- glm(conmedia>0~broadband+interest_following+income+race+educ+age+gender_respondent_x, data=subset(formatch,pid2=='Republican'),family="binomial")
conmedia2 <- glm(conmedia>0~broadband+interest_following+income+race+educ+age+gender_respondent_x, data=subset(formatch,pid2=='Democrat'),family="binomial")


libmedia1 <- glm(libmedia>0~broadband+interest_following+income+race+educ+age+gender_respondent_x, data=subset(formatch,pid2=='Republican'),family="binomial")
libmedia2 <- glm(libmedia>0~broadband+interest_following+income+race+educ+age+gender_respondent_x, data=subset(formatch,pid2=='Democrat'),family="binomial")

library(stargazer)

stargazer(conmedia1,conmedia2,libmedia1,libmedia2,covariate.labels = c("Dial-Up","Political Interest","Income: 2nd Quartile","Income: 3rd Quartile","Income: 4th Quartile","Race: Black","Race: Hispanic","Race: Other","Education: Some College","Education: College","Education: More than College","Age: 2nd Quartile","Age: 3rd Quartile","Age: 4th Quartile","Female","Intercept"),dep.var.labels = c("Selected Conservative Media","Selected Liberal Media"),column.labels = c("Republicans","Democrats","Republicans","Democrats"),model.numbers = F,out = "nesregs.tex",label="tab:nesregs",title ="Log-odds of selecting partisan media condition on internet speed by Party ID",no.space = T)

fortabs <- summary(matched)
f1 <- cbind(fortabs$sum.all[,1:2],fortabs$sum.matched[,1:2])
rownames(f1) <- c("Distance","Political Interest","Income 1st Quartile","Income 2nd Quartile","Income 3rd Quartile","Income 4th Quartile","Black","Hispanic","Race, Other","Education, HS or Less","Education: More than College","Education: Some college","Age, 2nd Quartile","Age 3rd Quartile","Age 4th Quartile","Female")

addtorow <- list()
addtorow$pos <- list(-1)
addtorow$command <- paste0('\\hline Covariate & \\multicolumn{2}{c}{All} & \\multicolumn{2}{c}{Matched}', '\\\\')
print(xtable(f1),add.to.row=addtorow,file="balanceanes.tex",floating=F)
