rm(list=c(ls()))
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##             						
##	Figure 3
##	for Hostile Audience, AJPS
##  Gaurav Sood						
## 	Last Edited: 21/07/14 by yl
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
load("gentzkow.RData")

if (!require("pacman")) install.packages("pacman")
pacman::p_load( plyr, stargazer,cem,MatchIt,effects,ggthemr,gridExtra)


# ggplot
partmedia 		<- lm(sum ~speed + household_size + (hoh_oldest_age) + (household_income) + (children) + (racial_background) + (country_of_origin), data=matched12,weight=weights)


c1 <- effect(partmedia,term="speed")
forggplot <- data.frame(y=c1$fit,speed=c1$x, upper=(c1$upper),lower=(c1$lower),media="Selecting \n Partisan Media")

ggthemr("fresh")

p <- ggplot(forggplot,aes(x=speed,y=y))+
  geom_bar(stat="identity")+
  geom_linerange(aes(ymin=lower,ymax=upper,y=y))+
  ylab("Minutes")+
  xlab("Type of Connection at Home")
p
ggsave(file="comScorebbonmedia.pdf", width = 8, height = 6)


