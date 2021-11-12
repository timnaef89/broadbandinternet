
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##           							
##	ComScore Providers Graph  	
##	Yphtach Lelkes						
## 	Last Edited: 11/07/14 by YL
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

## Libraries
library(ggplot2)
library(ggthemr)
require(scales)
ggthemr("fresh")

##
### Read in Comscore Data from Year of Study, and merge the 2 years
cs2 <- read.csv("demographics2004.csv")
cs5 <- read.csv("demographics2008.csv")
cs2$year <- 2004
cs5$year <- 2008
comscore <- rbind(cs2,cs5)
## Read in Zipcode level providersand merge
zips2004 <- read.csv('providers_ziplevel2004.csv')
zips2004 <- zips2004[,c(1:3)]
zip08 <- read.csv("providers_ziplevel2008.csv")
zip08$year = 2008
zips2004$year = 2004
names(zip08) <- names(zips2004)
providers <- rbind(zip08,zips2004)

## Merge providers and comscore
comout <- na.omit(merge(providers,comscore,by.x = c("zipcode","year"),by.y =c("zip_code","year")))

## GGPlot
ggplot(comout,aes(x=log(zipproviders),y=connection_speed))+geom_smooth(method='gam',formula=y~s(x,k=3,bs='cs'))+ylab("Proportion with \n High Speed Internet")+xlab("Number of Providers (logged)")+ylim(0,1)
ggsave("comscoreproviderssubscribers.pdf",width=8,height=6)
