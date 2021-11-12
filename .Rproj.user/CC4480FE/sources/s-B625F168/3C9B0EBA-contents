##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##             						
##	Graph plotting # of subscribers by number of providers  	
##  Figure 2 in Hostile Audience, AJPS
##	Yphtach Lelkes						
## 	Last Edited: 13/07/15 by YL
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

## load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, ggthemr, scales)
ggthemr("fresh")

#load data 
hh <- read.csv("hstracts.csv")
ggplot(hh,aes(x=log(total_prov+1),y=rfhsc_per_1000_hhs))+geom_smooth(method='gam',formula=y~s(x,k=3,bs='cs'))+xlab("Number of Providers (logged)")+ylab("Subscriptions per 1000 Residents")+scale_y_continuous(labels=c("0","0 < x <= 200","200 < x <= 400","400 < x <= 600","600 < x<= 800","800 < x"),limits=c(0,5))
ggsave("fccproviderssubscriptions.pdf")
