##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##           							
##	Validation Checks of ROW
##	for Hostile Audience, AJPS
##  Yphtach Lelkes						
## 	Last Edited: 13/05/14 by yl
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

#### ROW related to state ideology?
bbindex <- read.csv("bbindex.csv")
bbindex$state <- gsub("_"," ",bbindex$State)
bbindex$State <- bbindex$state


stateideo <- read.csv("T_S_ideologystate.csv")
stateideo$state <- state.name[match(stateideo$abb,state.abb)]
check1 <- merge(stateideo,bbindex,by="state")
summary(lm(mrp_estimate~log(Total),check1))

### Check 2: Governor related to ROW
data <- read.csv("Partisan_Balance_For_Use2011_06_09b.csv")
# Merge the two
all <- merge(bbindex,data,by="state")
all$repgov <- all$govparty_c==1
### Governor's Party
l1 <- glm(repgov~log(Total),data=subset(all,year==1996),family="binomial")
l2 <- glm(repgov~log(Total),data=subset(all,year==1998),family="binomial")
l3 <- glm(repgov~log(Total),data=subset(all,year==2000),family="binomial")
l4 <- glm(repgov~log(Total),data=subset(all,year==2002),family="binomial")
l5 <- glm(repgov~log(Total),data=subset(all,year==2004),family="binomial")
l6 <- glm(repgov~log(Total),data=subset(all,year==2006),family="binomial")
l7 <- glm(repgov~log(Total),data=subset(all,year==2008),family="binomial")
meanb <- cbind(l1$coefficients[2],
            l2$coefficients[2],
            l3$coefficients[2],
            l4$coefficients[2],
            l5$coefficients[2],
            l6$coefficients[2],
            l7$coefficients[2])
meanp <- cbind(summary(l1)$coefficients[2,4],
            summary(l2)$coefficients[2,4],
            summary(l3)$coefficients[2,4],
            summary(l4)$coefficients[2,4],
            summary(l5)$coefficients[2,4],
            summary(l6)$coefficients[2,4],
            summary(l7)$coefficients[2,4])
mean(meanb)
mean(meanp)

### Check 3: Legislature Ideology related to ROW
load("shor mccarty 1993-2013 state aggregate data public July 2014.RData")
x$state <- state.name[match(x$st,state.abb)]
# Merge the two
ideoleg <- merge(bbindex,x,by="state")
l1 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==1996))
l2 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==1998))
l3 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==2000))
l4 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==2002))
l5 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==2004))
l6 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==2006))
l7 <- lm(sen_chamber  ~log(Total),data=subset(ideoleg,year==2008))

ll <- cbind(l1$coefficients[2],
            l2$coefficients[2],
            l3$coefficients[2],
            l4$coefficients[2],
            l5$coefficients[2],
            l6$coefficients[2],
            l7$coefficients[2])
senmeanrel <- mean(ll)

ll <- cbind(summary(l1)$coefficients[2,4],
            summary(l2)$coefficients[2,4],
            summary(l3)$coefficients[2,4],
            summary(l4)$coefficients[2,4],
            summary(l5)$coefficients[2,4],
            summary(l6)$coefficients[2,4],
            summary(l7)$coefficients[2,4])
senmeanp <- mean(ll)

####
l1 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==1996))
l2 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==1998))
l3 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==2000))
l4 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==2002))
l5 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==2004))
l6 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==2006))
l7 <- lm(hou_chamber  ~log(Total),data=subset(ideoleg,year==2008))

ll <- cbind(l1$coefficients[2],
            l2$coefficients[2],
            l3$coefficients[2],
            l4$coefficients[2],
            l5$coefficients[2],
            l6$coefficients[2],
            l7$coefficients[2])
houmeanrel <- mean(ll)

ll <- cbind(summary(l1)$coefficients[2,4],
            summary(l2)$coefficients[2,4],
            summary(l3)$coefficients[2,4],
            summary(l4)$coefficients[2,4],
            summary(l5)$coefficients[2,4],
            summary(l6)$coefficients[2,4],
            summary(l7)$coefficients[2,4])
houmeanp <- mean(ll)


##################### ROW related to income
income <- read.csv("medianincomebystate.csv")
# merge income and ROW
statestuff <- merge(income,bbindex,by="State")
l1 <- (lm(log(X1996.income)~log(Total),statestuff))
l5 <- (lm(log(X2000.income)~log(Total),statestuff))
l9 <- (lm(log(X2004.income)~log(Total),statestuff))
l13 <- (lm(log(X2008.income)~log(Total),statestuff))



ll <- cbind(summary(l1)$coefficients[2,1:4],
            summary(l5)$coefficients[2,1:4],
            summary(l9)$coefficients[2,1:4],
            summary(l13)$coefficients[2,1:4])

mean(ll[1,])  
mean(ll[4,])             


##################### ROW related to Education?
education <- read.csv("education.csv")
educationbbindex <- merge(education,bbindex, by="State")
summary(lm(log(Total)~(bachelors.1990),educationbbindex))
summary(lm(log(Total)~(bachelors.2000),educationbbindex))

#### State level ROW related 2000 Affpol
load("naes200aff.RData")
naes2000aff$state <-   state.name[match(naes2000aff$state,state.abb)]
naes2000aff <- merge(naes2000aff,bbindex, by="state")
summary(lm(naes2000aff$inout~log(naes2000aff$Total)))
