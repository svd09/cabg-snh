#- this code is for the mirror histogram
#- get the data from dfum3 the

d = dfum3 %>% select(snh, ps)

d = d %>% mutate(group = ifelse(snh == 1, "SNH+","SNH-"))

dim(d)

dsnh = d %>% filter(group == "SNH+")

x1 = dsnh$ps

dno = d %>% filter(group == "SNH-")

x2 = dno$ps


# create plotting region

par(mfrow=c(2,1))

#Make the plot / plot saved directly from the graphics device
par(mar=c(0,5,3,3))
hist(x1 , main="" , xlim=c(0,0.8), ylab=" ",
 xlab="", ylim=c(0,10000) , xaxt="n", las=1 , col="tomato3", 
 breaks=100)
par(mar=c(5,5,0,3))
hist(x2 , main="" , xlim=c(0,0.8), ylab=" ",
 xlab="Propensity Score derived from the Logistic Regression Model", 
 ylim=c(40000,0) , las=1 , col="lightblue"  , breaks=100)