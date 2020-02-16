# urban status and ftr in safety net hospitals
# cabg and only urban status
# propensity match to get two groups - snh and nonsnh
# logistic regression for failure to rescue

# script to get variables from the entire datatable df_c
# get libraries

library(Epi); library(lme4); library(survival)
library(twang); library(tableone); library(survey) 
library(Matching); library(Hmisc); library(rms) 
library(MatchIt); library(CBPS); library(ebal)
library(rbounds); library(cobalt);library(tidyr) 
library(forcats); library(pander); library(tidyverse);
library(magrittr);library(DescTools);library(gmodels)


# set wd

setwd("G:\\snh_cad\\data\\")

# get table

library(survey)
options(survey.lonely.psu="adjust")

df = read_csv("datatable_all.csv")

# see all variables and arrange according to alphabetical order

keep = c("age", "aweekend", "cabg", "carotid", "chf", "died", "discwt.x", 
         "dispuniform", "dx1", "dx10", "dx2", "dx3", "dx4", "dx5", "dx6", 
         "dx7", "dx8", "dx9", "dyslip", "female", "group2", "hosp_nis", 
         "key_nis", "los", "nis_stratum.x", "pay1", "pci", "pct2or5", 
         "pr1", "pr10", "pr2", "pr3", "pr4", "pr5", "pr6", "pr7", "pr8", 
         "pr9", "priicd", "priorcabg", "priormi", "priorpci", "pristroke", 
         "race", "smoker", "snh", "totchg", "want", "year.x", "zipinc_qrtl", 
         "discwt.y", "hosp_bedsize", "hosp_locteach", "hosp_region", "year.y", 
         "cm_aids", "cm_alcohol", "cm_anemdef", "cm_arth", "cm_bldloss", 
         "cm_chf", "cm_chrnlung", "cm_coag", "cm_depress", "cm_dm", "cm_dmcx", 
         "cm_drug", "cm_htn_c", "cm_hypothy", "cm_liver", "cm_lymph", 
         "cm_lytes", "cm_mets", "cm_neuro", "cm_obese", "cm_para", "cm_perivasc", 
         "cm_psych", "cm_pulmcirc", "cm_renlfail", "cm_tumor", "cm_ulcer", 
         "cm_valve", "cm_wghtloss")

df2 = df[,(keep)]

dim(df2)

# get more variables 
# atrial fibrillation

af <- as.character(c("42731"))

df2$af <- df2 %$% ifelse((dx1 == '42731'| dx2 == '42731'| dx3 == '42731' |
                            dx4 == '42731'| dx5 == '42731'| dx6 == '42731'| 
                            dx7 == '42731'| dx8 == '42731'|dx9 == '42731'| 
                            dx10 == '42731'),1, 0)

df2$af[is.na(df2$af)]<- 0

CrossTable(df2$af)

# esrd on dialysis

dial <- as.character(c("5856","V4511"))

df2$esrd <- df2 %$% ifelse((dx1 %in% dial| dx2 %in% dial| dx3 %in% dial 
                            | dx4 %in% dial | dx5 %in% dial| dx6 %in% dial| 
                              dx7 %in% dial| dx8 %in% dial|dx9 %in% dial| dx10 %in% dial), 1,0)

CrossTable(df2$esrd)

# non dialysis dependent renal dysfunction

df2$crf <- df2$cm_renlfail

df2$rd[df2$crf == 1 & df2$esrd == 0]<- 1
df2$rd[df2$crf == 0]<- 0


CrossTable(df2$rd)


# diabetes mellitus

df2 = df2 %>%  
  mutate(dm = ifelse((cm_dm == 1 |cm_dmcx == 1), 1, 0))

CrossTable(df2$dm)

# hospital location 
# 1 rural, 2 urban nonteaching 3 urban teaching 

CrossTable(df2$hosp_locteach)

# discharge planning 

table(df2$dispuniform)



df2$dismissal[df2$dispuniform == "1"]<- "home"
df2$dismissal[df2$dispuniform == "2"]<- "shorttermhosp"
df2$dismissal[df2$dispuniform == "5"]<- "SNF"
df2$dismissal[df2$dispuniform == "6"]<- "HHC"
df2$dismissal[df2$dispuniform == "20"]<- "died"

table(df2$dismissal, useNA = "ifany")

# acute myocardial infarction


a <- as.character(c('41001':'41003'))
b <- as.character(c('41011':'41013'))
c <- as.character(c('41021':'41023'))
d <- as.character(c('41031':'41033'))
e <- as.character(c('41041':'41043'))
f <- as.character(c('41041':'41043'))
g <- as.character(c('41041':'41043'))
h <- as.character(c('41051':'41053'))
i <- as.character(c('41061':'41063'))
j <- as.character(c('41071':'41073'))
k <- as.character(c('41081':'41083'))
l <- as.character(c('41091':'41093'))

amic <- c(a,b,c,d,e,f,g,h,i,j,k,l)
amic

df2$acuteinf <- df2 %$% ifelse((dx1 %in% amic | dx2 %in% amic| dx3 %in% amic|
                                  dx4 %in% amic | dx6 %in% amic |dx7 %in% amic |dx8 %in% amic |
                                  dx9 %in% amic  |dx10 %in% amic),1,0)

df2$acuteinf[is.na(df2$acuteinf)]<- 0

table(df2$acuteinf,  useNA = "ifany")


# concomitant valve patients

valve <- as.character(c('3511','3512','3513',
                        '3514','3521','3522','3523','3524','3526','3525','3527',
                        '3528'))

a <- valve

df2$valve <- with(df2, ifelse((pr1 %in% a | pr2 %in% a | pr3 %in% a | 
                                 pr4 %in% a | pr5 %in% a | pr6 %in% a | pr7 %in% a | pr8 %in% a  
                               | pr9 %in% a |pr10 %in% a ), 1, 0))

table(df2$valve, useNA = "ifany")


# Post-procedure complications:



# acute stroke 

acustroke <- as.character(c('43401','43411','43491','99702'))

df2$acutestroke <- 
  df2 %$% ifelse((dx1 %in% acustroke | dx2 %in% acustroke| dx3 %in% acustroke|
                    dx4 %in% acustroke | dx6 %in% acustroke |dx7 %in% acustroke |dx8 %in% acustroke |dx9 %in% acustroke  |
                    dx10 %in% acustroke),1,0)

df2$acutestroke[is.na(df2$acutestroke)] <- 0
table(df2$acutestroke,  useNA = "ifany")


# cardiac arrest

df2$cpr <- df2 %$% ifelse((dx1 == "4275" | dx2 == "4275"| dx3 == "4275"|
                             dx4 ==" 4275" | dx5 == "4275"|dx6 == "4275"|dx8 == "4275"|dx9 == "4275"| dx10 == "4275"),1,0)

df2$cpr[is.na(df2$cpr)]<- 0
table(df2$cpr,  useNA = "ifany")



# kidney failure 


df2$aki = with(df2, ifelse((dx1 == "5849"|dx2 == "5849"|dx3 == "5849"
| dx4 == "5849"| dx5 == "5849"| dx6 == "5849"|dx7 == "5849"|
  dx8 == "5849"|dx9 == "5849"|dx10 == "5849"), 1, 0))

df2$aki[is.na(df2$aki)]<- 0

table(df2$aki,  useNA = "ifany")


# MACCE

df2$mace = with(df2, ifelse((died == 1|aki == 1|
                               cpr == 1|acutestroke == 1),
                            1, 0))

# race
#1 white, 2 black, 3 hispanic, 4 others, NA 

table(df2$race, useNA = 'ifany')

df2$race2 = df2$race

df2$race2[df2$race2 == 5]<- 4
df2$race2[df2$race2 == 6]<- 4

table(df2$race2, useNA = "ifany")

# cardiogenic shock 

df2$cs <- df2 %$% ifelse((dx1 == "78551" | dx2 == "78551"| 
                            dx3 == "78551"| dx5 == "78551"|dx4 =="78551" | 
                            dx6 == "78551"|dx7 == "78551"|dx8 == "78551"|dx9 == "78551" |
                            dx10 == "78551"),1,0)



df2$cs[is.na(df2$cs)]<- 0

table(df2$cs,  useNA = "ifany")


df2 = df2 %>% rename("discwt" = "discwt.x") %>% 
  rename("nis_stratum" = "nis_stratum.x") %>% 
  rename("year" = "year.x")

df2 = df2 %>% select(-year.y, -discwt.y) 

df2 = df2 %>% 
  mutate(location = ifelse((hosp_locteach %in% c(2,3)),1,0))

df2 = df2 %>% 
  mutate(age_gp = cut(age, breaks = c(-Inf,59,79,Inf), 
                      labels = c(1,2,3)))

glimpse(df2)

# create propensity score model to determine propensity score
# use all patients who have urban status


# dfu = df2[df2$location == 1, ] # keep only urban patients 

dfu = df2

var = c("age", "aweekend",  "carotid", "chf", 
        "dispuniform", "discwt",
        "dyslip", "female",  "hosp_nis", 
        "key_nis", "los", "nis_stratum",  "pct2or5", 
        "priicd", "priorcabg", "priormi", "priorpci", "pristroke", 
        "smoker", "snh",  "year", "zipinc_qrtl","location",
        "hosp_region", "cm_chrnlung",  "cm_dm","mace","aki",
        "cm_dmcx", "cm_htn_c", "cm_obese", "cm_perivasc","cm_renlfail","acutestroke",
        "af", "esrd", "crf",  "dm",  "cpr",  "race2", "cs","died", "valve")

dfu_cabg = dfu %>% filter(valve == 0)

dfu$cabgvalve = with(dfu, ifelse((cabg == 1 & valve == 1), 1, 0))


dfum = dfu_cabg[,c(var)] # create dfum table for create propensity score and then 
# to do the analysis

dfum2 = dfum[!is.na(dfum$race2),]


dfum2 = dfum2[! is.na(dfum2$snh), ]



propmiss <- function(dataframe) 
  lapply(dataframe,function(x) 
    data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

dfum3 = dfum2 %>% mutate(female2 = replace_na(female, 1))

dfum3 = dfum3 %>% mutate(zip2 = replace_na(zipinc_qrtl, 3))


# model without propensity score adjustment

des = svydesign(data = dfum3, ids = ~hosp_nis, 
                weights = ~discwt, strata = ~nis_stratum,
                nest = T)

# estimated mortality rates for patients according to snh status
# use this to create bar plot of results.
# use colorblind pallette for figures.



# died

died = svyglm(died ~ factor(snh), des)

# mace

mace = svyglm(mace ~ factor(snh), des)

# create propensity score for snh vs nonsnh 

psmodel = glm(snh ~ age + aweekend + carotid + 
                chf + discwt + female2 + dyslip + 
                priicd + priorcabg + priorpci + location +
                priormi + pristroke + smoker + year + zip2 +
                hosp_region + cm_chrnlung + dm + cm_htn_c + cm_obese +
                af + esrd + crf + race2, data = dfum3, 
              family = "binomial"(link = "logit"))

arm::display(psmodel,3)

dfum3$ps <- psmodel$fitted
dfum3$linps <- psmodel$linear.predictors

gr = dfum3 %>% select(ps, linps, snh)

gr2 = gr %>% mutate(snh2 = fct_recode(factor(snh),
                                       "SNH" = "1", 
                                       "Non-SNH" = "0"))

gr2 = gr2 %>% rename("Cohort" = "snh2")


a = ggplot(gr2, aes(x = ps,  fill=Cohort)) +
  geom_density(alpha=0.3) +
  labs(x = "Linear Propensity Score", colour="Type", shape="Type",
       title="Linear Propensity Score by SNH", y = "Density") 

a2 = a + theme_Publication() 

ggsave(plot = a2,filename =  "E:\\snh_cad\\prop_curve.tiff", device = "tiff",
       dpi = 1200)

### create quintiles for boxplot of propensity scores according to quintile

gr2$stratum <- cut2(gr2$ps, g=5) # cuts into 5 equal groups
gr2$quintile <- factor(gr2$stratum, labels=1:5)

library(ggstance) 

b = ggplot(data = gr2, aes(y = quintile, x = ps, fill = Cohort)) + 
  geom_boxploth(alpha = 0.5)

b2 = b + theme_Publication() + labs(title = "Quintile distribution of Propensity Score",
  x = "Propensity Score", y = "Quintiles")

ggsave(plot = b2, filename ="E:\\snh_cad\\quintiles.tiff",
       device = "tiff", dpi = 1200)


# create quintiles of table dfum3

## cut2 function comes from Hmisc library

dfum3$stratum <- cut2(dfum3$ps, g=5) # cuts into 5 equal groups
dfum3$quintile <- factor(dfum3$stratum, labels=1:5)
table(dfum3$stratum, dfum3$quintile) ## quick sanity check

# boxplot of propensity scores for each quintile 

boxplot(dfum3$ps ~ dfum3$quintile, horizontal=TRUE,
        ylab="Propensity Quintile", xlab="Raw Propensity Score")


ggplot(data = dfum3, aes(x = quintile, y = ps, fill = factor(snh))) +
  geom_boxplot()

# create 5 tables for each quintile

quin1 <- subset(dfum3, quintile==1)
quin2 <- subset(dfum3, quintile==2)
quin3 <- subset(dfum3, quintile==3)
quin4 <- subset(dfum3, quintile==4)
quin5 <- subset(dfum3, quintile==5)


addmargins(table(dfum3$quintile, factor(dfum3$snh))) # look at the two
# cohorts for each quintile

library(survey)
options(survey.lonely.psu = "adjust")


# 1

dq1 = svydesign(data = quin1, ids = ~hosp_nis, 
                weights = ~discwt, strata = ~nis_stratum,
                nest = T)

died1 = svyglm(died ~ factor(snh), design = dq1)

mace1 = svyglm(mace ~ factor(snh), design = dq1)


# 2


dq2 = svydesign(data = quin2, ids = ~hosp_nis, 
                weights = ~discwt, strata = ~nis_stratum,
                nest = T)

died2 = svyglm(died ~ factor(snh), design = dq2)

mace2 = svyglm(mace ~ factor(snh), design = dq2)


# 3

dq3 = svydesign(data = quin3, ids = ~hosp_nis, 
                weights = ~discwt, strata = ~nis_stratum,
                nest = T)

died3 = svyglm(died ~ factor(snh), design = dq3)

mace3 = svyglm(mace ~ factor(snh), design = dq3)


# 4

dq4 = svydesign(data = quin4, ids = ~hosp_nis, 
                weights = ~discwt, strata = ~nis_stratum,
                nest = T)

died4 = svyglm(died ~ factor(snh), design = dq4)

mace4 = svyglm(mace ~ factor(snh), design = dq4)

# 5


dq5 = svydesign(data = quin5, ids = ~hosp_nis,
                weights = ~discwt, strata = ~nis_stratum,
                nest = T)

died5 = svyglm(died ~ factor(snh), design = dq5)

mace5 = svyglm(mace ~ factor(snh), design = dq5)

###

# tidy results to identify p-values
library(broom)

d1 = broom::tidy(died1)

d2 = broom::tidy(died2)

d3 = tidy(died3)

d4 = tidy(died4)

d5 = tidy(died5)


###

# combined result death

### pooled p value

pooled_p = (d1[2,5] + d2[2,5] + d3[2,5] + d4[2,5] + d5[2,5])/5

est.st <- (coef(died1)[2] + coef(died2)[2] + coef(died3)[2] +
             coef(died4)[2] + coef(died5)[2])/5



se.q1 <- summary(died1)$coefficients[2,2]
se.q2 <- summary(died2)$coefficients[2,2]
se.q3 <- summary(died3)$coefficients[2,2]
se.q4 <- summary(died4)$coefficients[2,2]
se.q5 <- summary(died5)$coefficients[2,2]

se.st <- sqrt((se.q1^2 + se.q2^2 + se.q3^2 + se.q4^2 + se.q5^2)*(1/25))


temp.result2 <- c(exp(est.st), exp(est.st - 1.96*se.st), exp(est.st + 1.96*se.st))
names(temp.result2) <- c("Estimate", "Low 95% CI", "High 95% CI")
temp.result2


### combined result mace

m1 = broom::tidy(mace1)

m2 = broom::tidy(mace2)

m3 = tidy(mace3)

m4 = tidy(mace4)

m5 = tidy(mace5)


### pooled p value

pooled_p_mace = (m1[2,5] + m2[2,5] + m3[2,5] + m4[2,5] + m5[2,5])/5

est.st_mace <- (coef(mace1)[2] + coef(mace2)[2] + coef(mace3)[2] +
                  coef(mace4)[2] + coef(mace5)[2])/5



se.q1 <- summary(mace1)$coefficients[2,2]
se.q2 <- summary(mace2)$coefficients[2,2]
se.q3 <- summary(mace3)$coefficients[2,2]
se.q4 <- summary(mace4)$coefficients[2,2]
se.q5 <- summary(mace5)$coefficients[2,2]

se.st <- sqrt((se.q1^2 + se.q2^2 + se.q3^2 + se.q4^2 + se.q5^2)*(1/25))


temp.result_mace <- c(exp(est.st), exp(est.st - 1.96*se.st), exp(est.st + 1.96*se.st))
names(temp.result2) <- c("Estimate", "Low 95% CI", "High 95% CI")

temp.result_mace
