## setup ------------

library(readxl)
library(fastR2)
library(weights)
#weighted survey data
library(survey)

# 0. read data
# read full data
setwd("C:/Users/Finley/Desktop/research/healthcare/")
ds <- read.csv("task 2 percerption and knowledge/data/covid19_weight_2020.csv")
#ds <- read.csv("task 2 percerption and knowledge/depression/covid19_new_weight.csv")

# exclude those looking for answers or not
# ds <- ds[which(ds$Q14_1_T==0),]

#----------------------------------- depression_LVN--------------------------
#-------------------------------------------------------------------------------#
# Table 1. sample statistics (weighted / not weighted)

# 1.0 others
# gender proportion 1-male 2-female
round(wpct(ds$Q20,ds$Weight),3)
round(wpct(ds$Q20),3)
# age proportion 
round(wpct(ds$AGE, ds$Weight),3)
round(wpct(ds$AGE),3)
# education
round(wpct(ds$Q22,ds$Weight),3)
round(wpct(ds$Q22),3)
# ethnicity
round(wpct(ds$Q24,ds$Weight),3)
round(wpct(ds$Q24),3)
# province
round(wpct(ds$Q21a,ds$Weight),3)
round(wpct(ds$Q21a))
# 1-urban 2-rural
round(wpct(ds$Q21D,ds$Weight),3)
round(wpct(ds$Q21D),3)
# vocation
round(wpct(ds$Q23,ds$Weight),3)
round(wpct(ds$Q23),3)
# income
round(wpct(ds$Q25,ds$Weight),3)
round(wpct(ds$Q25),3)


# 1.1 self diagnose

round(wpct(ds$Q16,ds$Weight),4)
round(wpct(ds$Q16),4)

# 1.2 other diagnose "Q16","Q18_1","Q18_2","Q18_3","Q18_4","Q18_5"

round(wpct(ds$Q16,ds$Weight),4)
round(wpct(ds$Q16),4)
round(wpct(ds$Q18_1,ds$Weight),4) # family
round(wpct(ds$Q18_1),4)
round(wpct(ds$Q18_2,ds$Weight),4) # friend
round(wpct(ds$Q18_2),4)
round(wpct(ds$Q18_3,ds$Weight),4) # neighbor
round(wpct(ds$Q18_3),4)
round(wpct(ds$Q18_4,ds$Weight),4) # coworker
round(wpct(ds$Q18_4),4)
round(wpct(ds$Q18_5,ds$Weight),4) # other
round(wpct(ds$Q18_5),4)

# 1.3 depression

X9<-ds$Q13_1+ds$Q13_2+ds$Q13_3+ds$Q13_4+ds$Q13_5+ds$Q13_6+ds$Q13_7+ds$Q13_8+ds$Q13_9-9
depression <- floor(X9/5)+1
round(wpct(depression,ds$Weight),4)
depression <- ifelse(depression>1,1,0) #depression if PHQ-9>=10
round(wpct(depression,ds$Weight),4)
round(wpct(depression),4)

# 1.4 overall knowledge (label: questionnaire, all results are shown in weighted result, 
#                         if not weighted results are required, we can just delete ds$Weight)

#Question 1: Perceived risk of death among vulnerable groups --------------------
wtd.mean(x=ds$Q1_1_T,w=ds$Weight)
wtd.quantile(x=ds$Q1_1_T,weights=ds$Weight, probs=c(0.25,0.75))
wtd.mean(x=ds$Q1_1_T)
wtd.quantile(x=ds$Q1_1_T, probs=c(0.25,0.75))

#Question 2: Perceived risk of death among people with other diseases -----------
wpct(ds$Q2_3,ds$Weight)[2]
wilson.ci(wpct(ds$Q2_3,ds$Weight)[2]*length(na.omit(ds$Q2_3)),length(na.omit(ds$Q2_3)))
wpct(ds$Q2_3)[2]
wilson.ci(wpct(ds$Q2_3)[2]*length(na.omit(ds$Q2_3)),length(na.omit(ds$Q2_3)))

#Question 3: The elderly are a high-risk group of transmission ------------------
wpct(ds$Q3,ds$Weight)[1]
wilson.ci(wpct(ds$Q3,ds$Weight)[1]*length(na.omit(ds$Q3)),length(na.omit(ds$Q3)))
wpct(ds$Q3)[1]
wilson.ci(wpct(ds$Q3)[1]*length(na.omit(ds$Q3)),length(na.omit(ds$Q3)))

#Question 6: Vaccine is available that protects against COVID-19 transmission ----
wpct(ds$Q6,ds$Weight)[2]
wilson.ci(wpct(ds$Q6,ds$Weight)[2]*length(na.omit(ds$Q6)),length(na.omit(ds$Q6)))
wpct(ds$Q6)[2]
wilson.ci(wpct(ds$Q6)[2]*length(na.omit(ds$Q6)),length(na.omit(ds$Q6)))

#Question 8: Wearing mask is highly effective in protecting against COVID-19 transmission ------
wpct(ds$Q8,ds$Weight)[1]
wilson.ci(wpct(ds$Q8,ds$Weight)[1]*length(na.omit(ds$Q8)),length(na.omit(ds$Q8)))
wpct(ds$Q8)[1]
wilson.ci(wpct(ds$Q8)[1]*length(na.omit(ds$Q8)),length(na.omit(ds$Q8)))

#Question 7: Actions which help prevent COVID-19 transmission -------------------
d1 <- subset(ds, Q7_1==1 & Q7_2==1 & Q7_4==1 & Q7_6==1 & Q7_10==1 & Q7_3==2 & 
               Q7_5==2 & Q7_7==2 & Q7_8==2 & Q7_9==2 & Q7_11==2)
p<-sum(d1$Weight)/sum(ds$Weight)
p # weighted
wilson.ci(p*length(na.omit(ds$Q7_1)),length(na.omit(ds$Q7_1))) # weighted
p<-length(d1$Weight)/length(ds$Weight)
p # not weighted
wilson.ci(p*length(na.omit(ds$Q7_1)),length(na.omit(ds$Q7_1))) # not weighted

#Question 9: Main way of COVID-19 transmission_saliva ---------------------------
wpct(ds$Q9,ds$Weight)[6]
wilson.ci(wpct(ds$Q9,ds$Weight)[6]*length(na.omit(ds$Q9)),length(na.omit(ds$Q9)))
wpct(ds$Q9)[6]
wilson.ci(wpct(ds$Q9)[6]*length(na.omit(ds$Q9)),length(na.omit(ds$Q9)))

#Question 11: Symptoms of COVID-19 ----------------------------------------------
d1 <- subset(ds, Q11_2==1 & Q11_3==1 & Q11_6==1 & Q11_1==2 & Q11_4==2 & Q11_5==2 & Q11_7==2)
p<-sum(d1$Weight)/sum(ds$Weight)
p # weighted
wilson.ci(p*length(na.omit(ds$Q11_1)),length(na.omit(ds$Q11_1))) # weighted
p<-length(d1$Weight)/length(ds$Weight)
p # not weighted
wilson.ci(p*length(na.omit(ds$Q11_1)),length(na.omit(ds$Q11_1))) # not weighted

#Question 12: Recommended health care-seeking behavior --------------------------
d1 <- subset(ds, Q12==2 | Q12==4)
p<-sum(d1$Weight)/sum(ds$Weight)
p # weighted
wilson.ci(p*length(na.omit(ds$Q11_1)),length(na.omit(ds$Q12))) # weighted
p<-length(d1$Weight)/length(ds$Weight)
p # not weighted
wilson.ci(p*length(na.omit(ds$Q11_1)),length(na.omit(ds$Q12))) # not weighted

# Table 2. regression -----------------------------------------------------------

# 2.1. prepare the independent variables (all the covariates in Table 1)

# 2.1.1 socio-demographics

dsx<-subset(ds, select = c("Q16","Q18_1","Q18_2","Q18_3","Q18_4","Q18_5","AGE","Q20","Q21a","Q21D","Q22","Q23","Q25"))
colnames(dsx) <- c("self","family","friends","neighbor","coworker","others","age","gender","province","urban","education","vocation","income") 

# 2.1.2 knowledge items

dsx$score1 <- ds$Q1_1_T # which cannot be factorized
dsx$score2 <- ifelse((ds$Q2_3==1) & (ds$Q2_2==0) & (ds$Q2_1==0), 1, 0)
dsx$score3 <- ifelse(ds$Q3==1,1,0)
dsx$score6 <- ifelse(ds$Q6==2,1,0)
dsx$score8 <- ifelse(ds$Q8 == 1,1,0)
dsx$score7 <- ifelse((ds$Q7_1==1) & (ds$Q7_2==1) & (ds$Q7_4==1) & (ds$Q7_6==1) & (ds$Q7_10==1) & 
                       (ds$Q7_3==2) & (ds$Q7_5==2) & (ds$Q7_7==2) & (ds$Q7_8==2) & (ds$Q7_9==2) & (ds$Q7_11==2), 1, 0)
dsx$score9 <- ifelse(ds$Q9==6,1,0)
dsx$score11 <- ifelse((ds$Q11_2==1) & (ds$Q11_3==1) & (ds$Q11_6==1) & (ds$Q11_1==2) & 
                        (ds$Q11_4==2) & (ds$Q11_5==2) & (ds$Q11_7==2), 1, 0)
dsx$score12 <- ifelse((ds$Q12=="2") | (ds$Q12=="4"),1,0)

# 2.1.3 factorize attributes

dsx$province <- as.factor(dsx$province)
dsx$vocation <- as.factor(dsx$vocation)
dsx$age <- as.factor(dsx$age)
dsx$self <- ifelse(dsx$self==1,1,0)
dsx$self <- as.factor(dsx$self)
dsx$family <- as.factor(dsx$family)
dsx$neighbor <- as.factor(dsx$neighbor)
dsx$coworker <- as.factor(dsx$coworker)
dsx$friends <- as.factor(dsx$friends)
dsx$gender <- as.factor(dsx$gender)#1: male; 2: female
dsx$education <- as.factor(dsx$education)
dsx$others <- as.factor(dsx$others)
dsx$urban <- ifelse(dsx$urban==2,0,1)
dsx$urban <- as.factor(dsx$urban) #1: urban; 2: rural
dsx$income <- as.factor(dsx$income)

dsx$score2 <- as.factor(dsx$score2)
dsx$score3 <- as.factor(dsx$score3)
dsx$score6 <- as.factor(dsx$score6)
dsx$score8 <- as.factor(dsx$score8)
dsx$score7 <- as.factor(dsx$score7)
dsx$score9 <- as.factor(dsx$score9)
dsx$score11 <- as.factor(dsx$score11)
dsx$score12 <- as.factor(dsx$score12)

case_data <- read_excel("C:/Users/yufen/Desktop/research/healthcare/task 2 percerption and knowledge/depression/data/case_num.xlsx")
dsx <- merge(dsx, case_data, by.x = "province", by.y = "province_id")

dsx$weight <- ds$Weight
dsx$fpc <- dsx$weight*1403850000
dsx$id <- ds$答卷编号
  
dsx$log_prevlance <- log(dsx$prevalence)

summary(dsx)

# 2.1.4 strata: a label to identify the strata of the participant (gender, urban rural residence, province)
#dsx$strata <- as.character(as.hexmode(as.numeric(dsx$gender))*16^3+as.hexmode(as.numeric(dsx$urban))*16^2+as.hexmode(as.numeric(dsx$province)))
dsx$strata <- ds$strata

# 2.2 prepare the dependent variables: depression

X9<-ds$Q13_1+ds$Q13_2+ds$Q13_3+ds$Q13_4+ds$Q13_5+ds$Q13_6+ds$Q13_7+ds$Q13_8+ds$Q13_9-9
depression <- floor(X9/5)+1
dsx$depression <- ifelse(depression>1,1,0)


# dsy <- data.frame(depression)
# dsy$id <- ds$????????
# 
# summary(dsy)

# 2.3 build survey design

regress_data <- dsx
#regress_data <- merge(dsx,dsy,by.x = "id", by.y = "id")
#regress_data[regress_data$strata=="2217","weight"]=0.0001931
regress_data$fpc = regress_data$weight * 1403850000
mysvy<-svydesign(ids = ~1,strata = ~strata, data = regress_data, fpc = ~fpc, weights = ~weight)


# 2.4 full model: covariate-adjusted regression - depression multiple regression -----
#
model<-svyglm(data = regress_data, as.factor(depression) ~ log(prevalence) + gender + age + education + urban + 
                vocation + income + self + family + neighbor + coworker + friends + 
                score1 + score2 + score3 + score6 + score8 + score7 + score9 + 
                score11 + score12 + province, family = binomial(link = "logit"), design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value


# 2.5&2.6 reduce model: covariate-unadjusted regression - depression univariate regression & prevalence e.g. province --------------------------------------------------
library(dplyr)
library(tidyr)

#char_var_label <- list("gender","age","education","urban","vocation","income",
#                    "score1","score2","score3","score6","score8","score7",
#                    "score9","score11","score12")

### gender
dsx %>%
  dplyr::select(gender, depression) %>%
  group_by(gender) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ gender + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value
### age
dsx %>%
  dplyr::select(age, depression) %>%
  group_by(age) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ age + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value
### education 
dsx %>%
  dplyr::select(education, depression) %>%
  group_by(education) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ education + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value
### urban 
dsx %>%
  dplyr::select(urban, depression) %>%
  group_by(urban) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ urban + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### vocation 
dsx %>%
  dplyr::select(vocation, depression) %>%
  group_by(vocation) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ vocation + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### income 
dsx %>%
  dplyr::select(income, depression) %>%
  group_by(income) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ income + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score1 
dsx %>%
  dplyr::select(score1, depression) %>%
  group_by(score1) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score1 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score2 
dsx %>%
  dplyr::select(score2, depression) %>%
  group_by(score2) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score2 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score3 
dsx %>%
  dplyr::select(score3, depression) %>%
  group_by(score3) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score3 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score6 
dsx %>%
  dplyr::select(score6, depression) %>%
  group_by(score6) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score6 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score8 
dsx %>%
  dplyr::select(score8, depression) %>%
  group_by(score8) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score8 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score7 
dsx %>%
  dplyr::select(score7, depression) %>%
  group_by(score7) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score7 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score9 
dsx %>%
  dplyr::select(score9, depression) %>%
  group_by(score9) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score9 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score11 
dsx %>%
  dplyr::select(score11, depression) %>%
  group_by(score11) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score11 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### score12 
dsx %>%
  dplyr::select(score12, depression) %>%
  group_by(score12) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ score12 + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### self
dsx %>%
  dplyr::select(self, depression) %>%
  group_by(self) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ self + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### family
dsx %>%
  dplyr::select(family, depression) %>%
  group_by(family) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ family + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### neighbor
dsx %>%
  dplyr::select(neighbor, depression) %>%
  group_by(neighbor) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ neighbor + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### coworker
dsx %>%
  dplyr::select(coworker, depression) %>%
  group_by(coworker) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ coworker + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### friends
dsx %>%
  dplyr::select(friends, depression) %>%
  group_by(friends) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~ friends + province,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

### province
dsx %>%
  dplyr::select(friends, depression) %>%
  group_by(friends) %>%
  drop_na() %>%
  summarise(n = n(), num = sum(depression), mean = round(sum(depression)/n*100,1), 
            lower = round(wilson.ci(sum(depression),n)[1]*100,1), 
            higher = round(wilson.ci(sum(depression),n)[2]*100,1)) %>%
  knitr::kable()

model<- svyglm(data = regress_data, as.factor(depression) ~  urban,family = binomial(link = "logit"),design=mysvy)
ci<-confint(model)
cbind(round(cbind(OR = exp(coef(model)), exp(ci)),2),coef(summary(model))[,4])# estimation, CI and p-value

# 3. maps -----------------------------------------------------------------------

library(tidyr)
library(readxl)
library(maps)
library(fastR2)
library(mapdata)
library(maptools)
library(ggplot2)
library(mapproj)
library(rgdal)
library(dplyr)
library(weights)
library(MASS)
library(GGally)
library(reshape2)
library(Hmisc)
library(jtools)
library(pscl)
library(aod)

# 3.1. prepare map data
# load original map data
setwd("C:/Users/yufen/Desktop/research/healthcare/")
mydat = rgdal::readOGR("mapdata/mapdata for China/China basic data/China administrative region.shp")

ds <- read.csv("task 2 percerption and knowledge/data/covid19_weight_2020.csv")

ds$fpc <- ds$Weight*1403850000

map_data <- subset(ds,select = c("Q21a","fpc"))

colnames(map_data)<- c("province","fpc")

# convert the `SpatialPolygonsDataFrame` type into `data.frame`
mysh <- fortify(mydat)

# convert
mysh = transform(mysh, id = iconv(id, from = 'GBK'), group = iconv(group, from = 'GBK'))

names(mysh)[2:3] = c("x","y")

# transform the answer province order to province order
order <- c(12,25,29,30,10,5,15,6,19,18,7,17,31,1,3,22,24,21,28,14,32,13,2,33,4,11,20,8,26,34,23,9,27,16)

# depression score
PHQscore<-ds$Q13_1+ds$Q13_2+ds$Q13_3+ds$Q13_4+ds$Q13_5+ds$Q13_6+ds$Q13_7+ds$Q13_8+ds$Q13_9-9

# label those suffering from moderate depression as 1, 0 otherwise.
map_data$depression <- ifelse(PHQscore>4,1,0)

# construct the basic statistics (weighted 95% CI lower bound and higher bound)
stat1<- map_data%>%
  dplyr::select(depression,province,fpc) %>%
  drop_na() %>%
  group_by(province) %>%
  summarise(n=n(),
            meanDepression=weighted.mean(depression,fpc),
            lowDepression=qnorm(0.025,mean = meanDepression,sd=wtd.var(depression,fpc)/sqrt(n)),
            highDepression=qnorm(0.975,mean = meanDepression,sd=wtd.var(depression,fpc)/sqrt(n)))

# 3.2. plot the map using ggplot
setwd("C:/Users/yufen/Desktop/research/healthcare/task 2 percerption and knowledge/depression/figures")
# change the column name
pidat1 <- data.frame(id=unique(sort(mysh$id)))
for (i in 1:nrow(pidat1)) {
  pidat1$percentage[i] <- stat1$meanDepression[order[i]]
}

# determine the fonts
windowsFonts(myFont = windowsFont("Times New Roman"))

# draw the map
map1 = ggplot(pidat1) +
  geom_map(aes(map_id = id, fill = percentage*100), color = "white", map = mysh) +
  scale_fill_gradient2(high = "red",mid = 'white',low = "#9999FF", midpoint = 45,limits=c(35,55)) +
  expand_limits(mysh) + coord_map() + 
  labs(x = "", y = "", title = "")+theme(title=element_text(family="myFont",size=12,hjust=0.2,lineheight=0.2),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         panel.background = element_blank(), axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+labs(fill = "Percentage (%)")

#print(map1)
ggsave(paste0("depression", ".png"),map1)

# # 3.3. case number map
# 
# figure_name <- colnames(case_data)
# 
# for(i in 2:7){
#   pidat1 <- data.frame(id=unique(sort(mysh$id)))
#   for(j in 1:nrow(pidat1)){
#     pidat1$num[j] <- as.numeric(case_data[order[j],i])
#   }
#   # determine the fonts
#   windowsFonts(myFont = windowsFont("Times New Roman"))
#   
#   # draw the map
#   map1 = ggplot(pidat1) +
#     geom_map(aes(map_id = id, fill = num), color = "white", map = mysh) +
#     scale_fill_gradient(high = "red",low = "#9999FF") +
#     expand_limits(mysh) + coord_map() + 
#     labs(x = "", y = "", title = "")+theme(title=element_text(family="myFont",size=12,hjust=0.2,lineheight=0.2),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                            panel.background = element_blank(), axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+labs(fill = "Case Number")
#   
#   print(map1)
#   ggsave(paste0(figure_name[i],".png"),map1)
# }


# 3.4 prevalence map
library(latex2exp)

# change the column name
pidat1 <- data.frame(id=unique(sort(mysh$id)))
for (i in 1:nrow(pidat1)) {
  pidat1$pervalence[i] <- case_data$prevalence[order[i]]
}

# determine the fonts
windowsFonts(myFont = windowsFont("Times New Roman"))

# draw the map
map1 = ggplot(pidat1) +
  geom_map(aes(map_id = id, fill = pervalence), color = "white", map = mysh) +
  scale_fill_gradient(high = "red",low = "#9999FF",trans="log", breaks=c(1,10,100,1000)) +
  expand_limits(mysh) + coord_map() + 
  labs(x = "", y = "", title = "")+
  theme(title=element_text(family="myFont",size=12,hjust=0.2,lineheight=0.2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),legend.key.size = unit(0.15, "inches")) +
  labs(fill = TeX("Prevalence /$10^{6}$")) 


# print(map1)
ggsave(paste0("prevalence", ".png"),map1)



