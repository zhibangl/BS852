project <- read.csv("/Users/zl/Documents/BS852/project2021.csv")
library(survival)
## Data pre-processing procedure
table(project$Alive)
project$duration <- project$Age.last.contact-project$Age.enrollment
head(project)
project$duration <- ifelse(project$duration==0, NA, project$duration)
table(project$duration)
Project1 <- na.omit(project)
table(Project1$Alive)
Project1$Alive <-ifelse(Project1$Alive=="No",1,0)
table(Project1$Alive)
summary(Project1)
## check for normal distribution
hist(Project1$DSST,xlab="DDST scores",ylab="subjects",main="DSST graph")

## Kaplan_meier Curves --Figure 1
Project1$DSST_Level <- ifelse(Project1$DSST>=46,1,0)

survival_model <- survfit(Surv(duration, Alive) ~ DSST_Level, data=Project1)
summary(survival_model)
plot(survival_model,
     mark.time=TRUE,
     xlab="Duration (years)",
     ylab="Survival rate",
     main="Kaplan-Meier Curve",
     col=c("red","blue"))
legend("bottomleft", inset=0.03,
       legend=c("Low DSST","High DSST"),
       col=c("red","blue"), 
       lwd=3, 
       cex=1)
#Log-Rank Test
survdiff(Surv(duration,Alive) ~ DSST_Level, data=Project1)
survdiff(Surv(duration,Alive) ~ DSST_Level, data=Project1)$chisq


## Crude model -- Model 1
crude_model <- coxph(Surv(duration, Alive) ~ DSST, data=Project1)
summary(crude_model)
## Full model -- Model 2
Adjusted_model <- coxph(Surv(duration,Alive) ~ DSST+educ+sex+Age.enrollment+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
summary(Adjusted_model)
## ANOVA test
Adjusted_model_DSST <- coxph(Surv(duration, Alive) ~ Age.enrollment+sex+educ+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_DSST,Adjusted_model)

Adjusted_model_AgeEnroll <- coxph(Surv(duration, Alive) ~ DSST+sex+educ+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_AgeEnroll,Adjusted_model)

Adjusted_model_sex <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_sex,Adjusted_model)
##**Insignificant
Adjusted_model_educ <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+sex+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_educ,Adjusted_model)

Adjusted_model_il6 <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_il6,Adjusted_model)

## Insignificant
Adjusted_model_hscrp <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.il6+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_hscrp,Adjusted_model)
##**Insignificant
Adjusted_model_gripstrength <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.il6+log.new.hscrp+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_gripstrength,Adjusted_model)

Adjusted_model_bmi <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.il6+log.new.hscrp+Z.grip.strength+Z.gait.speed+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_bmi,Adjusted_model)

Adjusted_model_gaitspeed <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.fev1.7 +Z.sysbp, data=Project1)
anova(Adjusted_model_gaitspeed,Adjusted_model)

Adjusted_model_Z.fev1.7 <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.sysbp, data=Project1)
anova(Adjusted_model_Z.fev1.7,Adjusted_model)

##**Insignificant
Adjusted_model_Z.sysbp <- coxph(Surv(duration, Alive) ~ Age.enrollment+DSST+educ+sex+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7, data=Project1)
anova(Adjusted_model_Z.sysbp,Adjusted_model)

## data selection
Fullmodel <- ~DSST+educ+sex+Age.enrollment+log.il6+log.new.hscrp+Z.grip.strength+Z.bmi+Z.gait.speed+Z.fev1.7 +Z.sysbp
M0 <- coxph(Surv(duration,Alive) ~ 1, data=Project1)

## BIC forward test
BICforward <- step(M0, scope=Fullmodel, direction="forward",k=log(nrow(Project1)))

Adjusted_model_BIC <- coxph(Surv(duration,Alive) ~DSST+Age.enrollment+sex+log.il6+Z.gait.speed+Z.bmi+Z.fev1.7, data=Project1)
summary(Adjusted_model_BIC)
## Bonferroni Correction
0.05/7
## Schoenfeld Residual
Schoenfeld_Residual <-cox.zph(Adjusted_model_BIC)
Schoenfeld_Residual
## Time-varying
Time_varying <- coxph(Surv(duration, Alive) ~ DSST+Age.enrollment+sex+log.il6+Z.gait.speed+Z.bmi+Z.fev1.7 +tt(Z.bmi), data=Project1, 
                      tt=function(x,t,...)x*t)
summary(Time_varying)
## stratified BMI
summary(Project1$Z.bmi)
Stratified_BMI <- Project1$Z.bmi

Stratified_BMI[which(Project1$Z.bmi<(-0.70131))] <- 1
Stratified_BMI[which(Project1$Z.bmi>=(-0.70131) & Project1$Z.bmi< (-0.11278))] <- 2
Stratified_BMI[which(Project1$Z.bmi>=(-0.11278) & Project1$Z.bmi< 0.66955)] <- 3
Stratified_BMI[which(Project1$Z.bmi>=0.66955)] <- 4

## Final-adjusted model--Model 3
Final_model <- coxph(Surv(duration, Alive)~DSST+Age.enrollment+sex+log.il6+Z.gait.speed+Z.bmi+Z.fev1.7+strata(Stratified_BMI), data=Project1)
summary(Final_model)
Schoenfeld_Residual2 <-cox.zph(Final_model)
Schoenfeld_Residual2


#Characteristics table --Table 1
table(Project1$Alive)
Dead <- Project1[which(Project1$Alive==1),]
Alive <- Project1[which(Project1$Alive==0),]

## Characteristics for sex
table(Dead$sex)
347/nrow(Dead)
436/nrow(Dead)
table(Alive$sex)
1664/nrow(Alive)
1271/nrow(Alive)

t.test(Dead$sex,Alive$sex)

## Characteristics for DSST 
mean(Dead$DSST)
sd(Dead$DSST)
mean(Alive$DSST)
sd(Alive$DSST)

t.test(Dead$DSST,Alive$DSST)

## Characteristics for educ
mean(Dead$educ)
sd(Dead$educ)
mean(Alive$educ)
sd(Alive$educ)

t.test(Dead$educ,Alive$educ)

## Characteristics for Age at enrollment
mean(Dead$Age.enrollment)
sd(Dead$Age.enrollment)
mean(Alive$Age.enrollment)
sd(Alive$Age.enrollment)

t.test(Dead$Age.enrollment,Alive$Age.enrollment)

## Characteristics for Z-score of right-hand grip strength
mean(Dead$Z.grip.strength)
sd(Dead$Z.grip.strength)
mean(Alive$Z.grip.strength)
sd(Alive$Z.grip.strength)

t.test(Dead$Z.grip.strength,Alive$Z.grip.strength )

## Characteristics for Z-score of BMI
mean(Dead$Z.bmi)
sd(Dead$Z.bmi)
mean(Alive$Z.bmi)
sd(Alive$Z.bmi)

t.test(Dead$Z.bmi,Alive$Z.bmi)

## Characteristics for Z-score of lung function
mean(Dead$Z.fev1.7)
sd(Dead$Z.fev1.7)
mean(Alive$Z.fev1.7)
sd(Alive$Z.fev1.7)

t.test(Dead$Z.fev1.7,Alive$Z.fev1.7)

## Characteristics for Z-score of speed to walk a 4 meter distance
mean(Dead$Z.gait.speed)
sd(Dead$Z.gait.speed)
mean(Alive$Z.gait.speed)
sd(Alive$Z.gait.speed)

t.test(Dead$Z.gait.speed,Alive$Z.gait.speed)

## Characteristics for Log-transformed concentration of proteins IL6
mean(Dead$log.il6)
sd(Dead$log.il6)
mean(Alive$log.il6)
sd(Alive$log.il6)

t.test(Dead$log.il6,Alive$log.il6)

## Characteristics for Log-transformed concentration of hsCRP
mean(Dead$log.new.hscrp)
sd(Dead$log.new.hscrp)
mean(Alive$log.new.hscrp)
sd(Alive$log.new.hscrp)

t.test(Dead$log.new.hscrp,Alive$log.new.hscrp)


## Characteristics for Z-score of systolic blood pressure
mean(Dead$Z.sysbp)
sd(Dead$Z.sysbp)
mean(Alive$Z.sysbp)
sd(Alive$Z.sysbp)

t.test(Dead$Z.sysbp,Alive$Z.sysbp)
knitr::stitch_rhtml('/Users/zl/Documents/BS852/project1.R')
browseURL('project1.html')
knitr::stitch('/Users/zl/Documents/BS852/project1.R')

## backward and forward  might lead to different result.
## PH assumption must be clear. 
## 1. data processing, demographic analysis. such as min max, mean,  SD. IQR
## 2. description, relationship covariates, outcome correlation , filter covariate that are unnecessary
## 3. forward or backward
## 4. test model ph assumption. conclusion. 
## RR(0.98)


exp(0.088587)
