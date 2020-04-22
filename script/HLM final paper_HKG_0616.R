#Part I: Data Preparation

#HLM on PISA data set
#NOTE: "df_sch_stu_and_parent" is the final processed data frame.
setwd("D:/PISA")

library(lme4) #for HLM (LMM/GLMM)
library(dplyr) 
library(uwIntroStats)
#Step1 - Read in data and preprocess--------------
load("PISA_raw_compressed.RData")#Load in the raw data
#load("processed_merged_PISA_data.RData")# Load in the already-processed data

# To find the variables that are common in all the data frames
# intersect(intersect(names(df_stu),names(df_par)),names(df_sch))
df_stu_filtered=
  df_stu%>%
  select(CNT,SUBNATIO,STRATUM,SCHOOLID,StIDStd,NC,OECD,
         SES=ESCS,
         Anxiety_Math=ANXMAT,
         Age=AGE,
         Gender=ST04Q01,
         Efficacy=MATHEFF,
         Persev=PERSEV,
         Openness=OPENPS,
         PV1MATH=PV1MATH,
         PV2MATH=PV2MATH,
         PV3MATH=PV3MATH,
         PV4MATH=PV4MATH,
         PV5MATH=PV5MATH)%>% # Select specific variables from student data frame
  filter(CNT %in% c("TAP"))

df_sch_filtered=
  df_sch%>%
  select(CNT,SUBNATIO,STRATUM,SCHOOLID,NC,OECD,
         Autonomy=SCHAUTON,
         Tch_Participation=TCHPARTI,
         Tch_Short=TCSHORT,
         Edu_Resource=SCMATEDU)%>%  # Select specific variables from school data frame
  filter(CNT %in% c("TAP"))

# Merge Data Set
df_sch_stu=
  df_sch_filtered%>%
  left_join(df_stu_filtered,by = c("CNT","SUBNATIO",
                                     "STRATUM","SCHOOLID","NC","OECD"))

write.csv(x = df_sch_stu_and_parent,file = "PISA_TW.csv")




#############################################################################
#Part II: Data analysis
PISA <- read.csv(file="PISA_HKG.csv", header=TRUE, sep=",")

PISA<-df_sch_stu_and_parent

# checking the missing values
missing<-sapply(PISA,function(x) sum(is.na(x)))

#recode variable
PISA$female <- ifelse(PISA$Gender == 2, 0, 1)  #original code: F:1, M:2
# check whether the recoding is correctly
CrossTable(PISA$female, PISA$Gender)

#descriptive statistics
summary(PISA)

#delete all missing values
complete.cases(PISA)
rm.data <- PISA[complete.cases(PISA), ]

write.csv(x = rm.data,file = "PISA_HKG_rm.csv")

##################################################################################
##################################################################################
options(scipen=999)

colnames(PISA)[19] <- "PV1MATH"
result1 <- regress("mean", PV1MATH ~ SES + Efficacy, data=PISA)
res1 <- result1[["coefficients"]]
result2 <- regress("mean", PV2MATH ~ SES + Efficacy, data=PISA)
res2 <- result2[["coefficients"]]
result3 <- regress("mean", PV3MATH ~ SES + Efficacy, data=PISA)
res3 <- result3[["coefficients"]]
result4 <- regress("mean", PV4MATH ~ SES + Efficacy, data=PISA)
res4 <- result4[["coefficients"]]
result5 <- regress("mean", PV5MATH ~ SES + Efficacy, data=PISA)
res5 <- result5[["coefficients"]]
r <- cbind(res1[,1],res2[,1],res3[,1],res4[,1],res5[,1])
U <- cbind(res1[,3]^2,res2[,3]^2,res3[,3]^2,res4[,3]^2,res5[,3]^2)
r.star <- rowMeans(r)
U.star <- rowMeans(U)
M <- 5
B <- (1/(M-1))*rowSums((r-r.star)^2)
V <- U.star+(1+1/M)*B
f <- (1+1/M)*B/V
d <- 80
v <- ((f^2)/(M-1)+((1-f)^2)/d)^-1

se <- v^0.5
t <- r.star/(v^0.5)
p.value <- 2*pt(-abs(t),v)

#################################################################################
#################################################################################

#descriptive statistics after deleting missing value
summary(rm.data)

dim(rm.data)
str(rm.data)

PISA1<-rm.data

# Figure 0: Distribution of outcome
hist(PISA1$Math_grade, main = "Distribution of outcome", xlab = 'Math grade')

# Figure 1: Plot of math score vs. SES across schools
PISA1$SCHOOLID<-as.factor(PISA1$SCHOOLID)
levels(PISA1$SCHOOLID)

par(mfrow= c(1, 1))
plot(jitter(PISA1$SES), PISA1$Math_grade, main = "Math achievement score versus SES", xlab = 'SES', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ SES, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ SES, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)


# Figure 2: Plot of math score vs. anexity across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Anxiety_Math), PISA1$Math_grade, main = "Math achievement score versus math anxiety", xlab = 'Math Anxiety', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ Anxiety_Math, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Anxiety_Math, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 3: Plot of math anexity vs. ses across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$SES), PISA1$Anxiety_Math, main = "Math anxiety versus SES", xlab = 'SES', ylab = 'Math anxiety', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Anxiety_Math ~ SES, data = PISA1, PISA$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Anxiety_Math ~ SES, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 4: Plot of math score vs. efficacy across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Efficacy), PISA1$Math_grade, main = "Math achievement score versus math efficacy", xlab = 'Math efficacy', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ Efficacy, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Anxiety_Math, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)


# Figure 5: Plot of math score vs. persev across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Persev), PISA1$Math_grade, main = "Math achievement score versus persev", xlab = 'Math persev', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ Persev, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Persev, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 6: Plot of math score vs. openness across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Openness), PISA1$Math_grade, main = "Math achievement score versus openness", xlab = 'Math openness', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ Openness, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Openness, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 7: Plot of math score vs. parent's perceived school quality across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Percepted_Quality), PISA1$Math_grade, main = "Math achievement score versus parent's perceived school quality", xlab = 'Percepted quality', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ Percepted_Quality, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Percepted_Quality, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 8: Plot of math score vs. parent's support across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Par_Support), PISA1$Math_grade, main = "Math achievement score versus parent's support", xlab = 'Parent support', ylab = 'Math score', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade ~ Par_Support, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Par_Support, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)


# level 2 predictor to outcomes
# Figure 9: Plot of math grade vs. education resource across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Edu_Resource), PISA1$Math_grade, main = "Education resource versus math grade", xlab = 'educational resoruce', ylab = 'Math grade', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade~ Edu_Resource, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Edu_Resource, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 10: Plot of math grade vs. teacher autonomy across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Autonomy), PISA1$Math_grade, main = "Teacher autonomy versus math grade", xlab = ' teacher autonomy', ylab = 'Math grade', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade~ Autonomy, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Autonomy, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 11: Plot of math grade vs. teacher participation across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Tch_Participation), PISA1$Math_grade, main = "Teacher participation versus math grade", xlab = ' teacher participation', ylab = 'Math grade', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade~ Tch_Participation, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Tch_Participation, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# Figure 12: Plot of math grade vs. teacher shortage across schools
par(mfrow= c(1, 1))
plot(jitter(PISA1$Tch_Short), PISA1$Math_grade, main = "Teacher shortage versus math grade", xlab = ' teacher shortage', ylab = 'Math grade', col = 'gray')
for(i in levels(PISA1$SCHOOLID)){
  out.lm.i <- lm(Math_grade~ Tch_Short, data = PISA1, PISA1$SCHOOLID == i) 
  abline(out.lm.i$coef[1], out.lm.i$coef[2], col = 1)
}

out.lm <- lm(Math_grade ~ Tch_Short, data = PISA1)
abline(out.lm$coef[1], out.lm$coef[2], lwd = 3, col = 2)

# ::::::::::: functions: RIvarcomp, means, reliability :::::::::

# extract variance components and compute ICC in two-level random intercept models
# help(lmer), click the "mer" class to see "Slots" 

RIvarcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}


#scatter plots

attach(PISA1)

par(mfrow=c(1,1))

#math grade in different schools
plot(SCHOOLID, jitter(Math_grade), xlab = 'School ID', ylab = 'Math score')   
plot(as.numeric(SCHOOLID), jitter(Math_grade), xlab = 'School ID', ylab = 'Math score')   

#SES in different schools
plot(SCHOOLID, jitter(SES), xlab = 'School ID', ylab = 'ses')   
plot(as.numeric(SCHOOLID), jitter(SES), xlab = 'School ID', ylab = 'ses')  

detach(PISA1)

# create level-2 variables from level-1 variables

N.st    <- table(PISA1$SCHOOLID)                       # the number of students in each school
ses.gm   <- tapply(PISA1$SES, PISA1$SCHOOLID, mean) # SES group mean
math.gm <- tapply(PISA1$Math_grade, PISA1$SCHOOLID, mean)     # math group mean
Percepted_Quality.gm <-tapply(PISA1$Percepted_Quality, PISA1$SCHOOLID, mean) 
Anxiety_Math.gm <- tapply(PISA1$Anxiety_Math, PISA1$SCHOOLID, mean)
Efficacy.gm <- tapply(PISA1$Efficacy, PISA1$SCHOOLID, mean)
Persev.gm <- tapply(PISA1$Persev, PISA1$SCHOOLID, mean)
Par_Support.gm <- tapply(PISA1$Par_Support, PISA1$SCHOOLID, mean)

cbind(unique(PISA1$SCHOOLID), N.st,ses.gm,math.gm,Percepted_Quality.gm, Anxiety_Math.gm, Efficacy.gm, Persev.gm, Par_Support.gm)  # M = 10 (# level 2 units) rows


#attach new level-2 variables to data 
N.st
N.st[PISA1$SCHOOLID]
PISA1$N.students     <- N.st[PISA1$SCHOOLID]   
PISA1$ses.groupmean   <- ses.gm[PISA1$SCHOOLID]
PISA1$math.groupmean <- math.gm[PISA1$SCHOOLID]
PISA1$Percepted_Quality.groupmean <-Percepted_Quality.gm[PISA1$SCHOOLID]
PISA1$Anxiety_Math.groupmean <-Anxiety_Math.gm[PISA1$SCHOOLID]
PISA1$Efficacy.groupmean <- Efficacy.gm[PISA1$SCHOOLID]
PISA1$Par_Support.groupmean <- Par_Support.gm[PISA1$SCHOOLID]


# ::::::::::: functions: varcomp, harmonic mean, reliability :::::::::

# extract variance components and compute ICC in two-level random intercept models
# help(lmer), click the "mer" class to see "Slots" 
varcomp.old <- function(lmer.obj){
  REmat <- summary(lmer.obj)@REmat
  varcomp <- as.numeric(REmat[,3])
  names(varcomp) <- REmat[,1]
  ICC <- varcomp[1]/sum(varcomp)
  list(var.components = varcomp, ICC = ICC)
}


# By Tristan Mahr, for newer versions of package lme4 
varcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}


# harmonic.mean
h.mean <- function(x){length(x)/sum(1/x)}

# reliability of aggregated variables
reliability <- function(ICC, harmonic.mean){
  harmonic.mean*ICC/(1+(harmonic.mean-1)*ICC)
}

# extract variance components and compute ICC in two-level random intercept models
RIvarcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}

# extract variance components: the TAU matrix & sigma^2 in two-level random slope models

RSvarcomp <- function(obj) {
  TAU <- matrix(as.numeric(VarCorr(obj)[[1]]), ncol=2)
  sigmaSQ <- attr(VarCorr(obj), "sc")^2
  list(TAU=TAU, sigmaSQ=sigmaSQ)
}

RSvarcomp1 <- function(obj) {
  TAU <- matrix(as.numeric(VarCorr(obj)[[1]]), ncol=3)
  sigmaSQ <- attr(VarCorr(obj), "sc")^2
  list(TAU=TAU, sigmaSQ=sigmaSQ)
}

# ::::::::::::::::: END of functions ::::::::::::::::::::::::::::: 

out1 <- lmer(PV1MATH ~ 1+ (1|SCHOOLID), data=PISA1, REML = FALSE)
summary(out1)
deviance(out1)
VC1<- RIvarcomp(out1)
VC1

out2 <- lmer(PV1MATH ~ 1+ SES + Anxiety_Math + Efficacy + Openness + Persev + (1|SCHOOLID), data=PISA1, REML = FALSE)
summary(out2)
deviance(out2)
VC2<- RIvarcomp(out2)
VC2

out3 <- lmer(PV1MATH ~ 1+ SES +  Anxiety_Math + Efficacy + Openness + Persev +  Par_Support + Percepted_Quality + (1|SCHOOLID), data=PISA1, REML = FALSE)
summary(out3)
deviance(out3)
VC3<- RIvarcomp(out3)
VC3

out4 <- lmer(PV1MATH ~ 1+ SES + Anxiety_Math + Efficacy + Openness + Persev + Par_Support + Percepted_Quality + Autonomy + Tch_Participation + Edu_Resource + Tch_Short + (1+SES|SCHOOLID), data=PISA1, REML = FALSE)
summary(out4)
deviance(out4)
VC4<- RSvarcomp(out4)
VC4

out5 <- lmer(PV1MATH ~ 1+ SES + Anxiety_Math + Efficacy + Openness + Persev + Par_Support + Percepted_Quality + Autonomy + Tch_Participation + Edu_Resource + Tch_Short + (1+SES+Percepted_Quality|SCHOOLID), data=PISA1, REML = FALSE)
summary(out5)
deviance(out5)
VC5<- RSvarcomp1(out5)
VC5

summary<- anova(out1, out2, out3, out4, out5)
summary

