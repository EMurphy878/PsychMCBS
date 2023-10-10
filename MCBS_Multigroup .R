
#---------------------------------------------------------------------------------------#
# Psychometrician Example Code                                                          #
# Author: Erin Murphy                                                                   #
# Updated: 6/4/2023                                                                     #           
# Purpose: Multigroup (Male vs Femal) analysis of General Patient Satisfaction (GPS)    #
# Instrument from 2017 Medicare Current Beneficiary Survey (MCBS) PUF                   # 
#                                                                                       #
#---------------------------------------------------------------------------------------#

setwd("~/Desktop/EDMS724 - IRT")
dat <- read.csv("projectdat.csv")
is.na(dat)
require(mirt)
require(dplyr)
require(naniar)
require(psych)
dat2<-cbind(dat[,1:12],dat[,58:74])

#Cleaning#
na99 <-c("S1Q2","S1Q5")
na9 <-c("S1Q1","S1Q3","S1Q4","S1Q6","S1Q7","S1Q8","S1Q9","S1Q10","S1Q11","S4Q57","S4Q58","S4Q59","S4Q60","S4Q61","S4Q62","S4Q63","S4Q64","S4Q65","S4Q66","S4Q67","S4Q68","S4Q69","S4Q70","S4Q71","S4Q72","S4Q73")
dat2[, na99][dat2[,na99] == 99] <- NA
dat2[, na9][dat2[,na9] == 9] <- NA


dat2$S1Q1 <- factor(dat$S1Q1, levels = c("1","2"), exclude = NA)
dat2$S1Q3 <- factor(dat$S1Q3, levels = c("1","2","3","4"), exclude = NA)
dat2$S1Q4 <- factor(dat$S1Q4, levels = c("1","2","3","4"), exclude = NA)
dat2$S1Q6 <- factor(dat$S1Q6, levels = c("1","2","3","4","5","6"), exclude = NA)
dat2$S1Q9 <- factor(dat$S1Q9, levels = c("1","2","3","4","5"), exclude = NA)

str(dat2)

#Part 1 - EDA#
summary(dat2)
table(dat$S1Q1, dat$S1Q6)
count(dat2$S1Q1)
count(dat2$S1Q3)
count(dat2$S1Q4)
count(dat2$S1Q6)
count(dat2$S1Q9)

#Plots...
#Part 2 - EFA/CFA#

#EFA#
dat3 <-dat2[,13:26]
dat3.pca <-princomp(na.omit(dat3))
summary(dat3.pca)
plot(dat3.pca)

#Check Internal Conisitency of Stress Items (57-70)
#Recode reverse coded items S4Q58 S4Q60 S4Q64 S4Q65 S4Q66 S4Q67 S4Q68 S4Q70
dat3[,c(2,4,8,9,10,11,12,14)] <- abs(5 -dat3[,c(2,4,8,9,10,11,12,14)]) 
alpha(dat3) #Highest alpha so far....
summary(dat3)
dat3.pca <-princomp(na.omit(dat3))
dat3.fac <- factanal(na.omit(dat3), factors = 3)
colnam <- names(dat3)
tapply(na.omit(dat3), colnam, var)
cov(na.omit(dat3)) 

######Maladaptive coping S4Q58 S4Q60 S4Q64 S4Q65 S4Q66 S4Q67 S4Q68 S4Q70S4Q57######## 
dat.mal <- subset(dat3, select = c("S4Q58", "S4Q60", "S4Q64", "S4Q65", "S4Q66", "S4Q67", "S4Q68", "S4Q70"))
alpha(dat.mal)
cor(na.omit(dat.mal)) 
dat.mal.pca <-principal(na.omit(dat.mal), nfactors = 1, rotate = "varimax")
datfac.mal <- factanal(na.omit(dat.mal), factors = 1)
#Calibrate Items - Graded Model
mod.graded <- mirt(dat.mal, 1L, SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.graded, simplify = T)
plot(mod.graded, type = "trace", theta_lim = c(-3, 3))
#Calibrate Items - GPC
mod.gpc <- mirt(dat.mal, 1L, itemtype = "gpcm", SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.gpc, simplify = T)
plot(mod.gpc, type = "trace", theta_lim = c(-3, 3))
#Calibrate Items - Sequential
mod.seq <- mirt(dat.mal, 1L, itemtype = "sequential", SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.seq)
plot(mod.seq, type = "trace", theta_lim = c(-3, 3))

#Overlay Models for Item 1 (S4Q57) - Not working... 
key=list(columns=2, 
         text=list(lab=c("mod.grade","mod.gpc","mod.seq")), 
         lines=list(lwd=4, col=c("blue","orange","green")))
p.1 = update(plot(mod.graded, which.items = 1, type = "trace", key = key, main = "Item 1 Trace Lines"), col = "blue")
p.2 = update(plot(mod.gpc,which.items = 1,type="trace", key = key),col="orange")
p.3 = update(plot(mod.seq,which.items = 1,type="trace", key = key),col="green")
p.1+p.2+p.3


############### Maladaptive Coping Recode Combine Categories 1 and 2 (<= 2 =1)##########
#: Use this chunk here
#;
#################################################################################
vars <- c("S4Q58", "S4Q60", "S4Q64", "S4Q65", "S4Q66", "S4Q67", "S4Q68", "S4Q70")
level_key <- c("4" = 3, "3" = 2, "2" = 1,"1" = 1)
dat.mal$S4Q58<-recode(dat.mal$S4Q58, !!!level_key)
dat.mal$S4Q60<-recode(dat.mal$S4Q60, !!!level_key)
dat.mal$S4Q64<-recode(dat.mal$S4Q64, !!!level_key)
dat.mal$S4Q65<-recode(dat.mal$S4Q65, !!!level_key)
dat.mal$S4Q66<-recode(dat.mal$S4Q66, !!!level_key)
dat.mal$S4Q67<-recode(dat.mal$S4Q67, !!!level_key)
dat.mal$S4Q68<-recode(dat.mal$S4Q68, !!!level_key)
dat.mal$S4Q70<-recode(dat.mal$S4Q70, !!!level_key)
dat.mal[,1:8] <- dat.mal[,1:8] - 1
#Calibrate Items 
mod.1 <- mirt(dat.mal, 1L, SE = T, technical = list(removeEmptyRows = TRUE))
plot(mod.1, type = "trace", theta_lim = c(-3, 3))
#Calibrate Items - GPC
mod.gpc <- mirt(dat.mal, 1L, itemtype = "gpcm", SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.gpc, simplify = T)
plot(mod.gpc, type = "trace", theta_lim = c(-3, 3))
#Calibrate Items - Sequential
mod.seq <- mirt(dat.mal, 1L, itemtype = "sequential", SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.seq)
plot(mod.seq, type = "trace", theta_lim = c(-3, 3))

#Model Fit Assessment 
mod.pc <- mirt(dat.mal, 1L, itemtype = 'Rasch', SE=T, technical = list(removeEmptyRows = TRUE) )
coef(mod.pc, simplify = T)
#Difference in LL of GPC and PC = -5998.68 - -5955.134 = -43.456 (GPC, Graded, and Seq all within 1 LL unit)
#Gonna use graded (could use any one)

################## Multigroup analysis ###############
#Analysis 1: M vs F
dat.mal$gender <-dat2$S1Q1
dat.mal <- na.omit(dat.mal)
dat.f <- dat.mal[dat.mal$gender == 2,]
dat.m <- dat.mal[dat.mal$gender == 1,]
gender <- c( rep( "M", nrow(dat.m) ), rep( "F", nrow(dat.f)))

dat.mal <- dat.mal[,-9]
#Mod 1: Sepearate Analysis
mod1 <- multipleGroup(dat.mal, 1, group = gender, SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod1, simplify = T)
#Mod 2: Free Mean
mod2 <- multipleGroup(dat.mal, 1, group = gender, SE = T, invariance = c("free_mean"))
coef(mod2, simplify = T)
#Mod 3: Free Var - Not Identified... hmmm
mod3 <- multipleGroup(dat.mal, 1, group = gender, SE = T, invariance = "free_var")
coef(mod3, simplify = T)
#Mod 4: Constrained Slopes (Free Var, Free Mean)
mod4 <- multipleGroup(dat.mal, 1, group = gender, SE = T, invariance = c("slopes","free_var","free_mean"))
coef(mod4, simplify = T)
#Mod 5: Constrained Intercepts (Free Var, Free Mean)
mod5 <- multipleGroup(dat.mal, 1, group = gender, SE = T, invariance = c("intercepts", "free_var","free_mean"))
coef(mod5, simplify = T)
#Mod 6: Constrained Slopes and Intercepts (Free Var, Free Means) (Full Invariance)
mod6 <- multipleGroup(dat.mal, 1, group = gender, SE = T, invariance = c("slopes","intercepts", "free_mean","free_var"))
coef(mod6, simplify = T)


#Comparison ANOVAs 

#DIF Analysis 
schB <- DIF(mod6, scheme = "drop", which.par = c("a1","d1","d2","d3"))


#Analysis 2: Ethnicity (Drop Hispanic and Other)
dat.mal2 <- dat.mal
dat.mal2$eth <-dat2$S1Q3
dat.mal2 <- na.omit(dat.mal2)
dat.mal2 <- subset(dat.mal2, eth != 4)
dat.mal2 <- subset(dat.mal2, eth != 3)

dat.b <- dat.mal2[dat.mal2$eth == 1,]
dat.c <- dat.mal2[dat.mal2$eth == 2,]
eth <- c(rep( "B", nrow(dat.b) ), rep( "C", nrow(dat.c)))
dat.mal2 <- dat.mal2[,-9]

#Mod 1: Sepearate Analysis
mod1 <- multipleGroup(dat.mal2, 1, group = eth, SE = T)
coef(mod1, simplify = T)
#Mod 2: Free Mean
mod2 <- multipleGroup(dat.mal2, 1, group = eth, SE = T, invariance = c("free_mean"))
coef(mod2, simplify = T)
#Mod 3: Free Var - Not Identified... hmmm
mod3 <- multipleGroup(dat.mal2, 1, group = eth, SE = T, invariance = "free_var")
coef(mod3, simplify = T)
#Mod 4: Constrained Slopes (Free Var, Free Mean)
mod4 <- multipleGroup(dat.mal2, 1, group = eth, SE = T, invariance = c("slopes","free_var","free_mean"))
coef(mod4, simplify = T)
#Mod 5: Constrained Intercepts (Free Var, Free Mean) - Not identified... hmmm
mod5 <- multipleGroup(dat.mal2, 1, group = eth, SE = T, invariance = c("intercepts", "free_var","free_mean"))
coef(mod5, simplify = T)
#Mod 6: Constrained Slopes and Intercepts (Free Var, Free Means) (Full Invariance)
mod6 <- multipleGroup(dat.mal2, 1, group = eth, SE = T, invariance = c("slopes","intercepts", "free_mean","free_var"))
coef(mod6, simplify = T)

#Anlaysis 3: Marital Status 
dat.mal3 <- dat.mal

dat.mal3$mar <- dat2$S1Q9
dat.mal3 <- na.omit(dat.mal3)
level_key <- c("5" = 0, "4" = 0, "3" = 0, "2" = 1,"1" = 1)
dat.mal3$mar <-recode(dat.mal3$mar, !!!level_key)
dat.ma <- dat.mal3[dat.mal3$mar == 0,]
dat.s <- dat.mal3[dat.mal3$mar == 1,]
ma <- c(rep( "M", nrow(dat.ma) ), rep( "S", nrow(dat.s)))
dat.mal3 <- dat.mal3[,-9]

#Mod 1: Sepearate Analysis
mod1 <- multipleGroup(dat.mal3, 1, group = ma, SE = T)
coef(mod1, simplify = T)
#Mod 2: Free Mean
mod2 <- multipleGroup(dat.mal3, 1, group = eth, SE = T, invariance = c("free_mean"))
coef(mod2, simplify = T)
#Mod 3: Free Var - Not Identified... hmmm
mod3 <- multipleGroup(dat.mal3, 1, group = eth, SE = T, invariance = "free_var")
coef(mod3, simplify = T)
#Mod 4: Constrained Slopes (Free Var, Free Mean)
mod4 <- multipleGroup(dat.mal3, 1, group = eth, SE = T, invariance = c("slopes","free_var","free_mean"))
coef(mod4, simplify = T)
#Mod 5: Constrained Intercepts (Free Var, Free Mean) - Not identified... hmmm
mod5 <- multipleGroup(dat.mal3, 1, group = eth, SE = T, invariance = c("intercepts", "free_var","free_mean"))
coef(mod5, simplify = T)
#Mod 6: Constrained Slopes and Intercepts (Free Var, Free Means) (Full Invariance)
mod6 <- multipleGroup(dat.mal3, 1, group = eth, SE = T, invariance = c("slopes","intercepts", "free_mean","free_var"))
coef(mod6, simplify = T)

#################Active Coping: 57,58(Reverse),59,62,70(Reverse), 66###################

dat4 <- subset(dat3, select = c("S4Q58","S4Q62","S4Q59","S4Q70"))
dat4 <- subset(dat3, select = c("S4Q57","S4Q58","S4Q62","S4Q59","S4Q63","S4Q70","S4Q66"))

alpha(dat4)
cor(na.omit(dat4))
#Reverse Coding 58 and 70 (1,2,3,4) -> (4,3,2,1)
dat4[,c(1,4)] <- abs(5 -dat4[,c(1,4)] )

#Recode to Start at 0
dat4[,c(1,2,3,4)] <- dat4[,c(1,2,3,4)] - 1
dat4.pca <-princomp(na.omit(dat4))
summary(dat4.pca)

alpha(dat4)
dat4.fa <-factanal(na.omit(dat4), factors = 1, rotation - "varimax")
#I'm concenred about this because it seems these items don't have 1 factor (active coping), but I will persit...


#Calibrate Items - Graded Response Model (*Original 4 Items 58 (Rev), 62, 59, 70 (Rev) dat4)
mod.graded <- mirt(dat4, 1L, SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.graded, printSE = TRUE)
coef(mod.graded)
plot(mod.graded, type = "trace", theta_lim = c(-3, 3))
itemplot(mod.graded, item = 4, type = "score", theta_lim = c(-3, 3))
plot(mod.graded, type = "info", theta_lim = c(-3, 3))
plot(mod.graded, type = "score", theta_lim = c(-3, 3))
#Calibrate Items - GPC
mod.gpc <- mirt(dat4, 1L, itemtype = "gpcm", SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.gpc, simplify = T)
plot(mod.gpc, type = "trace", theta_lim = c(-3, 3))
#Calibrate Items - Sequential
mod.seq <- mirt(dat4, 1L, itemtype = "sequential", SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod.seq)
plot(mod.seq, type = "trace", theta_lim = c(-3, 3))

#Overlay Models for Item 1 (S4Q57) - Not working... 
key=list(columns=2, 
         text=list(lab=c("mod.grade","mod.gpc","mod.seq")), 
         lines=list(lwd=4, col=c("blue","orange","green")))
p.1 = update(plot(mod.graded, which.items = 1, type = "trace", key = key, main = "Item 1 Trace Lines"), col = "blue")
p.2 = update(plot(mod.gpc,which.items = 1,type="trace", key = key),col="orange")
p.3 = update(plot(mod.seq,which.items = 1,type="trace", key = key),col="green")
p.1+p.2+p.3



#Should I condense category 3 and 4 for items 70 and 57? 


#Multi Group Analysis
#Analysis 1: M vs F
dat4$gender <-dat2$S1Q1
dat4 <- na.omit(dat4)
dat.f <- dat4[dat4$gender == 2,]
dat.m <- dat4[dat4$gender == 1,]
gender <- c( rep( "M", nrow(dat.m) ), rep( "F", nrow(dat.f) ) )

dat4 <- dat4[,-5]
#Mod 1: Sepearate Analysis
mod1 <- multipleGroup(dat4, 1, group = gender, SE = T, technical = list(removeEmptyRows = TRUE))
coef(mod1, simplify = T)
#Mod 2: Free Mean
mod2 <- multipleGroup(dat4, 1, group = gender, SE = T, invariance = c("free_mean"))
coef(mod2, simplify = T)
#Mod 3: Free Var - Not Identified... hmmm
mod3 <- multipleGroup(dat4, 1, group = gender, SE = T, invariance = "free_var")
coef(mod3, simplify = T)
#Mod 4: Constrained Slopes (Free Var, Free Mean)
mod4 <- multipleGroup(dat4, 1, group = gender, SE = T, invariance = c("slopes","free_var","free_mean"))
coef(mod4, simplify = T)
#Mod 5: Constrained Intercepts (Free Var, Free Mean)
mod5 <- multipleGroup(dat4, 1, group = gender, SE = T, invariance = c("intercepts", "free_var","free_mean"))
coef(mod5, simplify = T)
#Mod 6: Constrained Slopes and Intercepts (Free Var, Free Means) (Full Invariance)
mod6 <- multipleGroup(dat4, 1, group = gender, SE = T, invariance = c("slopes","intercepts", "free_mean","free_var"))
coef(mod6, simplify = T)


#Comparisons 

#Equal Slopes
#Equal Slopes - Free 

#DIF Analysis 
schB <- DIF(mod6, scheme = "drop", which.par = c("a1","d1","d2","d3"))
#Analysis 2: Ethnicity (Drop Hispanic and Other)
dat5 <- dat4
dat5$eth <-dat2$S1Q3
dat5 <- na.omit(dat5)
dat5 <- subset(dat5, eth != 4)
dat5 <- subset(dat5, eth != 3)


dat.b <- dat5[dat5$eth == 1,]
dat.c <- dat5[dat5$eth == 2,]
eth <- c(rep( "B", nrow(dat.b) ), rep( "C", nrow(dat.c)))
dat5 <- dat5[,-6]

#Mod 1: Sepearate Analysis
mod1 <- multipleGroup(dat5, 1, group = eth, SE = T)
coef(mod1, simplify = T)
#Mod 2: Free Mean
mod2 <- multipleGroup(dat5, 1, group = eth, SE = T, invariance = c("free_mean"))
coef(mod2, simplify = T)
#Mod 3: Free Var - Not Identified... hmmm
mod3 <- multipleGroup(dat5, 1, group = eth, SE = T, invariance = "free_var")
coef(mod3, simplify = T)
#Mod 4: Constrained Slopes (Free Var, Free Mean)
mod4 <- multipleGroup(dat5, 1, group = eth, SE = T, invariance = c("slopes","free_var","free_mean"))
coef(mod4, simplify = T)
#Mod 5: Constrained Intercepts (Free Var, Free Mean) - Not identified... hmmm
mod5 <- multipleGroup(dat5, 1, group = eth, SE = T, invariance = c("intercepts", "free_var","free_mean"))
coef(mod5, simplify = T)
#Mod 6: Constrained Slopes and Intercepts (Free Var, Free Means) (Full Invariance)
mod6 <- multipleGroup(dat5, 1, group = eth, SE = T, invariance = c("slopes","intercepts", "free_mean","free_var"))
coef(mod6, simplify = T)

#Comparisons 


#Analysis 3: Marital Status 

#DIF


#Analysis 3: Marital Status 


