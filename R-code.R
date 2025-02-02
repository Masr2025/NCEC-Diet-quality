#==========================================================================================#
# R version 4.2.3
# Dietary indices and upper gastrointestinal tract tumors (esophageal and gastric cancers) were calculated for the relationship between dietary indices and cancer.
#==========================================================================================#

library(readxl)
P <- read_excel("[YourPath]/diet.xlsx")
library(dplyr)
P1 <- subset(P, education < 99)
P2 <- subset(P1, age < 70) 

##==========================================  Modify the variable name  ==============================================##
P2$Sex <- factor(P2$sex, levels=c(2,3),labels=c("Female","Male"))
P2$Age5_G <- factor(P2$age5, levels=c(1,2),labels=c("<55","≥55"))
P2$Marriage <- factor(P2$marriage, levels=c(1,2),labels=c("Unmarried/living alone/divorced/widowed", "Married"))
P2$Education1 <- factor(P2$education, levels=c(1,2,3),labels=c("Primary school or below", "Above Junior school","Above Junior school"))
P2$Occupation1 <- factor(P2$occupation, levels=c(1,2,3,4,5,6),labels=c("Manual labor", "Manual labor","Non-manual labor & Other","Non-manual labor & Other","Non-manual labor & Other","Non-manual labor & Other"))
P2$Income1 <- factor(P2$income, levels=c(1,2,3,4),labels=c("<7.0", "<7.0","≥7.0","≥7.0"))
P2$Smoking <- factor(P2$smoking, levels=c(0,2),labels=c("No", "Yes"))
P2$Drinking <- factor(P2$drinking, levels=c(0,2),labels=c("No", "Yes"))
P2$Tea <- factor(P2$`dringk tea`, levels=c(0,2),labels=c("No", "Yes"))
P2$Water <- factor(P2$water, levels=c(3,4),labels=c("Other", "Piped water"))
P2$BMI <- factor(P2$BMI_G, levels=c(1,2,3,4),labels=c("<18.5", "18.5-23.9","24.0-27.9","≥28.0"))
P2$BMI1 <- factor(P2$BMI_G, levels=c(1,2,3,4),labels=c("<24.0", "<24.0","≥24.0","≥24.0"))
P2$PA41 <- factor(P2$PA, levels=c(1,2,3,4,5),labels=c("Less 1-3 times/month","Less 1-3 times/month", "Above 1-2 times/week","Above 1-2 times/week","Above 1-2 times/week"))
P2$Digestive <- factor(P2$digestivesd, levels=c(0,1),labels=c("No", "Yes"))
P2$Hypertension <- factor(P2$hypertension, levels=c(0,1),labels=c("No", "Yes"))
P2$Diabetes <- factor(P2$diabetes, levels=c(0,1),labels=c("No", "Yes"))
P2$Site2 <- factor(P2$diqu, levels=c(1,2,3),labels=c("1", "2", "2")) # 1=east; 2=other
P2$Site1 <- factor(P2$diqu, levels=c(1,2,3),labels=c("1", "2", "1")) # 1=other; 2=north
P2$Spicy <- factor(P2$E13, levels=c(1,2,3,4,5),labels=c("Never or almost never","Never or almost never", "Over 1-2 days/week", "Over 1-2 days/week", "Over 1-2 days/week"))
P2$Spicy1 <- factor(P2$E13, levels=c(1,2,3,4,5),labels=c("Never or almost never","Over 1 time/week", "Over 1 time/week", "Over 1 time/week", "Over 1 time/week"))

##==========================================  Dietary index  ==============================================##
P2$HBS_G1 <- factor(P2$HBS_G, levels=c(1,2,3,4),labels=c("No problem or almost no problem", "Low level", "Moderate level", "High level"))
P2$LBS_G1 <- factor(P2$LBS_G, levels=c(1,2,3,4),labels=c("No problem or almost no problem", "Low level", "Moderate level", "High level"))
P2$DQD_G1 <- factor(P2$DQD_G, levels=c(1,2,3,4),labels=c("No problem or almost no problem", "Low level", "Moderate level", "High level"))
P2$HBS_G11 <- factor(P2$HBS_G, levels=c(1,2,3,4),labels=c("Low level","Low level", "Moderate level", "High level"))
P2$LBS_G11 <- factor(P2$LBS_G, levels=c(1,2,3,4),labels=c("Low level","Low level", "Moderate level", "High level"))
P2$DQD_G11 <- factor(P2$DQD_G, levels=c(1,2,3,4),labels=c("Low level","Low level", "Moderate level", "High level"))
P2$bl2 <- factor(P2$ybl, levels=c(1,2,3,4),labels=c("Normal or Inflammation", "Normal or Inflammation","Precancerous lesion","Precancerous lesion"))
P2$BL1 <- factor(P2$BL, levels=c(0,1),labels=c("Normal", "Cancer"))

P2 <- P2 %>%
  mutate_if(is.numeric, round, digits = 2) # 
P3 <- subset(P2, diet < 1)
P4 <- subset(P3, ybl < 4)
quantile(P4$tPDI, probs = seq(0,1,0.25), na.rm = FALSE)
quantile(P4$thPDI, probs = seq(0,1,0.25), na.rm = FALSE)
quantile(P4$tuPDI, probs = seq(0,1,0.25), na.rm = FALSE)
P4$tPDI_G <- cut(P4$tPDI, breaks=c(23,42,45,49,66),include.lowest=T,labels = c(1,2,3,4))
P4$thPDI_G <- cut(P4$thPDI, breaks=c(28,49,52,55,71),include.lowest=T,labels = c(1,2,3,4))
P4$tuPDI_G <- cut(P4$tuPDI, breaks=c(21,38,44,50,65),include.lowest=T,labels = c(1,2,3,4))

##==========================================  Baseline table  ==============================================##
library(scitb)
library(stringi)

allVars <- c("Energy1","age", "Age5_G","Sex", "diqu", "Marriage", "Education1", "Occupation1", "Income1", "Smoking", "Drinking", "Spicy", "BMI1", "PA41", "Digestive", "Hypertension", "Diabetes", "bl1", "bl2","HBS","LBS", "DQD", "tPDI", "tuPDI","thPDI", "inci_U", "inci_E","inci_S")
fvars <- c("Age5_G","Sex", "diqu", "Marriage", "Education1", "Occupation1", "Income1", "Smoking", "Drinking", "Spicy", "BMI1", "PA41", "Digestive", "Hypertension","Diabetes", "bl1", "bl2","inci_U", "inci_E","inci_S")
strata1 <- "HBS_G11"
out1 <- scitb1(vars=allVars, fvars=fvars, strata = strata1, data=P4, statistic=F, atotest=F, Overall=T, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))
strata2 <- "LBS_G11"
out2 <- scitb1(vars=allVars, fvars=fvars, strata = strata2, data=P4, statistic=F, atotest=F, Overall=F, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))
strata3 <- "DQD_G11"
out3 <- scitb1(vars=allVars, fvars=fvars, strata = strata3, data=P4, statistic=F, atotest=F, Overall=F, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))
strata4 <- "tPDI_5"
out4 <- scitb1(vars=allVars, fvars=fvars, strata = strata4, data=P4, statistic=F, atotest=F, Overall=F, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))
strata5 <- "tuPDI_5"
out5 <- scitb1(vars=allVars, fvars=fvars, strata = strata5, data=P4, statistic=F, atotest=F, Overall=F, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))
strata6 <- "thPDI_5"
out6 <- scitb1(vars=allVars, fvars=fvars, strata = strata6, data=P4, statistic=F, atotest=F, Overall=F, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))
strata7 <- "DII_E4"
out7 <- scitb1(vars=allVars, fvars=fvars, strata = strata7, data=P4, statistic=F, atotest=F, Overall=F, fisher=F, correct=T, nonnormal
               =c("HBS","LBS", "DQD", "tPDI", "thPDI", "tuPDI"))


##==========================================  Survival analysis_Cox  ==============================================##
library("survival")
library("survminer")
library(reportReg)

##==========================================  Model 2  ==============================================##
#DBI
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)  #UGI
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, data = P4)  #EC 
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, data = P4)  #GC reportReg(res1_1)
reportReg(res1_2)
reportReg(res1_3)
#PDI 
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #UGI
res0_2 <- coxph(Surv(T_inci_E,inci_E) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4) 
res0_3 <- coxph(Surv(T_inci_S,inci_S) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4) 
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)

#P for trend
library(foreign)
library(dplyr)
P4$HBS_g <- factor(P4$HBS_G, levels=c(1,3,4), labels=c("10","19","26"))
P4$LBS_g <- factor(P4$LBS_G, levels=c(1,3,4), labels=c("22", "33","41"))
P4$DQD_g <- factor(P4$DQD_G, levels=c(1,3,4), labels=c("29", "40","51"))
P4$tPDI_g <- factor(P4$tPDI_G, levels=c(1,2,3,4), labels=c("40", "44","47","52"))
P4$tuPDI_g <- factor(P4$tuPDI_G, levels=c(1,2,3,4), labels=c("35", "42","48","53"))
P4$thPDI_g <- factor(P4$thPDI_G, levels=c(1,2,3,4), labels=c("47", "51","54","58"))

#Model 2_P for trend
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #UGI
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #EC 
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #GC 
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)  #UGI
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)   #EC 
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)   #GC 
reportReg(res1_1)
reportReg(res1_2)
reportReg(res1_3)

# Model2_continuous variable
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI + tuPDI + thPDI + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #UGI
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI + tuPDI + thPDI + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #EC 
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI + tuPDI + thPDI + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, data = P4)   #GC 
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS + LBS + DQD + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)  #UGI
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS + LBS + DQD + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)   #EC 
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS + LBS + DQD + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension + Diabetes, data = P4)   #GC 
reportReg(res1_1)
reportReg(res1_2)
reportReg(res1_3)

##==========================================  Model 1  ==============================================##
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1, data = P4)   #UGI
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1, data = P4)   #EC 
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1, data = P4)   #GC
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2, data = P4)   #UGI
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2, data = P4)   #EC 
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2, data = P4)   #GC 
reportReg(res1_1)
reportReg(res1_2)
reportReg(res1_3)

# Model 1_P for trend
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1, data = P4)   #UGI
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1, data = P4)   #EC 
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1, data = P4)   #GC 
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2, data = P4)   #UGI
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2, data = P4)   #EC 
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2, data = P4)   #GC 
reportReg(res1_1)
reportReg(res1_2)
reportReg(res1_3)

# Model1_continuous variable
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI + tuPDI + thPDI + Age5_G + Sex + bl2 + Site1, data = P4)   #UGI
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI + tuPDI + thPDI + Age5_G + Sex + bl2 + Site1, data = P4)   #EC 
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI + tuPDI + thPDI + Age5_G + Sex + bl2 + Site1, data = P4)   #GC 
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS + LBS + DQD + Age5_G + Sex + bl2 + Site2, data = P4)   #UGI
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS + LBS + DQD + Age5_G + Sex + bl2 + Site2, data = P4)   #EC 
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS + LBS + DQD + Age5_G + Sex + bl2 + Site2, data = P4)   #GC 
reportReg(res1_1)
reportReg(res1_2)
reportReg(res1_3)

##==========================================  Subgroup analysis  ==============================================##
P4$spicy <- factor(P4$E13, levels=c("1","2","3","4","5"),labels=c("1","1","2","2","2"))
P4$spicy1 <- factor(P4$E13, levels=c("1","2","3","4","5"),labels=c("1","2","2","2","2"))
P4$bmi1 <- factor(P4$BMI_G, levels=c("1","2","3","4"),labels=c("1","1","2","2"))
P4$bl3 <- factor(P4$bl, levels=c(1,2,3),labels=c("1","1","2"))

# PDI_UGI
# age (≤55 y; >55 y)
res01_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="1"))   #UGI
res01_2 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="2")) 
reportReg(res01_1)
reportReg(res01_2)
# sex (female; male)
res01_3 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="2")) 
res01_4 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="3")) 
reportReg(res01_3)
reportReg(res01_4)
# residence district (Other; North)
res01_5 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="1")) 
res01_6 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="2")) 
reportReg(res01_5)
reportReg(res01_6)
# smoking (No; Yes)
res01_8 <- coxph(Surv(T_inci_U,inci_U) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="0")) #NO
res01_9 <- coxph(Surv(T_inci_U,inci_U) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="2")) #YES
reportReg(res01_8)
reportReg(res01_9)
# BMI (<24.0; ≥24.0)
res01_10 <- coxph(Surv(T_inci_U,inci_U) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="1")) #<24
res01_11 <- coxph(Surv(T_inci_U,inci_U) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="2")) #>24
reportReg(res01_10)
reportReg(res01_11)
# physiology (Normal & Inflammation; Precancerous lesion)
res01_14 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="1")) 
res01_15 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="2")) 
reportReg(res01_14)
reportReg(res01_15)

#PDI_EC
#age (≤55 y; >55 y)
res02_1 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="1")) 
res02_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="2")) 
reportReg(res02_1)
reportReg(res02_2)
# sex (female; male)
res02_3 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="2")) 
res02_4 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="3")) 
reportReg(res02_3)
reportReg(res02_4)
# residence district (Other; North)
res02_5 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="1")) 
res02_6 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="2")) 
reportReg(res02_5)
reportReg(res02_6)
# smoking (No; Yes)
res02_8 <- coxph(Surv(T_inci_E,inci_E) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="0")) #NO
res02_9 <- coxph(Surv(T_inci_E,inci_E) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="2")) #YES
reportReg(res02_8)
reportReg(res02_9)
# BMI (<24.0; ≥24.0)
res02_10 <- coxph(Surv(T_inci_E,inci_E) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="1")) #<24
res02_11 <- coxph(Surv(T_inci_E,inci_E) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="2")) #>24
reportReg(res02_10)
reportReg(res02_11)
# physiology (Normal & Inflammation; Precancerous lesion)
res02_14 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="1")) 
res02_15 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="2")) 
reportReg(res02_14)
reportReg(res02_15)

#GC_PDI
# age (≤55 y; >55 y)
res03_1 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="1")) 
res03_2 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="2")) 
reportReg(res03_1)
reportReg(res03_2)
# sex (female; male)
res03_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="2")) 
res03_4 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="3")) 
reportReg(res03_3)
reportReg(res03_4)
# residence district (Other; North)
res03_5 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="1")) 
res03_6 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="2")) 
reportReg(res03_5)
reportReg(res03_6)
# smoking (No; Yes)
res03_8 <- coxph(Surv(T_inci_S,inci_S) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="0")) #NO
res03_9 <- coxph(Surv(T_inci_S,inci_S) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="2")) #YES
reportReg(res03_8)
reportReg(res03_9)
# BMI (<24.0; ≥24.0)
res03_10 <- coxph(Surv(T_inci_S,inci_S) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="1")) #<24
res03_11 <- coxph(Surv(T_inci_S,inci_S) ~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="2")) #>24
reportReg(res03_10)
reportReg(res03_11)
# physiology (Normal & Inflammation; Precancerous lesion)
res03_14 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="1")) 
res03_15 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="2")) 
reportReg(res03_14)
reportReg(res03_15)

#UGI_DBI
# age (≤55 y; >55 y)
res11_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="1")) 
res11_2 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="2")) 
reportReg(res11_1)
reportReg(res11_2)
# sex (female; male)
res11_3 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="2")) 
res11_4 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="3")) 
reportReg(res11_3)
reportReg(res11_4)
# residence district (Other; North)
res11_5 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="1")) 
res11_6 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="2")) 
reportReg(res11_5)
reportReg(res11_6)
# smoking (No; Yes)
res11_8 <- coxph(Surv(T_inci_U,inci_U) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="0")) #NO
res11_9 <- coxph(Surv(T_inci_U,inci_U) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="2")) #YES
reportReg(res11_8)
reportReg(res11_9)
# BMI (<24.0; ≥24.0)
res11_10 <- coxph(Surv(T_inci_U,inci_U) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="1")) #<24
res11_11 <- coxph(Surv(T_inci_U,inci_U) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="2")) #>24
reportReg(res11_10)
reportReg(res11_11)
# physiology (Normal & Inflammation; Precancerous lesion)
res11_14 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="1")) 
res11_15 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="2")) 
reportReg(res11_14)
reportReg(res11_15)

#EC_DBI
# age (≤55 y; >55 y)
res12_1 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="1")) 
res12_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="2")) 
reportReg(res12_1)
reportReg(res12_2)
# sex (female; male)
res12_3 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="2")) 
res12_4 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="3")) 
reportReg(res12_3)
reportReg(res12_4)
# residence district (Other; North)
res12_5 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="1")) 
res12_6 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="2")) 
reportReg(res12_5)
reportReg(res12_6)
# smoking (No; Yes)
res12_8 <- coxph(Surv(T_inci_E,inci_E) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="0")) #NO
res12_9 <- coxph(Surv(T_inci_E,inci_E) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="2")) #YES
reportReg(res12_8)
reportReg(res12_9)
# BMI (<24.0; ≥24.0)
res12_10 <- coxph(Surv(T_inci_E,inci_E) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="1")) #<24
res12_11 <- coxph(Surv(T_inci_E,inci_E) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="2")) #>24
reportReg(res12_10)
reportReg(res12_11)
# physiology (Normal & Inflammation; Precancerous lesion)
res12_14 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="1")) 
res12_15 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="2")) 
reportReg(res12_14)
reportReg(res12_15)

#GC_DBI
# age (≤55 y; >55 y)
res13_1 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="1")) 
res13_2 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (age5 =="2")) 
reportReg(res13_1)
reportReg(res13_2)
# sex (female; male)
res13_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="2")) 
res13_4 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (sex =="3")) 
reportReg(res13_3)
reportReg(res13_4)
# residence district (Other; North)
res13_5 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="1")) 
res13_6 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (Site1 =="2")) 
reportReg(res13_5)
reportReg(res13_6)
# smoking
res13_8 <- coxph(Surv(T_inci_S,inci_S) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="0")) #NO
res13_9 <- coxph(Surv(T_inci_S,inci_S) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                 data = P4, subset = (smoking =="2")) #YES
reportReg(res13_8)
reportReg(res13_9)
# BMI (<24.0; ≥24.0)
res13_10 <- coxph(Surv(T_inci_S,inci_S) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="1")) #<24
res13_11 <- coxph(Surv(T_inci_S,inci_S) ~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bmi1 =="2")) #>24
reportReg(res13_10)
reportReg(res13_11)
# physiology (Normal & Inflammation; Precancerous lesion)
res13_14 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="1")) 
res13_15 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI1 + PA4 + Digestive + Hypertension + Diabetes, 
                  data = P4, subset = (bl3 =="2")) 
reportReg(res13_14)
reportReg(res13_15)


##==========================================  Sensitivity analysis  ==============================================##
# Exclusion of patients with digestive disorders
P5 <- subset(P4, digestivesd == 0)
#DBI & DBI_P for trend
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Hypertension + Diabetes, data = P5)  #UGI
reportReg(res1_1)
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Hypertension + Diabetes, data = P5)   #EC
reportReg(res1_2)
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Hypertension + Diabetes, data = P5)  #GC
reportReg(res1_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Hypertension + Diabetes, data = P5)   #UGI_P for trend
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Hypertension + Diabetes, data = P5)    #UGI_P for trend
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Hypertension + Diabetes, data = P5)   #UGI_P for trend
reportReg(res1_2)
reportReg(res1_2)
reportReg(res1_3)

#PDI & PDI_P for trend
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Hypertension + Diabetes, data = P5)   #UGI
reportReg(res0_1)
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Hypertension + Diabetes, data = P5)   #EC
reportReg(res0_2)
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Hypertension + Diabetes, data = P5)   #GC
reportReg(res0_3)
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Hypertension + Diabetes, data = P5)    #UGI_P for trend
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Hypertension + Diabetes, data = P5)    #EC_P for trend
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Hypertension + Diabetes, data = P5)    #GC_P for trend
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)

# Exclusion of diabetic patients
P7 <- subset(P4, diabetes == 0)
#DBI & DBI_P for trend
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension, data = P7)    #UGI
reportReg(res1_1)
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension, data = P7)    #EC
reportReg(res1_2)
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension, data = P7)    #GC
reportReg(res1_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension, data = P7)   #UGI_P for trend
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension, data = P7)   #EC_P for trend
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4 + Digestive + Hypertension, data = P7)   #GC_P for trend
reportReg(res1_2)
reportReg(res1_2)
reportReg(res1_3)

#PDI & PDI_P for trend
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension, data = P7)    #UGI
reportReg(res0_1)
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension, data = P7)    #EC
reportReg(res0_2)
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension, data = P7)   #GC
reportReg(res0_3)
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension, data = P7)    #UGI_P for trend
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension, data = P7)    #EC_P for trend
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41 + Digestive + Hypertension, data = P7)    #GC_P for trend
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)

# Exclusion of patients with digestive disorders, diabetes
P9 <- subset(P5, diabetes == 0)
#DBI & DBI_P for trend
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4, data = P9)    #UGI
reportReg(res1_1)
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4, data = P9)    #EC
reportReg(res1_2)
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ HBS_G11 + LBS_G11 + DQD_G11 + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4, data = P9)   #GC
reportReg(res1_3)
res1_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4, data = P9)   #UGI_P for trend
res1_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4, data = P9)   #EC_P for trend
res1_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(HBS_g) + as.numeric(LBS_g) + as.numeric(DQD_g) + Age5_G + Sex + bl2 + Site2 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Spicy1 + BMI + PA4, data = P9)   #GC_P for trend
reportReg(res1_2)
reportReg(res1_2)
reportReg(res1_3)

#PDI & PDI_P for trend
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41, data = P9)    #UGI
reportReg(res0_1)
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41, data = P9)    #EC
reportReg(res0_2)
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ tPDI_G + tuPDI_G + thPDI_G + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41, data = P9)   #GC
reportReg(res0_3)
res0_1 <- coxph(Surv(T_inci_U,inci_U)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41, data = P9)   #UGI_P for trend
res0_2 <- coxph(Surv(T_inci_E,inci_E)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41, data = P9)   #EC_P for trend
res0_3 <- coxph(Surv(T_inci_S,inci_S)~ as.numeric(tPDI_g) + as.numeric(tuPDI_g) + as.numeric(thPDI_g) + Age5_G + Sex + bl2 + Site1 + Marriage + Education1 + Occupation1 + Income1 + Smoking + Drinking + Spicy + Energy1 + BMI1 + PA41, data = P9)   #GC_P for trend
reportReg(res0_1)
reportReg(res0_2)
reportReg(res0_3)

##==========================================  Survival Analysis Subgroup Interaction Functions（P for interaction） ==============================================##
load("[YourPath]/scitb5coxph16.R")  # scitb5.coxph
library(foreign) 
library(survival)

P4$HBS_G <- as.factor(P4$HBS_G)
P4$LBS_G <- as.factor(P4$LBS_G)
P4$DQD_G <- as.factor(P4$DQD_G)
P4$tPDI_G <- as.factor(P4$tPDI_G)
P4$tuPDI_G <- as.factor(P4$tuPDI_G)
P4$thPDI_G <- as.factor(P4$thPDI_G)
P4$DII_E4 <- as.factor(P4$DII_E4)
P4$age5 <- as.factor(P4$age5)
P4$sex <- as.factor(P4$sex)
P4$Site2 <- as.factor(P4$Site2)
P4$marriage <- as.factor(P4$marriage)
P4$smoking <- as.factor(P4$smoking)
P4$Drinking <- as.factor(P4$Drinking)
P4$BMI_G <- as.factor(P4$BMI_G)
P4$digestivesd <- as.factor(P4$digestivesd)
P4$hypertension <- as.factor(P4$hypertension)
P4$diabetes <- as.factor(P4$diabetes)
P4$Site0 <- factor(P4$diqu, levels=c("1","2","3"),labels=c("1","2","3"))
P4$Site1 <- factor(P4$diqu, levels=c("1","2","3"),labels=c("1","2","1"))
P4$Site2 <- factor(P4$diqu, levels=c("1","2","3"),labels=c("1","2","2"))
P4$spicy <- factor(P4$E13, levels=c("1","2","3","4","5"),labels=c("1","1","2","2","2"))
P4$spicy1 <- factor(P4$E13, levels=c("1","2","3","4","5"),labels=c("1","2","2","2","2"))
P4$bmi1 <- factor(P4$BMI_G, levels=c("1","2","3","4"),labels=c("1","1","2","2"))
P4$bl3 <- factor(P4$bl, levels=c(1,2,3),labels=c("1","1","2"))

cov1 <- c("age5","sex", "Site2", "Variety_12", "Marriage", "Education1", "Occupation1", "Income1", "smoking", "spicy1", "BMI_G", "PA4", "digestivesd", "hypertension", "diabetes", "bl3")
interaction <- c("age5","sex", "Site1", "smoking", "bmi1", "bl3")  #hierarchical variable
res11 <- scitb5.coxph(data = P4, x = "HBS_G", y = "inci_U", time = "T_inci_U", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res11
res12 <- scitb5.coxph(data = P4, x = "HBS_G", y = "inci_E", time = "T_inci_E", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res12
res13 <- scitb5.coxph(data = P4, x = "HBS_G", y = "inci_S", time = "T_inci_S", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res13
res14 <- scitb5.coxph(data = P4, x = "LBS_G", y = "inci_U", time = "T_inci_U", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res14
res15 <- scitb5.coxph(data = P4, x = "LBS_G", y = "inci_E", time = "T_inci_E", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res15
res16 <- scitb5.coxph(data = P4, x = "LBS_G", y = "inci_S", time = "T_inci_S", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res16
res17 <- scitb5.coxph(data = P4, x = "DQD_G", y = "inci_U", time = "T_inci_U", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res17
res18 <- scitb5.coxph(data = P4, x = "DQD_G", y = "inci_E", time = "T_inci_E", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res18
res19 <- scitb5.coxph(data = P4, x = "DQD_G", y = "inci_S", time = "T_inci_S", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res19

cov1 <- c("age5","sex", "Site1", "Marriage", "Education1", "Occupation1", "Income1", "smoking", "drinking", "spicy", "BMI1", "PA41", "digestivesd", "hypertension", "diabetes", "bl3")
interaction <- c("age5","sex", "Site1", "smoking",  "bmi1", "bl3")  #hierarchical variable
res01 <- scitb5.coxph(data = P4, x = "tPDI_G", y = "inci_U", time = "T_inci_U", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res01
res02 <- scitb5.coxph(data = P4, x = "tPDI_G", y = "inci_E", time = "T_inci_E", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res02
res03 <- scitb5.coxph(data = P4, x = "tPDI_G", y = "inci_S", time = "T_inci_S", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res03
res04 <- scitb5.coxph(data = P4, x = "tuPDI_G", y = "inci_U", time = "T_inci_U", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res04
res05 <- scitb5.coxph(data = P4, x = "tuPDI_G", y = "inci_E", time = "T_inci_E", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res05
res06 <- scitb5.coxph(data = P4, x = "tuPDI_G", y = "inci_S", time = "T_inci_S", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res06
res07 <- scitb5.coxph(data = P4, x = "thPDI_G", y = "inci_U", time = "T_inci_U", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res07
res08 <- scitb5.coxph(data = P4, x = "thPDI_G", y = "inci_E", time = "T_inci_E", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res08
res09 <- scitb5.coxph(data = P4, x = "thPDI_G", y = "inci_S", time = "T_inci_S", Interaction = interaction, cov = cov1, family = "cox", dec = 2, pdec = 3, p.intervaue = 3, contain = F)
res09

##==========================================  Food group intake  ==============================================##
P4$Cereal_G <- as.factor(P4$Cereal_G)
P4$Meat_G <- as.factor(P4$Meat_G)
P4$Fish_G <- as.factor(P4$Fish_G)
P4$Egg_G <- as.factor(P4$Egg_G)
P4$Vege_G <- as.factor(P4$Vege_G)
P4$YFruit_G <- as.factor(P4$YFruit_G)
P4$Fruit_G <- as.factor(P4$Fruit_G)
P4$Soybean_G <- as.factor(P4$Soybean_G)
P4$Dairy_G <- as.factor(P4$Dairy_G)
P4$Salt_G <- as.factor(P4$Salt_G)
P4$Oil_G <- as.factor(P4$Oil_G)
P4$Alcohol_G <- as.factor(P4$Alcohol_G)



