# claim 1 coding and anaysis 9/12

rm(list=ls())
dev.off()

library(foreign)
data<-read.csv("~/Dropbox/IndiaMisinformation_Sumitra/Data/Raw data (Qualtrics export 06:21)/ChauchardFlynn_India_rumors__MAIN_STUDY_27June.csv",stringsAsFactors = F)
attach(data) 

# coding muslim demography (claim 1)

## Creating pure control dummy
MuslimpcDummy<-ifelse(X1.pc1_HI==1|X1.pc2_HI==1|X1.pc3_HI==1|X1.pc4_HI==1|X1.pc5_HI==1|X1.pc6_HI==1|X1.pc7_HI==1|X1.pc8_HI==1|X1.pc9_HI==1|X1.pc10_HI==1|X1.pc11_HI==1|
                        X1.pc12HI==1|X1.pc13_HI==1|X1.pc14_HI==1|X1.pc15_HI==1|X1.pc16_HI==1|X1.pc17_HI==1, 1, 0)
table(MuslimpcDummy)


# reattaching pure control to main dataset
data$MuslimpcDummy<-ifelse(X1.pc1_HI==1|X1.pc2_HI==1|X1.pc3_HI==1|X1.pc4_HI==1|X1.pc5_HI==1|X1.pc6_HI==1|X1.pc7_HI==1|X1.pc8_HI==1|X1.pc9_HI==1|X1.pc10_HI==1|X1.pc11_HI==1|
                             X1.pc12HI==1|X1.pc13_HI==1|X1.pc14_HI==1|X1.pc15_HI==1|X1.pc16_HI==1|X1.pc17_HI==1, 1, 0)
table(data$MuslimpcDummy)

# misinformnation, no correction (control) dummy
MuslimcontDummy<-ifelse(X1.1_HI==1|X1.1copy_HI==1|X1.1copy2_HI==1|X1.1copy3_HI==1|X1.1copy4_HI==1|
                          X1.9_HI==1|X1.9copy_HI==1|X1.9copy2_HI==1|X1.9copy3_HI==1|X1.9copy4_HI==1|
                          X1.17_HI==1|X1.17copy_HI==1|X1.17copy2_HI==1|X1.17copy3_HI==1|X1.17copy4_HI==1|
                          X1.25_HI==1|X1.25copy_HI==1|X1.25copy2_HI==1|X1.25copy3_HI==1|X1.25copy4_HI==1|
                          X1.33_HI==1|X1.33copy_HI==1|X1.33copy2_HI==1|X1.33copy3_HI==1|X1.33copy4_HI==1|
                          X1.41_HI==1|X1.41copy_HI==1|X1.41copy2_HI==1|X1.41copy3_HI==1|X1.41copy4_HI==1, 1, 0)
table(MuslimcontDummy)

# reattaching no correction control to main data
data$MuslimcontDummy<-ifelse(X1.1_HI==1|X1.1copy_HI==1|X1.1copy2_HI==1|X1.1copy3_HI==1|X1.1copy4_HI==1|
                               X1.9_HI==1|X1.9copy_HI==1|X1.9copy2_HI==1|X1.9copy3_HI==1|X1.9copy4_HI==1|
                               X1.17_HI==1|X1.17copy_HI==1|X1.17copy2_HI==1|X1.17copy3_HI==1|X1.17copy4_HI==1|
                               X1.25_HI==1|X1.25copy_HI==1|X1.25copy2_HI==1|X1.25copy3_HI==1|X1.25copy4_HI==1|
                               X1.33_HI==1|X1.33copy_HI==1|X1.33copy2_HI==1|X1.33copy3_HI==1|X1.33copy4_HI==1|
                               X1.41_HI==1|X1.41copy_HI==1|X1.41copy2_HI==1|X1.41copy3_HI==1|X1.41copy4_HI==1, 1, 0)
table(data$MuslimcontDummy)

# expert correction
MuslimexpertDummy<-ifelse(X1.2_HI==1 | X1.2copy_HI==1 | X1.2copy2_HI==1 | X1.2copy3_HI==1 | X1.2copy4_HI==1 | 
                            X1.10_HI==1 | X1.10copy_HI==1 | X1.10copy2_HI==1 | X1.10copy3_HI==1 | X1.10copy4_HI==1 | 
                            X1.18_HI==1 | X1.18copy_HI==1 | X1.18copy2_HI==1 | X1.18copy3_HI==1 | X1.18copy4_HI==1 | 
                            X1.26_HI==1 | X1.26copy_HI==1 | X1.26copy2_HI==1 | X1.26copy3_HI==1 | X1.26copy4_HI==1 | 
                            X1.34_HI==1 | X1.34copy_HI==1 | X1.34copy2_HI==1 | X1.34copy3_HI==1 | X1.34.copy.4==1 | 
                            X1.42_HI==1 | X1.42copy_HI==1 | X1.42copy2_HI==1 | X1.42copy3_HI==1 | X1.42copy4_HI==1, 1, 0)
table(MuslimexpertDummy)

# peer correction
MuslimpeerDummy<-ifelse(X1.8_HI==1|X1.8copy_HI==1|X1.8copy2_HI==1|X1.8copy3_HI==1|X1.8copy4_HI==1|
                          X1.16_HI==1|X1.16copy_HI==1|X1.16copy2_HI==1|X1.16copy3_HI==1|X1.16copy4_HI==1|
                          X1.24_HI==1|X1.24copy_HI==1|X1.24copy2_HI==1|X1.24copy3_HI==1|X1.24copy4_HI==1|
                          X1.32_HI==1|X1.32copy_HI==1|X1.32copy2_HI==1|X1.32copy3_HI==1|X1.32copy4_HI==1|
                          X1.40_HI==1|X1.40copy_HI==1|X1.40copy2_HI==1|X1.40copy3_HI==1|X1.40copy4_HI==1|
                          X1.48_HI==1|X1.48copy_HI==1|X1.48copy2_HI==1|X1.48copy3_HI==1|X1.48copy4_HI==1, 1, 0)
table(MuslimpeerDummy)

#fact checker correction (any fact checker) 
MuslimfactcheckDummy<-ifelse(X1.3_HI==1|X1.4_HI==1|X1.5_HI==1|X1.6_HI==1|X1.7_HI==1|X1.11_HI==1|X1.12_HI==1|
                               X1.13_HI==1|X1.14_HI==1|X1.15_HI==1|X1.19_HI==1|X1.20_HI==1|X1.21_HI==1|X1.22_HI==1|
                               X1.23_HI==1|X1.27_HI==1|X1.28_HI==1|X1.29_HI==1|X1.30_HI==1|X1.31_HI==1| 
                               X1.35_HI==1|X1.36_HI==1|X1.37_HI==1|X1.38_HI==1|X1.39_HI==1|X1.43_HI==1| 
                               X1.44_HI==1|X1.45_HI==1|X1.46_HI==1|X1.47_HI==1, 1, 0)
table(MuslimfactcheckDummy)

#Altnews correction
MuslimaltnewsDummy<-ifelse(X1.3_HI==1|X1.11_HI==1|X1.19_HI==1|X1.27_HI==1|X1.35_HI==1|X1.43_HI==1, 1, 0)
table(MuslimaltnewsDummy)

#VishwasNews correction
MuslimvishwasDummy<-ifelse(X1.4_HI==1|X1.12_HI==1|X1.20_HI==1|X1.28_HI==1|X1.36_HI==1|X1.44_HI==1, 1, 0)
table(MuslimvishwasDummy)

#Times of India correction
MuslimtoiDummy<-ifelse(X1.5_HI==1|X1.13_HI==1|X1.21_HI==1|X1.29_HI==1|X1.37_HI==1|X1.45_HI==1, 1, 0)
table(MuslimtoiDummy)

#Facebook correction
MuslimfbDummy<-ifelse(X1.6_HI==1|X1.14_HI==1|X1.22_HI==1|X1.30_HI==1|X1.38_HI==1|X1.46_HI==1, 1, 0)
table(MuslimfbDummy)

#WhatsApp correction
MuslimwhatsappDummy<-ifelse(X1.7_HI==1|X1.15_HI==1|X1.23_HI==1|X1.31_HI==1|X1.39_HI==1|X1.47_HI==1, 1, 0)
table(MuslimwhatsappDummy)

#Any correction
MuslimanycorrectionDummy<-ifelse(MuslimexpertDummy==1|MuslimpeerDummy==1|MuslimfactcheckDummy==1,1,0)
table(MuslimanycorrectionDummy)

#Any sources correction
MuslimanysourcedDummy<-ifelse(MuslimexpertDummy==1|MuslimfactcheckDummy==1,1,0)
table(MuslimanysourcedDummy)


#Copartisan source/speaker
MuslimcopartisanDummy<-ifelse(X1.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                X1.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                X1.17_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.18_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.19_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.20_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.21_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.22_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.23_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.24_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                X1.40_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(MuslimcopartisanDummy)

#Outpartisan source/speaker
MuslimoutpartisanDummy<-ifelse(X1.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                 X1.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X1.17_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.18_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.19_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.20_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.21_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.22_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.23_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.24_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.40_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(MuslimoutpartisanDummy)

#Unidentified source/speaker
MuslimunidDummy<-ifelse(X1.9_HI==1 | X1.10_HI==1 | X1.11_HI==1 | X1.12_HI==1 | X1.13_HI==1 | 
                          X1.14_HI==1 |X1.15_HI==1 | X1.16_HI==1 | X1.25_HI==1 | X1.26_HI==1 | 
                          X1.27_HI==1 |X1.28_HI==1 | X1.29_HI==1 | X1.30_HI==1 | X1.31_HI==1 | 
                          X1.32_HI==1 |X1.41_HI==1 | X1.42_HI==1 | X1.43_HI==1 | X1.44_HI==1 | 
                          X1.45_HI==1 |X1.46_HI==1 | X1.47_HI==1 | X1.48_HI==1, 1, 0)
table(MuslimunidDummy)

#Congenial broadcaster
MuslimcongenialDummy<-ifelse(X1.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                               X1.2_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.3_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.4_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.5_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.6_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.7_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.8_HI==1 & close_party_HI_1%in%c(1,2) |
                               X1.9_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.10_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.11_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.12_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.13_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.14_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.15_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.16_HI==1 & close_party_HI_1%in%c(1,2)|
                               X1.33_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.34_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.35_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.36_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.37_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.38_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.39_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.40_HI==1 & close_party_HI_1%in%c(3,4)| 
                               X1.41_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.42_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.43_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.44_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.45_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.46_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.47_HI==1 & close_party_HI_1%in%c(3,4)|
                               X1.48_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(MuslimcongenialDummy)

#Dissonant broadcaster
MuslimdissonantDummy <- ifelse(X1.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                 X1.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X1.9_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.10_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.11_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.12_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.13_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.14_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.15_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.16_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X1.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.40_HI==1 & close_party_HI_1%in%c(1,2)| 
                                 X1.41_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.42_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.43_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.44_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.45_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.46_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.47_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X1.48_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(MuslimdissonantDummy)

# congenial and dissonant claims

# 1 if claim is congenial w/ party ID
congenial_claim1 <- ifelse(close_party_HI_1%in%c(3,4), 1, 0)
table(congenial_claim1)

# 1 if claim is dissonant w/ party ID
dissonant_claim1 <- ifelse(close_party_HI_1%in%c(1,2), 1, 0)
table(dissonant_claim1)

#DV = dv_UNESCO (recode 1-4 so that higher=more belief in claim)
data$dv_muslimpop_new<-ifelse(dv_muslimpop==4, 4,
                         ifelse(dv_muslimpop==6, 3,
                                ifelse(dv_muslimpop==8, 2,
                                       ifelse(dv_muslimpop==10, 1, NA))))
table(data$dv_muslimpop_new)

dv_muslimpop_new<-ifelse(dv_muslimpop==4, 4,
                              ifelse(dv_muslimpop==6, 3,
                                     ifelse(dv_muslimpop==8, 2,
                                            ifelse(dv_muslimpop==10, 1, NA))))

# Coding party ID
data$X2019party <- as.numeric(data$X2019party)
table(data$X2019party)
# BJP = 78, Congress = 79
# Dummy for BJP vote
BJP <- ifelse(data$X2019party==78, 1, 0)
table(BJP)

Congress <- ifelse(data$X2019party==79, 1, 0)
table(Congress)

# coding hindu religion 
table(data$religion_HI) #1 = hindu
data$religion_HI <- as.numeric(data$religion_HI)
table(data$religion_HI)

hindu <- ifelse(data$religion_HI==1, 1, 0)
table(hindu)

# frequency of prayer
table(data$religiousact_HI_1) #1 = daily
prayer <- as.numeric(data$religiousact_HI_1)
table(prayer)

# freqency of visit to temple, mosque etc
table(data$religiousact_HI_2) #1 = daily
temple <- as.numeric(data$religiousact_HI_2)
table(temple)

# religious leader: do you follow any guru, religious leader
table(data$religiousleader_HI) # 1 = yes
data$religiousleader_HI <- as.numeric(data$religiousleader_HI)
leader <- ifelse(data$religiousleader_HI==1, 1, 0)

# age
data$age_HI <- as.numeric(data$age_HI)
age <- subset(data$age_HI, data$age_HI < 80)
table(age)

# gender
male <- NA
male[data$sex_HI==6] <- 1
male[data$sex_HI==9 | data$sex_HI==10] <- 0
table(male)


# education
# new variable for education
table(data$edu_HI)# drop 6, keep graduate and no grad 
data$edu_HI <- as.numeric(data$edu_HI)
education <- NA
education[data$edu_HI==1] <- 1
education[data$edu_HI==7] <- 2
education[data$edu_HI==10] <- 3
education[data$edu_HI==11] <- 4
education[data$edu_HI==12] <- 5
education[data$edu_HI==3] <- 6
education[data$edu_HI==4] <- 7
education[data$edu_HI==5] <- 8
table(education)

length(education)
summary(education)
sd(education, na.rm=T)

head(data$ResponseID)

# 1 if claim is congenial , -1 if not (so we can code 0 as claims that are neutral)
congenial_claim1new <- ifelse(close_party_HI_1%in%c(3,4), 1, -1)
table(congenial_claim1new)

# 1 if claim is dissonant w/ party ID
dissonant_claim1 <- ifelse(close_party_HI_1%in%c(1,2), 1, 0)
table(dissonant_claim1)


# creating new dataset of needed dummies for standardization
claim1data <- data.frame(cbind(dv_muslimpop_new, MuslimanycorrectionDummy, MuslimoutpartisanDummy,
                               MuslimcopartisanDummy,MuslimcongenialDummy,MuslimdissonantDummy, 
                               congenial_claim1, dissonant_claim1, BJP, Congress, MuslimpcDummy,
                               MuslimpeerDummy, MuslimexpertDummy, MuslimaltnewsDummy, MuslimvishwasDummy,
                               MuslimtoiDummy, MuslimfbDummy, MuslimwhatsappDummy, MuslimfactcheckDummy,
                               MuslimcontDummy, MuslimanysourcedDummy, hindu, leader, 
                               prayer, temple, age, male, education, data$ResponseID))
colnames(claim1data)[2] <- "AnyCorrection"
colnames(claim1data)[3] <- "OutpartisanSpeaker"
colnames(claim1data)[4] <- "CopartisanSpeaker"
colnames(claim1data)[5] <- "CongenialMedia"
colnames(claim1data)[6] <- "DissonantMedia"
colnames(claim1data)[7] <- "CongenialClaim"
colnames(claim1data)[8] <- "DissonantClaim"
colnames(claim1data)[11] <- "PureControl"
colnames(claim1data)[12] <- "Peer"
colnames(claim1data)[13] <- "Expert"
colnames(claim1data)[14] <- "AltNews"
colnames(claim1data)[15] <- "Vishwas"
colnames(claim1data)[16] <- "TOI"
colnames(claim1data)[17] <- "Facebook"
colnames(claim1data)[18] <- "WhatsApp"
colnames(claim1data)[19] <- "AnyFactcheck"
colnames(claim1data)[20] <- "Control"
colnames(claim1data)[21] <- "AnySourced"
head(claim1data)
names(claim1data)

write.csv(claim1data, "~/Dropbox/IndiaMisinformation_Sumitra/IJPP_Submission/claim1.csv")


