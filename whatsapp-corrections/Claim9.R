# Claim 9 coding & analysis 9/14

rm(list=ls())
dev.off()

library(foreign)
data<-read.csv("~/Dropbox/IndiaMisinformation_Sumitra/Data/Raw data (Qualtrics export 06:21)/ChauchardFlynn_India_rumors__MAIN_STUDY_27June.csv",stringsAsFactors = F)
dim(data)
names(data)
attach(data) 

# claim 9. Bose plane crash

## Creating pure control dummy
Bosepcdummy<-ifelse(X9.pc1_HI==1|X9.pc2_HI==1|X9.pc3_HI==1|X9.pc4_HI==1|X9.pc5_HI==1|X9.pc6_HI==1|X9.pc7_HI==1|X9.pc8_HI==1|X9.pc9_HI==1, 1, 0)
table(Bosepcdummy)

# reattach pure control to dataset
data$Bosepcdummy<-ifelse(X9.pc1_HI==1|X9.pc2_HI==1|X9.pc3_HI==1|X9.pc4_HI==1|X9.pc5_HI==1|X9.pc6_HI==1|X9.pc7_HI==1|X9.pc8_HI==1|X9.pc9_HI==1, 1, 0)
table(data$Bosepcdummy)

#expert correction
BoseexpertDummy<-ifelse(X9.2_HI==1| X9.2copy_HI==1| X9.2copy2_HI==1| X9.2copy3_HI==1| X9.2copy4_HI==1|
                       X9.10_HI==1|
                       X9.18_HI==1|X9.18copy_HI==1|X9.18copy2_HI==1|X9.18copy3_HI==1|X9.18copy4_HI==1, 1, 0)
table(BoseexpertDummy)

# misinformnation, no correction (control) dummy
data$Bosecontdummy  <- ifelse(X9.1_HI==1 | X9.1copy_HI==1 | X9.1copy2_HI==1 | X9.1copy3_HI==1 | X9.1copy4_HI==1 |
                              X9.9_HI==1 | X9.9copy_HI==1 | X9.9copy2_HI==1 | X9.9copy3_HI==1 | X9.9copy4_HI==1 |
                              X9.17_HI==1| X9.17copy_HI==1| X9.17copy2_HI==1| X9.17copy3_HI==1| X9.17copy4_HI==1, 1, 0) 
table(data$Bosecontdummy)

Bosecontdummy  <- ifelse(X9.1_HI==1 | X9.1copy_HI==1 | X9.1copy2_HI==1 | X9.1copy3_HI==1 | X9.1copy4_HI==1 |
                                X9.9_HI==1 | X9.9copy_HI==1 | X9.9copy2_HI==1 | X9.9copy3_HI==1 | X9.9copy4_HI==1 |
                                X9.17_HI==1| X9.17copy_HI==1| X9.17copy2_HI==1| X9.17copy3_HI==1| X9.17copy4_HI==1, 1, 0) 
table(Bosecontdummy)

#peer correction
BosepeerDummy<-ifelse(X9.8_HI==1| X9.8copy_HI==1| X9.8copy2_HI==1| X9.8copy3_HI==1| X9.8copy4_HI==1|
                      X9.16_HI==1|X9.16copy_HI==1|X9.16copy2_HI==1|X9.16copy3_HI==1|X9.16copy4_HI==1|
                      X9.24_HI==1|X9.24copy_HI==1|X9.24copy2_HI==1|X9.24copy3_HI==1|X9.24copy3_HI==1|X9.24copy4_HI==1, 1, 0)
table(BosepeerDummy)

#fact checker correction (any fact checker)
BosefactcheckDummy<-ifelse(X9.3_HI==1| X9.4_HI==1| X9.5_HI==1| X9.6_HI==1| X9.7_HI==1| X9.11_HI==1|
                          X9.12_HI==1|X9.13_HI==1|X9.14_HI==1|X9.15_HI==1|X9.19_HI==1|X9.20_HI==1|
                          X9.21_HI==1|X9.22_HI==1|X9.23_HI==1, 1, 0)
table(BosefactcheckDummy)

#Altnews correction
BosealtnewsDummy<-ifelse(X9.3_HI==1|X9.11_HI==1|X9.19_HI==1, 1, 0)
table(BosealtnewsDummy)

#VishwasNews correction
BosevishwasDummy<-ifelse(X9.4_HI==1|X9.12_HI==1|X9.20_HI==1, 1, 0)
table(BosevishwasDummy)

#Times of India correction
BosetoiDummy<-ifelse(X9.5_HI==1|X9.13_HI==1|X9.21_HI==1, 1, 0)
table(BosetoiDummy)

#Facebook correction
BosefbDummy<-ifelse(X9.6_HI==1|X9.14_HI==1|X9.22_HI==1, 1, 0)
table(BosefbDummy)

#WhatsApp correction
BosewhatsappDummy<-ifelse(X9.7_HI==1|X9.15_HI==1|X9.23_HI==1, 1, 0)
table(BosewhatsappDummy)

#Any sourced correction
BoseanysourcedDummy<-ifelse(BoseexpertDummy==1|BosefactcheckDummy==1,1,0)
table(BoseanysourcedDummy)

#Any correction
BoseanycorrectionDummy<-ifelse(BoseexpertDummy==1|BosepeerDummy==1|BosefactcheckDummy==1,1,0)
table(BoseanycorrectionDummy)

#Congenial broadcaster
BosecongenialDummy<-ifelse( X9.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                            X9.2_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.3_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.4_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.5_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.6_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.7_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.8_HI==1 & close_party_HI_1%in%c(1,2) |
                            X9.17_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.18_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.19_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.20_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.21_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.22_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.23_HI==1 & close_party_HI_1%in%c(3,4)|
                            X9.24_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(BosecongenialDummy)

#Dissonant broadcaster
BosedissonantDummy<-ifelse( X9.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                            X9.2_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.3_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.4_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.5_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.6_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.7_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.8_HI==1 & close_party_HI_1%in%c(3,4) |
                            X9.17_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.18_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.19_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.20_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.21_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.22_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.23_HI==1 & close_party_HI_1%in%c(1,2)|
                            X9.24_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(BosedissonantDummy)

#Neutral broadcaster
BoseneutralDummy<-ifelse(X9.9_HI==1 | X9.10_HI==1 | X9.11_HI==1 | X9.12_HI==1 | 
                           X9.13_HI==1 | X9.14_HI==1 | X9.15_HI==1 | X9.16_HI==1, 1, 0)
table(BoseneutralDummy)

#DV = dv_Bose (recode 1-4 so that higher=more belief in claim)
dv_bose_new<-ifelse(dv_Bose==4, 4,
                   ifelse(dv_Bose==6, 3,
                          ifelse(dv_Bose==8, 2,
                                 ifelse(dv_Bose==10, 1, NA))))
table(dv_bose_new)

#DV reattach to dataset
data$dv_bose_new<-ifelse(dv_Bose==4, 4,
                    ifelse(dv_Bose==6, 3,
                           ifelse(dv_Bose==8, 2,
                                  ifelse(dv_Bose==10, 1, NA))))
table(data$dv_bose_new)


# 10/13 results by hypothesis

claim9data <- data.frame(cbind(dv_bose_new, BoseanycorrectionDummy,
                               BosecongenialDummy, BosedissonantDummy, BJP, Congress, Bosepcdummy,
                               BosepeerDummy, BoseexpertDummy, BosealtnewsDummy, BosevishwasDummy,
                               BosetoiDummy, BosefbDummy, BosewhatsappDummy, BosefactcheckDummy, 
                               Bosecontdummy, BoseanysourcedDummy, male, age, education,
                               data$ResponseID))

colnames(claim9data)[2] <- "AnyCorrection"
colnames(claim9data)[3] <- "CongenialMedia"
colnames(claim9data)[4] <- "DissonantMedia"
colnames(claim9data)[7] <- "PureControl"
colnames(claim9data)[8] <- "Peer"
colnames(claim9data)[9] <- "Expert"
colnames(claim9data)[10] <- "AltNews"
colnames(claim9data)[11] <- "Vishwas"
colnames(claim9data)[12] <- "TOI"
colnames(claim9data)[13] <- "Facebook"
colnames(claim9data)[14] <- "WhatsApp"
colnames(claim9data)[15] <- "AnyFactcheck"
colnames(claim9data)[16] <- "Control"
colnames(claim9data)[17] <- "AnySourced"
head(claim9data)

