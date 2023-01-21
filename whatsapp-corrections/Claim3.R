rm(list=ls())
dev.off()

library(foreign)
data<-read.csv("~/Dropbox/IndiaMisinformation_Sumitra/Data/Raw data (Qualtrics export 06:21)/ChauchardFlynn_India_rumors__MAIN_STUDY_27June.csv",stringsAsFactors = F)
dim(data)
names(data)
attach(data) #attaching dataset

### MMR (claim 3) ### 

#pure control (no thread)
MMRpcDummy<-ifelse(X3.pc1_HI==1|X3.pc2_HI==1|X3.pc3_HI==1|X3.pc4_HI==1|X3.pc5_HI==1|X3.pc6_HI==1|X3.pc7_HI==1|X3.pc8_HI==1|X3.pc9_HI==1, 1, 0)
table(MMRpcDummy)

#reattach pc to dataset
data$MMRpcDummy<-ifelse(X3.pc1_HI==1|X3.pc2_HI==1|X3.pc3_HI==1|X3.pc4_HI==1|X3.pc5_HI==1|X3.pc6_HI==1|X3.pc7_HI==1|X3.pc8_HI==1|X3.pc9_HI==1, 1, 0)
table(data$MMRpcDummy)

#control (misinfo, no correction)
MMRcontDummy<-ifelse(X3.1_HI==1|X3.1copy_HI==1|X3.1copy2_HI==1|X3.1copy3_HI==1|X3.1copy4_HI==1|X3.9_HI==1|X3.9copy_HI==1|X3.9copy2_HI==1|X3.9copy3_HI==1|X3.9copy4_HI==1|X3.17_HI==1|X3.17copy_HI==1|X3.17copy2_HI==1|X3.17copy3_HI==1|X3.17copy3_HI==1|X3.17copy4_HI==1, 1, 0)
table(MMRcontDummy)

#reattach control (misinfo, no correction) to dataset
data$MMRcontDummy<-ifelse(X3.1_HI==1|X3.1copy_HI==1|X3.1copy2_HI==1|X3.1copy3_HI==1|X3.1copy4_HI==1|X3.9_HI==1|X3.9copy_HI==1|X3.9copy2_HI==1|X3.9copy3_HI==1|X3.9copy4_HI==1|X3.17_HI==1|X3.17copy_HI==1|X3.17copy2_HI==1|X3.17copy3_HI==1|X3.17copy3_HI==1|X3.17copy4_HI==1, 1, 0)
table(data$MMRcontDummy)

#expert correction
MMRexpertDummy<-ifelse(X3.2_HI==1|X3.2copy_HI==1|X3.2copy2_HI==1|X3.2copy3_HI==1|X3.2copy4_HI==1|X3.10_HI==1|X3.10copy_HI==1|X3.10copy2_HI==1|X3.10copy3_HI==1|X3.10copy4_HI==1|X3.18_HI==1|X3.18copy_HI==1|X3.18copy2_HI==1|X3.18copy3_HI==1|X3.18copy3_HI==1|X3.18copy4_HI==1, 1, 0)
table(MMRexpertDummy)

#peer correction
MMRpeerDummy<-ifelse(X3.8_HI==1|X3.8copy_HI==1|X3.8copy2_HI==1|X3.8copy3_HI==1|X3.8_copy_4_HI==1|X3.16_HI==1|X3.16copy_HI==1|X3.16copy2_HI==1|X3.16copy3_HI==1|X3.16copy4_HI==1|X3.24_HI==1|X3.24copy_HI==1|X3.24copy2_HI==1|X3.24copy3_HI==1|X3.24copy3_HI==1|X3.24copy4_HI==1, 1, 0)
table(MMRpeerDummy)

#fact checker correction (any fact checker)
MMRfactcheckDummy<-ifelse(X3.3_HI==1|X3.4_HI==1|X3.5_HI==1|X3.6_HI==1|X3.7_HI==1|X3.11_HI==1|X3.12_HI==1|X3.13_HI==1|X3.14_HI==1|X3.15_HI==1|X3.19_HI==1|X3.20_HI==1|X3.21_HI==1|X3.22_HI==1|X3.23==1, 1, 0)
table(MMRfactcheckDummy)

#Altnews correction
MMRaltnewsDummy<-ifelse(X3.3_HI==1|X3.11_HI==1|X3.19_HI==1, 1, 0)
table(MMRaltnewsDummy)

#VishwasNews correction
MMRvishwasDummy<-ifelse(X3.4_HI==1|X3.12_HI==1|X3.20_HI==1, 1, 0)
table(MMRvishwasDummy)

#Times of India correction
MMRtoiDummy<-ifelse(X3.5_HI==1|X3.13_HI==1|X3.21_HI==1, 1, 0)
table(MMRtoiDummy)

#Facebook correction
MMRfbDummy<-ifelse(X3.6_HI==1|X3.14_HI==1|X3.22_HI==1, 1, 0)
table(MMRfbDummy)

#WhatsApp correction
MMRwhatsappDummy<-ifelse(X3.7_HI==1|X3.15_HI==1|X3.23==1, 1, 0)
table(MMRwhatsappDummy)

#Any sourced correction
MMRanysourcedDummy<-ifelse(MMRexpertDummy==1|MMRfactcheckDummy==1,1,0)
table(MMRanysourcedDummy)

#Any correction
MMRanycorrectionDummy<-ifelse(MMRexpertDummy==1|MMRpeerDummy==1|MMRfactcheckDummy==1,1,0)
table(MMRanycorrectionDummy)

# Note: no congenial/dissonant speaker treatment for this rumor

#Congenial broadcaster
MMRcongenialDummy<-ifelse(X3.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                            X3.2_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.3_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.4_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.5_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.6_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.7_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.8_HI==1 & close_party_HI_1%in%c(1,2) |
                            X3.17_HI==1 & close_party_HI_1%in%c(3,4)|
                            X3.18_HI==1 & close_party_HI_1%in%c(3,4)|
                            X3.19_HI==1 & close_party_HI_1%in%c(3,4)|
                            X3.20_HI==1 & close_party_HI_1%in%c(3,4)|
                            X3.21_HI==1 & close_party_HI_1%in%c(3,4)|
                            X3.22_HI==1 & close_party_HI_1%in%c(3,4)|
                            X3.23==1 & close_party_HI_1%in%c(3,4)|
                            X3.24_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(MMRcongenialDummy)

#Dissonant broadcaster
MMRdissonantDummy<-ifelse(X3.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                            X3.2_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.3_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.4_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.5_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.6_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.7_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.8_HI==1 & close_party_HI_1%in%c(3,4) |
                            X3.17_HI==1 & close_party_HI_1%in%c(1,2)|
                            X3.18_HI==1 & close_party_HI_1%in%c(1,2)|
                            X3.19_HI==1 & close_party_HI_1%in%c(1,2)|
                            X3.20_HI==1 & close_party_HI_1%in%c(1,2)|
                            X3.21_HI==1 & close_party_HI_1%in%c(1,2)|
                            X3.22_HI==1 & close_party_HI_1%in%c(1,2)|
                            X3.23==1 & close_party_HI_1%in%c(1,2)|
                            X3.24_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(MMRdissonantDummy)

#Neutral broadcaster
MMRneutralDummy<-ifelse(X3.9_HI==1 | X3.10_HI==1 | X3.11_HI==1 | X3.12_HI==1 | X3.13_HI==1 | X3.14_HI==1 | X3.15_HI==1 | X3.16_HI==1, 1, 0)
table(MMRneutralDummy)

#DV = dv_MMR (recode 1-4 so that higher=more belief in claim)
dv_MMR_new<-ifelse(dv_MMR==4, 4,
                   ifelse(dv_MMR==6, 3,
                          ifelse(dv_MMR==8, 2,
                                 ifelse(dv_MMR==10, 1, NA))))
table(dv_MMR_new)

#DV reattach to dataset
data$dv_MMR_new<-ifelse(dv_MMR==4, 4,
                   ifelse(dv_MMR==6, 3,
                          ifelse(dv_MMR==8, 2,
                                 ifelse(dv_MMR==10, 1, NA))))
table(data$dv_MMR_new)

# 10/13 results by hypothesis

claim3data <- data.frame(cbind(dv_MMR_new, MMRanycorrectionDummy, MMRcongenialDummy, 
                               MMRdissonantDummy, BJP, Congress, MMRpcDummy,
                               MMRpeerDummy, MMRexpertDummy, MMRaltnewsDummy, MMRvishwasDummy,
                               MMRtoiDummy, MMRfbDummy, MMRwhatsappDummy, MMRfactcheckDummy,
                               MMRcontDummy, MMRanysourcedDummy, age, male, education, data$ResponseID))


colnames(claim3data)[2] <- "AnyCorrection"
colnames(claim3data)[3] <- "CongenialMedia"
colnames(claim3data)[4] <- "DissonantMedia"
colnames(claim3data)[7] <- "PureControl"
colnames(claim3data)[8] <- "Peer"
colnames(claim3data)[9] <- "Expert"
colnames(claim3data)[10] <- "AltNews"
colnames(claim3data)[11] <- "Vishwas"
colnames(claim3data)[12] <- "TOI"
colnames(claim3data)[13] <- "Facebook"
colnames(claim3data)[14] <- "WhatsApp"
colnames(claim3data)[15] <- "AnyFactcheck"
colnames(claim3data)[16] <- "Control"
colnames(claim3data)[17] <- "AnySourced"
head(claim3data)
