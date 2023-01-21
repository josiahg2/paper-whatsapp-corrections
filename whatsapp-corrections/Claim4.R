# claim 4 coding and analysis 9/13

# coding gomutra (claim 4)

## Creating pure control dummy
GomutrapcDummy<-ifelse(X4.pc1_HI==1|X4.pc2_HI==1|X4.pc3_HI==1|X4.pc4_HI==1|X4.pc5_HI==1|X4.pc6_HI==1|X4.pc7_HI==1|X4.pc8_HI==1|X4.pc9_HI==1|X4.pc10_HI==1|X4.pc11_HI==1|X4.pc12_HI==1|X4.pc13_HI==1|X4.pc14_HI==1|X4.pc15_HI==1|X4.pc16_HI==1|X4.pc17_HI==1, 1, 0)
table(GomutrapcDummy)

## reattaching pc dummy to main data
data$GomutrapcDummy<-ifelse(X4.pc1_HI==1|X4.pc2_HI==1|X4.pc3_HI==1|X4.pc4_HI==1|X4.pc5_HI==1|X4.pc6_HI==1|X4.pc7_HI==1|X4.pc8_HI==1|X4.pc9_HI==1|X4.pc10_HI==1|X4.pc11_HI==1|X4.pc12_HI==1|X4.pc13_HI==1|X4.pc14_HI==1|X4.pc15_HI==1|X4.pc16_HI==1|X4.pc17_HI==1, 1, 0)
table(data$GomutrapcDummy)

## information, no control 4
data$GomutracontDummy<-ifelse(X4.1_HI==1 | X4.1copy_HI==1 | X4.1copy2_HI==1 | X4.1copy3_HI==1 | X4.1copy4_HI==1 |
                              X4.9_HI==1 | X4.9copy_HI==1 | X4.9copy2_HI==1 | X4.9copy3_HI==1 | X4.9copy4_HI==1 |
                              X4.17_HI==1| X4.17copy_HI==1| X4.17copy2_HI==1| X4.17copy3_HI==1| X4.17copy4_HI==1|
                              X4.25_HI==1| X4.25copy_HI==1| X4.25copy2_HI==1| X4.25copy3_HI==1| X4.25copy4HI==1 |  
                              X4.33_HI==1| X4.33copy_HI==1| X4.33copy2_HI==1| X4.33copy3_HI==1| X4.33copy4_HI==1|  
                              X4.41_HI==1| X4.41copy_HI==1| X4.41copy2_HI==1| X4.41copy3_HI==1| X4.41copy4_HI==1, 1, 0) 
table(data$GomutracontDummy)

## information, no control 4
GomutracontDummy<-ifelse(X4.1_HI==1 | X4.1copy_HI==1 | X4.1copy2_HI==1 | X4.1copy3_HI==1 | X4.1copy4_HI==1 |
                                X4.9_HI==1 | X4.9copy_HI==1 | X4.9copy2_HI==1 | X4.9copy3_HI==1 | X4.9copy4_HI==1 |
                                X4.17_HI==1| X4.17copy_HI==1| X4.17copy2_HI==1| X4.17copy3_HI==1| X4.17copy4_HI==1|
                                X4.25_HI==1| X4.25copy_HI==1| X4.25copy2_HI==1| X4.25copy3_HI==1| X4.25copy4HI==1 |  
                                X4.33_HI==1| X4.33copy_HI==1| X4.33copy2_HI==1| X4.33copy3_HI==1| X4.33copy4_HI==1|  
                                X4.41_HI==1| X4.41copy_HI==1| X4.41copy2_HI==1| X4.41copy3_HI==1| X4.41copy4_HI==1, 1, 0) 
table(GomutracontDummy)

# expert correction
GomutraexpertDummy<-ifelse(X4.2_HI==1 |X4.2copy_HI==1 |  X4.2copy2_HI==1 |  X4.2copy3_HI==1 |  X4.2copy4_HI==1 | 
                              X4.10_HI==1 | X4.10copy_HI==1 | X4.10copy2_HI==1 | X4.10copy3_HI==1 | X4.10copy4_HI==1 | 
                              X4.18_HI==1 | X4.18copy_HI==1 | X4.18copy2_HI==1 | X4.18copy3_HI==1 | X4.18copy4_HI==1 | 
                              X4.26_HI==1 | X4.26copy_HI==1 | X4.26copy2_HI==1 | X4.26copy3_HI==1 | X4.26copy4_HI==1 | 
                              X4.34_HI==1 | X4.34copy_HI==1 | X4.34copy2_HI==1 | X4.34copy3_HI==1 | X4.34copy4_HI==1 | 
                              X4.42_HI==1 | X4.42copy_HI==1 | X4.42copy2_HI==1 | X4.42copy3_HI==1 | X4.42copy4_HI==1, 1, 0)
table(GomutraexpertDummy)

# peer correction
GomutrapeerDummy <-ifelse(X4.8_HI==1| X4.8copy_HI==1| X4.8copy2_HI==1| X4.8copy3_HI==1| X4.8copy4_HI==1|
                            X4.16_HI==1|X4.16copy_HI==1|X4.16copy2_HI==1|X4.16copy3_HI==1|X4.16copy4_HI==1|
                            X4.24_HI==1|X4.24copy_HI==1|X4.24copy2_HI==1|X4.24copy3_HI==1|X4.24copy4_HI==1|
                            X4.32_HI==1|X4.32copy_HI==1|X4.32copy2_HI==1|X4.32copy3_HI==1|X4.32copy4_HI==1|
                            X4.40_HI==1|X4.40copy_HI==1|X4.40copy2_HI==1|X4.40copy3_HI==1|X4.40copy4_HI==1|
                            X4.48_HI==1|X4.48copy_HI==1|X4.48copy2_HI==1|X4.48copy3_HI==1|X4.48copy4_HI==1, 1, 0)
table(GomutrapeerDummy)

#fact checker correction (any fact checker) 
GomutrafactcheckDummy<-ifelse(X4.3_HI==1| X4.4_HI==1| X4.5_HI==1| X4.6_HI==1| X4.7_HI==1| X4.11_HI==1|X4.12_HI==1|
                                 X4.13_HI==1|X4.14_HI==1|X4.15_HI==1|X4.19_HI==1|X4.20_HI==1|X4.21_HI==1|X4.22_HI==1|
                                 X4.23_HI==1|X4.27_HI==1|X4.28_HI==1|X4.29_HI==1|X4.30_HI==1|X4.31_HI==1| 
                                 X4.35_HI==1|X4.36_HI==1|X4.37_HI==1|X4.38_HI==1|X4.39_HI==1|X4.43_HI==1| 
                                 X4.44_HI==1|X4.45_HI==1|X4.46_HI==1|X4.47_HI==1, 1, 0)
table(GomutrafactcheckDummy)


#Altnews correction
GomutraaltnewsDummy<-ifelse(X4.3_HI==1|X2.11_HI==1|X4.19_HI==1|X4.27_HI==1|X4.35_HI==1|X4.43_HI==1, 1, 0)
table(GomutraaltnewsDummy)

#VishwasNews correction
GomutravishwasDummy<-ifelse(X4.4_HI==1|X4.12_HI==1|X4.20_HI==1|X4.28_HI==1|X4.36_HI==1|X4.44_HI==1, 1, 0)
table(GomutravishwasDummy)

#Times of India correction
GomutratoiDummy<-ifelse(X4.5_HI==1|X4.13_HI==1|X4.21_HI==1|X4.29_HI==1|X4.37_HI==1|X4.45_HI==1, 1, 0)
table(GomutratoiDummy)

#Facebook correction
GomutrafbDummy<-ifelse(X4.6_HI==1|X4.14_HI==1|X4.22_HI==1|X4.30_HI==1|X4.38_HI==1|X4.46_HI==1, 1, 0)
table(GomutrafbDummy)

#WhatsApp correction
GomutrawhatsappDummy<-ifelse(X4.7_HI==1|X4.15_HI==1|X4.23_HI==1|X4.31_HI==1|X4.39_HI==1|X4.47_HI==1, 1, 0)
table(GomutrawhatsappDummy)

#Any sourced correction
GomutraanysourcedDummy<-ifelse(GomutraexpertDummy==1|GomutrafactcheckDummy==1,1,0)
table(GomutraanysourcedDummy)

#Any correction
GomutraanycorrectionDummy<-ifelse(GomutraexpertDummy==1|GomutrapeerDummy==1|GomutrafactcheckDummy==1,1,0)
table(GomutraanycorrectionDummy)

#Copartisan source/speaker
GomutracopartisanDummy<-  ifelse(X4.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                    X4.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X4.17_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.18_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.19_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.20_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.21_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.22_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.23_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.24_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                    X4.40_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(GomutracopartisanDummy)

#Outpartisan source/speaker
GomutraoutpartisanDummy<-ifelse(X4.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                   X4.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                   X4.17_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.18_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.19_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.20_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.21_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.22_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.23_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.24_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.40_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(GomutraoutpartisanDummy)

#Unidentified source/speaker
GomutraunidDummy<-ifelse(X4.9_HI==1 | X4.10_HI==1 | X4.11_HI==1 | X4.12_HI==1 | X4.13_HI==1 | 
                            X4.14_HI==1 |X4.15_HI==1 | X4.16_HI==1 | X4.25_HI==1 | X4.26_HI==1 | 
                            X4.27_HI==1 |X4.28_HI==1 | X4.29_HI==1 | X4.30_HI==1 | X4.31_HI==1 | 
                            X4.32_HI==1 |X4.41_HI==1 | X4.42_HI==1 | X4.43_HI==1 | X4.44_HI==1 | 
                            X4.45_HI==1 |X4.46_HI==1 | X4.47_HI==1 | X4.48_HI==1, 1, 0)
table(GomutraunidDummy)

#Congenial broadcaster
GomutracongenialDummy<-ifelse(X4.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                 X4.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X4.9_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.10_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.11_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.12_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.13_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.14_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.15_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.16_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X4.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.40_HI==1 & close_party_HI_1%in%c(3,4)| 
                                 X4.41_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.42_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.43_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.44_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.45_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.46_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.47_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X4.48_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(GomutracongenialDummy)

#Dissonant broadcasterX2
GomutradissonantDummy <- ifelse(X4.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                   X4.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X4.9_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.10_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.11_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.12_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.13_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.14_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.15_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.16_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X4.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.40_HI==1 & close_party_HI_1%in%c(1,2)| 
                                   X4.41_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.42_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.43_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.44_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.45_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.46_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.47_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X4.48_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(GomutradissonantDummy)

# 1 if claim is congenial w/ party ID
congenial_claim4 <- ifelse(close_party_HI_1%in%c(3,4), 1, 0)
table(congenial_claim4)

# 1 if claim is dissonant w/ party ID
dissonant_claim4 <- ifelse(close_party_HI_1%in%c(1,2), 1, 0)
table(dissonant_claim4)

#DV = dv_gomutra (recode 1-4 so that higher=more belief in claim)
dv_gomutra_new<-ifelse(dv_gomutra==4, 4,
                        ifelse(dv_gomutra==6, 3,
                               ifelse(dv_gomutra==8, 2,
                                      ifelse(dv_gomutra==10, 1, NA))))
table(dv_gomutra_new)

#DV reattaching to dataset
data$dv_gomutra_new<-ifelse(dv_gomutra==4, 4,
                       ifelse(dv_gomutra==6, 3,
                              ifelse(dv_gomutra==8, 2,
                                     ifelse(dv_gomutra==10, 1, NA))))
table(data$dv_gomutra_new)

# 10/12 results by hypothesis

claim4data <- data.frame(cbind(dv_gomutra_new, GomutraanycorrectionDummy, GomutraoutpartisanDummy,
                               GomutracopartisanDummy, GomutracongenialDummy, GomutradissonantDummy,
                               congenial_claim4, dissonant_claim4, BJP, Congress, GomutrapcDummy,
                               GomutrapeerDummy, GomutraexpertDummy, GomutraaltnewsDummy,
                                 GomutravishwasDummy, GomutratoiDummy, GomutrafbDummy, 
                               GomutrawhatsappDummy, GomutrafactcheckDummy, GomutracontDummy,
                               GomutraanysourcedDummy, hindu, leader, 
                               prayer, temple, age, male, education, data$ResponseID))
colnames(claim4data)[2] <- "AnyCorrection"
colnames(claim4data)[3] <- "OutpartisanSpeaker"
colnames(claim4data)[4] <- "CopartisanSpeaker"
colnames(claim4data)[5] <- "CongenialMedia"
colnames(claim4data)[6] <- "DissonantMedia"
colnames(claim4data)[7] <- "CongenialClaim"
colnames(claim4data)[8] <- "DissonantClaim"
colnames(claim4data)[11] <- "PureControl"
colnames(claim4data)[12] <- "Peer"
colnames(claim4data)[13] <- "Expert"
colnames(claim4data)[14] <- "AltNews"
colnames(claim4data)[15] <- "Vishwas"
colnames(claim4data)[16] <- "TOI"
colnames(claim4data)[17] <- "Facebook"
colnames(claim4data)[18] <- "WhatsApp"
colnames(claim4data)[19] <- "AnyFactcheck"
colnames(claim4data)[20] <- "Control"
colnames(claim4data)[21] <- "AnySourced"
head(claim4data)
