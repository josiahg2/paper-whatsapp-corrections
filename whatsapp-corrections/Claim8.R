### UNENSCO Modi (claim 8) ###

#pure control (no thread)
UNESCOpcDummy<-ifelse(X8.pc1_HI==1|X8.pc2_HI==1|X8.pc3_HI==1|X8.pc4_HI==1|X8.pc5_HI==1|X8.pc6_HI==1|X8.pc7_HI==1|X8.pc8_HI==1|X8.pc9_HI==1, 1, 0)
table(UNESCOpcDummy)

# reattaching pure control (no thread) to dataset
data$UNESCOpcDummy<-ifelse(X8.pc1_HI==1|X8.pc2_HI==1|X8.pc3_HI==1|X8.pc4_HI==1|X8.pc5_HI==1|X8.pc6_HI==1|X8.pc7_HI==1|X8.pc8_HI==1|X8.pc9_HI==1, 1, 0)
table(data$UNESCOpcDummy)

#control (misinfo, no correction)
UNESCOcontDummy<-ifelse(X8.1_HI==1|X8.1copy_HI==1|X8.1copy2_HI==1|X8.1copy3_HI==1|X8.1copy4_HI==1|X8.9_HI==1|X8.9copy_HI==1|X8.9copy2_HI==1|X8.9copy3_HI==1|X8.9copy4_HI==1|X8.17_HI==1|X8.17copy_HI==1|X8.17copy2_HI==1|X8.17copy3_HI==1|X8.17copy3_HI==1|X8.25_HI==1|X8.25copy_HI==1|X8.25copy2_HI==1|X8.25copy3_HI==1|X8.25copy4_HI==1|X8.33_HI==1|X8.33copy_HI==1|X8.33copy2_HI==1|X8.33copy3_HI==1|X8.33copy4_HI==1|X8.41_HI==1|X8.41copy_HI==1|X8.41copy2_HI==1|X8.41copy3_HI==1|X8.41copy4_HI==1, 1, 0)
table(UNESCOcontDummy)

#reattaching control (misinfo, no correction) to dataset
data$UNESCOcontDummy<-ifelse(X8.1_HI==1|X8.1copy_HI==1|X8.1copy2_HI==1|X8.1copy3_HI==1|X8.1copy4_HI==1|X8.9_HI==1|X8.9copy_HI==1|X8.9copy2_HI==1|X8.9copy3_HI==1|X8.9copy4_HI==1|X8.17_HI==1|X8.17copy_HI==1|X8.17copy2_HI==1|X8.17copy3_HI==1|X8.17copy3_HI==1|X8.25_HI==1|X8.25copy_HI==1|X8.25copy2_HI==1|X8.25copy3_HI==1|X8.25copy4_HI==1|X8.33_HI==1|X8.33copy_HI==1|X8.33copy2_HI==1|X8.33copy3_HI==1|X8.33copy4_HI==1|X8.41_HI==1|X8.41copy_HI==1|X8.41copy2_HI==1|X8.41copy3_HI==1|X8.41copy4_HI==1, 1, 0)
table(data$UNESCOcontDummy)

#expert correction
UNESCOexpertDummy<-ifelse(X8.2_HI==1 | X8.2copy_HI==1 | X8.2copy2_HI==1 | X8.2copy3_HI==1 | X8.2copy4_HI==1 | X8.10_HI==1 | X8.10copy_HI==1 | X8.10copy2_HI==1 | X8.10copy3_HI==1 | X8.10copy4_HI==1 | X8.18_HI==1 | X8.18copy_HI==1 | X8.18copy2_HI==1 | X8.18copy3_HI==1 | X8.18copy3_HI==1 | X8.18copy4_HI==1 | X8.26_HI==1 | X8.26copy_HI==1 | X8.26copy2_HI==1 | X8.26copy3_HI==1 | X8.26copy4_HI==1 | X8.34_HI==1 | X8.34copy_HI==1 | X8.34copy2_HI==1 | X8.34copy3_HI==1 | X8.34copy4_HI==1 | X8.42_HI==1 | X8.42copy_HI==1 | X8.42copy2_HI==1 | X8.42copy3_HI==1 | X8.42copy4_HI==1, 1, 0)
table(UNESCOexpertDummy)

#peer correction
UNESCOpeerDummy<-ifelse(X8.8_HI==1|X8.8copy_HI==1|X8.8copy2_HI==1|X8.8copy3_HI==1|X8.8copy4_HI==1|X8.16_HI==1|X8.16copy_HI==1|X8.16copy2_HI==1|X8.16copy3_HI==1|X8.16copy4_HI==1|X8.24_HI==1|X8.24copy_HI==1|X8.24copy2_HI==1|X8.24copy3_HI==1|X8.24copy3_HI==1|X8.24copy4_HI==1|X8.32_HI==1|X8.32copy_HI==1|X8.32copy2_HI==1|X8.32copy3_HI==1|X8.32copy4_HI==1|X8.40_HI==1|X8.40copy_HI==1|X8.40copy2_HI==1|X8.40copy3_HI==1|X8.40copy4_HI==1|X8.48_HI==1|X8.48copy_HI==1|X8.48copy2_HI==1|X8.48copy3_HI==1|X8.48copy4_HI==1, 1, 0)
table(UNESCOpeerDummy)

#fact checker correction (any fact checker) 
UNESCOfactcheckDummy<-ifelse(X8.3_HI==1|X8.4_HI==1|X8.5_HI==1|X8.6_HI==1|X8.7_HI==1|X8.11_HI==1|X8.12_HI==1|X8.13_HI==1|X8.14_HI==1|X8.15_HI==1|X8.19_HI==1|X8.20_HI==1|X8.21_HI==1|X8.22_HI==1|X8.23_HI==1|
                               X8.27_HI==1| X8.28_HI==1| X8.29_HI==1| X8.30_HI==1| X8.31_HI==1| X8.35_HI==1| X8.36_HI==1| X8.37_HI==1| X8.38_HI==1| X8.39_HI==1| X8.43_HI==1| X8.44_HI==1| X8.45_HI==1| X8.46_HI==1| X8.47_HI==1, 1, 0)
table(UNESCOfactcheckDummy)

#Altnews correction
UNESCOaltnewsDummy<-ifelse(X8.3_HI==1|X8.11_HI==1|X8.19_HI==1|X8.27_HI==1|X8.35_HI==1|X8.43_HI==1, 1, 0)
table(UNESCOaltnewsDummy)

#VishwasNews correction
UNESCOvishwasDummy<-ifelse(X8.4_HI==1|X8.12_HI==1|X8.20_HI==1|X8.28_HI==1|X8.36_HI==1|X8.44_HI==1, 1, 0)
table(UNESCOvishwasDummy)

#Times of India correction
UNESCOtoiDummy<-ifelse(X8.5_HI==1|X8.13_HI==1|X8.21_HI==1|X8.29_HI==1|X8.37_HI==1|X8.45_HI==1, 1, 0)
table(UNESCOtoiDummy)

#Facebook correction
UNESCOfbDummy<-ifelse(X8.6_HI==1|X8.14_HI==1|X8.22_HI==1|X8.30_HI==1|X8.38_HI==1|X8.46_HI==1, 1, 0)
table(UNESCOfbDummy)

#WhatsApp correction
UNESCOwhatsappDummy<-ifelse(X8.7_HI==1|X8.15_HI==1|X8.23_HI==1|X8.31_HI==1|X8.39_HI==1|X8.47_HI==1, 1, 0)
table(UNESCOwhatsappDummy)

#Any sourced correction
UNESCOanysourcedDummy<-ifelse(UNESCOexpertDummy==1|UNESCOfactcheckDummy==1,1,0)
table(UNESCOanysourcedDummy)

#Any correction
UNESCOanycorrectionDummy<-ifelse(UNESCOexpertDummy==1|UNESCOpeerDummy==1|UNESCOfactcheckDummy==1,1,0)
table(UNESCOanycorrectionDummy)

#Copartisan source/speaker
UNESCOcopartisanDummy<-ifelse(X8.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                X8.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                X8.17_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.18_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.19_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.20_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.21_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.22_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.23_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.24_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                X8.40_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(UNESCOcopartisanDummy)

#Outpartisan source/speaker
UNESCOoutpartisanDummy<-ifelse(X8.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                 X8.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                 X8.17_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.18_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.19_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.20_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.21_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.22_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.23_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.24_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X8.40_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(UNESCOoutpartisanDummy)

#Unidentified source/speaker
UNESCOunidDummy<-ifelse(X8.9_HI==1 | 
                          X8.10_HI==1 | 
                          X8.11_HI==1 | 
                          X8.12_HI==1 | 
                          X8.13_HI==1 | 
                          X8.14_HI==1 | 
                          X8.15_HI==1 | 
                          X8.16_HI==1 | 
                          X8.25_HI==1 | 
                          X8.26_HI==1 | 
                          X8.27_HI==1 | 
                          X8.28_HI==1 | 
                          X8.29_HI==1 | 
                          X8.30_HI==1 | 
                          X8.31_HI==1 | 
                          X8.32_HI==1 | 
                          X8.41_HI==1 | 
                          X8.42_HI==1 | 
                          X8.43_HI==1 | 
                          X8.44_HI==1 | 
                          X8.45_HI==1 | 
                          X8.46_HI==1|
                          X8.47_HI==1 |
                          X8.48_HI==1, 1, 0)
table(UNESCOunidDummy)

#Congenial broadcaster
UNESCOcongenialDummy<-ifelse(X8.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                               X8.2_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.3_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.4_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.5_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.6_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.7_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.8_HI==1 & close_party_HI_1%in%c(1,2) |
                               X8.9_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.10_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.11_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.12_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.13_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.14_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.15_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.16_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.33_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.34_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.35_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.36_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.37_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.38_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.39_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.40_HI==1 & close_party_HI_1%in%c(3,4)| 
                               X8.41_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.42_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.43_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.44_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.45_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.46_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.47_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.48_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(UNESCOcongenialDummy)

#Dissonant broadcaster
UNESCOdissonantDummy<-ifelse(X8.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                               X8.2_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.3_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.4_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.5_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.6_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.7_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.8_HI==1 & close_party_HI_1%in%c(3,4) |
                               X8.9_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.10_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.11_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.12_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.13_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.14_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.15_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.16_HI==1 & close_party_HI_1%in%c(3,4)|
                               X8.33_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.34_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.35_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.36_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.37_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.38_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.39_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.40_HI==1 & close_party_HI_1%in%c(1,2)| 
                               X8.41_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.42_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.43_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.44_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.45_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.46_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.47_HI==1 & close_party_HI_1%in%c(1,2)|
                               X8.48_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(UNESCOdissonantDummy)

#Neutral broadcaster
UNESCOneutralDummy<-ifelse(X8.17_HI==1 | X8.18_HI==1 | X8.19_HI==1 | X8.20_HI==1 | X8.21_HI==1 | X8.22_HI==1 | X8.23_HI==1 | X8.24_HI==1|
                             X8.25_HI==1 | X8.26_HI==1 | X8.27_HI==1 | X8.28_HI==1 | X8.29_HI==1 | X8.30_HI==1 | X8.31_HI==1 | X8.32_HI==1, 1, 0)
table(UNESCOneutralDummy)

# 1 if claim is congenial w/ party ID
congenial_claim8 <- ifelse(close_party_HI_1%in%c(3,4), 1, 0)
table(congenial_claim8)

# 1 if claim is dissonant w/ party ID
dissonant_claim8 <- ifelse(close_party_HI_1%in%c(1,2), 1, 0)
table(dissonant_claim8)

#DV = dv_UNESCO (recode 1-4 so that higher=more belief in claim)
dv_UNESCO_new<-ifelse(dv_UNESCO==4, 4,
                      ifelse(dv_UNESCO==6, 3,
                             ifelse(dv_UNESCO==8, 2,
                                    ifelse(dv_UNESCO==10, 1, NA))))
table(dv_UNESCO_new)

#reattaching DV to dataset
data$dv_UNESCO_new<-ifelse(dv_UNESCO==4, 4,
                      ifelse(dv_UNESCO==6, 3,
                             ifelse(dv_UNESCO==8, 2,
                                    ifelse(dv_UNESCO==10, 1, NA))))
table(data$dv_UNESCO_new)

# 10/12 results by hypothesis

claim8data <- data.frame(cbind(dv_UNESCO_new, UNESCOanycorrectionDummy, UNESCOoutpartisanDummy,
                               UNESCOcopartisanDummy, UNESCOcongenialDummy, UNESCOdissonantDummy,
                               congenial_claim8, dissonant_claim8, Congress, BJP, UNESCOpcDummy,
                               UNESCOpeerDummy, UNESCOexpertDummy, UNESCOaltnewsDummy, 
                               UNESCOvishwasDummy, UNESCOtoiDummy,  UNESCOfbDummy, 
                               UNESCOwhatsappDummy, UNESCOfactcheckDummy, UNESCOcontDummy,
                               UNESCOanysourcedDummy, age, male, education, data$ResponseID))
colnames(claim8data)[2] <- "AnyCorrection"
colnames(claim8data)[3] <- "OutpartisanSpeaker"
colnames(claim8data)[4] <- "CopartisanSpeaker"
colnames(claim8data)[5] <- "CongenialMedia"
colnames(claim8data)[6] <- "DissonantMedia"
colnames(claim8data)[7] <- "CongenialClaim"
colnames(claim8data)[8] <- "DissonantClaim"
colnames(claim8data)[11] <- "PureControl"
colnames(claim8data)[12] <- "Peer"
colnames(claim8data)[13] <- "Expert"
colnames(claim8data)[14] <- "AltNews"
colnames(claim8data)[15] <- "Vishwas"
colnames(claim8data)[16] <- "TOI"
colnames(claim8data)[17] <- "Facebook"
colnames(claim8data)[18] <- "WhatsApp"
colnames(claim8data)[19] <- "AnyFactcheck"
colnames(claim8data)[20] <- "Control"
colnames(claim8data)[21] <- "AnySourced"
head(claim8data)
