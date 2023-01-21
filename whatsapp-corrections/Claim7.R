# claim 7 coding and anaysis 9/14

# coding EVM (claim 7)

# pure control
EVMpcdummy<-ifelse(X7.pc1_HI==1|X7.pc2_HI==1|X7.pc3_HI==1|X7.pc4_HI==1|X7.pc5_HI==1|X7.pc6_HI==1|X7.pc7_HI==1|X7.pc8_HI==1|X7.pc9_HI==1|X7.pc10_HI==1|X7.pc11_HI==1|X7.pc12_HI==1|X7.pc13_HI==1|X7.pc14_HI==1|X7.pc15_HI==1|X7.pc16_HI==1|X7.pc17_HI==1, 1, 0)
table(EVMpcdummy)

# reattach pc to dataset pure control
data$EVMpcdummy<-ifelse(X7.pc1_HI==1|X7.pc2_HI==1|X7.pc3_HI==1|X7.pc4_HI==1|X7.pc5_HI==1|X7.pc6_HI==1|X7.pc7_HI==1|X7.pc8_HI==1|X7.pc9_HI==1|X7.pc10_HI==1|X7.pc11_HI==1|X7.pc12_HI==1|X7.pc13_HI==1|X7.pc14_HI==1|X7.pc15_HI==1|X7.pc16_HI==1|X7.pc17_HI==1, 1, 0)
table(data$EVMpcdummy)

# misinformnation, no correction (control) dummy7
data$EVMcontDummy <-   ifelse(X7.1_HI==1 | X7.1copy_HI==1 | X7.1copy2_HI==1 | X7.1copy3_HI==1 | X7.1copy4_HI==1 |
                              X7.9_HI==1 | X7.9copy_HI==1 | X7.9copy2_HI==1 | X7.9copy3_HI==1 | X7.9copy4_HI==1 |
                              X7.17_HI==1| X7.17copy_HI==1| X7.17copy2_HI==1| X7.17copy3_HI==1| X7.17copy4_HI==1|
                              X7.25_HI==1| X7.25copy_HI==1| X7.25copy2_HI==1| X7.25copy3_HI==1| X7.25copy4_HI==1 |  
                              X7.33_HI==1| X7.33copy_HI==1| X7.33copy2_HI==1| X7.33copy3_HI==1| X7.33copy4_HI==1|  
                              X7.41_HI==1| X7.41copy_HI==1| X7.41copy2_HI==1| X7.41copy3_HI==1| X7.41copy4_HI==1, 1, 0) 
table(data$EVMcontDummy)

EVMcontDummy <-   ifelse(X7.1_HI==1 | X7.1copy_HI==1 | X7.1copy2_HI==1 | X7.1copy3_HI==1 | X7.1copy4_HI==1 |
                                X7.9_HI==1 | X7.9copy_HI==1 | X7.9copy2_HI==1 | X7.9copy3_HI==1 | X7.9copy4_HI==1 |
                                X7.17_HI==1| X7.17copy_HI==1| X7.17copy2_HI==1| X7.17copy3_HI==1| X7.17copy4_HI==1|
                                X7.25_HI==1| X7.25copy_HI==1| X7.25copy2_HI==1| X7.25copy3_HI==1| X7.25copy4_HI==1 |  
                                X7.33_HI==1| X7.33copy_HI==1| X7.33copy2_HI==1| X7.33copy3_HI==1| X7.33copy4_HI==1|  
                                X7.41_HI==1| X7.41copy_HI==1| X7.41copy2_HI==1| X7.41copy3_HI==1| X7.41copy4_HI==1, 1, 0) 

# expert correction
EVMexpertDummy<-ifelse(X7.2_HI==1 |  X7.2copy_HI==1 |  X7.2copy2_HI==1 |  X7.2copy3_HI==1 |  X7.2copy4_HI==1 | 
                              X7.10_HI==1 | X7.10copy_HI==1 | X7.10copy2_HI==1 | X7.10copy3_HI==1 | X7.10copy4_HI==1 | 
                              X7.18_HI==1 | X7.18copy_HI==1 | X7.18copy2_HI==1 | X7.18copy3_HI==1 | X7.18copy4_HI==1 | 
                              X7.26_HI==1 | X7.26copy_HI==1 | X7.26copy2_HI==1 | X7.26copy3_HI==1 | X7.26copy4_HI==1 | 
                              X7.34_HI==1 | X7.34copy_HI==1 | X7.34copy2_HI==1 | X7.34copy3_HI==1 | X7.34copy4_HI==1 | 
                              X7.42_HI==1 | X7.42copy_HI==1 | X7.42copy2_HI==1 | X7.42copy3_HI==1 | X7.42copy4_HI==1, 1, 0)
table(EVMexpertDummy)

# peer correction
EVMpeerDummy <-ifelse(X7.8_HI==1| X7.8copy_HI==1| X7.8copy2_HI==1| X7.8copy3_HI==1| X7.8copy4_HI==1|
                             X7.16_HI==1|X7.16copy_HI==1|X7.16copy2_HI==1|X7.16copy3_HI==1|X7.16copy4_HI==1|
                             X7.24_HI==1|X7.24copy_HI==1|X7.24copy2_HI==1|X7.24copy3_HI==1|X7.24copy4_HI==1|
                             X7.32_HI==1|X7.32copy_HI==1|X7.32copy2_HI==1|X7.32copy3_HI==1|X7.32copy4_HI==1|
                             X7.40_HI==1|X7.40copy_HI==1|X7.40copy2_HI==1|X7.40copy3_HI==1|X7.40copy4_HI==1|
                             X7.48_HI==1|X7.48copy_HI==1|X7.48copy2_HI==1|X7.48copy3_HI==1|X7.48copy4_HI==1, 1, 0)
table(EVMpeerDummy)

#fact checker correction (any fact checker) 
EVMfactcheckDummy<-ifelse(X7.3_HI==1| X7.4_HI==1| X7.5_HI==1| X7.6_HI==1| X7.7_HI==1| X7.11_HI==1|X7.12_HI==1|
                                 X7.13_HI==1|X7.14_HI==1|X7.15_HI==1|X7.19_HI==1|X7.20_HI==1|X7.21_HI==1|X7.22_HI==1|
                                 X7.23_HI==1|X7.27_HI==1|X7.28_HI==1|X7.29_HI==1|X7.30_HI==1|X7.31_HI==1| 
                                 X7.35_HI==1|X7.36_HI==1|X7.37_HI==1|X7.38_HI==1|X7.39_HI==1|X7.43_HI==1| 
                                 X7.44_HI==1|X7.45_HI==1|X7.46_HI==1|X7.47_HI==1, 1, 0)
table(EVMfactcheckDummy)

#Altnews correction
EVMaltnewsDummy<-ifelse(X7.3_HI==1|X7.11_HI==1|X7.19_HI==1|X7.27_HI==1|X7.35_HI==1|X7.43_HI==1, 1, 0)
table(EVMaltnewsDummy)

#VishwasNews correction
EVMvishwasDummy<-ifelse(X7.4_HI==1|X7.12_HI==1|X7.20_HI==1|X7.28_HI==1|X7.36_HI==1|X7.44_HI==1, 1, 0)
table(EVMvishwasDummy)

#Times of India correction
EVMtoiDummy<-ifelse(X7.5_HI==1|X7.13_HI==1|X7.21_HI==1|X7.29_HI==1|X7.37_HI==1|X7.45_HI==1, 1, 0)
table(EVMtoiDummy)

#Facebook correction
EVMfbDummy<-ifelse(X7.6_HI==1|X7.14_HI==1|X7.22_HI==1|X7.30_HI==1|X7.38_HI==1|X7.46_HI==1, 1, 0)
table(EVMfbDummy)

#WhatsApp correction
EVMwhatsappDummy<-ifelse(X7.7_HI==1|X7.15_HI==1|X7.23_HI==1|X7.31_HI==1|X7.39_HI==1|X7.47_HI==1, 1, 0)
table(EVMwhatsappDummy)

#Any sourced correction
EVManysourcedDummy<-ifelse(EVMexpertDummy==1|EVMfactcheckDummy==1,1,0)
table(EVManysourcedDummy)

#Any correction
EVManycorrectionDummy<-ifelse(EVMexpertDummy==1|EVMpeerDummy==1|EVMfactcheckDummy==1,1,0)
table(EVManycorrectionDummy)

#Copartisan source/speaker
EVMcopartisanDummy<-ifelse(X7.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                     X7.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                     X7.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                     X7.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                     X7.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                     X7.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                     X7.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                     X7.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                    X7.17_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.18_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.19_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.20_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.21_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.22_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.23_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.24_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                    X7.40_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(EVMcopartisanDummy)

#Outpartisan source/speaker3
EVMoutpartisanDummy<-ifelse(X7.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                    X7.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X7.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X7.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X7.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X7.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X7.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                    X7.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.17_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.18_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.19_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.20_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.21_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.22_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.23_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.24_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.40_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(EVMoutpartisanDummy)

#Unidentified source/speaker
EVMunidDummy<-ifelse(X7.9_HI==1 | X7.10_HI==1 | X7.11_HI==1 | X7.12_HI==1 | X7.13_HI==1 | 
                            X7.14_HI==1 |X7.15_HI==1 | X7.16_HI==1 | X7.25_HI==1 | X7.26_HI==1 | 
                            X7.27_HI==1 |X7.28_HI==1 | X7.29_HI==1 | X7.30_HI==1 | X7.31_HI==1 | 
                            X7.32_HI==1 |X7.41_HI==1 | X7.42_HI==1 | X7.43_HI==1 | X7.44_HI==1 | 
                            X7.45_HI==1 |X7.46_HI==1 | X7.47_HI==1 | X7.48_HI==1, 1, 0)
table(EVMunidDummy)

#Congenial broadcaster
EVMcongenialDummy<-ifelse(X7.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                X7.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                X7.9_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.10_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.11_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.12_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.13_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.14_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.15_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.16_HI==1 & close_party_HI_1%in%c(1,2)|
                                X7.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.40_HI==1 & close_party_HI_1%in%c(3,4)| 
                                X7.41_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.42_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.43_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.44_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.45_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.46_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.47_HI==1 & close_party_HI_1%in%c(3,4)|
                                X7.48_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(EVMcongenialDummy)

#Dissonant broadcaster
EVMdissonantDummy <- ifelse(X7.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                   X7.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                   X7.9_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.10_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.11_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.12_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.13_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.14_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.15_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.16_HI==1 & close_party_HI_1%in%c(3,4)|
                                   X7.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.40_HI==1 & close_party_HI_1%in%c(1,2)| 
                                   X7.41_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.42_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.43_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.44_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.45_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.46_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.47_HI==1 & close_party_HI_1%in%c(1,2)|
                                   X7.48_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(EVMdissonantDummy)

# 1 if claim is congenial w/ party ID
congenial_claim7 <- ifelse(close_party_HI_1%in%c(1,2), 1, 0)
table(congenial_claim7)

# 1 if claim is dissonant w/ party ID
dissonant_claim7 <- ifelse(close_party_HI_1%in%c(3,4), 1, 0)
table(dissonant_claim7)

#DV = dv_EVMs (recode 1-4 so that higher=more belief in claim)
dv_EVM_new<-ifelse(dv_EVMs==4, 4,
                        ifelse(dv_EVMs==6, 3,
                               ifelse(dv_EVMs==8, 2,
                                      ifelse(dv_EVMs==10, 1, NA))))
table(dv_EVM_new)

#DV reattach to dataset
data$dv_EVM_new<-ifelse(dv_EVMs==4, 4,
                   ifelse(dv_EVMs==6, 3,
                          ifelse(dv_EVMs==8, 2,
                                 ifelse(dv_EVMs==10, 1, NA))))
table(data$dv_EVM_new)

# 10/12 results by hypothesis

claim7data <- data.frame(cbind(dv_EVM_new, EVManycorrectionDummy, EVMoutpartisanDummy, EVMcopartisanDummy,
                               EVMcongenialDummy, EVMdissonantDummy, congenial_claim7, 
                               dissonant_claim7, Congress, BJP, EVMpcdummy,
                               EVMpeerDummy, EVMexpertDummy, EVMaltnewsDummy,
                                 EVMvishwasDummy, EVMtoiDummy, EVMfbDummy, EVMwhatsappDummy,
                               EVMfactcheckDummy, EVMcontDummy, EVManysourcedDummy, 
                               male, age, education, data$ResponseID))
colnames(claim7data)[2] <- "AnyCorrection"
colnames(claim7data)[3] <- "OutpartisanSpeaker"
colnames(claim7data)[4] <- "CopartisanSpeaker"
colnames(claim7data)[5] <- "CongenialMedia"
colnames(claim7data)[6] <- "DissonantMedia"
colnames(claim7data)[7] <- "CongenialClaim"
colnames(claim7data)[8] <- "DissonantClaim"
colnames(claim7data)[11] <- "PureControl"
colnames(claim7data)[12] <- "Peer"
colnames(claim7data)[13] <- "Expert"
colnames(claim7data)[14] <- "AltNews"
colnames(claim7data)[15] <- "Vishwas"
colnames(claim7data)[16] <- "TOI"
colnames(claim7data)[17] <- "Facebook"
colnames(claim7data)[18] <- "WhatsApp"
colnames(claim7data)[19] <- "AnyFactcheck"
colnames(claim7data)[20] <- "Control"
colnames(claim7data)[21] <- "AnySourced"
head(claim7data)
