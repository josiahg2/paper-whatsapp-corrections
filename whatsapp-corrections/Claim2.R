# claim 2 coding and analysis 9/13

# coding polygamy (claim 2)

## Creating pure control dummy
PolygamypcDummy <- ifelse(X2.pc1_HI==1|X2.pc2_HI==1|X2.pc3_HI==1|X2.pc4_HI==1|X2.pc5_HI==1|X2.pc6_HI==1|X2.pc7_HI==1|X2.pc8_HI==1|X2.pc9_HI==1|X2.pc10_HI==1|X2.pc11_HI==1|X2.pc12_HI==1|X2.pc13_HI==1|X2.pc14_HI==1|X2.pc15_HI==1|X2.pc16_HI==1|X2.pc17_HI==1, 1, 0)
table(PolygamypcDummy)

## reattaching pure control to main dataset
data$PolygamypcDummy <- ifelse(X2.pc1_HI==1|X2.pc2_HI==1|X2.pc3_HI==1|X2.pc4_HI==1|X2.pc5_HI==1|X2.pc6_HI==1|X2.pc7_HI==1|X2.pc8_HI==1|X2.pc9_HI==1|X2.pc10_HI==1|X2.pc11_HI==1|X2.pc12_HI==1|X2.pc13_HI==1|X2.pc14_HI==1|X2.pc15_HI==1|X2.pc16_HI==1|X2.pc17_HI==1, 1, 0)
table(data$PolygamypcDummy)

# misinformation, no correction (control) dummy
PolygamycontDummy <- ifelse(X2.1_HI==1 | X2.1copy_HI==1 | X2.1copy2_HI==1 | X2.1copy3_HI==1 | X2.1copy4_HI==1 |
                    X2.9_HI==1 | X2.9copy_HI==1 | X2.9copy2_HI==1 | X2.9copy3_HI==1 | X2.9copy4_HI==1 |
                    X2.17_HI==1| X2.17copy_HI==1| X2.17copy2_HI==1| X2.17copy3_HI==1| X2.17copy4_HI==1|
                    X2.25_HI==1| X2.25copy_HI==1| X2.25copy2_HI==1| X2.25copy3_HI==1| X2.25copy4HI==1 |  
                    X2.33_HI==1| X2.33copy_HI==1| X2.33copy2_HI==1| X2.33copy3_HI==1| X2.33copy4_HI==1|  
                    X2.41_HI==1| X2.41copy_HI==1| X2.41copy2_HI==1| X2.41copy3_HI==1| X2.41copy4_HI==1, 1, 0) 
table(PolygamycontDummy)

# reattaching no correction (control) dummy to main dataset
data$PolygamycontDummy <- ifelse(X2.1_HI==1 | X2.1copy_HI==1 | X2.1copy2_HI==1 | X2.1copy3_HI==1 | X2.1copy4_HI==1 |
                              X2.9_HI==1 | X2.9copy_HI==1 | X2.9copy2_HI==1 | X2.9copy3_HI==1 | X2.9copy4_HI==1 |
                              X2.17_HI==1| X2.17copy_HI==1| X2.17copy2_HI==1| X2.17copy3_HI==1| X2.17copy4_HI==1|
                              X2.25_HI==1| X2.25copy_HI==1| X2.25copy2_HI==1| X2.25copy3_HI==1| X2.25copy4HI==1 |  
                              X2.33_HI==1| X2.33copy_HI==1| X2.33copy2_HI==1| X2.33copy3_HI==1| X2.33copy4_HI==1|  
                              X2.41_HI==1| X2.41copy_HI==1| X2.41copy2_HI==1| X2.41copy3_HI==1| X2.41copy4_HI==1, 1, 0) 
table(data$PolygamycontDummy)

# expert correction
PolygamyexpertDummy<-ifelse(X2.2_HI==1 | X2.2copy_HI==1 | X2.2copy2_HI==1 | X2.2copy3_HI==1 | X2.2copy4_HI==1 | 
                            X2.10_HI==1 | X2.10copy_HI==1 | X2.10copy2_HI==1 | X2.10copy3_HI==1 | X2.10copy4_HI==1 | 
                            X2.18_HI==1 | X2.18copy_HI==1 | X2.18copy2_HI==1 | X2.18copy3_HI==1 | X2.18copy4_HI==1 | 
                            X2.26_HI==1 | X2.26copy_HI==1 | X2.26copy2_HI==1 | X2.26copy3_HI==1 | X2.26copy4_HI==1 | 
                            X2.34_HI==1 | X2.34copy_HI==1 | X2.34copy2_HI==1 | X2.34copy3_HI==1 | X2.34copy4_HI==1 | 
                            X2.42_HI==1 | X2.42copy_HI==1 | X2.42copy2_HI==1 | X2.42copy3_HI==1 | X2.42copy4_HI==1, 1, 0)
table(PolygamyexpertDummy)

# peer correction
PolygamypeerDummy <-ifelse(X2.8_HI==1| X2.8copy_HI==1| X2.8copy2_HI==1| X2.8copy3_HI==1| X2.8copy4_HI==1|
                          X2.16_HI==1|X2.16copy_HI==1|X2.16copy2_HI==1|X2.16copy3_HI==1|X2.16copy4_HI==1|
                          X2.24_HI==1|X2.24copy_HI==1|X2.24copy2_HI==1|X2.24copy3_HI==1|X2.24copy4_HI==1|
                          X2.32_HI==1|X2.32copy_HI==1|X2.32copy2_HI==1|X2.32copy3_HI==1|X2.32copy4_HI==1|
                          X2.40_HI==1|X2.40copy_HI==1|X2.40copy2_HI==1|X2.40copy3_HI==1|X2.40copy4_HI==1|
                          X2.48_HI==1|X2.48copy_HI==1|X2.48copy2_HI==1|X2.48copy3_HI==1|X2.48copy4_HI==1, 1, 0)
table(PolygamypeerDummy)

#fact checker correction (any fact checker) 
PolygamyfactcheckDummy<-ifelse(X2.3_HI==1|X2.4_HI==1|X2.5_HI==1|X2.6_HI==1|X2.7_HI==1|X2.11_HI==1|X2.12_HI==1|
                               X2.13_HI==1|X2.14_HI==1|X2.15_HI==1|X2.19_HI==1|X2.20_HI==1|X2.21_HI==1|X2.22_HI==1|
                               X2.23_HI==1|X2.27_HI==1|X2.28_HI==1|X2.29_HI==1|X2.30_HI==1|X2.31_HI==1| 
                               X2.35_HI==1|X2.36_HI==1|X2.37_HI==1|X2.38_HI==1|X2.39_HI==1|X2.43_HI==1| 
                               X2.44_HI==1|X2.45_HI==1|X2.46_HI==1|X2.47_HI==1, 1, 0)
table(PolygamyfactcheckDummy)

#Altnews correction
PolygamyaltnewsDummy<-ifelse(X2.3_HI==1|X2.11_HI==1|X2.19_HI==1|X2.27_HI==1|X2.35_HI==1|X2.43_HI==1, 1, 0)
table(PolygamyaltnewsDummy)

#VishwasNews correction
PolygamyvishwasDummy<-ifelse(X2.4_HI==1|X2.12_HI==1|X2.20_HI==1|X2.28_HI==1|X2.36_HI==1|X2.44_HI==1, 1, 0)
table(PolygamyvishwasDummy)

#Times of India correction
PolygamytoiDummy<-ifelse(X2.5_HI==1|X2.13_HI==1|X2.21_HI==1|X2.29_HI==1|X2.37_HI==1|X2.45_HI==1, 1, 0)
table(PolygamytoiDummy)

#Facebook correction
PolygamyfbDummy<-ifelse(X2.6_HI==1|X2.14_HI==1|X2.22_HI==1|X2.30_HI==1|X2.38_HI==1|X2.46_HI==1, 1, 0)
table(PolygamyfbDummy)

#WhatsApp correction
PolygamywhatsappDummy<-ifelse(X2.7_HI==1|X2.15_HI==1|X2.23_HI==1|X2.31_HI==1|X2.39_HI==1|X2.47_HI==1, 1, 0)
table(PolygamywhatsappDummy)

#Any sourced correction
PolygamyanysourcedDummy<-ifelse(PolygamyexpertDummy==1|PolygamyfactcheckDummy==1,1,0)
table(PolygamyanysourcedDummy)

#Any correction
PolygamyanycorrectionDummy<-ifelse(PolygamyexpertDummy==1|PolygamypeerDummy==1|PolygamyfactcheckDummy==1,1,0)
table(PolygamyanycorrectionDummy)


#Copartisan source/speaker
PolygamycopartisanDummy<-  ifelse(X2.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                X2.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                X2.17_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.18_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.19_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.20_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.21_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.22_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.23_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.24_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.33_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.34_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.35_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.36_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.37_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.38_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.39_HI==1 & close_party_HI_1%in%c(3,4)|
                                X2.40_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(PolygamycopartisanDummy)

#Outpartisan source/speaker
PolygamyoutpartisanDummy<-ifelse(X2.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                                X2.2_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.3_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.4_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.5_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.6_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.7_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.8_HI==1 & close_party_HI_1%in%c(1,2) |
                                X2.17_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.18_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.19_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.20_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.21_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.22_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.23_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.24_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                X2.40_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(PolygamyoutpartisanDummy)

#Unidentified source/speaker
PolygamyunidDummy<-ifelse(X2.9_HI==1 | X2.10_HI==1 | X2.11_HI==1 | X2.12_HI==1 | X2.13_HI==1 | 
                         X2.14_HI==1 |X2.15_HI==1 | X2.16_HI==1 | X2.25_HI==1 | X2.26_HI==1 | 
                         X2.27_HI==1 |X2.28_HI==1 | X2.29_HI==1 | X2.30_HI==1 | X2.31_HI==1 | 
                         X2.32_HI==1 |X2.41_HI==1 | X2.42_HI==1 | X2.43_HI==1 | X2.44_HI==1 | 
                         X2.45_HI==1 |X2.46_HI==1 | X2.47_HI==1 | X2.48_HI==1, 1, 0)
table(PolygamyunidDummy)

#Congenial broadcaster
PolygamycongenialDummy<-ifelse(X2.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                               X2.2_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.3_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.4_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.5_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.6_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.7_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.8_HI==1 & close_party_HI_1%in%c(1,2) |
                               X2.9_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.10_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.11_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.12_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.13_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.14_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.15_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.16_HI==1 & close_party_HI_1%in%c(1,2)|
                               X2.33_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.34_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.35_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.36_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.37_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.38_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.39_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.40_HI==1 & close_party_HI_1%in%c(3,4)| 
                               X2.41_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.42_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.43_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.44_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.45_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.46_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.47_HI==1 & close_party_HI_1%in%c(3,4)|
                               X2.48_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(PolygamycongenialDummy)

#Dissonant broadcasterX2
PolygamydissonantDummy <- ifelse(X2.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                                 X2.2_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.3_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.4_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.5_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.6_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.7_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.8_HI==1 & close_party_HI_1%in%c(3,4) |
                                 X2.9_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.10_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.11_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.12_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.13_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.14_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.15_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.16_HI==1 & close_party_HI_1%in%c(3,4)|
                                 X2.33_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.34_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.35_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.36_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.37_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.38_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.39_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.40_HI==1 & close_party_HI_1%in%c(1,2)| 
                                 X2.41_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.42_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.43_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.44_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.45_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.46_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.47_HI==1 & close_party_HI_1%in%c(1,2)|
                                 X2.48_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(PolygamydissonantDummy)

# congenial and dissonant claims

# 1 if claim is congenial w/ party ID
congenial_claim2 <- ifelse(close_party_HI_1%in%c(3,4), 1, 0)
table(congenial_claim2)

# 1 if claim is dissonant w/ party ID
dissonant_claim2 <- ifelse(close_party_HI_1%in%c(1,2), 1, 0)
table(dissonant_claim2)


#DV = dv_polygamy (recode 1-4 so that higher=more belief in claim)
dv_polygamy_new<-ifelse(dv_polygamy==4, 4,
                         ifelse(dv_polygamy==6, 3,
                                ifelse(dv_polygamy==8, 2,
                                       ifelse(dv_polygamy==10, 1, NA))))
table(dv_polygamy_new)

#reattaching dv to dataset
data$dv_polygamy_new<-ifelse(dv_polygamy==4, 4,
                        ifelse(dv_polygamy==6, 3,
                               ifelse(dv_polygamy==8, 2,
                                      ifelse(dv_polygamy==10, 1, NA))))
table(data$dv_polygamy_new)

# 10/12 results by hypothesis

claim2data <- data.frame(cbind(dv_polygamy_new, PolygamyanycorrectionDummy,
                               PolygamyoutpartisanDummy, PolygamycopartisanDummy,
                              PolygamycongenialDummy, PolygamydissonantDummy,
                               congenial_claim2, dissonant_claim2, BJP, Congress, PolygamypcDummy,
                              PolygamypeerDummy ,PolygamyexpertDummy, PolygamyaltnewsDummy, 
                              PolygamyvishwasDummy, PolygamytoiDummy, PolygamyfbDummy, 
                              PolygamywhatsappDummy, PolygamyfactcheckDummy, PolygamycontDummy,
                              PolygamyanysourcedDummy, hindu, leader, 
                              prayer, temple, age, male, education, data$ResponseID))

colnames(claim2data)[2] <- "AnyCorrection"
colnames(claim2data)[3] <- "OutpartisanSpeaker"
colnames(claim2data)[4] <- "CopartisanSpeaker"
colnames(claim2data)[5] <- "CongenialMedia"
colnames(claim2data)[6] <- "DissonantMedia"
colnames(claim2data)[7] <- "CongenialClaim"
colnames(claim2data)[8] <- "DissonantClaim"
colnames(claim2data)[11] <- "PureControl"
colnames(claim2data)[12] <- "Peer"
colnames(claim2data)[13] <- "Expert"
colnames(claim2data)[14] <- "AltNews"
colnames(claim2data)[15] <- "Vishwas"
colnames(claim2data)[16] <- "TOI"
colnames(claim2data)[17] <- "Facebook"
colnames(claim2data)[18] <- "WhatsApp"
colnames(claim2data)[19] <- "AnyFactcheck"
colnames(claim2data)[20] <- "Control"
colnames(claim2data)[21] <- "AnySourced"
head(claim2data)
