# Claim 5 and 6 9/14

rm(list=ls())
dev.off()

data<-read.csv("~/Dropbox/IndiaMisinformation_Sumitra/Data/Raw data (Qualtrics export 06:21)/ChauchardFlynn_India_rumors__MAIN_STUDY_27June.csv",stringsAsFactors = F)
attach(data) 


# claim 5 : australia cricket

# no pure control

# congenial broadcaster
CricketcongenialDummy<-ifelse( X5.1_HI==1 & close_party_HI_1%in%c(1,2) | 
                            X5.3_HI==1 & close_party_HI_1%in%c(3,4), 1, 0)
table(CricketcongenialDummy)


# dissonant broadcaster
CricketdissonantDummy<-ifelse(X5.1_HI==1 & close_party_HI_1%in%c(3,4) | 
                              X5.3_HI==1 & close_party_HI_1%in%c(1,2), 1, 0)
table(CricketdissonantDummy)

# neutral broadcaster
CricketneutralDummy <- ifelse(X5.2_HI==1, 1, 0)
table(CricketneutralDummy)

# dv for claim 5 
dv_australia_new<-ifelse(dv_australia==4, 4,
                      ifelse(dv_australia==6, 3,
                             ifelse(dv_australia==8, 2,
                                    ifelse(dv_australia==10, 1, NA))))
table(dv_australia_new)

#DV reattaching to dataset
data$dv_australia_new<-ifelse(dv_australia==4, 4,
                         ifelse(dv_australia==6, 3,
                                ifelse(dv_australia==8, 2,
                                       ifelse(dv_australia==10, 1, NA))))
table(data$dv_australia_new)

claim5data <- data.frame(dv_australia_new)


## claim 6 HIV

# dv for claim 6 
dv_HIV_new<-ifelse(dv_HIV==4, 4,
                         ifelse(dv_HIV==6, 3,
                                ifelse(dv_HIV==8, 2,
                                       ifelse(dv_HIV==10, 1, NA))))
table(dv_HIV_new)

# dv reattaching to dataset
data$dv_HIV_new<-ifelse(dv_HIV==4, 4,
                   ifelse(dv_HIV==6, 3,
                          ifelse(dv_HIV==8, 2,
                                 ifelse(dv_HIV==10, 1, NA))))
table(data$dv_HIV_new)

