# clear environment and plot window
rm(list=ls())
if(dev.cur() > 1){
  dev.off()
}

# import and attach data
library(foreign)
data<-read.csv("ChauchardFlynn_India_rumors__MAIN_STUDY_27June.csv",stringsAsFactors = F)
dim(data)
names(data)
attach(data)

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