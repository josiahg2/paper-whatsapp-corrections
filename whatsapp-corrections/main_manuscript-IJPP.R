# replication for IJPP: main manuscript
# december 28, 2022

##### run dependencies #####

# run these files before running the rest of this script
source("loaddata.R")
source("Claim1.R")
source("Claim2.R")
source("Claim3.R")
source("Claim4.R")
source("Claim5-6.R")
source("Claim7.R")
source("Claim8.R")
source("Claim9.R")

##### load packages #####

library(dplyr)
library(dotwhisker)
library(ggplot2)
library(multiwayvcov)
library(stargazer)

##### create subsets #####

# remove NAs
claim1new <- claim1data[!is.na(claim1data$dv_muslimpop_new),]
claim2new <- claim2data[!is.na(claim2data$dv_polygamy_new),]
claim3new <- claim3data[!is.na(claim3data$dv_MMR_new),]
claim4new <- claim4data[!is.na(claim4data$dv_gomutra_new),]
claim7new <- claim7data[!is.na(claim7data$dv_EVM_new),]
claim8new <- claim8data[!is.na(claim8data$dv_UNESCO_new),]
claim9new <- claim9data[!is.na(claim9data$dv_bose_new),]

# create treatment groups
treat1 <- subset(claim1new, claim1new$AnyCorrection==1)
treat2 <- subset(claim2new, claim2new$AnyCorrection==1)
treat3 <- subset(claim3new, claim3new$AnyCorrection==1)
treat4 <- subset(claim4new, claim4new$AnyCorrection==1)
treat7 <- subset(claim7new, claim7new$AnyCorrection==1)
treat8 <- subset(claim8new, claim8new$AnyCorrection==1)
treat9 <- subset(claim9new, claim9new$AnyCorrection==1)

# create control groups
cont1 <- subset(claim1new, claim1new$AnyCorrection==0)
cont2 <- subset(claim2new, claim2new$AnyCorrection==0)
cont3 <- subset(claim3new, claim3new$AnyCorrection==0)
cont4 <- subset(claim4new, claim4new$AnyCorrection==0)
cont7 <- subset(claim7new, claim7new$AnyCorrection==0)
cont8 <- subset(claim8new, claim8new$AnyCorrection==0)
cont9 <- subset(claim9new, claim9new$AnyCorrection==0)

##### stacked table #####

new1 <- claim1new[c("dv_muslimpop_new","AnyCorrection", "V29", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new1)[1] <- "dv"
new1$claim <- "claim1"

new2 <- claim2new[c("dv_polygamy_new","AnyCorrection", "V29", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new2)[1] <- "dv"
new2$claim <- "claim2"

new3 <- claim3new[c("dv_MMR_new","AnyCorrection", "V21", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new3)[1] <- "dv"
colnames(new3)[3] <- "V29"
new3$claim <- "claim3"

new4 <- claim4new[c("dv_gomutra_new","AnyCorrection", "V29", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    , "DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new4)[1] <- "dv"
new4$claim <- "claim4"

new7 <- claim7new[c("dv_EVM_new","AnyCorrection", "V25", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new7)[1] <- "dv"
colnames(new7)[3] <- "V29"
new7$claim <- "claim7"

new8 <- claim8new[c("dv_UNESCO_new","AnyCorrection", "V25", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    , "DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new8)[1] <- "dv"
colnames(new8)[3] <- "V29"
new8$claim <- "claim8"

new9 <- claim9new[c("dv_bose_new","AnyCorrection", "V21", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    , "DissonantMedia", "Control", "PureControl", "AnySourced")]
colnames(new9)[1] <- "dv"
colnames(new9)[3] <- "V29"
new9$claim <- "claim9"

stacked <- rbind(new1, new2, new3, new4, new7, new8, new9)

##### main: figure 1 #####

# extract proportion strongly + somewhat believe for control group
dv1 <- prop.table(table(cont1$dv_muslimpop_new))[3] + prop.table(table(cont1$dv_muslimpop_new))[4] 
dv2 <- prop.table(table(cont2$dv_polygamy_new))[3] + prop.table(table(cont2$dv_polygamy_new))[4] 
dv3 <- prop.table(table(cont3$dv_MMR_new))[3] + prop.table(table(cont3$dv_MMR_new))[4] 
dv4 <- prop.table(table(cont4$dv_gomutra_new))[3] + prop.table(table(cont4$dv_gomutra_new))[4]
dv5 <- prop.table(table(cont7$dv_EVM_new))[3] + prop.table(table(cont7$dv_EVM_new))[4]
dv6 <- prop.table(table(cont8$dv_UNESCO_new))[3] + prop.table(table(cont8$dv_UNESCO_new))[4]
dv7 <- prop.table(table(cont9$dv_bose_new))[3] + prop.table(table(cont9$dv_bose_new))[4]
dv8 <- prop.table(table(data$dv_HIV_new))[3] + prop.table(table(data$dv_HIV_new))[4]
dv9 <- prop.table(table(data$dv_australia_new))[3] + prop.table(table(data$dv_australia_new))[4]

rumors <- c("Muslim\nPopulation", "Polygamy", "MMR", "Gomutra", "EVM", "UNESCO", "Bose", "HIV",
            "Australia")
Veracity <- c("False", "False", "False", "False", "False", "False", "False", "True", "True")
pct_belief <- c(dv1, dv2, dv3, dv4, dv5, dv6, dv7, dv8, dv9)
belief_data <- data.frame(pct_belief,rumors, Veracity)
belief_data$pct_belief <- belief_data$pct_belief*100

# graph

q <- ggplot(data=belief_data, aes(x=reorder(rumors, -pct_belief), y=pct_belief, fill=Veracity)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge()) +
  geom_text(aes(label = round(pct_belief, digits=1)), vjust = -0.3) +
  theme_bw()  +
  xlab("Rumors") + ylab("Percent of Control Group Respondents Who Believe Each Story") + 
  ggtitle("Baseline Rate of Belief in DV Stories (Control Group)")
q + theme_bw() +
  scale_fill_grey(start=0.4, end=0.7)


##### main: figure 2 #####

treat_all <- subset(stacked, stacked$AnyCorrection==1)
cont_all <- subset(stacked, stacked$AnyCorrection==0)

treat_all$dv <- as.numeric(treat_all$dv)
cont_all$dv <- as.numeric(cont_all$dv)

new <- data.frame(
  name=c("Overall", "Overall", "Muslim\nPopulation", "Muslim\nPopulation", "Polygamy", "Polygamy", "MMR", "MMR",
         "Gomutra", "Gomutra", "EVM", "EVM", "UNESCO", "UNESCO", "Bose", "Bose"),
  group=c("Treatment", "Control", "Treatment", "Control", "Treatment", "Control", "Treatment", "Control", 
          "Treatment", "Control", "Treatment", "Control", "Treatment", "Control", "Treatment", "Control"),
  value=c(mean(as.numeric(treat_all$dv)),
          mean(as.numeric(cont_all$dv)),
          mean(as.numeric(treat1$dv_muslimpop_new)), 
          mean(as.numeric(cont1$dv_muslimpop_new)), 
          mean(as.numeric(treat2$dv_polygamy_new)), 
          mean(as.numeric(cont2$dv_polygamy_new)),
          mean(as.numeric(treat3$dv_MMR_new)), 
          mean(as.numeric(cont3$dv_MMR_new)),
          mean(as.numeric(treat4$dv_gomutra_new)), 
          mean(as.numeric(cont4$dv_gomutra_new)),
          mean(as.numeric(treat7$dv_EVM_new)), 
          mean(as.numeric(cont7$dv_EVM_new)),
          mean(as.numeric(treat8$dv_UNESCO_new)), 
          mean(as.numeric(cont8$dv_UNESCO_new)),
          mean(as.numeric(treat9$dv_bose_new)), 
          mean(as.numeric(cont9$dv_bose_new))
  ),
  sd=c(sd(as.numeric(treat_all$dv)), 
       sd(as.numeric(cont_all$dv)),
       sd(as.numeric(treat1$dv_muslimpop_new)), 
       sd(as.numeric(cont1$dv_muslimpop_new)), 
       sd(as.numeric(treat2$dv_polygamy_new)), 
       sd(as.numeric(cont2$dv_polygamy_new)),
       sd(as.numeric(treat3$dv_MMR_new)), 
       sd(as.numeric(cont3$dv_MMR_new)),
       sd(as.numeric(treat4$dv_gomutra_new)), 
       sd(as.numeric(cont4$dv_gomutra_new)),
       sd(as.numeric(treat7$dv_EVM_new)), 
       sd(as.numeric(cont7$dv_EVM_new)),
       sd(as.numeric(treat8$dv_UNESCO_new)), 
       sd(as.numeric(cont8$dv_UNESCO_new)),
       sd(as.numeric(treat9$dv_bose_new)), 
       sd(as.numeric(cont9$dv_bose_new))
  ))
new

new$se <- new$sd/sqrt(5100)
new$ci <- new$se * 1.96

new$name <- factor(new$name, levels = c("Overall", "Muslim\nPopulation", "Polygamy", "MMR", "Gomutra", 
                                        "EVM", "UNESCO", "Bose"))

dodge <- position_dodge(0.9)

p <- ggplot(new, aes(x = name, y = value, fill = factor(group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = value + ci, ymin = value - ci), position = dodge, width = 0.2, size=0.4)
p + theme_bw() +
  xlab("") + ylab("Average Belief in False Story (higher = more belief)") + 
  ggtitle("") +
  theme(legend.position="bottom", legend.title = element_blank())

##### main: table 3 #####

# rumor 1
claim1_h1control <- lm(dv_muslimpop_new ~ AnyCorrection + DissonantMedia + 
                        CongenialMedia + CopartisanSpeaker + OutpartisanSpeaker, 
                        data=claim1data)
# rumor 2
claim2_h1control <- lm(dv_polygamy_new ~ AnyCorrection + DissonantMedia + CongenialMedia + 
                          CopartisanSpeaker + OutpartisanSpeaker, data=claim2data)
# rumor 3
claim3_h1control <- lm(dv_MMR_new ~ AnyCorrection + DissonantMedia + CongenialMedia, 
                        data=claim3data)
# rumor 4
claim4_h1control <- lm(dv_gomutra_new ~ AnyCorrection + DissonantMedia + CongenialMedia + 
                        CopartisanSpeaker + OutpartisanSpeaker, data=claim4data)
# rumor 7
claim7_h1control <- lm(dv_EVM_new ~ AnyCorrection +
                          DissonantMedia + CongenialMedia + 
                          CopartisanSpeaker + OutpartisanSpeaker, data=claim7data)
# rumor 8
claim8_h1control <- lm(dv_UNESCO_new ~ AnyCorrection + DissonantMedia + CongenialMedia + 
                        CopartisanSpeaker + OutpartisanSpeaker, data=claim8data)
# rumor 9
claim9_h1control <- lm(dv_bose_new ~ AnyCorrection + DissonantMedia + CongenialMedia, 
                        data=claim9data)
# table
stargazer(claim1_h1control, claim2_h1control, claim3_h1control, claim4_h1control, 
          claim7_h1control, claim8_h1control, claim9_h1control)


##### main: table 4 #####

# rumor 1
claim1_h4a <- lm(dv_muslimpop_new ~ AnyCorrection * CongenialClaim, data=claim1data)

# rumor 2
claim2_h4a <- lm(dv_polygamy_new ~ AnyCorrection * CongenialClaim, data=claim2data)

# rumor 4
claim4_h4a <- lm(dv_gomutra_new ~ AnyCorrection * CongenialClaim, data=claim4data)

# rumor 7
claim7_h4a <- lm(dv_EVM_new ~ AnyCorrection * CongenialClaim, data=claim7data)

# rumor 8
claim8_h4a <- lm(dv_UNESCO_new ~ AnyCorrection * CongenialClaim, data=claim8data)

# table
stargazer(claim1_h4a, claim2_h4a, claim4_h4a, claim7_h4a, claim8_h4a, 
          star.cutoffs = c(0.05, 0.01, 0.001)) 

##### main: table 5 #####

# rumor 1
claim1_h4b <- lm(dv_muslimpop_new ~ AnyCorrection * DissonantClaim, data=claim1data)

# rumor 2
claim2_h4b <- lm(dv_polygamy_new ~ AnyCorrection * DissonantClaim, data=claim2data)

# rumor 4
claim4_h4b <- lm(dv_gomutra_new ~ AnyCorrection * DissonantClaim, data=claim4data)

# rumor 7
claim7_h4b <- lm(dv_EVM_new ~ AnyCorrection * DissonantClaim, data=claim7data)

# rumor 8
claim8_h4b <- lm(dv_UNESCO_new ~ AnyCorrection * DissonantClaim, data=claim8data)

# table
stargazer(claim1_h4b, claim2_h4b, claim4_h4b, claim7_h4b, claim8_h4b)

##### main: table 6 #####

# control and any correction regression
bivariate1 <- lm(dv ~ AnyCorrection, data=stacked)
summary(bivariate1)

# cluster SE by response ID (V29) and claim
bivariate1.variance <- vcov(bivariate1)
bivariate1.stderrors <- sqrt(diag(bivariate1.variance))
bivariate1.rvariance <- cluster.vcov(bivariate1, cbind(stacked$V29, stacked$claim))
bivariate1.rstderrors <- sqrt(diag(bivariate1.rvariance))

# control and all 3 correction groups
fourgroups <- lm(dv ~ Peer +  Expert + AnyFactcheck, data=stacked)
summary(fourgroups)

#cluster SE
fourgroups.variance <- vcov(fourgroups)
fourgroups.stderrors <- sqrt(diag(fourgroups.variance))
fourgroups.rvariance <- cluster.vcov(fourgroups ,cbind(stacked$V29, stacked$claim))
fourgroups.rstderrors <- sqrt(diag(fourgroups.rvariance))

# table
stargazer(bivariate1, fourgroups,
          se  = list(bivariate1.rstderrors, 
                     fourgroups.rstderrors), digits = 4, 
          omit = c("state", "year"), omit.stat = c("f"), 
          notes = "All regressions include robust SEs clustered at respondent 
          and headline levels", 
          notes.append = FALSE, notes.align = "l")

##### main: figure 3 #####

# rumor 1 model
claim1_4groups <- lm(dv_muslimpop_new ~ Peer +  Expert + AnyFactcheck, data=claim1data)

# rumor 2 model
claim2_4groups <- lm(dv_polygamy_new ~ Peer +  Expert + AnyFactcheck, data=claim2data)

# rumor 3 model
claim3_4groups <- lm(dv_MMR_new ~ Peer +  Expert + AnyFactcheck, data=claim3data)

# rumor 4 model
claim4_4groups <- lm(dv_gomutra_new ~ Peer +  Expert + AnyFactcheck, data=claim4data)

# rumor 7 model
claim7_4groups <- lm(dv_EVM_new ~ Peer +  Expert + AnyFactcheck, data=claim7data)

# rumor 8 model
claim8_4groups <- lm(dv_UNESCO_new ~ Peer +  Expert + AnyFactcheck, data=claim8data)

# rumor 9 model
claim9_4groups <- lm(dv_bose_new ~ Peer + Expert + AnyFactcheck, data=claim9data)

# overall model
overall_4groups <- lm(dv ~ Peer + Expert + AnyFactcheck, data=stacked)

# figure dwplot
c <- dwplot(list(claim1_4groups, claim2_4groups, claim3_4groups, claim4_4groups, 
                 claim7_4groups, claim8_4groups, claim9_4groups, overall_4groups),
            dot_args = list(size = 3, pch = 19),
            vline = geom_vline(
              xintercept = 0,
              colour = "grey60",
              linetype = 2)) %>%
  relabel_predictors(c("Peer1" = "Unsourced\nCorrection",
                       "Expert1" = "Expert\nCorrection",
                       "AnyFactcheck1" = "Any Factchecker\nCorrection"))
c +  theme_light() + 
  scale_colour_brewer(palette="Paired",
                      labels = c("MuslimPop", "Polygamy", "MMR", "Gomutra",
                                 "EVM", "UNESCO", "Bose", "All Headlines")) +
  xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(legend.title = element_blank())
