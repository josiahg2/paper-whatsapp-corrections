# replication for IJPP: online appendix
# december 28, 2022
# dependencies: first run claim 1 thru claim 9

##### table D.1 #####
claim1_h2a <- lm(dv_muslimpop_new ~ AnyCorrection * OutpartisanSpeaker, data=claim1data)
claim2_h2a <- lm(dv_polygamy_new ~ AnyCorrection * OutpartisanSpeaker, data=claim2data)
claim4_h2a <- lm(dv_gomutra_new ~ AnyCorrection * OutpartisanSpeaker, data=claim4data)
claim7_h2a <- lm(dv_EVM_new ~ AnyCorrection * OutpartisanSpeaker, data=claim7data)
claim8_h2a <- lm(dv_UNESCO_new ~ AnyCorrection * OutpartisanSpeaker, data=claim8data)

stargazer(claim1_h2a, claim2_h2a, claim4_h2a, claim7_h2a, claim8_h2a)

##### table D.2 #####
claim1_h2b <- lm(dv_muslimpop_new ~ AnyCorrection * CopartisanSpeaker, data=claim1data)
claim2_h2b <- lm(dv_polygamy_new ~ AnyCorrection * CopartisanSpeaker, data=claim2data)
claim4_h2b <- lm(dv_gomutra_new ~ AnyCorrection * CopartisanSpeaker, data=claim4data)
claim7_h2b <- lm(dv_EVM_new ~ AnyCorrection * CopartisanSpeaker, data=claim7data)
claim8_h2b <- lm(dv_UNESCO_new ~ AnyCorrection * CopartisanSpeaker, data=claim8data)

stargazer(claim1_h2b, claim2_h2b, claim4_h2b, claim7_h2b, claim8_h2b)

##### table D.3 #####
test1 <- claim1new[c("dv_muslimpop_new","AnyCorrection", "V29", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced", "OutpartisanSpeaker", "CopartisanSpeaker",
                    "CongenialClaim", "DissonantClaim")]
colnames(test1)[1] <- "dv"
test1$claim <- NA
test1$claim <- rep("claim1", nrow(test1))
test2 <- claim2new[c("dv_polygamy_new","AnyCorrection", "V29", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced", "OutpartisanSpeaker", "CopartisanSpeaker",
                    "CongenialClaim", "DissonantClaim")]
colnames(test2)[1] <- "dv"
test2$claim <- NA
test2$claim <- rep("claim2", nrow(test2))
test4 <- claim4new[c("dv_gomutra_new","AnyCorrection", "V29", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    , "DissonantMedia", "Control", "PureControl", "AnySourced", "OutpartisanSpeaker", "CopartisanSpeaker",
                    "CongenialClaim", "DissonantClaim")]
colnames(test4)[1] <- "dv"
test4$claim <- NA
test4$claim <- rep("claim4", nrow(test4))
test7 <- claim7new[c("dv_EVM_new","AnyCorrection", "V25", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    ,"DissonantMedia", "Control", "PureControl", "AnySourced", "OutpartisanSpeaker", "CopartisanSpeaker",
                    "CongenialClaim", "DissonantClaim")]
colnames(test7)[1] <- "dv"
colnames(test7)[3] <- "V29"
test7$claim <- NA
test7$claim <- rep("claim7", nrow(test7))
test8 <- claim8new[c("dv_UNESCO_new","AnyCorrection", "V25", "Peer", "Expert", "AnyFactcheck", "CongenialMedia"    
                    , "DissonantMedia", "Control", "PureControl", "AnySourced", "OutpartisanSpeaker", "CopartisanSpeaker",
                    "CongenialClaim", "DissonantClaim")]
colnames(test8)[1] <- "dv"
colnames(test8)[3] <- "V29"
test8$claim <- NA
test8$claim <- rep("claim8", nrow(test8))

stacked_d3 <- rbind(test1, test2, test4, test7, test8)


claim1_h2a_stacked <- lm(dv ~ AnyCorrection * OutpartisanSpeaker, data=stacked_d3)
summary(claim1_h2a_stacked)

claim1_h2b_stacked <- lm(dv ~ AnyCorrection * CopartisanSpeaker, data=stacked_d3)
summary(claim1_h2b_stacked)

bivariate1.variance <- vcov(claim1_h2a_stacked)
bivariate1.stderrors <- sqrt(diag(bivariate1.variance))

bivariate1.rvariance <- cluster.vcov(claim1_h2a_stacked ,cbind(stacked_new$V29, stacked_new$claim))
bivariate1.rstderrors <- sqrt(diag(bivariate1.rvariance))

bivariate2.variance <- vcov(claim1_h2b_stacked)
bivariate2.stderrors <- sqrt(diag(bivariate2.variance))

bivariate2.rvariance <- cluster.vcov(claim1_h2b_stacked ,cbind(stacked_new$V29, stacked_new$claim))
bivariate2.rstderrors <- sqrt(diag(bivariate2.rvariance))

stargazer(claim1_h2a_stacked, claim1_h2b_stacked,
          se  = list(bivariate1.rstderrors,bivariate2.rstderrors
          ), digits = 4, 
          omit.stat = c("f"), 
          notes = "All regressions include robust SEs clustered at respondent and headline levels", 
          notes.append = FALSE, notes.align = "l", 
          star.cutoffs = c(0.05, 0.01, 0.001))

##### table E.1 #####
claim1_h3 <- lm(dv_muslimpop_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim1data)
claim2_h3 <- lm(dv_polygamy_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim2data)
claim3_h3 <- lm(dv_MMR_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim3data)
claim4_h3 <- lm(dv_gomutra_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim4data)
claim7_h3 <- lm(dv_EVM_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim7data)
claim8_h3 <- lm(dv_UNESCO_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim8data)
claim9_h3 <- lm(dv_bose_new ~ AnyCorrection * CongenialMedia + AnyCorrection * DissonantMedia, 
                data=claim9data)

stargazer(claim1_h3, claim2_h3, claim3_h3, claim4_h3, claim7_h3, claim8_h3, claim9_h3)

##### table E.2 #####
h3_stacked <- lm(dv ~ AnyCorrection * CongenialMedia 
                 + AnyCorrection * DissonantMedia, data=stacked)
summary(h3_stacked)

bivariate1.variance <- vcov(h3_stacked)
bivariate1.stderrors <- sqrt(diag(bivariate1.variance))

bivariate1.rvariance <- cluster.vcov(h3_stacked ,cbind(stacked$V29, stacked$claim))
bivariate1.rstderrors <- sqrt(diag(bivariate1.rvariance))

stargazer(h3_stacked,
          se  = list(bivariate1.rstderrors), digits = 4, 
          omit.stat = c("f"), 
          notes = "All regressions include robust SEs clustered at respondent and headline levels", 
          notes.append = FALSE, notes.align = "l", 
          star.cutoffs = c(0.05, 0.01, 0.001))

##### table E.3 ##### 
# stacked h4a
claim1_h4a_stacked <- lm(dv ~ AnyCorrection * CongenialClaim, data=stacked_d3)
summary(claim1_h4a_stacked)

bivariate1.variance <- vcov(claim1_h4a_stacked)
bivariate1.stderrors <- sqrt(diag(bivariate1.variance))

# cluster SE by response ID (V29) and claim
bivariate1.rvariance <- cluster.vcov(claim1_h4a_stacked ,cbind(stacked_new$V29, stacked_new$claim))
bivariate1.rstderrors <- sqrt(diag(bivariate1.rvariance))

# stacked h4b
claim1_h4b_stacked <- lm(dv ~ AnyCorrection * DissonantClaim, data=stacked_d3)
summary(claim1_h4b_stacked)

h4b.variance <- vcov(claim1_h4b_stacked)
h4b.stderrors <- sqrt(diag(h4b.variance))

# cluster SE by response ID (V29) and claim
h4b.rvariance <- cluster.vcov(claim1_h4b_stacked ,cbind(stacked_new$V29, stacked_new$claim))
h4b.rstderrors <- sqrt(diag(h4b.rvariance))


stargazer(claim1_h4a_stacked, claim1_h4b_stacked,
          se  = list(bivariate1.rstderrors, h4b.rstderrors), digits = 4, 
          omit.stat = c("f"), 
          notes = "All regressions include robust SEs clustered at respondent and headline levels", 
          notes.append = FALSE, notes.align = "l", 
          star.cutoffs = c(0.05, 0.01, 0.001))
##### table E.4 #####
fourgroups2 <- lm(dv~ Control + Expert + AnyFactcheck, data=stacked)
summary(fourgroups2)
#cluster SE
fourgroups2.rvariance <- cluster.vcov(fourgroups2 ,cbind(stacked$V29, stacked$claim))
fourgroups2.rstderrors <- sqrt(diag(fourgroups2.rvariance))
stargazer(fourgroups2,
          se  = list(fourgroups2.rstderrors), digits = 4, 
          omit = c("state", "year"), omit.stat = c("f"), 
          notes = "All regressions include robust SEs clustered at respondent and headline levels", 
          notes.append = FALSE, notes.align = "l")

##### table G.1 #####
claim1_4groups <- lm(dv_muslimpop_new ~ Peer +  Expert + AnyFactcheck, data=claim1data)
claim2_4groups <- lm(dv_polygamy_new ~ Peer +  Expert + AnyFactcheck, data=claim2data)
claim3_4groups <- lm(dv_MMR_new ~ Peer +  Expert + AnyFactcheck, data=claim3data)
claim4_4groups <- lm(dv_gomutra_new ~ Peer +  Expert + AnyFactcheck, data=claim4data)
claim7_4groups <- lm(dv_EVM_new ~ Peer +  Expert + AnyFactcheck, data=claim7data)
claim8_4groups <- lm(dv_UNESCO_new ~ Peer +  Expert + AnyFactcheck, data=claim8data)
claim9_4groups <- lm(dv_bose_new ~ Peer + Expert + AnyFactcheck, data=claim9data)

stargazer(claim1_4groups, claim2_4groups, claim3_4groups, claim4_4groups, claim7_4groups,
          claim8_4groups, claim9_4groups, star.cutoffs = c(0.05, 0.01, 0.001))

##### table G.2 #####
claim1_3groups <- lm(dv_muslimpop_new ~ Control +  Expert + AnyFactcheck, data=claim1data)
claim2_3groups <- lm(dv_polygamy_new ~ Control +  Expert + AnyFactcheck, data=claim2data)
claim3_3groups <- lm(dv_MMR_new ~ Control +  Expert + AnyFactcheck, data=claim3data)
claim4_3groups <- lm(dv_gomutra_new ~ Control +  Expert + AnyFactcheck, data=claim4data)
claim7_3groups <- lm(dv_EVM_new ~ Control +  Expert + AnyFactcheck, data=claim7data)
claim8_3groups <- lm(dv_UNESCO_new ~ Control +  Expert + AnyFactcheck, data=claim8data)
claim9_3groups <- lm(dv_bose_new ~ Control +  Expert + AnyFactcheck, data=claim9data)

stargazer(claim1_3groups, claim2_3groups, claim3_3groups, claim4_3groups, 
          claim7_3groups, claim8_3groups, claim9_3groups, 
          star.cutoffs = c(0.05, 0.01, 0.001))


##### figure G.1 #####
c <- dwplot(list(claim1_3groups, claim2_3groups, claim3_3groups, claim4_3groups, 
                 claim7_3groups, claim8_3groups, claim9_3groups),
            dot_args = list(size = 3, pch = 19),
            vline = geom_vline(
              xintercept = 0,
              colour = "grey60",
              linetype = 2)) %>%
  relabel_predictors(c("Control1" = "Control",
                       "Expert1" = "Expert\nCorrection",
                       "AnyFactcheck1" = "Any Factchecker\nCorrection"))
c +  theme_light() + 
  scale_colour_brewer(palette="Paired",
                      labels = c("MuslimPop", "Polygamy", "MMR", "Gomutra",
                                 "EVM", "UNESCO", "Bose")) +
  xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(legend.title = element_blank())

##### figure G.2 #####
claim1_2groups <- lm(dv_muslimpop_new ~ Control +  AnySourced, data=claim1data)
claim2_2groups <- lm(dv_polygamy_new ~ Control + AnySourced, data=claim2data)
claim3_2groups <- lm(dv_MMR_new ~ Control + AnySourced, data=claim3data)
claim4_2groups <- lm(dv_gomutra_new ~ Control + AnySourced, data=claim4data)
claim7_2groups <- lm(dv_EVM_new ~ Control + AnySourced, data=claim7data)
claim8_2groups <- lm(dv_UNESCO_new ~ Control + AnySourced, data=claim8data)
claim9_2groups <- lm(dv_bose_new ~ Control + AnySourced, data=claim9data)

t <- dwplot(list(claim1_2groups, claim2_2groups, claim3_2groups, claim4_2groups, 
                 claim7_2groups, claim8_2groups, claim9_2groups), 
            dot_args = list(size = 3, pch = 19),
            vline = geom_vline(
              xintercept = 0,
              colour = "grey60",
              linetype = 2)) %>%
  relabel_predictors(c("Control1" = "Control",
                       "AnySourced1" = "All Sourced\nCorrections"))
t +  theme_light() + 
  scale_colour_brewer(palette="Paired",
                      labels = c("MuslimPop", "Polygamy", "MMR", "Gomutra",
                                 "EVM", "UNESCO", "Bose")) +
  xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(legend.title = element_blank())
##### table G.3 #####
claim1_corrections <- lm(dv_muslimpop_new ~ Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim1data)
claim2_corrections <- lm(dv_polygamy_new ~ Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim2data)
claim3_corrections <- lm(dv_MMR_new ~  Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim3data)
claim4_corrections <- lm(dv_gomutra_new ~ Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim4data)
claim7_corrections <- lm(dv_EVM_new ~ Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim7data)
claim8_corrections <- lm(dv_UNESCO_new ~ Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim8data)
claim9_corrections <- lm(dv_bose_new ~ Peer +  Expert + AltNews + Vishwas + 
                           TOI + Facebook + WhatsApp, data=claim9data)

stargazer(claim1_corrections, claim2_corrections, claim3_corrections, claim4_corrections,
          claim7_corrections, claim8_corrections, 
          claim9_corrections)




