##clear environment
rm(list = ls())

##set directory
setwd("C:/Users/Ari/Documents/Dissertation Data/UK-LFS-complete")

##some libraries
library(foreign)
library(ggplot2)
library(tidyverse)
library(aod)
library(survey)
library(tibble)
library(mlogit)

# paths
data_in_path = 'data'
model_out_path = 'results/models'
plot_out_path = 'results/plots'

##read in fresh data
##########################
lfs <- read_csv("lfs07.16.dta", convert.factors = FALSE)

##########################
## SETTING UP VARIABLES
##########################

##cut down to wave 1
##########################
lfs <- lfs[which(lfs$THISWV == 1),]

##cut down to working age 
##########################
lfs <- lfs[which(lfs$AGE > 15 & lfs$AGE < 66),]

##sex
##########################
count(lfs$SEX)
lfs$SEX <- factor(lfs$SEX)
levels(lfs$SEX) <- c("Male", "Female")
count(lfs$SEX)

##UNEMPLOYMENT
##########################
count(lfs$ILODEFR)
lfs$ACTIVE <- ifelse(lfs$ILODEFR == 1, 1, 
              ifelse(lfs$ILODEFR == 2, 1, 0))
lfs <- lfs[which(lfs$ACTIVE == 1),]
count(lfs$ILODEFR)
lfs$UNEMP <- ifelse(lfs$ILODEFR == 2, 1, 0)
lfs$UNEMP <- factor(lfs$UNEMP)
levels(lfs$UNEMP) <- c("Employed", "Unemployed")
count(lfs$UNEMP)

##Creating sample of employed workers
#########################

##PRECARIOUS WORK
##########################
##TEMP WORK
count(lfs$JOBTYP)
lfs$TEMPWRK <- ifelse(lfs$JOBTYP < 0, NA, 
               ifelse(lfs$JOBTYP == 1, 0, 1))
count(lfs$TEMPWRK)
lfs$TEMPWRK <- factor(lfs$TEMPWRK)
levels(lfs$TEMPWRK) <- c("Permanent", "Temporary")
count(lfs$TEMPWRK)

##ALL PART TIME WORK
count(lfs$FTPTW)
lfs$PT <- ifelse(lfs$FTPTW < 0, NA, 
          ifelse(lfs$FTPTW == 6, 0, 1))
lfs$PT <- factor(lfs$PT)
levels(lfs$PT) <- c("FullTime", "PartTime")
count(lfs$PT)

##INVOLUNTARY PART TIME WORK
lfs$PT.INV <- ifelse(lfs$FTPTW < 0, NA, 
              ifelse(lfs$FTPTW == 6, 0,
              ifelse(lfs$FTPTW == 3, 1, 0)))
lfs$PT.INV <- factor(lfs$PT.INV)
levels(lfs$PT.INV) <- c("FullTime", "InvoluntaryPartTime")
count(lfs$PT.INV)

##ZERO HOURS CONTRACTS
count(lfs$FLEXW7)
lfs$ZH <- ifelse(lfs$FLEXW7 == 1, 1,
          ifelse(lfs$FLEXW7 == 2, 0, NA))
lfs$ZH <- factor(lfs$ZH)
levels(lfs$ZH) <- c("NonZeroHour", "ZeroHour")
count(lfs$ZH)

##Self-employment
count(lfs$STAT)
lfs$SE <- ifelse(lfs$STAT == 2, 1,
          ifelse(lfs$STAT == 1 || lfs$STAT == 3 || lfs$stat == 4, 0, NA))
lfs$SE <- factor(lfs$SE)
levels(lfs$SE) <- c("Employee", "SelfEmployed")
count(lfs$SE)

table(lfs$TEMPWRK, lfs$SE)
table(lfs$MIGGRP, lfs$SE)

##Overtime
lfs$OT <- ifelse(lfs$TTUSHR <= 0, NA, 
          ifelse(lfs$TTUSHR > 45, 1, 0))
lfs$OT <- factor(lfs$OT)
levels(lfs$OT) <- c("NoOvertime", "Overtime")
count(lfs$OT)

##NonStandard Hours
lfs$NS <- ifelse(lfs$TTUSHR <= 0, NA,
          ifelse(lfs$TTUSHR <= 20 & lfs$TTUSHR > 0, 1, 
          ifelse(lfs$TTUSHR > 48, 2, 0)))
lfs$NS <- factor(lfs$NS)
levels(lfs$NS) <- c("FullTime", "PartTime", "Overtime")
count(lfs$NS)

##cutting down missing NS and TMPWRK values.
lfs$delete[lfs$UNEMP =="Employed" & is.na(lfs$TEMPWRK)] <- "delete"
lfs$delete[lfs$UNEMP =="Employed" & is.na(lfs$NS)] <- "delete"

xtabs(~delete+NS, lfs)
xtabs(~delete+TEMPWRK, lfs)
xtabs(~delete+UNEMP, lfs)


lfs <- lfs[which(is.na(lfs$delete)),]

####Dependent Variables
##precwork binomial
lfs$PWK[lfs$UNEMP == "Unemployed"] <- "Unemployed"
lfs$PWK[lfs$TEMPWRK == "Permanent" & lfs$NS == "FullTime"] <- "FullTimePermanent"
lfs$PWK[lfs$NS == "Overtime"] <- "Precarious"
lfs$PWK[lfs$NS == "PartTime"] <- "Precarious"
lfs$PWK[lfs$TEMPWRK == "Temporary"] <- "Precarious"
count(lfs$PWK)

##multinomial 
lfs$multi[lfs$UNEMP == "Unemployed"] <- "UE"
lfs$multi[lfs$NS == "FullTime" & lfs$TEMPWRK == "Permanent"] <- "FTP"
lfs$multi[lfs$NS == "FullTime" & lfs$TEMPWRK == "Temporary"] <- "FTT"
lfs$multi[lfs$NS == "Overtime" & lfs$TEMPWRK == "Permanent"] <- "OTP"
lfs$multi[lfs$NS == "Overtime" & lfs$TEMPWRK == "Temporary"] <- "OTT"
lfs$multi[lfs$NS == "PartTime" & lfs$TEMPWRK == "Permanent"] <- "PTP"
lfs$multi[lfs$NS == "PartTime" & lfs$TEMPWRK == "Temporary"] <- "PTT"
count(lfs$multi)

##SKILL GROUPS
##########################
count(lfs$EDAGE)
lfs$EDU <- ifelse(lfs$EDAGE < 0, NA,
           ifelse(lfs$EDAGE == 96, lfs$AGE, 
           ifelse(lfs$EDAGE == 97, 0, 
           ifelse(lfs$EDAGE > 16, 1, 0))))
lfs$EDU[lfs$EDAGE > 21] <- 2
lfs$EDU <- factor(lfs$EDU)
levels(lfs$EDU) <- c("LowSkilled", "MediumSkilled", "HighSkilled")
count(lfs$EDU)

##MAJOR OCCUPATION GROUPS
##########################
count(lfs$SC10MMJ)
lfs$SC10MMJ <- ifelse(lfs$SC10MMJ < 0, NA, lfs$SC10MMJ)
count(lfs$SC10MMJ)
lfs$SC10MMJ <- factor(lfs$SC10MMJ)
levels(lfs$SC10MMJ) <- c("Managers", "Professionals", "AssociateProfessionals", "Administrative", "SkilledTrades", "CaringAndLeisure", "SalesAndCustomerService", "MachineOperatives", "ElementaryOccupations")
count(lfs$SC10MMJ)

##MIGRANT GROUPS
##########################
count(lfs$NATO7)
count(lfs$NATION)
##Non-EU = 3
lfs$MIGGRP <- ifelse(lfs$NATO7 > 0, 3, NA)

##Non-EU OECD = 4
##includes Australia, Canada, Chile, Israel, Japan, Korea, Mexico, Turkey, and United States
lfs$MIGGRP <- ifelse(lfs$NATO7 == 036, 4, 
                      ifelse(lfs$NATO7 == 124, 4, 
                      ifelse(lfs$NATO7 == 152, 4,
                      ifelse(lfs$NATO7 == 376, 4, 
                      ifelse(lfs$NATO7 == 392, 4,
                      ifelse(lfs$NATO7 == 410, 4, 
                      ifelse(lfs$NATO7 == 484, 4, 
                      ifelse(lfs$NATO7 == 840, 4, 
                      ifelse(lfs$NATO7 == 792, 4, lfs$MIGGRP)))))))))

##UK = 0
lfs$MIGGRP <- ifelse(lfs$NATION == 926, 0, lfs$MIGGRP)

##Old EU = 1
##includes Austria, Belgium, Denmark, Finland, France, Germany, Greece, Ireland, Italy, Luxembourg, Netherlands, Portugal, and Sweden
##CAR recode function***
lfs$MIGGRP <- ifelse(lfs$NATO7 == 040, 1, 
                    ifelse(lfs$NATO7 == 056, 1,
                    ifelse(lfs$NATO7 == 208, 1,
                    ifelse(lfs$NATO7 == 246, 1,
                    ifelse(lfs$NATO7 == 250, 1,
                    ifelse(lfs$NATO7 == 276, 1,
                    ifelse(lfs$NATO7 == 300, 1,
                    ifelse(lfs$NATO7 == 372, 1,
                    ifelse(lfs$NATO7 == 380, 1,
                    ifelse(lfs$NATO7 == 442, 1,
                    ifelse(lfs$NATO7 == 528, 1,
                    ifelse(lfs$NATO7 == 620, 1,
                    ifelse(lfs$NATO7 == 911, 1,
                    ifelse(lfs$NATO7 == 913, 1,
                    ifelse(lfs$NATO7 == 752, 1, lfs$MIGGRP)))))))))))))))

##A8 = 2
## includes Czech Republic, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, and Slovenia. Does not include Cyprus and Malta 
lfs$MIGGRP <- ifelse(lfs$NATO7 == 203, 2, 
                    ifelse(lfs$NATO7 == 233, 2,
                    ifelse(lfs$NATO7 == 348, 2,
                    ifelse(lfs$NATO7 == 428, 2,
                    ifelse(lfs$NATO7 == 440, 2,
                    ifelse(lfs$NATO7 == 616, 2,
                    ifelse(lfs$NATO7 == 703, 2,
                    ifelse(lfs$NATO7 == 705, 2, lfs$MIGGRP))))))))
count(lfs$MIGGRP)
lfs$MIGGRP <- factor(lfs$MIGGRP)
levels(lfs$MIGGRP) <- c("UK", "OldEU", "A8", "NonEUNonOECD", "NonEUOECD")
count(lfs$MIGGRP)

                                                        
##GDP per capita change over time
##########################

naccounts <- read.csv("GDPgrowth.csv")
lfs <- merge(lfs, naccounts, by = "Y_Q", all = FALSE)

##turning QUARTER into factor
lfs$QUARTER <- factor(lfs$QUARTER)
count(lfs$QUARTER)


##adding linear and quadratic time trends
##########################
lfs$TLINEAR[lfs$Y_Q == "2007.1"] <- 1
lfs$TLINEAR[lfs$Y_Q == "2007.2"] <- 2
lfs$TLINEAR[lfs$Y_Q == "2007.3"] <- 3
lfs$TLINEAR[lfs$Y_Q == "2007.4"] <- 4
lfs$TLINEAR[lfs$Y_Q == "2008.1"] <- 5
lfs$TLINEAR[lfs$Y_Q == "2008.2"] <- 6
lfs$TLINEAR[lfs$Y_Q == "2008.3"] <- 7
lfs$TLINEAR[lfs$Y_Q == "2008.4"] <- 8
lfs$TLINEAR[lfs$Y_Q == "2009.1"] <- 9
lfs$TLINEAR[lfs$Y_Q == "2009.2"] <- 10
lfs$TLINEAR[lfs$Y_Q == "2009.3"] <- 11
lfs$TLINEAR[lfs$Y_Q == "2009.4"] <- 12
lfs$TLINEAR[lfs$Y_Q == "2010.1"] <- 13
lfs$TLINEAR[lfs$Y_Q == "2010.2"] <- 14
lfs$TLINEAR[lfs$Y_Q == "2010.3"] <- 15
lfs$TLINEAR[lfs$Y_Q == "2010.4"] <- 16
lfs$TLINEAR[lfs$Y_Q == "2011.1"] <- 17
lfs$TLINEAR[lfs$Y_Q == "2011.2"] <- 18
lfs$TLINEAR[lfs$Y_Q == "2011.3"] <- 19
lfs$TLINEAR[lfs$Y_Q == "2011.4"] <- 20
lfs$TLINEAR[lfs$Y_Q == "2012.1"] <- 21
lfs$TLINEAR[lfs$Y_Q == "2012.2"] <- 22
lfs$TLINEAR[lfs$Y_Q == "2012.3"] <- 23
lfs$TLINEAR[lfs$Y_Q == "2012.4"] <- 24
lfs$TLINEAR[lfs$Y_Q == "2013.1"] <- 25
lfs$TLINEAR[lfs$Y_Q == "2013.2"] <- 26
lfs$TLINEAR[lfs$Y_Q == "2013.3"] <- 27
lfs$TLINEAR[lfs$Y_Q == "2013.4"] <- 28
lfs$TLINEAR[lfs$Y_Q == "2014.1"] <- 29
lfs$TLINEAR[lfs$Y_Q == "2014.2"] <- 30
lfs$TLINEAR[lfs$Y_Q == "2014.3"] <- 31
lfs$TLINEAR[lfs$Y_Q == "2014.4"] <- 32
lfs$TLINEAR[lfs$Y_Q == "2015.1"] <- 33
lfs$TLINEAR[lfs$Y_Q == "2015.2"] <- 34
lfs$TLINEAR[lfs$Y_Q == "2015.3"] <- 35
lfs$TLINEAR[lfs$Y_Q == "2015.4"] <- 36
lfs$TLINEAR[lfs$Y_Q == "2016.1"] <- 37
lfs$TLINEAR[lfs$Y_Q == "2016.2"] <- 38
lfs$TLINEAR[lfs$Y_Q == "2016.3"] <- 39
lfs$TLINEAR[lfs$Y_Q == "2016.4"] <- 40

lfs$TLINEAR_F = factor(lfs$TLINEAR)

lfs$TQUAD <- lfs$TLINEAR*lfs$TLINEAR

write.dta(lfs, "C:/Users/Ari/Documents/Dissertation Data/UK-LFS-complete/lfs.PrecMig.dta")

########################################################################################################
########################################################################################################
##read in fresh data
##########################
lfs <- read.dta("lfs.PrecMig.dta")
table(lfs$PWK, lfs$MIGGRP)
table(lfs$multi, lfs$MIGGRP)
##creating weighted survey object
##########################
lfsw <- svydesign(id=lfs$CASENOP, weights=lfs$PWT14, data=lfs)
summary(lfsw)
##for if analyzing zero-hour contracts
lfsz <- lfs[which(lfs$QUARTER=="2" | lfs$QUARTER=="4"),]
lfszw <- svydesign(id=lfsz$CASENOP, weights=lfsz$PWT14, data=lfsz)
########################################################################################################
########################################################################################################

##sample descriptives
##########################
svymean(~SEX+AGE+EDU+UNEMP, lfsw, na.rm=TRUE)

UK <- subset(lfsw, MIGGRP=="UK")
a1 <- svymean(~SEX+AGE+EDU+SC10MMJ, UK, na.rm=TRUE)

OldEU <- subset(lfsw, MIGGRP=="OldEU")
a2 <- svymean(~SEX+AGE+EDU+SC10MMJ, OldEU, na.rm=TRUE)

A8 <- subset(lfsw, MIGGRP=="A8")
a3 <- svymean(~SEX+AGE+EDU+SC10MMJ, A8, na.rm=TRUE)

NonEUNonOECD <- subset(lfsw, MIGGRP=="NonEUNonOECD")
a4 <- svymean(~SEX+AGE+EDU+SC10MMJ, NonEUNonOECD, na.rm=TRUE)

NonEUOECD <- subset(lfsw, MIGGRP=="NonEUOECD")
a5 <- svymean(~SEX+AGE+EDU+SC10MMJ, NonEUOECD, na.rm=TRUE)

a <- cbind(a1, a2, a3, a4, a5)


##PrecWork Descriptives
##########################

t1 <- svytable(~PWK+MIGGRP, lfsw, Ntotal=33500000)
t1 <- as.data.frame.matrix(t1)
t1$order <- c(1, 4, 2, 3)
t1 <- dplyr::arrange(t1, order)
t1 <- dplyr::select(t1, -order)
t1 <- dplyr::mutate(t1, UK_Prop = UK/sum(UK)*100)
t1 <- dplyr::mutate(t1, OldEU_Prop = OldEU/sum(OldEU)*100)
t1 <- dplyr::mutate(t1, A8_Prop = A8/sum(A8)*100)
t1 <- dplyr::mutate(t1, NonEUNonOECD_Prop = NonEUNonOECD/sum(NonEUNonOECD)*100)
t1 <- dplyr::mutate(t1, NonEUOECD_Prop = NonEUOECD/sum(NonEUOECD)*100)
rownames(t1) <- c("Full Time Permanent", "Precarious", "Unemployed")
t1

t2 <- svytable(~multi+MIGGRP, lfsw, Ntotal=33500000)
t2 <- as.data.frame.matrix(t2)
t2$order <- c(1, 2, 8, 3, 4, 5, 6, 7)
t2 <- dplyr::arrange(t2, order)
t2 <- dplyr::select(t2, -order)
t2 <- dplyr::mutate(t2, UK_Prop = UK/sum(UK)*100)
t2 <- dplyr::mutate(t2, OldEU_Prop = OldEU/sum(OldEU)*100)
t2 <- dplyr::mutate(t2, A8_Prop = A8/sum(A8)*100)
t2 <- dplyr::mutate(t2, NonEUNonOECD_Prop = NonEUNonOECD/sum(NonEUNonOECD)*100)
t2 <- dplyr::mutate(t2, NonEUOECD_Prop = NonEUOECD/sum(NonEUOECD)*100)
rownames(t2) <- c("Full Time Permanent", "Full Time Temporary", "Overtime Permanent", "Overtime Temporary", "Part Time Permanent", "Part Time Temporary", "Unemployed")
t2

t3 <- svytable(~multi+MIGGRP, lfsw, Ntotal=355068)
t3 <- as.data.frame.matrix(t3)
t3$order <- c(1, 2, 8, 3, 4, 5, 6, 7)
t3 <- dplyr::arrange(t3, order)
t3 <- dplyr::select(t3, -order)
t3 <- dplyr::mutate(t3, UK_Prop = UK/sum(UK)*100)
t3 <- dplyr::mutate(t3, OldEU_Prop = OldEU/sum(OldEU)*100)
t3 <- dplyr::mutate(t3, A8_Prop = A8/sum(A8)*100)
t3 <- dplyr::mutate(t3, NonEUNonOECD_Prop = NonEUNonOECD/sum(NonEUNonOECD)*100)
t3 <- dplyr::mutate(t3, NonEUOECD_Prop = NonEUOECD/sum(NonEUOECD)*100)
rownames(t3) <- c("Full Time Permanent", "Full Time Temporary", "Overtime Permanent", "Overtime Temporary", "Part Time Permanent", "Part Time Temporary", "Unemployed")
t3

##Graphics attempt
##########################

###Graphing Multi by MIGGRP (unweighted)
lfs1 <- lfs[which(lfs$multi != "FTP"),]
lfs2 <- lfs[which(lfs$MULTI2 != "FTP"),]
ggplot(data=lfs1[!is.na(lfs1$MIGGRP) & !is.na(lfs1$multi),], aes(x=multi, group=MIGGRP)) +
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat="count") +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1), expand=c(0,0)) +
  facet_grid(~MIGGRP) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid=element_blank()) +
  ylab("Relative Proportion") 

ggplot(data=lfs2[!is.na(lfs2$MIGGRP) & !is.na(lfs2$MULTI2),], aes(x=MULTI2, group=MIGGRP)) +
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat="count") +
  scale_y_continuous(labels=scales::percent, limits=c(0, .80), expand=c(0,0)) +
  facet_grid(~MIGGRP) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid=element_blank()) +
  ylab("Relative Proportion") +
  scale_fill_discrete(name="Employment Status",
                      breaks=c("1", "2", "3"),
                      labels=c("Full-time temporary", "Part-time permanent", "Part-time temporary")) 

##Modelling
##########################

##multinomial logit models for each DV across all data
#####################################################
lfs.models <- subset(lfs, select=c(CASENOP, SE, ZH, multi, PWK, MIGGRP, SEX, AGE, EDU, SC10MMJ, TLINEAR, TLINEAR_F, TQUAD, QUARTER, PWT14))
lfs.models <- dplyr::filter(lfs.models, multi != "PTT", multi != "OTT")

write.dta(lfs.models, "C:/Users/Ari/Documents/Dissertation Data/UK-LFS-complete/lfs.models.dta")

lfs.models <- read.dta("lfs.models.dta")

lfs.models.All <- dplyr::filter(lfs.models)
lfs.models.NoUE <- dplyr::filter(lfs.models, multi != "UE", PWK != "UE")



##PW
lfsm1 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="PWK")
f1 <- mFormula(PWK~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F+SC10MMJ)
m1 <- mlogit(f1, lfsm1, weights=PWT14)
summary(m1)
odds1 <- exp(coef(m1))
odds1 <- enframe(odds1)
odds1 <- dplyr::filter(odds1, grepl('MIGGRP', name))
odds1$name <- factor(odds1$name, levels=unique(as.character(odds1$name)))
levels(odds1$name) <- c("Old EU",
                       "A8",
                       "Non-OECD",
                       "OECD")
odds1$odds <- "Precarious Work"

##Unemployment
lfsm2 <- mlogit.data(lfs.models.All, shape="wide", choice="PWK")
f2 <- mFormula(PWK~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F)
m2 <- mlogit(f2, lfsm2, weights=PWT14)
summary(m2)
odds2 <- exp(coef(m2))
odds2 <- enframe(odds2)
odds2 <- dplyr::filter(odds2, grepl('Unemployed:MIGGRP', name))
odds2$name <- factor(odds2$name, levels=unique(as.character(odds2$name)))
levels(odds2$name) <- c("Old EU",
                        "A8",
                        "Non-OECD",
                        "OECD")
odds2$odds <- "Unemployment"

##PW UE graph
odds <- rbind(odds2, odds1)
odds$sig <- c("***", "***", "***", "", "",
               "***", "", "***")

png(filename = "pw_ue_plot.png", width = 5, height = 6, units = "in", res = 1000)
ggplot(data=odds, aes(x=name, group=odds, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 2), expand=c(0,0)) +
  facet_grid(odds~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus UK nationals") +
  geom_hline(aes(yintercept=1))
dev.off()

##multi
#memory.limit(50000)
#lfsm3 <- mlogit.data(lfs.models.All, shape="wide", choice="multi")
#f3 <- mFormula(multi~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F+SC10MMJ)
#m3 <- mlogit(f3, lfsm3, weights=PWT14)
#m3.RDatasummary(m3)
#save(m3, file="m3.RData")
load("m3.RData")
odds3 <- exp(coef(m3))
odds3 <- enframe(odds3)
odds3 <- dplyr::filter(odds3, grepl('MIGGRP', name))
odds3$name <- factor(odds3$name, levels=unique(as.character(odds3$name)))
odds3$MIGGRP <- c("Old EU", "Old EU", "Old EU", "A8",
                  "A8", "A8", "Non-OECD", "Non-OECD",
                  "Non-OECD", "OECD",  "OECD",  "OECD")
levels(odds3$name) <- c("Full Time Temporary",
                       "Overtime Permanent", 
                       "Part-time Permanent",
                       "Full Time Temporary",
                       "Overtime Permanent", 
                       "Part-time Permanent",
                       "Full Time Temporary",
                       "Overtime Permanent", 
                       "Part-time Permanent",
                       "Full Time Temporary",
                       "Overtime Permanent", 
                       "Part-time Permanent")
odds3$sig <- c("***", "***", "***", "***", "***",
              "***", "***", "", "***", "***",
              "***", "")

png(filename = "multi_plot.png", width = 7, height = 8, units = "in", res = 2000)
ggplot(data=odds3, aes(x=name, group=MIGGRP, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 2), expand=c(0,0)) +
  facet_grid(MIGGRP~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus UK nationals") +
  geom_hline(aes(yintercept=1))
dev.off()

###############################################
########Unadjusted Models
###############################################

##PW
lfsm4 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="PWK")
f4 <- mFormula(PWK~1 | MIGGRP+TLINEAR_F)
m4 <- mlogit(f4, lfsm4, weights=PWT14)
summary(m4)
odds4 <- exp(coef(m4))
odds4 <- enframe(odds4)
odds4 <- dplyr::filter(odds4, grepl('MIGGRP', name))
odds4$name <- factor(odds4$name, levels=unique(as.character(odds4$name)))
levels(odds4$name) <- c("Old EU",
                        "A8",
                        "Non-OECD",
                        "OECD")
odds4$odds <- "Precarious Work"

##Unemployment
lfsm5 <- mlogit.data(lfs.models.All, shape="wide", choice="PWK")
f5 <- mFormula(PWK~1 | MIGGRP+TLINEAR_F)
m5 <- mlogit(f5, lfsm5, weights=PWT14)
summary(m5)
odds5 <- exp(coef(m5))
odds5 <- enframe(odds5)
odds5 <- dplyr::filter(odds5, grepl('Unemployed:MIGGRP', name))
odds5$name <- factor(odds5$name, levels=unique(as.character(odds5$name)))
levels(odds5$name) <- c("Old EU",
                        "A8",
                        "Non-OECD",
                        "OECD")
odds5$odds <- "Unemployment"

##PW UE graph
odds <- rbind(odds5, odds4)
odds$sig <- c("***", "**", "***", "", "***",
              "", "***", "***")
ggplot(data=odds, aes(x=name, group=odds, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 2), expand=c(0,0)) +
  facet_grid(odds~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus UK nationals") +
  geom_hline(aes(yintercept=1))

##multi
memory.limit(50000)
lfsm6 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="multi")
f6 <- mFormula(multi~1 | MIGGRP+TLINEAR_F)
m6 <- mlogit(f6, lfsm6, weights=PWT14)
summary(m6)
odds6 <- exp(coef(m6))
odds6 <- enframe(odds6)
odds6 <- dplyr::filter(odds6, grepl('MIGGRP', name))
odds6$name <- factor(odds6$name, levels=unique(as.character(odds6$name)))
odds6$MIGGRP <- c("Old EU", "Old EU", "Old EU", "A8",
                  "A8", "A8", "Non-OECD", "Non-OECD",
                  "Non-OECD", "OECD",  "OECD",  "OECD")
levels(odds6$name) <- c("Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent")
odds6$sig <- c("***", "***", "**", "***", "",
               "***", "***", "", "***", "***",
               "***", "")
ggplot(data=odds6, aes(x=name, group=MIGGRP, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 3), expand=c(0,0)) +
  facet_grid(MIGGRP~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus UK nationals") +
  geom_hline(aes(yintercept=1))



###############################################
########A8 as reference group
###############################################


lfs.models <- read.dta("lfs.models.dta")
lfs.models <- within(lfs.models, MIGGRP <- relevel(MIGGRP, ref = "A8"))

lfs.models.All <- dplyr::filter(lfs.models)
lfs.models.NoUE <- dplyr::filter(lfs.models, multi != "UE", PWK != "UE")


##PW
lfsm7 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="PWK")
f7 <- mFormula(PWK~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F+SC10MMJ)
m7 <- mlogit(f7, lfsm7, weights=PWT14)
summary(m7)
odds7 <- exp(coef(m7))
odds7 <- enframe(odds7)
odds7 <- dplyr::filter(odds7, grepl('MIGGRP', name))
odds7$name <- factor(odds7$name, levels=unique(as.character(odds7$name)))
levels(odds7$name) <- c("UK",
                        "Old EU",
                        "Non-OECD",
                        "OECD")
odds7$odds <- "Precarious Work"

##Unemployment
lfsm8 <- mlogit.data(lfs.models.All, shape="wide", choice="PWK")
f8 <- mFormula(PWK~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F)
m8 <- mlogit(f8, lfsm8, weights=PWT14)
summary(m8)
odds8 <- exp(coef(m8))
odds8 <- enframe(odds8)
odds8 <- dplyr::filter(odds8, grepl('Unemployed:MIGGRP', name))
odds8$name <- factor(odds8$name, levels=unique(as.character(odds8$name)))
levels(odds8$name) <- c("UK",
                        "Old EU",
                        "Non-OECD",
                        "OECD")
odds8$odds <- "Unemployment"

##PW UE graph
odds <- rbind(odds8, odds7)
odds$sig <- c("***", "***", "**", "***", "***",
              "***", "***", "***")
ggplot(data=odds, aes(x=name, group=odds, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 2.5), expand=c(0,0)) +
  facet_grid(odds~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus A8 migrants") +
  geom_hline(aes(yintercept=1))


##multi
memory.limit(50000)
lfsm9 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="multi")
f9 <- mFormula(multi~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F+SC10MMJ)
m9 <- mlogit(f9, lfsm9, weights=PWT14)
summary(m9)
save(m9, file="m9.RData")
odds9 <- exp(coef(m9))
odds9 <- enframe(odds9)
odds9 <- dplyr::filter(odds9, grepl('MIGGRP', name))
odds9$name <- factor(odds9$name, levels=unique(as.character(odds9$name)))
odds9$MIGGRP <- c("UK", "UK", "UK", 
                  "Old EU", "Old EU", "Old EU",
                  "Non-OECD", "Non-OECD", "Non-OECD", 
                  "OECD",  "OECD",  "OECD")
levels(odds9$name) <- c("Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent")
odds9$sig <- c("***", "***", "***", "", "*",
               "***", "", "***", "***", "",
               "***", "***")
ggplot(data=odds9, aes(x=name, group=MIGGRP, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 4.5), expand=c(0,0)) +
  facet_grid(MIGGRP~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus A8 migrants") +
  geom_hline(aes(yintercept=1))


###############################################
########OECD as reference group
###############################################


lfs.models <- read.dta("lfs.models.dta")
lfs.models <- within(lfs.models, MIGGRP <- relevel(MIGGRP, ref = "NonEUOECD"))

lfs.models.All <- dplyr::filter(lfs.models)
lfs.models.NoUE <- dplyr::filter(lfs.models, multi != "UE", PWK != "UE")


##PW
lfsm10 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="PWK")
f10 <- mFormula(PWK~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F+SC10MMJ)
m10 <- mlogit(f10, lfsm10, weights=PWT14)
summary(m10)
odds10 <- exp(coef(m10))
odds10 <- enframe(odds10)
odds10 <- dplyr::filter(odds10, grepl('MIGGRP', name))
odds10$name <- factor(odds10$name, levels=unique(as.character(odds10$name)))
levels(odds10$name) <- c("UK",
                        "Old EU",
                        "A8",
                        "Non-OECD")
odds10$odds <- "Precarious Work"

##Unemployment
lfsm11 <- mlogit.data(lfs.models.All, shape="wide", choice="PWK")
f11 <- mFormula(PWK~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F)
m11 <- mlogit(f11, lfsm11, weights=PWT14)
summary(m11)
odds11 <- exp(coef(m11))
odds11 <- enframe(odds11)
odds11 <- dplyr::filter(odds11, grepl('Unemployed:MIGGRP', name))
odds11$name <- factor(odds11$name, levels=unique(as.character(odds11$name)))
levels(odds11$name) <- c("UK",
                        "Old EU",
                        "A8",
                        "Non-OECD")
odds11$odds <- "Unemployment"

##PW UE graph
odds <- rbind(odds10, odds11)
odds$sig <- c("***", "***", "***", "***", "",
              "*", "**", "**")
ggplot(data=odds, aes(x=name, group=odds, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 2), expand=c(0,0)) +
  facet_grid(odds~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus OECD migrants") +
  geom_hline(aes(yintercept=1))


##multi
memory.limit(50000)
lfsm12 <- mlogit.data(lfs.models.NoUE, shape="wide", choice="multi")
f12 <- mFormula(multi~1 | MIGGRP+SEX+AGE+EDU+TLINEAR_F+SC10MMJ)
m12 <- mlogit(f12, lfsm12, weights=PWT14)
summary(m12)
save(m12, file="m12.RData")
odds12 <- exp(coef(m12))
odds12 <- enframe(odds12)
odds12 <- dplyr::filter(odds12, grepl('MIGGRP', name))
odds12$name <- factor(odds12$name, levels=unique(as.character(odds12$name)))
odds12$MIGGRP <- c("UK", "UK", "UK", 
                   "Old EU", "Old EU", "Old EU",
                   "A8", "A8", "A8",
                   "Non-OECD", "Non-OECD", "Non-OECD")
levels(odds12$name) <- c("Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent",
                        "Full Time Temporary",
                        "Overtime Permanent", 
                        "Part-time Permanent")
odds12$sig <- c("***", "***", "", "*", "***",
               "***", "", "***", "***", "*",
               "***", "")
ggplot(data=odds12, aes(x=name, group=MIGGRP, y=value)) +
  geom_point(size=3) +
  coord_flip() +
  theme(axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_continuous(limits=c(0, 2), expand=c(0,0)) +
  facet_grid(MIGGRP~., scales="free", space="free" ) +
  geom_text(aes(label=sig, vjust=-0.1)) +
  ylab("Odds ratio versus OECD migrants") +
  geom_hline(aes(yintercept=1))
