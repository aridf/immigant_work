library(ggplot2)
library(tidyverse)
library(aod)
library(survey)
library(mlogit)

## paths
data_in_path = 'data/'
data_out_path = 'data/'

###################################################
# convenience functions

# to check % missing from a column
check_missing <- function(varname, data = lfs) {
  print(
    paste(
      '% missing:', sum(is.na(data[,varname]))/nrow(data)*100
    )
  )
}

# to check num deleted
num_deleted <- function(num = pre, data = lfs) {
  print(
    paste(
      'num deleted:', num - nrow(data)
    )
  )
}
###################################################

##read in data
##########################
lfs <- read_csv(paste0(data_in_path, "lfs.csv"))

###################################################
# Cleaning columns for modelling
###################################################

## age  
##########################
check_missing('AGE')
table(lfs$AGE)
pre <- nrow(lfs)
lfs <- lfs[which(lfs$AGE > 17 & lfs$AGE < 66),] # remove non-working age
num_deleted()
table(lfs$AGE)

## sex
##########################
check_missing('SEX')
table(lfs$SEX)
lfs$SEX <- factor(lfs$SEX)
levels(lfs$SEX) <- c("Male", "Female")

## unemployment
##########################
check_missing('ILODEFR')
table(lfs$ILODEFR)
lfs$ACTIVE <- ifelse(lfs$ILODEFR == 1, 1, 
                     ifelse(lfs$ILODEFR == 2, 1, 0))
pre <- nrow(lfs)
lfs <- lfs[which(lfs$ACTIVE == 1),] # remove non-labor-force
num_deleted()

lfs$UNEMP <- ifelse(lfs$ILODEFR == 2, 1, 0)
lfs$UNEMP <- factor(lfs$UNEMP)
levels(lfs$UNEMP) <- c("Employed", "Unemployed")

## temporary work
##########################
check_missing('JOBTYP')
table(lfs$JOBTYP)
lfs$TEMPWRK <- ifelse(lfs$JOBTYP < 0, NA, 
                      ifelse(lfs$JOBTYP == 1, 0, 1))
lfs$TEMPWRK <- factor(lfs$TEMPWRK)
levels(lfs$TEMPWRK) <- c("Permanent", "Temporary")
table(lfs$TEMPWRK)

## part-time work
#########################
check_missing('FTPTW')
table(lfs$FTPTW)
lfs$PT <- ifelse(lfs$FTPTW < 0, NA, 
                 ifelse(lfs$FTPTW == 6, 0, 1))
lfs$PT <- factor(lfs$PT)
levels(lfs$PT) <- c("FullTime", "PartTime")
table(lfs$PT)

## involuntary part-time 
#########################
lfs$PT.INV <- ifelse(lfs$FTPTW < 0, NA, 
                     ifelse(lfs$FTPTW == 6, 0,
                            ifelse(lfs$FTPTW == 3, 1, 0)))
lfs$PT.INV <- factor(lfs$PT.INV)
levels(lfs$PT.INV) <- c("FullTime", "InvPartTime")
table(lfs$PT.INV)

## zero hour contracts
#########################
## to - do: fix
#########################
#check_missing('FLEXW7')
#table(lfs$FLEXW7)
#lfs$ZH <- ifelse(lfs$FLEXW7 == 1, 1,
#                 ifelse(lfs$FLEXW7 == 2, 0, NA))
#lfs$ZH <- factor(lfs$ZH)
#levels(lfs$ZH) <- c("NonZeroHour", "ZeroHour")
#table(lfs$ZH)

## self-employment
#########################
check_missing('STAT')
table(lfs$STAT)
lfs$SE <- ifelse(lfs$STAT == 2, 1,
                 ifelse(lfs$STAT == 1 || lfs$STAT == 3 || lfs$stat == 4, 0, NA))
lfs$SE <- factor(lfs$SE)
levels(lfs$SE) <- c("Employed", "SelfEmployed")
table(lfs$SE)

## overtime
#########################
lfs$OT <- ifelse(lfs$TTUSHR <= 0, NA, 
                 ifelse(lfs$TTUSHR > 48, 1, 0))
lfs$OT <- factor(lfs$OT)
levels(lfs$OT) <- c("NoOvertime", "Overtime")
table(lfs$OT)
table(lfs$OT, lfs$PT) 

## nonstandard hours 
lfs$NS <- ifelse(lfs$TTUSHR <= 0, NA,
                 ifelse(lfs$TTUSHR <= 20 & lfs$TTUSHR > 0, 1, 
                        ifelse(lfs$TTUSHR > 48, 2, 0)))
lfs$NS <- factor(lfs$NS)
levels(lfs$NS) <- c("FullTime", "PartTime", "Overtime")
table(lfs$NS)
table(lfs$NS, lfs$PT) 

# remove 4 workers working ot and pt
## to-do: figure out why this doesn't work
#pre <- nrow(lfs)
#lfs <- lfs[which(!(lfs$PT=='PartTime' && lfs$NS == 'Overtime')),]
#num_deleted()

##cutting down missing NS and TMPWRK values.
lfs$delete <- NA
lfs$delete[lfs$UNEMP =="Employed" & is.na(lfs$TEMPWRK)] <- "delete"
lfs$delete[lfs$UNEMP =="Employed" & is.na(lfs$NS)] <- "delete"

table(lfs$delete, lfs$NS)
table(lfs$delete, lfs$TEMPWRK)
table(lfs$delete, lfs$UNEMP)

####
# Dependent variables
####

# PW binary
lfs$PWK <- NA
lfs$PWK[lfs$UNEMP == "Unemployed"] <- "Unemployed"
lfs$PWK[lfs$TEMPWRK == "Permanent" & lfs$NS == "FullTime"] <- "FullTimePermanent"
lfs$PWK[lfs$NS == "Overtime"] <- "Precarious"
lfs$PWK[lfs$NS == "PartTime"] <- "Precarious"
lfs$PWK[lfs$TEMPWRK == "Temporary"] <- "Precarious"
table(lfs$PWK)

# PW multi
lfs$multi <- NA
lfs$multi[lfs$UNEMP == "Unemployed"] <- "UE"
lfs$multi[lfs$NS == "FullTime" & lfs$TEMPWRK == "Permanent"] <- "FTP"
lfs$multi[lfs$NS == "FullTime" & lfs$TEMPWRK == "Temporary"] <- "FTT"
lfs$multi[lfs$NS == "Overtime" & lfs$TEMPWRK == "Permanent"] <- "OTP"
lfs$multi[lfs$NS == "Overtime" & lfs$TEMPWRK == "Temporary"] <- "OTT"
lfs$multi[lfs$NS == "PartTime" & lfs$TEMPWRK == "Permanent"] <- "PTP"
lfs$multi[lfs$NS == "PartTime" & lfs$TEMPWRK == "Temporary"] <- "PTT"
table(lfs$multi)

####
# Independent Variables
####

# education groups
table(lfs$EDAGE)
lfs$EDU <- ifelse(lfs$EDAGE < 0, NA,
                  ifelse(lfs$EDAGE == 96, lfs$AGE, 
                         ifelse(lfs$EDAGE == 97, 0, 
                                ifelse(lfs$EDAGE > 16, 1, 0))))
lfs$EDU[lfs$EDAGE > 21] <- 2
lfs$EDU <- factor(lfs$EDU)
levels(lfs$EDU) <- c("LowSkilled", "MediumSkilled", "HighSkilled")
table(lfs$EDU)

# major occ. group
table(lfs$SC10MMJ)
lfs$SC10MMJ <- ifelse(lfs$SC10MMJ < 0, NA, lfs$SC10MMJ)
lfs$SC10MMJ <- factor(lfs$SC10MMJ)
levels(lfs$SC10MMJ) <- c("Managers", "Professionals", "AssociateProfessionals", 
                         "Administrative", "SkilledTrades", "CaringAndLeisure", 
                         "SalesAndCustomerService", "MachineOperatives", "ElementaryOccupations")
table(lfs$SC10MMJ)

###
# Migrant group
###
table(lfs$NATO7)
table(lfs$NATION)
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

lfs$MIGGRP <- factor(lfs$MIGGRP)
levels(lfs$MIGGRP) <- c("UK", "OldEU", "A8", "NonEUNonOECD", "NonEUOECD")
table(lfs$MIGGRP)

###
# time-varying independent variables
###

naccounts <- read.csv("data/ref/GDPgrowth.csv")
lfs <- merge(lfs, naccounts, by = "Y_Q", all = FALSE)

## turning QUARTER into factor
lfs$QUARTER <- factor(lfs$QUARTER)

## linear time trend
## adding linear and quadratic time trends
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

# write data for analysis
write_csv(lfs, paste0(data_out_path,'model_data.csv'))
