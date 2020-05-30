library(ggplot2)
library(tidyverse)
library(aod)
library(survey)
library(mlogit)

# Path
# ===
data_in_path = 'data/model_data.csv'
model_out_path = 'data/results/models/'
plot_out_path = 'data/results/plots/'

# Read in data
# ===
df <- read_csv(data_in_path)
dfw <- svydesign(id=df$CASENOP, weights=df$PWT14, data = df) # weighted dataset

# Covariate descriptives
# ===

svymean(~SEX+AGE+EDU+UNEMP, dfw, na.rm=TRUE)

UK <- subset(dfw, MIGGRP=="UK")
a1 <- svymean(~SEX+AGE+EDU+SC10MMJ, UK, na.rm=TRUE)

OldEU <- subset(dfw, MIGGRP=="OldEU")
a2 <- svymean(~SEX+AGE+EDU+SC10MMJ, OldEU, na.rm=TRUE)

A8 <- subset(dfw, MIGGRP=="A8")
a3 <- svymean(~SEX+AGE+EDU+SC10MMJ, A8, na.rm=TRUE)

NonEUNonOECD <- subset(dfw, MIGGRP=="NonEUNonOECD")
a4 <- svymean(~SEX+AGE+EDU+SC10MMJ, NonEUNonOECD, na.rm=TRUE)

NonEUOECD <- subset(dfw, MIGGRP=="NonEUOECD")
a5 <- svymean(~SEX+AGE+EDU+SC10MMJ, NonEUOECD, na.rm=TRUE)

a <- cbind(a1, a2, a3, a4, a5)
a

# Prec work descriptives
# ===
t1 <- svytable(~PWK+MIGGRP, dfw, Ntotal=33500000)
t1 <- as.data.frame.matrix(t1)
t1$order <- c(1, 3, 2)
t1 <- dplyr::arrange(t1, order)
t1 <- dplyr::select(t1, -order)
t1 <- dplyr::mutate(t1, UK_pct = UK/sum(UK)*100)
t1 <- dplyr::mutate(t1, OldEU_pct = OldEU/sum(OldEU)*100)
t1 <- dplyr::mutate(t1, A8_pct = A8/sum(A8)*100)
t1 <- dplyr::mutate(t1, NonEUNonOECD_pct = NonEUNonOECD/sum(NonEUNonOECD)*100)
t1 <- dplyr::mutate(t1, NonEUOECD_pct = NonEUOECD/sum(NonEUOECD)*100)
rownames(t1) <- c("Full Time Permanent", "Precarious", "Unemployed")
t1

t2 <- svytable(~multi+MIGGRP, dfw, Ntotal=33500000)
t2 <- as.data.frame.matrix(t2)
t2$order <- c(1, 3, 4, 5, 6, 7, 1)
t2 <- dplyr::arrange(t2, order)
t2 <- dplyr::select(t2, -order)
t2 <- dplyr::mutate(t2, UK_Prop = UK/sum(UK)*100)
t2 <- dplyr::mutate(t2, OldEU_Prop = OldEU/sum(OldEU)*100)
t2 <- dplyr::mutate(t2, A8_Prop = A8/sum(A8)*100)
t2 <- dplyr::mutate(t2, NonEUNonOECD_Prop = NonEUNonOECD/sum(NonEUNonOECD)*100)
t2 <- dplyr::mutate(t2, NonEUOECD_Prop = NonEUOECD/sum(NonEUOECD)*100)
rownames(t2) <- c("Full Time Permanent", "Full Time Temporary", "Overtime Permanent", "Overtime Temporary", "Part Time Permanent", "Part Time Temporary", "Unemployed")
t2