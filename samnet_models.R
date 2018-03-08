#### --------------------
#### PRELIMINARIES
#### --------------------

# import the statnet library
library(foreign)
library(tidyverse)
library(statnet)
require(foreign)

# sets the current working directory
setwd("/Users/vaniawang/Google Drive/grad school/PhD UCSB/projects/msm_seattle/raw data/")

# importing the .csv file and converting it to a tibble
msmdata <- as.tibble(read.dta('msm_data.dta'))

#### --------------------
#### DATA CLEANING
#### --------------------

# makes a new race variable that combines race___1 to 99, and then
# removes all race variables except "race" from the dataset
## msmdata <-
##     mutate(msmdata, race = case_when(
##                         msmdata$race___1 == 'Checked' ~ 1,
##                         msmdata$race___2 == 'Checked' ~ 2,
##                         msmdata$race___3 == 'Checked' ~ 3,
##                         msmdata$race___4 == 'Checked' ~ 4,
##                         msmdata$race___5 == 'Checked' ~ 5,
##                         msmdata$race___6 == 'Checked' ~ 6,
##                         msmdata$race___88 == 'Checked' ~ 88,
##                         msmdata$race___99 == 'Checked' ~ 99,
##                         TRUE ~ 0)) %>% select(-matches('race___'))

# removes the date_today and agecheck variable
msmdata <- select(msmdata, -date_today)

# removes the agecheck variable
msmdata <- select(msmdata, -agecheck)

# removes the age_incorrect variable
msmdata <- select(msmdata, -age_incorrect)

# making subsets of data
## msmdata_het <- msmdata %>%
##     select(-matches(race___))

#### --------------------
#### Descriptive Statistics
#### --------------------

# vector degree list of lifetime sexual partnerships with women,
f_rel.life <- msmdata$lifetime_women[!is.na(msmdata$lifetime_women)]

# number of msm who have ever had sex with women
f_rel.msm_num <- sum(!is.na(msmdata$lifetime_women))

# number of women msm have ever had sex with, assuming no duplicates
f_rel.f_num

#### --------------------
#### MODELS
#### --------------------

## -----
## Model 1: ERGM model for LIFETIME sexual relationships with women
## -----

# 146 is the number of men who have ever had sex with women in their
# lifetime
het.net <-  network.initialize(272, directed = FALSE)
het.net %v% 'sex' <- c(rep(0, 146), rep(1, 126))
het.deg <- msmdata$lifetime_women[!is.na(msmdata$lifetime_women)]
het.mixmat <- c(0, 275)/2
het.edges <- sum(het.mixmat)
het.rel <- het.mixmat[1]
het.target.stats <- c(het.edges, het.rel)
het.fit <- ergm(
    het.net ~ edges + nodematch('sex'),
    target.stats = het.target.stats)

summary <- (het.fit)
het.sim1 <- simulate(het.fit)

## -----
## MODEL 2: ERGM model for LIFETIME sexual relationships with men
## -----


