#### --------------------
#### PRELIMINARIES
#### --------------------

# import statnet and tidyverse
library(foreign)
library(tidyverse)
library(statnet)
library(EpiModel)
library(networkD3)

# sets the current working directory
setwd("/Users/vaniawang/Google Drive/grad school/PhD UCSB/projects/msm_seattle/raw_data/Data/")

# importing the .csv file and converting it to a tibble
msmdata <- as.tibble(read.dta('msm_data.dta'))

#### --------------------
#### LOGIT PROB FUNCTION
#### --------------------

## calculates probability from logit coefficients
prob.fun <- function(coef) {
    prob <- exp(coef)/(1+exp(coef))
    return(prob)
}

## bins degrees given a list of degrees
count.deg <- function(deg.lst) {
    for(i in deg.lst) {
        d[i] <- d[i] + 1
    }
    return(d)
}

## bootstrap function that takes a degree vector and returns
## the number of edges in network given n.prime
boot <- function(deg.vec, n, n.prime) {
    ## bootstrapping from the observed degree distribution
    boot <- matrix(sample(deg.vec, n, replace = TRUE), 1000, n)

    ## a matrix of degree distributions for 1000 interations of sampling
    meandeg <- apply(boot, 1, mean)

    ## calculating the edge mean
    edge.mean <- round(mean(meandeg) * n.prime / 2)

    return(edge.mean)
}

read.filename <- function() { 
  name <- readline(prompt="Enter a filename: ")
  return(as.character(name))
}

## plots d3 plots given a ERGM simulation
d3.plot <- function(sim) {
    ## some preliminaries
    v.lab <- get.vertex.attribute(sim, 'sex')
    source.pre <- as.edgelist(sim)[,1]-1
    target.pre <- as.edgelist(sim)[,2]-1
    mod.vlst <- unique(sort(c(source.pre, target.pre)))

    source <- plyr::ldply(source.pre,
                          function (x) which(mod.vlst == x)) - 1
    target <- plyr::ldply(target.pre,
                          function (x) which(mod.vlst == x)) - 1

    ## create the link data frame
    mod.links <- data.frame(source, target)

    ## create the nodes data frame
    group <- as.numeric(v.lab[mod.vlst+1] + 1)
    name <- as.character(mod.vlst)
    size <- as.numeric(rep(1, length(names)))
    mod.nodes <- data.frame(name, group, size)

    mod.netviz <- forceNetwork(Links = mod.links,
                               Nodes = mod.nodes,
                               NodeID = "name",
                               Group = "group",
                               opacity = 1,
                               zoom = TRUE)

    ## saves the nodes and links lists
    linksname <- paste(read.filename(), '.rds', sep = '')
    nodesname <- paste(read.filename(), '.rds', sep = '')
    saveRDS(mod.links, file = linksname)
    saveRDS(mod.nodes, file = nodesname)
    return(mod.netviz)
}

#### --------------------
#### DATA CLEANING
#### --------------------

##' removes the date_today, agecheck, and age_incorrect variable
msmdata <- select(msmdata, -date_today)

##' makes new social score variables that assigns numerical values to
##' different combos of responses to questions ss1 to ss12
##' @ss.score The composite sum of the social support questionnaire
##' @ss.sp The composite sum from the ss questionnaire regarding
##'        'special friends'
##' @ss.fren The composite sum from the ss questionnaire regarding
##'          friends
##' @ss.fam The composite sum from the ss questionnaire regarding
##'         family
##' @ss.sp.bin Binary form of ss.sp (ss.sp/4 > 4 is assigned 1)
##' @ss.fam.bin Binary form of ss.fam (ss.fam/4 > 4 is assigned 1)
##' @ss.fren.bin Banary form of ss.fren (ss.fren/4 > 4 is assigned 1)
msmdata <- mutate_at(msmdata, vars(ss1:ss12),
                     function(x) recode(x,
                                        'Very Strongly Disagree' = 1,
                                        'Strongly Disagree' = 2,
                                        'Mildly Disagree' = 3,
                                        'Neutral' = 4,
                                        'Mildly Agree' = 5,
                                        'Strongly Agree' = 6,
                                        'Very Strongly Agree' = 7,
                                        'Don\'t Know' = 0,
                                        'Refuse to Answer' = 0,
                                        .default = 0)) %>%
    mutate(ss.score = rowSums(select(., ss1:ss12))) %>%
    mutate(ss.sp = rowSums(select(., ss1, ss2, ss5, ss10))) %>%
    mutate(ss.fren = rowSums(select(., ss6, ss7, ss9, ss12))) %>%
    mutate(ss.fam = rowSums(select(., ss3, ss4, ss8, ss11))) %>%
    mutate(ss.sp.bin = ifelse(ss.sp/4 > 4, 1, 0)) %>%
    mutate(ss.fam.bin = ifelse(ss.fam/4 > 4, 1, 0)) %>%
    mutate(ss.fren.bin = ifelse(ss.fren/4 > 4, 1, 0))

##' cleans up the binary 'chosen' variable
msmdata <- mutate_at(msmdata, vars(chosen),
                     function(x) recode(x,
                                        'Yes' = 1,
                                        'No' = 0,
                                        'Dont know' = 0,
                                        'Refuse to answer' = 0,
                                        .default = 0))

##' cleans up the 'comeout' variable
msmdata <- mutate_at(msmdata, vars(comeout),
                     function(x) recode(x,
                                        'Yes' = 1,
                                        'No' = 0,
                                        'Dont know' = 0,
                                        'Refuse to answer' = 0,
                                        .default = 0))

#' cleans up the 'comeout_fam' variable
msmdata <-
    msmdata %>%
    mutate_at(vars(comeout_fam),
              function(x) recode(x,
                                 'Yes' = 1,
                                 'No' = 0,
                                 'Dont know' = 0,
                                 'Refuse to answer' = 0,
                                 .default = 0)) %>%
    mutate_at(vars(comeout_fam),
              function(x) ifelse(is.na(x), 0, 1))

#' cleans up the 'sex_woman' variable
msmdata <- mutate_at(msmdata, vars(sex_woman),
                     function(x) recode(x,
                                        'Yes' = 1,
                                        'No' = 0,
                                        'Dont know' = 1,
                                        'Refuse to answer' = 1,
                                        .default = 0))

#' cleans up the 'hivstatus' variable
msmdata.hiv <- mutate_at(msmdata, vars(hivstatus),
                     function(x) recode(x,
                                        'HIV-positive' = 1,
                                        'HIV-negative' = 0,
                                        'Dont know' = 0,
                                        'Refuse to answer' = 1,
                                        .default = 0))

#' makes a new variable sex12_women.bin
msmdata.sex12_bin <-
    mutate(msmdata.hiv,
           sex12_women.bin = ifelse(is.na(sex12_women), NA,
                                    ifelse(sex12_women > 0, 1, 0)))

msmdata <- msmdata.sex12_bin

#### --------------------
#### DESCRIPTIVE STATISTICS
#### --------------------

##' Some numbers
##' ------------

## the number of men who are HIV positive and have had sex with women
## within the past twelve months
sum(msmdata$hivstatus == 1 & msmdata$sex12_women > 0, na.rm=TRUE)

## the number of men who have had sex with women in the last twelve
## months and who have had unprotected anal sex
sum(msmdata$sex3_uai > 0 & msmdata$sex12_women > 0, na.rm=TRUE)

## the number of men who have had sex with women in the last twelve
## months and who have not had unprotected anal sex
sum(msmdata$sex3_uai == 0 & msmdata$sex12_women > 0, na.rm=TRUE)

## the number of men who have not had sex with women in the last twelve
## months and who have had unprotected anal sex
sum(msmdata$sex3_uai > 0 & msmdata$sex12_women == 0, na.rm=TRUE)

## the number of men who have not had sex with women in the last twelve
## months and who have not had unprotected anal sex
sum(msmdata$sex3_uai == 0 & msmdata$sex12_women == 0, na.rm=TRUE)

## the number of men who have had sex with women in the last twelve
## months, who have had unprotected anal sex, who are HIV pos
sum(msmdata$sex3_uai > 0 & msmdata$sex12_women > 0 & msmdata$hivstatus == 1,
    na.rm=TRUE)

##' Plots
##' -----

## plot (hist) of when respondent told family about their sexual
## orientation
age.tell.fam_hist <-
    ggplot(data=msmdata) +
    geom_bar(aes(x=age_tell_fam), na.rm = TRUE) +
    labs(x = 'Age',
         y = 'Number of respondents',
         title = 'Age when respondent told family about their sexual orientation')

##' Linear regression models
##' ------------------------

## outcome: lifetime_women, predictors: the ss scores
summary(lm(lifetime_women ~ ss.score, data = msmdata))
summary(lm(lifetime_women ~ ss.fam, data = msmdata))
summary(lm(lifetime_women ~ ss.sp, data = msmdata))
summary(lm(lifetime_women ~ ss.fren, data = msmdata))
summary(
    lm(lifetime_women ~ ss.fam + ss.fren + ss.sp,
       data = msmdata))

## **SIG** outcome: sex12_women, predictors: the ss scores
summary(lm(sex12_women ~ ss.score, data = msmdata))
summary(lm(sex12_women ~ ss.fam, data = msmdata))
summary(lm(sex12_women ~ ss.sp, data = msmdata))
summary(lm(sex12_women ~ ss.fren, data = msmdata))
summary(
    lm(sex12_women ~ ss.fam + ss.fren + ss.sp,
       data = msmdata))

## **SIG** outcome: sex12_women, predictors: comeout
summary(lm(sex12_women ~ comeout, data = msmdata))

## outcome: sex12_women, predictors: chosen
summary(lm(lifetime_women ~ chosen, data = msmdata))
summary(lm(sex12_women ~ chosen, data = msmdata))

## outcome: sex3_uai, predictor: hivstatus
summary(lm(sex3_uai ~ hivstatus, data = msmdata))

## outcome: sex12_women, predictor: hivstatus, sex3_uai
summary(lm(sex12_women ~ sex3_uai + hivstatus + ss.score, data = msmdata))

##' Logistic regression models
##' --------------------------

## outcome: sex_woman, predictor: chosen
summary(glm(sex_woman ~ chosen, family = 'binomial', data = msmdata))

## **SIG** outcome: chosen, predictor: sex_woman
summary(glm(chosen ~ sex_woman, family = 'binomial', data = msmdata))

## outcome: sex_woman, predictor: social support vars
summary(glm(sex_woman ~ ss.score, family = 'binomial', data = msmdata))
summary(glm(sex_woman ~ ss.fren, family = 'binomial', data = msmdata))
summary(glm(sex_woman ~ ss.sp, family = 'binomial', data = msmdata))
summary(glm(sex_woman ~ ss.fam, family = 'binomial', data = msmdata))
summary(
    glm(sex_woman ~ + ss.fren + ss.sp + ss.fam,
        family = 'binomial',
        data = msmdata))

## **SIG** outcome: chosen, predictor: social support vars
summary(glm(chosen ~ ss.score, family = 'binomial', data = msmdata))
summary(glm(chosen ~ ss.fren, family = 'binomial', data = msmdata))
summary(glm(chosen ~ ss.sp, family = 'binomial', data = msmdata))
summary(glm(chosen ~ ss.fam, family = 'binomial', data = msmdata))
summary(
    glm(chosen ~ + ss.fren + ss.fam + ss.sp,
        family = 'binomial',
        data = msmdata))

## **SIG** outcome: comeout, predictor: social support vars
summary(glm(comeout ~ ss.score, family = 'binomial', data = msmdata))
summary(glm(comeout ~ ss.fren, family = 'binomial', data = msmdata))
summary(glm(comeout ~ ss.sp, family = 'binomial', data = msmdata))
summary(glm(comeout ~ ss.fam, family = 'binomial', data = msmdata))
summary(
    glm(comeout ~ + ss.fren + ss.fam + ss.sp,
        family = 'binomial',
        data = msmdata))

## **SIG** outcome: comeout, predictor: sex12_woman
summary(glm(comeout ~ sex12_women, family = 'binomial', data = msmdata))

## **SIG** outcome: comeout_fam, predictor: sex12_woman
summary(glm(comeout_fam ~ sex12_women, family = 'binomial', data = msmdata))

## outcome: hivstatus, predictor: sex12_woman
summary(glm(hivstatus ~ sex12_women, family = 'binomial', data = msmdata))

## **SIG** outcome: hivstatus, predictor: sex_woman
summary(glm(hivstatus ~ sex_woman, family = 'binomial', data = msmdata))

## **SIG** outcome: hivstatus, predictor: sex3_uai
summary(glm(hivstatus ~ sex3_uai, family = 'binomial', data = msmdata))

## **SIG** outcome: hivstatus, predictor: sex12_woman.bin
summary(glm(hivstatus ~ sex12_women.bin,
            family = 'binomial',
            data = msmdata))

## **SIG** outcome: sex12_woman.bin, predictor: sex3_uai
summary(glm(sex12_women.bin ~ sex3_uai,
            family = 'binomial',
            data = msmdata))

## **SIG** outcome: sex12_woman.bin, predictor: hivstatus
summary(glm(sex12_women.bin ~ hivstatus,
            family = 'binomial',
            data = msmdata))

#### --------------------
#### BASIC NETWORK STATS
#### --------------------

##' @m.12ms degree vector of past 12 month partnerships with men
m.12ms <- msmdata$sex12_men[!is.na(msmdata$sex12_men)]

##' @f.12ms degree vector of past 12 month partnerships with women
f.12ms <- msmdata$sex12_women[!is.na(msmdata$sex12_women)]

##' @uai.12ms degree vector of past 12 month UAI occurences
uai.12ms <- msmdata$sex12_uai[!is.na(msmdata$sex12_uai)]

##' @n the number of total nodes in simulated population
n <- 1000

##' @n.m the number of male nodes
##' According to the Jeremy Grey et al 2016 and the American community
##' survey between 2009 and 2013, the number of MSM in King County is
##' 61752. According to the 2010 census, the number of adult women in
##' King County is 767536
n.m <- round((61752 / (767536 + 61752)) * n)

##' @n.f the number of female nodes
##' According to 2010 census, there were 767536 adult females in King
##' County
n.f <- n - n.m

##' Initializing the base network
##' -----------------------------

## f_rel.msm_num is the number of men who have ever had sex with women
## in their lifetime
mf.net <-  network.initialize(n, directed = FALSE)
mf.net %v% 'sex' <- c(rep(0, n.m), rep(1, n.f))

##' Boostrapping the MALE and FEMALE partner degree distribution
##' ------------------------------------------------------------

##' @m.edge the number of male <--> male edges
m.edges <- boot(m.12ms, n, n.m)

##' @f.edge the number of male <--> female edges
##'         need to multiply by two because no female respondents
##'         in survey
f.edges <- boot(f.12ms, n, n.m) * 2

##' @mf.edges Total number of edges in network
mf.edges <- m.edges + f.edges

##' HIV positive nodal attribute
##' ----------------------------

##' @p.hiv the proportion of men who are HIV positive
hivpos <- msmdata$hivstatus[!is.na(msmdata$hivstatus)]
n.resp <- nrow(msmdata)
p.hiv <-  sum(hivpos == 1)/n.resp

n.hivpos <- round(n.m * p.hiv)
n.hivneg <- n.m - n.hivpos

##' Assigning the hiv pos attribute to the nodes.
##' Note that here, women are all HIV-negative.
mf.net %v% 'hivpos' <- c(rep(0, n.hivneg), rep(1, n.hivpos), rep(0, n.f))

##' The number of sexual relationships among HIV positive men
##' ---------------------------------------------------------
m.hiv.edges <- round(p.hiv * mf.edges)

##' Boostrapping risky sex (sex12_uai) behavior for MSM
##' ---------------------------------------------------
uai.edges <- boot(uai.12ms, n, n.m)

#### --------------------
#### MODELS
#### --------------------

##' MODEL 1: 
##' --------
##' DESCRIPTION: The base model, with edges parameter only
mod1.target.stats <- mf.edges

mod1 <- ergm(
    mf.net ~ edges,
    target.stats = mod1.target.stats)

sum1 <- summary(mod1)

sim1 <- simulate(mod1)

##' MODEL 2: 
##' --------
##' DESCRIPTION: (1) 'nodemix' term to mod1, where number of edges
##'              are specified for each 'sex' level. These edges are the
##'              number of sexual ties per sex combination
mod2.target.stats <-
    c(mf.edges, f.edges, 0) 

mod2 <- ergm(
    mf.net ~ edges + nodemix('sex', base = 1),
    target.stats = mod2.target.stats)

sum2 <- summary(mod2)

sim2 <- simulate(mod2)

##' MODEL 3:
##' --------
##' DESCRIPTION: (1) 'nodemix' , where number of edges
##'              are specified for each 'sex' level. This represents
##'              number of sexual ties per sex combination.
##'              (2) nodefactor(hivpos) attribute
mod3.target.stats <-
    c(mf.edges, f.edges, 0, m.hiv.edges) 

mod3 <- ergm(
    mf.net ~ edges +
        nodemix('sex', base = 1) +
        nodefactor('hivpos', base = 1),
    target.stats = mod3.target.stats)

sum3 <- summary(mod3)

sim3 <- simulate(mod3)

#### --------------------
#### D3 VISUALIZATION
#### --------------------

##' MODEL 1: 
##' --------
d3.plot(sim1)

##' MODEL 2: 
##' --------
d3.plot(sim2)

##' MODEL 3: 
##' --------
d3.plot(sim3)
