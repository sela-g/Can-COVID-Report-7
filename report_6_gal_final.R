##report_6 GAL final

#copied from work by Arianne Albert (arianne.albert@cw.bc.ca)
#written in
# R version 4.1.1 "Kick Things"
# Platform: x86_64-apple-darwin17.0 (64 bit)
# working in RStudio
### plot themes

setwd("/Users/mac/Desktop/RID/report_6")
library(Gmisc)
library(broman)
library(dplyr)
library(MASS)
library(effects)
library(emmeans)
library(DescTools)
library(tidyverse)
library(sjPlot)
library(here)
library(janitor)
library(zoo)
library(lubridate)
library(logbin)
library(lme4)
library(readxl)
library(ggeffects)
library(kableExtra)
library(logistf)
library(corrplot)
library(ghibli)
library(ggpubr)
library(fmsb)
library(epiR)
library(fmsb)
library(epitools)



## I think this is for the formatting of the plot later on so I will keep it
theme_set(theme_pubr)

theme_pubr2 <- theme_pubr() +
  theme(panel.grid.major = element_line(linetype = 2, color = "grey"),
        text = element_text(size = 14))

theme_set(theme_pubr2)




## read in BC data

bc <- read.csv("BC_CLEANED_2023-02-28.csv", header = TRUE)
bc$province <- "BC"
# bc$d_naso1_collect

## Read in MB, NB, PE, and Yukon Territory data

mb.nb.pe <- read.csv("MB_NB_PE_CLEANED_2023-02-27.csv")
mb.nb.pe <- mb.nb.pe %>% 
  mutate(province = case_when(
    redcap_data_access_group.x == "mb" ~ "MB",
    redcap_data_access_group.x == "nb" ~ "NB",
    redcap_data_access_group.x == "pe" ~ "PEI",
    redcap_data_access_group.x == "yt" ~ "YT"
  ))

mb.nb.pe$d_naso1_collect <- mb.nb.pe$d_naso1_collect.x

## read in quebec data
qc <- read.csv("QC_CLEANED_2023-02-27.csv")
qc$province <- "QC"

qc$n_covid_date1 <- qc$n_covid_date
qc$g_othersx_spec
qc$g_othersx_spec_2 <- NA

describeFactors(qc$e_tx_other_specify)

bc$e_ecmo___1
mb.nb.pe$e_ecmo___1
qc$e_ecmo___1 <- qc$e_ecmo
# 
qc$f_pres_cort_oth_s <- NA
# qc$e_tx_other_specify

## load in ontario and alberta data:
#
### ok ariannes code here is pretty damn wacky, so lets just look at report 6 and try and replicate it

## combine QC, BC, NB, MB, PEI, and YT line level data, the


length(colnames(qc))
length(colnames(mb.nb.pe))
length(colnames(bc))

all_cols <- intersect(colnames(qc),colnames(bc))

all_cols <- intersect(all_cols, colnames(mb.nb.pe))


all_combined <- rbind(qc[,which(colnames(qc) %in% all_cols)],bc[,which(colnames(bc) %in% all_cols)])

all_combined <- rbind(all_combined,mb.nb.pe[,which(colnames(mb.nb.pe) %in% all_cols)])


bcmbnbpe <- all_combined

describeFactors(bcmbnbpe$province)

bcmbnbpe %>% group_by(province) %>% summarise(n = n(), date_range = range(as.Date(e_diagnosis), na.rm = TRUE))


## date range
summary(as.Date(bcmbnbpe$e_diagnosis))


#### ontario and alberta aggregate data loaded:

table2_ontario <- read.csv("table2_on.csv",header = TRUE)
table2_alberta <- read.csv("table2_alberta.csv", header = TRUE)

## create table 2 for all data 



### MATERNAL AGE

bcmbnbpe <- bcmbnbpe %>% mutate(age_cat2 = case_when(
  age_cat == "25-29 years" ~ "<30 years",
  age_cat == "25-30 years" ~ "<30 years",
  age_cat == "30-34 years" ~ "30-35 years",
  age_cat == "30-35 years" ~ "30-35 years",
  age_cat == "35-39 years" ~ "≥36 years",
  age_cat == "36-39 years" ~ "≥36 years",
  age_cat == "≥40 years" ~ "≥36 years",
  age_cat == "<25 years" ~ "<30 years"
))

bcmbnbpe$age_cat2 <- factor(bcmbnbpe$age_cat2, levels = c("<30 years","30-35 years","≥36 years"))

bcmbnbpe %>% group_by(age_cat2) %>% summarise(n = n())


table2 <- bcmbnbpe %>% group_by(age_cat2) %>% summarise(n = n())

table2$n_alberta <- c(840,1187,532,2)
table2$n_ontario <- c(847,941,357,345)


table2$totals <- table2$n + table2$n_alberta + table2$n_ontario
sum(table2$totals[1:3])
sum(table2$totals)


### GESTATIONAL AGE AT DIAGNOSIS

bcmbnbpe$ga_at_diag <- as.numeric((280 - (as.Date(bcmbnbpe$i_deliverydate_est) - as.Date(bcmbnbpe$d_naso1_collect)))/7) # in weeks
describeMedian(bcmbnbpe$ga_at_diag, iqr = FALSE) # check the range and fix any that are not possible - or remove. <0 and >43 weeks
bcmbnbpe$ga_at_diag <- replace(bcmbnbpe$ga_at_diag, which(bcmbnbpe$ga_at_diag >43 | bcmbnbpe$ga_at_diag < 0), NA)

bcmbnbpe$ga_dx_cat <- factor(case_when(
  bcmbnbpe$ga_at_diag <= 14 ~ "<=14 weeks",
  bcmbnbpe$ga_at_diag < 27 ~ "14-27 weeks",
  bcmbnbpe$ga_at_diag <38 ~ "28-38 weeks",
  bcmbnbpe$ga_at_diag >=38 ~ "≥38 weeks"
), levels = c("<=14 weeks", "14-27 weeks", "28-38 weeks", "≥38 weeks"))

describeFactors(bcmbnbpe$ga_dx_cat)

table2_ga <- bcmbnbpe %>% group_by(ga_dx_cat) %>% summarise(n = n())
  


table2_ga$n_alberta <- c(438,936,950,234,3)
table2_ga$n_ontario <- c(310,700,597,251,632)

table2_ga$sums <- table2_ga$n + table2_ga$n_alberta + table2_ga$n_ontario
sum(table2_ga$sums[1:4])
# as.Date(bcmbnbpe$e_diagnosis) - as.Date(bcmbnbpe$)
unique(qc$ga_at_diag)


### COMORBIDITIES:
table2_cvs <- bcmbnbpe %>% group_by(cvs) %>% summarise(n = n())
table2_cvs$n_ontario <- c(2488,2,0)
table2_cvs$n_alberta <- c(2561-49,49,0)
table2_cvs$sums <- table2_cvs$n + table2_cvs$n_alberta + table2_cvs$n_ontario
sum(table2_cvs$sums[1:2])




table2_htn <- bcmbnbpe %>% group_by(htn) %>% summarise(n = n())
table2_htn$n_ontario <- c(2490-123,123,0)
table2_htn$n_alberta <- c(2461,100,0)
table2_htn$sums <- table2_htn$n + table2_htn$n_alberta + table2_htn$n_ontario
sum(table2_htn$sums[1:2])


table2_diabetes <- bcmbnbpe %>% group_by(diabetes) %>% summarise(n = n())
table2_diabetes$n_ontario <- c(2490 - 267, 267, 0)
table2_diabetes$n_alberta <- c(2561 - 75, 75, 0)
table2_diabetes$sums <- table2_diabetes$n + table2_diabetes$n_alberta + table2_diabetes$n_ontario
sum(table2_diabetes$sums[1:2])


table2_lung <- bcmbnbpe %>% group_by(lung) %>% summarise(n = n())
table2_lung$n_ontario <- c(2490,0,0)
table2_lung$n_alberta <- c(2561-85,85,0)
table2_lung$sums <- table2_lung$n + table2_lung$n_alberta + table2_lung$n_ontario
sum(table2_lung$sums[1:2])



table2_immun <- bcmbnbpe %>% group_by(autoimm) %>% summarise(n = n())
table2_immun$n_ontario <- c(2490,0,0)
table2_immun$n_alberta <- c(2561-51,51,0)
table2_immun$sums <- table2_immun$n + table2_immun$n_alberta + table2_immun$n_ontario
sum(table2_immun$sums[1:2])



bcmbnbpe$obese <- bcmbnbpe$BMI_cat == "≥30"
table2_obese <-  bcmbnbpe %>% group_by(obese) %>% summarise(n = n())
table2_obese$n_ontario <- c(2490 - 948, 948, 0)
table2_obese$n_alberta <- c(2561-108,108,0)
table2_obese$sums <- table2_obese$n + table2_obese$n_alberta + table2_obese$n_ontario
sum(table2_obese$sums[1:2])

  
  
  
table2_ontario 
table2_alberta 
#cardiovascular
describeFactors(bcmbnbpe$cvs)

#hypertension
describeFactors(bcmbnbpe$htn)
#6294 + 99

describeFactors(bcmbnbpe$diabetes)

#lung disease
describeFactors(bcmbnbpe$lung)



#immunosuppression
describeFactors(bcmbnbpe$autoimm)

#obesity
describeFactors(bcmbnbpe$obese)




### MODE OF COVID 19 ACQUISITION
table2_mode <- bcmbnbpe %>% group_by(contact) %>% summarise(n = n())
table2_mode$n_ontario <- c(2490-1182-157,1182,157)
table2_mode$n_alberta <- c(2561-1003,1003,0)
table2_mode$sums <- table2_mode$n + table2_mode$n_alberta + table2_mode$n_ontario
sum(table2_mode$sums[1:2])

table2_mode2 <- bcmbnbpe %>% group_by(hcw) %>% summarise(n = n())
table2_mode2$n_ontario <- c(2490-73-157,73,157)
table2_mode2$n_alberta <- c(2561-197,197,0)
table2_mode2$sums <- table2_mode2$n + table2_mode2$n_alberta + table2_mode2$n_ontario
sum(table2_mode2$sums[1:2])


table2_mode3 <- bcmbnbpe %>% group_by(other_aq) %>% summarise(n = n())
table2_mode3$n_ontario <- c(2490-300-157,300,157)
table2_mode3$n_alberta <- c(2561-930,930,0)
table2_mode3$sums <- table2_mode3$n + table2_mode3$n_alberta + table2_mode3$n_ontario
sum(table2_mode3$sums[1:2])

table2_mode4 <- bcmbnbpe %>% group_by(travel) %>% summarise(n = n())
table2_mode4$n_ontario <- c(2490-48-157,48,157)
table2_mode4$n_alberta <- c(2561-51,51,0)
table2_mode4$sums <- table2_mode4$n + table2_mode4$n_alberta + table2_mode4$n_ontario
sum(table2_mode4$sums[1:2])

table2_mode5 <- bcmbnbpe %>% group_by(unknown_aq) %>% summarise(n = n())
table2_mode5$n_ontario <- c(2490-48-157,48,157)
table2_mode5$n_alberta <- c(2561-51,51,0)
table2_mode5$sums <- table2_mode5$n + table2_mode5$n_alberta + table2_mode5$n_ontario
sum(table2_mode5$sums[1:2])

table2_ontario 
table2_alberta 

# Community - > contact
# Healthcare Worker - > hcw
# Other - > other_aq
# Travel - > travel
# Unknown - > unknown_aq





## COVID SYMPTOMS:

describeFactors(bcmbnbpe$cough)
describeFactors(bcmbnbpe$fever)
describeFactors(bcmbnbpe$head)
describeFactors(bcmbnbpe$runny)
describeFactors(bcmbnbpe$throat)
describeFactors(bcmbnbpe$muscle)
describeFactors(bcmbnbpe$breath)

describeFactors(bcmbnbpe$malaise)
describeFactors(bcmbnbpe$anosmia)
describeFactors(bcmbnbpe$vomit)
describeFactors(bcmbnbpe$asym)
describeFactors(bcmbnbpe$anorexia)

describeFactors(bcmbnbpe$diarrhea)
describeFactors(bcmbnbpe$e_othersx)
describeFactors(bcmbnbpe$chest_pain)


## hospitalizations
describeFactors(bcmbnbpe$e_hosp)

describeFactors(bcmbnbpe$abx_pne)
describeFactors(bcmbnbpe$e_coag)
describeFactors(bcmbnbpe$g_icu)
describeFactors(bcmbnbpe$e_oxygen___1)
describeFactors(bcmbnbpe$e_inv___1)
describeFactors(bcmbnbpe$e_sepsis)

# e_inv___1
# e_sepsis

describeFactors(bcmbnbpe$gravida)
describeFactors(bcmbnbpe$r_num)#rememer how to singleton/multiple ?
dim(bcmbnbpe)

## pregnancy outcomes
describeFactors(bcmbnbpe$o_spont)
describeFactors(bcmbnbpe$o_elective)
describeFactors(bcmbnbpe$o_sb)
describeFactors(bcmbnbpe$o_sb)

describeFactors(bcmbnbpe$p_outcome)

## mode of delivery
describeFactors(bcmbnbpe$mode_del)

describeFactors(bcmbnbpe$p_labour___1)
describeFactors(bcmbnbpe$p_labour___2)
describeFactors(bcmbnbpe$p_labour___3)
describeFactors(bcmbnbpe$p_labour___4)
describeFactors(bcmbnbpe$p_labour___999) # missing


## ga at delivery
bcmbnbpe$ga_at_del <- as.numeric((280 - (as.Date(bcmbnbpe$i_deliverydate_est) - as.Date(bcmbnbpe$r_dob)))/7)


bcmbnbpe$ga_del_cat <- case_when(
  bcmbnbpe$ga_at_del < 28 ~ "extremely preterm",
  bcmbnbpe$ga_at_del < 32 ~ "very preterm",
  bcmbnbpe$ga_at_del < 34 ~ "moderate preterm",
  bcmbnbpe$ga_at_del < 37 ~ "late preterm",
  bcmbnbpe$ga_at_del >= 37 ~ "term",
)


describeFactors(bcmbnbpe$ga_del_cat) # missings


## all pre term births

preterm_births <- bcmbnbpe %>% filter(ga_at_del < 37)

dim(preterm_births)
preterm_births$o_loss
describeFactors(preterm_births$o_sb)
preterm_births$l_bleed_etiology___1


## Table 8 

bcmbnbpe$apgar5


# 5 minute apgar
bcmbnbpe$apgar5_new <- factor(ifelse(bcmbnbpe$s_apgar_5 < 7, "<7", "≥7"))
describeFactors(bcmbnbpe$apgar5_new) # missings



# birth weight
bcmbnbpe$bw_cat <- case_when(
  bcmbnbpe$s_bw_gm < 2500 ~ "<2500",
  bcmbnbpe$s_bw_gm >= 2500 & bcmbnbpe$s_bw_gm <=4000 ~ "2500-4000",
  bcmbnbpe$s_bw_gm > 4000 ~ ">4000"
)

describeFactors(bcmbnbpe$bw_cat) # missings
# NICU admission
# assuming if we have birth weight then we should know if baby admitted to NICU or not
bcmbnbpe$NICU <- case_when(
  bcmbnbpe$t_nicu == 1 ~ "Yes",
  bcmbnbpe$t_nicu == 0 ~ "No",
  is.na(bcmbnbpe$s_bw_gm) == FALSE ~ "No")


describeFactors(bcmbnbpe$NICU) # missings

#### Small for gestational age #####
# make weight vectors
### From: A New and Improved Population-Based Canadian Reference for Birth Weight for Gestational Age, 
## Michael S. Kramer, Robert W. Platt, Shi Wu Wen, K.S. Joseph, Alexander Allen, Michal Abrahamowicz, 
## Béatrice Blondel, Gérard Bréart and for the Fetal/Infant Health Study Group of the Canadian Perinatal 
## Surveillance System Pediatrics 2001;108;e35 DOI: 10.1542/peds.108.2.e35

ga.F <- seq(22, 43)

weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above

weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above

# if unknown go with male to be conservative
SGA <- data.frame(ga.F, weight.F, weight.M)

# function to determine SGA
sga.fun <- function(dat){
  sga <- c()
  for(i in 1:dim(dat)[1]){
    
    if(is.na(dat$s_bw_gm[i])){
      sga <- c(sga, NA)
      next
    }
    
    if(is.na(dat$s_bw_gm[i]) & is.na(dat$s_pe_genitalia[i])){
      sga <- c(sga, NA)
      next
    }
    
    if(is.na(dat$ga_at_del[i])){
      sga <- c(sga, NA)
      next
    }
    
    if(dat$ga_at_del[i] < 22 | dat$ga_at_del[i] > 43){
      sga <- c(sga, NA)
      next
    }
    
    if(is.na(dat$s_pe_genitalia[i])){
      w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
      sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
      next
    }
    
    if(dat$s_pe_genitalia[i] == 2){
      w.thresh <- SGA$weight.F[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
      sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
      next
    }
    
    if(dat$s_pe_genitalia[i] == 1){
      w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
      sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
      next
    }
    
    if(is.na(sga[i])){
      sga <- c(sga, NA)
      next
    }
  }
  return(sga)
}

bcmbnbpe$SGA <- sga.fun(bcmbnbpe)


describeFactors(bcmbnbpe$SGA)

## percentage admitted to ICU of those hospitalized
140/473
sum(bcmbnbpe$e_hosp, na.rm = TRUE)
sum(bcmbnbpe$g_icu, na.rm = TRUE)


## covid test
bcmbnbpe$n_covid



### pre delta, delta to omicron, and post omicron columns
### hospitalizations / ICU, preterm births, still births, NICU admissions

## omicron start December 19 2021 
## Delta started April 4th 2021
## dates from https://nccid.ca/covid-19-variants/
bcmbnbpe <- bcmbnbpe %>% mutate(covid_period = case_when(
  e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
  e_diagnosis >= as.Date("2021-04-04") & e_diagnosis < as.Date("2021-12-19") ~ "Delta",
  e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
))

describeFactors(bcmbnbpe$o_sb)
bcmbnbpe$covid_period <- factor(bcmbnbpe$covid_period, levels = c("pre-Delta","Delta","Omicron"))

covid_period_table<- bcmbnbpe %>% group_by(covid_period) %>% summarise(n = n(),nicu = sum(NICU == "Yes", na.rm = TRUE),stillbirth = sum(o_sb == 1, na.rm = TRUE),hospitalizations = sum(e_hosp, na.rm = TRUE),icu_admissions = sum(g_icu, na.rm = TRUE))

covid_period_table2 <- bcmbnbpe %>% group_by(covid_period) %>% summarise(n = n(),term_births = sum(ga_del_cat == "term", na.rm = TRUE), late_preterm = sum(ga_del_cat == "late preterm", na.rm = TRUE),moderate_preterm = sum(ga_del_cat == "moderate preterm", na.rm = TRUE),very_preterm = sum(ga_del_cat == "very preterm", na.rm = TRUE))

write.csv(covid_period_table,"covid_period_table.csv")
write.csv(covid_period_table2,"covid_period_table2.csv")

# "extremely preterm",
# bcmbnbpe$ga_at_del < 32 ~ "very preterm",
# bcmbnbpe$ga_at_del < 34 ~ "moderate preterm",
# bcmbnbpe$ga_at_del < 37 ~ "late preterm",
# bcmbnbpe$ga_at_del >= 37 ~ "term",
# ga_del_cat
