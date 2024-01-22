############ ALL REPORT 6 ##########
rm(list = ls())

getwd()

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


######## LOAD IN AND MERGE ALL QC DATA ###########
qcdem <- read.csv("QCdata/COVIPREGQV4-EXPORT1DEMOAB_DATA_2024-01-16_2009.csv", header = TRUE, na.strings = c("", NA, 999))
qc1 <- read.csv("QCdata/COVIPREGQV4-EXPORT2aSARSCOV2CDE_DATA_2024-01-16_2010.csv", header = TRUE, na.strings = c("", NA, 999))
qc2 <- read.csv("QCdata/COVIPREGQV4-EXPORT2bSARSCOV2F_DATA_2024-01-16_2012.csv", header = TRUE, na.strings = c("", NA, 999))
qc3 <- read.csv("QCdata/COVIPREGQV4-EXPORT2c1SARSCOV2G_DATA_2024-01-16_2014.csv", header = TRUE, na.strings = c("", NA, 999))
qc4 <- read.csv("QCdata/COVIPREGQV4-EXPORT2c2SARSCOV2G_DATA_2024-01-16_2015.csv", header = TRUE, na.strings = c("", NA, 999))
qc_ant1 <- read.csv("QCdata/COVIPREGQV4-EXPORT3aANTEHToL_DATA_2024-01-16_2015.csv", header = TRUE, na.strings = c("", NA, 999))
qc_ant2 <- read.csv("QCdata/COVIPREGQV4-EXPORT3bANTEM_DATA_2024-01-16_2015.csv", header = TRUE, na.strings = c("", NA, 999))
qc_ant3 <- read.csv("QCdata/COVIPREGQV4-EXPORT3cANTENO_DATA_2024-01-16_2016.csv", header = TRUE, na.strings = c("", NA, 999))
qc_int <- read.csv("QCdata/COVIPREGQV4-EXPORT4INTRAPToU_DATA_2024-01-16_2019.csv", header = TRUE, na.strings = c("", NA, 999))
qc_pos <- read.csv("QCdata/COVIPREGQV4-EXPORT5POSTVToY_DATA_2024-01-16_2020.csv", header = TRUE, na.strings = c("", NA, 999))
qc_oth <- read.csv("QCdata/COVIPREGQV4-EXPORT6OTHERZToAB_DATA_2024-01-16_2021.csv", header = TRUE, na.strings = c("", NA, 999))

dim(qc1)
dim(qc2)
dim(qc3)
dim(qc4)


## all record ids in qc2 are in qc1 as well so left join with qc1 on left
sum(!(qc2$a_record_id %in% qc1$a_record_id))
sum(!(qc3$a_record_id %in% qc1$a_record_id))
sum(!(qc4$a_record_id %in% qc1$a_record_id))


sum(!(qc_ant2$a_record_id %in% qc_ant1$a_record_id))
sum(!(qc_ant3$a_record_id %in% qc_ant1$a_record_id))


qc_cov <- left_join(qc1,qc2,by = "a_record_id")
qc_cov <- left_join(qc_cov,qc3,by = "a_record_id")
qc_cov <- left_join(qc_cov,qc4,by = "a_record_id")

qc_ant <- left_join(qc_ant1,qc_ant2,by = "a_record_id")
qc_ant <- left_join(qc_ant,qc_ant3,by = "a_record_id")

# clean out any empty columns
cp.cov <- remove_empty(dat = qc_cov, quiet = TRUE)
cp.dem <- remove_empty(dat = qcdem, quiet = TRUE)
cp.ant <- remove_empty(dat = qc_ant, quiet = TRUE)
cp.int <- remove_empty(dat = qc_int, quiet = TRUE)
cp.pos <- remove_empty(dat = qc_pos, quiet = TRUE)
cp.oth <- remove_empty(dat = qc_oth, quiet = TRUE)

########## CLEAN QUEBEC DATA #############
##  first check for stillbirths


cc <- cp.ant$a_record_id[which(cp.ant$o_sb == 1 | cp.ant$o_spont == 1)] # remove 186-24
cc <- cc[-2]
cp.int$r_dob[which(cp.int$a_record_id %in% cc)]
cc2 <- cp.int$a_record_id[which(cp.int$a_record_id %in% cc)]
cp.int$r_dob[which(cp.int$a_record_id %in% cc)] <- cp.ant$o_date[which(cp.ant$a_record_id %in% cc2)]

cp.ant$a_record_id[which(cp.ant$o_sb == 1)]

cp.cov$d_naso1_collect[which(is.na(cp.cov$d_naso1_collect))] <- cp.cov$e_diagnosis[which(is.na(cp.cov$d_naso1_collect))]


cp.dem <- cp.dem %>% 
  rowwise() %>% 
  mutate(eth = case_when(
    all(c(b_ethnicity___1, b_ethnicity___2, b_ethnicity___3, b_ethnicity___4, b_ethnicity___5, b_ethnicity___6, b_ethnicity___7, b_ethnicity___8, b_ethnicity___998, b_ethnicity___999) == 0) ~ "Missing",
    b_ethnicity___999 == 1 ~ "Unknown",
    b_ethnicity___998 == 1 ~ "Other",
    b_ethnicity___1 == 1 ~ "White",
    b_ethnicity___2 == 1 ~ "African/Carribean/Black",
    b_ethnicity___3 == 1 ~ "Hispanic/Latino",
    b_ethnicity___4 == 1 ~ "East Asian",
    b_ethnicity___5 == 1 ~ "South Asian",
    b_ethnicity___6 == 1 ~ "South East Asian",
    b_ethnicity___7 == 1 ~ "Middle East",
    b_ethnicity___8 == 1 ~ "Indigenous"
  ))

describeFactors(cp.dem$eth)
cp.dem$eth <- factor(cp.dem$eth)
# change the missing and unknown to NA
levels(cp.dem$eth)[which(levels(cp.dem$eth) == "Missing" | levels(cp.dem$eth) == "Unknown")] <- NA

# reduce the number of categories
cp.dem$eth_cat <- cp.dem$eth
levels(cp.dem$eth_cat)[which(levels(cp.dem$eth) == "Hispanic/Latino" | levels(cp.dem$eth) == "Middle East" | levels(cp.dem$eth) == "Other")] <- "Other"
cp.dem$eth_cat <- relevel(cp.dem$eth_cat, ref = "White")

## AGE
summary(cp.dem$b_age) # check that Missing are coded as NA and not as 999 or something like that. Should be NA
cp.dem$b_age <- ifelse(cp.dem$b_age == 999, NA, cp.dem$b_age)

## Employment
describeFactors(cp.dem$b_employment)
cp.dem$employ <- case_when(
  cp.dem$b_employment == 1 ~ "Employed",
  cp.dem$b_employment == 2 ~ "Unemployed"
)
describeFactors(cp.dem$employ)


# number of years of school - missing coded as 999 or 888 - recode as NA
summary(cp.dem$b_school)
cp.dem$education <- replace(cp.dem$b_school, which(cp.dem$b_school == 999 | cp.dem$b_school == 888), NA)
summary(cp.dem$education)
# nearly all missing - likely won't use this variable


summary(cp.cov$e_onset)

### testing date - more complete
summary(cp.cov$e_diagnosis) 
summary(cp.cov$d_naso1_collect) # check that these dates make sense. Fix or exclude ones that are errors

### calculate gestational age at testing
# EDD is only available on the cp.ant data set, therefore, some of the people tested will be missing GA at testing/diagnosis
cp.ant$i_deliverydate_est # EDD
cp.ant$i_deliverydate_est <- as.Date(as.character(cp.ant$i_deliverydate_est), format = "%Y-%m-%d")

# fix
# cp.ant$i_deliverydate_est[which(cp.ant$a_record_id == "CCP-01-600")] <- as.Date("2021-05-09")

cp.cov$d_naso1_collect
cp.ant <- merge(cp.ant, cp.cov[, c("a_record_id", "d_naso1_collect")], by = "a_record_id")

cp.ant$d_naso1_collect <- as.Date(cp.ant$d_naso1_collect)
# replace 9999-09-09 with NA
cp.ant$i_deliverydate_est <- replace(cp.ant$i_deliverydate_est, which(cp.ant$i_deliverydate_est == "9999-09-09"), NA)

cp.ant$ga_at_diag <- as.numeric((280 - (cp.ant$i_deliverydate_est - cp.ant$d_naso1_collect))/7) # in weeks
describeMedian(cp.ant$ga_at_diag, iqr = FALSE) # check the range and fix any that are not possible - or remove. <0 and >43 weeks
cp.ant$ga_at_diag <- replace(cp.ant$ga_at_diag, which(cp.ant$ga_at_diag >43 | cp.ant$ga_at_diag < 0), NA)


## what is their GA now? If still preg?
# replace Today with whatever date we decide to use as the final cutoff
cp.ant$ga.now <- as.numeric((280 - (cp.ant$i_deliverydate_est - Today()))/7)
# remove the ones that have delivered or had losses
# cp.int$a_record_id # intrapartum data
# cp.ant$ga.now <- replace(cp.ant$ga.now, which(cp.ant$a_record_id %in% cp.int$a_record_id | cp.ant$o_loss == 1), NA) # have they delivered (in the intrapartum data set) or did they have a loss?
# 
# describeMedian(cp.ant$ga.now, iqr = FALSE) # check the range and fix any that are not possible  - or remove. <0 and >43 weeks

# split maternal age and GA at diagnosis into categories
cp.dem$age_cat <- factor(case_when(
  cp.dem$b_age <25 ~ "<25 years",
  cp.dem$b_age <30 ~ "25-30 years",
  cp.dem$b_age <35 ~ "30-34 years",
  cp.dem$b_age <40 ~ "35-39 years",
  cp.dem$b_age >=40 ~ "≥40 years"
),
levels = c("<25 years", "25-30 years", "30-34 years", "35-39 years", "≥40 years"))

describeFactors(cp.dem$age_cat)

# cp.ant$ga_at_diag <- as.numeric((280 - (cp.ant$i_deliverydate_est - cp.ant$d_naso1_collect))/7) # in weeks
# describeMedian(cp.ant$ga_at_diag, iqr = FALSE) # check the range and fix any that are not possible - or remove. <0 and >43 weeks
# cp.ant$ga_at_diag <- replace(cp.ant$ga_at_diag, which(cp.ant$ga_at_diag >43 | cp.ant$ga_at_diag < 0), NA)

cp.ant$ga_dx_cat <- factor(case_when(
  cp.ant$ga_at_diag <= 14 ~ "<=14 weeks",
  cp.ant$ga_at_diag < 27 ~ "14-27 weeks",
  cp.ant$ga_at_diag <38 ~ "28-38 weeks",
  cp.ant$ga_at_diag >=38 ~ "≥38 weeks"
), levels = c("<=14 weeks", "14-27 weeks", "28-38 weeks", "≥38 weeks"))

describeFactors(cp.ant$ga_dx_cat)

## maternal BMI
cp.ant$i_weight <- replace(cp.ant$i_weight, which(cp.ant$i_weight == 999 | cp.ant$i_weight == 666), NA)
cp.ant$i_height <- replace(cp.ant$i_height, which(cp.ant$i_height == 999 | cp.ant$i_height == 666), NA)
cp.ant$i_height <- replace(cp.ant$i_height, which(cp.ant$i_height == 1676.00 ), 167)
cp.ant$i_height[which(cp.ant$i_height < 100)] <- cp.ant$i_height[which(cp.ant$i_height < 100)]*100

cp.ant$BMI <- cp.ant$i_weight/(cp.ant$i_height/100)^2
describeMedian(cp.ant$BMI, iqr = FALSE) # check for out of range values - and remove or correct

# BMI >= 30 variable
cp.ant$BMI_cat <- factor(case_when(
  cp.ant$BMI < 18.5 ~ "<18.5",
  cp.ant$BMI < 25 ~ "18.5-25",
  cp.ant$BMI < 30 ~ "25-30",
  cp.ant$BMI >= 30 ~ "≥30"
))
describeFactors(cp.ant$BMI_cat)
cp.ant$BMI_cat <- factor(cp.ant$BMI_cat, levels = c("<18.5", "18.5-25", "25-30", "≥30"))


### acquisition
cp.cov$c_travel
cp.cov$c_contact
cp.cov$c_hcw
cp.cov$c_other
cp.cov$c_unknown

cp.cov <- cp.cov %>% 
  rowwise() %>% 
  mutate(travel = case_when(
    all(c(c_travel, c_contact, c_hcw, c_other) == 0) ~ "No entry",
    c_travel == 1 ~ "Yes",
    c_travel == 0 ~ "No"
  ),
  contact = case_when(
    all(c(c_travel, c_contact, c_hcw, c_other) == 0) ~ "No entry",
    c_contact == 1 ~ "Yes",
    c_contact == 0 ~ "No"
  ),
  hcw = case_when(
    all(c(c_travel, c_contact, c_hcw, c_other) == 0) ~ "No entry",
    c_hcw == 1 ~ "Yes",
    c_hcw == 0 ~ "No"
  ),
  other_aq = case_when(
    all(c(c_travel, c_contact, c_hcw, c_other) == 0) ~ "No entry",
    c_other == 1 ~ "Yes",
    c_other == 0 ~ "No"
  ),
  # unknown_aq = case_when(
  #   all(c(c_travel, c_contact, c_hcw, c_other) == 0) ~ "No entry",
  #   c_unknown == 1 ~ "Yes",
  #   c_unknown == 0 ~ "No")
  )

describeFactors(cp.cov$travel)
describeFactors(cp.cov$contact)
describeFactors(cp.cov$hcw)
describeFactors(cp.cov$other_aq)
# describeFactors(cp.cov$unknown_aq)

# replace the No entry with NA
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(travel, contact, hcw, other_aq), ~ifelse(.x == "No entry", NA, .x)))

### symptoms
cp.cov <- cp.cov %>% 
  mutate(asym = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_asymptomatic == 1 ~ "Yes",
    e_asymptomatic == 0 ~ "No"
  ),
  fever = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_fever == 1 ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_fever == 0 ~ "No"
  ),
  cough = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_cough == 1 ~ "Yes",
    e_cough == 0 ~ "No"
  ),
  head = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_head == 1 ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_head == 0 ~ "No"
  ),
  breath = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_breath == 1 ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_breath == 0 ~ "No"
  ),
  runny = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_runny == 1 ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec, ignore.case = TRUE) & !grepl("chest", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec_2, ignore.case = TRUE) & !grepl("chest", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_runny == 0 ~ "No"
  ),
  weakness = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("weakness", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("weakness", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_othersx == 0 ~ "No"
  ),
  muscle = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_muscle == 1 ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_muscle == 0 ~ "No"
  ),
  anorexia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_anorexia == 1 ~ "Yes",
    e_anorexia == 0 ~ "No"
  ),
  diarrhea = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_diarrhea == 1 ~ "Yes",
    e_diarrhea == 0 ~ "No"
  ),
  vomit = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_vomit == 1 ~ "Yes",
    e_vomit == 0 ~ "No"
  ),
  fatigue = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_fatigue == 1 ~ "Yes",
    e_fatigue == 0 ~ "No"
  ),
  anosmia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_anosmia == 1 ~ "Yes",
    e_anosmia == 0 ~ "No"
  ),
  throat = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_throat == 1 ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_throat == 0 ~ "No"
  ),
  sputum = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_sputum == 1 ~ "Yes",
    e_sputum == 0 ~ "No"
  ),
  malaise = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_malaise == 1 ~ "Yes",
    e_malaise == 0 ~ "No"
  ),
  chest_pain = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    TRUE ~ "No"
  ),
  tachycardia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    TRUE ~ "No"
  ),
  # other = case_when(
  #   all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
  #   !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec, ignore.case = TRUE) & !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec_2, ignore.case = TRUE) & !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec_3, ignore.case = TRUE) & e_othersx == 1 ~ "Yes",
  #   e_othersx == 0 ~ "No"
  # )
  )

# replace the No entry with NA
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(asym, fever, cough, head, breath, runny, muscle, anorexia, diarrhea, vomit, fatigue, anosmia, throat, sputum, malaise, chest_pain, tachycardia, weakness), ~ifelse(.x == "Miss", NA, .x)))
describeFactors(cp.cov$fever)
describeFactors(cp.cov$chest_pain)


# given these are major outcomes, we will conservatively assume that NA = no event rather than missing data
# maternal death
cp.cov$e_death

cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(e_pneumonia, e_sepsis, e_respfail, e_respdist, e_heartfail, e_shock, e_coag, e_renal, e_liver, e_dic, e_hosp, g_icu, e_death, e_ecmo), ~case_when(.x == 1 ~ 1, TRUE ~ 0)))

# pneumonia or abnormal x-ray
cp.cov$abx_pne <- case_when(
  cp.cov$e_xray_result == 1 | cp.cov$e_pneumonia == 1 ~ 1,
  TRUE ~ 0)

# composite outcome
cp.cov$comp.pne_o2 <- case_when(
  cp.cov$abx_pne == 1 | cp.cov$e_oxygen == 1 ~ 1,
  TRUE ~ 0
)

cp.cov$comp.icu_vent_ecmo <- case_when(
  cp.cov$g_icu == 1 | cp.cov$e_inv == 1 | cp.cov$e_ecmo == 1 ~ 1,
  TRUE ~ 0
)

cp.cov$e_oxygen___1 <- cp.cov$e_oxygen
cp.cov$g_icu_dur <- cp.cov$g_icu_duration
cp.cov$e_inv___1 <- cp.cov$e_inv


# thrombosis

describeFactors(cp.cov$e_dvt)
describeFactors(cp.cov$e_pe)



### maternal comorbidities
# These are difficult because the number of missing responses is hard to determine. 
# All variables get either a 1 or a 0, or a NA if nothing was checked. 
# The problem is that people doing data entry will often just skip the ones that a person does not have, 
# and just fill in the ones they do. This is not consistent, unfortunately, but speeds up data entry for them.
# This makes it difficult to determine if someone is missing a response because they did not have that comorbidity,
# or if they are missing because there was no data at all on comorbidities available. 
# To determine an approximately correct number missing, we need to evaluate how many were either not "none" or 0
# across the board indicating that they never had an entry - likely indicating no data was available. 
# If they also have some delivery data, we can count these as No because data entry was likely complete 
# for these participants. A 999 indicates unknown as well.

# these all may have different numbers "Missing"
describeFactors(cp.ant$j_none___1) # if this is 1, they had no comorbidities and that section was skipped
describeFactors(cp.ant$j_cns)
describeFactors(cp.ant$j_hem)
describeFactors(cp.ant$j_resp)
# ...

# determine the number missing
cp.ant <- cp.ant %>%
  mutate(cvs = case_when(
    all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
    all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
    j_none___1 == 1 ~ "No",
    j_cvs == 1 ~ "Yes",
    l_htn == 1 ~ "Yes",
    j_cvs == 0 ~ "No",
    is.na(j_cvs) ~ "No"
  ))
# ,
#   cns = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_cns == 1 ~ "Yes",
#     j_cns == 0 ~ "No",
#     is.na(j_cns) ~ "No"
#   ),
#   resp = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_resp == 1 ~ "Yes",
#     j_resp == 0 ~ "No",
#     is.na(j_resp) ~ "No"
#   ),
#   eentm = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_eentm == 1 ~ "Yes",
#     j_eentm == 0 ~ "No",
#     is.na(j_eentm) ~ "No"
#   ),
#   gi = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_gi == 1 ~ "Yes",
#     j_gi == 0 ~ "No",
#     is.na(j_gi) ~ "No"
#   ),
#   gu = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_gu == 1 ~ "Yes",
#     j_gu == 0 ~ "No",
#     is.na(j_gu) ~ "No"
#   ),
#   repro = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_repro == 1 ~ "Yes",
#     j_repro == 0 ~ "No",
#     is.na(j_repro) ~ "No"
#   ),
#   endo = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_endo == 1 ~ "Yes",
#     j_endo == 0 ~ "No",
#     is.na(j_endo) ~ "No"
#   ),
#   ms = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_ms == 1 ~ "Yes",
#     j_ms == 0 ~ "No",
#     is.na(j_ms) ~ "No"
#   ),
#   hem = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_hem == 1 ~ "Yes",
#     j_hem == 0 ~ "No",
#     is.na(j_hem) ~ "No"
#   ),
#   mh = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_mh == 1 ~ "Yes",
#     j_mh == 0 ~ "No",
#     is.na(j_mh) ~ "No"
#   ),
#   aai = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_aai == 1 ~ "Yes",
#     j_aai == 0 ~ "No",
#     is.na(j_aai) ~ "No"
#   ),
#   other_comor = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_oth == 1 ~ "Yes",
#     j_oth == 0 ~ "No",
#     is.na(j_oth) ~ "No"
#   )
#   )

# # replace the No entry with NA
# cp.ant <- cp.ant %>% 
#   mutate(across(.cols = c(cvs, cns, resp, eentm, gi, gu, repro, endo, ms, hem, mh, aai, other_comor), ~ifelse(.x == "No entry", NA, .x)))

# replace the No entry with NA
cp.ant <- cp.ant %>% 
  mutate(across(.cols = c(cvs), ~ifelse(.x == "No entry", NA, .x)))


# check to make sure
describeFactors(cp.ant$cvs)

## extract specific comorbidities of interest
# asthma
cp.ant$asthma <- case_when(
  cp.ant$j_resp_asth___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$asthma)

# Chronic obstructive lung disease
cp.ant$lung <- case_when(
  cp.ant$j_resp_lung___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$lung)

# hypertension not including gestational
cp.ant$htn <- case_when(
  cp.ant$j_cvs_htn___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$htn)

# diabetes 1 or 2 not including gestational
cp.ant$diabetes <- case_when(
  cp.ant$j_endo_diabt1___1 == 1 | cp.ant$j_endo_diabt2___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$diabetes)


# autoimmune diseases have to be gathered from a few places
# celiac, lupus, rheumatoid arthritis, Ankylosing spondylitis, others
cp.ant$j_aai_oth_s # check this variable for others eg MS

cp.ant$autoimm <- case_when(
  cp.ant$j_gi_celiac___1 == 1 | cp.ant$j_ms_lupus___1 == 1 | cp.ant$j_ms_as___1 == 1 | cp.ant$j_ms_ra___1 | grepl("Multiple sclerosis|Antiphospho|neutropenia|SLE|lupus", cp.ant$j_aai_oth_s, ignore.case = TRUE) ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$autoimm)

## composite comorbidities
# c(cvs, diabetes, htn, asthma, hbv, hcv)

cp.ant$any_comor <- case_when(
  cp.ant$cvs == "Yes" | cp.ant$asthma == "Yes" | cp.ant$diabetes == "Yes" | cp.ant$htn == "Yes" | cp.ant$k_hbv == 1 | cp.ant$k_hcv == 1 ~ "Yes",
  cp.ant$cvs == "No" & cp.ant$asthma == "No" & cp.ant$diabetes == "No" & cp.ant$htn == "No" ~ "No"
)


# maternal smoking during pregnancy
cp.ant$smoking <- case_when(
  cp.ant$i_smokingd == 999 | is.na(cp.ant$i_smokingd) ~ NA_character_,
  cp.ant$i_smokingd == 0 ~ "No",
  cp.ant$i_smokingd == 1 ~ "Yes"
)

describeFactors(cp.ant$smoking)

# HepB sAg positive
cp.ant$k_hbv
cp.ant$k_hbv <- ifelse(cp.ant$k_hbv == 999 | cp.ant$k_hbv == 888, NA, cp.ant$k_hbv)

# HepC Ab+
cp.ant$k_hcv
cp.ant$k_hcv <- ifelse(cp.ant$k_hcv == 999 | cp.ant$k_hcv == 888, NA, cp.ant$k_hcv)

# Gravida
cp.ant$gravida <- cp.ant$h_gravida
cp.ant$gravida <- factor(case_when(
  cp.ant$gravida == 0 ~ "0",
  cp.ant$gravida == 1 ~ "1",
  cp.ant$gravida >= 2 ~ "2+"
))

# ga at delivery
cp.ant$date_conc <- cp.ant$i_deliverydate_est - 280
cp.int <- merge(cp.int, cp.ant[, c("a_record_id", "date_conc")], by = "a_record_id")

cp.int$r_dob <- as.Date(cp.int$r_dob)

cp.int$ga_at_del <- as.numeric(cp.int$r_dob - cp.int$date_conc)/7
cp.int$ga_del_cat <- case_when(
  cp.int$ga_at_del < 28 ~ "extremely preterm",
  cp.int$ga_at_del < 32 ~ "very preterm",
  cp.int$ga_at_del < 34 ~ "moderate preterm",
  cp.int$ga_at_del < 37 ~ "late preterm",
  cp.int$ga_at_del >= 37 ~ "term",
)

## losses/stillbirth CHECK STILLBIRTHS!! MAY NOT BE BEING CAPTURED BY R_DOB
cp.int$p_outcome <- case_when(
  cp.int$p_outcome == 1 ~ "Loss",
  cp.int$p_outcome == 2 ~ "Stillbirth",
  cp.int$p_outcome == 3 ~ "Livebirth"
)

cp.int$p_outcome[which(cp.int$a_record_id == "186-22")] <- "Stillbirth"

# mode of delivery
# Vag/CS
cp.int$mode_del <- case_when(
  cp.int$p_mode == 1 ~ "Vaginal",
  cp.int$p_mode == 2 ~ "CS"
)

# 5 minute apgar
cp.int$apgar5 <- factor(ifelse(cp.int$s_apgar_5 < 7, "<7", "≥7"))

# birth weight
cp.int$bw_cat <- case_when(
  cp.int$s_bw_gm < 2500 ~ "<2500",
  cp.int$s_bw_gm >= 2500 & cp.int$s_bw_gm <=4000 ~ "2500-4000",
  cp.int$s_bw_gm > 4000 ~ ">4000"
)

# NICU admission
# assuming if we have birth weight then we should know if baby admitted to NICU or not
cp.int$NICU <- case_when(
  cp.int$t_nicu == 1 ~ "Yes",
  cp.int$t_nicu == 0 ~ "No",
  is.na(cp.int$s_bw_gm) == FALSE ~ "No")

# duration of admission
cp.int$nicu_dur <- as.numeric(as.Date(cp.int$t_nicu_discharge) - as.Date(cp.int$t_nicu_admission))

# time between delivery and diagnosis

cp.int <- left_join(cp.int, cp.cov[, c("a_record_id", "d_naso1_collect")])
cp.int$time_del <- as.numeric(as.Date(cp.int$r_dob) - as.Date(cp.int$d_naso1_collect))
# are any negative? 
summary(cp.int$time_del)
# if so, either remove or fix the dates

# time between stillbirth and diagnosis
cp.ant$time_lossSB <- as.numeric(as.Date(cp.ant$o_date) - as.Date(cp.ant$d_naso1_collect))
summary(cp.ant$time_lossSB)
# if so, either remove or fix the dates
# 
# 
# # make weight vectors
# ### From: A New and Improved Population-Based Canadian Reference for Birth Weight for Gestational Age, Michael S. Kramer, Robert W. Platt, Shi Wu Wen, K.S. Joseph, Alexander Allen, Michal Abrahamowicz, Béatrice Blondel, Gérard Bréart and for the Fetal/Infant Health Study Group of the Canadian Perinatal Surveillance System Pediatrics 2001;108;e35 DOI: 10.1542/peds.108.2.e35
# 
# ga.F <- seq(22, 43)
# weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above
# 
# weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above
# 
# # if unknown go with male to be conservative
# SGA <- data.frame(ga.F, weight.F, weight.M)
# 
# # function to determine SGA
# sga.fun <- function(dat){
#   sga <- c()
#   for(i in 1:dim(dat)[1]){
#     
#     if(is.na(dat$s_bw_gm[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$s_bw_gm[i]) & is.na(dat$s_pe_genitalia[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$ga_at_del[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(dat$ga_at_del[i] < 22 | dat$ga_at_del[i] > 43){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$s_pe_genitalia[i])){
#       w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#     
#     if(dat$s_pe_genitalia[i] == 2){
#       w.thresh <- SGA$weight.F[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#     
#     if(dat$s_pe_genitalia[i] == 1){
#       w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#   }
#   return(sga)
# }
# 
# cp.int$SGA <- sga.fun(cp.int)

# combine cov and ant
cp.cov.ant <- left_join(cp.cov, cp.ant, by = "a_record_id")
cp.cov.ant.dem <- left_join(cp.cov.ant, cp.dem, by = "a_record_id")
cp.cov.ant.dem.int <- left_join(cp.cov.ant.dem, cp.int, by = "a_record_id")
cp.cov.ant.dem.int.oth <- left_join(cp.cov.ant.dem.int, cp.oth, by = "a_record_id")

##################### WRITE QC DATA ######################
# replace the filepath, to one appropriate for your project
write_csv(cp.cov.ant.dem.int.oth, file = here("COVID_PREG_cleaned_QC_2023-02-26.csv"))





####### LOAD IN OTHER DATA #########
bc_data <- read.csv("CanadianSurveillance_DATA_2024-01-16_1232.csv",header = TRUE, na.strings = c("", NA, 999))
dim(bc_data)
## bc data needs to be cleaned

other_data <- read.csv("CanadianCOVID19InPre_DATA_2024-01-16_1237.csv",header = TRUE, na.strings = c("", NA, 999))

dim(other_data)
## other data needs to be cleaned as well.



######## CLEAN ALL DATA ##########


#### BC DATA ####
cp <- bc_data
# ????????? need to restrict to data with delivery up to July 4th, 2022
levels(factor(cp$redcap_event_name))

cp.cov <- cp[which(cp$redcap_event_name == "sarscov2_arm_1"), ]
cp.dem <- cp[which(cp$redcap_event_name == "demographics_arm_1"), ]
cp.ant <- cp[which(cp$redcap_event_name == "antepartum_arm_1"), ]
cp.int <- cp[which(cp$redcap_event_name == "intrapartum_arm_1"), ]
cp.pos <- cp[which(cp$redcap_event_name == "postpartum_arm_1"), ]
cp.oth <- cp[which(cp$redcap_event_name == "other_forms_arm_1"), ]

### need to include "other" forms

# clean out any empty columns
cp.cov <- remove_empty(dat = cp.cov, quiet = TRUE)
cp.dem <- remove_empty(dat = cp.dem, quiet = TRUE)
cp.ant <- remove_empty(dat = cp.ant, quiet = TRUE)
cp.int <- remove_empty(dat = cp.int, quiet = TRUE)
cp.pos <- remove_empty(dat = cp.pos, quiet = TRUE)
cp.oth <- remove_empty(dat = cp.oth, quiet = TRUE)


## check for still births and losses and replace r_dob with o_date

cc <- cp.ant$a_record_id[which(cp.ant$o_sb == 1 | cp.ant$o_spont == 1)] 

cc2 <- cp.int$a_record_id[which(cp.int$a_record_id %in% cc)]

cp.int$r_dob[which(cp.int$a_record_id %in% cc)] <- cp.ant$o_date[which(cp.ant$a_record_id %in% cc2)]


## demographic variables
## ethnicity:
# 1 = White/Caucasian, 2 = African/Caribbean/Black, 3 = Hispanic/Latino, 4 = East Asian, 5 = South Asian, 6 = South East Asian, 7 = Middle East, 8 = Indigenous, 998 = Other, 999 = Unknown
# missing data are not reliably coded here. Has to be inferred from 999 and if all entries for all possible ethnicities is 0 (no option checked).

describeFactors(cp.dem$b_ethnicity___1) # White/Caucasian
describeFactors(cp.dem$b_ethnicity___999)

cp.dem <- cp.dem %>% 
  rowwise() %>% 
  mutate(eth = case_when(
    all(c(b_ethnicity___1, b_ethnicity___2, b_ethnicity___3, b_ethnicity___4, b_ethnicity___5, b_ethnicity___6, b_ethnicity___7, b_ethnicity___8, b_ethnicity___998, b_ethnicity___999) == 0) ~ "Missing",
    b_ethnicity___999 == 1 ~ "Unknown",
    b_ethnicity___998 == 1 ~ "Other",
    b_ethnicity___1 == 1 ~ "White",
    b_ethnicity___2 == 1 ~ "African/Carribean/Black",
    b_ethnicity___3 == 1 ~ "Hispanic/Latino",
    b_ethnicity___4 == 1 ~ "East Asian",
    b_ethnicity___5 == 1 ~ "South Asian",
    b_ethnicity___6 == 1 ~ "South East Asian",
    b_ethnicity___7 == 1 ~ "Middle East",
    b_ethnicity___8 == 1 ~ "Indigenous"
  ))

describeFactors(cp.dem$eth)
cp.dem$eth <- factor(cp.dem$eth)


# change the missing and unknown to NA
levels(cp.dem$eth)[which(levels(cp.dem$eth) == "Missing" | levels(cp.dem$eth) == "Unknown")] <- NA

# reduce the number of categories
cp.dem$eth_cat <- cp.dem$eth
levels(cp.dem$eth_cat)[which(levels(cp.dem$eth) == "East Asian" | levels(cp.dem$eth) == "South East Asian")] <- "East or SE Asian"
cp.dem$eth_cat <- relevel(cp.dem$eth_cat, ref = "White")
describeFactors(cp.dem$eth_cat)


## AGE
summary(cp.dem$b_age) # check that Missing are coded as NA and not as 999 or something like that. Should be NA
cp.dem$b_age <- ifelse(cp.dem$b_age == 999, NA, cp.dem$b_age)

## Employment
describeFactors(cp.dem$b_employment)
cp.dem$employ <- case_when(
  cp.dem$b_employment == 1 ~ "Employed",
  cp.dem$b_employment == 2 ~ "Unemployed"
)
describeFactors(cp.dem$employ)


### Education
# number of years of school - missing coded as 999 or 888 - recode as NA
summary(cp.dem$b_school)
cp.dem$education <- replace(cp.dem$b_school, which(cp.dem$b_school == 999 | cp.dem$b_school == 888), NA)
summary(cp.dem$education)
# nearly all missing - likely won't use this variable



### symptom onset date - many missing
summary(as.Date(cp.cov$e_onset))

### testing date - more complete
summary(as.Date(cp.cov$e_diagnosis)) # check that these dates make sense. Fix or exclude ones that are errors

### calculate gestational age at testing
# EDD is only available on the cp.ant data set, therefore, some of the people tested will be missing GA at testing/diagnosis
# cp.ant$i_deliverydate_est # EDD
cp.ant$i_deliverydate_est <- as.Date(as.character(cp.ant$i_deliverydate_est), format = "%Y-%m-%d")
summary(cp.ant$i_deliverydate_est)

# fix
cp.ant <- merge(cp.ant, cp.cov[, c("a_record_id", "e_diagnosis")], by = "a_record_id")

cp.ant$e_diagnosis <- as.Date(cp.ant$e_diagnosis)
# replace 9999-09-09 with NA
cp.ant$i_deliverydate_est <- replace(cp.ant$i_deliverydate_est, which(cp.ant$i_deliverydate_est == "9999-09-09"), NA)

# split maternal age and GA at diagnosis into categories
cp.dem$age_cat <- factor(case_when(
  cp.dem$b_age <25 ~ "<25 years",
  cp.dem$b_age <30 ~ "25-29 years",
  cp.dem$b_age <36 ~ "30-35 years",
  cp.dem$b_age <40 ~ "36-39 years",
  cp.dem$b_age >=40 ~ "≥40 years"
),
levels = c("<25 years", "25-29 years", "30-35 years", "36-39 years", "≥40 years"))

describeFactors(cp.dem$age_cat)

# maternal BMI
cp.ant$i_weight <- replace(cp.ant$i_weight, which(cp.ant$i_weight == 999 | cp.ant$i_weight == 666), NA)
cp.ant$i_height <- replace(cp.ant$i_height, which(cp.ant$i_height == 999 | cp.ant$i_height == 666), NA)

cp.ant$BMI <- cp.ant$i_weight/(cp.ant$i_height/100)^2
describeMedian(cp.ant$BMI, iqr = FALSE) # check for out of range values - and remove or correct

# BMI >= 30 variable
cp.ant$BMI_cat <- factor(case_when(
  cp.ant$BMI < 18.5 ~ "<18.5",
  cp.ant$BMI < 25 ~ "18.5-24",
  cp.ant$BMI < 30 ~ "25-29",
  cp.ant$BMI >= 30 ~ "≥30"
))
describeFactors(cp.ant$BMI_cat)
cp.ant$BMI_cat <- factor(cp.ant$BMI_cat, levels = c("<18.5", "18.5-24", "25-29", "≥30"))


cp.cov <- cp.cov %>% 
  rowwise() %>% 
  mutate(travel = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_travel___1 == 1 ~ "Yes",
    c_travel___1 == 0 ~ "No"
  ),
  contact = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_contact___1 == 1 ~ "Yes",
    c_contact___1 == 0 ~ "No"
  ),
  hcw = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_hcw___1 == 1 ~ "Yes",
    c_hcw___1 == 0 ~ "No"
  ),
  other_aq = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_other___1 == 1 ~ "Yes",
    c_other___1 == 0 ~ "No"
  ),
  unknown_aq = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_unknown___1 == 1 ~ "Yes",
    c_unknown___1 == 0 ~ "No"
  ))

describeFactors(cp.cov$travel)
describeFactors(cp.cov$contact)
describeFactors(cp.cov$hcw)
describeFactors(cp.cov$other_aq)
describeFactors(cp.cov$unknown_aq)

# replace the No entry with NA
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(travel, contact, hcw, other_aq, unknown_aq), ~ifelse(.x == "No entry", NA, .x)))






### symptoms
cp.cov <- cp.cov %>% 
  mutate(asym = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_asymptomatic == 1 ~ "Yes",
    e_asymptomatic == 0 ~ "No"
  ),
  fever = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_fever == 1 ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_fever == 0 ~ "No"
  ),
  cough = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_cough == 1 ~ "Yes",
    e_cough == 0 ~ "No"
  ),
  head = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_head == 1 ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_head == 0 ~ "No"
  ),
  breath = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_breath == 1 ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_breath == 0 ~ "No"
  ),
  runny = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_runny == 1 ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec, ignore.case = TRUE) & !grepl("chest", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec_2, ignore.case = TRUE) & !grepl("chest", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec_3, ignore.case = TRUE) & !grepl("chest", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_runny == 0 ~ "No"
  ),
  weakness = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("weakness", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("weakness", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_othersx == 0 ~ "No"
  ),
  muscle = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_muscle == 1 ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_muscle == 0 ~ "No"
  ),
  anorexia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_anorexia == 1 ~ "Yes",
    e_anorexia == 0 ~ "No"
  ),
  diarrhea = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_diarrhea == 1 ~ "Yes",
    e_diarrhea == 0 ~ "No"
  ),
  vomit = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_vomit == 1 ~ "Yes",
    e_vomit == 0 ~ "No"
  ),
  fatigue = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_fatigue == 1 ~ "Yes",
    e_fatigue == 0 ~ "No"
  ),
  anosmia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_anosmia == 1 ~ "Yes",
    e_anosmia == 0 ~ "No"
  ),
  throat = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_throat == 1 ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_throat == 0 ~ "No"
  ),
  sputum = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_sputum == 1 ~ "Yes",
    e_sputum == 0 ~ "No"
  ),
  malaise = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_malaise == 1 ~ "Yes",
    e_malaise == 0 ~ "No"
  ),
  chest_pain = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    TRUE ~ "No"
  ),
  tachycardia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    TRUE ~ "No"
  )
  # other = case_when(
  #   all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
  #   !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec, ignore.case = TRUE) & !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec_2, ignore.case = TRUE) & !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec_3, ignore.case = TRUE) & e_othersx == 1 ~ "Yes",
  #   e_othersx == 0 ~ "No"
  # )
  )


# replace the No entry with NA
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(asym, fever, cough, head, breath, runny, muscle, anorexia, diarrhea, vomit, fatigue, anosmia, throat, sputum, malaise, chest_pain, tachycardia, weakness), ~ifelse(.x == "Miss", NA, .x)))
describeFactors(cp.cov$fever)
describeFactors(cp.cov$chest_pain)

# given these are major outcomes, we will conservatively assume that NA = no event rather than missing data
# maternal death
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(e_pneumonia, e_sepsis, e_respfail, e_respdist, e_heartfail, e_shock, e_coag, e_renal, e_liver, e_dic, e_hosp, g_icu, e_death), ~case_when(.x == 1 ~ 1, TRUE ~ 0)))

# pneumonia or abnormal x-ray
cp.cov$abx_pne <- case_when(
  cp.cov$e_xray_result == 1 | cp.cov$e_pneumonia == 1 ~ 1,
  TRUE ~ 0)

# composite outcome
cp.cov$composite <- case_when(
  cp.cov$e_hosp == 1 | cp.cov$abx_pne == 1 | cp.cov$g_icu == 1 | cp.cov$e_oxygen___1 == 1 | cp.cov$e_inv___1 == 1 | cp.cov$e_coag == 1 | cp.cov$e_sepsis == 1 | cp.cov$e_death == 1 ~ 1,
  TRUE ~ 0
)

# thrombosis
any(grepl("pul|PE|DVT", cp.cov$e_compoth_specify))

cp.cov <- cp.cov %>% 
  mutate(
    thrombo_details1 = case_when(
      grepl("DVT|embolus|embolism", e_compoth_specify, ignore.case = TRUE) ~ e_compoth_specify,
      grepl("DVT|embolus|embolism", e_othersx_spec, ignore.case = TRUE) ~ e_othersx_spec,
      grepl("PE", e_compoth_specify, ignore.case = F) ~ e_compoth_specify,
      grepl("PE", e_othersx_spec, ignore.case = F) ~ e_othersx_spec
    ),
    thrombo_details2 = case_when(
      grepl("DVT|embolus|embolism", e_compoth_specify_2, ignore.case = TRUE) ~ e_compoth_specify_2,
      grepl("DVT|embolus|embolism", e_othersx_spec_2, ignore.case = TRUE) ~ e_othersx_spec_2,
      grepl("PE", e_compoth_specify_2, ignore.case = F) ~ e_compoth_specify_2,
      grepl("PE", e_othersx_spec_2, ignore.case = F) ~ e_othersx_spec_2
    )
  )

describeFactors(cp.cov$thrombo_details1)
describeFactors(cp.cov$thrombo_details2)



cp.cov <- cp.cov %>% 
  mutate(
    dvt = case_when(
      grepl("DVT", thrombo_details1, ignore.case = TRUE) | grepl("DVT", thrombo_details2, ignore.case = TRUE) ~ "DVT",
      is.na(thrombo_details1) ~ "No",
      TRUE ~ "No"
    ),
    pul_embo = case_when(
      grepl("ruled out", thrombo_details1, ignore.case = TRUE) | grepl("ruled out", thrombo_details2, ignore.case = TRUE) ~ "pulmonary embolism",
      grepl("pulmonary embolism", thrombo_details1, ignore.case = TRUE) | grepl("pulmonary embolism", thrombo_details2, ignore.case = TRUE) ~ "pulmonary embolism",
      is.na(thrombo_details1) ~ "No",
      TRUE ~ "No"
    )
  )

describeFactors(cp.cov$dvt)
describeFactors(cp.cov$pul_embo)

cp.cov$e_dvt <- ifelse(cp.cov$dvt == "DVT", 1, 0)
cp.cov$e_pe <- ifelse(cp.cov$pul_embo == "pulmonary embolism", 1, 0)




### maternal comorbidities
# These are difficult because the number of missing responses is hard to
# determine. All variables get either a 1 or a 0, or a NA if nothing was checked. 
# The problem is that people doing data entry will often just skip the ones that a person 
# does not have, and just fill in the ones they do. This is not consistent, 
# unfortunately, but speeds up data entry for them. This makes it difficult to 
# determine if someone is missing a response because they did not have that 
# comorbidity, or if they are missing because there was no data at all on comorbidities available. 
# To determine an approximately correct number missing, we need to evaluate how many were either not "none" or 0 
# across the board indicating that they never had an entry - likely indicating no data was available. 
# If they also have some delivery data, we can count these as No because data entry was likely complete for these participants. A 999 indicates unknown as well.

# these all may have different numbers "Missing"
describeFactors(cp.ant$j_none___1) # if this is 1, they had no comorbidities and that section was skipped
describeFactors(cp.ant$j_cns)
describeFactors(cp.ant$j_hem)
describeFactors(cp.ant$j_resp)
# ...

# determine the number missing
cp.ant <- cp.ant %>%
  rowwise() %>%
  mutate(cvs = case_when(
    all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
    all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
    j_none___1 == 1 ~ "No",
    j_cvs == 1 ~ "Yes",
    l_htn == 1 ~ "Yes",
    j_cvs == 0 ~ "No",
    is.na(j_cvs) ~ "No"
  ))

# cp.ant <- cp.ant %>%
#   rowwise() %>%
#   mutate(cns = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_cns == 1 ~ "Yes",
#     j_cns == 0 ~ "No",
#     is.na(j_cns) ~ "No"
#   ),
#   resp = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_resp == 1 ~ "Yes",
#     j_resp == 0 ~ "No",
#     is.na(j_resp) ~ "No"
#   ),
#   eentm = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_eentm == 1 ~ "Yes",
#     j_eentm == 0 ~ "No",
#     is.na(j_eentm) ~ "No"
#   ),
#   gi = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_gi == 1 ~ "Yes",
#     j_gi == 0 ~ "No",
#     is.na(j_gi) ~ "No"
#   ),
#   gu = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_gu == 1 ~ "Yes",
#     j_gu == 0 ~ "No",
#     is.na(j_gu) ~ "No"
#   ),
#   repro = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_repro == 1 ~ "Yes",
#     j_repro == 0 ~ "No",
#     is.na(j_repro) ~ "No"
#   ),
#   endo = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_endo == 1 ~ "Yes",
#     j_endo == 0 ~ "No",
#     is.na(j_endo) ~ "No"
#   ),
#   ms = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_ms == 1 ~ "Yes",
#     j_ms == 0 ~ "No",
#     is.na(j_ms) ~ "No"
#   ),
#   hem = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_hem == 1 ~ "Yes",
#     j_hem == 0 ~ "No",
#     is.na(j_hem) ~ "No"
#   ),
#   mh = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_mh == 1 ~ "Yes",
#     j_mh == 0 ~ "No",
#     is.na(j_mh) ~ "No"
#   ),
#   aai = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_aai == 1 ~ "Yes",
#     j_aai == 0 ~ "No",
#     is.na(j_aai) ~ "No"
#   ),
#   other_comor = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_oth == 1 ~ "Yes",
#     j_oth == 0 ~ "No",
#     is.na(j_oth) ~ "No"
#   )
#   )

# #replace the No entry with NA
# cp.ant <- cp.ant %>%
#   mutate(across(.cols = c(cvs, cns, resp, eentm, gi, gu, repro, endo, ms, hem, mh, aai, other_comor), ~ifelse(.x == "No entry", NA, .x)))
cp.ant <- cp.ant %>%
  mutate(across(.cols = c(cvs), ~ifelse(.x == "No entry", NA, .x)))

# check to make sure
describeFactors(cp.ant$cvs)

## extract specific comorbidities of interest
# asthma
cp.ant$asthma <- case_when(
  cp.ant$j_resp_asth___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$asthma)

# Chronic obstructive lung disease
cp.ant$lung <- case_when(
  cp.ant$j_resp_lung___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$lung)

# hypertension pre-existing
cp.ant$htn <- case_when(
  cp.ant$j_cvs_htn___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$htn)

# diabetes 1 or 2
cp.ant$diabetes <- case_when(
  cp.ant$j_endo_diabt1___1 == 1 | cp.ant$j_endo_diabt2___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$diabetes)


# autoimmune diseases have to be gathered from a few places
# celiac, lupus, rheumatoid arthritis, Ankylosing spondylitis, others
cp.ant$j_aai_oth_s # check this variable for others eg MS

cp.ant$autoimm <- case_when(
  cp.ant$j_gi_celiac___1 == 1 | cp.ant$j_ms_lupus___1 == 1 | cp.ant$j_ms_as___1 == 1 | cp.ant$j_ms_ra___1 | grepl("Multiple sclerosis|Antiphospho|neutropenia|SLE|lupus", cp.ant$j_aai_oth_s, ignore.case = TRUE) ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$autoimm)

## composite comorbidities
c(cp.ant$cvs, cp.ant$diabetes, cp.ant$htn, cp.ant$asthma, cp.ant$k_hbv, cp.ant$k_hcv)

cp.ant$any_comor <- case_when(
  cp.ant$cvs == "Yes" | cp.ant$asthma == "Yes" | cp.ant$diabetes == "Yes" | cp.ant$htn == "Yes" | cp.ant$k_hbv == 1 | cp.ant$k_hcv == 1 ~ "Yes",
  cp.ant$cvs == "No" & cp.ant$asthma == "No" & cp.ant$diabetes == "No" & cp.ant$htn == "No" ~ "No"
)



# maternal smoking during pregnancy
cp.ant$smoking <- case_when(
  cp.ant$i_smokingd == 999 | is.na(cp.ant$i_smokingd) ~ NA_character_,
  cp.ant$i_smokingd == 0 ~ "No",
  cp.ant$i_smokingd == 1 ~ "Yes"
)

describeFactors(cp.ant$smoking)

# HepB sAg positive
cp.ant$k_hbv
cp.ant$k_hbv <- ifelse(cp.ant$k_hbv == 999 | cp.ant$k_hbv == 888, NA, cp.ant$k_hbv)

# HepC Ab+
cp.ant$k_hcv
cp.ant$k_hcv <- ifelse(cp.ant$k_hcv == 999 | cp.ant$k_hcv == 888, NA, cp.ant$k_hcv)


# Gravida
cp.ant$gravida <- cp.ant$h_gravida
cp.ant$gravida <- factor(case_when(
  cp.ant$gravida == 0 ~ "0",
  cp.ant$gravida == 1 ~ "1",
  cp.ant$gravida >= 2 ~ "2+"
))

## losses/stillbirth
cp.int$p_outcome <- case_when(
  cp.int$p_outcome == 1 ~ "Loss",
  cp.int$p_outcome == 2 ~ "Stillbirth",
  cp.int$p_outcome == 3 ~ "Livebirth"
)


# mode of delivery
# Vag/CS
cp.int$mode_del <- case_when(
  cp.int$p_mode == 1 ~ "Vaginal",
  cp.int$p_mode == 2 ~ "CS"
)

# 5 minute apgar
cp.int$apgar5 <- factor(ifelse(cp.int$s_apgar_5 < 7, "<7", "≥7"))

# birth weight
cp.int$bw_cat <- case_when(
  cp.int$s_bw_gm < 2500 ~ "<2500",
  cp.int$s_bw_gm >= 2500 & cp.int$s_bw_gm <=4000 ~ "2500-4000",
  cp.int$s_bw_gm > 4000 ~ ">4000"
)

# NICU admission
# assuming if we have birth weight then we should know if baby admitted to NICU or not
cp.int$NICU <- case_when(
  cp.int$t_nicu == 1 ~ "Yes",
  cp.int$t_nicu == 0 ~ "No",
  is.na(cp.int$s_bw_gm) == FALSE ~ "No")

# duration of admission
cp.int$nicu_dur <- as.numeric(as.Date(cp.int$t_nicu_discharge) - as.Date(cp.int$t_nicu_admission))

cp.ant$ga_at_diag <- as.numeric((280 - (as.Date(cp.ant$i_deliverydate_est) - as.Date(cp.cov$d_naso1_collect)))/7) # in weeks
# describeMedian(cp.ant$ga_at_diag, iqr = FALSE) # check the range and fix any that are not possible - or remove. <0 and >43 weeks
cp.ant$ga_at_diag <- replace(cp.ant$ga_at_diag, which(cp.ant$ga_at_diag >43 | cp.ant$ga_at_diag < 0), NA)

cp.ant$ga_dx_cat <- factor(case_when(
  cp.ant$ga_at_diag <= 14 ~ "<=14 weeks",
  cp.ant$ga_at_diag < 27 ~ "14-27 weeks",
  cp.ant$ga_at_diag <38 ~ "28-38 weeks",
  cp.ant$ga_at_diag >=38 ~ "≥38 weeks"
), levels = c("<=14 weeks", "14-27 weeks", "28-38 weeks", "≥38 weeks"))

# ############################# make weight vectors #####################
# ### From: A New and Improved Population-Based Canadian Reference for Birth Weight for Gestational Age, Michael S. Kramer, Robert W. Platt, Shi Wu Wen, K.S. Joseph, Alexander Allen, Michal Abrahamowicz, Béatrice Blondel, Gérard Bréart and for the Fetal/Infant Health Study Group of the Canadian Perinatal Surveillance System Pediatrics 2001;108;e35 DOI: 10.1542/peds.108.2.e35
# 
# ga.F <- seq(22, 43)
# weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above
# 
# weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above
# 
# # if unknown go with male to be conservative
# SGA <- data.frame(ga.F, weight.F, weight.M)
# 
# # need to create new column for ga_at_del by calculating it
# # calculation referenced in REDCap is 40-(datediff([r_dob],[i_deliverydate_est], "d", "dmy", true)/7)
# # started to do this within the csv but many errors...
# 
# cp.int$r_dob <- as.Date(as.character(cp.int$r_dob), format = "%Y-%m-%d")
# 
# cp.int$r_dob <- replace(cp.int$r_dob, which(cp.int$r_dob == "9999-09-09"), NA)
# 
# cp.int$ga_at_del <- 40-((cp.int$r_dob - cp.ant$i_deliverydate_est)/7)
# 
# # function to determine SGA
# sga.fun <- function(dat){
#   sga <- c()
#   for(i in 1:dim(dat)[1]){
#     
#     if(is.na(dat$s_bw_gm[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$s_bw_gm[i]) & is.na(dat$s_pe_genitalia[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$ga_at_del[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(dat$ga_at_del[i] < 22 | dat$ga_at_del[i] > 43){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$s_pe_genitalia[i])){
#       w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#     
#     if(dat$s_pe_genitalia[i] == 2){
#       w.thresh <- SGA$weight.F[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#     
#     if(dat$s_pe_genitalia[i] == 1){
#       w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#   }
#   return(sga)
# }
# 
# cp.int$SGA <- sga.fun(cp.int)


cp.cov.ant <- left_join(cp.cov, cp.ant, by = "a_record_id")
cp.cov.ant.dem <- left_join(cp.cov.ant, cp.dem, by = "a_record_id")
cp.cov.ant.dem.int <- left_join(cp.cov.ant.dem, cp.int, by = "a_record_id")
cp.cov.ant.dem.int.oth <- left_join(cp.cov.ant.dem.int, cp.oth, by = "a_record_id")
cp.cov.ant.dem.int.oth.pos <- left_join(cp.cov.ant.dem.int.oth, cp.pos, by = "a_record_id")

##################### WRITE BC DATA ######################
# replace the filepath, to one appropriate for your project
write_csv(cp.cov.ant.dem.int.oth.pos, file = here("COVID_PREG_cleaned_BC_2023-02-26.csv"))



###### REMAINING PROVINCES ########

## currently the different instruments are in different rows for the same person
cp <- other_data[which(other_data$redcap_data_access_group != "bc"), ]
# describeFactors(data$a_site)
# levels(factor(cp$redcap_event_name))

cp.cov <- cp[which(cp$redcap_event_name == "sarscov2_arm_1"), ]
cp.dem <- cp[which(cp$redcap_event_name == "demographics_arm_1"), ]
cp.ant <- cp[which(cp$redcap_event_name == "antepartum_arm_1"), ]
cp.int <- cp[which(cp$redcap_event_name == "intrapartum_arm_1"), ]
cp.pos <- cp[which(cp$redcap_event_name == "postpartum_arm_1"), ]
cp.oth <- cp[which(cp$redcap_event_name == "other_forms_arm_1"), ]

### need to include "other" form

# clean out any empty columns
cp.cov <- remove_empty(dat = cp.cov, quiet = TRUE)
cp.dem <- remove_empty(dat = cp.dem, quiet = TRUE)
cp.ant <- remove_empty(dat = cp.ant, quiet = TRUE)
cp.int <- remove_empty(dat = cp.int, quiet = TRUE)
cp.pos <- remove_empty(dat = cp.pos, quiet = TRUE)
cp.oth <- remove_empty(dat = cp.oth, quiet = TRUE)

# 
## but first check for stillbirths - none
# describeFactors(cp.ant$o_loss)
describeFactors(cp.ant$o_sb)
cp.ant$o_date[which(cp.ant$o_sb == 1)]
cp.ant$o_date[which(cp.ant$o_spont == 1)]
cc <- cp.ant$a_record_id[which(cp.ant$o_sb == 1 | cp.ant$o_spont == 1)] # 
cp.int$r_dob[which(cp.int$a_record_id %in% cc)]
cc2 <- cp.int$a_record_id[which(cp.int$a_record_id %in% cc)]
cp.int$r_dob[which(cp.int$a_record_id %in% cc)] <- cp.ant$o_date[which(cp.ant$a_record_id %in% cc2)]
# 
cp.ant$a_record_id[which(cp.ant$o_sb == 1)]
# 
cp.cov$e_diagnosis[which(is.na(cp.cov$e_diagnosis))] <- cp.cov$d_naso1_collect[which(is.na(cp.cov$e_diagnosis))]



## ethnicity:
# 1 = White/Caucasian, 2 = African/Caribbean/Black, 3 = Hispanic/Latino, 4 = East Asian, 5 = South Asian, 6 = South East Asian, 7 = Middle East, 8 = Indigenous, 998 = Other, 999 = Unknown
# missing data are not reliably coded here. Has to be inferred from 999 and if all entries for all possible ethnicities is 0 (no option checked).

describeFactors(cp.dem$b_ethnicity___1) # White/Caucasian
describeFactors(cp.dem$b_ethnicity___999)

cp.dem <- cp.dem %>% 
  rowwise() %>% 
  mutate(eth = case_when(
    all(c(b_ethnicity___1, b_ethnicity___2, b_ethnicity___3, b_ethnicity___4, b_ethnicity___5, b_ethnicity___6, b_ethnicity___7, b_ethnicity___8, b_ethnicity___998, b_ethnicity___999) == 0) ~ "Missing",
    b_ethnicity___999 == 1 ~ "Unknown",
    b_ethnicity___998 == 1 ~ "Other",
    b_ethnicity___1 == 1 ~ "White",
    b_ethnicity___2 == 1 ~ "African/Carribean/Black",
    b_ethnicity___3 == 1 ~ "Hispanic/Latino",
    b_ethnicity___4 == 1 ~ "East Asian",
    b_ethnicity___5 == 1 ~ "South Asian",
    b_ethnicity___6 == 1 ~ "South East Asian",
    b_ethnicity___7 == 1 ~ "Middle East",
    b_ethnicity___8 == 1 ~ "Indigenous"
  ))

describeFactors(cp.dem$eth)
cp.dem$eth <- factor(cp.dem$eth)
# change the missing and unknown to NA
levels(cp.dem$eth)[which(levels(cp.dem$eth) == "Missing" | levels(cp.dem$eth) == "Unknown")] <- NA

# reduce the number of categories
cp.dem$eth_cat <- cp.dem$eth
levels(cp.dem$eth_cat)[which(levels(cp.dem$eth) == "East Asian" | levels(cp.dem$eth) == "South East Asian")] <- "East or SE Asian"
cp.dem$eth_cat <- relevel(cp.dem$eth_cat, ref = "White")
describeFactors(cp.dem$eth_cat)

## AGE
summary(cp.dem$b_age) # check that Missing are coded as NA and not as 999 or something like that. Should be NA
cp.dem$b_age <- ifelse(cp.dem$b_age == 999, NA, cp.dem$b_age)

## Employment
describeFactors(cp.dem$b_employment)
cp.dem$employ <- case_when(
  cp.dem$b_employment == 1 ~ "Employed",
  cp.dem$b_employment == 2 ~ "Unemployed"
)
describeFactors(cp.dem$employ)


### Education
# number of years of school - missing coded as 999 or 888 - recode as NA
summary(cp.dem$b_school)
cp.dem$education <- replace(cp.dem$b_school, which(cp.dem$b_school == 999 | cp.dem$b_school == 888), NA)
summary(cp.dem$education)
# nearly all missing - likely won't use this variable

### symptom onset date - many missing
summary(cp.cov$e_onset)

### testing date - more complete
summary(cp.cov$e_diagnosis)
summary(cp.cov$d_naso1_collect)# check that these dates make sense. Fix or exclude ones that are errors

# cp.cov$e_diagnosis[which(cp.cov$e_diagnosis == "2021-11-09")] <- "2021-02-11"

### calculate gestational age at testing
# EDD is only available on the cp.ant data set, therefore, some of the people tested will be missing GA at testing/diagnosis
cp.ant$i_deliverydate_est # EDD
cp.ant$i_deliverydate_est <- as.Date(as.character(cp.ant$i_deliverydate_est), format = "%Y-%m-%d")

# fix
# cp.ant$i_deliverydate_est[which(cp.ant$a_record_id == "CCP-01-600")] <- as.Date("2021-05-09")

cp.cov$d_naso1_collect
cp.ant <- merge(cp.ant, cp.cov[, c("a_record_id", "e_diagnosis")], by = "a_record_id")

cp.ant$e_diagnosis <- as.Date(cp.ant$e_diagnosis)
# replace 9999-09-09 with NA
cp.ant$i_deliverydate_est <- replace(cp.ant$i_deliverydate_est, which(cp.ant$i_deliverydate_est == "9999-09-09"), NA)


# split maternal age and GA at diagnosis into categories
cp.dem$age_cat <- factor(case_when(
  cp.dem$b_age <25 ~ "<25 years",
  cp.dem$b_age <30 ~ "25-29 years",
  cp.dem$b_age <36 ~ "30-35 years",
  cp.dem$b_age <40 ~ "36-39 years",
  cp.dem$b_age >=40 ~ "≥40 years"
),
levels = c("<25 years", "25-29 years", "30-35 years", "36-39 years", "≥40 years"))

describeFactors(cp.dem$age_cat)


## maternal BMI
cp.ant$i_weight <- replace(cp.ant$i_weight, which(cp.ant$i_weight == 999 | cp.ant$i_weight == 666), NA)
cp.ant$i_height <- replace(cp.ant$i_height, which(cp.ant$i_height == 999 | cp.ant$i_height == 666), NA)

cp.ant$BMI <- cp.ant$i_weight/(cp.ant$i_height/100)^2
describeMedian(cp.ant$BMI, iqr = FALSE) # check for out of range values - and remove or correct

# BMI >= 30 variable
cp.ant$BMI_cat <- factor(case_when(
  cp.ant$BMI < 18.5 ~ "<18.5",
  cp.ant$BMI < 25 ~ "18.5-24",
  cp.ant$BMI < 30 ~ "25-29",
  cp.ant$BMI >= 30 ~ "≥30"
))
describeFactors(cp.ant$BMI_cat)
cp.ant$BMI_cat <- factor(cp.ant$BMI_cat, levels = c("<18.5", "18.5-24", "25-29", "≥30"))

### acquisition
cp.cov$c_travel___1
cp.cov$c_contact___1
cp.cov$c_hcw___1
cp.cov$c_other___1
cp.cov$c_unknown___1

cp.cov <- cp.cov %>% 
  rowwise() %>% 
  mutate(travel = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_travel___1 == 1 ~ "Yes",
    c_travel___1 == 0 ~ "No"
  ),
  contact = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_contact___1 == 1 ~ "Yes",
    c_contact___1 == 0 ~ "No"
  ),
  hcw = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_hcw___1 == 1 ~ "Yes",
    c_hcw___1 == 0 ~ "No"
  ),
  other_aq = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_other___1 == 1 ~ "Yes",
    c_other___1 == 0 ~ "No"
  ),
  unknown_aq = case_when(
    all(c(c_travel___1, c_contact___1, c_hcw___1, c_unknown___1, c_other___1) == 0) ~ "No entry",
    c_unknown___1 == 1 ~ "Yes",
    c_unknown___1 == 0 ~ "No"
  ))

describeFactors(cp.cov$travel)
describeFactors(cp.cov$contact)
describeFactors(cp.cov$hcw)
describeFactors(cp.cov$other_aq)
describeFactors(cp.cov$unknown_aq)

# replace the No entry with NA
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(travel, contact, hcw, other_aq, unknown_aq), ~ifelse(.x == "No entry", NA, .x)))

### symptoms
cp.cov <- cp.cov %>% 
  mutate(asym = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_asymptomatic == 1 ~ "Yes",
    e_asymptomatic == 0 ~ "No"
  ),
  fever = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_fever == 1 ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("chill|sweat|Frissons|Chiils", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_fever == 0 ~ "No"
  ),
  cough = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_cough == 1 ~ "Yes",
    e_cough == 0 ~ "No"
  ),
  head = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_head == 1 ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("tender frontal|head", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_head == 0 ~ "No"
  ),
  breath = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_breath == 1 ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("chest congestion|breathing|wheez|orthpnoea|asthma", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_breath == 0 ~ "No"
  ),
  runny = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_runny == 1 ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec, ignore.case = TRUE) & !grepl("chest", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec_2, ignore.case = TRUE) & !grepl("chest", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("nasal|nose|congestion|rhin|sinus|URI", e_othersx_spec_3, ignore.case = TRUE) & !grepl("chest", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_runny == 0 ~ "No"
  ),
  weakness = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("weakness", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("weakness", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    e_othersx == 0 ~ "No"
  ),
  muscle = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_muscle == 1 ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_muscle == 0 ~ "No"
  ),
  anorexia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_anorexia == 1 ~ "Yes",
    e_anorexia == 0 ~ "No"
  ),
  diarrhea = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_diarrhea == 1 ~ "Yes",
    e_diarrhea == 0 ~ "No"
  ),
  vomit = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_vomit == 1 ~ "Yes",
    e_vomit == 0 ~ "No"
  ),
  fatigue = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_fatigue == 1 ~ "Yes",
    e_fatigue == 0 ~ "No"
  ),
  anosmia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_anosmia == 1 ~ "Yes",
    e_anosmia == 0 ~ "No"
  ),
  throat = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_throat == 1 ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("odynophagia|gorge|Laryngitis|throat", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    e_throat == 0 ~ "No"
  ),
  sputum = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_sputum == 1 ~ "Yes",
    e_sputum == 0 ~ "No"
  ),
  malaise = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    e_malaise == 1 ~ "Yes",
    e_malaise == 0 ~ "No"
  ),
  chest_pain = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    TRUE ~ "No"
  ),
  tachycardia = case_when(
    all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec, ignore.case = TRUE) ~ "Yes",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
    grepl("Tachycardia|Increased heart rate|heart rate", e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
    TRUE ~ "No"
  ),
  # other = case_when(
  #   all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))) ~ "Miss",
  #   !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec, ignore.case = TRUE) & !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec_2, ignore.case = TRUE) & !grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|nasal|nose|congestion|rhin|sinus|URI|chest congestion|breathing|wheez|tender frontal|chill|sweat|weakness|chest", e_othersx_spec_3, ignore.case = TRUE) & e_othersx == 1 ~ "Yes",
  #   e_othersx == 0 ~ "No"
  # )
  )

# replace the No entry with NA
cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(asym, fever, cough, head, breath, runny, muscle, anorexia, diarrhea, vomit, fatigue, anosmia, throat, sputum, malaise, chest_pain, tachycardia, weakness), ~ifelse(.x == "Miss", NA, .x)))
describeFactors(cp.cov$fever)
describeFactors(cp.cov$chest_pain)


# given these are major outcomes, we will conservatively assume that NA = no event rather than missing data
# maternal death
cp.cov$e_death

cp.cov <- cp.cov %>% 
  mutate(across(.cols = c(e_pneumonia, e_sepsis, e_respfail, e_respdist, e_heartfail, e_shock, e_coag, e_renal, e_liver, e_dic, e_hosp, g_icu, e_death, e_ecmo___1), ~case_when(.x == 1 ~ 1, TRUE ~ 0)))

# pneumonia or abnormal x-ray
cp.cov$abx_pne <- case_when(
  cp.cov$e_xray_result == 1 | cp.cov$e_pneumonia == 1 ~ 1,
  TRUE ~ 0)

# composite outcome
cp.cov$comp.pne_o2 <- case_when(
  cp.cov$abx_pne == 1 | cp.cov$e_oxygen___1 == 1 ~ 1,
  TRUE ~ 0
)

cp.cov$comp.icu_vent_ecmo <- case_when(
  cp.cov$g_icu == 1 | cp.cov$e_inv___1 == 1 | cp.cov$e_ecmo___1 == 1 ~ 1,
  TRUE ~ 0
)


### embolism
cp.cov$e_dvt
cp.cov$e_pe

any(grepl("pul|PE|DVT", cp.cov$e_compoth_specify))
# View(cp.cov[which(grepl("PE|DVT|embolus|embolism", cp.cov$e_comments)), ])

describeFactors(cp.cov$e_compoth_specify)
describeFactors(cp.cov$e_othersx_spec)
describeFactors(cp.cov$e_othersx_spec_2)
describeFactors(cp.cov$e_othersx_spec_3)
describeFactors(cp.cov$e_comments)

cp.cov <- cp.cov %>% 
  mutate(
    thrombo_details1 = case_when(
      grepl("DVT|embolus|embolism", e_compoth_specify, ignore.case = TRUE) ~ e_compoth_specify,
      grepl("DVT|embolus|embolism", e_othersx_spec, ignore.case = TRUE) ~ e_othersx_spec,
      grepl("PE", e_compoth_specify, ignore.case = F) ~ e_compoth_specify,
      grepl("PE", e_othersx_spec, ignore.case = F) ~ e_othersx_spec
    ),
    thrombo_details2 = case_when(
      # grepl("DVT|embolus|embolism", e_compoth_specify_2, ignore.case = TRUE) ~ e_compoth_specify_2,
      grepl("DVT|embolus|embolism", e_othersx_spec_2, ignore.case = TRUE) ~ e_othersx_spec_2,
      # grepl("PE", e_compoth_specify_2, ignore.case = F) ~ e_compoth_specify_2,
      grepl("PE", e_othersx_spec_2, ignore.case = F) ~ e_othersx_spec_2
    )
  )

describeFactors(cp.cov$thrombo_details1)
describeFactors(cp.cov$thrombo_details2)

cp.cov <- cp.cov %>% 
  mutate(
    dvt = case_when(
      grepl("DVT", thrombo_details1, ignore.case = TRUE) | grepl("DVT", thrombo_details2, ignore.case = TRUE) ~ "DVT",
      is.na(thrombo_details1) ~ "No",
      TRUE ~ "No"
    ),
    pul_embo = case_when(
      grepl("ruled out", thrombo_details1, ignore.case = TRUE) | grepl("ruled out", thrombo_details2, ignore.case = TRUE) ~ "pulmonary embolism",
      grepl("pulmonary embolism", thrombo_details1, ignore.case = TRUE) | grepl("pulmonary embolism", thrombo_details2, ignore.case = TRUE) ~ "pulmonary embolism",
      is.na(thrombo_details1) ~ "No",
      TRUE ~ "No"
    )
  )

describeFactors(cp.cov$dvt)
describeFactors(cp.cov$pul_embo)

cp.cov$e_dvt <- ifelse(cp.cov$dvt == "DVT", 1, 0)
cp.cov$e_pe <- ifelse(cp.cov$pul_embo == "pulmonary embolism", 1, 0)

### maternal comorbidities
# These are difficult because the number of missing responses is hard to determine. All variables get either a 1 or a 0, or a NA if nothing was checked. The problem is that people doing data entry will often just skip the ones that a person does not have, and just fill in the ones they do. This is not consistent, unfortunately, but speeds up data entry for them. This makes it difficult to determine if someone is missing a response because they did not have that comorbidity, or if they are missing because there was no data at all on comorbidities available. To determine an approximately correct number missing, we need to evaluate how many were either not "none" or 0 across the board indicating that they never had an entry - likely indicating no data was available. If they also have some delivery data, we can count these as No because data entry was likely complete for these participants. A 999 indicates unknown as well.

# these all may have different numbers "Missing"
describeFactors(cp.ant$j_none___1) # if this is 1, they had no comorbidities and that section was skipped
describeFactors(cp.ant$j_cns)
describeFactors(cp.ant$j_hem)
describeFactors(cp.ant$j_resp)
# ...

# determine the number missing
cp.ant <- cp.ant %>% 
  rowwise() %>% 
  mutate(cvs = case_when(
    all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
    all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
    j_none___1 == 1 ~ "No",
    j_cvs == 1 ~ "Yes",
    l_htn == 1 ~ "Yes",
    j_cvs == 0 ~ "No",
    is.na(j_cvs) ~ "No"
  ))

# 
#   cns = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_cns == 1 ~ "Yes",
#     j_cns == 0 ~ "No",
#     is.na(j_cns) ~ "No"
#   ),
#   resp = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_resp == 1 ~ "Yes",
#     j_resp == 0 ~ "No",
#     is.na(j_resp) ~ "No"
#   ),
#   eentm = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_eentm == 1 ~ "Yes",
#     j_eentm == 0 ~ "No",
#     is.na(j_eentm) ~ "No"
#   ),
#   gi = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_gi == 1 ~ "Yes",
#     j_gi == 0 ~ "No",
#     is.na(j_gi) ~ "No"
#   ),
#   gu = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_gu == 1 ~ "Yes",
#     j_gu == 0 ~ "No",
#     is.na(j_gu) ~ "No"
#   ),
#   repro = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_repro == 1 ~ "Yes",
#     j_repro == 0 ~ "No",
#     is.na(j_repro) ~ "No"
#   ),
#   endo = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_endo == 1 ~ "Yes",
#     j_endo == 0 ~ "No",
#     is.na(j_endo) ~ "No"
#   ),
#   ms = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_ms == 1 ~ "Yes",
#     j_ms == 0 ~ "No",
#     is.na(j_ms) ~ "No"
#   ),
#   hem = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_hem == 1 ~ "Yes",
#     j_hem == 0 ~ "No",
#     is.na(j_hem) ~ "No"
#   ),
#   mh = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_mh == 1 ~ "Yes",
#     j_mh == 0 ~ "No",
#     is.na(j_mh) ~ "No"
#   ),
#   aai = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_aai == 1 ~ "Yes",
#     j_aai == 0 ~ "No",
#     is.na(j_aai) ~ "No"
#   ),
#   other_comor = case_when(
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & a_record_id %in% cp.int$a_record_id ~ "No",
#     all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))) & j_none___1 == 0 ~ "No entry",
#     j_none___1 == 1 ~ "No",
#     j_oth == 1 ~ "Yes",
#     j_oth == 0 ~ "No",
#     is.na(j_oth) ~ "No"
#   )
#   )

# replace the No entry with NA
cp.ant <- cp.ant %>% 
  mutate(across(.cols = c(cvs), ~ifelse(.x == "No entry", NA, .x)))

# check to make sure
describeFactors(cp.ant$cvs)

## extract specific comorbidities of interest
# asthma
cp.ant$asthma <- case_when(
  cp.ant$j_resp_asth___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$asthma)

# Chronic obstructive lung disease
cp.ant$lung <- case_when(
  cp.ant$j_resp_lung___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$lung)

# hypertension
cp.ant$htn <- case_when(
  cp.ant$j_cvs_htn___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$htn)

# diabetes 1 or 2
cp.ant$diabetes <- case_when(
  cp.ant$j_endo_diabt1___1 == 1 | cp.ant$j_endo_diabt2___1 == 1 ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$diabetes)


# autoimmune diseases have to be gathered from a few places
# celiac, lupus, rheumatoid arthritis, Ankylosing spondylitis, others
cp.ant$j_aai_oth_s # check this variable for others eg MS

cp.ant$autoimm <- case_when(
  cp.ant$j_gi_celiac___1 == 1 | cp.ant$j_ms_lupus___1 == 1 | cp.ant$j_ms_as___1 == 1 | cp.ant$j_ms_ra___1 | grepl("Multiple sclerosis|Antiphospho|neutropenia|SLE|lupus", cp.ant$j_aai_oth_s, ignore.case = TRUE) ~ "Yes",
  is.na(cp.ant$cvs) ~ NA_character_,
  !is.na(cp.ant$cvs) ~ "No"
)
describeFactors(cp.ant$autoimm)

## composite comorbidities
# c(cvs, diabetes, htn, asthma, hbv, hcv)

cp.ant$any_comor <- case_when(
  cp.ant$cvs == "Yes" | cp.ant$asthma == "Yes" | cp.ant$diabetes == "Yes" | cp.ant$htn == "Yes" | cp.ant$k_hbv == 1 | cp.ant$k_hcv == 1 ~ "Yes",
  cp.ant$cvs == "No" & cp.ant$asthma == "No" & cp.ant$diabetes == "No" & cp.ant$htn == "No" ~ "No"
)



# maternal smoking during pregnancy
cp.ant$smoking <- case_when(
  cp.ant$i_smokingd == 999 | is.na(cp.ant$i_smokingd) ~ NA_character_,
  cp.ant$i_smokingd == 0 ~ "No",
  cp.ant$i_smokingd == 1 ~ "Yes"
)

describeFactors(cp.ant$smoking)

# HepB sAg positive
cp.ant$k_hbv
cp.ant$k_hbv <- ifelse(cp.ant$k_hbv == 999 | cp.ant$k_hbv == 888, NA, cp.ant$k_hbv)

# HepC Ab+
cp.ant$k_hcv
cp.ant$k_hcv <- ifelse(cp.ant$k_hcv == 999 | cp.ant$k_hcv == 888, NA, cp.ant$k_hcv)

# Gravida
cp.ant$gravida <- cp.ant$h_gravida
cp.ant$gravida <- factor(case_when(
  cp.ant$gravida == 0 ~ "0",
  cp.ant$gravida == 1 ~ "1",
  cp.ant$gravida >= 2 ~ "2+"
))


## losses/stillbirth losses/stillbirth CHECK STILLBIRTHS!! MAY NOT BE BEING CAPTURED BY R_DOB
cp.int$p_outcome <- case_when(
  cp.int$p_outcome == 1 ~ "Loss",
  cp.int$p_outcome == 2 ~ "Stillbirth",
  cp.int$p_outcome == 3 ~ "Livebirth"
)


# mode of delivery
# Vag/CS
cp.int$mode_del <- case_when(
  cp.int$p_mode == 1 ~ "Vaginal",
  cp.int$p_mode == 2 ~ "CS"
)

# 5 minute apgar
cp.int$apgar5 <- factor(ifelse(cp.int$s_apgar_5 < 7, "<7", "≥7"))

# birth weight
cp.int$bw_cat <- case_when(
  cp.int$s_bw_gm < 2500 ~ "<2500",
  cp.int$s_bw_gm >= 2500 & cp.int$s_bw_gm <=4000 ~ "2500-4000",
  cp.int$s_bw_gm > 4000 ~ ">4000"
)

# NICU admission
# assuming if we have birth weight then we should know if baby admitted to NICU or not
cp.int$NICU <- case_when(
  cp.int$t_nicu == 1 ~ "Yes",
  cp.int$t_nicu == 0 ~ "No",
  is.na(cp.int$s_bw_gm) == FALSE ~ "No")

##### TODO: FIX BELOW ####### duration of admission
cp.int$nicu_dur <- (!is.na(cp.int$t_nicu_discharge)) - (!is.na(cp.int$t_nicu_admission))
max(cp.int$nicu_dur)

# time between delivery and diagnosis
cp.int <- left_join(cp.int, cp.cov[, c("a_record_id", "d_naso1_collect")])

cp.ant$ga_at_diag <- as.numeric((280 - (as.Date(cp.ant$i_deliverydate_est) - as.Date(cp.cov$d_naso1_collect)))/7) # in weeks
# describeMedian(cp.ant$ga_at_diag, iqr = FALSE) # check the range and fix any that are not possible - or remove. <0 and >43 weeks
cp.ant$ga_at_diag <- replace(cp.ant$ga_at_diag, which(cp.ant$ga_at_diag >43 | cp.ant$ga_at_diag < 0), NA)

cp.ant$ga_dx_cat <- factor(case_when(
  cp.ant$ga_at_diag <= 14 ~ "<=14 weeks",
  cp.ant$ga_at_diag < 27 ~ "14-27 weeks",
  cp.ant$ga_at_diag < 38 ~ "28-38 weeks",
  cp.ant$ga_at_diag >= 38 ~ "≥38 weeks"
), levels = c("<=14 weeks", "14-27 weeks", "28-38 weeks", "≥38 weeks"))


# # make weight vectors
# ### From: A New and Improved Population-Based Canadian Reference for Birth Weight for Gestational Age, Michael S. Kramer, Robert W. Platt, Shi Wu Wen, K.S. Joseph, Alexander Allen, Michal Abrahamowicz, Béatrice Blondel, Gérard Bréart and for the Fetal/Infant Health Study Group of the Canadian Perinatal Surveillance System Pediatrics 2001;108;e35 DOI: 10.1542/peds.108.2.e35
# 
# ga.F <- seq(22, 43)
# weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above
# 
# weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above
# 
# # if unknown go with male to be conservative
# SGA <- data.frame(ga.F, weight.F, weight.M)
# 
# # function to determine SGA
# sga.fun <- function(dat){
#   sga <- c()
#   for(i in 1:dim(dat)[1]){
#     
#     if(is.na(dat$s_bw_gm[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$s_bw_gm[i]) & is.na(dat$s_pe_genitalia[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$ga_at_del[i])){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(dat$ga_at_del[i] < 22 | dat$ga_at_del[i] > 43){
#       sga <- c(sga, NA)
#       next
#     }
#     
#     if(is.na(dat$s_pe_genitalia[i])){
#       w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#     
#     if(dat$s_pe_genitalia[i] == 2){
#       w.thresh <- SGA$weight.F[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#     
#     if(dat$s_pe_genitalia[i] == 1){
#       w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
#       sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
#       next
#     }
#   }
#   return(sga)
# }
# 
# cp.int$SGA <- sga.fun(cp.int)

# combine cov and ant
cp.cov.ant <- left_join(cp.cov, cp.ant, by = "a_record_id")
cp.cov.ant.dem <- left_join(cp.cov.ant, cp.dem, by = "a_record_id")
cp.cov.ant.dem.int <- left_join(cp.cov.ant.dem, cp.int, by = "a_record_id")
cp.cov.ant.dem.int.oth <- left_join(cp.cov.ant.dem.int, cp.oth, by = "a_record_id")

#################### WRITE MB NB PE YT ####################
# replace the filepath, to one appropriate for your project
write_csv(cp.cov.ant.dem.int.oth, file = here("COVID_PREG_cleaned_MB_NB_PE_2023-02-26.csv"))

write_csv(cp.ant, file = here("antenatal_test.csv"))





#### CLEAN NEWFOUNDLAND ####


NL_ante <- read.csv("NLdata/NL_antepartum.csv", header = TRUE, na.strings = c("", NA, 999))

NL <- read.csv("NLdata/NL CAN COVID DATA RELEASE FILE.csv", header = TRUE, na.strings = c("", NA, 999))

nl_clean <- remove_empty(dat = NL, quiet = TRUE)

# make all No Entry's and NA

# delivery date time separation
nl_clean <- nl_clean %>% 
  rowwise() %>% 
  # TODO : fix
  mutate(r_dob = as.Date(Delivery.DateTime), r_tob = as.Date(Delivery.DateTime))

#Breastfeeding initiation Y/N 1/0
nl_clean <- nl_clean %>% 
  rowwise() %>% 
  # TODO : fix
  mutate()


write.csv(nl_clean, here("NL_CLEANED_2024-01-21.csv"))


## Clean Nova Scotia data ##
NS <- read.csv("2024-JAN-18_RAW_COVID19InPreg_DATA_2024-01-18_1559.csv", header = TRUE, na.strings = c("", NA, 999))

# clean out any empty columns
ns_clean <- remove_empty(dat = NS, quiet = TRUE)

write.csv(ns_clean, here("NS_CLEANED_2024-01-21.csv"))
