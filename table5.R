##  outcomes Gal Av-Gay June 14 2023

library(Gmisc)
library(broman)
library(dplyr)
library(DescTools)
library(tidyverse)
library(sjPlot)
library(here)
library(janitor)
library(zoo)
library(lubridate)
library(haven)
library(ghibli)
library(ggpubr)

#### LOAD ALL DATA ####

full_data_sansON <- read.csv("full_data_sansON_06_13_2023.csv", header = TRUE)
data_on <- read.csv("ONdata/data_on_06_13_2023.csv", header = TRUE)

colnames(data_on)[which(colnames(data_on) == "C_maternal_age_group")] <- "age_cat2"      
colnames(data_on)[which(colnames(data_on) == "C_COVID_diagnosis_date")] <- "e_diagnosis"      
colnames(data_on)[which(colnames(data_on) == "SGA10")] <- "sga2"      

# make sure all ICU also get counted as hospitalized here to start

full_data_sansON$hospitalization 
#[which(full_data_sansON$icu == 1)] 
#<- 1
data_on$hospitalization[which(data_on$icu == 1)] <- 1


#### Go through the main variables (aggregators) and then individual data points to fill in Joseph Tings tables
## aggregators: 
# time period (omicron vs pre)
# No NICU admission vs Level II-III NICU
# unvaccinated, 1 dose, 2+ doses
# maternal hospitalizatoin vs no need for maternal hospitalization


# 1. time period
## omicron start December 19 2021 
## Delta started April 4th 2021
## dates from https://nccid.ca/covid-19-variants/
full_data_sansON$covid_period = case_when(
  full_data_sansON$e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
  full_data_sansON$e_diagnosis >= as.Date("2021-04-04") & full_data_sansON$e_diagnosis < as.Date("2021-12-19") ~ "Delta",
  full_data_sansON$e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
)

## omicron start December 19 2021 
## Delta started April 4th 2021
## dates from https://nccid.ca/covid-19-variants/
data_on$covid_period = case_when(
  data_on$e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
  data_on$e_diagnosis >= as.Date("2021-04-04") & data_on$e_diagnosis < as.Date("2021-12-19") ~ "Delta",
  data_on$e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
)


## NICU admission - not sure if we can get Level II and III only ?? Not sure how it works
summary(as.factor(data_on$NICU))
summary(as.factor(full_data_sansON$NICU))

##Vaccination status
summary(as.factor(data_on$number_of_vaccines))
summary(as.factor(full_data_sansON$number_of_vaccines))

## hospitalization
summary(as.factor(data_on$hospitalization))
data_on$hospitalization[is.na(data_on$hospitalization)] <- 0

summary(as.factor(full_data_sansON$hospitalization))

#### Now do individual data points for each table
#### table 1:
# Proportion of preterm delivery (<37 weeks GA)
# Maternal age
# Ethnicity 
# Hypertensive disorder
# Gestational diabetes
# Vaccine status:
#   -	Unvaccinated
# -	1 dose only
# -	2 doses
# -	Unknown
# Time of diagnosis
# -	1st trimester
# -	2nd trimester
# -	3rd trimester
# -	Unknown
# Maternal hospital admission
# Maternal ICU admission
# Maternal need for invasive mechanical ventilation

# proportion of pre-term delivery
summary(as.factor(data_on$ga_del_cat2))
summary(as.factor(full_data_sansON$ga_del_cat2))

# maternal age - will have to use age category because numerical age not available from ontario
summary(as.factor(data_on$age_cat2))
summary(as.factor(full_data_sansON$age_cat2))

# ethnicity
summary(as.factor(data_on$eth_cat))
summary(as.factor(full_data_sansON$eth_cat))

#hypertensive disorder?? double check this
summary(as.factor(data_on$htn))
summary(as.factor(full_data_sansON$htn))

#gestational diabetes

data_on$gestational_diabetes <- case_when(
  data_on$diabetes_final == 2 ~ 1,
  data_on$diabetes_final == 0 ~ 0,
  data_on$diabetes_final == 1 ~ 0
)

full_data_sansON$gestational_diabetes <- case_when(
  full_data_sansON$l_diab == 1 ~ 1,
  full_data_sansON$l_diab == 0 ~ 0,
  full_data_sansON$l_diab == 999 ~ NA
)

summary(as.factor(data_on$gestational_diabetes))

summary(as.factor(full_data_sansON$gestational_diabetes))

## number of vaccines is done

# Maternal hospital admission
summary(as.factor(data_on$hospitalization))
summary(as.factor(full_data_sansON$hospitalization))

# Maternal ICU admission
summary(as.factor(data_on$icu))
summary(as.factor(full_data_sansON$icu))

# Maternal need for invasive mechanical ventilation
describeFactors(data_on$C_mech_ventilation)
describeFactors(full_data_sansON$e_inv)
describeFactors(full_data_sansON$e_inv___1)

describeFactors(full_data_sansON$e_noninv)

full_data_sansON %>% group_by(prov) %>% summarise(inv = sum(e_inv == 1, na.rm = TRUE))
## turns out a value of 2 is what Quebec used to indicate "unknown"
full_data_sansON %>% group_by(prov) %>% summarise(inv = sum(e_inv == 2, na.rm = TRUE))
full_data_sansON %>% group_by(prov) %>% summarise(inv = sum(e_inv == 1, na.rm = TRUE))
full_data_sansON %>% group_by(prov) %>% summarise(inv = sum(e_inv___1 == 1, na.rm = TRUE))


describeFactors(full_data_sansON$e_noninv)

full_data_sansON$inv_mech_vent <- case_when(
  full_data_sansON$e_inv == 1 ~ 1,
  full_data_sansON$e_inv___1 == 1 ~ 1
)
full_data_sansON$inv_mech_vent[which(is.na(full_data_sansON$inv_mech_vent))] <- 0
describeFactors(full_data_sansON$inv_mech_vent)

data_on$inv_mech_vent <- case_when(
  data_on$C_mech_ventilation == "Yes - ECMO" ~ 1,
  data_on$C_mech_ventilation == "Yes - Invasive Mechanical Ventilation" ~ 1
)

data_on$inv_mech_vent[which(is.na(data_on$inv_mech_vent))] <- 0
describeFactors(data_on$inv_mech_vent)


# Time of diagnosis
# -	1st trimester
# -	2nd trimester
# -	3rd trimester
# - unknown

## going to have to calculate this one manually using e_diagnosis and gestational age at delivery
full_data_sansON$ga_at_del
summary(as.factor(data_on$B_GA_birth))

summary(as.factor(data_on$ga_del_cat2))


#### OK seems that we will have difficulty with "time of diagnosis"
# next table, should be doable

# Gestational age
# Birth weight
# Sex
# SGA
# Maternal Hypertensive disorder
# Maternal Gestational diabetes
# Maternal COVID diagnosis during pregnancy
# Vaccine status of mother:
#   -	Unvaccinated
# -	1 dose only
# -	2 doses
# -	Unknown
# Time of diagnosis of mother
# -	1st trimester
# -	2nd trimester
# -	3rd trimester
# -	Unknown
# Cesearean birth
# Apgar Score < 5 
# Infants tested positive for SARS COV-2
# 
# 

stats_can <- read.csv("COVID19-eng.csv",header = TRUE)
stats_can$COV_EY

stats_can %>% filter(COV_AGR %in% c(2,3,4)) %>% filter(COV_GDR %in% c(2))  %>% filter(COV_EY %in% c(20,21,22)) %>% summarise(n = n(), hosp = sum(COV_HSP %in% c(1,2)), hosp_prop =  sum(COV_HSP %in% c(1,2))/n(),icu = sum(COV_HSP %in% c(1), na.rm = TRUE), icu_prop = sum(COV_HSP %in% c(1), na.rm = TRUE)/n(),icu_prop_hosp = sum(COV_HSP %in% c(1), na.rm = TRUE)/sum(COV_HSP %in% c(1,2)))


partial_data_ON <- data_on[,c("age_cat2","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","e_diagnosis","prov","ga_del_cat","ga_del_cat2","gravida","oxygen","mode_del","preg_outcome","multiple_preg","history_preterm","labour_type","apgar5","bw_cat","number_of_vaccines","sga2")]
# colnames(partial_data_ON)[which(colnames(partial_data_ON) == "C_maternal_age_group")] <- "age_cat2"      
# colnames(partial_data_ON)[which(colnames(partial_data_ON) == "C_COVID_diagnosis_date")] <- "e_diagnosis"      
# colnames(partial_data_ON)[which(colnames(partial_data_ON) == "SGA10")] <- "sga2"      

partial_data_sansON_1 <- full_data_sansON[,c("age_cat2","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","e_diagnosis","prov","ga_del_cat","ga_del_cat2","gravida","oxygen","mode_del","preg_outcome","multiple_preg","history_preterm","labour_type","apgar5","bw_cat","number_of_vaccines","sga2")] 


partial_data_ON$e_diagnosis <- as.Date(partial_data_ON$e_diagnosis)
partial_data_sansON_1$e_diagnosis <- as.Date(partial_data_sansON_1$e_diagnosis)
# data_on$YEAR <- format(as.Date(data_on$C_COVID_diagnosis_date),"%Y")

# data_on %>% group_by(YEAR) %>% summarise(n=n())

combined_data <- rbind(partial_data_sansON_1,partial_data_ON) 

## replace NA with 0
combined_data$hospitalization[is.na(combined_data$hospitalization)] <- 0

combined_data$covid_period = case_when(
  combined_data$e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
  combined_data$e_diagnosis >= as.Date("2021-04-04") & combined_data$e_diagnosis < as.Date("2021-12-19") ~ "Delta",
  combined_data$e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
)

# combined_data$e_diagnosis_cat = case_when(
#   combined_data$e_diagnosis < 
# )
# combined_data <- combined_data %>% filter(e_diagnosis < as.Date("2022-01-01"))

combined_data  %>%  summarise(n = n(), hosp = sum(hospitalization == 1, na.rm = TRUE), hosp_prop =  sum(hospitalization == 1, na.rm = TRUE)/n(),icu2 = sum(icu == 1, na.rm = TRUE), icu_prop = sum(icu == 1, na.rm = TRUE)/n(),icu_prop_hosp = sum(icu == 1, na.rm = TRUE)/sum(hospitalization == 1, na.rm = TRUE))
stats_can %>% filter(COV_AGR %in% c(2,3,4)) %>% filter(COV_GDR %in% c(2))  %>% filter(COV_EY %in% c(20,21,22)) %>% summarise(n = n(), hosp = sum(COV_HSP %in% c(1,2)), hosp_prop =  sum(COV_HSP %in% c(1,2))/n(),icu = sum(COV_HSP %in% c(1), na.rm = TRUE), icu_prop = sum(COV_HSP %in% c(1), na.rm = TRUE)/n(),icu_prop_hosp = sum(COV_HSP %in% c(1), na.rm = TRUE)/sum(COV_HSP %in% c(1,2)))


# install.packages("epitools")
library(DescTools)

risk_ratio_table <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(risk_ratio_table) <- c("pregnant","non-pregnant")
rownames(risk_ratio_table) <- c("hospitalized","non-hospitalized")

risk_ratio_table[2,2] <- 614243 - 10688
risk_ratio_table[2,1] <- 12535 - 698 
risk_ratio_table[1,2] <- 10688
risk_ratio_table[1,1] <- 698 

RelRisk(t(risk_ratio_table), conf.level=0.95, method="wald")





risk_ratio_table2 <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(risk_ratio_table2) <- c("pregnant","non-pregnant")
rownames(risk_ratio_table2) <- c("icu","non-icu")

risk_ratio_table2[2,2] <- 614243 - 1392
risk_ratio_table2[2,1] <- 12535 - 183 
risk_ratio_table2[1,2] <- 1392
risk_ratio_table2[1,1] <- 183 

RelRisk(t(risk_ratio_table2), conf.level=0.95, method="wald")




risk_ratio_table2 <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(risk_ratio_table2) <- c("pregnant","non-pregnant")
rownames(risk_ratio_table2) <- c("icu","non-icu")

risk_ratio_table2[2,2] <- 10688 - 1392
risk_ratio_table2[2,1] <- 698 - 183 
risk_ratio_table2[1,2] <- 1392
risk_ratio_table2[1,1] <- 183 

RelRisk(t(risk_ratio_table2), conf.level=0.95, method="wald")

###### AND ONCE MORE FOR full data #####

risk_ratio_table <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(risk_ratio_table) <- c("pregnant","non-pregnant")
rownames(risk_ratio_table) <- c("hospitalized","non-hospitalized")

risk_ratio_table[2,2] <- 1149539 - 20211
risk_ratio_table[2,1] <- 28056 - 1001 
risk_ratio_table[1,2] <- 20211
risk_ratio_table[1,1] <- 1001 

RelRisk(t(risk_ratio_table), conf.level=0.95, method="wald")





risk_ratio_table2 <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(risk_ratio_table2) <- c("pregnant","non-pregnant")
rownames(risk_ratio_table2) <- c("icu","non-icu")

risk_ratio_table2[2,2] <- 1149539 - 1998
risk_ratio_table2[2,1] <- 28056 - 203 
risk_ratio_table2[1,2] <- 1998
risk_ratio_table2[1,1] <- 203 

RelRisk(t(risk_ratio_table2), conf.level=0.95, method="wald")




risk_ratio_table2 <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(risk_ratio_table2) <- c("pregnant","non-pregnant")
rownames(risk_ratio_table2) <- c("icu","non-icu")

risk_ratio_table2[2,2] <- 20211 - 1998
risk_ratio_table2[2,1] <- 1001 - 203 
risk_ratio_table2[1,2] <- 1998
risk_ratio_table2[1,1] <- 203 

RelRisk(t(risk_ratio_table2), conf.level=0.95, method="wald")


full_data_sansON$winni_period = case_when(
  full_data_sansON$e_diagnosis < as.Date("2021-03-31") ~ "winnie_period"
)

full_data_sansON %>% filter(prov == "BC") %>% group_by(winni_period == "winnie_period") %>% summarise(n = n(), unique_ids = length(unique(a_record_id)))

