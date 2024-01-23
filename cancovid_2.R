#### CANCOVID 2.0 ####
# Gal Av-Gay #

#setwd("/Users/mac/Desktop/CANCOVID_2.0/CANCOVID")
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
getwd()
data_bc <- read.csv("CanadianSurveillance_DATA_2024-01-23_1307.csv", header = TRUE)
data_ns <- read.csv("2024-JAN-18_RAW_COVID19InPreg_DATA_2024-01-18_1559.csv", header = TRUE)
#data_on <- read_sas("ONdata/covid_preg_infants_sep3022.sas7bdat")
data_on <- read.csv("ONdata/data_on_06_13_2023.csv", header = TRUE)
data_other <- read.csv("CanadianCOVID19InPre_DATA_2024-01-23_1258.csv", header = TRUE)
data_NL <- read.csv("NL_CLEANED_2024-01-21.csv")
data_NL_antepartum <- read.csv("NLdata/NL_antepartum.csv")

## quebec is complicated...##
qc1 <- read.csv("QCdata/COVIPREGQV4-EXPORT1DEMOAB_DATA_2024-01-16_2009.csv", header = TRUE)
qc2 <- read.csv("QCdata/COVIPREGQV4-EXPORT2aSARSCOV2CDE_DATA_2024-01-16_2010.csv", header = TRUE)
qc3 <- read.csv("QCdata/COVIPREGQV4-EXPORT2bSARSCOV2F_DATA_2024-01-16_2012.csv", header = TRUE)
qc4 <- read.csv("QCdata/COVIPREGQV4-EXPORT2c1SARSCOV2G_DATA_2024-01-16_2014.csv", header = TRUE)
qc5 <- read.csv("QCdata/COVIPREGQV4-EXPORT2c2SARSCOV2G_DATA_2024-01-16_2015.csv", header = TRUE)
qc6 <- read.csv("QCdata/COVIPREGQV4-EXPORT3aANTEHToL_DATA_2024-01-16_2015.csv", header = TRUE)
qc7 <- read.csv("QCdata/COVIPREGQV4-EXPORT3bANTEM_DATA_2024-01-16_2015.csv", header = TRUE)
qc8 <- read.csv("QCdata/COVIPREGQV4-EXPORT3cANTENO_DATA_2024-01-16_2016.csv", header = TRUE)
qc9 <- read.csv("QCdata/COVIPREGQV4-EXPORT4INTRAPToU_DATA_2024-01-16_2019.csv", header = TRUE)
qc10 <- read.csv("QCdata/COVIPREGQV4-EXPORT5POSTVToY_DATA_2024-01-16_2020.csv", header = TRUE)
qc11 <- read.csv("QCdata/COVIPREGQV4-EXPORT6OTHERZToAB_DATA_2024-01-16_2021.csv", header = TRUE)

data_qc <- left_join(qc1,qc2, by = "a_record_id")
data_qc <- left_join(data_qc,qc3, by = "a_record_id")
data_qc <- left_join(data_qc,qc4, by = "a_record_id")
data_qc <- left_join(data_qc,qc5, by = "a_record_id")
data_qc <- left_join(data_qc,qc6, by = "a_record_id")
data_qc <- left_join(data_qc,qc7, by = "a_record_id")
data_qc <- left_join(data_qc,qc8, by = "a_record_id")
data_qc <- left_join(data_qc,qc9, by = "a_record_id")
data_qc <- left_join(data_qc,qc10, by = "a_record_id")
data_qc <- left_join(data_qc,qc11, by = "a_record_id")

dim(data_qc)

## data other split up

unique(data_other$redcap_data_access_group)
data_other <- data_other[-which(data_other$redcap_data_access_group == ""),]

data_mb <- data_other %>% filter(redcap_data_access_group == "mb")
data_nb <- data_other %>% filter(redcap_data_access_group == "nb")
data_pe <- data_other %>% filter(redcap_data_access_group == "pe")
data_yt <- data_other %>% filter(redcap_data_access_group == "yt")


#### FIX DATA (combine record_ids into single row) ####

## BC 
length(unique(data_bc$a_record_id))
unique(data_bc$redcap_event_name)

data_bc[data_bc == ""] <- NA
data_bc_1 <- data_bc %>% filter(redcap_event_name == "demographics_arm_1")
data_bc_2 <- data_bc %>% filter(redcap_event_name == "sarscov2_arm_1")
data_bc_3 <- data_bc %>% filter(redcap_event_name == "antepartum_arm_1")
data_bc_4 <- data_bc %>% filter(redcap_event_name == "intrapartum_arm_1")
data_bc_5 <- data_bc %>% filter(redcap_event_name == "other_forms_arm_1")
data_bc_6 <- data_bc %>% filter(redcap_event_name == "postpartum_arm_1")

data_bc_1 <- remove_empty(data_bc_1, which = "cols")
data_bc_2 <- remove_empty(data_bc_2, which = "cols")
data_bc_3 <- remove_empty(data_bc_3, which = "cols")
data_bc_4 <- remove_empty(data_bc_4, which = "cols")
data_bc_5 <- remove_empty(data_bc_5, which = "cols")
data_bc_6 <- remove_empty(data_bc_6, which = "cols")

data_bc <- left_join(data_bc_1,data_bc_2, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_3, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_4, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_5, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_6, by = "a_record_id")

dim(data_bc)

## Nova Scotia (already combined)

length(unique(data_ns$a_record_id))
length(unique(data_ns$a_record_id))

# data_ns$r_dob

colnames(data_ns)

## Ontario (weird one)

length(unique(data_on$new_preg_id)) ## looks correct to me, pre merged

data_on$C_COVID_diagnosis_date
data_on$baby_birth_date
sum(is.na(data_on$delivery_date))

tail(data_on$baby_birth_date)


on.dat <- data_on 

## remove duplicates (twins)
data_on <-  on.dat[-which(duplicated(on.dat$new_preg_id)), ]


## qc
length(data_qc$a_record_id)
length(unique(data_qc$a_record_id)) ## already squished it seems

data_qc$r_dob
## Manitoba
length(data_mb$a_record_id)
length(unique(data_mb$a_record_id)) 

unique(data_mb$redcap_event_name)

data_mb[data_mb == ""] <- NA
data_mb_1 <- data_mb %>% filter(redcap_event_name == "demographics_arm_1")
data_mb_2 <- data_mb %>% filter(redcap_event_name == "sarscov2_arm_1")
data_mb_3 <- data_mb %>% filter(redcap_event_name == "antepartum_arm_1")
data_mb_4 <- data_mb %>% filter(redcap_event_name == "intrapartum_arm_1")
data_mb_5 <- data_mb %>% filter(redcap_event_name == "other_forms_arm_1")
data_mb_6 <- data_mb %>% filter(redcap_event_name == "postpartum_arm_1")

data_mb_1 <- remove_empty(data_mb_1, which = "cols")
data_mb_2 <- remove_empty(data_mb_2, which = "cols")
data_mb_3 <- remove_empty(data_mb_3, which = "cols")
data_mb_4 <- remove_empty(data_mb_4, which = "cols")
data_mb_5 <- remove_empty(data_mb_5, which = "cols")
data_mb_6 <- remove_empty(data_mb_6, which = "cols")

data_mb <- left_join(data_mb_1,data_mb_2, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_3, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_4, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_5, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_6, by = "a_record_id")

data_mb$r_dob


## new brunswick
length(data_nb$a_record_id)
length(unique(data_nb$a_record_id)) 

unique(data_nb$redcap_event_name)

data_nb[data_nb == ""] <- NA
data_nb_1 <- data_nb %>% filter(redcap_event_name == "demographics_arm_1")
data_nb_2 <- data_nb %>% filter(redcap_event_name == "sarscov2_arm_1")
data_nb_3 <- data_nb %>% filter(redcap_event_name == "antepartum_arm_1")
data_nb_4 <- data_nb %>% filter(redcap_event_name == "intrapartum_arm_1")
data_nb_5 <- data_nb %>% filter(redcap_event_name == "other_forms_arm_1")
data_nb_6 <- data_nb %>% filter(redcap_event_name == "postpartum_arm_1")

data_nb_1 <- remove_empty(data_nb_1, which = "cols")
data_nb_2 <- remove_empty(data_nb_2, which = "cols")
data_nb_3 <- remove_empty(data_nb_3, which = "cols")
data_nb_4 <- remove_empty(data_nb_4, which = "cols")
data_nb_5 <- remove_empty(data_nb_5, which = "cols")
data_nb_6 <- remove_empty(data_nb_6, which = "cols")

data_nb <- left_join(data_nb_1,data_nb_2, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_3, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_4, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_5, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_6, by = "a_record_id")


# data_nb$r_dob

## yukon
length(data_pe$a_record_id)
length(unique(data_pe$a_record_id)) 

unique(data_pe$redcap_event_name)

data_pe[data_pe == ""] <- NA
data_pe_1 <- data_pe %>% filter(redcap_event_name == "demographics_arm_1")
data_pe_2 <- data_pe %>% filter(redcap_event_name == "sarscov2_arm_1")
data_pe_3 <- data_pe %>% filter(redcap_event_name == "antepartum_arm_1")
data_pe_4 <- data_pe %>% filter(redcap_event_name == "intrapartum_arm_1")
data_pe_5 <- data_pe %>% filter(redcap_event_name == "other_forms_arm_1")
data_pe_6 <- data_pe %>% filter(redcap_event_name == "postpartum_arm_1")

data_pe_1 <- remove_empty(data_pe_1, which = "cols")
data_pe_2 <- remove_empty(data_pe_2, which = "cols")
data_pe_3 <- remove_empty(data_pe_3, which = "cols")
data_pe_4 <- remove_empty(data_pe_4, which = "cols")
data_pe_5 <- remove_empty(data_pe_5, which = "cols")
data_pe_6 <- remove_empty(data_pe_6, which = "cols")

data_pe <- left_join(data_pe_1,data_pe_2, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_3, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_4, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_5, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_6, by = "a_record_id")

## PEI
length(data_yt$a_record_id)
length(unique(data_yt$a_record_id)) 

unique(data_yt$redcap_event_name)

data_yt[data_yt == ""] <- NA
data_yt_1 <- data_yt %>% filter(redcap_event_name == "demographics_arm_1")
data_yt_2 <- data_yt %>% filter(redcap_event_name == "sarscov2_arm_1")
data_yt_3 <- data_yt %>% filter(redcap_event_name == "antepartum_arm_1")
data_yt_4 <- data_yt %>% filter(redcap_event_name == "intrapartum_arm_1")
data_yt_5 <- data_yt %>% filter(redcap_event_name == "other_forms_arm_1")
data_yt_6 <- data_yt %>% filter(redcap_event_name == "postpartum_arm_1")

data_yt_1 <- remove_empty(data_yt_1, which = "cols")
data_yt_2 <- remove_empty(data_yt_2, which = "cols")
data_yt_3 <- remove_empty(data_yt_3, which = "cols")
data_yt_4 <- remove_empty(data_yt_4, which = "cols")
data_yt_5 <- remove_empty(data_yt_5, which = "cols")
data_yt_6 <- remove_empty(data_yt_6, which = "cols")

data_yt <- left_join(data_yt_1,data_yt_2, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_3, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_4, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_5, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_6, by = "a_record_id")






##### NEWFOUNDLAND ####
#data_NL <- read.csv("everything_else/NL_cancovid.csv")
#data_NL_antepartum <- read.csv("everything_else/NL_antepartum.csv")

# colnames(data_NL)

length(data_NL$NL_COVIDSTUDYID)
length(unique(data_NL$NL_COVIDSTUDYID))

length(data_NL_antepartum$NL_COVIDSTUDYID)
length(unique(data_NL_antepartum$NL_COVIDSTUDYID))

## how to combine antepartum and normal data? add new columns for each antepartum hospital visit? ad hoc?

data_NL_antepartum$NL_COVIDSTUDYID[duplicated(data_NL_antepartum$NL_COVIDSTUDYID)]

## isolate only first visit, get rid of duplicates and put it in "duplicates"
data_NL_antepartum_first_visit <- data_NL_antepartum[-which(duplicated(data_NL_antepartum$NL_COVIDSTUDYID)),]

#remaining visits
data_NL_antepartum_duplicates <- data_NL_antepartum[which(duplicated(data_NL_antepartum$NL_COVIDSTUDYID)),]

## second visits
data_NL_antepartum_second_visit <- data_NL_antepartum_duplicates[-which(duplicated(data_NL_antepartum_duplicates$NL_COVIDSTUDYID)),]

#remaining duplicates
data_NL_antepartum_duplicates2 <- data_NL_antepartum_duplicates[which(duplicated(data_NL_antepartum_duplicates$NL_COVIDSTUDYID)),]

## third visits 
data_NL_antepartum_third_visit <- data_NL_antepartum_duplicates2[-which(duplicated(data_NL_antepartum_duplicates2$NL_COVIDSTUDYID)),]

#remaining duplicates (fourth visit)
data_NL_antepartum_fourth_visit <- data_NL_antepartum_duplicates2[which(duplicated(data_NL_antepartum_duplicates2$NL_COVIDSTUDYID)),]


dim(data_NL_antepartum)
dim(data_NL_antepartum_first_visit)
dim(data_NL_antepartum_duplicates)
dim(data_NL_antepartum_duplicates2)


## NOW combine all into one row per mother, rename each column based on the visit number
colnames(data_NL_antepartum_first_visit)[2:dim(data_NL_antepartum_first_visit)[2]] <- paste0(colnames(data_NL_antepartum_first_visit)[2:dim(data_NL_antepartum_first_visit)[2]],"first_visit")
colnames(data_NL_antepartum_second_visit)[2:dim(data_NL_antepartum_second_visit)[2]] <- paste0(colnames(data_NL_antepartum_second_visit)[2:dim(data_NL_antepartum_second_visit)[2]],"second_visit")
colnames(data_NL_antepartum_third_visit)[2:dim(data_NL_antepartum_third_visit)[2]] <- paste0(colnames(data_NL_antepartum_third_visit)[2:dim(data_NL_antepartum_third_visit)[2]],"third_visit")
colnames(data_NL_antepartum_fourth_visit)[2:dim(data_NL_antepartum_fourth_visit)[2]] <- paste0(colnames(data_NL_antepartum_fourth_visit)[2:dim(data_NL_antepartum_fourth_visit)[2]],"fourth_visit")


data_NL_ap <- left_join(data_NL_antepartum_first_visit,data_NL_antepartum_second_visit,by = c("NL_COVIDSTUDYID"))
data_NL_ap <- left_join(data_NL_ap,data_NL_antepartum_third_visit,by = c("NL_COVIDSTUDYID"))
data_NL_ap <- left_join(data_NL_ap,data_NL_antepartum_fourth_visit,by = c("NL_COVIDSTUDYID"))

dim(data_NL_ap)

# check for NL data size is correct
length(unique(data_NL_antepartum$NL_COVIDSTUDYID))



#### FIX INCORRECT DATA ####
# DO THIS LATER? #
data_bc$time_del <- as.numeric(as.Date(data_bc$r_dob) - as.Date(data_bc$e_diagnosis))
data_ns$time_del <- as.numeric(as.Date(data_ns$r_dob, format = "%m/%d/%Y") - as.Date(data_ns$e_diagnosis, format = "%m/%d/%Y"))
data_on$time_del <- as.numeric(as.Date(data_on$baby_birth_date) - as.Date(data_on$C_COVID_diagnosis_date))
data_qc$time_del <- as.numeric(as.Date(data_qc$r_dob) - as.Date(data_qc$e_diagnosis))
data_mb$time_del <- as.numeric(as.Date(data_mb$r_dob) - as.Date(data_mb$e_diagnosis))
data_nb$time_del <- as.numeric(as.Date(data_nb$r_dob) - as.Date(data_nb$e_diagnosis))
data_pe$time_del <- as.numeric(as.Date(data_pe$r_dob) - as.Date(data_pe$e_diagnosis))
data_yt$time_del <- as.numeric(as.Date(data_yt$r_dob) - as.Date(data_yt$e_diagnosis))
data_NL$time_del <- as.numeric(as.Date(data_NL$B1_Birth.Date, format = "%d-%b-%y") - as.Date(data_NL$MT_COLLECTION_DT, format = "%d-%b-%y") )


data_bc$prov <- "BC"
data_ns$prov <- "NS"
data_on$prov <- "ON"
data_qc$prov <- "QC"
data_mb$prov <- "MB"
data_nb$prov <- "NB"
data_pe$prov <- "PE"
data_yt$prov <- "YT"
data_NL$prov <- "NL"

# View(data_bc[which(data_bc$time_del > 280), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# View(data_bc[which(data_bc$time_del < 0), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])

## combine non-ontario, non NL data

colnames(data_bc)[which(!(colnames(data_bc) %in% colnames(data_mb)))]
sum((colnames(data_bc) %in% colnames(data_qc)))

data_bc <- data_bc %>%
  mutate(across(everything(), as.character))

data_ns <- data_ns %>%
  mutate(across(everything(), as.character))

data_qc <- data_qc %>%
  mutate(across(everything(), as.character))

data_mb <- data_mb %>%
  mutate(across(everything(), as.character))

data_nb <- data_nb %>%
  mutate(across(everything(), as.character))

data_pe <- data_pe %>%
  mutate(across(everything(), as.character))

data_yt <- data_yt %>%
  mutate(across(everything(), as.character))



full_data_sansON <- bind_rows(data_bc,data_ns)
full_data_sansON <- bind_rows(full_data_sansON,data_qc)
full_data_sansON <- bind_rows(full_data_sansON,data_mb)
full_data_sansON <- bind_rows(full_data_sansON,data_nb)
full_data_sansON <- bind_rows(full_data_sansON,data_pe)
full_data_sansON <- bind_rows(full_data_sansON,data_yt)

full_data_sansON[full_data_sansON == ""] <- NA
## This looks fine so far
## Now collect minimal data - refer to ariannes cancovid script to make sure all the variables are collected properismo
## Needs to be done separately for main data vs ON vs AB
## The most important data points are
# 1. Date of diagnosis
# 2. Expected Date of Delivery
# 3. Date of Delivery / Date of Birth
# 4. LMP -> Gestational Age at Delivery -> Preterm Birth
# 5. Admission to NICU
# 6. Admission to ICU 
# 7. Apgar 5
# 8. Vaccine History
# 9. Pregnancy outcome (still birth, live birth, ectopic pregnancy, elective abortion, term abortion, spontaneous abortion)

## We begin with  1.Date of Diagnosis and 2. Hospitalization, Admission to ICU, and Admission to NICU. Because we need these variables to estimate the actual number of cases by province
dim(full_data_sansON)
sum(is.na(full_data_sansON$e_diagnosis))
glimpse(full_data_sansON$e_diagnosis)

full_data_sansON %>% group_by(prov) %>% summarise(n=n())
## fill in diagnosis date 
full_data_sansON_temp <- full_data_sansON %>% filter(is.na(e_diagnosis))


full_data_sansON_temp$e_diagnosis <- case_when(
  full_data_sansON_temp$d_naso1_result == 1 ~ full_data_sansON_temp$d_naso1_collect,
  full_data_sansON_temp$d_naso2_result == 1 ~ full_data_sansON_temp$d_naso2_collect,
  full_data_sansON_temp$d_naso3_result == 1 ~ full_data_sansON_temp$d_naso3_collect,
  full_data_sansON_temp$d_naso4_result == 1 ~ full_data_sansON_temp$d_naso4_collect,
  full_data_sansON_temp$d_throat_result == 1 ~ full_data_sansON_temp$d_throat_collect,
  full_data_sansON_temp$d_blood_result == 1 ~ full_data_sansON_temp$d_blood_collect,
  full_data_sansON_temp$d_milk_result == 1 ~ full_data_sansON_temp$d_milk_collect,
  full_data_sansON_temp$d_amnio_result == 1 ~ full_data_sansON_temp$d_amnio_collect,
  full_data_sansON_temp$d_igm_result == 1 ~ full_data_sansON_temp$d_igm_collect,
  full_data_sansON_temp$d_igg_result == 1 ~ full_data_sansON_temp$d_igg_collect,
  full_data_sansON_temp$d_other_result == 1 ~ full_data_sansON_temp$d_other_collect,
  full_data_sansON_temp$d_other_result_2 == 1 ~ full_data_sansON_temp$d_other_collect_2,
  full_data_sansON_temp$d_other_result_3 == 1 ~ full_data_sansON_temp$d_other_collect_3
)


full_data_sansON_temp$e_diagnosis 
dim(full_data_sansON_temp)
dim(full_data_sansON[which(is.na(full_data_sansON$e_diagnosis)),])

full_data_sansON[which(is.na(full_data_sansON$e_diagnosis)),c("e_diagnosis")] <- full_data_sansON_temp$e_diagnosis

sum(is.na(full_data_sansON$e_diagnosis))

# hospitalization, admission to ICU, admission to NICU


#hospitalization:
full_data_sansON$hospitalization <- case_when(
  full_data_sansON$e_hosp == 1 ~ 1,
  full_data_sansON$e_hosp2 == 1 ~ 1
  )

full_data_sansON$hospitalization[is.na(full_data_sansON$hospitalization)] <- 0

#ICU
full_data_sansON$icu <- case_when(
  full_data_sansON$g_icu == 1 ~ 1,
  full_data_sansON$g_icu2 == 1 ~ 1
  )

full_data_sansON$icu[is.na(full_data_sansON$icu)] <- 0


#### MAKE SURE EACH DATA SET IS SPLIT UP INTO MATERNITY vs INFANT DATA. #####
## data_on (only mothers) vs on.dat (infants)  already done ##
## full_data_sansON (only mothers) vs full_data_sansON_wtwins (multiplied with twins)
## data_NL (only mothers) vs data_NL_wtwins (multiplied with twins)
full_data_sansON$i_fetusnumber <- as.numeric(full_data_sansON$i_fetusnumber)
full_data_sansON$i_fetusnumber[which(full_data_sansON$i_fetusnumber == 999)] <- 1
full_data_sansON$i_fetusnumber[which(is.na(full_data_sansON$i_fetusnumber))] <- 1

indexes <- 1
for(i in 2:dim(full_data_sansON)[1]){
  indexes <- c(indexes,rep(i,full_data_sansON$i_fetusnumber[i]))
}

full_data_sansON_wtwins <- full_data_sansON[indexes,]
dim(full_data_sansON_wtwins)
#full_data_sansON$
  
  
## placental Pathology  
#placental_path_NS <- full_data_sansON[which(full_data_sansON$prov == "NS"),c("a_record_id","r_path","r_path_res","r_path_res_desc","r_covtest","r_covtest_collect_d","r_covtest_collect_t","r_covtest_lab","r_covtest_test","r_cov_res","prov")]
#placental_path_NS <- placental_path_NS[which(placental_path_NS$r_path == 1),]

## columns to combine 
# s_baby, s_baby_2, s_baby_3, 
# s_apgar_5, s_apgar_5_2, s_apgar_5_3, 
# s_bw_gm, s_bw_gm_2, s_bw_gm_3
# s_pe_genitalia, s_pe_genitalia_2, s_pe_genitalia_3
# s_infdeath, s_infdeath_2, s_infdeath_3
# s_nicu, s_nicu_2, s_nicu_3
  
  
# full_data_sansON_wtwins2 <- pivot_longer(full_data_sansON,cols = c(),)
                                         
                                         
## NL
dim(data_NL)
length(unique(data_NL$NL_COVIDSTUDYID))


# NICU

full_data_sansON$NICU <- case_when(
  full_data_sansON$t_nicu == 1 ~ "Yes",
  full_data_sansON$t_nicu == 0 ~ "No",
  is.na(full_data_sansON$s_bw_gm) == FALSE ~ "No")


# data only after omicron that needs to be multiplied 

# full_data_sansON$to_multiply <- 0
# 
# full_data_sansON$to_multiply[intersect(intersect(intersect(which(full_data_sansON$e_diagnosis > as.Date("2021-12-31")),which(full_data_sansON$hospitalization != 1)),which(full_data_sansON$icu != 1)),which(full_data_sansON$NICU != "Yes"))] <- 1
# 
# sum(full_data_sansON$to_multiply, na.rm = TRUE)
# 
# dim(full_data_sansON)
# dim(full_data_sansON[c(which(full_data_sansON$to_multiply != 1),which(full_data_sansON$to_multiply == 1)),])
# 




data_count <- full_data_sansON %>% group_by(prov) %>% summarise(n=n())



## fill these out first
# 2. Expected Date of Delivery
# 3. Date of Delivery / Date of Birth
# 4. LMP -> Gestational Age at Delivery -> Preterm Birth
# 5. Admission to NICU
# 6. Admission to ICU 
# 7. Apgar 5
# 8. Vaccine History
# 9. Pregnancy outcome (still birth, live birth, ectopic pregnancy, elective abortion, term abortion, spontaneous abortion)

#### CLEAN DATA ####
#The point here is to identify missing as 999 or 99 or 9999-99-99 or whatever, and all NAs can be "0"
#Then recode everything as a factor or numeric. 
#This needs to be done for all provinces and then ontario separately 
full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)
full_data_sansON$i_deliverydate_est <- replace(full_data_sansON$i_deliverydate_est, which(full_data_sansON$i_deliverydate_est == "9999-09-09"), NA)
full_data_sansON$e_diagnosis <- replace(full_data_sansON$e_diagnosis, which(full_data_sansON$e_diagnosis == "9999-09-09"), NA)
full_data_sansON$s_apgar_5 <- replace(full_data_sansON$s_apgar_5, which(full_data_sansON$s_apgar_5 == "999"), NA)
full_data_sansON$n_covid <- replace(full_data_sansON$n_covid, which(full_data_sansON$n_covid == "999"), NA)
full_data_sansON$s_bw_gm <- replace(full_data_sansON$s_bw_gm, which(full_data_sansON$s_bw_gm == "999"), NA)
full_data_sansON$b_age <- replace(full_data_sansON$b_age, which(full_data_sansON$b_age == "999"), NA)
full_data_sansON$h_gravida <- replace(full_data_sansON$h_gravida, which(full_data_sansON$h_gravida == "999"), NA)
full_data_sansON$e_diagnosis <- replace(full_data_sansON$e_diagnosis, which(full_data_sansON$e_diagnosis == "8888-08-08"), NA)
full_data_sansON$e_hosp <- replace(full_data_sansON$e_hosp, which(full_data_sansON$e_hosp == "999"), NA)
full_data_sansON$e_hosp2 <- replace(full_data_sansON$e_hosp2, which(full_data_sansON$e_hosp2 == "999"), NA)



# expected date of delivery
sum(is.na(full_data_sansON$i_deliverydate_est))
# date of birth
sum(is.na(full_data_sansON$r_dob))
#ga_at_del

full_data_sansON$r_dob <- as.Date(as.character(full_data_sansON$r_dob), format = "%Y-%m-%d")

# full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)

full_data_sansON$ga_at_del <- as.numeric((280 - (as.Date(full_data_sansON$i_deliverydate_est) - as.Date(full_data_sansON$r_dob)))/7)




# function to determine SGA

# 
# ga.F <- seq(22, 43)
# weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above
# 
# weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above
# 
# # if unknown go with male to be conservative
# SGA <- data.frame(ga.F, weight.F, weight.M)
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
# full_data_sansON$SGA <- sga.fun(full_data_sansON)








# full_data_sansON$ga_at_del <- 40-((as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$i_deliverydate_est))/7)

##### CCLEAN THIS UP !! #####
# full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del < 20)] <- NA
# full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del > 50)] <- NA


full_data_sansON$date_diff <- as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$i_deliverydate_est)

data_check1 <- full_data_sansON %>% filter(abs(date_diff) > 100) %>% dplyr::select(a_record_id, date_diff,r_dob,i_deliverydate_est,i_lmp,e_diagnosis,p_outcome)


## do this instead for now
full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del < 20)] <- NA
full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del > 50)] <- NA

# plot(full_data_sansON$ga_at_del )

full_data_sansON$ga_del_cat <- case_when(
  full_data_sansON$ga_at_del < 28 ~ "extremely preterm",
  full_data_sansON$ga_at_del < 32 ~ "very preterm",
  full_data_sansON$ga_at_del < 34 ~ "moderate preterm",
  full_data_sansON$ga_at_del < 37 ~ "late preterm",
  full_data_sansON$ga_at_del >= 37 ~ "term",
)

full_data_sansON$ga_del_cat2 <- case_when(
  full_data_sansON$ga_at_del < 37 ~ "preterm",
  full_data_sansON$ga_at_del >= 37 ~ "term"
)

sum(full_data_sansON$ga_del_cat2 == "preterm",na.rm = TRUE)

## APGAR 5

full_data_sansON$apgar5 <- factor(ifelse(as.numeric(full_data_sansON$s_apgar_5) < 7, "<7", "≥7"))

## Preg Outcome
full_data_sansON$birth_outcome <- case_when(
  full_data_sansON$p_outcome == 1 ~ "Loss",
  full_data_sansON$p_outcome == 2 ~ "Stillbirth",
  full_data_sansON$p_outcome == 3 ~ "Livebirth",
  full_data_sansON$o_loss == 1 ~ "Loss",
  full_data_sansON$o_sb == 1 ~ "Stillbirth",
  full_data_sansON$o_spont == 1 ~ "Loss",
  full_data_sansON$o_elective == 1 ~ "Loss"
)

sum(is.na(full_data_sansON$birth_outcome))

# full_data_sansON$o_spont[is.na(full_data_sansON$birth_outcome)]

## Vaccine history

summary(as.factor(full_data_sansON$n_covid))

## Birth Weight
full_data_sansON$s_bw_gm <- as.numeric(full_data_sansON$s_bw_gm)

full_data_sansON$bw_cat <- case_when(
  full_data_sansON$s_bw_gm < 2500 ~ "<2500",
  full_data_sansON$s_bw_gm >= 2500 & full_data_sansON$s_bw_gm <=4000 ~ "2500-4000",
  full_data_sansON$s_bw_gm > 4000 ~ ">4000"
)

#Ethnicity


full_data_sansON$eth <-  case_when(
  all(c(full_data_sansON$b_ethnicity___1, full_data_sansON$b_ethnicity___2, full_data_sansON$b_ethnicity___3, full_data_sansON$b_ethnicity___4, full_data_sansON$b_ethnicity___5, full_data_sansON$b_ethnicity___6, full_data_sansON$b_ethnicity___7, full_data_sansON$b_ethnicity___8, full_data_sansON$b_ethnicity___998, full_data_sansON$b_ethnicity___999) == 0) ~ "Missing",
  full_data_sansON$b_ethnicity___999 == 1 ~ "Unknown",
  full_data_sansON$b_ethnicity___998 == 1 ~ "Other",
  full_data_sansON$b_ethnicity___1 == 1 ~ "White",
  full_data_sansON$b_ethnicity___2 == 1 ~ "African/Carribean/Black",
  full_data_sansON$b_ethnicity___3 == 1 ~ "Hispanic/Latino",
  full_data_sansON$b_ethnicity___4 == 1 ~ "East Asian",
  full_data_sansON$b_ethnicity___5 == 1 ~ "South Asian",
  full_data_sansON$b_ethnicity___6 == 1 ~ "South East Asian",
  full_data_sansON$b_ethnicity___7 == 1 ~ "Middle East",
  full_data_sansON$b_ethnicity___8 == 1 ~ "Indigenous"
)


# describeFactors(full_data_sansON$eth)
full_data_sansON$eth <- factor(full_data_sansON$eth)
# change the missing and unknown to NA
levels(full_data_sansON$eth)[which(levels(full_data_sansON$eth) == "Missing" | levels(full_data_sansON$eth) == "Unknown")] <- NA

# reduce the number of categories
full_data_sansON$eth_cat <- full_data_sansON$eth
levels(full_data_sansON$eth_cat)[which(levels(full_data_sansON$eth) == "East Asian" | levels(full_data_sansON$eth) == "South East Asian")] <- "East or SE Asian"
full_data_sansON$eth_cat <- relevel(full_data_sansON$eth_cat, ref = "White")
describeFactors(full_data_sansON$eth_cat)


## age
full_data_sansON$b_age







full_data_sansON <- full_data_sansON %>%
  mutate(allna_check = all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))))

### symptoms


full_data_sansON$asym <- NA
full_data_sansON$asym <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_asymptomatic == 1 ~ "Yes",
  full_data_sansON$e_asymptomatic == 0 ~ "No"
)

full_data_sansON$fever <- NA
full_data_sansON$fever <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_fever == 1 ~ "Yes",
  grepl("chill|sweat|Frissons|Chiils", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("chill|sweat|Frissons|Chiils", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("chill|sweat|Frissons|Chiils", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_fever == 0 ~ "No"
)

full_data_sansON$cough <- NA
full_data_sansON$cough <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_cough == 1 ~ "Yes",
  full_data_sansON$e_cough == 0 ~ "No"
)

full_data_sansON$head <- NA
full_data_sansON$head <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_head == 1 ~ "Yes",
  grepl("tender frontal|head", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("tender frontal|head", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("tender frontal|head", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_head == 0 ~ "No"
)

full_data_sansON$breath <- NA
full_data_sansON$breath <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_breath == 1 ~ "Yes",
  grepl("chest congestion|breathing|wheez|orthpnoea|asthma", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("chest congestion|breathing|wheez|orthpnoea|asthma", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("chest congestion|breathing|wheez|orthpnoea|asthma", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_breath == 0 ~ "No"
)

full_data_sansON$runny <- NA
full_data_sansON$runny <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_runny == 1 ~ "Yes",
  grepl("nasal|nose|congestion|rhin|sinus|URI", full_data_sansON$e_othersx_spec, ignore.case = TRUE) & !grepl("chest", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("nasal|nose|congestion|rhin|sinus|URI", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) & !grepl("chest", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("nasal|nose|congestion|rhin|sinus|URI", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) & !grepl("chest", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_runny == 0 ~ "No"
)

full_data_sansON$weakness <- NA
full_data_sansON$weakness <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  grepl("weakness", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("weakness", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_othersx == 0 ~ "No"
)

full_data_sansON$muscle <- NA
full_data_sansON$muscle <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_muscle == 1 ~ "Yes",
  grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("flu|joint|abdominal pain|Back and musculoskeletal pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_muscle == 0 ~ "No"
)

full_data_sansON$anorexia <- NA
full_data_sansON$anorexia <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_anorexia == 1 ~ "Yes",
  full_data_sansON$e_anorexia == 0 ~ "No"
)

full_data_sansON$diarrhea <- NA
full_data_sansON$diarrhea <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_diarrhea == 1 ~ "Yes",
  full_data_sansON$e_diarrhea == 0 ~ "No"
)

full_data_sansON$vomit <- NA

full_data_sansON$vomit <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_vomit == 1 ~ "Yes",
  full_data_sansON$e_vomit == 0 ~ "No"
)

full_data_sansON$fatigue <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_fatigue == 1 ~ "Yes",
  full_data_sansON$e_fatigue == 0 ~ "No"
)

full_data_sansON$anosmia <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_anosmia == 1 ~ "Yes",
  full_data_sansON$e_anosmia == 0 ~ "No"
)

full_data_sansON$throat <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_throat == 1 ~ "Yes",
  grepl("odynophagia|gorge|Laryngitis|throat", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("odynophagia|gorge|Laryngitis|throat", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("odynophagia|gorge|Laryngitis|throat", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_throat == 0 ~ "No"
)

full_data_sansON$sputum <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_sputum == 1 ~ "Yes",
  full_data_sansON$e_sputum == 0 ~ "No"
)

full_data_sansON$malaise <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_malaise == 1 ~ "Yes",
  full_data_sansON$e_malaise == 0 ~ "No"
)

full_data_sansON$chest_pain <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("douleur pleurétique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  TRUE ~ "No"
)


full_data_sansON$tachycardia <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  grepl("Tachycardia|Increased heart rate|heart rate", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("Tachycardia|Increased heart rate|heart rate", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("Tachycardia|Increased heart rate|heart rate", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  TRUE ~ "No"
)




# replace the No entry with NA
full_data_sansON <- full_data_sansON %>% 
  mutate(across(.cols = c(asym, fever, cough, head, breath, runny, muscle, anorexia, diarrhea, vomit, fatigue, anosmia, throat, sputum, malaise, chest_pain, tachycardia, weakness), ~ifelse(.x == "Miss", NA, .x)))
describeFactors(full_data_sansON$fever)
describeFactors(full_data_sansON$chest_pain)


full_data_sansON <- full_data_sansON %>% mutate(all_nacheck2 = all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))))

full_data_sansON$cvs <- case_when(
  full_data_sansON$all_nacheck2 & as.numeric(full_data_sansON$j_none___1) == 0 ~ "No entry",
  full_data_sansON$all_nacheck2 ~ "No",
  as.numeric(full_data_sansON$j_none___1) == 1 ~ "No",
  as.numeric(full_data_sansON$j_cvs) == 1 ~ "Yes",
  as.numeric(full_data_sansON$l_htn) == 1 ~ "Yes",
  as.numeric(full_data_sansON$j_cvs) == 0 ~ "No",
  is.na(full_data_sansON$j_cvs) ~ "No"
)



# full_data_sansON <- full_data_sansON %>% 
#   mutate(cvs = case_when(
#     all_nacheck2 & as.numeric(j_none___1) == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     as.numeric(j_none___1) == 1 ~ "No",
#     as.numeric(j_cvs) == 1 ~ "Yes",
#     as.numeric(l_htn) == 1 ~ "Yes",
#     as.numeric(j_cvs) == 0 ~ "No",
#     is.na(j_cvs) ~ "No"
#   ))
# 
#   cns = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_cns == 1 ~ "Yes",
#     j_cns == 0 ~ "No",
#     is.na(j_cns) ~ "No"
#   ),
#   resp = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_resp == 1 ~ "Yes",
#     j_resp == 0 ~ "No",
#     is.na(j_resp) ~ "No"
#   ),
#   eentm = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_eentm == 1 ~ "Yes",
#     j_eentm == 0 ~ "No",
#     is.na(j_eentm) ~ "No"
#   ),
#   gi = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2 ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_gi == 1 ~ "Yes",
#     j_gi == 0 ~ "No",
#     is.na(j_gi) ~ "No"
#   ),
#   gu = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2 ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_gu == 1 ~ "Yes",
#     j_gu == 0 ~ "No",
#     is.na(j_gu) ~ "No"
#   ),
#   repro = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2 ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_repro == 1 ~ "Yes",
#     j_repro == 0 ~ "No",
#     is.na(j_repro) ~ "No"
#   ),
#   endo = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_endo == 1 ~ "Yes",
#     j_endo == 0 ~ "No",
#     is.na(j_endo) ~ "No"
#   ),
#   ms = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_ms == 1 ~ "Yes",
#     j_ms == 0 ~ "No",
#     is.na(j_ms) ~ "No"
#   ),
#   hem = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_hem == 1 ~ "Yes",
#     j_hem == 0 ~ "No",
#     is.na(j_hem) ~ "No"
#   ),
#   mh = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_mh == 1 ~ "Yes",
#     j_mh == 0 ~ "No",
#     is.na(j_mh) ~ "No"
#   ),
#   aai = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2  ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_aai == 1 ~ "Yes",
#     j_aai == 0 ~ "No",
#     is.na(j_aai) ~ "No"
#   ),
#   other_comor = case_when(
#     all_nacheck2 & j_none___1 == 0 ~ "No entry",
#     all_nacheck2 ~ "No",
#     j_none___1 == 1 ~ "No",
#     j_oth == 1 ~ "Yes",
#     j_oth == 0 ~ "No",
#     is.na(j_oth) ~ "No"
#   )
#   )
# 
# # replace the No entry with NA
full_data_sansON$cvs <- ifelse(full_data_sansON$cvs  == "No entry", NA, full_data_sansON$cvs)

# check to make sure
full_data_sansON %>% group_by(prov) %>% summarise(missing_percent = sum(is.na(cvs))/n())
describeFactors(full_data_sansON$cvs)

## extract specific comorbidities of interest
# asthma
full_data_sansON$asthma <- case_when(
  full_data_sansON$j_resp_asth___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
describeFactors(full_data_sansON$asthma)

# Chronic obstructive lung disease
full_data_sansON$lung <- case_when(
  full_data_sansON$j_resp_lung___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
describeFactors(full_data_sansON$lung)

# hypertension pre-existing
full_data_sansON$htn <- case_when(
  full_data_sansON$j_cvs_htn___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
describeFactors(full_data_sansON$htn)

# diabetes 1 or 2
full_data_sansON$diabetes <- case_when(
  full_data_sansON$j_endo_diabt1___1 == 1 | full_data_sansON$j_endo_diabt2___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
describeFactors(full_data_sansON$diabetes)


# autoimmune diseases have to be gathered from a few places
# celiac, lupus, rheumatoid arthritis, Ankylosing spondylitis, others
full_data_sansON$j_aai_oth_s # check this variable for others eg MS

full_data_sansON$autoimm <- case_when(
  (full_data_sansON$j_gi_celiac___1 == 1) | (full_data_sansON$j_ms_lupus___1 == 1) | (full_data_sansON$j_ms_as___1 == 1) | (full_data_sansON$j_ms_ra___1 == 1) | grepl("Multiple sclerosis|Antiphospho|neutropenia|SLE|lupus", full_data_sansON$j_aai_oth_s, ignore.case = TRUE) ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
describeFactors(full_data_sansON$autoimm)

## composite comorbidities
c(full_data_sansON$cvs, full_data_sansON$diabetes, full_data_sansON$htn, full_data_sansON$asthma, full_data_sansON$hbv, full_data_sansON$hcv)

full_data_sansON$any_comor <- case_when(
  full_data_sansON$cvs == "Yes" | full_data_sansON$asthma == "Yes" | full_data_sansON$diabetes == "Yes" | full_data_sansON$htn == "Yes" | full_data_sansON$k_hbv == 1 | full_data_sansON$k_hcv == 1 ~ "Yes",
  full_data_sansON$cvs == "No" & full_data_sansON$asthma == "No" & full_data_sansON$diabetes == "No" & full_data_sansON$htn == "No" ~ "No"
)



full_data_sansON$i_weight <- replace(full_data_sansON$i_weight, which(full_data_sansON$i_weight == 999 | full_data_sansON$i_weight == 666), NA)
full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(full_data_sansON$i_height == 999 | full_data_sansON$i_height == 666), NA)

full_data_sansON$BMI <- as.numeric(full_data_sansON$i_weight)/(as.numeric(full_data_sansON$i_height)/100)^2
describeMedian(full_data_sansON$BMI, iqr = FALSE) # check for out of range values - and remove or correct

# BMI >= 30 variable
full_data_sansON$BMI_cat <- factor(case_when(
  full_data_sansON$BMI < 18.5 ~ "<18.5",
  full_data_sansON$BMI < 25 ~ "18.5-24",
  full_data_sansON$BMI < 30 ~ "25-29",
  full_data_sansON$BMI >= 30 ~ "≥30"
))
describeFactors(full_data_sansON$BMI_cat)
full_data_sansON$BMI_cat <- factor(full_data_sansON$BMI_cat, levels = c("<18.5", "18.5-24", "25-29", "≥30"))

full_data_sansON$age_cat <- factor(case_when(
  full_data_sansON$b_age <25 ~ "<25 years",
  full_data_sansON$b_age <30 ~ "25-29 years",
  full_data_sansON$b_age <36 ~ "30-35 years",
  full_data_sansON$b_age <40 ~ "36-39 years",
  full_data_sansON$b_age >=40 ~ "≥40 years"
),
levels = c("<25 years", "25-29 years", "30-35 years", "36-39 years", "≥40 years"))


full_data_sansON$age_cat2 <- factor(case_when(
  full_data_sansON$age_cat == "<25 years" ~ "Less than 30",
  full_data_sansON$age_cat == "25-29 years" ~ "Less than 30",
  full_data_sansON$age_cat == "30-35 years" ~ "30-35 years",
  full_data_sansON$age_cat ==  "36-39 years" ~ "36 years and older",
  full_data_sansON$age_cat == "≥40 years" ~ "36 years and older"
),
levels = c("Less than 30", "30-35 years", "36 years and older"))

describeFactors(full_data_sansON$age_cat2)





## omicron start December 19 2021 
## Delta started April 4th 2021
## dates from https://nccid.ca/covid-19-variants/
full_data_sansON$covid_period = case_when(
  full_data_sansON$e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
  full_data_sansON$e_diagnosis >= as.Date("2021-04-04") & full_data_sansON$e_diagnosis < as.Date("2021-12-19") ~ "Delta",
  full_data_sansON$e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
)

full_data_sansON$covid_period <- factor(full_data_sansON$covid_period, levels = c("pre-Delta","Delta","Omicron"))

covid_period_table <- full_data_sansON %>% group_by(covid_period) %>% summarise(n = n(),nicu = sum(NICU == "Yes", na.rm = TRUE),stillbirth = sum(o_sb == 1, na.rm = TRUE),hospitalizations = sum(e_hosp == "1", na.rm = TRUE),icu_admissions = sum(g_icu == "1", na.rm = TRUE))

covid_period_table2 <- full_data_sansON %>% group_by(covid_period) %>% summarise(n = n(),term_births = sum(ga_del_cat == "term", na.rm = TRUE), late_preterm = sum(ga_del_cat == "late preterm", na.rm = TRUE),moderate_preterm = sum(ga_del_cat == "moderate preterm", na.rm = TRUE),very_preterm = sum(ga_del_cat == "very preterm", na.rm = TRUE))




# # how to multiply data? duplicate rows 10x that have "to_multiply" = 1
# # do this later?
# full_data_sansON_duplicated <- full_data_sansON[c(which(full_data_sansON$to_multiply != 1),rep(which(full_data_sansON$to_multiply == 1), 10)), ]
# 
# covid_period_table3<- full_data_sansON_duplicated %>% group_by(covid_period) %>% summarise(n = n(),nicu = sum(NICU == "Yes", na.rm = TRUE),stillbirth = sum(o_sb == 1, na.rm = TRUE),hospitalizations = sum(e_hosp == "1", na.rm = TRUE),icu_admissions = sum(g_icu == "1", na.rm = TRUE))
# 
# covid_period_table23 <- full_data_sansON_duplicated %>% group_by(covid_period) %>% summarise(n = n(),term_births = sum(ga_del_cat == "term", na.rm = TRUE), late_preterm = sum(ga_del_cat == "late preterm", na.rm = TRUE),moderate_preterm = sum(ga_del_cat == "moderate preterm", na.rm = TRUE),very_preterm = sum(ga_del_cat == "very preterm", na.rm = TRUE))


## Combine with ontario data?? Then present outcomes by hospitalization? Then show temporal trends?
#### ontario and NL ####
#### ontario fixes: #####

##BMI
data_on$BMI_cat <- factor(case_when(
  data_on$new_BMI < 18.5 ~ "<18.5",
  data_on$new_BMI < 25 ~ "18.5-24",
  data_on$new_BMI < 30 ~ "25-29",
  data_on$new_BMI >= 30 ~ "≥30"
))

describeFactors(data_on$BMI_cat)
data_on$BMI_cat <- factor(data_on$BMI_cat, levels = c("<18.5", "18.5-24", "25-29", "≥30"))


data_NL$Pre.Preg.Wgt.kg[which(data_NL$Pre.Preg.Wgt.kg %in% c("999","**","","No Entry"))] <- NA
data_NL$Height.in.cm[which(data_NL$Height.in.cm %in% c("999","**","","No Entry"))] <- NA

data_NL$Height.in.cm <- as.numeric(data_NL$Height.in.cm)
data_NL$Pre.Preg.Wgt.kg <- as.numeric(data_NL$Pre.Preg.Wgt.kg)

data_NL$Height.in.m <-  data_NL$Height.in.cm/100

data_NL$BMI <- data_NL$Pre.Preg.Wgt.kg/(data_NL$Height.in.m^2)


data_NL$BMI_cat <- factor(case_when(
  data_NL$BMI < 18.5 ~ "<18.5",
  data_NL$BMI < 25 ~ "18.5-24",
  data_NL$BMI < 30 ~ "25-29",
  data_NL$BMI >= 30 ~ "≥30"
))



## hypertension and diabetes (Pre-Existing)
data_on$htn <-  ifelse(data_on$hyper_final == 1, 1,0)
data_on$diabetes <-  ifelse(data_on$diabetes_final == 1, 1,0)
unique(data_on$CVD)

data_NL$Pre.existing.Diab.Insulin.Required
data_NL$Pre.existing.Diab
data_NL$OTHER

## NL pre existin diabetes and hypertension don't seem to be available and cannot find out how to derive them from existing data.
data_NL$diabetes <- NA
data_NL$htn <- NA

## ethnicity (F)
unique(full_data_sansON$eth_cat)
data_on$eth_cat <- case_when(
  data_on$PH_SES_EAST_ASIAN == "YES" | data_on$PH_SES_EAST_SE_ASIAN == "YES" ~ "East or SE Asian",
  data_on$PH_SES_WHITE == "YES" ~ "White",
  data_on$PH_SES_LATINO == "YES" ~ "Hispanic/Latino",
  data_on$PH_SES_MIDDLE_EASTERN == "YES" ~ "Middle East",
  data_on$PH_SES_BLACK == "YES" ~ "African/Carribean/Black",
  data_on$PH_SES_SOUTH_ASIAN == "YES" ~ "South Asian",
  data_on$PH_RACE_OTHER == "YES" ~ "Other"
)

## ethnicity also not available for NL

data_on$hospitalization <- case_when(
  data_on$C_hospitalized_COVID == "Yes" ~ 1,
  data_on$C_hospitalized_COVID == "No" ~ 0
)


data_NL$hospitalization <- factor(case_when(
  data_NL$HOSP_FOR_COVID == "Yes" ~ 1,
  TRUE ~ 0
))
 

data_on$icu <- case_when(
  data_on$C_ICU_admission == "Yes" ~ 1,
  data_on$C_ICU_admission == "No" ~ 0
)


## 0 records of ICU admission
data_NL$icu <- case_when(
  data_NL$Care.Unit == "Yes" ~ 1,
  TRUE ~ 0
)



## should only be done for infant data
data_on$NICU <- case_when(
  data_on$NICU_admission_flag == "Y" ~ 1,
  data_on$NICU_admission_flag == "N" ~ 0
)

data_NL$NICU <- case_when(
  data_NL$B1_Care.Unit == "50 Neonatal Intensive Care" ~ 1,
  data_NL$B2_Care.Unit == "50 Neonatal Intensive Care" ~ 1,
  TRUE ~ 0
)


## gestational age at birth (term vs preterm)
data_on$B_GA_birth[which(data_on$B_GA_birth == "")] <- NA

summary(as.factor(data_on$B_GA_birth))
summary(as.factor(full_data_sansON$ga_del_cat2))
summary(as.factor(full_data_sansON$ga_del_cat))

full_data_sansON$ga_del_cat <- factor(full_data_sansON$ga_del_cat, levels = c("extremely preterm","very preterm","moderate preterm","late preterm","term"))

data_on$ga_del_cat <- case_when(
  data_on$B_GA_birth == "< 28 weeks" ~ "extremely preterm",
  data_on$B_GA_birth == "28 - 31 weeks" ~ "very preterm",
  data_on$B_GA_birth == "32-33 weeks" ~ "moderate preterm",
  data_on$B_GA_birth == "34-36 weeks" ~ "late preterm",
  data_on$B_GA_birth == "Term" ~ "term"
)
data_on$ga_del_cat <- factor(data_on$ga_del_cat, levels = c("extremely preterm","very preterm","moderate preterm","late preterm","term"))

summary(as.factor(data_on$ga_del_cat))

data_on$ga_del_cat2 <- case_when(
  data_on$ga_del_cat == "extremely preterm" ~ "preterm",
  data_on$ga_del_cat == "very preterm" ~ "preterm",
  data_on$ga_del_cat == "moderate preterm" ~ "preterm",
  data_on$ga_del_cat == "late preterm" ~ "preterm",
  data_on$ga_del_cat == "term" ~ "term"
)
summary(as.factor(data_on$ga_del_cat2))


data_NL$ga_del_cat <- case_when(
  data_NL$Gestation.in.weeks < 28 ~ "extremely preterm",
  data_NL$Gestation.in.weeks < 32 ~ "very preterm",
  data_NL$Gestation.in.weeks < 34 ~ "moderate preterm",
  data_NL$Gestation.in.weeks < 37 ~ "late preterm",
  TRUE ~ "term"
)

data_NL$ga_del_cat2 <- factor(case_when(
  data_NL$ga_del_cat == "extremely preterm" ~ "preterm",
  data_NL$ga_del_cat == "very preterm" ~ "preterm",
  data_NL$ga_del_cat == "moderate preterm" ~ "preterm",
  data_NL$ga_del_cat == "late preterm" ~ "preterm",
  data_NL$ga_del_cat == "term" ~ "term"
))
summary(data_NL$ga_del_cat2)



#gravidity
summary(as.factor(data_on$gravida))

summary(as.factor(full_data_sansON$h_gravida))

data_on$h_gravida  <- data_on$gravida

full_data_sansON$gravida <- as.numeric(full_data_sansON$h_gravida)



## NL GRAVIDA ## 
data_NL$Gravida




# asthma (skip for now? non ontario only? missin also for NL)
summary(as.factor(full_data_sansON$asthma))


#hospitalization/ icu done. Try "oxygen"

summary(as.factor(full_data_sansON$e_oxygen___1))
summary(as.factor(data_on$H_Any_Other_Oxygen))

full_data_sansON$oxygen <- case_when(
  full_data_sansON$e_oxygen___1 == 1 ~ TRUE,
  full_data_sansON$e_oxygen___1 == 0 ~ FALSE,
  is.na(full_data_sansON$e_oxygen___1) ~ NA
)

data_on$oxygen <- case_when(
  data_on$H_Any_Other_Oxygen == "Yes" ~ TRUE,
  data_on$H_Any_Other_Oxygen == "No" ~ FALSE,
  data_on$H_Any_Other_Oxygen == "Unknown" ~ NA,
  is.na(data_on$H_Any_Other_Oxygen) ~ NA
)

summary(as.factor(data_on$oxygen))
summary(as.factor(full_data_sansON$oxygen))

## oxygen seems unavailable for NLCHECK DIANOSIS CODES

# mode of delivery
summary(as.factor(full_data_sansON$p_mode))
summary(as.factor(data_on$birth_type_id))

data_on$mode_del <- case_when(
  data_on$birth_type_id == 1012880 ~ "vaginal",
  data_on$birth_type_id == 1012890 ~ "cs",
  data_on$birth_type_id == 1012900 ~ "cs",
  data_on$birth_type_id == 1012910 ~ "vaginal"
)

data_on$mode_del2 <- case_when(
  data_on$birth_type_id == 1012880 ~ "assisted vaginal",
  data_on$birth_type_id == 1012890 ~ "induced or spontaneous labour cs",
  data_on$birth_type_id == 1012900 ~ "no labour cs",
  data_on$birth_type_id == 1012910 ~ "spontaneous vaginal"
)


## NL mode del, mode del 2

data_NL$mode_del <- case_when(
  data_NL$Type.of.Labour == "1 SPONTANEOUS" ~ "vaginal",
  data_NL$Type.of.Labour == "2 INDUCED" ~ "vaginal",
  data_NL$Type.of.Labour == "3 NO LABOUR (ELECTIVE C/S)" ~ "cs",
  TRUE ~ NA
)

data_NL$mode_del2 <- case_when(
  data_NL$Type.of.Labour == "1 SPONTANEOUS" ~ "spontaneous vaginal",
  data_NL$Type.of.Labour == "2 INDUCED" ~ "assisted vaginal",
  data_NL$Type.of.Labour == "3 NO LABOUR (ELECTIVE C/S)" ~ "no labour cs",
  TRUE ~ NA
)
  
  unique(data_NL$Type.of.Labour)


summary(as.factor(full_data_sansON$p_mode))
full_data_sansON$mode_del <- case_when(
  full_data_sansON$p_mode == 1 ~ "vaginal",
  full_data_sansON$p_mode == 2 ~ "cs"
)

summary(as.factor(full_data_sansON$mode_del))
summary(as.factor(data_on$mode_del))




## pregnancy outcome
summary(as.factor(full_data_sansON$p_outcome))
summary(as.factor(data_on$pregnancy_outcome_id))

data_on$preg_outcome <- case_when(
  data_on$pregnancy_outcome_id == 1021030 ~ "live birth",
  data_on$pregnancy_outcome_id == 1021040 ~ "loss",
  data_on$pregnancy_outcome_id == 1021050 ~ "loss",
  data_on$pregnancy_outcome_id == 1021070 ~ "stillbirth",
  data_on$pregnancy_outcome_id == 1021080 ~ "stillbirth",
  data_on$pregnancy_outcome_id == 1021090 ~ "stillbirth"
)

summary(as.factor(full_data_sansON$p_outcome))

full_data_sansON$preg_outcome <- case_when(
  full_data_sansON$p_outcome == 1 ~ "loss",
  full_data_sansON$p_outcome == 2 ~ "stillbirth",
  full_data_sansON$p_outcome == 3 ~ "live birth"
  
)
summary(as.factor(full_data_sansON$preg_outcome))


## NL preg outcome, only diagnosis codes 1-10 are populated


icd_10 <- read.csv("ICD10-CA.csv", header = TRUE)

all_unique_codes <- unique(c(unique(data_NL$Diagnosis.1),
                           unique(data_NL$Diagnosis.2),
                           unique(data_NL$Diagnosis.3),
                           unique(data_NL$Diagnosis.4),
                           unique(data_NL$Diagnosis.5),
                           unique(data_NL$Diagnosis.6),
                           unique(data_NL$Diagnosis.7),
                           unique(data_NL$Diagnosis.8),
                           unique(data_NL$Diagnosis.9),
                           unique(data_NL$Diagnosis.10)))

all_unique_codes <- all_unique_codes[-which(all_unique_codes %in% c("","**","No Entry"))]
all_unique_codes <- as.data.frame(all_unique_codes)
colnames(all_unique_codes)
mini_data_dictionary <- inner_join(all_unique_codes,icd_10, by = c("all_unique_codes" = "ICD10CA_code"))

## from these codes for NL extract preg_outcome, pre existing hypertension, asthma, and pre-existing diabetes

## preg_outcome 
data_NL$all_icd <- paste(data_NL$Diagnosis.1,data_NL$Diagnosis.2,data_NL$Diagnosis.3,data_NL$Diagnosis.4,data_NL$Diagnosis.5,data_NL$Diagnosis.6,data_NL$Diagnosis.7,data_NL$Diagnosis.8,data_NL$Diagnosis.9,data_NL$Diagnosis.10)


data_NL$preg_outcome <- case_when(
  grepl("Z37000",data_NL$all_icd) ~ "live birth",
  grepl("Z37001",data_NL$all_icd) ~ "live birth",
  grepl("Z37100",data_NL$all_icd) ~ "stillbirth",
  grepl("Z37200",data_NL$all_icd) ~ "live birth",
  grepl("Z37201",data_NL$all_icd) ~ "live birth",
  grepl("Z37501",data_NL$all_icd) ~ "live birth",
  grepl("Z37910",data_NL$all_icd) ~ "live birth"
)
summary(as.factor(data_NL$preg_outcome))

## pre existing hypertension and diabetes (0 and 1)

data_NL$diabetes <- case_when(grepl("O24501",data_NL$all_icd) ~ 1,
                         grepl("O24601",data_NL$all_icd) ~ 1,
                         grepl("O24701",data_NL$all_icd) ~ 1,
                         TRUE ~ 0)



data_NL$htn <- case_when(grepl("I100",data_NL$all_icd) ~ 1,
                              grepl("O10001",data_NL$all_icd) ~ 1,
                              grepl("O11001",data_NL$all_icd) ~ 1,
                              grepl("O16002",data_NL$all_icd) ~ 1,
                              TRUE ~ 0)

## asthma

data_NL$asthma <- case_when(grepl("J4590",data_NL$all_icd) ~ 1,
                         TRUE ~ 0)

#multiple pregnancy
summary(as.factor(full_data_sansON$r_num))
summary(as.factor(data_on$BIS_num_fetuses_grp))

data_on$multiple_preg <- case_when(
  data_on$BIS_num_fetuses_grp == "multiple" ~ "multiple",
  data_on$BIS_num_fetuses_grp == "singleton" ~ "singleton"
)

full_data_sansON$multiple_preg <- case_when(
  full_data_sansON$r_num == 2 ~ "multiple",
  full_data_sansON$r_num == 1 ~ "singleton"
)

summary(as.factor(full_data_sansON$multiple_preg))
summary(as.factor(data_on$multiple_preg))


data_NL$multiple_preg <- factor(case_when(
  data_NL$MULTIPLENEWBORN == "YES" ~ "multiple",
  TRUE ~ "singleton"
))
  

## previous preterm births
data_NL$history_preterm <- factor(case_when(
  data_NL$Prev.Preterm.Births == 1 ~ TRUE,
  data_NL$Prev.Preterm.Births == 2 ~ TRUE,
  data_NL$Prev.Preterm.Births == 0 ~ FALSE
))

summary(as.factor(data_on$prev_preterm_births))
summary(as.factor(full_data_sansON$h_preterm))

full_data_sansON$h_preterm[which(full_data_sansON$h_preterm == 999)] <- NA

full_data_sansON$history_preterm <- case_when(
  full_data_sansON$h_preterm == 0 ~ FALSE,
  full_data_sansON$h_preterm > 0 ~ TRUE
)

summary(as.factor(full_data_sansON$history_preterm))


data_on$history_preterm <- case_when(
  data_on$prev_preterm_births == 0 ~ FALSE,
  data_on$prev_preterm_births > 0 ~ TRUE
)

summary(as.factor(data_on$history_preterm))

## previous stillbirths
# prev_stillbirths
# 
# summary(as.factor(data_on$prev_stillbirths))
# summary(as.factor(full_data_sansON$h_))

## labour type
data_on$labour_type <- case_when(
  data_on$birth_type_id == 1012880 ~ "induced",
  data_on$birth_type_id == 1012890 ~ "no labour",
  data_on$birth_type_id == 1012900 ~ "no labour",
  data_on$birth_type_id == 1012910 ~ "spontaneous"
)
summary(as.factor(data_on$labour_type))


full_data_sansON$labour_type <- case_when(
  full_data_sansON$p_labour___1 == 1 ~ "no labour",
  full_data_sansON$p_labour___2 == 1 ~ "spontaneous",
  full_data_sansON$p_labour___3 == 1 ~ "induced"
  )
summary(as.factor(full_data_sansON$labour_type))

data_NL$labour_type <- factor(case_when(
  data_NL$mode_del2 == "spontaneous vaginal" ~ "spontaneous",
  data_NL$mode_del2 == "no labour cs" ~ "no labour",
  data_NL$mode_del2 == "assisted vaginal"  ~ "induced"
))


## apgar 5
summary(as.factor(full_data_sansON$apgar5))
summary(as.factor(data_on$B_APGAR_5))


data_on$apgar5 <- case_when(
  data_on$B_APGAR_5 == "7 or more" ~ "≥7",
  data_on$B_APGAR_5 == "Less than 7" ~ "<7",
  data_on$B_APGAR_5 == "Missing" ~ NA
)
summary(as.factor(data_on$apgar5))


## apgar 5 largely missing for NL
data_NL$apgar5 <- case_when(
  data_NL$B1_Apgar.Score.5.min < 7 ~ "<7",
  data_NL$B1_Apgar.Score.5.min >= 7 ~ "≥7"
)


## birthweight
summary(as.factor(data_on$B_infant_weight))
summary(as.factor(full_data_sansON$bw_cat))

data_on$bw_cat <- case_when(
  data_on$B_infant_weight == "< 2500g" ~ "<2500",
  data_on$B_infant_weight == "> 4000g" ~ ">4000",
  data_on$B_infant_weight == "2500 - 4000g" ~ "2500-4000"
)
summary(as.factor(data_on$bw_cat))
summary(as.factor(full_data_sansON$bw_cat))


data_NL$bw_cat <- case_when(
  data_NL$B1_Weight.in.Kilos < 2.5 ~ "<2500",
  data_NL$B1_Weight.in.Kilos <= 4.0 ~ "2500-4000",
  data_NL$B1_Weight.in.Kilos > 4.0 ~ ">4000"
)

## NICU
summary(as.factor(data_on$NICU))
summary(as.factor(full_data_sansON$NICU))
full_data_sansON$NICU[which(full_data_sansON$NICU == "Yes")] <- 1
full_data_sansON$NICU[which(full_data_sansON$NICU == "No")] <- 0

# data_NL$NICU

## vaccination status
summary(as.factor(data_on))
summary(as.factor(full_data_sansON$n_covid))
summary(as.factor(full_data_sansON$n_covid_date))
summary(as.factor(full_data_sansON$n_covid_date1))
summary(as.factor(full_data_sansON$n_covid_date2))
summary(as.factor(full_data_sansON$n_covid_date3))
summary(as.factor(full_data_sansON$n_covid_date4))


full_data_sansON$n_covid_date[which(full_data_sansON$n_covid_date == "9999-09-09")] <- NA
full_data_sansON$n_covid_date1[which(full_data_sansON$n_covid_date1 == "9999-09-09")] <- NA
full_data_sansON$n_covid_date2[which(full_data_sansON$n_covid_date2 == "9999-09-09")] <- NA
full_data_sansON$n_covid_date3[which(full_data_sansON$n_covid_date3 == "9999-09-09")] <- NA
full_data_sansON$n_covid_date4[which(full_data_sansON$n_covid_date4 == "9999-09-09")] <- NA


## ok so I don't actually know the difference between n_covid_date and n_covid_date1
## so the trick here will be to count the number of unique dates and that will be 
## the number of vaccines that the patient recieved. I will worry about 'during' pregnancy' or whatever later

full_data_sansON$number_of_vaccines <- NA

for(i in 1:length(full_data_sansON$number_of_vaccines)){
  vacc_date_list <- c(as.Date(full_data_sansON$n_covid_date[i], format = "%Y-%m-%d"),
                      as.Date(full_data_sansON$n_covid_date1[i], format = "%Y-%m-%d"),
                      as.Date(full_data_sansON$n_covid_date2[i], format = "%Y-%m-%d"),
                      as.Date(full_data_sansON$n_covid_date3[i], format = "%Y-%m-%d"),
                      as.Date(full_data_sansON$n_covid_date4[i], format = "%Y-%m-%d"))
  if(length(which(is.na(vacc_date_list))) > 0){
    vacc_date_list <- vacc_date_list[-which(is.na(vacc_date_list))]
  }
  if(length(which(vacc_date_list > as.Date(full_data_sansON$e_diagnosis[i], format = "%Y-%m-%d"))) > 0){
    vacc_date_list <- vacc_date_list[-which(as.Date(vacc_date_list) > as.Date(full_data_sansON$e_diagnosis[i], format = "%Y-%m-%d"))]
  }
  full_data_sansON$number_of_vaccines[i] <- length(unique(vacc_date_list))
  if(length(unique(vacc_date_list)) == 0){
    if(is.na(full_data_sansON$n_covid[i])){
      full_data_sansON$number_of_vaccines[i] <- NA
    }
  }
  }

summary(as.factor(full_data_sansON$number_of_vaccines))

data_on$number_of_vaccines <- NA

for(i in 1:length(data_on$number_of_vaccines)){
  vacc_date_list <- c(data_on$H_Immunization_Date1[i],data_on$H_Immunization_Date2[i],data_on$H_Immunization_Date3[i])
  if(length(which(is.na(vacc_date_list))) > 0){
    vacc_date_list <- vacc_date_list[-which(is.na(vacc_date_list))]
  }
  if(length(which(vacc_date_list > as.Date(data_on$C_COVID_diagnosis_date[i]))) > 0){
    vacc_date_list <- vacc_date_list[-which(vacc_date_list > as.Date(data_on$C_COVID_diagnosis_date[i]))]
  }
  print(length(unique(vacc_date_list)))
  data_on$number_of_vaccines[i] <- length(unique(vacc_date_list))
}

summary(as.factor(data_on$number_of_vaccines))


## ok lets go for it
summary(as.factor(full_data_sansON$number_of_vaccines))
summary(as.factor(data_on$number_of_vaccines))


## calculate SGA 10 and SGA 3 for Ontario, NL, and rest of provinces using INTERGROWTH standards

unique(data_on$SGA3)
unique(data_on$SGA10)
unique(data_on$B_infant_weight)

## weight
full_data_sansON$s_bw_gm
data_NL$B1_Weight.in.Kilos

## 



#small for gestational age
############################# make weight vectors #####################
### From: A New and Improved Population-Based Canadian Reference for Birth Weight for Gestational Age, Michael S. Kramer, Robert W. Platt, Shi Wu Wen, K.S. Joseph, Alexander Allen, Michal Abrahamowicz, Béatrice Blondel, Gérard Bréart and for the Fetal/Infant Health Study Group of the Canadian Perinatal Surveillance System Pediatrics 2001;108;e35 DOI: 10.1542/peds.108.2.e35

## ACTUALLY using intergrowth standards

ga.F <- seq(22, 43)
# weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above
# 
# weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above


## first two values in each vector below are totally contrived, these were unavailable for such low gestational age via the intergrowth network
## tenth percentile
weight.F <- c(385, 450, 470, 540, 610, 700, 790, 900, 1010, 1140, 1280, 1410, 1680, 1920, 2140, 2330, 2500, 2650, 2780, 2890, 2980, 3040) # the threshold weight for 10th%ile for each ga above

weight.M <- c(401, 475, 500, 570,650,740,840,950,1070,1210,1360,1430,1710,1950,2180,2380,2570,2730,2880,3010,3120,3210) # the threshold weight for 10th%ile for each ga above

## third percentile
weight.F3 <- c(350, 350, 420, 480, 550, 620, 700, 800, 900, 1020, 1140, 1200, 1470, 1710, 1920, 2110, 2280, 2420, 2550, 2650, 2740, 2800) # the threshold weight for 3rd%ile for each ga above

weight.M3 <- c(400, 400, 440, 510,580,660,750,840,950,1080,1210,1180,1450,1700,1930,2130,2320,2490,2630,2760,2880,2960) # the threshold weight for 3rd%ile for each ga above

# if unknown go with male to be conservative
SGA <- data.frame(ga.F, weight.F, weight.M)

# need to create new column for ga_at_del by calculating it
# calculation referenced in REDCap is 40-(datediff([r_dob],[i_deliverydate_est], "d", "dmy", true)/7)
# started to do this within the csv but many errors...

full_data_sansON$r_dob <- as.Date(as.character(full_data_sansON$r_dob), format = "%Y-%m-%d")

full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)


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
    sga <- c(sga,NA)
  }
  return(sga)
}

full_data_sansON$SGA10 <- sga.fun(full_data_sansON)

full_data_sansON$sga10 <- case_when(
  full_data_sansON$SGA10 == "not SGA" ~ 0,
  full_data_sansON$SGA10 == "SGA" ~ 1
)

data_NL$ga_at_del <- data_NL$Gestation.in.weeks
data_NL$s_bw_gm <- data_NL$B1_Weight.in.Kilos*1000

data_NL$s_pe_genitalia <- case_when(
  data_NL$B1_Gender == "F Female" ~ 2,
  data_NL$B1_Gender == "M Male" ~ 1,
  data_NL$B1_Gender == "Female F" ~ 2,
  data_NL$B1_Gender == "Male M" ~ 1,
  data_NL$B1_Gender == "Female" ~ 2,
  data_NL$B1_Gender == "Male" ~ 1
) 

data_NL$SGA10 <- sga.fun(data_NL)

data_NL$sga10 <- case_when(
  data_NL$SGA10 == "not SGA" ~ 0,
  data_NL$SGA10 == "SGA" ~ 1
)

weight.F <- weight.F3
weight.M <- weight.M3

data_NL$SGA3 <- sga.fun(data_NL)

data_NL$sga3 <- case_when(
  data_NL$SGA3 == "not SGA" ~ 0,
  data_NL$SGA3 == "SGA" ~ 1
)

full_data_sansON$SGA3 <- sga.fun(full_data_sansON)

full_data_sansON$sga3 <- case_when(
  full_data_sansON$SGA3 == "not SGA" ~ 0,
  full_data_sansON$SGA3 == "SGA" ~ 1
)


## assign SGA to full_data_sansON and to data_NL DONE

## to do:
# gestational age / preterm birth - B_GA_birth
# gravidity - gravida
# asthma - 
# hospitalized, icu admission, oxygen - C_ICU_admission, C_hospitalized_COVID, H_Any_Other_Oxygen, H_High_Flow_Nasal_Oxygen_Therapy, oxygen_therapy_flag (infant)
# maternal death - H_Maternal_Death_COVID19, 
# mode of delivery - birth_type_id / birth_type_preg
# pregnancy outcome - pregnancy_outcome_id
# multiple pregnancy  - BIS_num_fetuses_grp
# number of previous preterm births - prev_preterm_births
# number of previous stillbirths - prev_stillbirths
# labour (induced v spontaneous v no labour) - birth_type_id / birth_type_preg
# infant outcomes:
# apgar ( 5 minutes) - B_APGAR_5
# birth weight - B_infant_weight
# NICU admission - NICU_admission_flag
# SGA (small for gestational age) - SGA10, SGA10, SGA3, SGA5
# vaccination status - H_Immunization_Date1, H_Immunization_Date2, H_Immunization_Date3, H_COVID_Vaccine_Name1, H_COVID_Vaccine_Name2, H_COVID_Vaccine_Name3
# vaccination status v preterm birth chi squared table
# plots (see report 6)

#### Fill in Extra PMA Variables ####
# ventilation ## don't bother with NL yet for most of these
unique(full_data_sansON$e_inv) ## invasive mechanical, 0 no, 1 yes, 2 unknown
unique(full_data_sansON$e_noninv) ## non-invasive mechanical, 0 no, 1 yes, 2 unknown
unique(full_data_sansON$e_ecmo) ## e_ecmo, 0 no, 1 yes, 2 unknown

unique(data_on$C_mech_ventilation)

full_data_sansON$ventilation <- case_when(
  full_data_sansON$e_inv == "0" ~ 0,
  full_data_sansON$e_inv == "1" ~ 1,
  full_data_sansON$e_inv == "2" ~ NA,
  full_data_sansON$e_noninv == "0" ~ 0,
  full_data_sansON$e_noninv == "1" ~ 1,
  full_data_sansON$e_noninv == "2" ~ NA,
  full_data_sansON$e_ecmo == "0" ~ 0,
  full_data_sansON$e_ecmo == "1" ~ 1,
  full_data_sansON$e_ecmo == "2" ~ NA
)

data_on$ventilation <- case_when(
  data_on$C_mech_ventilation == "No" ~ 0,
  data_on$C_mech_ventilation == "" ~ 0,
  data_on$C_mech_ventilation == "YES" ~ 1,
  data_on$C_mech_ventilation == "Yes - Non-Invasive Mechanical Ventilation" ~ 1,
  data_on$C_mech_ventilation == "Yes - Invasive Mechanical Ventilation" ~ 1,
  data_on$C_mech_ventilation == "Yes - ECMO" ~ 1
)

## critical care
# (ICU, Ventilation, and/or other high-level treatment (ECMO, Dialysis)) ## see if you can add dialysis
data_on$critical_care <- case_when(
  data_on$ventilation == 1 ~ 1,
  data_on$icu == 1 ~ 1,
  TRUE ~ 0
)

full_data_sansON$critical_care <- case_when(
  full_data_sansON$ventilation == 1 ~ 1,
  full_data_sansON$icu == 1 ~ 1,
  TRUE ~ 0
)

## Pneumonia - pregnant person diagnosed with pneumonia
unique(data_on$C_pneumonia)
unique(full_data_sansON$e_pneumonia)


data_on$pneumonia <- case_when(
  data_on$C_pneumonia == "Yes" ~ 1,
  TRUE ~ 0
)

full_data_sansON$pneumonia <- case_when(
  full_data_sansON$e_pneumonia == "1" ~ 1,
  TRUE ~ 0
)


# Pregnancy-related death - should note that ontario maternal deaths are associated with Covid19
unique(data_on$H_Maternal_Death_COVID19)
unique(full_data_sansON$e_death)


data_on$death <- case_when(
  data_on$H_Maternal_Death_COVID19 == "Yes" ~ 1,
  TRUE ~ 0
)

full_data_sansON$death <- case_when(
  full_data_sansON$e_death == "1" ~ 1,
  TRUE ~ 0
)

# Haemorrhage (check severe outcomes - nothing)
## nothing is showing up for just 'hemorrhage' or 'haemorrhage' except history of..
#full_data_sansON$l_bleed
# 
# 
# unique(full_data_sansON$p_othersx_spec)
# unique(full_data_sansON$p_othersx_spec_2)
unique(full_data_sansON$p_complications_oth_s)
# full_data_sansON$p_complications_oth

full_data_sansON$hemorrhage <- case_when(
  grepl("pph",tolower(full_data_sansON$p_complications_oth_s)) ~ 1,
  grepl("hemorrhage",tolower(full_data_sansON$p_complications_oth_s)) ~ 1,
  TRUE ~ 0
)

sum(full_data_sansON$hemorrhage)
# unique(data_on$ppartum_compl) ##1020140 , 310056
# unique(data_on$lbr_and_birth_complication_id) #1014540

data_on$hemorrhage <- case_when(
  grepl("310056",data_on$ppartum_compl) ~ 1,
  grepl("1014540",data_on$lbr_and_birth_complication_id) ~ 1,
  TRUE ~ 0
)

 sum(data_on$hemorrhage)

# Placental abruption
full_data_sansON$placental_abruption <- case_when(
  full_data_sansON$l_bleed_etiology___1 == 1 ~ 1,
  TRUE ~ 0
) 

data_on$placental_abruption <- case_when(
  grepl("1020300",data_on$complication_id) ~1,
  grepl("1014530",data_on$lbr_and_birth_complication_id) ~ 1,
  TRUE ~ 0
)

sum(data_on$placental_abruption)
sum(full_data_sansON$placental_abruption )


# Hypertensive disorders of pregnancy (ever in current pregnancy)
data_on$htn_preg <- case_when(
  grepl("1020290",data_on$complication_id) ~1, ## gestational hypertension
  data_on$hyper_final == 2 ~ 1,  ## gestational hypertension
  grepl("3100157",data_on$all_ind_for_CS_id) ~ 1, ## gestational hypertension
  grepl("1023780",data_on$all_ind_for_CS_id) ~ 1, ## ecclampsia
  grepl("1023790",data_on$all_ind_for_CS_id) ~ 1, ## hellp
  grepl("1023800",data_on$all_ind_for_CS_id) ~ 1, ## pre ecclampsia
  grepl("1014330",data_on$all_ind_for_lbr_induct_id) ~ 1, ## pre ecclampsia
  
  TRUE ~ 0
)

sum(data_on$htn_preg)

full_data_sansON$htn_preg <- case_when(
  full_data_sansON$l_gh == "1" ~ 1,
  full_data_sansON$l_pree ==  "1" ~ 1, ## pre eclampsia
  full_data_sansON$l_htn ==  "1" ~ 1, ## hypertensive disorders of pregnancy
  TRUE ~ 0
)

sum(full_data_sansON$htn_preg)

## hypertensive disorders of pregnancy on or after diagnosis of COVID 19
data_on$htn_preg_after <- case_when(
  grepl("1020290",data_on$complication_id) ~1, ## gestational hypertension
  data_on$hyper_final == 2 ~ 1,  ## gestational hypertension
  grepl("3100157",data_on$all_ind_for_CS_id) ~ 1, ## gestational hypertension
  grepl("1023780",data_on$all_ind_for_CS_id) ~ 1, ## ecclampsia
  grepl("1023790",data_on$all_ind_for_CS_id) ~ 1, ## hellp
  grepl("1023800",data_on$all_ind_for_CS_id) ~ 1, ## pre ecclampsia
  grepl("1014330",data_on$all_ind_for_lbr_induct_id) ~ 1, ## pre ecclampsia
  TRUE ~ 0
)


full_data_sansON$l_htn_date[which(full_data_sansON$l_htn_date == "9999-09-09")] <- NA

full_data_sansON$htn_preg_after <- case_when(
  full_data_sansON$l_gh == "1" ~ 1,
  full_data_sansON$l_pree ==  "1" ~ 1, ## pre eclampsia
  full_data_sansON$l_htn ==  "1" & (as.Date(full_data_sansON$l_htn_date) > full_data_sansON$e_diagnosis) ~ 1, ## hypertensive disorders of pregnancy
  TRUE ~ 0
)

sum(full_data_sansON$htn_preg_after)
sum(full_data_sansON$htn_preg)
sum(data_on$htn_preg)

## htn_preg_after may not be available for Ontario data because date of hypertensive event not recorded?


## PRE ECLAMPSIA AND ECLAMPSIA !! what to do here? Seems that we do not actually have instances of ECLAMPSIA in data dictionary for main CANCOVID Preg Data

# unique(full_data_sansON$g_othersx_spec)
# unique(full_data_sansON$g_othersx_spec2)
# unique(full_data_sansON$g_othersx_spec_2)
# unique(full_data_sansON$g_othersx_spec_22)


full_data_sansON$pre_eclampsia <- case_when(
  full_data_sansON$v_issues___3 == "1" ~ 1,
  grepl("pre eclampsia",tolower(full_data_sansON$p_complications_oth_s)) ~ 1,
  grepl("preeclampsia",tolower(full_data_sansON$p_complications_oth_s)) ~ 1,
  grepl("eclampsia",tolower(full_data_sansON$p_complications_oth_s)) ~ 1,
  TRUE ~ 0
)

data_on$pre_eclampsia <- case_when(
  grepl("1014330",data_on$all_ind_for_lbr_induct_id) ~ 1, ## pre ecclampsia
  grepl("1023800",data_on$all_ind_for_CS_id) ~ 1, ## pre ecclampsia
  TRUE ~ 0
)

sum(data_on$pre_eclampsia)    # these numbers are drastically different, probably shouldn't use them
sum(full_data_sansON$pre_eclampsia)

### embolic disease (including dvt)

full_data_sansON$dvt <- case_when(
  full_data_sansON$j_hem_dvt___1 == "1" ~ TRUE,
  TRUE ~ FALSE
)


full_data_sansON$pulm_emb <- case_when(
  grepl("ruled out",full_data_sansON$e_compoth_specify, ignore.case = TRUE) ~ FALSE,
  grepl("ruled out",full_data_sansON$e_compoth_specify_2, ignore.case = TRUE) ~ FALSE,
  grepl("ruled out",full_data_sansON$e_compoth_specify_3, ignore.case = TRUE) ~ FALSE,
  grepl("ruled out",full_data_sansON$e_compoth_specify_4, ignore.case = TRUE) ~ FALSE,
  grepl("ruled out",full_data_sansON$e_compoth_specify_5, ignore.case = TRUE) ~ FALSE,
  grepl("pulmonary embolism",full_data_sansON$e_compoth_specify, ignore.case = TRUE) ~ TRUE,
  grepl("pulmonary embolism",full_data_sansON$e_compoth_specify_2, ignore.case = TRUE) ~ TRUE,
  grepl("pulmonary embolism",full_data_sansON$e_compoth_specify_3, ignore.case = TRUE) ~ TRUE,
  grepl("pulmonary embolism",full_data_sansON$e_compoth_specify_4, ignore.case = TRUE) ~ TRUE,
  grepl("thombosis",full_data_sansON$e_compoth_specify, ignore.case = TRUE) ~ TRUE,
  grepl("thombosis",full_data_sansON$e_compoth_specify_2, ignore.case = TRUE) ~ TRUE,
  grepl("thombosis",full_data_sansON$e_compoth_specify_3, ignore.case = TRUE) ~ TRUE,
  grepl("thombosis",full_data_sansON$e_compoth_specify_4, ignore.case = TRUE) ~ TRUE,
  TRUE ~ FALSE
)

data_on$pulm_emb <- case_when(
  grepl("1020150",data_on$ppartum_compl) ~ TRUE,
  grepl("1014545",data_on$lbr_and_birth_complication_id) ~ TRUE,
  TRUE ~ FALSE
)

data_on$dvt <- case_when(
  grepl("1020160",data_on$ppartum_compl) ~ TRUE,
  TRUE ~ FALSE
)



data_on$embolic_disease <- case_when(
  data_on$dvt ~ 1,
  data_on$pulm_emb ~ 1,
  TRUE ~ 0
)

full_data_sansON$embolic_disease <- case_when(
  full_data_sansON$dvt ~ 1,
  full_data_sansON$pulm_emb ~ 1,
  TRUE ~ 0
)

sum(full_data_sansON$embolic_disease )
sum(data_on$embolic_disease )

## preterm labour
full_data_sansON$ga_del_cat2
unique(data_on$ga_del_cat2)

## preterm labour with other denominator (only cases with gestational age at onset of covid < 37 weeks), we don't have this information yet for ontario
full_data_sansON$covid_pre_37 <- full_data_sansON$ga_at_del - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis))/7.0 < 37
# data_on$covid_pre_37 <- full_data_sansON$ga_at_del - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis))/7.0 < 37




### FOR C-SECTION AND INTRAPARTUM C-SECTION, EXCLUDE PREGNANCY LOSS, ONLY STILL BIRTHS AND LIVE BIRTHS
## c-section
full_data_sansON$mode_del
unique(data_on$mode_del)


## intrapartum c-section (unscheduled only)
unique(full_data_sansON$p_cs_type) ## 2 = urgent 3 = emergent
unique(data_on$mode_del2)

full_data_sansON$intrapartum_cs <- case_when(
  full_data_sansON$p_cs_type == 2 ~ 1,
  full_data_sansON$p_cs_type == 3 ~ 1,
  TRUE ~ 0
)

data_on$intrapartum_cs <- case_when(
  data_on$mode_del2 == "induced or spontaneous labour cs" ~ 1,
  TRUE ~ 0
)


## stillbirth
length(unique(data_on$new_preg_id))
dim(data_on)

## stillbirth
sum(full_data_sansON$preg_outcome == "stillbirth", na.rm = TRUE)
sum(data_on$preg_outcome == "stillbirth", na.rm = TRUE)

full_data_sansON$stillbirth <- case_when(
  full_data_sansON$preg_outcome == "stillbirth" ~ 1,
  TRUE ~ 0
)

data_on$stillbirth <- case_when(
  data_on$preg_outcome == "stillbirth" ~ 1,
  TRUE ~ 0
)

## DUPLICATE DATA FOR EACH INFANT FOR INFANT OUTCOMES. INFANT OUTCOMES only include live births except still birth and perinatal death (which should include still births)
# Still birth, Perinatal Death, Early Neonatal Death, Neonatal Death, NICU admission at birth, Very low birth weight (<1500 g), low birth weight (<2500g), SGA3, SGA10, 
# Very preterm (<34 week), Preterm (<37 week), Very preterm (using restricted denominator), preterm (using restricted denominator)
## columns in infant data with twins:


## first create duplicate rows for twins and triplets
# full_data_sansON$i_fetusnumber[which(full_data_sansON$i_fetusnumber == "999")] <- NA
full_data_sansON$i_fetusnumber <- as.numeric(full_data_sansON$i_fetusnumber)

## make empty data frame with row for each infant
full_data_sansON_wtwins <- as.data.frame(matrix(data = NA, nrow = sum(full_data_sansON$i_fetusnumber), ncol = 12))

colnames(full_data_sansON_wtwins) <- c("a_record_id","fetus_number","s_pe_genitalia","e_diagnosis","s_infdeath","aa_age","NICU","birthweight", "SGA3", "SGA10","ga_at_del","stillbirth")


## populate the twins data frame
i <- 1
j <- 0
while(i <= dim(full_data_sansON)[1]){
  if(full_data_sansON$i_fetusnumber[i] == 1){
    full_data_sansON_wtwins$a_record_id[i+j] <- full_data_sansON$a_record_id[i]
    full_data_sansON_wtwins$e_diagnosis[i+j] <- full_data_sansON$e_diagnosis[i]
    full_data_sansON_wtwins$birthweight[i+j] <- full_data_sansON$s_bw_gm[i]
    full_data_sansON_wtwins$s_infdeath[i+j] <- full_data_sansON$s_infdeath[i]
    full_data_sansON_wtwins$aa_age[i+j] <- full_data_sansON$aa_age[i]
    full_data_sansON_wtwins$ga_at_del[i+j] <- full_data_sansON$ga_at_del[i]
    full_data_sansON_wtwins$NICU[i+j] <- full_data_sansON$s_nicu[i]
    full_data_sansON_wtwins$s_pe_genitalia[i+j] <- full_data_sansON$s_pe_genitalia[i]
    full_data_sansON_wtwins$stillbirth[i+j] <- full_data_sansON$preg_outcome[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 1
    i <- i + 1
  }
  else if(full_data_sansON$i_fetusnumber[i] == 2){
    full_data_sansON_wtwins$a_record_id[i+j] <- full_data_sansON$a_record_id[i]
    full_data_sansON_wtwins$e_diagnosis[i+j] <- full_data_sansON$e_diagnosis[i]
    full_data_sansON_wtwins$birthweight[i+j] <- full_data_sansON$s_bw_gm[i]
    full_data_sansON_wtwins$s_infdeath[i+j] <- full_data_sansON$s_infdeath[i]
    full_data_sansON_wtwins$aa_age[i+j] <- full_data_sansON$aa_age[i]
    full_data_sansON_wtwins$ga_at_del[i+j] <- full_data_sansON$ga_at_del[i]
    full_data_sansON_wtwins$NICU[i+j] <- full_data_sansON$s_nicu[i]
    full_data_sansON_wtwins$s_pe_genitalia[i+j] <- full_data_sansON$s_pe_genitalia[i]
    full_data_sansON_wtwins$stillbirth[i+j] <- full_data_sansON$preg_outcome[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 1
    j <- j + 1
    full_data_sansON_wtwins$a_record_id[i+j] <- full_data_sansON$a_record_id[i]
    full_data_sansON_wtwins$e_diagnosis[i+j] <- full_data_sansON$e_diagnosis[i]
    full_data_sansON_wtwins$birthweight[i+j] <- full_data_sansON$s_bw_gm_2[i]
    full_data_sansON_wtwins$s_infdeath[i+j] <- full_data_sansON$s_infdeath_2[i]
    full_data_sansON_wtwins$aa_age[i+j] <- full_data_sansON$aa_age_2[i]
    full_data_sansON_wtwins$ga_at_del[i+j] <- full_data_sansON$ga_at_del[i]
    full_data_sansON_wtwins$NICU[i+j] <- full_data_sansON$s_nicu_2[i]
    full_data_sansON_wtwins$s_pe_genitalia[i+j] <- full_data_sansON$s_pe_genitalia_2[i]
    full_data_sansON_wtwins$stillbirth[i+j] <- full_data_sansON$preg_outcome[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 2
    i <- i + 1
  }
  else if(full_data_sansON$i_fetusnumber[i] == 3){
    full_data_sansON_wtwins$a_record_id[i+j] <- full_data_sansON$a_record_id[i]
    full_data_sansON_wtwins$e_diagnosis[i+j] <- full_data_sansON$e_diagnosis[i]
    full_data_sansON_wtwins$birthweight[i+j] <- full_data_sansON$s_bw_gm[i]
    full_data_sansON_wtwins$s_infdeath[i+j] <- full_data_sansON$s_infdeath[i]
    full_data_sansON_wtwins$aa_age[i+j] <- full_data_sansON$aa_age[i]
    full_data_sansON_wtwins$ga_at_del[i+j] <- full_data_sansON$ga_at_del[i]
    full_data_sansON_wtwins$NICU[i+j] <- full_data_sansON$s_nicu[i]
    full_data_sansON_wtwins$s_pe_genitalia[i+j] <- full_data_sansON$s_pe_genitalia[i]
    full_data_sansON_wtwins$stillbirth[i+j] <- full_data_sansON$preg_outcome[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 1
    j <- j + 1
    full_data_sansON_wtwins$a_record_id[i+j] <- full_data_sansON$a_record_id[i]
    full_data_sansON_wtwins$e_diagnosis[i+j] <- full_data_sansON$e_diagnosis[i]
    full_data_sansON_wtwins$birthweight[i+j] <- full_data_sansON$s_bw_gm_2[i]
    full_data_sansON_wtwins$s_infdeath[i+j] <- full_data_sansON$s_infdeath_2[i]
    full_data_sansON_wtwins$aa_age[i+j] <- full_data_sansON$aa_age_2[i]
    full_data_sansON_wtwins$ga_at_del[i+j] <- full_data_sansON$ga_at_del[i]
    full_data_sansON_wtwins$NICU[i+j] <- full_data_sansON$s_nicu_2[i]
    full_data_sansON_wtwins$s_pe_genitalia[i+j] <- full_data_sansON$s_pe_genitalia_2[i]
    full_data_sansON_wtwins$stillbirth[i+j] <- full_data_sansON$preg_outcome[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 2
    j <- j + 1
    full_data_sansON_wtwins$a_record_id[i+j] <- full_data_sansON$a_record_id[i]
    full_data_sansON_wtwins$e_diagnosis[i+j] <- full_data_sansON$e_diagnosis[i]
    full_data_sansON_wtwins$birthweight[i+j] <- full_data_sansON$s_bw_gm_3[i]
    full_data_sansON_wtwins$s_infdeath[i+j] <- full_data_sansON$s_infdeath_3[i]
    full_data_sansON_wtwins$aa_age[i+j] <- full_data_sansON$aa_age_3[i]
    full_data_sansON_wtwins$ga_at_del[i+j] <- full_data_sansON$ga_at_del[i]
    full_data_sansON_wtwins$NICU[i+j] <- full_data_sansON$s_nicu_3[i]
    full_data_sansON_wtwins$s_pe_genitalia[i+j] <- full_data_sansON$s_pe_genitalia_3[i]
    full_data_sansON_wtwins$stillbirth[i+j] <- full_data_sansON$preg_outcome[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 3
    i <- i + 1
  }
}

## quality_check: 
# full_data_sansON[which(full_data_sansON$a_record_id == "CCP-01-1261"),c("e_diagnosis","aa_age","s_bw_gm","s_bw_gm_2","ga_at_del", "preg_outcome")]



### ACCOUNT FOR TWINS!! Make sure denominator for neonatal outcomes includes twins
## perinatal death - stillbirths at or after 28 weeks gestational age AND early neonatal deaths occurring at less than 7 days of life
## early neonatal death-  death at less than 7 days of life
## neonatal death - death at less than 28 days of life

full_data_sansON

## neonatal death
data_on$neonatal_death <- grepl("1018200",data_on$neonatal_death_id)
sum(data_on$neonatal_death)

full_data_sansON$neonatal_death <- case_when(
  full_data_sansON$s_infdeath == "1" ~ 1,
  TRUE ~ 0
) 

sum(full_data_sansON$neonatal_death)




#### SUMMARISE BY PROVINCE AND DATE RANGE #####
full_data_sansON %>% group_by(prov) %>% summarise(n = length(unique(a_record_id)), min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))

# full_data_sansON_duplicated %>% group_by(prov) %>% summarise(n = length(a_record_id), min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))
# full_data_sansON_duplicated$e_diagnosis <- as.Date(full_data_sansON_duplicated$e_diagnosis)

data_on %>% summarise(n=n(),min_date = min(as.Date(C_COVID_diagnosis_date, format = "%Y-%m-%d"), na.rm = TRUE), 
                      max_date = max(as.Date(C_COVID_diagnosis_date, format = "%Y-%m-%d"), na.rm = TRUE))


full_data_sansON$e_diagnosis <- as.Date(full_data_sansON$e_diagnosis)

full_data_sansON$a_date <- as.Date(full_data_sansON$a_date)
# data_on$C_COVID_diagnosis_date

partial_data_sansON_1 <- full_data_sansON[,c("age_cat2","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","e_diagnosis","prov","ga_del_cat","ga_del_cat2","gravida","oxygen","mode_del","preg_outcome","multiple_preg","history_preterm","labour_type","apgar5","bw_cat","number_of_vaccines","sga3","sga10","ventilation","critical_care","pneumonia","death","hemorrhage","placental_abruption","htn_preg","htn_preg_after","pre_eclampsia","embolic_disease","mode_del","intrapartum_cs","stillbirth","neonatal_death")] 
# partial_data_sansON_2 <- full_data_sansON_duplicated[,c("age_cat2","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","e_diagnosis","prov")] 

data_on$prov <- "ON"

partial_data_ON <- data_on[,c("C_maternal_age_group","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","C_COVID_diagnosis_date","prov","ga_del_cat","ga_del_cat2","gravida","oxygen","mode_del","preg_outcome","multiple_preg","history_preterm","labour_type","apgar5","bw_cat","number_of_vaccines","SGA3","SGA10","ventilation","critical_care","pneumonia","death","hemorrhage","placental_abruption","htn_preg","htn_preg_after","pre_eclampsia","embolic_disease","mode_del","intrapartum_cs","stillbirth","neonatal_death")]
 colnames(partial_data_ON)[which(colnames(partial_data_ON) == "C_maternal_age_group")] <- "age_cat2"      
 colnames(partial_data_ON)[which(colnames(partial_data_ON) == "C_COVID_diagnosis_date")] <- "e_diagnosis"      
 colnames(partial_data_ON)[which(colnames(partial_data_ON) == "SGA10")] <- "sga10"      
 colnames(partial_data_ON)[which(colnames(partial_data_ON) == "SGA3")] <- "sga3"      
 
partial_data_ON$e_diagnosis <- as.Date(partial_data_ON$e_diagnosis)
 
data_on$YEAR <- format(as.Date(data_on$C_COVID_diagnosis_date),"%Y")

data_on %>% group_by(YEAR) %>% summarise(n=n())


# partial_data_ON %>% filter(e_diagnosis < as.Date("2020-03-01"))

## fix NL data some more

data_NL$e_diagnosis <- as.Date(data_NL$MT_COLLECTION_DT, format = "%d-%b-%y")
data_NL$age_cat2 <- case_when(
  data_NL$AGE < 30 ~ "Less than 30",
  data_NL$AGE < 36 ~ "30-35 years",
  data_NL$AGE >= 36 ~ "36 years and older",
  TRUE ~ "missing"
) ###### FIX THIS ONE
data_NL$BMI_cat
data_NL$htn
data_NL$diabetes
data_NL$eth_cat <- NA
data_NL$hospitalization  
data_NL$icu  
data_NL$NICU  
data_NL$prov  
data_NL$ga_del_cat  
data_NL$ga_del_cat2  
data_NL$gravida  <- data_NL$Gravida  ## FIX THIS
data_NL$oxygen   <- NA ### perhaps fix this?
data_NL$mode_del  
data_NL$preg_outcome  
data_NL$multiple_preg  
data_NL$history_preterm  
data_NL$labour_type  
data_NL$apgar5  ## This one is iffy
data_NL$bw_cat  
data_NL$number_of_vaccines <- NA
# data_NL$sga2 ## calculate these
data_NL$sga10 ## calculate these
data_NL$sga3 ## calculate these

colnames(partial_data_ON)

data_NL_partial <- data_NL[,colnames(partial_data_ON)]

write.csv(full_data_sansON, here("non-ontario data.csv"))
write.csv(partial_data_ON, here("partial ontario data.csv"))

##### FOR PMA WE NEED THE FOLLOWING #####

# Outcomes: 
# ICU admission
unique(combined_data$icu)
# Critical care
unique(combined_data$icu)
unique(combined_data$NICU)
# Ventilation

# Pneumonia

# Pregnancy-related death

# Hemorrhage

# Placental abruption

# Hypertensive disorders of pregnancy

# Preeclampsia/eclampsia

# Preterm labor ?
unique(combined_data$ga_del_cat2)
# C-section (any)
unique(combined_data$mode_del)

# Intrapartum c-section 

# Stillbirth at/after 28 weeks GA
unique(combined_data$preg_outcome)
unique(combined_data$ga_del_cat)

# Perinatal death

# Neonatal death/early neonatal death

# NICU admission at birth
unique(combined_data$NICU)
# Preterm birth
unique(combined_data$ga_del_cat2)
# Low birthweight (<2500g or <1500g)
unique(combined_data$bw_cat)

# Small for gestational age (<10th or <3rd percentile using INTERGROWTH standards)
unique(combined_data$sga3)
unique(combined_data$sga10)

# Risk Factors: 
# Maternal comorbidities:
# Diabetes (preexisting prior to pregnancy)
unique(combined_data$diabetes)
# Hypertension (preexisting prior to pregnancy)
unique(combined_data$htn)


# Cardiovascular disease
# HIV
# Nutrition-related
# Pre-pregnancy BMI 
# Anemia at the time of COVID-19 diagnosis (defined as Hb < 110)
# Other risk factors
# Maternal age
# Primiparity 
# TB coinfection
# Syphliis coinfection
# Malaria coinfection






# write.csv(full_data_sansON,"full_data_sansON_06_13_2023.csv")
# write.csv(data_on,"data_on_06_13_2023.csv")
# 
# full_data_sansON <- read.csv("full_data_sansON_06_13_2023.csv", header = TRUE)
# data_on <- read.csv("data_on_06_13_2023.csv", header = TRUE)

combined_data <- rbind(partial_data_sansON_1,partial_data_ON) 
# combined_data <- rbind(combined_data,data_NL_partial) 

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

combined_data <- combined_data %>% filter(e_diagnosis < as.Date("2023-01-01"))
combined_data <- combined_data %>% filter(e_diagnosis > as.Date("2020-03-01"))


combined_data %>% group_by(prov) %>% summarise(n = n())

combined_data %>% group_by(prov) %>% summarise(n = n(), min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))


denominator1 <- sum(!is.na(combined_data$age_cat2)) - sum(combined_data$age_cat2 == "missing", na.rm = TRUE)
c1 <- combined_data %>% group_by(age_cat2) %>% summarise(n = n(),n_p = n()/denominator1,pre_omicron = sum(covid_period != "Omicron", na.rm = TRUE), omicron = sum(covid_period == "Omicron", na.rm = TRUE), hospitalized = sum(hospitalization, na.rm = TRUE),not_hospitalized = sum(hospitalization == 0, na.rm = TRUE))
denominator2 <- sum(!is.na(combined_data$BMI_cat))
c2 <- combined_data %>% group_by(BMI_cat) %>% summarise(n = n(), n_p = 100*n()/denominator2, pre_omicron = sum(covid_period != "Omicron", na.rm = TRUE), omicron = sum(covid_period == "Omicron", na.rm = TRUE), hospitalized = sum(hospitalization, na.rm = TRUE),not_hospitalized = sum(hospitalization == 0, na.rm = TRUE))

combined_data$htn[which(combined_data$htn == "No")] <- 0
combined_data$htn[which(combined_data$htn == "Yes")] <- 1

denominator3 <- sum(!is.na(combined_data$htn))
c3 <- combined_data %>% group_by(htn) %>% summarise(n = n(), pre_omicron = sum(covid_period != "Omicron", na.rm = TRUE), omicron = sum(covid_period == "Omicron", na.rm = TRUE), hospitalized = sum(hospitalization, na.rm = TRUE),not_hospitalized = sum(hospitalization == 0, na.rm = TRUE))

combined_data$diabetes[which(combined_data$diabetes == "No")] <- 0
combined_data$diabetes[which(combined_data$diabetes == "Yes")] <- 1

denominator4 <- sum(!is.na(combined_data$diabetes))
c4 <- combined_data %>% group_by(diabetes) %>% summarise(n = n(), pre_omicron = sum(covid_period != "Omicron", na.rm = TRUE), omicron = sum(covid_period == "Omicron", na.rm = TRUE), hospitalized = sum(hospitalization, na.rm = TRUE),not_hospitalized = sum(hospitalization == 0, na.rm = TRUE))

denominator5 <- sum(!is.na(combined_data$eth_cat))
c5 <- combined_data %>% group_by(eth_cat) %>% summarise(n = n(),  pre_omicron = sum(covid_period != "Omicron", na.rm = TRUE), omicron = sum(covid_period == "Omicron", na.rm = TRUE), hospitalized = sum(hospitalization, na.rm = TRUE),not_hospitalized = sum(hospitalization == 0, na.rm = TRUE))

denominator6 <- sum(!is.na(combined_data$ga_del_cat))
c6 <- combined_data %>% group_by(ga_del_cat) %>% summarise(n = n(),n_percent = n()/denominator6)

#gravida
combined_data$gravida2 <- case_when(
  combined_data$gravida < 2 ~ "1",
  combined_data$gravida >= 2 ~ "2+"
)
denominator7 <- sum(!is.na(combined_data$gravida2))
c7 <- combined_data %>% group_by(gravida2) %>% summarise(n = n(),n_percent = n()/denominator7)

denominator8 <- sum(!is.na(combined_data$htn))
c8 <- combined_data %>% group_by(htn) %>% summarise(n = n(),n_percent = n()/denominator8)


denominator9 <- sum(!is.na(combined_data$diabetes))
c9 <- combined_data %>% group_by(diabetes) %>% summarise(n = n(),n_percent = n()/denominator9)


denominator10 <- sum(!is.na(combined_data$multiple_preg))
c10 <- combined_data %>% group_by(multiple_preg) %>% summarise(n = n(),n_percent = n()/denominator10)

denominator11 <- sum(!is.na(combined_data$preg_outcome))
c11 <- combined_data %>% group_by(preg_outcome) %>% summarise(n = n(),n_percent = n()/denominator11)


combined_dataSANS_ON <- combined_data %>% filter(prov != "ON")

denominator11_2 <- sum(!is.na(combined_dataSANS_ON$preg_outcome))
c11_2 <- combined_dataSANS_ON %>% group_by(preg_outcome) %>% summarise(n = n(),n_percent = n()/denominator11)

denominator12 <- sum(!is.na(combined_data$mode_del))
c12 <- combined_data %>% group_by(mode_del) %>% summarise(n = n(),n_percent = n()/denominator12)

denominator13 <- sum(!is.na(combined_data$labour_type))
c13 <- combined_data %>% group_by(labour_type) %>% summarise(n = n(),n_percent = n()/denominator13)


denominator14 <- sum(!is.na(combined_data$apgar5))
c14 <- combined_data %>% group_by(apgar5) %>% summarise(n = n(),n_percent = n()/denominator14)

denominator15 <- sum(!is.na(combined_data$bw_cat))
c15 <- combined_data %>% group_by(bw_cat) %>% summarise(n = n(),n_percent = n()/denominator15)

denominator16 <- sum(!is.na(combined_data$NICU))
c16 <- combined_data %>% group_by(NICU) %>% summarise(n = n(),n_percent = n()/denominator16)

denominator17 <- sum(!is.na(combined_data$sga2))
c17 <- combined_data %>% group_by(sga2) %>% summarise(n = n(),n_percent = n()/denominator17)


denominator18 <- sum(!is.na(combined_data$covid_period))
denominator19 <- sum(!is.na(combined_data$ga_del_cat2))

combined_data %>% group_by(covid_period) %>% summarise(n = n(), n_percent = n()/denominator18,
                                                       preterm = sum(ga_del_cat2 == "preterm", na.rm = TRUE), pre_perc = sum(ga_del_cat2 == "preterm", na.rm = TRUE)/sum(!is.na(ga_del_cat2)),
                                                       denom_pre = sum(!is.na(ga_del_cat2)),
                                                       nicu = sum(NICU,na.rm = TRUE), nicu_prop =sum(NICU,na.rm = TRUE)/sum(!is.na(NICU)),
                                                       denom_nicu = sum(!is.na(NICU)))

denominator20 <- sum(!is.na(combined_data$ga_del_cat2))

combined_data$icu[is.na(combined_data$icu)] <- 0
combined_data %>% group_by(covid_period) %>% summarise(n = n(), n_percent = n()/denominator18,
                                                       hosp = sum(hospitalization, na.rm = TRUE), hosp_perc = sum(hospitalization, na.rm = TRUE)/sum(!is.na(hospitalization)),
                                                       denom_pre = sum(!is.na(hospitalization)),
                                                       ICU_add = sum(icu,na.rm = TRUE), icu_prop =sum(icu,na.rm = TRUE)/sum(!is.na(icu)),
                                                       denom_icu = sum(!is.na(icu)))

describeFactors(combined_data$ga_del_cat2)
###First Plot replicate
library(chron)

combined_data$e_diagnosis <- as.Date(combined_data$e_diagnosis)
combined_data$month <- cut(combined_data$e_diagnosis, breaks = "months")
combined_data$week <- cut(combined_data$e_diagnosis, breaks = "weeks")

combined_data$month <- as.Date(combined_data$month)


ggplot(combined_data, aes(x = month, y = hospitalization)) +
  geom_col()


combined_data$e_hosp.f <- factor(combined_data$hospitalization)

time.hosp <- combined_data %>% 
  group_by(month, e_hosp.f, .drop = FALSE) %>% 
  dplyr::summarise(n = n())
# time.hosp <- time.hosp[-which(is.na(time.hosp$e_hosp.f)), ]

combined_data$g_icu.f <- factor(combined_data$icu)

combined_data$g_icu.f[is.na(combined_data$g_icu.f)] <- 0

time.icu <- combined_data %>% 
  group_by(month, g_icu.f, .drop = FALSE) %>% 
  dplyr::summarise(n = n())

# time.icu <- time.icu[-which(is.na(time.icu$g_icu.f)), ]

combined_data$suppO2_hosp.f <- factor(combined_data$oxygen)
time.o2 <- combined_data %>% 
  group_by(month, suppO2_hosp.f, .drop = FALSE) %>% 
  dplyr::summarise(n = n())
time.o2 <- time.o2[-which(is.na(time.o2$suppO2_hosp.f)), ]

time.hosp <- time.hosp %>% 
  group_by(month) %>% 
  mutate(prop = n/(sum(n)),
         tot = sum(n))

time.hosp <- time.hosp %>% 
  rowwise() %>% 
  mutate(LL = prop.test(n, tot)$conf.int[1],
         UL = prop.test(n, tot)$conf.int[2])

time.o2 <- time.o2 %>% 
  group_by(month) %>% 
  mutate(prop = n/(sum(n)),
         tot = sum(n))

time.o2$prop[which(is.nan(time.o2$prop))] <- 1
# time.o2$n[which(time.o2$n == 0)] <- 1
# time.o2$prop[which(time.o2$prop == 0)] <- 1



time.o2 <- time.o2 %>% 
  rowwise() %>% 
  mutate(LL = prop.test(n, tot)$conf.int[1],
         UL = prop.test(n, tot)$conf.int[2])

time.o2$n

time.icu <- time.icu %>% 
  group_by(month) %>% 
  mutate(prop = n/(sum(n)),
         tot = sum(n))

time.icu <- time.icu[-which(time.icu$month < "2020-03-01"), ]

time.icu <- time.icu %>% 
  rowwise() %>% 
  mutate(LL = prop.test(n, tot)$conf.int[1],
         UL = prop.test(n, tot)$conf.int[2])


## plot all together

# Based on Canadian epidemiology, the first confirmed case of the Alpha variant (B.1.1.7) was identified on December 25th, 2020. The first confirmed cases of the Delta variant (B.1.617.2) were identified on April 21st, 2021 and shortly thereafter it became the dominant variant until November/December 2021.20 
# National Collaborating Centre for Infectious Diseases. Updates on COVID-19 Variants of Concern. 2021; https://nccid.ca/covid-19-variants/. Accessed 17 October 2021.

## the months are for the beginning of the months, but should be the ends
time.hosp$month2 <- time.hosp$month + 0
time.o2$month2 <- time.o2$month + 0
time.icu$month2 <- time.icu$month + 0

# time.hosp <- time.hosp[-which(time.hosp$month < "2020-03-01"), ]
# time.hosp <- time.hosp[-which(time.hosp$month > "2021-10-01"), ]

time.o2 <- time.o2[-which(time.o2$month < "2020-03-01"), ]
# time.o2 <- time.o2[-which(time.o2$month > "2021-10-01"), ]

# time.icu <- time.icu[-which(time.icu$month < "2020-03-01"), ]
# time.icu <- time.icu[-which(time.icu$month > "2021-10-01"), ]

## get the case numbers by month
cases <- time.hosp %>% 
  group_by(month2) %>% 
  summarise(cases = sum(n))

quartz(width = 12, height = 5)

g1 <- ggplot() +
  annotate("rect", xmin = as.Date(c("2020-12-25")), xmax = as.Date(c("2021-04-04")), ymin = -Inf, ymax = 0.22,alpha = .2) +
  annotate("rect", xmin = as.Date(c("2021-04-05")), xmax = as.Date(c("2021-12-18")), ymin = -Inf, ymax = 0.22,alpha = .2, fill = "orange") +
  annotate("rect", xmin = as.Date(c("2021-12-19")), xmax = as.Date(c("2022-9-30")), ymin = -Inf, ymax = 0.22,alpha = .2, fill = "blue") +
  geom_errorbar(time.hosp[which(time.hosp$e_hosp.f == 1), ], mapping = aes(x = month2, ymin = LL, ymax = UL), width = 0.2, position = position_nudge(x = 4)) +
  # geom_errorbar(time.o2[which(time.o2$suppO2_hosp.f == "Yes"), ], mapping = aes(x = month2, ymin = LL, ymax = UL), width = 0.2) +
  geom_errorbar(time.icu[which(time.icu$g_icu.f == 1), ], mapping = aes(x = month2, ymin = LL, ymax = UL), width = 0.2, position = position_nudge(x = -4)) +
  geom_line(time.hosp[which(time.hosp$e_hosp.f == 1), ], mapping = aes(x = month2, y = prop, col = ghibli_palette("PonyoMedium")[3]), size = 1.05, position = position_nudge(x = 4)) +
  #geom_line(time.o2[which(time.o2$suppO2_hosp.f == "Yes"), ], mapping = aes(x = month2, y = prop, col = ghibli_palette("PonyoMedium")[4]), size = 1.05) +
  geom_line(time.icu[which(time.icu$g_icu.f == 1), ], mapping = aes(x = month2, y = prop, col = ghibli_palette("TotoroDark")[1]), size = 1.05, position = position_nudge(x = -4)) +
  geom_point(time.hosp[which(time.hosp$e_hosp.f == 1), ], mapping = aes(x = month2, y = prop, col = ghibli_palette("PonyoMedium")[3]), size = 2.5, position = position_nudge(x = 4)) +
  #geom_point(time.o2[which(time.o2$suppO2_hosp.f == "Yes"), ], mapping = aes(x = month2, y = prop, col = ghibli_palette("PonyoMedium")[4]), size = 2.5) +
  geom_point(time.icu[which(time.icu$g_icu.f == 1), ], mapping = aes(x = month2, y = prop, col = ghibli_palette("TotoroDark")[1]), size = 2.5, position = position_nudge(x = -4)) +
  theme_pubr() +
  theme(panel.grid.major.y = element_line(linetype = 2, colour = "black"), 
        text = element_text(size = 12),
        plot.margin = margin(0.5, 0.5, 1.5, 0.5, "cm"),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-0.5, 0.5)) +
  scale_colour_identity(guide = legend, breaks = c(ghibli_palette("PonyoMedium")[3], ghibli_palette("PonyoMedium")[4], ghibli_palette("TotoroDark")[1]), labels = c("Hospitalization", "O2 therapy", "ICU/high acuity")) +
  guides(colour = guide_legend(title = "")) +
  ylab("Percent of cases per month") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y", limits = as.Date(c("2020-03-01", "2022-9-30"))) +
  coord_cartesian(ylim = c(0, 0.22), clip = "off", expand = F, xlim = as.Date(c("2020-02-15", "2022-9-30"))) +
  xlab("") +
  annotate(geom = "text", x = cases$month2, y = -0.15, label = cases$cases) +
  annotate("text", x = as.Date("2020-03-01"), y = -0.15, label = "Total SARS-CoV-2\ncases by month:", hjust = 1.2, size = 3) +
  annotate("text", x = as.Date(c("2021-02-01", "2021-07-30", "2022-02-01")), y = 0.20, label = c("Alpha\nB.1.1.7", "Delta\nB.1.617.2", "Omicron\nB.1.1.529"), hjust = 0.2, size = 5)


ggsave("g1.jpeg")



##### TWO NEW PLOTS ####
time.icu.wide <- reshape(as.data.frame(time.icu), timevar = "g_icu.f", direction = "wide", idvar = "month", times = c(0, 1)) 

time.icu.wide <- na.omit(time.icu.wide)

time.icu.wide$time <- seq(1:35)
combined_data$time_elapsed <- as.numeric(combined_data$e_diagnosis - min(combined_data$e_diagnosis, na.rm = TRUE))

library(mgcv)
gam.icu <- gam(cbind(icu) ~ s(time_elapsed), data = combined_data, family = "binomial")
summary(gam.icu)
plot(gam.icu)
library(ggeffects)

gam.gg <- ggpredict(gam.icu, terms = c("time_elapsed"))

gam.gg$date <- as.Date(gam.gg$x, origin = min(combined_data$e_diagnosis, na.rm = TRUE))

quartz(width = 12, height = 5)

ggplot(gam.gg, aes(x = date, y = predicted)) +
  annotate("rect", xmin = as.Date(c("2020-12-25")), xmax = as.Date(c("2021-04-29")), ymin = -Inf, ymax = 0.15,alpha = .2) +
  annotate("rect", xmin = as.Date(c("2021-05-01")), xmax = as.Date(c("2021-11-29")), ymin = -Inf, ymax = 0.15,alpha = .2, fill = "orange") +
  annotate("rect", xmin = as.Date(c("2021-11-30")), xmax = as.Date(c("2022-05-31")), ymin = -Inf, ymax = 0.15,alpha = .2, fill = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  geom_line() +
  theme_pubr() +
  theme(text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "grey", linetype = 2),
        strip.text.y = element_text(size = 24),
        plot.margin = margin(0.5, 2, 0, 1, "cm"),
        axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b-%Y", limits = c(as.Date("2020-03-13"), as.Date("2022-05-31"))) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), as.Date("2022-06-01")), expand = F) +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0, 0.15)) +
  ylab("Percent ICU admission") +
  annotate("text", x = as.Date(c("2021-02-15", "2021-07-30", "2022-02-01")), y = 0.14, label = c("Alpha\nB.1.1.7", "Delta\nB.1.617.2", "Omicron\nB.1.1.529"), hjust = 0.2, size = 5) 
# geom_vline(xintercept = as.Date("2021-04-30"), linetype = 2) #+
# annotate("text", x = as.Date("2021-04-30"), y = 0.6, label = "Priority vaccine")



gam.hosp <- gam(cbind(hospitalization) ~ s(time_elapsed), data = combined_data, family = "binomial")
summary(gam.hosp)
plot(gam.hosp)

gam.gg2 <- ggpredict(gam.hosp, terms = c("time_elapsed"))

gam.gg2$date <- as.Date(gam.gg2$x, origin = min(combined_data$e_diagnosis, na.rm = TRUE))

quartz(width = 12, height = 5)

ggplot(gam.gg2, aes(x = date, y = predicted)) +
  annotate("rect", xmin = as.Date(c("2020-12-25")), xmax = as.Date(c("2021-04-29")), ymin = -Inf, ymax = 0.25,alpha = .2) +
  annotate("rect", xmin = as.Date(c("2021-05-01")), xmax = as.Date(c("2021-11-29")), ymin = -Inf, ymax = 0.25,alpha = .2, fill = "orange") +
  annotate("rect", xmin = as.Date(c("2021-11-30")), xmax = as.Date(c("2022-05-31")), ymin = -Inf, ymax = 0.25,alpha = .2, fill = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  geom_line() +
  theme_pubr() +
  theme(text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "grey", linetype = 2),
        strip.text.y = element_text(size = 24),
        plot.margin = margin(0.5, 2, 0, 1, "cm"),
        axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b-%Y", limits = c(as.Date("2020-03-13"), as.Date("2022-05-31"))) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), as.Date("2022-06-01")), expand = F) +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0, 0.25)) +
  ylab("Percent Hospital admission") +
  annotate("text", x = as.Date(c("2021-02-15", "2021-07-30", "2022-02-01")), y = 0.23, label = c("Alpha\nB.1.1.7", "Delta\nB.1.617.2", "Omicron\nB.1.1.529"), hjust = 0.2, size = 5) 
# geom_vline(xintercept = as.Date("2021-04-30"), linetype = 2) #+
# annotate("text", x = as.Date("2021-04-30"), y = 0.6, label = "Priority vaccine")


####################






# combined_data

 ggplot() + geom_smooth(data = combined_data , aes(x=as.Date(e_diagnosis), y = hospitalization, colour = "black"), se = FALSE) + geom_smooth(data = combined_data , aes(x=as.Date(e_diagnosis), y = icu, colour = "red"), se = FALSE) + scale_fill_gradient(low = "#ADD8E6", high = "#1488c5") + ylab("Proportion") + xlab("Date") +  
  scale_color_discrete(name = "Line", labels = c("Hospitalization Rate","ICU Rate")) + theme(panel.grid.minor = element_blank(),strip.background = element_rect(colour="red", fill="#CCCCFF"))
#                
 
 
 combined_data$covid_period = case_when(
   combined_data$e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
   combined_data$e_diagnosis >= as.Date("2021-04-04") & combined_data$e_diagnosis < as.Date("2021-12-19") ~ "Delta",
   combined_data$e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
 )
 
 combined_data$covid_period <- factor(combined_data$covid_period, levels = c("pre-Delta","Delta","Omicron"))
 
 covid_period_table<- combined_data %>% group_by(covid_period) %>% summarise(n = n(),hospitalizations = sum(hospitalization == "1", na.rm = TRUE),icu_admissions = sum(icu == "1", na.rm = TRUE))
 
 covid_period_infant_table<- combined_data %>% group_by(covid_period) %>% summarise(n = n(), n_p = n()/dim(combined_data)[1],preterm = sum(ga_del_cat2 == "preterm", na.rm = TRUE), preterm_p = sum(ga_del_cat2 == "preterm", na.rm = TRUE)/sum(!is.na(ga_del_cat2)),term = sum(ga_del_cat2 == "term", na.rm = TRUE), term_p = sum(ga_del_cat2 == "term", na.rm = TRUE)/sum(!is.na(ga_del_cat2)),denominator = sum(!is.na(ga_del_cat2)),NICU_k = sum(NICU == "1", na.rm = TRUE), NICU_p = sum(NICU == "1", na.rm = TRUE)/sum(!is.na(NICU)), denominator_NICU = sum(!is.na(NICU)))
 
 
 combined_data$number_of_vaccines2 <- case_when(
   combined_data$number_of_vaccines == 0 ~ "0",
   combined_data$number_of_vaccines == 1 ~ "1",
   combined_data$number_of_vaccines >= 2 ~ "2+"
 )
 
combined_data %>% group_by(covid_period) %>% summarise(n=n(),na_vacc = sum(!is.na(number_of_vaccines2))) 
 

combined_data <- combined_data %>% filter(e_diagnosis < as.Date("2023-01-01"))
dim(combined_data)

omicron_and_delta <- combined_data %>% filter(e_diagnosis < as.Date("2023-01-01")) %>%filter(covid_period %in% c("Omicron","Delta"))
dim(omicron_and_delta)

 # omicron_and_delta <- omicron_and_delta %>% filter(number_of_vaccines2 == "0")


######## OMICRON DELTA COMPARISON FOR DR MONEY ##########



omicron_and_delta <- omicron_and_delta[,c("covid_period","hospitalization","icu","ga_del_cat2","NICU", "BMI_cat","history_preterm","htn","diabetes","age_cat2","prov")]
omicron_and_delta$hospitalization <- as.factor(omicron_and_delta$hospitalization)
omicron_and_delta$covid_period <- factor(omicron_and_delta$covid_period, levels = c("Delta","Omicron"))
omicron_and_delta$icu <- as.factor(omicron_and_delta$icu)
omicron_and_delta$diabetes <- as.factor(omicron_and_delta$diabetes)
omicron_and_delta$htn <- as.factor(omicron_and_delta$htn)
omicron_and_delta$htn[which(is.na(omicron_and_delta$htn))] <- "0"
omicron_and_delta$age_cat2[which(is.na(omicron_and_delta$age_cat2))] <- "missing"
omicron_and_delta$diabetes[which(is.na(omicron_and_delta$diabetes))] <- "0"
omicron_and_delta$prov2 <- case_when(
  omicron_and_delta$prov == "ON" ~ "Ontario",
  omicron_and_delta$prov != "ON" ~ "Not Ontario",
) 
omicron_and_delta$prov2 <- as.factor(omicron_and_delta$prov2)

describeFactors(omicron_and_delta$history_preterm)

omicron_and_delta$history_preterm[is.na(omicron_and_delta$history_preterm)] <- FALSE

# omicron_and_delta$ga_del_cat2 <- case_when(
#   omicron_and_delta$ga_del_cat2 == "term" ~ "0",
#   omicron_and_delta$ga_del_cat2 == "preterm" ~ "1"
# )

omicron_and_delta$ga_del_cat2 <- factor(omicron_and_delta$ga_del_cat2, levels = c("term","preterm"))

omicron_and_delta$NICU <- as.factor(omicron_and_delta$NICU)

getT1Stat <- function(varname, digits=1, useNA = "no"){
  getDescriptionStatsBy(omicron_and_delta[, varname], 
                        omicron_and_delta$covid_period, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = FALSE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data_1 <- list()


for(i in 2:5){
  print(colnames(omicron_and_delta)[i])
  print(getT1Stat(colnames(omicron_and_delta)[i]))
  table_data_1[[colnames(omicron_and_delta)[i]]] <- getT1Stat(colnames(omicron_and_delta)[i])
}

# library(lme4)

# Now merge everything into a matrix
# and create the rgroup & n.rgroup variables
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data_1)) {

  rgroup <- c(rgroup,varlabel)
  n.rgroup <- c(n.rgroup,nrow(table_data_1[[varlabel]]))
  
  if(varlabel == "hospitalization"){
    regression1 <- glm(hospitalization ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta, family = binomial)
  }
  
  
  if(varlabel == "icu"){
    regression1 <- glm(icu ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta, family = binomial)
  }
  
  
  if(varlabel == "ga_del_cat2"){
    regression1 <- glm(ga_del_cat2 ~ covid_period + diabetes + htn + age_cat2 + history_preterm, data = omicron_and_delta[which(!is.na(omicron_and_delta$ga_del_cat2)),], family = binomial)
  }
  
  
  if(varlabel == "NICU"){
    regression1 <- glm(NICU ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta[which(!is.na(omicron_and_delta$NICU)),], family = binomial)
  }
  
  # summary(regression1)
  # 
  # drop1(regression1, test = "Chi")
  # 
  # plot(ggeffect(regression1, terms = "covid_period"))
  # 
  reg_summary <- summary(regression1)
  reg_coef <- reg_summary$coefficients
  odds_ratio <- exp(reg_coef[2,1])
  conf_interval <-exp(c(reg_coef[2,1] - 1.96*reg_coef[2,2],reg_coef[2,1] + 1.96*reg_coef[2,2]))
  
  
  odds_ratio_matrix <- matrix(data = NA,nrow = 2, ncol = 1)  
  colnames(odds_ratio_matrix) <- "Odds Ratio (95% CI)"
  odds_ratio_matrix[2,1] <- paste0(round(odds_ratio, digits = 3), ": (", round(conf_interval[1],digits = 3)," - ", round(conf_interval[2],digits = 3), ")")
  
  output_data <- rbind(output_data,cbind(table_data_1[[varlabel]],odds_ratio_matrix))
  
  print(paste("done variable",varlabel))
}

# Add a column spanner for the columns
cgroup <- c("", "Covid Wave - All Cases")
n.cgroup <- c(1, 3)
colnames(output_data) <- gsub("[ ]*Covid Wave", "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()

###### Unvaccinated cases #####

omicron_and_delta <- combined_data %>% filter(covid_period %in% c("Omicron","Delta"))
dim(omicron_and_delta)

omicron_and_delta <- omicron_and_delta %>% filter(number_of_vaccines2 == "0")


omicron_and_delta <- omicron_and_delta[,c("covid_period","hospitalization","icu","ga_del_cat2","NICU", "BMI_cat","history_preterm","htn","diabetes","age_cat2","prov")]
omicron_and_delta$hospitalization <- as.factor(omicron_and_delta$hospitalization)
omicron_and_delta$covid_period <- factor(omicron_and_delta$covid_period, levels = c("Delta","Omicron"))
omicron_and_delta$icu <- as.factor(omicron_and_delta$icu)
omicron_and_delta$diabetes <- as.factor(omicron_and_delta$diabetes)
omicron_and_delta$htn <- as.factor(omicron_and_delta$htn)
omicron_and_delta$htn[which(is.na(omicron_and_delta$htn))] <- "0"
omicron_and_delta$age_cat2[which(is.na(omicron_and_delta$age_cat2))] <- "missing"
omicron_and_delta$diabetes[which(is.na(omicron_and_delta$diabetes))] <- "0"
omicron_and_delta$prov2 <- case_when(
  omicron_and_delta$prov == "ON" ~ "Ontario",
  omicron_and_delta$prov != "ON" ~ "Not Ontario",
) 
omicron_and_delta$prov2 <- as.factor(omicron_and_delta$prov2)

describeFactors(omicron_and_delta$history_preterm)

omicron_and_delta$history_preterm[is.na(omicron_and_delta$history_preterm)] <- FALSE

# omicron_and_delta$ga_del_cat2 <- case_when(
#   omicron_and_delta$ga_del_cat2 == "term" ~ "0",
#   omicron_and_delta$ga_del_cat2 == "preterm" ~ "1"
# )

omicron_and_delta$ga_del_cat2 <- factor(omicron_and_delta$ga_del_cat2, levels = c("term","preterm"))

omicron_and_delta$NICU <- as.factor(omicron_and_delta$NICU)

getT1Stat <- function(varname, digits=1, useNA = "no"){
  getDescriptionStatsBy(omicron_and_delta[, varname], 
                        omicron_and_delta$covid_period, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data_1 <- list()


for(i in 2:5){
  print(colnames(omicron_and_delta)[i])
  print(getT1Stat(colnames(omicron_and_delta)[i]))
  table_data_1[[colnames(omicron_and_delta)[i]]] <- getT1Stat(colnames(omicron_and_delta)[i])
}

# library(lme4)

# Now merge everything into a matrix
# and create the rgroup & n.rgroup variables
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data_1)) {
  
  rgroup <- c(rgroup,varlabel)
  n.rgroup <- c(n.rgroup,nrow(table_data_1[[varlabel]]))
  
  if(varlabel == "hospitalization"){
    regression1 <- glm(hospitalization ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta, family = binomial)
  }
  
  
  if(varlabel == "icu"){
    regression1 <- glm(icu ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta, family = binomial)
  }
  
  
  if(varlabel == "ga_del_cat2"){
    regression1 <- glm(ga_del_cat2 ~ covid_period + diabetes + htn + age_cat2 + history_preterm, data = omicron_and_delta[which(!is.na(omicron_and_delta$ga_del_cat2)),], family = binomial)
  }
  
  
  if(varlabel == "NICU"){
    regression1 <- glm(NICU ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta[which(!is.na(omicron_and_delta$NICU)),], family = binomial)
  }
  
  # summary(regression1)
  # 
  # drop1(regression1, test = "Chi")
  # 
  # plot(ggeffect(regression1, terms = "covid_period"))
  # 
  reg_summary <- summary(regression1)
  reg_coef <- reg_summary$coefficients
  odds_ratio <- exp(reg_coef[2,1])
  conf_interval <-exp(c(reg_coef[2,1] - 1.96*reg_coef[2,2],reg_coef[2,1] + 1.96*reg_coef[2,2]))
  
  
  odds_ratio_matrix <- matrix(data = NA,nrow = 2, ncol = 1)  
  colnames(odds_ratio_matrix) <- "Odds Ratio (95% CI)"
  odds_ratio_matrix[1,1] <- paste0(round(odds_ratio, digits = 3), ": (", round(conf_interval[1],digits = 3)," - ", round(conf_interval[2],digits = 3), ")")
  
  output_data <- rbind(output_data,cbind(table_data_1[[varlabel]],odds_ratio_matrix))
  
  print(paste("done variable",varlabel))
}

# Add a column spanner for the columns
cgroup <- c("", "Covid Wave - Unvaccinated Cases only")
n.cgroup <- c(1, 4)
colnames(output_data) <- gsub("[ ]*Covid Wave", "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()




##### 2+ Doses Only #####




omicron_and_delta <- combined_data %>% filter(covid_period %in% c("Omicron","Delta"))
dim(omicron_and_delta)

omicron_and_delta <- omicron_and_delta %>% filter(number_of_vaccines2 == "2+")


omicron_and_delta <- omicron_and_delta[,c("covid_period","hospitalization","icu","ga_del_cat2","NICU", "BMI_cat","history_preterm","htn","diabetes","age_cat2","prov")]
omicron_and_delta$hospitalization <- as.factor(omicron_and_delta$hospitalization)
omicron_and_delta$covid_period <- factor(omicron_and_delta$covid_period, levels = c("Delta","Omicron"))
omicron_and_delta$icu <- as.factor(omicron_and_delta$icu)
omicron_and_delta$diabetes <- as.factor(omicron_and_delta$diabetes)
omicron_and_delta$htn <- as.factor(omicron_and_delta$htn)
omicron_and_delta$htn[which(is.na(omicron_and_delta$htn))] <- "0"
omicron_and_delta$age_cat2[which(is.na(omicron_and_delta$age_cat2))] <- "missing"
omicron_and_delta$diabetes[which(is.na(omicron_and_delta$diabetes))] <- "0"
omicron_and_delta$prov2 <- case_when(
  omicron_and_delta$prov == "ON" ~ "Ontario",
  omicron_and_delta$prov != "ON" ~ "Not Ontario",
) 
omicron_and_delta$prov2 <- as.factor(omicron_and_delta$prov2)

describeFactors(omicron_and_delta$history_preterm)

omicron_and_delta$history_preterm[is.na(omicron_and_delta$history_preterm)] <- FALSE

# omicron_and_delta$ga_del_cat2 <- case_when(
#   omicron_and_delta$ga_del_cat2 == "term" ~ "0",
#   omicron_and_delta$ga_del_cat2 == "preterm" ~ "1"
# )

omicron_and_delta$ga_del_cat2 <- factor(omicron_and_delta$ga_del_cat2, levels = c("term","preterm"))

omicron_and_delta$NICU <- as.factor(omicron_and_delta$NICU)

getT1Stat <- function(varname, digits=1, useNA = "no"){
  getDescriptionStatsBy(omicron_and_delta[, varname], 
                        omicron_and_delta$covid_period, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data_1 <- list()


for(i in 2:5){
  print(colnames(omicron_and_delta)[i])
  print(getT1Stat(colnames(omicron_and_delta)[i]))
  table_data_1[[colnames(omicron_and_delta)[i]]] <- getT1Stat(colnames(omicron_and_delta)[i])
}

# library(lme4)

# Now merge everything into a matrix
# and create the rgroup & n.rgroup variables
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data_1)) {
  
  rgroup <- c(rgroup,varlabel)
  n.rgroup <- c(n.rgroup,nrow(table_data_1[[varlabel]]))
  
  if(varlabel == "hospitalization"){
    regression1 <- glm(hospitalization ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta, family = binomial)
  }
  
  
  if(varlabel == "icu"){
    regression1 <- glm(icu ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta, family = binomial)
  }
  
  
  if(varlabel == "ga_del_cat2"){
    regression1 <- glm(ga_del_cat2 ~ covid_period + diabetes + htn + age_cat2 + history_preterm, data = omicron_and_delta[which(!is.na(omicron_and_delta$ga_del_cat2)),], family = binomial)
  }
  
  
  if(varlabel == "NICU"){
    regression1 <- glm(NICU ~ covid_period + diabetes + htn + age_cat2, data = omicron_and_delta[which(!is.na(omicron_and_delta$NICU)),], family = binomial)
  }
  
  # summary(regression1)
  # 
  # drop1(regression1, test = "Chi")
  # 
  # plot(ggeffect(regression1, terms = "covid_period"))
  # 
  reg_summary <- summary(regression1)
  reg_coef <- reg_summary$coefficients
  odds_ratio <- exp(reg_coef[2,1])
  conf_interval <-exp(c(reg_coef[2,1] - 1.96*reg_coef[2,2],reg_coef[2,1] + 1.96*reg_coef[2,2]))
  
  
  odds_ratio_matrix <- matrix(data = NA,nrow = 2, ncol = 1)  
  colnames(odds_ratio_matrix) <- "Odds Ratio (95% CI)"
  odds_ratio_matrix[1,1] <- paste0(round(odds_ratio, digits = 3), ": (", round(conf_interval[1],digits = 3)," - ", round(conf_interval[2],digits = 3), ")")
  
  output_data <- rbind(output_data,cbind(table_data_1[[varlabel]],odds_ratio_matrix))
  
  print(paste("done variable",varlabel))
}

# Add a column spanner for the columns
cgroup <- c("", "Covid Wave - 2+ Doses only")
n.cgroup <- c(1, 4)
colnames(output_data) <- gsub("[ ]*Covid Wave", "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()















# combined_data2 <- combined_data[which(combined_data$covid_period == "Omicron"),] 
combined_data2 <- combined_data
combined_data <- combined_data2
unique(combined_data$covid_period)





# combined_data <- combined_data[which(combined_data$covid_period == "pre-Delta"),] 

vaccine_table <- combined_data %>% group_by(number_of_vaccines2) %>% summarise(n = n(), n_p = n()/dim(combined_data)[1],preterm = sum(ga_del_cat2 == "preterm", na.rm = TRUE), preterm_p = sum(ga_del_cat2 == "preterm", na.rm = TRUE)/sum(!is.na(ga_del_cat2)),denominator = sum(!is.na(ga_del_cat2)),NICU_k = sum(NICU == "1", na.rm = TRUE), NICU_p = sum(NICU == "1", na.rm = TRUE)/sum(!is.na(NICU)), denominator_NICU = sum(!is.na(NICU)),hospitalizations = sum(hospitalization == "1", na.rm = TRUE),hospitalizations_p = sum(hospitalization == "1", na.rm = TRUE)/sum(!is.na(hospitalization)), denominator_hosp = sum(!is.na(hospitalization)), icu_admissions = sum(icu == "1", na.rm = TRUE))

zzz_hosp_table<- combined_data %>% group_by(hospitalization) %>% summarise(n = n(), n_p = n()/dim(combined_data)[1], unvaxxed = sum(number_of_vaccines2 == "0", na.rm = TRUE), unvaxxed_p = sum(number_of_vaccines2 == "0", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator1 =sum(!is.na(number_of_vaccines2)), vaxxed = sum(number_of_vaccines2 == "2+", na.rm = TRUE), vaxxed_p = sum(number_of_vaccines2 == "2+", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator2 =sum(!is.na(number_of_vaccines2)) )
zzz_icu_table<- combined_data %>% group_by(icu) %>% summarise(n = n(), n_p = n()/dim(combined_data)[1], unvaxxed = sum(number_of_vaccines2 == "0", na.rm = TRUE), unvaxxed_p = sum(number_of_vaccines2 == "0", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator1 =sum(!is.na(number_of_vaccines2)), vaxxed = sum(number_of_vaccines2 == "2+", na.rm = TRUE), vaxxed_p = sum(number_of_vaccines2 == "2+", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator2 =sum(!is.na(number_of_vaccines2)) )
zzz_preterm_table <- combined_data  %>% group_by(ga_del_cat2) %>% summarise(n = n(), n_p = n()/dim(combined_data)[1], unvaxxed = sum(number_of_vaccines2 == "0", na.rm = TRUE), unvaxxed_p = sum(number_of_vaccines2 == "0", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator1 =sum(!is.na(number_of_vaccines2)), vaxxed = sum(number_of_vaccines2 == "2+", na.rm = TRUE), vaxxed_p = sum(number_of_vaccines2 == "2+", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator2 =sum(!is.na(number_of_vaccines2)) )
zzz_nicu_table<- combined_data %>% group_by(NICU) %>% summarise(n = n(), n_p = n()/dim(combined_data)[1], unvaxxed = sum(number_of_vaccines2 == "0", na.rm = TRUE), unvaxxed_p = sum(number_of_vaccines2 == "0", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator1 =sum(!is.na(number_of_vaccines2)), vaxxed = sum(number_of_vaccines2 == "2+", na.rm = TRUE), vaxxed_p = sum(number_of_vaccines2 == "2+", na.rm = TRUE)/sum(!is.na(number_of_vaccines2)), denominator2 =sum(!is.na(number_of_vaccines2)) )

combined_data$vacc_status <- case_when(
  combined_data$number_of_vaccines2 == "0" ~ "unvaccinated",
  combined_data$number_of_vaccines2 == "2+" ~ "2+"
)
combined_data$NICU
combined_data %>% group_by(vacc_status) %>% summarise(n = n(), n_hosp = sum(hospitalization, na.rm = TRUE), prop_hosp = sum(hospitalization, na.rm = TRUE)/n(), n_icu = sum(icu, na.rm = TRUE), prop_icu = sum(icu, na.rm = TRUE)/n(), n_preterm = sum(ga_del_cat2 == "preterm", na.rm = TRUE), prop_preterm = sum(ga_del_cat2 == "preterm", na.rm = TRUE)/sum(!is.na(ga_del_cat2)),n_nicu = sum(NICU == "1", na.rm = TRUE), prop_nicu =sum(NICU == "1", na.rm = TRUE)/sum(!is.na(NICU)) )

event_matrix_hosp <- matrix(data = c(zzz_hosp_table$vaxxed[2],zzz_hosp_table$unvaxxed[2],zzz_hosp_table$vaxxed[1],zzz_hosp_table$unvaxxed[1]), nrow = 2,ncol =2)
RR_hosp <- (event_matrix_hosp[1,1]/(event_matrix_hosp[1,1] + event_matrix_hosp[1,2]))/(event_matrix_hosp[2,1]/(event_matrix_hosp[2,1] + event_matrix_hosp[2,2]))
LL_RR_hosp <- exp(log(RR_hosp) - 1.96*(sqrt(1/event_matrix_hosp[1,1] + 1/event_matrix_hosp[2,1] + 1/(event_matrix_hosp[1,1] + event_matrix_hosp[1,2]) + 1/(event_matrix_hosp[2,1] + event_matrix_hosp[2,2]))))
UP_RR_hosp <- exp(log(RR_hosp) + 1.96*(sqrt(1/event_matrix_hosp[1,1] + 1/event_matrix_hosp[2,1] + 1/(event_matrix_hosp[1,1] + event_matrix_hosp[1,2]) + 1/(event_matrix_hosp[2,1] + event_matrix_hosp[2,2]))))

event_matrix_icu <- matrix(data = c(zzz_icu_table$vaxxed[2],zzz_icu_table$unvaxxed[2],zzz_icu_table$vaxxed[1],zzz_icu_table$unvaxxed[1]), nrow = 2,ncol =2)
RR_icu <- (event_matrix_icu[1,1]/(event_matrix_icu[1,1] + event_matrix_icu[1,2]))/(event_matrix_icu[2,1]/(event_matrix_icu[2,1] + event_matrix_icu[2,2]))
LL_RR_icu <- exp(log(RR_icu) - 1.96*(sqrt(1/event_matrix_icu[1,1] + 1/event_matrix_icu[2,1] + 1/(event_matrix_icu[1,1] + event_matrix_icu[1,2]) + 1/(event_matrix_icu[2,1] + event_matrix_icu[2,2]))))
UP_RR_icu <- exp(log(RR_icu) + 1.96*(sqrt(1/event_matrix_icu[1,1] + 1/event_matrix_icu[2,1] + 1/(event_matrix_icu[1,1] + event_matrix_icu[1,2]) + 1/(event_matrix_icu[2,1] + event_matrix_icu[2,2]))))

event_matrix_preterm <- matrix(data = c(zzz_preterm_table$vaxxed[1],zzz_preterm_table$unvaxxed[1],zzz_preterm_table$vaxxed[2],zzz_preterm_table$unvaxxed[2]), nrow = 2,ncol =2)
RR_preterm <- (event_matrix_preterm[1,1]/(event_matrix_preterm[1,1] + event_matrix_preterm[1,2]))/(event_matrix_preterm[2,1]/(event_matrix_preterm[2,1] + event_matrix_preterm[2,2]))
LL_RR_preterm <- exp(log(RR_preterm) - 1.96*(sqrt(1/event_matrix_preterm[1,1] + 1/event_matrix_preterm[2,1] + 1/(event_matrix_preterm[1,1] + event_matrix_preterm[1,2]) + 1/(event_matrix_preterm[2,1] + event_matrix_preterm[2,2]))))
UP_RR_preterm <- exp(log(RR_preterm) + 1.96*(sqrt(1/event_matrix_preterm[1,1] + 1/event_matrix_preterm[2,1] + 1/(event_matrix_preterm[1,1] + event_matrix_preterm[1,2]) + 1/(event_matrix_preterm[2,1] + event_matrix_preterm[2,2]))))

event_matrix_nicu <- matrix(data = c(zzz_nicu_table$vaxxed[2],zzz_nicu_table$unvaxxed[2],zzz_nicu_table$vaxxed[1],zzz_nicu_table$unvaxxed[1]), nrow = 2,ncol =2)
RR_nicu <- (event_matrix_nicu[1,1]/(event_matrix_nicu[1,1] + event_matrix_nicu[1,2]))/(event_matrix_nicu[2,1]/(event_matrix_nicu[2,1] + event_matrix_nicu[2,2]))
LL_RR_nicu <- exp(log(RR_nicu) - 1.96*(sqrt(1/event_matrix_nicu[1,1] + 1/event_matrix_nicu[2,1] + 1/(event_matrix_nicu[1,1] + event_matrix_nicu[1,2]) + 1/(event_matrix_nicu[2,1] + event_matrix_nicu[2,2]))))
UP_RR_nicu <- exp(log(RR_nicu) + 1.96*(sqrt(1/event_matrix_nicu[1,1] + 1/event_matrix_nicu[2,1] + 1/(event_matrix_nicu[1,1] + event_matrix_nicu[1,2]) + 1/(event_matrix_nicu[2,1] + event_matrix_nicu[2,2]))))


combined_data$YEAR <- format(as.Date(combined_data$e_diagnosis),"%Y")
 
 yearly_data <- combined_data %>% group_by(prov) %>% summarise(n=n(), n_2020 = sum(YEAR == 2020,na.rm = TRUE),n_2021 = sum(YEAR == 2021,na.rm = TRUE),n_2022 = sum(YEAR == 2022,na.rm = TRUE))
 
 
 combined_data_old <- combined_data[which(combined_data$e_diagnosis < as.Date("2022-06-30")),]
 
 combined_data_old %>% group_by(ga_del_cat2) %>% summarise(n=n())
 combined_data %>% group_by(ga_del_cat2) %>% summarise(n=n())
 
 
 
 
 ##### VAXX PLOT
 
 
 describeFactors(combined_data$number_of_vaccines2)
 combined_data$number_of_vaccines2 <- factor(combined_data$number_of_vaccines2, levels = c("0","1","2+"))
 
 combined_data$n_covid_date1 <- as.Date(combined_data$n_covid_date1)
 combined_data$month_vacc1 <- cut(combined_data$n_covid_date1, breaks = "months")
 combined_data$month <- cut(combined_data$e_diagnosis, breaks = "months")
 combined_data$week_vacc1 <- cut(combined_data$n_covid_date1, breaks = "weeks")
 describeFactors(combined_data$month_vacc1)
 describeFactors(combined_data$week_vacc1)
 combined_data$month_vacc1 <- as.Date(combined_data$month_vacc1)
 combined_data$month <- as.Date(combined_data$month)
 
 ggplot(combined_data[-which(is.na(combined_data$number_of_vaccines2)), ], aes(fill = number_of_vaccines2, group = number_of_vaccines2, x = hospitalization)) +
   geom_bar(position = position_fill())
 
 ## vaccinated status by dx date?
 
 ggplot(combined_data[-which(is.na(combined_data$number_of_vaccines2) | is.na(combined_data$month)), ], aes(x = month, group = number_of_vaccines2, fill = number_of_vaccines2)) +
   geom_bar()
 
 quartz(width = 12, height = 5)
 ggplot(combined_data[-which(is.na(combined_data$number_of_vaccines2) | is.na(combined_data$month)), ], aes(x = month, group = number_of_vaccines2, fill = number_of_vaccines2)) +
   geom_bar(position = position_fill()) +
   theme(panel.grid.major.y = element_line(linetype = 2, colour = "black"), 
         text = element_text(size = 12),
         plot.margin = margin(0.5, 0.5, 1.5, 0.5, "cm"),
         axis.text.x = element_text(angle = 90)) +
   scale_y_continuous(labels = scales::percent_format()) +
   scale_fill_manual(values = c(ghibli_palette("PonyoMedium")[c(4, 6, 3)]), labels = c("0", "1", "2+ doses")) +
   guides(fill = guide_legend(title = "Vaccination before diagnosis")) +
   ylab("Percent of cases per month") +
   scale_x_date(date_breaks = "1 month", date_labels = "%b-%y", limits = as.Date(c("2020-03-01", "2022-12-31"))) +
   coord_cartesian(xlim = c(as.Date("2020-03-01"), as.Date("2022-12-31")), expand = F) +
   xlab("")
 
 
 summary(as.factor(combined_data$number_of_vaccines2))
 
 combined_data %>% group_by(covid_period, number_of_vaccines2) %>% summarise(n=n())
 combined_data$year <- format(combined_data$e_diagnosis, "%Y")
 
 combined_data %>% group_by(year, number_of_vaccines2) %>% summarise(n=n())
 
 combined_data$prov <- as.factor(combined_data$prov)
 
 combined_data <- as.data.frame(combined_data)
 combined_data %>% filter(prov == "ON") %>% group_by(covid_period) %>% summarise(n = n(),preterm1 = sum(ga_del_cat2 == "preterm", na.rm = TRUE), preterm_percent = sum(ga_del_cat2 == "preterm", na.rm = TRUE)/sum(!is.na(ga_del_cat2)))
 combined_data %>% filter(prov != "ON") %>% group_by(covid_period) %>% summarise(n = n(),preterm1 = sum(ga_del_cat2 == "preterm", na.rm = TRUE), preterm_percent = sum(ga_del_cat2 == "preterm", na.rm = TRUE)/sum(!is.na(ga_del_cat2)))
 
 
           
 
 # check for data abstraction team
 # s_pe_neuro
 # 
 # t_diag_neuro
 
 # ### PMA DATA REQUEST ###
 # ICU admission - icu
 # Ventilation
 # Critical Care (ICU, Ventilation, and/or other high-level treatment (ECMO, Dialysis))
 # Pneumonia
 # Pregnancy-related death
 # Haemorrhage
 # Placental abruption
 # Hypertensive disorders of pregnancy (ever in current pregnancy)
 # Hypertensive disorders of pregnancy (diagnosed at or after COVID-19 onset)
 # Pre-eclampsia
 # Eclampsia
 # Pre-eclampsia/eclampsia
 # Embolic disease
 # Preterm labor
 # Preterm labor - using restricted denominator
 # C-section
 # Intrapartum c-section
 # Stillbirth
 # Perinatal death
 # Early neonatal death
 # Neonatal death
 # NICU Admission at birth
 # Very low birthweight (<1500g)
 # Low birthweight (<2500g)
 # Small for gestational age (3rd)
 # Small for gestational age (10th)
 # Very preterm birth (<34wks)
 # Preterm birth (<37 wks)
 # Very preterm birth (<34wks) - using restricted denominator
 # Preterm birth (<37 wks) - using restricted denominator

