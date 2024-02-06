#### ENTIRELY NEW REORGANIZED CANCOVID 2.0 SCRIPT for PMA data ####
#### Gal Av-Gay, November 14, 2023 ####
#### Separate everything into Maternal Outcomes vs Infant Outcomes ####


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
data_ns <- read.csv("2024-JAN-18_RAW_COVID19InPreg_DATA_2024-01-18_1559_FIXED.csv", header = TRUE)
#data_on <- read_sas("ONdata/covid_preg_infants_sep3022.sas7bdat")
data_on <- read.csv("ONdata/data_on_06_13_2023.csv", header = TRUE)
data_other <- read.csv("CanadianCOVID19InPre_DATA_2024-01-23_1258.csv", header = TRUE)
data_NL <- read.csv("NL_CLEANED_2024-01-21.csv")
data_NL_antepartum <- read.csv("NLdata/NL_antepartum.csv")

## quebec is complicated...##
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

summary(as.Date(data_bc$e_diagnosis[-which(data_bc$e_diagnosis == "9999-09-09"|data_bc$e_diagnosis == "8888-08-08")], format = "%Y-%m-%d"))

## data other split up

# unique(data_other$redcap_data_access_group)
data_mb <- data_other %>% filter(redcap_data_access_group == "mb")
data_nb <- data_other %>% filter(redcap_data_access_group == "nb")
data_pe <- data_other %>% filter(redcap_data_access_group == "pe")
data_yt <- data_other %>% filter(redcap_data_access_group == "yt")


#### FIX DATA (combine record_ids into single row) ####

## BC 
# length(unique(data_bc$a_record_id))
# unique(data_bc$redcap_event_name)

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


summary(as.Date(data_bc$e_diagnosis[-which(data_bc$e_diagnosis == "9999-09-09"|data_bc$e_diagnosis == "8888-08-08")], format = "%Y-%m-%d"))

# dim(data_bc)

## Nova Scotia (already combined)
# 
# length(unique(data_ns$a_record_id))
# length(unique(data_ns$a_record_id))



## Ontario (weird one)
## on.dat will contain twins, data_on will not (infant outcomes versus maternity outcomes)
on.dat <- data_on 

data_on <-  on.dat[-which(duplicated(on.dat$new_preg_id)), ]

summary(as.Date(data_on$C_COVID_diagnosis_date))


## qc
# length(data_qc$a_record_id)
# length(unique(data_qc$a_record_id)) ## already squished it seems

# data_qc$r_dob
## Manitoba
# length(data_mb$a_record_id)
# length(unique(data_mb$a_record_id)) 

# unique(data_mb$redcap_event_name)

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

data_mb <- data_mb[-which(data_mb$a_record_id == "MB1063"),]

summary(as.Date(data_mb$e_diagnosis))

# data_mb$r_dob

summary(as.Date(data_ns$e_diagnosis))

# ## new brunswick
# length(data_nb$a_record_id)
# length(unique(data_nb$a_record_id)) 
# 
# unique(data_nb$redcap_event_name)

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

summary(as.Date(data_nb$e_diagnosis))

# data_nb$r_dob

## yukon
# length(data_pe$a_record_id)
# length(unique(data_pe$a_record_id)) 
# 
# unique(data_pe$redcap_event_name)

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

summary(as.Date(data_pe$e_diagnosis[-which(is.na(data_pe$e_diagnosis)|data_pe$e_diagnosis == "9999-09-09")], format = "%Y-%m-%d"))

# ## PEI
# length(data_yt$a_record_id)
# length(unique(data_yt$a_record_id)) 
# 
# unique(data_yt$redcap_event_name)

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

summary(as.Date(data_yt$e_diagnosis))
summary(as.Date(data_NL$MT_REPORTED_DT, format = "%d-%b-%y"))

##### NEWFOUNDLAND ####
# data_NL <- read.csv("everything_else/NL_cancovid.csv")
# data_NL_antepartum <- read.csv("everything_else/NL_antepartum.csv")

# colnames(data_NL)

# length(data_NL$NL_COVIDSTUDYID)
# length(unique(data_NL$NL_COVIDSTUDYID))
# 
# length(data_NL_antepartum$NL_COVIDSTUDYID)
# length(unique(data_NL_antepartum$NL_COVIDSTUDYID))

## how to combine antepartum and normal data? add new columns for each antepartum hospital visit? ad hoc?

# data_NL_antepartum$NL_COVIDSTUDYID[duplicated(data_NL_antepartum$NL_COVIDSTUDYID)]

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


# dim(data_NL_antepartum)
# dim(data_NL_antepartum_first_visit)
# dim(data_NL_antepartum_duplicates)
# dim(data_NL_antepartum_duplicates2)


## NOW combine all into one row per mother, rename each column based on the visit number
colnames(data_NL_antepartum_first_visit)[2:dim(data_NL_antepartum_first_visit)[2]] <- paste0(colnames(data_NL_antepartum_first_visit)[2:dim(data_NL_antepartum_first_visit)[2]],"first_visit")
colnames(data_NL_antepartum_second_visit)[2:dim(data_NL_antepartum_second_visit)[2]] <- paste0(colnames(data_NL_antepartum_second_visit)[2:dim(data_NL_antepartum_second_visit)[2]],"second_visit")
colnames(data_NL_antepartum_third_visit)[2:dim(data_NL_antepartum_third_visit)[2]] <- paste0(colnames(data_NL_antepartum_third_visit)[2:dim(data_NL_antepartum_third_visit)[2]],"third_visit")
colnames(data_NL_antepartum_fourth_visit)[2:dim(data_NL_antepartum_fourth_visit)[2]] <- paste0(colnames(data_NL_antepartum_fourth_visit)[2:dim(data_NL_antepartum_fourth_visit)[2]],"fourth_visit")


data_NL_ap <- left_join(data_NL_antepartum_first_visit,data_NL_antepartum_second_visit,by = c("NL_COVIDSTUDYID"))
data_NL_ap <- left_join(data_NL_ap,data_NL_antepartum_third_visit,by = c("NL_COVIDSTUDYID"))
data_NL_ap <- left_join(data_NL_ap,data_NL_antepartum_fourth_visit,by = c("NL_COVIDSTUDYID"))

# dim(data_NL_ap)

# check for NL data size is correct
# length(unique(data_NL_antepartum$NL_COVIDSTUDYID))





#### FIX INCORRECT DATA ####
# DO THIS LATER? #
data_bc$time_del <- as.numeric(as.Date(data_bc$r_dob) - as.Date(data_bc$e_diagnosis))
data_ns$time_del <- as.numeric(as.Date(data_ns$r_dob) - as.Date(data_ns$e_diagnosis))
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

# colnames(data_bc)[which(!(colnames(data_bc) %in% colnames(data_mb)))]
# sum((colnames(data_bc) %in% colnames(data_qc)))

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

write.csv(full_data_sansON, here("full_data_sansON_06_13_2023.csv"))

## MATERNITY OUTCOMES - 


## We begin with  1.Date of Diagnosis and 2. Hospitalization, Admission to ICU, and Admission to NICU. Because we need these variables to estimate the actual number of cases by province
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

full_data_sansON[which(is.na(full_data_sansON$e_diagnosis)),c("e_diagnosis")] <- full_data_sansON_temp$e_diagnosis


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


## NL
# dim(data_NL)
# length(unique(data_NL$NL_COVIDSTUDYID))

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



# # expected date of delivery
# sum(is.na(full_data_sansON$i_deliverydate_est))
# # date of birth
# sum(is.na(full_data_sansON$r_dob))
#ga_at_del
full_data_sansON$r_dob <- as.Date(as.character(full_data_sansON$r_dob), format = "%Y-%m-%d")

# full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)
full_data_sansON$ga_at_del <- as.numeric((280 - (as.Date(full_data_sansON$i_deliverydate_est) - as.Date(full_data_sansON$r_dob)))/7)

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


summary(as.factor(full_data_sansON$n_covid))


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
# full_data_sansON$b_age


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


# # replace the No entry with NA
full_data_sansON$cvs <- ifelse(full_data_sansON$cvs  == "No entry", NA, full_data_sansON$cvs)

# check to make sure
full_data_sansON %>% group_by(prov) %>% summarise(missing_percent = sum(is.na(cvs))/n())
# describeFactors(full_data_sansON$cvs)

## extract specific comorbidities of interest
# asthma
full_data_sansON$asthma <- case_when(
  full_data_sansON$j_resp_asth___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$asthma)

# Chronic obstructive lung disease
full_data_sansON$lung <- case_when(
  full_data_sansON$j_resp_lung___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$lung)

# hypertension pre-existing
full_data_sansON$htn <- case_when(
  full_data_sansON$j_cvs_htn___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$htn)

# diabetes 1 or 2
full_data_sansON$diabetes <- case_when(
  full_data_sansON$j_endo_diabt1___1 == 1 | full_data_sansON$j_endo_diabt2___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$diabetes)


# autoimmune diseases have to be gathered from a few places
# celiac, lupus, rheumatoid arthritis, Ankylosing spondylitis, others
# full_data_sansON$j_aai_oth_s # check this variable for others eg MS

full_data_sansON$autoimm <- case_when(
  (full_data_sansON$j_gi_celiac___1 == 1) | (full_data_sansON$j_ms_lupus___1 == 1) | (full_data_sansON$j_ms_as___1 == 1) | (full_data_sansON$j_ms_ra___1 == 1) | grepl("Multiple sclerosis|Antiphospho|neutropenia|SLE|lupus", full_data_sansON$j_aai_oth_s, ignore.case = TRUE) ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$autoimm)

## composite comorbidities
# c(full_data_sansON$cvs, full_data_sansON$diabetes, full_data_sansON$htn, full_data_sansON$asthma, full_data_sansON$hbv, full_data_sansON$hcv)

full_data_sansON$any_comor <- case_when(
  full_data_sansON$cvs == "Yes" | full_data_sansON$asthma == "Yes" | full_data_sansON$diabetes == "Yes" | full_data_sansON$htn == "Yes" | full_data_sansON$k_hbv == 1 | full_data_sansON$k_hcv == 1 ~ "Yes",
  full_data_sansON$cvs == "No" & full_data_sansON$asthma == "No" & full_data_sansON$diabetes == "No" & full_data_sansON$htn == "No" ~ "No"
)



full_data_sansON$i_weight <- replace(full_data_sansON$i_weight, which(full_data_sansON$i_weight == 999 | full_data_sansON$i_weight == 666), NA)
full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(full_data_sansON$i_height == 999 | full_data_sansON$i_height == 666), NA)

full_data_sansON$i_weight <- replace(full_data_sansON$i_weight, which(as.numeric(full_data_sansON$i_weight) > 300), NA)
full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(as.numeric(full_data_sansON$i_height) > 250), NA)

## fix qc heights
full_data_sansON$i_height[which(as.numeric(full_data_sansON$i_height) < 2)] <- as.numeric(full_data_sansON$i_height[which(as.numeric(full_data_sansON$i_height) < 2)])*100



overweights<- full_data_sansON[which(as.numeric(full_data_sansON$i_weight) > 200), c("prov","a_record_id","i_weight")]
overheights<- full_data_sansON[which(as.numeric(full_data_sansON$i_height) > 200), c("prov","a_record_id","i_height")]
underheights <- full_data_sansON[which(as.numeric(full_data_sansON$i_height) < 100), c("prov","a_record_id","i_height")]


full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(as.numeric(full_data_sansON$i_height) < 90), NA)


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

covid_period_table<- full_data_sansON %>% group_by(covid_period) %>% summarise(n = n(),stillbirth = sum(o_sb == 1, na.rm = TRUE),hospitalizations = sum(e_hosp == "1", na.rm = TRUE),icu_admissions = sum(g_icu == "1", na.rm = TRUE))

covid_period_table2 <- full_data_sansON %>% group_by(covid_period) %>% summarise(n = n(),term_births = sum(ga_del_cat == "term", na.rm = TRUE), late_preterm = sum(ga_del_cat == "late preterm", na.rm = TRUE),moderate_preterm = sum(ga_del_cat == "moderate preterm", na.rm = TRUE),very_preterm = sum(ga_del_cat == "very preterm", na.rm = TRUE))


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

# describeFactors(data_on$BMI_cat)
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

# data_NL$Pre.existing.Diab.Insulin.Required
# data_NL$Pre.existing.Diab
# data_NL$OTHER

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




## gestational age at birth (term vs preterm)
data_on$B_GA_birth[which(data_on$B_GA_birth == "")] <- NA
# 
# summary(as.factor(data_on$B_GA_birth))
# summary(as.factor(full_data_sansON$ga_del_cat2))
# summary(as.factor(full_data_sansON$ga_del_cat))

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
# summary(as.factor(data_on$gravida))
# 
# summary(as.factor(full_data_sansON$h_gravida))

data_on$h_gravida  <- data_on$gravida

full_data_sansON$gravida <- as.numeric(full_data_sansON$h_gravida)



## NL GRAVIDA ## 
# data_NL$Gravida




# asthma (skip for now? non ontario only? missin also for NL)
# summary(as.factor(full_data_sansON$asthma))


#hospitalization/ icu done. Try "oxygen"
# 
# summary(as.factor(full_data_sansON$e_oxygen___1))
# summary(as.factor(data_on$H_Any_Other_Oxygen))

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
# 
# summary(as.factor(data_on$oxygen))
# summary(as.factor(full_data_sansON$oxygen))

## oxygen seems unavailable for NL CHECK INTERVENTION CODES

# mode of delivery
# summary(as.factor(full_data_sansON$p_mode))
# summary(as.factor(data_on$birth_type_id))

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

# unique(data_NL$Type.of.Labour)


# summary(as.factor(full_data_sansON$p_mode))
full_data_sansON$mode_del <- case_when(
  full_data_sansON$p_mode == 1 ~ "vaginal",
  full_data_sansON$p_mode == 2 ~ "cs"
)
# 
# summary(as.factor(full_data_sansON$mode_del))
# summary(as.factor(data_on$mode_del))
# 



# ## pregnancy outcome
# summary(as.factor(full_data_sansON$p_outcome))
# summary(as.factor(data_on$pregnancy_outcome_id))

data_on$preg_outcome <- case_when(
  data_on$pregnancy_outcome_id == 1021030 ~ "live birth",
  data_on$pregnancy_outcome_id == 1021040 ~ "loss",
  data_on$pregnancy_outcome_id == 1021050 ~ "loss",
  data_on$pregnancy_outcome_id == 1021070 ~ "stillbirth",
  data_on$pregnancy_outcome_id == 1021080 ~ "stillbirth",
  data_on$pregnancy_outcome_id == 1021090 ~ "stillbirth"
)

# summary(as.factor(full_data_sansON$p_outcome))

full_data_sansON$preg_outcome <- case_when(
  full_data_sansON$p_outcome == 1 ~ "loss",
  full_data_sansON$p_outcome == 2 ~ "stillbirth",
  full_data_sansON$p_outcome == 3 ~ "live birth"
  
)
# summary(as.factor(full_data_sansON$preg_outcome))


## NL preg outcome, only diagnosis codes 1-10 are populated


#icd_10 <- read.csv("ICD10-CA.csv", header = TRUE)

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
# colnames(all_unique_codes)
#mini_data_dictionary <- inner_join(all_unique_codes,icd_10, by = c("all_unique_codes" = "ICD10CA_code"))

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
# summary(as.factor(full_data_sansON$r_num))
# summary(as.factor(data_on$BIS_num_fetuses_grp))

data_on$multiple_preg <- case_when(
  data_on$BIS_num_fetuses_grp == "multiple" ~ "multiple",
  data_on$BIS_num_fetuses_grp == "singleton" ~ "singleton"
)

full_data_sansON$multiple_preg <- case_when(
  full_data_sansON$r_num == 2 ~ "multiple",
  full_data_sansON$r_num == 1 ~ "singleton"
)
# 
# summary(as.factor(full_data_sansON$multiple_preg))
# summary(as.factor(data_on$multiple_preg))


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

# summary(as.factor(data_on$prev_preterm_births))
# summary(as.factor(full_data_sansON$h_preterm))

full_data_sansON$h_preterm[which(full_data_sansON$h_preterm == 999)] <- NA

full_data_sansON$history_preterm <- case_when(
  full_data_sansON$h_preterm == 0 ~ FALSE,
  full_data_sansON$h_preterm > 0 ~ TRUE
)

# summary(as.factor(full_data_sansON$history_preterm))


data_on$history_preterm <- case_when(
  data_on$prev_preterm_births == 0 ~ FALSE,
  data_on$prev_preterm_births > 0 ~ TRUE
)

# summary(as.factor(data_on$history_preterm))

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
# summary(as.factor(data_on$labour_type))


full_data_sansON$labour_type <- case_when(
  full_data_sansON$p_labour___1 == 1 ~ "no labour",
  full_data_sansON$p_labour___2 == 1 ~ "spontaneous",
  full_data_sansON$p_labour___3 == 1 ~ "induced"
)
# summary(as.factor(full_data_sansON$labour_type))

data_NL$labour_type <- factor(case_when(
  data_NL$mode_del2 == "spontaneous vaginal" ~ "spontaneous",
  data_NL$mode_del2 == "no labour cs" ~ "no labour",
  data_NL$mode_del2 == "assisted vaginal"  ~ "induced"
))


## apgar 5
# summary(as.factor(full_data_sansON$apgar5))
# summary(as.factor(data_on$B_APGAR_5))


data_on$apgar5 <- case_when(
  data_on$B_APGAR_5 == "7 or more" ~ "≥7",
  data_on$B_APGAR_5 == "Less than 7" ~ "<7",
  data_on$B_APGAR_5 == "Missing" ~ NA
)
# summary(as.factor(data_on$apgar5))


## apgar 5 largely missing for NL
data_NL$apgar5 <- case_when(
  data_NL$B1_Apgar.Score.5.min < 7 ~ "<7",
  data_NL$B1_Apgar.Score.5.min >= 7 ~ "≥7"
)


## birthweight
# summary(as.factor(data_on$B_infant_weight))
# summary(as.factor(full_data_sansON$bw_cat))

data_on$bw_cat <- case_when(
  data_on$B_infant_weight == "< 2500g" ~ "<2500",
  data_on$B_infant_weight == "> 4000g" ~ ">4000",
  data_on$B_infant_weight == "2500 - 4000g" ~ "2500-4000"
)
# summary(as.factor(data_on$bw_cat))
# summary(as.factor(full_data_sansON$bw_cat))


data_NL$bw_cat <- case_when(
  data_NL$B1_Weight.in.Kilos < 2.5 ~ "<2500",
  data_NL$B1_Weight.in.Kilos <= 4.0 ~ "2500-4000",
  data_NL$B1_Weight.in.Kilos > 4.0 ~ ">4000"
)

## NICU
# summary(as.factor(data_on$NICU))
# summary(as.factor(full_data_sansON$NICU))
# full_data_sansON$NICU[which(full_data_sansON$NICU == "Yes")] <- 1
# full_data_sansON$NICU[which(full_data_sansON$NICU == "No")] <- 0

# data_NL$NICU

## vaccination status
# summary(as.factor(data_on))
# summary(as.factor(full_data_sansON$n_covid))
# summary(as.factor(full_data_sansON$n_covid_date))
# summary(as.factor(full_data_sansON$n_covid_date1))
# summary(as.factor(full_data_sansON$n_covid_date2))
# summary(as.factor(full_data_sansON$n_covid_date3))
# summary(as.factor(full_data_sansON$n_covid_date4))


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
  vacc_date_list <- c(full_data_sansON$n_covid_date[i],full_data_sansON$n_covid_date1[i],full_data_sansON$n_covid_date2[i],full_data_sansON$n_covid_date3[i],full_data_sansON$n_covid_date4[i])
  if(length(which(is.na(vacc_date_list))) > 0){
    vacc_date_list <- vacc_date_list[-which(is.na(vacc_date_list))]
  }
  if(length(which(as.Date(vacc_date_list) > as.Date(full_data_sansON$e_diagnosis[i]))) > 0){
    vacc_date_list <- vacc_date_list[-which(as.Date(vacc_date_list) > as.Date(full_data_sansON$e_diagnosis[i]))]
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
  # print(length(unique(vacc_date_list)))
  data_on$number_of_vaccines[i] <- length(unique(vacc_date_list))
}

summary(as.factor(data_on$number_of_vaccines))


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
# unique(data_on$C_pneumonia)
# unique(full_data_sansON$e_pneumonia)


data_on$pneumonia <- case_when(
  data_on$C_pneumonia == "Yes" ~ 1,
  TRUE ~ 0
)

full_data_sansON$pneumonia <- case_when(
  full_data_sansON$e_pneumonia == "1" ~ 1,
  TRUE ~ 0
)


# Pregnancy-related death - should note that ontario maternal deaths are associated with Covid19
# unique(data_on$H_Maternal_Death_COVID19)
# unique(full_data_sansON$e_death)


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
# unique(full_data_sansON$p_complications_oth_s)
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
# full_data_sansON$ga_del_cat2
unique(data_on$ga_del_cat2)

## preterm labour with other denominator (only cases with gestational age at onset of covid < 37 weeks), we don't have this information yet for ontario
full_data_sansON$covid_pre_37 <- full_data_sansON$ga_at_del - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis))/7.0 < 37
# data_on$covid_pre_37 <- full_data_sansON$ga_at_del - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis))/7.0 < 37




### FOR C-SECTION AND INTRAPARTUM C-SECTION, EXCLUDE PREGNANCY LOSS, ONLY STILL BIRTHS AND LIVE BIRTHS
## c-section


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





##### PMA RISK FACTORS #####

## list here: c("diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis")

###### Maternal comorbidities:
# Diabetes (preexisting prior to pregnancy)
full_data_sansON$diabetes <- case_when(
  full_data_sansON$diabetes == "Yes" ~ 1,
  TRUE ~ 0
)
describeFactors(full_data_sansON$diabetes)
describeFactors(data_on$diabetes)
describeFactors(data_NL$diabetes)

# Hypertension (preexisting prior to pregnancy)

full_data_sansON$htn <- case_when(
  full_data_sansON$htn == "Yes" ~ 1,
  TRUE ~ 0
)

describeFactors(full_data_sansON$htn)
describeFactors(data_on$htn)
describeFactors(data_NL$htn)

# Cardiovascular disease ## EXCLUDE ENTIRELY
describeFactors(full_data_sansON$cvs)
describeFactors(data_on$CVD) ## these don't match with what we have above
describeFactors(data_NL$CVS.New) ## these don't match with what we have above

# Pre-pregnancy BMI 
mean(full_data_sansON$BMI, na.rm = TRUE)
mean(data_on$new_BMI, na.rm = TRUE) 
mean(data_NL$BMI, na.rm = TRUE) 


data_NL$BMI_cat <- factor(case_when(
  data_NL$BMI < 18.5 ~ "<18.5",
  data_NL$BMI < 25 ~ "18.5-24",
  data_NL$BMI < 30 ~ "25-29",
  data_NL$BMI >= 30 ~ "≥30"
), levels = c("<18.5","18.5-24","25-29","≥30"))

full_data_sansON$BMI_cat <- factor(case_when(
  full_data_sansON$BMI < 18.5 ~ "<18.5",
  full_data_sansON$BMI < 25 ~ "18.5-24",
  full_data_sansON$BMI < 30 ~ "25-29",
  full_data_sansON$BMI >= 30 ~ "≥30"
), levels = c("<18.5","18.5-24","25-29","≥30"))

data_on$BMI_cat <- factor(case_when(
  data_on$new_BMI < 18.5 ~ "<18.5",
  data_on$new_BMI < 25 ~ "18.5-24",
  data_on$new_BMI < 30 ~ "25-29",
  data_on$new_BMI >= 30 ~ "≥30"
), levels = c("<18.5","18.5-24","25-29","≥30"))

describeFactors(full_data_sansON$BMI_cat, na.rm = TRUE)
describeFactors(data_on$BMI_cat, na.rm = TRUE) 
describeFactors(data_NL$BMI_cat, na.rm = TRUE) 


# Anemia at the time of COVID-19 diagnosis (defined as Hb < 110) ## EXCLUDE
sum(as.numeric(full_data_sansON$j_hem_anem___1), na.rm = TRUE) ## 977
sum(grepl("1020250",data_on$complication_id)) ## 54 - something is wrong here, lets leave this one out

# Maternal age
describeFactors(full_data_sansON$age_cat2)
describeFactors(data_on$C_maternal_age_group)

data_on$age_cat2 <- factor(data_on$C_maternal_age_group, levels = c("Less than 30","30-35 years","36 years and older"))
data_NL$age_cat2 <- factor(case_when(
  data_NL$AGE < 30 ~ "Less than 30",
  data_NL$AGE < 36 ~ "30-35 years",
  data_NL$AGE >= 36 ~ "36 years and older"
  ),levels = c("Less than 30","30-35 years","36 years and older"))

describeFactors(full_data_sansON$age_cat2)
describeFactors(data_on$age_cat2 )
describeFactors(data_NL$age_cat2)

# Primiparity (parity  = 1) (lots of missing data)

# bc and others

full_data_sansON$h_preterm_noNA <- full_data_sansON$h_preterm
# full_data_sansON$h_preterm_noNA[is.na(full_data_sansON$h_preterm_noNA)] <- 0
full_data_sansON$h_preterm_noNA <- as.numeric(full_data_sansON$h_preterm_noNA)

full_data_sansON$h_term_noNA <- full_data_sansON$h_term
# full_data_sansON$h_term_noNA[is.na(full_data_sansON$h_term_noNA)] <- 0
full_data_sansON$h_term_noNA[full_data_sansON$h_term_noNA == "999"] <- NA
full_data_sansON$h_term_noNA <- as.numeric(full_data_sansON$h_term_noNA)

full_data_sansON$parity <- case_when(
  is.na(full_data_sansON$h_preterm_noNA) ~ full_data_sansON$h_term_noNA,
  is.na(full_data_sansON$h_term_noNA) ~ full_data_sansON$h_preterm_noNA,
  TRUE ~ full_data_sansON$h_preterm_noNA + full_data_sansON$h_term_noNA
  )

full_data_sansON$parity  <- full_data_sansON$parity

# ontario - has 
# 1 = Parity 0
# 2 = Parity 1
# 3 = Parity 2 or more
describeFactors(data_on$parity_cat)

data_on$primiparity <- case_when( 
  data_on$parity_cat == 1 ~ TRUE,
  data_on$parity_cat != 1 ~ FALSE
  )

data_NL$primiparity <- case_when( 
  data_NL$Parity == 0 ~ TRUE,
  data_NL$Parity != 0 ~ FALSE
)

full_data_sansON$primiparity <- case_when( 
  full_data_sansON$parity == 1 ~ TRUE,
  full_data_sansON$parity != 1 ~ FALSE
)


describeFactors(data_NL$primiparity )
describeFactors(full_data_sansON$primiparity)
describeFactors(data_on$primiparity )


### SEARCH FOR ALL OF THESE BELOW IN TEXT COMPLICATIONS?
# HIV
unique(full_data_sansON$k_hiv)

sum(grepl("1020940",data_on$infection_id))

# no NL hiv

full_data_sansON$hiv <- full_data_sansON$k_hiv == "1"
full_data_sansON$hiv[is.na(full_data_sansON$hiv)] <- FALSE
data_on$hiv <- grepl("1020940",data_on$infection_id)
data_NL$hiv <- FALSE

describeFactors(full_data_sansON$hiv)
describeFactors(data_on$hiv)
describeFactors(data_NL$hiv)



# TB coinfection (one case)

# unique(full_data_sansON$k_other_1)
# unique(full_data_sansON$k_other_2)
# unique(full_data_sansON$k_other_3)
# unique(full_data_sansON$k_other_4)
# 
# sum(grepl("tuberculosis",full_data_sansON$k_other_1_name))
# sum(grepl("tuberculosis",full_data_sansON$k_other_2_name))
# sum(grepl("tuberculosis",full_data_sansON$k_other_3_name))
# sum(grepl("tuberculosis",full_data_sansON$k_other_4_name))
# 
# sum(grepl("tb",full_data_sansON$k_other_1_name))
# sum(grepl("tb",full_data_sansON$k_other_2_name))
# sum(grepl("tb",full_data_sansON$k_other_3_name))
# sum(grepl("tb",full_data_sansON$k_other_4_name))

full_data_sansON$k_other_1[grepl("tuberculosis",full_data_sansON$k_other_1_name)]

full_data_sansON$tb <- case_when(
  grepl("tuberculosis",full_data_sansON$k_other_1_name) & full_data_sansON$k_other_1 == "1" ~ TRUE,
  TRUE ~ FALSE
)
sum(full_data_sansON$tb)

data_on$tb <- case_when(
  grepl("1021000",data_on$infection_id) ~ TRUE,
  TRUE ~ FALSE  
)
sum(data_on$tb)

# no NL tb/ tuberculosis
data_NL$tb <- case_when(
  grepl("tuberculosis",data_NL$all_icd) ~ TRUE,
  TRUE ~ FALSE  
)
sum(data_on$tb)


# full_data_sansON$tb <- full_data_sansON$k_hiv == "1"
# full_data_sansON$tb[is.na(full_data_sansON$hiv)] <- FALSE
# data_on$tb <- grepl("1020940",data_on$infection_id)
# data_NL$tb <- FALSE
# 
# describeFactors(full_data_sansON$hiv)
# describeFactors(data_on$hiv)
# describeFactors(data_NL$hiv)



# Syphliis coinfection
full_data_sansON$syphilis <- case_when(
  full_data_sansON$k_syphillis == "1" ~ TRUE,
  TRUE ~ FALSE
)
sum(full_data_sansON$syphilis)


data_on$syphilis <- case_when(
  grepl("1020985",data_on$infection_id) ~ TRUE,
  TRUE ~ FALSE
)
sum(data_on$syphilis)

# 
# unique(full_data_sansON$k_syphillis)
# 
# sum(grepl("1020985",data_on$infection_id))

# no NL syphillis
data_NL$syphilis <- case_when(
  data_NL$Syphilis == "1 REACTIVE" ~ TRUE,
  TRUE ~ FALSE
)

sum(data_NL$syphilis)
# sum(data_NL$Syphilis == "1 REACTIVE")


# 
# # Malaria coinfection - totally unknown
# unique(full_data_sansON$k_other_1)
# unique(full_data_sansON$k_other_2)
# unique(full_data_sansON$k_other_3)
# unique(full_data_sansON$k_other_4)
# unique(full_data_sansON$k_other_1_name)
# unique(full_data_sansON$k_other_2_name)
# unique(full_data_sansON$k_other_3_name)
# unique(full_data_sansON$k_other_4_name)
# 
# 
# 










## DUPLICATE DATA FOR EACH INFANT FOR INFANT OUTCOMES. INFANT OUTCOMES only include live births except still birth and perinatal death (which should include still births)
# Still birth, Perinatal Death, Early Neonatal Death, Neonatal Death, NICU admission at birth, Very low birth weight (<1500 g), low birth weight (<2500g), SGA3, SGA10, 
# Very preterm (<34 week), Preterm (<37 week), Very preterm (using restricted denominator), preterm (using restricted denominator)
## columns in infant data with twins:


## first create duplicate rows for twins and triplets
# full_data_sansON$i_fetusnumber[which(full_data_sansON$i_fetusnumber == "999")] <- NA
full_data_sansON$i_fetusnumber <- as.numeric(full_data_sansON$i_fetusnumber)

## make empty data frame with row for each infant
full_data_sansON_wtwins <- as.data.frame(matrix(data = NA, nrow = sum(full_data_sansON$i_fetusnumber), 
                                                ncol = 15))

colnames(full_data_sansON_wtwins) <- c("a_record_id","fetus_number","s_pe_genitalia","e_diagnosis","s_infdeath","aa_age","NICU","birthweight","r_dob", "SGA3", "SGA10","ga_at_del","stillbirth",
                                       "diabetes","htn")


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
    full_data_sansON_wtwins$r_dob[i+j] <- full_data_sansON$r_dob[i]
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
    full_data_sansON_wtwins$r_dob[i+j] <- full_data_sansON$r_dob[i]
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
    full_data_sansON_wtwins$r_dob[i+j] <- full_data_sansON$r_dob_2[i]
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
    full_data_sansON_wtwins$r_dob[i+j] <- full_data_sansON$r_dob[i]
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
    full_data_sansON_wtwins$r_dob[i+j] <- full_data_sansON$r_dob_2[i]
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
    full_data_sansON_wtwins$r_dob[i+j] <- full_data_sansON$r_dob_3[i]
    full_data_sansON_wtwins$fetus_number[i+j] <- 3
    i <- i + 1
  }
}

## quality_check: 
# full_data_sansON[which(full_data_sansON$a_record_id == "CCP-01-1261"),c("e_diagnosis","aa_age","s_bw_gm","s_bw_gm_2","ga_at_del", "preg_outcome")]


## make data_NL_wtwins
# 
# length(data_NL$NL_COVIDSTUDYID)
# length(unique(data_NL$NL_COVIDSTUDYID))
# 
# sum(data_NL$MULTIPLENEWBORN == "YES")
data_NL_wtwins <- as.data.frame(matrix(data = NA, nrow = sum(as.numeric(!is.na(data_NL$MULTIPLENEWBORN))) + length(data_NL$MULTIPLENEWBORN), ncol = 13))

colnames(data_NL_wtwins) <- c("a_record_id","fetus_number","s_pe_genitalia","e_diagnosis","s_infdeath","aa_age","NICU","birthweight","r_dob", "SGA3", "SGA10","ga_at_del","stillbirth")


## populate the twins data frame
i <- 1
j <- 0
while(i <= dim(data_NL)[1]){
  if(is.na(data_NL$MULTIPLENEWBORN[i])){
    data_NL_wtwins$a_record_id[i+j] <- data_NL$NL_COVIDSTUDYID[i]
    data_NL_wtwins$e_diagnosis[i+j] <- as.Date(data_NL$MT_COLLECTION_DT[i], format = "%d-%b-%y")
    data_NL_wtwins$birthweight[i+j] <- data_NL$B1_Weight.in.Kilos[i]*1000
    data_NL_wtwins$s_infdeath[i+j] <- FALSE
    data_NL_wtwins$aa_age[i+j] <- NA
    data_NL_wtwins$ga_at_del[i+j] <- data_NL$Gestation.in.weeks[i]
    data_NL_wtwins$NICU[i+j] <- data_NL$B1_Care.Unit[i] == "50 Neonatal Intensive Care"
    data_NL_wtwins$s_pe_genitalia[i+j] <- case_when(
      data_NL$B1_Gender[i] %in% c("M Male","Male M","Male") ~ "1",
      data_NL$B1_Gender[i] %in% c("F Female","Female F","Female") ~ "2"
    )
    data_NL_wtwins$stillbirth[i+j] <- data_NL$preg_outcome[i] 
    data_NL_wtwins$r_dob[i+j] <- data_NL$B1_Birth.Date[i]
    data_NL_wtwins$fetus_number[i+j] <- 1
    i <- i + 1
  }
  else if(data_NL$MULTIPLENEWBORN[i] == "YES"){
    data_NL_wtwins$a_record_id[i+j] <- data_NL$NL_COVIDSTUDYID[i]
    data_NL_wtwins$e_diagnosis[i+j] <- as.Date(data_NL$MT_COLLECTION_DT[i], format = "%d-%b-%y")
    data_NL_wtwins$birthweight[i+j] <- data_NL$B1_Weight.in.Kilos[i]*1000
    data_NL_wtwins$s_infdeath[i+j] <- FALSE
    data_NL_wtwins$aa_age[i+j] <- NA
    data_NL_wtwins$ga_at_del[i+j] <- data_NL$Gestation.in.weeks[i]
    data_NL_wtwins$NICU[i+j] <- data_NL$B1_Care.Unit[i] == "50 Neonatal Intensive Care"
    data_NL_wtwins$s_pe_genitalia[i+j] <- case_when(
      data_NL$B1_Gender[i] %in% c("M Male","Male M","Male") ~ "1",
      data_NL$B1_Gender[i] %in% c("F Female","Female F","Female") ~ "2"
    )
    data_NL_wtwins$stillbirth[i+j] <- data_NL$preg_outcome[i]
    data_NL_wtwins$r_dob[i+j] <- data_NL$B1_Birth.Date[i]
    data_NL_wtwins$fetus_number[i+j] <- 1
    j <- j + 1
    data_NL_wtwins$a_record_id[i+j] <- data_NL$NL_COVIDSTUDYID[i]
    data_NL_wtwins$e_diagnosis[i+j] <- as.Date(data_NL$MT_COLLECTION_DT[i], format = "%d-%b-%y")
    data_NL_wtwins$birthweight[i+j] <- data_NL$B2_Weight.in.Kilos[i]*1000
    data_NL_wtwins$s_infdeath[i+j] <- FALSE
    data_NL_wtwins$aa_age[i+j] <- NA
    data_NL_wtwins$ga_at_del[i+j] <- data_NL$Gestation.in.weeks[i]
    data_NL_wtwins$NICU[i+j] <- data_NL$B2_Care.Unit[i] == "50 Neonatal Intensive Care"
    data_NL_wtwins$s_pe_genitalia[i+j] <- case_when(
      data_NL$B2_Gender[i] %in% c("M Male","Male M","Male") ~ "1",
      data_NL$B2_Gender[i] %in% c("F Female","Female F","Female") ~ "2"
    )
    data_NL_wtwins$stillbirth[i+j] <- data_NL$preg_outcome[i]
    data_NL_wtwins$r_dob[i+j] <- data_NL$B2_Birth.Date[i]
    data_NL_wtwins$fetus_number[i+j] <- 2
    i <- i + 1
  }
}


## fix on.dat for twins
on.dat$a_record_id <- on.dat$new_preg_id
on.dat$e_diagnosis <- as.Date(on.dat$C_COVID_diagnosis_date)
on.dat$birthweight <- on.dat$B_infant_weight
on.dat$s_infdeath <- on.dat$neonatal_death_id %in% c("1018200","1018210")
on.dat$aa_age <- NA
on.dat$ga_at_del <- factor(on.dat$B_GA_birth, levels = c("< 28 weeks","28 - 31 weeks","32-33 weeks","34-36 weeks","Term"))
on.dat$NICU <- on.dat$NICU_admission_flag == "Y"
on.dat$s_pe_genitalia <- case_when(
  on.dat$gender_sex_id == "1013880" ~ "1",
  on.dat$gender_sex_id == "1013870" ~ "2"
)
on.dat$stillbirth <- case_when(
  on.dat$pregnancy_outcome_id == 1021030 ~ "live birth",
  on.dat$pregnancy_outcome_id == 1021040 ~ "loss",
  on.dat$pregnancy_outcome_id == 1021050 ~ "loss",
  on.dat$pregnancy_outcome_id == 1021070 ~ "stillbirth",
  on.dat$pregnancy_outcome_id == 1021080 ~ "stillbirth",
  on.dat$pregnancy_outcome_id == 1021090 ~ "stillbirth"
)

on.dat$r_dob <- on.dat$baby_birth_date
on.dat$fetus_number <- duplicated(on.dat$a_record_id) + 1

## merge risk factors with infant outcomes
on.dat <- inner_join(on.dat,data_on[,c("new_preg_id","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","prov","ga_del_cat","ga_del_cat2","icu")], by = c("a_record_id" = "new_preg_id"))
data_NL_wtwins <- inner_join(data_NL_wtwins,data_NL[,c("NL_COVIDSTUDYID","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","prov","ga_del_cat","ga_del_cat2","icu")], by = c("a_record_id" = "NL_COVIDSTUDYID"))
full_data_sansON_wtwins <- inner_join(full_data_sansON_wtwins,full_data_sansON[,c("a_record_id","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","prov","ga_del_cat","ga_del_cat2","icu")], by = c("a_record_id" = "a_record_id"))



### WE SHOULD HAVE 2 DATA SETS FOR EACH OF NL, ONTARIO, AND BC+:
### ONE FOR TWINS, ONE FOR MOTHERS,
### EACH WITH EVERY RISK FACTOR c("diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis")
### MOTHER DATA SETS : data_on, full_data_sansON, data_NL
### INFANT DATA SETS : on.dat, full_data_sansON_wtwins, data_NL_wtwins



## Birth Weight
full_data_sansON_wtwins$birthweight <- as.numeric(full_data_sansON_wtwins$birthweight)

full_data_sansON_wtwins$bw_cat <- case_when(
  full_data_sansON_wtwins$birthweight < 2500 ~ "<2500",
  full_data_sansON_wtwins$birthweight >= 2500 & full_data_sansON_wtwins$birthweight <=4000 ~ "2500-4000",
  full_data_sansON_wtwins$birthweight > 4000 ~ ">4000"
)

data_NL_wtwins$birthweight <- as.numeric(data_NL_wtwins$birthweight)

data_NL_wtwins$bw_cat <- case_when(
  data_NL_wtwins$birthweight < 2500 ~ "<2500",
  data_NL_wtwins$birthweight >= 2500 & data_NL_wtwins$birthweight <=4000 ~ "2500-4000",
  data_NL_wtwins$birthweight > 4000 ~ ">4000"
)

on.dat$bw_cat <- on.dat$birthweight

data_NL_wtwins$s_bw_gm <- data_NL_wtwins$birthweight


## neonatal death
on.dat$neonatal_death <- grepl("1018200",on.dat$neonatal_death_id)
sum(on.dat$neonatal_death)

full_data_sansON_wtwins$neonatal_death <- case_when(
  full_data_sansON_wtwins$s_infdeath == "1" ~ 1,
  TRUE ~ 0
) 

sum(full_data_sansON_wtwins$neonatal_death)

data_NL_wtwins$neonatal_death <- 0

#small for gestational age - cannot calculate for ontario data unfortuntaly as they do not provide birthweight outside of categories.
############################# make weight vectors #####################
##  using intergrowth standards

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

full_data_sansON_wtwins$r_dob <- as.Date(as.character(full_data_sansON_wtwins$r_dob), format = "%Y-%m-%d")

full_data_sansON_wtwins$r_dob <- replace(full_data_sansON_wtwins$r_dob, which(full_data_sansON_wtwins$r_dob == "9999-09-09"), NA)
full_data_sansON_wtwins$s_bw_gm <- full_data_sansON_wtwins$birthweight

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

full_data_sansON_wtwins$SGA10 <- sga.fun(full_data_sansON_wtwins)

full_data_sansON_wtwins$sga10 <- case_when(
  full_data_sansON_wtwins$SGA10 == "not SGA" ~ 0,
  full_data_sansON_wtwins$SGA10 == "SGA" ~ 1
)

# data_NL_wtwins$ga_at_del <- data_NL_wtwins$Gestation.in.weeks
# data_NL_wtwins$s_bw_gm <- data_NL_wtwins$B1_Weight.in.Kilos*1000



data_NL_wtwins$SGA10 <- sga.fun(data_NL_wtwins)

data_NL_wtwins$sga10 <- case_when(
  data_NL_wtwins$SGA10 == "not SGA" ~ 0,
  data_NL_wtwins$SGA10 == "SGA" ~ 1
)

weight.F <- weight.F3
weight.M <- weight.M3

data_NL_wtwins$SGA3 <- sga.fun(data_NL_wtwins)

data_NL_wtwins$sga3 <- case_when(
  data_NL_wtwins$SGA3 == "not SGA" ~ 0,
  data_NL_wtwins$SGA3 == "SGA" ~ 1
)

full_data_sansON_wtwins$SGA3 <- sga.fun(full_data_sansON_wtwins)

full_data_sansON_wtwins$sga3 <- case_when(
  full_data_sansON_wtwins$SGA3 == "not SGA" ~ 0,
  full_data_sansON_wtwins$SGA3 == "SGA" ~ 1
)


## should only be done for infant data
on.dat$NICU <- case_when(
  on.dat$NICU_admission_flag == "Y" ~ 1,
  on.dat$NICU_admission_flag == "N" ~ 0,
  TRUE ~ FALSE
)

# data_NL_wtwins$NICU



############ NOW SUMMARISE MATERNAL OUTCOMESF OR PMA #######

### ALL PREGNANCIES:

## ICU ADMISSION
## VENTILATOIN
## CRITICAL CARE
## PNEUMONIA
full_data_sansON$ventilation[which(is.na(full_data_sansON$ventilation))] <- 0

maternity_sansON <- full_data_sansON[,c("e_diagnosis", "icu","critical_care","ventilation", "pneumonia","a_record_id","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","prov")]

data_on$a_record_id <- data_on$new_preg_id
data_on$e_diagnosis <- data_on$C_COVID_diagnosis_date
data_on$icu[which(is.na(data_on$icu))] <- 0

maternity_ON <- data_on[,c("e_diagnosis", "icu","critical_care","ventilation", "pneumonia","a_record_id","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","prov")]

data_NL$e_diagnosis <- as.Date(data_NL$MT_COLLECTION_DT, format = "%d-%b-%y")
data_NL$critical_care <- data_NL$icu
data_NL$ventilation <- NA
data_NL$pneumonia <- 0
data_NL$a_record_id <- data_NL$NL_COVIDSTUDYID

maternity_NL <- data_NL[,c("e_diagnosis", "icu","critical_care","ventilation", "pneumonia","a_record_id","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","prov")]



# c("diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis")
### ALL COMPLETED PREGNANCIES (Excluding termination):

## PREGNANCY-RELATED DEATH
full_data_sansON$death
data_on$death
data_NL$death <- 0 ## none found in ICD

## HAEMORRHAGE
full_data_sansON$hemorrhage
data_on$hemorrhage
data_NL$hemorrhage <- case_when(
  grepl("O72102",data_NL$all_icd) ~ 1,
  grepl("O72002",data_NL$all_icd) ~ 1,
  TRUE ~ 0
)

## PLACENTAL ABRUPTION
full_data_sansON$placental_abruption
data_on$placental_abruption
data_NL$placental_abruption <- 0 ## none found in ICD

## HYPERTENSIVE DISORDERS OF PREGNANCY
full_data_sansON$htn_preg
data_on$htn_preg
data_NL$htn_preg <- case_when(
  grepl("O13001",data_NL$all_icd) ~ 1,
  grepl("O11001",data_NL$all_icd) ~ 1,
  grepl("O13002",data_NL$all_icd) ~ 1,
  grepl("O16002",data_NL$all_icd) ~ 1,
  TRUE ~ 0
)

## HYPERTENSIVE DISORDERS OF PREGNANCY (diagnosed at or after covid19 onset) - unavailable timing of the diagnosis for NL and ON
full_data_sansON$htn_preg_after
data_on$htn_preg_after <- NA
data_NL$htn_preg_after <- NA

## PRE ECLAMPSIA
full_data_sansON$pre_eclampsia
data_on$pre_eclampsia
data_NL$pre_eclampsia <- case_when(
  grepl("O14901",data_NL$all_icd) ~ 1,
  grepl("O11001",data_NL$all_icd) ~ 1,
  grepl("O14101",data_NL$all_icd) ~ 1,
  grepl("O14902",data_NL$all_icd) ~ 1,
  grepl("O14001",data_NL$all_icd) ~ 1,
  TRUE ~ 0
)


## ECLAMPSIA - only able to identify eclampsia among newfoundland data so far, nothing showing up for seizures in ontario or rest of data. We therefore do not trust this data point.
data_NL$eclampsia <- case_when(
  grepl("O15001",data_NL$all_icd) ~ 1,
  TRUE ~ 0
)
sum(data_NL$eclampsia)

## EMBOLIC DISEASE
describeFactors(full_data_sansON$embolic_disease)
describeFactors(data_on$embolic_disease)
data_NL$embolic_disease <- 0 ## none found in ICD
describeFactors(data_NL$embolic_disease)


## PRETERM LABOUR
describeFactors(full_data_sansON$ga_del_cat2)
describeFactors(data_on$ga_del_cat2)
describeFactors(data_NL$ga_del_cat2)
## PRETERM LABOUR
describeFactors(full_data_sansON$ga_del_cat)
describeFactors(data_on$ga_del_cat)
describeFactors(data_NL$ga_del_cat)
## PRETERM LABOUR (using restricted denominator) - already restricted by the nature of our data


## C-SECTION
describeFactors(full_data_sansON$mode_del)
describeFactors(data_on$mode_del)
describeFactors(data_NL$mode_del)

## INTRAPARTUM C-SECTION
describeFactors(full_data_sansON$intrapartum_cs)
describeFactors(data_on$intrapartum_cs)
data_NL$intrapartum_cs <- NA ## weirdly unavailable for NL

### MAKE NEW DATASETS "completed_pregnancies" for each. This should be the same for ontario and NL as there were no abortions or losses recorded

completed_preg_sansON <- full_data_sansON[-which(full_data_sansON$preg_outcome == "loss"),c("a_record_id","ga_del_cat","ga_del_cat2","embolic_disease","pre_eclampsia","htn_preg_after","htn_preg","placental_abruption","hemorrhage","death","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","preg_outcome","mode_del","intrapartum_cs","prov","e_diagnosis","icu")]
completed_preg_ON <- data_on[-which(data_on$preg_outcome == "loss"),c("a_record_id","ga_del_cat","ga_del_cat2","embolic_disease","pre_eclampsia","htn_preg_after","htn_preg","placental_abruption","hemorrhage","death","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","preg_outcome","mode_del","intrapartum_cs","prov","e_diagnosis","icu")]
completed_preg_NL <- data_NL[,c("a_record_id","ga_del_cat","ga_del_cat2","embolic_disease","pre_eclampsia","htn_preg_after","htn_preg","placental_abruption","hemorrhage","death","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","preg_outcome","mode_del","intrapartum_cs","prov","e_diagnosis","icu")]

### ALL COMPLETED PREGNANCIES ENDING IN A BIRTH / STILLBIRTH AFTER 28 WEEKS GA
## generate new maternity data-sets for this
completed_preg_sansON_2 <- completed_preg_sansON[-which(completed_preg_sansON$preg_outcome == "stillbirth" & completed_preg_sansON$ga_del_cat == "extremely preterm" ),c("a_record_id","ga_del_cat","ga_del_cat2","embolic_disease","pre_eclampsia","htn_preg_after","htn_preg","placental_abruption","hemorrhage","death","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","mode_del","intrapartum_cs","prov","e_diagnosis","icu")]
completed_preg_ON_2 <- completed_preg_ON[-which(completed_preg_ON$preg_outcome == "stillbirth" & completed_preg_ON$ga_del_cat ==  "extremely preterm"),c("a_record_id","ga_del_cat","ga_del_cat2","embolic_disease","pre_eclampsia","htn_preg_after","htn_preg","placental_abruption","hemorrhage","death","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","mode_del","intrapartum_cs","prov","e_diagnosis","icu")]
#none showing up in NL so removing this: -which(completed_preg_NL$preg_outcome == "stillbirth" & completed_preg_NL$ga_del_cat == "extremely preterm")
completed_preg_NL_2 <- completed_preg_NL[,c("a_record_id","ga_del_cat","ga_del_cat2","embolic_disease","pre_eclampsia","htn_preg_after","htn_preg","placental_abruption","hemorrhage","death","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","mode_del","intrapartum_cs","prov","e_diagnosis","icu")]




####### combine all into 1 and add column for prov #####

dim(maternity_sansON)
dim(maternity_ON)
dim(maternity_NL)

maternity_1_all <- rbind(maternity_sansON,maternity_ON)
maternity_1_all <- rbind(maternity_1_all,maternity_NL)


dim(completed_preg_sansON)
dim(completed_preg_ON)
dim(completed_preg_NL)

maternity_2_all <- rbind(completed_preg_sansON,completed_preg_ON)
maternity_2_all <- rbind(maternity_2_all,completed_preg_NL)

dim(completed_preg_sansON_2)
dim(completed_preg_ON_2)
dim(completed_preg_NL_2)

maternity_3_all <- rbind(completed_preg_sansON_2,completed_preg_ON_2)
maternity_3_all <- rbind(maternity_3_all,completed_preg_NL_2)

### now same for infants
colnames(full_data_sansON_wtwins)
colnames(data_NL_wtwins)
on.dat$s_bw_gm <- NA
on.dat$sga10 <- NA
on.dat$sga3 <- NA

full_data_sansON$fetusnumber <- full_data_sansON$i_fetusnumber
on.dat_wtwins <- on.dat[, intersect(colnames(full_data_sansON_wtwins),colnames(on.dat))]
data_NL_wtwins <- on.dat[, intersect(colnames(full_data_sansON_wtwins),colnames(on.dat))]
full_data_sansON_wtwins <- full_data_sansON_wtwins[, intersect(colnames(full_data_sansON_wtwins),colnames(on.dat))]
#dim(on.dat_wtwins)
dim(data_NL_wtwins)
dim(full_data_sansON_wtwins)
on.dat_wtwins$diabetes < - on.dat_wtwins$diabetes.x

infant_all <- rbind(full_data_sansON_wtwins,on.dat_wtwins)
infant_all <- rbind(infant_all,data_NL_wtwins)

## create function for aggregating proportions/confidence intervals/ missingness across risk factors and outcomes
# maternity_1_all
# maternity_2_all
# maternity_3_all
# infant_all

maternity_1_all$month_year <- format(as.Date(maternity_1_all$e_diagnosis), "%Y-%m")
maternity_2_all$month_year <- format(as.Date(maternity_2_all$e_diagnosis), "%Y-%m")
maternity_3_all$month_year <- format(as.Date(maternity_3_all$e_diagnosis), "%Y-%m")
infant_all$month_year <- format(as.Date(infant_all$e_diagnosis), "%Y-%m")




risk_factors <- c("month_year","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis")
outcomes_mat1 <- c("icu","critical_care","ventilation","pneumonia")
outcomes_mat2 <- c("death","hemorrhage","placental_abruption","htn_preg","htn_preg_after","pre_eclampsia","embolic_disease","ga_del_cat","ga_del_cat2")
outcomes_mat3 <- c("mode_del","intrapartum_cs")
outcomes_inf <- c("stillbirth","neonatal_death","NICU","bw_cat","sga10","sga3","ga_del_cat","ga_del_cat2")


maternity_1_all$diabetes <- factor(case_when(
  maternity_1_all$diabetes == 0 ~ FALSE,
  maternity_1_all$diabetes == 1 ~ TRUE))

maternity_1_all$htn <- factor(case_when(
  maternity_1_all$htn == 0 ~ FALSE,
  maternity_1_all$htn == 1 ~ TRUE))

maternity_2_all$diabetes <- factor(case_when(
  maternity_2_all$diabetes == 0 ~ FALSE,
  maternity_2_all$diabetes == 1 ~ TRUE))

maternity_2_all$htn <- factor(case_when(
  maternity_2_all$htn == 0 ~ FALSE,
  maternity_2_all$htn == 1 ~ TRUE))

maternity_2_all$ga_del_cat <- factor(case_when(
  maternity_2_all$ga_del_cat == "term" ~ 0,
  maternity_2_all$ga_del_cat == "late preterm" ~ 0,
  maternity_2_all$ga_del_cat == "moderate preterm" ~ 1,
  maternity_2_all$ga_del_cat == "very preterm" ~ 1,
  maternity_2_all$ga_del_cat == "extremely preterm" ~ 1
))


maternity_2_all$ga_del_cat2 <- factor(case_when(
  maternity_2_all$ga_del_cat2 == "term" ~ 0,
  maternity_2_all$ga_del_cat2 == "preterm" ~ 1
))



maternity_3_all$diabetes <- factor(case_when(
  maternity_3_all$diabetes  == 0 ~ FALSE,
  maternity_3_all$diabetes == 1 ~ TRUE))

maternity_3_all$htn <- factor(case_when(
  maternity_3_all$htn  == 0 ~ FALSE,
  maternity_3_all$htn == 1 ~ TRUE))

infant_all$diabetes <- factor(case_when(
  infant_all$diabetes.x == 0 ~ FALSE,
  infant_all$diabetes.x == 1 ~ TRUE))

infant_all$htn <- factor(case_when(
  infant_all$htn.x == 0 ~ FALSE,
  infant_all$htn.x == 1 ~ TRUE))


library(epitools)

for(k in 1:length(outcomes_mat1)){
  
mat1_spreadsheet1 <- data.frame(matrix(data = NA,nrow = 62, ncol = 8))
colnames(mat1_spreadsheet1) <- c("Risk_Factor","Subgroup","Events","Total","Proportion","Lower_CI","Upper_CI","Missing")

index <- 0

for(i in 1:length(risk_factors)){
  mat1_spreadsheet1$Risk_Factor[1] <- "none"
  mat1_spreadsheet1$Subgroup[1] <- "all"
  mat1_spreadsheet1$Total[1] <- dim(maternity_1_all)[1]
  mat1_spreadsheet1$Events[1] <- sum(maternity_1_all[,outcomes_mat1[k]] == 1, na.rm = TRUE)
  mat1_spreadsheet1$Proportion[1] <- mat1_spreadsheet1$Events[1]/mat1_spreadsheet1$Total[1]
  # 
  # risk_factor_level_vector <- maternity_1_all[,risk_factors[i]] == levels(factor(maternity_1_all[,risk_factors[i]]))[j]
  # outcome_level_vector <- maternity_1_all[,outcomes_mat1[k]] == 1
  mat1_spreadsheet1$Lower_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$lower
  mat1_spreadsheet1$Upper_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$upper
  mat1_spreadsheet1$Missing[1] <- sum(is.na(maternity_1_all[,outcomes_mat1[k]]))
  
  for(j in 1:length(levels(factor(maternity_1_all[,risk_factors[i]])))){
    mat1_spreadsheet1$Risk_Factor[j+ 1 + index] <- risk_factors[i]
    mat1_spreadsheet1$Subgroup[j + 1 + index] <- levels(factor(maternity_1_all[,risk_factors[i]]))[j]
    mat1_spreadsheet1$Total[j+1+ index] <- sum(maternity_1_all[,risk_factors[i]] == levels(factor(maternity_1_all[,risk_factors[i]]))[j],
                                               na.rm = TRUE)
    mat1_spreadsheet1$Events[j+1+ index] <- sum(maternity_1_all[which(maternity_1_all[,outcomes_mat1[k]] == 1),
                                                                risk_factors[i]] == levels(factor(maternity_1_all[,risk_factors[i]]))[j], 
                                                na.rm = TRUE)
    mat1_spreadsheet1$Proportion[j+1+ index] <- mat1_spreadsheet1$Events[j+1]/mat1_spreadsheet1$Total[j+1]
    # 
    # risk_factor_level_vector <- maternity_1_all[,risk_factors[i]] == levels(factor(maternity_1_all[,risk_factors[i]]))[j]
    # outcome_level_vector <- maternity_1_all[,outcomes_mat1[k]] == 1
    
    mat1_spreadsheet1$Lower_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$lower
    mat1_spreadsheet1$Upper_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$upper
    mat1_spreadsheet1$Missing[j+1+ index] <- sum(is.na(maternity_1_all[maternity_1_all[,risk_factors[i]] == levels(factor(maternity_1_all[,risk_factors[i]]))[j],outcomes_mat1[k]])
                                                 + is.na(maternity_1_all[maternity_1_all[,risk_factors[i]] == levels(factor(maternity_1_all[,risk_factors[i]]))[j],risk_factors[i]]) > 0)
  }
  
  index <- index + j
}

write.csv(mat1_spreadsheet1,paste0(outcomes_mat1[k],"11-17-2023.csv"))

}

#write.csv(mat1_spreadsheet1, here("mat1"))



for(k in 1:length(outcomes_mat2)){
  
  mat1_spreadsheet1 <- data.frame(matrix(data = NA, nrow = 65, ncol = 8))
  colnames(mat1_spreadsheet1) <- c("Risk_Factor","Subgroup","Events","Total","Proportion","Lower_CI","Upper_CI","Missing")
  
  index <- 0
  
  for(i in 1:length(risk_factors)){
    mat1_spreadsheet1$Risk_Factor[1] <- "none"
    mat1_spreadsheet1$Subgroup[1] <- "all"
    mat1_spreadsheet1$Total[1] <- dim(maternity_2_all)[1]
    mat1_spreadsheet1$Events[1] <- sum(maternity_2_all[,outcomes_mat2[k]] == 1, na.rm = TRUE)
    mat1_spreadsheet1$Proportion[1] <- mat1_spreadsheet1$Events[1]/mat1_spreadsheet1$Total[1]
    # 
    # risk_factor_level_vector <- maternity_2_all[,risk_factors[i]] == levels(factor(maternity_2_all[,risk_factors[i]]))[j]
    # outcome_level_vector <- maternity_2_all[,outcomes_mat2[k]] == 1
    mat1_spreadsheet1$Lower_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$lower
    mat1_spreadsheet1$Upper_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$upper
    mat1_spreadsheet1$Missing[1] <- sum(is.na(maternity_2_all[,outcomes_mat2[k]]))
    
    for(j in 1:length(levels(factor(maternity_2_all[,risk_factors[i]])))){
      mat1_spreadsheet1$Risk_Factor[j+ 1 + index] <- risk_factors[i]
      mat1_spreadsheet1$Subgroup[j + 1 + index] <- levels(factor(maternity_2_all[,risk_factors[i]]))[j]
      mat1_spreadsheet1$Total[j+1+ index] <- sum(maternity_2_all[,risk_factors[i]] == levels(factor(maternity_2_all[,risk_factors[i]]))[j], na.rm = TRUE)
      mat1_spreadsheet1$Events[j+1+ index] <- sum(maternity_2_all[which(maternity_2_all[,outcomes_mat2[k]] == 1),risk_factors[i]] == levels(factor(maternity_2_all[,risk_factors[i]]))[j], na.rm = TRUE)
      mat1_spreadsheet1$Proportion[j+1+ index] <- mat1_spreadsheet1$Events[j+1]/mat1_spreadsheet1$Total[j+1]
      # 
      # risk_factor_level_vector <- maternity_2_all[,risk_factors[i]] == levels(factor(maternity_2_all[,risk_factors[i]]))[j]
      # outcome_level_vector <- maternity_2_all[,outcomes_mat2[k]] == 1
      
      mat1_spreadsheet1$Lower_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$lower
      mat1_spreadsheet1$Upper_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$upper
      mat1_spreadsheet1$Missing[j+1+ index] <- sum(is.na(maternity_2_all[,outcomes_mat2[k]]) + is.na(maternity_2_all[,risk_factors[i]]) > 0)
    }
    
    index <- index + j
  }
  
  write.csv(mat1_spreadsheet1,paste0(outcomes_mat2[k],"11-17-2023.csv"))
  
}




maternity_3_all$mode_del <- factor(case_when(
  maternity_3_all$mode_del == "cs" ~ 1,
  maternity_3_all$mode_del == "vaginal" ~ 0
))

# maternity_3_all$intrapartum_cs




for(k in 1:length(outcomes_mat3)){
  
  mat1_spreadsheet1 <- data.frame(matrix(data = NA,nrow = 63, ncol = 8))
  colnames(mat1_spreadsheet1) <- c("Risk_Factor","Subgroup","Events","Total","Proportion","Lower_CI","Upper_CI","Missing")
  
  index <- 0
  
  for(i in 1:length(risk_factors)){
    mat1_spreadsheet1$Risk_Factor[1] <- "none"
    mat1_spreadsheet1$Subgroup[1] <- "all"
    mat1_spreadsheet1$Total[1] <- dim(maternity_3_all)[1]
    mat1_spreadsheet1$Events[1] <- sum(maternity_3_all[,outcomes_mat3[k]] == 1, na.rm = TRUE)
    mat1_spreadsheet1$Proportion[1] <- mat1_spreadsheet1$Events[1]/mat1_spreadsheet1$Total[1]
    # 
    # risk_factor_level_vector <- maternity_3_all[,risk_factors[i]] == levels(factor(maternity_3_all[,risk_factors[i]]))[j]
    # outcome_level_vector <- maternity_3_all[,outcomes_mat3[k]] == 1
    mat1_spreadsheet1$Lower_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$lower
    mat1_spreadsheet1$Upper_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$upper
    mat1_spreadsheet1$Missing[1] <- sum(is.na(maternity_3_all[,outcomes_mat3[k]]))
    
    for(j in 1:length(levels(factor(maternity_3_all[,risk_factors[i]])))){
      mat1_spreadsheet1$Risk_Factor[j+ 1 + index] <- risk_factors[i]
      mat1_spreadsheet1$Subgroup[j + 1 + index] <- levels(factor(maternity_3_all[,risk_factors[i]]))[j]
      mat1_spreadsheet1$Total[j+1+ index] <- sum(maternity_3_all[,risk_factors[i]] == levels(factor(maternity_3_all[,risk_factors[i]]))[j], na.rm = TRUE)
      mat1_spreadsheet1$Events[j+1+ index] <- sum(maternity_3_all[which(maternity_3_all[,outcomes_mat3[k]] == 1),risk_factors[i]] == levels(factor(maternity_3_all[,risk_factors[i]]))[j], na.rm = TRUE)
      mat1_spreadsheet1$Proportion[j+1+ index] <- mat1_spreadsheet1$Events[j+1]/mat1_spreadsheet1$Total[j+1]
      # 
      # risk_factor_level_vector <- maternity_3_all[,risk_factors[i]] == levels(factor(maternity_3_all[,risk_factors[i]]))[j]
      # outcome_level_vector <- maternity_3_all[,outcomes_mat3[k]] == 1
      
      mat1_spreadsheet1$Lower_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$lower
      mat1_spreadsheet1$Upper_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$upper
      mat1_spreadsheet1$Missing[j+1+ index] <- sum(is.na(maternity_3_all[,outcomes_mat3[k]]) + is.na(maternity_3_all[,risk_factors[i]]) > 0)
    }
    
    index <- index + 1:length(levels(factor(maternity_3_all[,risk_factors[i]])))
  }
  
  write.csv(mat1_spreadsheet1,paste0(outcomes_mat3[k],"11-17-2023.csv"))
  
}

write.csv(mat1_spreadsheet1, here("mat2.csv"))

unique(infant_all$bw_cat)

infant_all$stillbirth <- factor(case_when(
  infant_all$stillbirth == "stillbirth" ~ 1,
  infant_all$stillbirth == "live birth" ~ 0,
  infant_all$stillbirth == "loss" ~ 0,
))

infant_all$NICU <- factor(case_when(
  infant_all$NICU == "1" ~ 1,
  infant_all$NICU == "0" ~ 0,
  infant_all$NICU == "999" ~ 0,
  infant_all$NICU == "FALSE" ~ 0,
  infant_all$NICU == "TRUE" ~ 1
))

infant_all$bw_cat <- factor(case_when(
  infant_all$bw_cat == "<2500" ~ 1,
  infant_all$bw_cat == "2500-4000" ~ 0,
  infant_all$bw_cat == ">4000" ~ 0,
  infant_all$bw_cat == "" ~ 0,
  infant_all$bw_cat == "2500 - 4000g" ~ 0,
  infant_all$bw_cat == "> 4000g" ~ 0,
  infant_all$bw_cat == "< 2500g"  ~ 1
))


unique(infant_all$sga10)

unique(infant_all$sga3)


infant_all$ga_del_cat <- factor(case_when(
  infant_all$ga_del_cat == "term" ~ 0,
  infant_all$ga_del_cat == "late preterm" ~ 0,
  infant_all$ga_del_cat == "moderate preterm" ~ 1,
  infant_all$ga_del_cat == "very preterm" ~ 1,
  infant_all$ga_del_cat == "extremely preterm" ~ 1
))


infant_all$ga_del_cat2 <- factor(case_when(
  infant_all$ga_del_cat2 == "term" ~ 0,
  infant_all$ga_del_cat2 == "preterm" ~ 1
))


for(k in 1:length(outcomes_inf)){
  
  mat1_spreadsheet1 <- data.frame(matrix(data = NA, nrow = 61, ncol = 8))
  colnames(mat1_spreadsheet1) <- c("Risk_Factor","Subgroup","Events","Total","Proportion","Lower_CI","Upper_CI","Missing")
  
  index <- 0
  
  for(i in 1:length(risk_factors)){
    mat1_spreadsheet1$Risk_Factor[1] <- "none"
    mat1_spreadsheet1$Subgroup[1] <- "all"
    mat1_spreadsheet1$Total[1] <- dim(infant_all)[1]
    mat1_spreadsheet1$Events[1] <- sum(infant_all[,outcomes_inf[k]] == 1, na.rm = TRUE)
    mat1_spreadsheet1$Proportion[1] <- mat1_spreadsheet1$Events[1]/mat1_spreadsheet1$Total[1]
    # 
    # risk_factor_level_vector <- infant_all[,risk_factors[i]] == levels(factor(infant_all[,risk_factors[i]]))[j]
    # outcome_level_vector <- infant_all[,outcomes_inf[k]] == 1
    mat1_spreadsheet1$Lower_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$lower
    mat1_spreadsheet1$Upper_CI[1] <- binom.exact(mat1_spreadsheet1$Events[1],mat1_spreadsheet1$Total[1])$upper
    mat1_spreadsheet1$Missing[1] <- sum(is.na(infant_all[,outcomes_inf[k]]))
    
    for(j in 1:length(levels(factor(infant_all[,risk_factors[i]])))){
      mat1_spreadsheet1$Risk_Factor[j+ 1 + index] <- risk_factors[i]
      mat1_spreadsheet1$Subgroup[j + 1 + index] <- levels(factor(infant_all[,risk_factors[i]]))[j]
      mat1_spreadsheet1$Total[j+1+ index] <- sum(infant_all[,risk_factors[i]] == levels(factor(infant_all[,risk_factors[i]]))[j], na.rm = TRUE)
      mat1_spreadsheet1$Events[j+1+ index] <- sum(infant_all[which(infant_all[,outcomes_inf[k]] == 1),risk_factors[i]] == levels(factor(infant_all[,risk_factors[i]]))[j], na.rm = TRUE)
      mat1_spreadsheet1$Proportion[j+1+ index] <- mat1_spreadsheet1$Events[j+1]/mat1_spreadsheet1$Total[j+1]
      # 
      # risk_factor_level_vector <- infant_all[,risk_factors[i]] == levels(factor(infant_all[,risk_factors[i]]))[j]
      # outcome_level_vector <- infant_all[,outcomes_inf[k]] == 1
      
      mat1_spreadsheet1$Lower_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$lower
      mat1_spreadsheet1$Upper_CI[j+1+ index] <- binom.exact(mat1_spreadsheet1$Events[j+1],mat1_spreadsheet1$Total[j+1])$upper
      mat1_spreadsheet1$Missing[j+1+ index] <- sum(is.na(infant_all[,outcomes_inf[k]]) + is.na(infant_all[,risk_factors[i]]) > 0)
    }
    
    index <- index + j
  }
  
  write.csv(mat1_spreadsheet1,paste0(outcomes_inf[k],"11-17-2023.csv"))
  
}


write.csv(mat1_spreadsheet1, here("mat3.csv"))















# A quick summary for Dr. Money

risk_factors <- c("month_year","diabetes","htn","BMI_cat","age_cat2","primiparity","hiv","tb","syphilis","icu")
outcomes_mat1 <- c("icu","critical_care","ventilation","pneumonia")
outcomes_mat2 <- c("death","hemorrhage","placental_abruption","htn_preg","htn_preg_after","pre_eclampsia","embolic_disease","ga_del_cat","ga_del_cat2")
outcomes_mat3 <- c("mode_del","intrapartum_cs")
outcomes_inf <- c("stillbirth","neonatal_death","NICU","bw_cat","sga10","sga3","ga_del_cat","ga_del_cat2")

maternity_1_all$icu
maternity_2_all$icu
maternity_3_all$icu
infant_all$icu



getT1Stat <- function(data_set ,riskfactor, outcome, digits=2, useNA = "no"){
  getDescriptionStatsBy(data_set[, riskfactor],
                        data_set[, outcome],
                        add_total_col=TRUE,
                        show_all_values=TRUE,
                        hrzl_prop=TRUE,
                        statistics = TRUE,
                        html=TRUE,
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data_1 <- list()
j <- 1


# risk_factors <- risk_factors[-1]
section1 <- c(risk_factors,outcomes_mat1)

for(i in 1:length(section1)){
  print(section1[i])
  maternity_1_all[,section1[i]] <- factor(maternity_1_all[,section1[i]])
  print(getT1Stat(maternity_1_all,section1[i],outcomes_mat1[j]))
  table_data_1[[section1[i]]] <- getT1Stat(maternity_1_all,section1[i],outcomes_mat1[j])
}

section1 <- c(risk_factors,outcomes_mat2)

for(i in 1:length(section1)){
  print(section1[i])
  maternity_2_all[,section1[i]] <- factor(maternity_2_all[,section1[i]])
  print(getT1Stat(maternity_2_all,section1[i],outcomes_mat1[j]))
  table_data_1[[section1[i]]] <- getT1Stat(maternity_2_all,section1[i],outcomes_mat1[j])
}

section1 <- c(risk_factors,outcomes_mat3)

for(i in 1:length(section1)){
  print(section1[i])
  maternity_3_all[,section1[i]] <- factor(maternity_3_all[,section1[i]])
  print(getT1Stat(maternity_3_all,section1[i],outcomes_mat1[j]))
  table_data_1[[section1[i]]] <- getT1Stat(maternity_3_all,section1[i],outcomes_mat1[j])
}


# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data_1)) {
  output_data <- rbind(output_data,
                       table_data_1[[varlabel]])
  rgroup <- c(rgroup,
              varlabel)
  n.rgroup <- c(n.rgroup,
                nrow(table_data_1[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", paste(outcomes_mat1[j]))
n.cgroup <- c(1, 3)
colnames(output_data) <- gsub(paste0("[ ]*",outcomes_mat1[j]), "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()













## infant stuff for dr. money




getT1Stat <- function(data_set ,riskfactor, outcome, digits=2, useNA = "no"){
  getDescriptionStatsBy(data_set[, riskfactor],
                        data_set[, outcome],
                        add_total_col=TRUE,
                        show_all_values=TRUE,
                        hrzl_prop=TRUE,
                        statistics = TRUE,
                        html=TRUE,
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data_1 <- list()
j <- 1


# risk_factors <- risk_factors[-1]
section1 <- c(risk_factors,outcomes_inf)

for(i in 1:length(section1)){
  print(section1[i])
  infant_all[,section1[i]] <- factor(infant_all[,section1[i]])
  print(getT1Stat(infant_all,section1[i],outcomes_inf[j]))
  table_data_1[[section1[i]]] <- getT1Stat(infant_all,section1[i],outcomes_inf[j])
}


# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data_1)) {
  output_data <- rbind(output_data,
                       table_data_1[[varlabel]])
  rgroup <- c(rgroup,
              varlabel)
  n.rgroup <- c(n.rgroup,
                nrow(table_data_1[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", paste(outcomes_mat1[j]))
n.cgroup <- c(1, 3)
colnames(output_data) <- gsub(paste0("[ ]*",outcomes_mat1[j]), "", colnames(output_data))

htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()









library(epitools)
# 
# # Now merge everything into a matrix
# # and create the rgroup & n.rgroup variabels
# rgroup <- c()
# n.rgroup <- c()
# output_data <- NULL
# for (varlabel in names(table_data_1)) {
#   
#   rgroup <- c(rgroup, 
#               varlabel)
#   n.rgroup <- c(n.rgroup, 
#                 nrow(table_data_1[[varlabel]]))
#   # ## calculate relative risks +/- 
#   # # fo <- formula(paste0("pre_term ~ ",varlabel))
#   # # risk_lm <- glm(fo, data = mat_for_table2, family = binomial)
#   # if(!is.numeric(mat_for_table2[,varlabel])){
#   #   table_cont <- table(mat_for_table2[,varlabel],mat_for_table2$pre_term)
#   #   risk_ratio1 <- riskratio(table_cont)
#   #   risk_ratio_measure <- risk_ratio1$measure
#   #   risk_ratio_matrix <- matrix(data = NA,nrow = dim(risk_ratio_measure)[1], ncol = 1)  
#   #   
#   #   for(i in 1:dim(risk_ratio_matrix)[1]){
#   #     risk_ratio_matrix[i,1] <- paste(round(risk_ratio_measure[i,1], digits = 2), "(",round(risk_ratio_measure[i,2],digits = 2),"-",round(risk_ratio_measure[i,3], digits = 2),")")
#   #     if(risk_ratio_matrix[i,1] == "1 ( NA - NA )" ){
#   #       risk_ratio_matrix[i,1] <- "reference"
#   #     }
#   #   }
#   #   colnames(risk_ratio_matrix) <- "Risk Ratio"
#   
#   ## calculate lower and upper 95% CIs as well as number of NAs for each risk factor
#   fisher.test(factor(maternity_1_all[,c(varlabel)]),factor(maternity_1_all[,outcomes_mat1[j]]))
#     output_data <- rbind(output_data, cbind(table_data_1[[varlabel]],lower_cis,upper_cis,num_nas))
#   }
#   
# }
# 
# 
# 
# # Add a column spanner for the columns
# cgroup <- c("", "preterm")
# n.cgroup <- c(1, 4)
# colnames(output_data) <- gsub("[ ]*preterm", "", colnames(output_data))
# 
# htmlTable::htmlTable(output_data, align = "rrrr",
#                      rgroup = rgroup, n.rgroup = n.rgroup,
#                      css.rgroup.sep = "",
#                      cgroup = cgroup,
#                      n.cgroup = n.cgroup,
#                      rowlabel = "",
#                      caption = "",
#                      ctable = TRUE)  %>% htmltools::html_print()
# 
# 
#   
  
  
  
  





















#### SUMMARISE BY PROVINCE AND DATE RANGE #####
full_data_sansON %>% group_by(prov) %>% summarise(n = length(unique(a_record_id)), min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))

# full_data_sansON_duplicated %>% group_by(prov) %>% summarise(n = length(a_record_id), min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))
# full_data_sansON_duplicated$e_diagnosis <- as.Date(full_data_sansON_duplicated$e_diagnosis)

data_on %>% summarise(n=n(),min_date = min(as.Date(C_COVID_diagnosis_date), na.rm = TRUE), max_date = max(as.Date(C_COVID_diagnosis_date), na.rm = TRUE))


full_data_sansON$e_diagnosis <- as.Date(full_data_sansON$e_diagnosis)

full_data_sansON$a_date <- as.Date(full_data_sansON$a_date)
# data_on$C_COVID_diagnosis_date

partial_data_sansON_1 <- full_data_sansON[,c("age_cat2","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","e_diagnosis","prov","ga_del_cat","ga_del_cat2","gravida","oxygen","mode_del","preg_outcome","multiple_preg","history_preterm","labour_type","apgar5","bw_cat","number_of_vaccines","sga3","sga10","ventilation","critical_care","pneumonia","death","hemorrhage","placental_abruption","htn_preg","htn_preg_after","pre_eclampsia","embolic_disease","covid_pre_37","mode_del","intrapartum_cs","stillbirth","neonatal_death")] 
# partial_data_sansON_2 <- full_data_sansON_duplicated[,c("age_cat2","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","e_diagnosis","prov")] 

data_on$prov <- "ON"

partial_data_ON <- data_on[,c("C_maternal_age_group","BMI_cat","htn","diabetes","eth_cat","hospitalization","icu","NICU","C_COVID_diagnosis_date","prov","ga_del_cat","ga_del_cat2","gravida","oxygen","mode_del","preg_outcome","multiple_preg","history_preterm","labour_type","apgar5","bw_cat","number_of_vaccines","SGA3","SGA10","ventilation","critical_care","pneumonia","death","hemorrhage","placental_abruption","htn_preg","htn_preg_after","pre_eclampsia","embolic_disease","covid_pre_37","mode_del","intrapartum_cs","stillbirth","neonatal_death")]
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
combined_data <- rbind(combined_data,data_NL_partial) 

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










## 2+ vs no vaccination table for omicron vs delta vs whatever (in severe outcomes manuscript.)



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






