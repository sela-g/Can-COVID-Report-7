## infant outcomes Gal Av-Gay June 10 2023
setwd("/Users/mac/Desktop/CANCOVID_2.0/CANCOVID")
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
data_on <- read.csv("data_on_06_13_2023.csv", header = TRUE)

colnames(data_on)[which(colnames(data_on) == "C_maternal_age_group")] <- "age_cat2"      
colnames(data_on)[which(colnames(data_on) == "C_COVID_diagnosis_date")] <- "e_diagnosis"      
colnames(data_on)[which(colnames(data_on) == "SGA10")] <- "sga2"      

# make sure all ICU also get counted as hospitalized here to start
full_data_sansON$hospitalization[which(full_data_sansON$icu == 1)] <- 1
data_on$hospitalization[which(data_on$icu == 1)] <- 1


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

#hypertensive disorder of pregnancy double check this
summary(as.factor(data_on$hyper_final))
data_on$l_htn <- case_when(
  data_on$hyper_final == 2 ~ 1,
  TRUE ~ 0
)
summary(as.factor(data_on$l_htn))


summary(as.factor(full_data_sansON$l_htn))

full_data_sansON$l_htn[which(full_data_sansON$l_htn == 999)] <- NA

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

summary(as.factor(data_on$B_GA_birth))
summary(as.factor(data_on$delivery_date))
summary(as.factor(data_on$e_diagnosis))

tail(as.Date(data_on$delivery_date)) - 14
tail( as.Date(data_on$delivery_date) - as.Date(data_on$e_diagnosis) - 37*7)

data_on$preg_length <- case_when(
  data_on$B_GA_birth  == "Term" ~ 280,
  data_on$B_GA_birth  == "34-36 weeks" ~ 280 - 6*7,
  data_on$B_GA_birth  == "32-33 weeks" ~ 280  - 8*7,
  data_on$B_GA_birth  == "28 - 31 weeks" ~ 280 - 12*7,
  data_on$B_GA_birth  == "< 28 weeks" ~ 280 - 13*7
)

data_on$preg_length[is.na(data_on$preg_length)]  <- 280

data_on$time_of_diag <- case_when(
  data_on$preg_length - (as.Date(data_on$delivery_date) - as.Date(data_on$e_diagnosis)) < 7*14 ~ "1st Trimester",
  data_on$preg_length - (as.Date(data_on$delivery_date) - as.Date(data_on$e_diagnosis)) < 7*26 ~ "2nd Trimester",
  data_on$preg_length - (as.Date(data_on$delivery_date) - as.Date(data_on$e_diagnosis)) < 7*40 ~ "3rd Trimester"
)

summary(as.factor(data_on$time_of_diag ))


full_data_sansON$preg_length <- 280 - (as.Date(full_data_sansON$i_deliverydate_est) - as.Date(full_data_sansON$r_dob))
full_data_sansON$preg_length[is.na(full_data_sansON$preg_length)]  <- 280

full_data_sansON$time_of_diag <- case_when(
  full_data_sansON$preg_length - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis)) < 7*14 ~ "1st Trimester",
  full_data_sansON$preg_length - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis)) < 7*26 ~ "2nd Trimester",
  full_data_sansON$preg_length - (as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$e_diagnosis)) < 7*40 ~ "3rd Trimester"
)

summary(as.factor(full_data_sansON$time_of_diag))



# maternal chorioamnionitis
describeFactors(full_data_sansON$p_chorio)

full_data_sansON$p_chorio[which(full_data_sansON$p_chorio == 999)] <- NA

# test_chorio <- full_data_sansON %>% group_by(p_chorio, prov) %>% summarise (n=n())

describeFactors(data_on$infection_id) # 1020987 = suspected chorio
data_on$p_chorio <- case_when(
  data_on$delivered == 0 ~ NA_real_,
  grepl("1020987", data_on$infection_id) ~ 1,
  TRUE ~ 0
)

describeFactors(data_on$p_chorio) # way less cases in ontario for some reason

## maternal mortality
describeFactors(data_on$H_Maternal_Death_COVID19)
describeFactors(full_data_sansON$e_death)

data_on$e_death <- data_on$H_Maternal_Death_COVID19
data_on$H_Maternal_Death_COVID19[which(data_on$H_Maternal_Death_COVID19 == "")] <- "No"
data_on$H_Maternal_Death_COVID19[which(is.na(data_on$H_Maternal_Death_COVID19))] <- "No"
data_on$H_Maternal_Death_COVID19[which(data_on$H_Maternal_Death_COVID19 == "Unknown")] <- "No"

full_data_sansON$e_death[which(full_data_sansON$e_death == 999)] <- "No"
full_data_sansON$e_death[which(full_data_sansON$e_death == 1)] <- "Yes"
full_data_sansON$e_death[which(full_data_sansON$e_death == 0)] <- "No"
full_data_sansON$e_death[which(is.na(full_data_sansON$e_death))] <- "No"




#### remove twins / duplicate data for twins then combine
#### check to see twin delivery dates not more than a few days apart


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

# gestational age done
# birthweight

full_data_sansON$bw_cat <- factor(full_data_sansON$bw_cat, levels = c("<2500","2500-4000",">4000"))
unique(full_data_sansON$bw_cat)

data_on$bw_cat <- factor(data_on$bw_cat, levels = c("<2500","2500-4000",">4000") )
unique(data_on$bw_cat)

# sex
sexcheck<- full_data_sansON %>% group_by(prov, s_pe_genitalia) %>% summarise(n = n())

full_data_sansON$infant_sex <- case_when(
  full_data_sansON$s_pe_genitalia == "1" ~ "Male",
  full_data_sansON$s_pe_genitalia == "2" ~ "Female",
  full_data_sansON$s_pe_genitalia == "3" ~ "Ambiguous"
)

describeFactors(full_data_sansON$infant_sex)

unique(data_on$gender_sex_id)

data_on$infant_sex <- case_when(
  data_on$gender_sex_id == "1013870" ~ "Female",
  data_on$gender_sex_id == "1013880" ~ "Male"
)


## infant covid test 
full_data_sansON <- full_data_sansON %>% 
  mutate(
    cov_test_inf_res = case_when(
      u_naso1_res == 1 | u_naso2_res == 1 | u_naso3_res == 1 | u_other_res == 1 ~ 1,
      is.na(u_done) ~ NA_real_,
      TRUE ~ 0
    )
  )

describeFactors(full_data_sansON$cov_test_inf_res)

data_on <- data_on %>% 
  mutate(
    cov_test_inf_res = case_when(
      H_newborn_NP_1_result == "1: Positive" ~ 1,
      H_newborn_NP_1_result == "2: Negative" ~ 0
    )
  )



## smoking
describeFactors(full_data_sansON$smoking)

full_data_sansON$smoking <- case_when(
  full_data_sansON$i_smokingd == 1 ~ "Yes",
  full_data_sansON$i_smokingd == 0 ~ "No",
  full_data_sansON$i_smokingd == 999 ~ NA
)

describeFactors(data_on$any_smoking)
data_on$smoking <- case_when(
  data_on$delivered == 0 ~ NA_character_,
  data_on$any_smoking == 1 ~ "Yes",
  data_on$any_smoking == 0 ~ "No"
)


## alcohol use
describeFactors(full_data_sansON$i_bingedrinking)
full_data_sansON$i_bingedrinking[which(full_data_sansON$i_bingedrinking == 999)] <- NA
describeFactors(data_on$preg_expos_alcohol_id) # 1020444 = bingedrinking
data_on$i_bingedrinking <- case_when(
  data_on$delivered == 0 ~ NA_real_,
  data_on$preg_expos_alcohol_id == 1020444 ~ 1,
  data_on$preg_expos_alcohol_id != 1020444 ~ 0
)

describeFactors(data_on$i_bingedrinking) # 1020444 = bingedrinking

describeFactors(data_on$cov_test_inf_res)
 
## immunization dates

data_on$n_covid_date1 <- data_on$H_Immunization_Date1
data_on$n_covid_date2 <- data_on$H_Immunization_Date2
data_on$n_covid_date3 <- data_on$H_Immunization_Date3

# id
data_on$a_record_id <- paste0(data_on$new_preg_id, "ON")

#date
data_on$e_hosp_date <- data_on$C_hospitalized_admission_date



## covid time period
describeFactors(full_data_sansON$covid_period)
describeFactors(data_on$covid_period)

full_data_sansON$covid_period <- factor(full_data_sansON$covid_period, levels = c("pre-Delta","Delta","Omicron"))
data_on$covid_period <- factor(data_on$covid_period, levels = c("pre-Delta","Delta","Omicron"))


#####  MATERNAL OUTCOMES COMBINED ####



data.h <- rbind(
  full_data_sansON[, c("a_record_id", "hospitalization", "icu", "age_cat2", "BMI_cat", "eth_cat", "time_of_diag", 
           "ga_del_cat","ga_del_cat2", "prov", "e_hosp_date", "l_htn", "gestational_diabetes", "smoking", 
           "i_bingedrinking", "preg_outcome", "e_death", "inv_mech_vent", "n_covid_date1", "n_covid_date2", "n_covid_date3", "number_of_vaccines","covid_period")], 
  data_on[-which(duplicated(data_on$new_preg_id)), c("a_record_id", "hospitalization", "icu", "age_cat2", "BMI_cat", "eth_cat", "time_of_diag", 
           "ga_del_cat","ga_del_cat2", "prov", "e_hosp_date", "l_htn", "gestational_diabetes", "smoking", 
           "i_bingedrinking", "preg_outcome", "e_death", "inv_mech_vent", "n_covid_date1", "n_covid_date2", "n_covid_date3", "number_of_vaccines","covid_period")])



data.h$preterm <- data.h$ga_del_cat2

describeFactors(full_data_sansON$n_covid_date3)
describeFactors(data_on$ga_del_cat2)
describeFactors(data.h$age_cat2)

describeFactors(data.h$vaccine_status)


data.h$smoking <- factor(data.h$smoking)
data.h$i_bingedrinking <- factor(data.h$i_bingedrinking, levels = c(0, 1), labels = c("No", "Yes"))
data.h$l_htn <- factor(data.h$l_htn, levels = c(0, 1), labels = c("No", "Yes"))
data.h$gestational_diabetes <- factor(data.h$gestational_diabetes, levels = c(0, 1), labels = c("No", "Yes"))
data.h$hospitalization <- factor(data.h$hospitalization, levels = c(0, 1), labels = c("No", "Yes"))
data.h$icu <- factor(data.h$icu, levels = c(0, 1), labels = c("No", "Yes"))
data.h$inv_mech_vent <- factor(data.h$inv_mech_vent, levels = c(0, 1), labels = c("No", "Yes"))
data.h$age_cat2 <- factor(data.h$age_cat2, levels = c("Less than 30", "30-35 years","36 years and older"))

data.h$vaccine_status <- case_when(
  data.h$number_of_vaccines == 0 ~ "none",
  data.h$number_of_vaccines == 1 ~ "1",
  data.h$number_of_vaccines > 1 ~ "2+"
)

data.h$vaccine_status  <- factor(data.h$vaccine_status, levels = c("none","1","2+") )

data.h$icu[which(is.na(data.h$icu))] <- "No"

data.h.pre <- data.h[-which(data.h$preterm == "term"), ]
data.h.notpre <- data.h[-which(data.h$preterm == "preterm"), ]

dim(data.h.pre)
dim(data.h.notpre)

length(unique(data.h.pre$a_record_id))
length(unique(data.h.notpre$a_record_id))

sum(data.h$preterm == "preterm", na.rm = TRUE)
sum(data.h$preterm == "term", na.rm = TRUE)

data.h$age_cat2[which(data.h$age_cat2 == "missing")] <- NA

data.h$covid_period2 <- case_when(
  data.h$covid_period == "Omicron" ~ "Omicron",
  data.h$covid_period != "Omicron" ~ "Pre-Omicron"
  
)

# data.h$hospitalization <- as.factor(data.h$hospitalization )

# data.h.table <- data.h[-which(is.na(data.h$preterm)),]

data.h.table <- data.h
getT1Stat <- function(varname, digits=0, useNA = "ifany"){
  getDescriptionStatsBy(data.h.table[, varname], 
                        data.h.table$covid_period2, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = "ifany",
                        digits=digits,
                        header_count = TRUE)
}

table_data <- list()


# Get the basic stats
table_data[["Preterm"]] <- getT1Stat("preterm", 1)
table_data[["Age"]] <- getT1Stat("age_cat2", 1)
table_data[["ethnicity"]] <- getT1Stat("eth_cat", 1)
table_data[["Smoking during pregnancy"]] <- getT1Stat("smoking", 1)
table_data[["Binge drinking during pregnancy"]] <- getT1Stat("i_bingedrinking", 1)
table_data[["Pregnancy induced hypertension"]] <- getT1Stat("l_htn", 1)
table_data[["Gestational Diabetes"]] <- getT1Stat("gestational_diabetes", 1)
table_data[["COVID vaccine status"]] <- getT1Stat("vaccine_status", 1)
table_data[["GA at diagnosis"]] <- getT1Stat("time_of_diag", 1)
table_data[["Maternal hospital admission"]] <- getT1Stat("hospitalization", 1)
table_data[["Maternal ICU admission"]] <- getT1Stat("icu", 1)
table_data[["Maternal invasive ventilation"]] <- getT1Stat("inv_mech_vent", 1)


# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data)) {
  output_data <- rbind(output_data, 
                       table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
# cgroup <- c("", "Preterm among diagnosed <37 weeks")
n.cgroup <- c(1, 3)
colnames(output_data) <- gsub("[ ]*Preterm", "", colnames(output_data))



htmlTable::htmlTable(output_data, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     #cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()




##### INFANT STUFF NOW ######


full_data_sansON_babe <- reshape(full_data_sansON, id = "a_record_id", idvar = "a_record_id", direction = "long", varying = 
                      list(
                        dob = c("r_dob", "r_dob_2"),
                        baby_number = c("r_baby", "r_baby_2"),
                        apgar_5 = c("s_apgar_5", "s_apgar_5_2"),
                        pH = c("r_baseexcess", "r_baseexcess_2"),
                        iatrogenic = c("r_iaotrogenic", "r_iaotrogenic_2"),
                        spontaneous = c("r_spontaneous", "r_spontaneous_2"),
                        resus_any = c("s_resus_any", "s_resus_any_2"),
                        intubation = c("s_intubation", "s_intubation_2"),
                        noninv = c("t_care_noninv___1", "t_care_noninv_2___1"),
                        invasive = c("t_care_inv___1", "t_care_inv_2___1"),
                        ippv = c("s_ippv", "s_ippv_2"),
                        cpap = c("s_cpap", "s_cpap_2"),
                        BW = c("s_bw_gm", "s_bw_gm_2"),
                        hypogly = c("s_hypoglycemia", "s_hypoglycemia_2"),
                        infant_death = c("s_infdeath", "s_infdeath_2"),
                        any_health_events = c("s_healthevents", "s_healthevents_2"),
                        nicu = c("s_nicu", "s_nicu_2"),
                        t_nicu = c("t_nicu", "t_nicu_2"),
                        inhaled_nitric = c("t_care_nitric___1", "t_care_nitric_2___1"),
                        ecmo_baby = c("t_care_ecmo___1", "t_care_ecmo_2___1"),
                        parenteral = c("t_care_nut___1", "t_care_nut_2___1"),
                        sex = c("s_pe_genitalia", "s_pe_genitalia_2"),
                        surfactant = c("t_care_surf___1","t_care_surf_2___1"),
                        therap_hypo = c("t_care_hypo___1","t_care_hypo_2___1"),
                        gast_surg = c("t_care_surg___1","t_care_surg_2___1")
                        
                      ))

full_data_sansON_babe <- full_data_sansON_babe[-which(is.na(full_data_sansON_babe$r_dob)), ]

# full_data_sansON_test <- full_data_sansON[-which(is.na(full_data_sansON$r_dob)), ]


full_data_sansON_babe <- full_data_sansON_babe %>% 
  mutate(
    apgar5 = case_when(
      s_apgar_5 < 7 ~ "<7",
      s_apgar_5 >= 7 ~ "≥7"
    ),
    BW_cat = case_when(
      s_bw_gm < 2500 ~ "< 2500g",
      s_bw_gm > 4000 ~ "> 4000g",
      s_bw_gm >= 2500 & s_bw_gm <=4000 ~ "2500 - 4000g",
      is.na(s_bw_gm) ~ NA_character_
    ),
    NICU = case_when(
      s_nicu == 1 | t_nicu == 1 ~ 1,
      s_nicu == 0 | t_nicu == 0 ~ 0
    ),
    sex = case_when(
      s_pe_genitalia == 1 ~ "Male",
      s_pe_genitalia == 2 ~ "Female"
    )
  )


data_on_babe <- data_on[-which(is.na(data_on$new_birth_id)), ]

### rename the variables to match
describeFactors(data_on_babe$neonatal_death_id)


data_on_babe <- data_on_babe %>% 
  mutate(
    s_infdeath = case_when(
      data_on_babe$neonatal_death_id == 1018200 ~ 1,
      TRUE ~ 0),
    s_resus_any = case_when(
      newborn_resuscitation_id == 1018680 ~ 0,
      newborn_resuscitation_id == -1 ~ NA_real_,
      TRUE ~ 1
    ),
    s_intubation = case_when(
      neonatal_therapy_id == 1026820 ~ 0,
      grepl("3100006", neonatal_therapy_id) ~ 1,
      neonatal_therapy_id == -1 ~ NA_real_,
      TRUE ~ 0
    ),
    BW_cat = B_infant_weight,
    s_hypoglycemia = case_when(
      newborn_condition_cmplctn_id == 1018410 ~ 0,
      grepl("101400", newborn_condition_cmplctn_id) ~ 1,
      newborn_condition_cmplctn_id == -1 ~ NA_real_,
      newborn_condition_cmplctn_id == "" ~ 0
    ),
    NICU = case_when(
      NICU_admission_flag == "Y" ~ 1,
      NICU_admission_flag == "N" ~ 0
    ),
    t_care_nitric___1 = case_when(
      neonatal_therapy_id == 1026820 ~ 0,
      grepl("1026810", neonatal_therapy_id) ~ 1,
      neonatal_therapy_id == -1 ~ NA_real_,
      TRUE ~ 0
    ),
    t_care_nut___1 = case_when(
      IV_TPN == 1 ~ 1,
      TRUE ~ 0
    ),
    sex = case_when(
      gender_sex_id == -1 ~ NA_character_,
      gender_sex_id == 1013870 ~ "Female",
      gender_sex_id == 1013880 ~ "Male"
    ),
    t_care_noninv___1 = case_when( ## may be incorrect
      H_NonInvasive_Mech_Num_Days > 0 ~ 1,
      TRUE ~ 0
    ),
    t_care_surf___1 = case_when( ## may be incorrect
      neonatal_therapy_id == 1026880 ~ 1,
      TRUE ~ 0
    )
  )


## check and compare between ontario and rest of data
# Iatrogenic preterm delivery - non ON
# Non-invasive ventilation - non ON
# Use of surfactant - not sure, non ON 
# ECMO (VA or VV) - good
# Gastrointestinal surgery - non ON
# Therapeutic hypothermia - non ON
# Diagnosis of COVID in neonates



### ECMO
describeFactors(full_data_sansON_babe$t_care_ecmo___1)

full_data_sansON_babe$ecmo <- case_when(
  full_data_sansON_babe$t_care_ecmo___1 == 1 ~ 1,
  full_data_sansON_babe$e_ecmo == 1 ~ 1,
  TRUE ~ 0
)

describeFactors(data_on_babe$H_ECMO_num_days)

data_on_babe$ecmo <- case_when(
  data_on_babe$H_ECMO_num_days > 0 ~ 1,
  TRUE ~ 0
)

describeFactors(data_on_babe$ecmo)
describeFactors(full_data_sansON_babe$ecmo)


## surfactants (may be missing ON data)
describeFactors(data_on_babe$t_care_surf___1) ## probs incorrect? maybe fine
describeFactors(full_data_sansON_babe$t_care_surf___1)


## non invasive ventailation
describeFactors(data_on_babe$t_care_noninv___1) ## seems low
describeFactors(full_data_sansON_babe$t_care_noninv___1)

# iatrogenic 
# full_data_sansON_babe$r_iaotrogenic
data_on_babe$r_iaotrogenic <- case_when(
  (data_on_babe$labour_type == "induced")*(data_on_babe$ga_del_cat2 == "preterm") == 1 ~ 1,
  data_on_babe$ga_del_cat2 == "term" ~ 0,
  data_on_babe$labour_type != "induced" ~ 0
)

full_data_sansON_babe$r_iaotrogenic[which(full_data_sansON_babe$r_iaotrogenic == 999)] <- NA
full_data_sansON_babe$r_iaotrogenic[which(is.na(full_data_sansON_babe$r_iaotrogenic ))] <- 0

describeFactors(full_data_sansON_babe$r_iaotrogenic)
describeFactors(data_on_babe$r_iaotrogenic)


## gastrointestinal surgery, therapeutic hypothermia (no data from ON)

data_on_babe$t_care_hypo___1 <- NA
data_on_babe$t_care_surg___1 <- NA

## diagnosis of neonates:

describeFactors(data_on_babe$cov_test_inf_res) ## might be incorrect, might also be unavailable for ON
describeFactors(full_data_sansON_babe$cov_test_inf_res)


full_data_sansON_babe %>% group_by(prov) %>% summarise(ns = sum(t_care_surf___1, na.rm = TRUE))

full_data_sansON_babe$vacc_status <- case_when(
  full_data_sansON_babe$number_of_vaccines == 0 ~ "none",
  full_data_sansON_babe$number_of_vaccines == 1 ~ "1",
  full_data_sansON_babe$number_of_vaccines > 1 ~ "2+"
)

data_on_babe$vacc_status <- case_when(
  data_on_babe$number_of_vaccines == 0 ~ "none",
  data_on_babe$number_of_vaccines == 1 ~ "1",
  data_on_babe$number_of_vaccines > 1 ~ "2+"
)



describeFactors(data_on_babe$t_care_noninv___1)
describeFactors(full_data_sansON_babe$t_care_noninv___1)

# describeFactors(full_data_sansON_babe$sga2)
# describeFactors(data_on_babe$sga2)
# describeFactors(data_on_babe$labour_type_id)
# 
# describeFactors(full_data_sansON_babe$p_labour___1)
# 
# describeFactors(full_data_sansON_babe$p_labour___2)
# 
# describeFactors(full_data_sansON_babe$p_labour___3)
# 
# describeFactors(full_data_sansON_babe$p_labour___4)




data.babe = rbind(
  full_data_sansON_babe[, c("a_record_id", "prov", "ga_del_cat","age_cat2","ga_del_cat2", "BW_cat", "sex", "l_htn", "gestational_diabetes", "smoking", "time_of_diag", "mode_del", "vacc_status", "apgar5", "NICU", "s_infdeath", "s_resus_any", "s_intubation", "s_hypoglycemia", "t_care_nitric___1", "t_care_nut___1", "hospitalization", "icu", "inv_mech_vent","sga2","cov_test_inf_res","r_iaotrogenic","t_care_noninv___1","t_care_surf___1","ecmo","t_care_surg___1","t_care_hypo___1","covid_period")],
  data_on_babe[, c("a_record_id", "prov", "ga_del_cat","age_cat2","BW_cat","ga_del_cat2", "sex", "l_htn", "gestational_diabetes", "smoking", "time_of_diag", "mode_del", "vacc_status", "apgar5", "NICU", "s_infdeath", "s_resus_any", "s_intubation", "s_hypoglycemia", "t_care_nitric___1", "t_care_nut___1", "hospitalization", "icu", "inv_mech_vent","sga2","cov_test_inf_res","r_iaotrogenic","t_care_noninv___1","t_care_surf___1","ecmo","t_care_surg___1","t_care_hypo___1","covid_period")]
)




data.babe$preterm <- data.babe$ga_del_cat2

# describeFactors(data.babe$n_covid_date3)
describeFactors(data.babe$ga_del_cat2)
describeFactors(data.babe$age_cat2)

data.babe$age_cat2[which(data.babe$age_cat2 == "missing")] <- NA

describeFactors(data.babe$vacc_status)


data.babe$smoking <- factor(data.babe$smoking)
# data.babe$i_bingedrinking <- factor(data.babe$i_bingedrinking, levels = c(0, 1), labels = c("No", "Yes"))
data.babe$l_htn <- factor(data.babe$l_htn, levels = c(0, 1), labels = c("No", "Yes"))
data.babe$gestational_diabetes <- factor(data.babe$gestational_diabetes, levels = c(0, 1), labels = c("No", "Yes"))
data.babe$hospitalization <- factor(data.babe$hospitalization, levels = c(0, 1), labels = c("No", "Yes"))
data.babe$icu <- factor(data.babe$icu, levels = c(0, 1), labels = c("No", "Yes"))
data.babe$inv_mech_vent <- factor(data.babe$inv_mech_vent, levels = c(0, 1), labels = c("No", "Yes"))
data.babe$age_cat2 <- factor(data.babe$age_cat2, levels = c("Less than 30", "30-35 years","36 years and older"))

# data.babe$vaccine_status <- case_when(
#   data.babe$number_of_vaccines == 0 ~ "none",
#   data.babe$number_of_vaccines == 1 ~ "1",
#   data.babe$number_of_vaccines > 1 ~ "2+"
# )

data.babe$vaccine_status  <- factor(data.babe$vacc_status, levels = c("none","1","2+") )

data.babe$covid_period2 <- case_when(
  data.babe$covid_period == "Omicron" ~ "Omicron",
  data.babe$covid_period != "Omicron" ~ "Pre-Omicron"
)



describeFactors(data.babe$NICU)
describeFactors(data.babe$age_cat2)
describeFactors(data.babe$covid_period2)


#### BY COVID TIME PERIOD ####
data.babe <- as.data.frame(data.babe)

# data.babe %>% group_by(prov) %>% summarise(num = sum(s_intubation == 1, na.rm = TRUE))

describeFactors(data.babe$s_resus_any)

data.babe$s_resus_any[which(data.babe$s_resus_any == 999)] <- 0
data.babe$s_resus_any[which(is.na(data.babe$s_resus_any))] <- 0

data.babe$s_intubation[which(data.babe$s_intubation == 999)] <- 0
data.babe$s_intubation[which(is.na(data.babe$s_intubation))] <- 0

data.babe$t_care_nitric___1[which(data.babe$t_care_nitric___1 == 999)] <- 0
data.babe$t_care_nitric___1[which(is.na(data.babe$t_care_nitric___1))] <- 0

data.babe$s_hypoglycemia[which(data.babe$s_hypoglycemia == 999)] <- 0
data.babe$s_hypoglycemia[which(is.na(data.babe$s_hypoglycemia))] <- 0

data.babe$s_infdeath[which(data.babe$s_infdeath == 999)] <- 0
data.babe$s_infdeath[which(is.na(data.babe$s_infdeath))] <- 0


data.babe$s_resus_any <- factor(data.babe$s_resus_any)
data.babe$s_intubation <- factor(data.babe$s_intubation)
data.babe$t_care_nitric___1 <- factor(data.babe$t_care_nitric___1)
data.babe$t_care_nut___1 <- factor(data.babe$t_care_nut___1)
data.babe$s_hypoglycemia <- factor(data.babe$s_hypoglycemia)
data.babe$s_infdeath <- factor(data.babe$s_infdeath)


data.babe$BW_cat <- factor(data.babe$BW_cat, levels = c("< 2500g", "2500 - 4000g", "> 4000g"))

data.babe$SGA <- factor(data.babe$sga2)
levels(data.babe$SGA) <- c("Not SGA","SGA")
summary(data.babe$SGA)

data.babe$apgar5 <- factor(data.babe$apgar5)
data.babe$smoking <- factor(data.babe$smoking)
data.babe$l_htn <- factor(data.babe$l_htn)
data.babe$gestational_diabetes <- factor(data.babe$gestational_diabetes)
data.babe$hospitalization <- factor(data.babe$hospitalization)
data.babe$icu <- factor(data.babe$icu)
data.babe$inv_mech_vent <- factor(data.babe$inv_mech_vent)

data.babe$ga_del_cat <- factor(data.babe$ga_del_cat, levels = c("term", "late preterm", "moderate preterm", "very preterm", "extremely preterm"))


describeFactors(data.babe$NICU)

data.babe$NICU <- factor(data.babe$NICU)
levels(data.babe$NICU) <- c("No","Yes")

data.babe$vacc_status2 <- factor(data.babe$vacc_status)


data.babe$NICU <- factor(data.babe$NICU)

data.babe$vacc_status2 <- factor(data.babe$vacc_status, levels = c("none","1","2+"))

data.babe$cov_test_inf_res <- factor(data.babe$cov_test_inf_res)





data.babe$r_iaotrogenic[which(data.babe$prov == "ON")] <- NA
data.babe$t_care_noninv___1[which(data.babe$prov == "ON")] <- NA
data.babe$t_care_surf___1[which(data.babe$prov == "ON")] <- NA
data.babe$t_care_surg___1[which(data.babe$prov == "ON")] <- NA
data.babe$t_care_hypo___1[which(data.babe$prov == "ON")] <- NA

data.babe$r_iaotrogenic <- factor(data.babe$r_iaotrogenic)
data.babe$t_care_noninv___1 <- factor(data.babe$t_care_noninv___1)
data.babe$t_care_surf___1 <- factor(data.babe$t_care_surf___1)
data.babe$t_care_surg___1 <- factor(data.babe$t_care_surg___1)
data.babe$t_care_hypo___1 <- factor(data.babe$t_care_hypo___1)

#### BY NICU ADMISSION ####



data.babe.pre <- data.babe[which(data.babe$covid_period2 == "Pre-Omicron"), ]
data.babe.omicron<- data.babe[which(data.babe$covid_period2 == "Omicron"), ]


getT1Stat <- function(varname, digits=0, useNA = "no"){
 getDescriptionStatsBy(data.babe[, varname], 
                             data.babe$NICU, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

getT2Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe.omicron[, varname], 
                              data.babe.omicron$NICU, 
                              add_total_col=TRUE,
                              show_all_values=TRUE, 
                              hrzl_prop=FALSE,
                              statistics = TRUE,
                              html=TRUE, 
                              useNA = useNA,
                              digits=digits,
                              header_count = TRUE)
}

getT3Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe.pre[, varname], 
                        data.babe.pre$NICU, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}


table_data <- list()

data.babe <- as.data.frame(data.babe)

# Get the basic stats
table_data[["Gestational age"]] <- getT1Stat("ga_del_cat", 1)
table_data[["Birth weight"]] <- getT1Stat("BW_cat", 1)
table_data[["Sex"]] <- getT1Stat("sex", 1)
table_data[["SGA"]] <- getT1Stat("SGA", 1)
table_data[["APGAR 5"]] <- getT1Stat("apgar5", 1)
table_data[["Mode of delivery"]] <- getT1Stat("mode_del", 1)
table_data[["Smoking during pregnancy"]] <- getT1Stat("smoking", 1)
table_data[["Pregnancy induced hypertension"]] <- getT1Stat("l_htn", 1)
table_data[["Gestational Diabetes"]] <- getT1Stat("gestational_diabetes", 1)
table_data[["COVID vaccine status"]] <- getT1Stat("vacc_status", 1)
table_data[["GA at diagnosis"]] <- getT1Stat("time_of_diag", 1)
table_data[["Maternal hospital admission"]] <- getT1Stat("hospitalization", 1)
table_data[["Maternal ICU admission"]] <- getT1Stat("icu", 1)
table_data[["Positive Infant COVID diagnosis"]] <- getT1Stat("cov_test_inf_res", 1)


table_data_pre <- list()

data.babe.pre <- as.data.frame(data.babe.pre)

# Get the basic stats
table_data_pre[["Gestational age"]] <- getT3Stat("ga_del_cat", 1)
table_data_pre[["Birth weight"]] <- getT3Stat("BW_cat", 1)
table_data_pre[["Sex"]] <- getT3Stat("sex", 1)
table_data_pre[["SGA"]] <- getT3Stat("SGA", 1)
table_data_pre[["APGAR 5"]] <- getT3Stat("apgar5", 1)
table_data_pre[["Mode of delivery"]] <- getT3Stat("mode_del", 1)
table_data_pre[["Smoking during pregnancy"]] <- getT3Stat("smoking", 1)
table_data_pre[["Pregnancy induced hypertension"]] <- getT3Stat("l_htn", 1)
table_data_pre[["Gestational Diabetes"]] <- getT3Stat("gestational_diabetes", 1)
table_data_pre[["COVID vaccine status"]] <- getT3Stat("vacc_status", 1)
table_data_pre[["GA at diagnosis"]] <- getT3Stat("time_of_diag", 1)
table_data_pre[["Maternal hospital admission"]] <- getT3Stat("hospitalization", 1)
table_data_pre[["Maternal ICU admission"]] <- getT3Stat("icu", 1)
table_data_pre[["Positive Infant COVID diagnosis"]] <- getT3Stat("cov_test_inf_res", 1)




table_data_omc <- list()

data.babe.omicron <- as.data.frame(data.babe.omicron)

# Get the basic stats
table_data_omc[["Gestational age"]] <- getT2Stat("ga_del_cat", 1)
table_data_omc[["Birth weight"]] <- getT2Stat("BW_cat", 1)
table_data_omc[["Sex"]] <- getT2Stat("sex", 1)
table_data_omc[["SGA"]] <- getT2Stat("SGA", 1)
table_data_omc[["APGAR 5"]] <- getT2Stat("apgar5", 1)
table_data_omc[["Mode of delivery"]] <- getT2Stat("mode_del", 1)
table_data_omc[["Smoking during pregnancy"]] <- getT2Stat("smoking", 1)
table_data_omc[["Pregnancy induced hypertension"]] <- getT2Stat("l_htn", 1)
table_data_omc[["Gestational Diabetes"]] <- getT2Stat("gestational_diabetes", 1)
table_data_omc[["COVID vaccine status"]] <- getT2Stat("vacc_status", 1)
table_data_omc[["GA at diagnosis"]] <- getT2Stat("time_of_diag", 1)
table_data_omc[["Maternal hospital admission"]] <- getT2Stat("hospitalization", 1)
table_data_omc[["Maternal ICU admission"]] <- getT2Stat("icu", 1)
table_data_omc[["Positive Infant COVID diagnosis"]] <- getT2Stat("cov_test_inf_res", 1)




# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_NICU <- NULL
for (varlabel in names(table_data)) {
  output_data_NICU <- rbind(output_data_NICU, 
                            table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
# cgroup <- c("", "NICU")
n.cgroup <- c(1, 3)
# colnames(output_data_NICU) <- gsub("[ ]*NICU", "", colnames(output_data_NICU))

htmlTable::htmlTable(output_data_NICU, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     # cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()


# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_NICU_pre <- NULL
for (varlabel in names(table_data_pre)) {
  output_data_NICU_pre <- rbind(output_data_NICU_pre, 
                            table_data_pre[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data_pre[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Pre-Omicron")
n.cgroup <- c(1, 3)
colnames(output_data_NICU_pre) <- gsub("[ ]*Pre-Omicron", "", colnames(output_data_NICU_pre))

htmlTable::htmlTable(output_data_NICU_pre, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()

# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_NICU_omc <- NULL
for (varlabel in names(table_data_omc)) {
  output_data_NICU_omc <- rbind(output_data_NICU_omc, 
                            table_data_omc[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data_omc[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Omicron")
n.cgroup <- c(1, 3)
colnames(output_data_NICU_omc) <- gsub("[ ]*Omicron", "", colnames(output_data_NICU_omc))

htmlTable::htmlTable(output_data_NICU_omc, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()


#### BY VACCINE STATUS ####

# levels(data.babe$vacc_status2)[c(2, 3)] <- "2 or 3 doses before dx"

getT1Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe[, varname], 
                        data.babe$vacc_status2, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data <- list()



# Get the basic stats
table_data[["Gestational age"]] <- getT1Stat("ga_del_cat", 1)
table_data[["Birth weight"]] <- getT1Stat("BW_cat", 1)
table_data[["SGA"]] <- getT1Stat("SGA", 1)
table_data[["Any resuscitation"]] <- getT1Stat("s_resus_any", 1)
table_data[["Invasive mechanical ventilation"]] <- getT1Stat("s_intubation", 1)
table_data[["Inhaled nitric oxide"]] <- getT1Stat("t_care_nitric___1", 1)
table_data[["Parenteral nutrition"]] <- getT1Stat("t_care_nut___1", 1)
table_data[["Hypoglycemia"]] <- getT1Stat("s_hypoglycemia", 1)
table_data[["Neonatal death"]] <- getT1Stat("s_infdeath", 1)
table_data[["Iatrogenic preterm*"]] <- getT1Stat("r_iaotrogenic", 1)
table_data[["Non-Invasive Ventilation*"]] <- getT1Stat("t_care_noninv___1", 1)







# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_vacc <- NULL
for (varlabel in names(table_data)) {
  output_data_vacc <- rbind(output_data_vacc, 
                            table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Vaccine")
n.cgroup <- c(1, 4)
colnames(output_data_vacc) <- gsub("[ ]*Vaccine", "", colnames(output_data_vacc))

htmlTable::htmlTable(output_data_vacc, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()


getT2Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe.pre[, varname], 
                        data.babe.pre$vacc_status2, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}


table_data2 <- list()



# Get the basic stats
table_data2[["Gestational age"]] <- getT2Stat("ga_del_cat", 1)
table_data2[["Birth weight"]] <- getT2Stat("BW_cat", 1)
table_data2[["SGA"]] <- getT2Stat("SGA", 1)
table_data2[["Any resuscitation"]] <- getT2Stat("s_resus_any", 1)
table_data2[["Invasive mechanical ventilation"]] <- getT2Stat("s_intubation", 1)
table_data2[["Inhaled nitric oxide"]] <- getT2Stat("t_care_nitric___1", 1)
table_data2[["Parenteral nutrition"]] <- getT2Stat("t_care_nut___1", 1)
table_data2[["Hypoglycemia"]] <- getT2Stat("s_hypoglycemia", 1)
table_data2[["Neonatal death"]] <- getT2Stat("s_infdeath", 1)
table_data2[["Iatrogenic preterm*"]] <- getT2Stat("r_iaotrogenic", 1)
table_data2[["Non-Invasive Ventilation*"]] <- getT2Stat("t_care_noninv___1", 1)






# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_vacc <- NULL
for (varlabel in names(table_data2)) {
  output_data_vacc <- rbind(output_data_vacc, 
                            table_data2[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data2[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Pre-Omicron")
n.cgroup <- c(1, 4)
colnames(output_data_vacc) <- gsub("[ ]*Pre-Omicron", "", colnames(output_data_vacc))

htmlTable::htmlTable(output_data_vacc, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()










getT3Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe.omicron[, varname], 
                        data.babe.omicron$vacc_status2, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}


table_data3 <- list()



# Get the basic stats
table_data3[["Gestational age"]] <- getT3Stat("ga_del_cat", 1)
table_data3[["Birth weight"]] <- getT3Stat("BW_cat", 1)
table_data3[["SGA"]] <- getT3Stat("SGA", 1)
table_data3[["Any resuscitation"]] <- getT3Stat("s_resus_any", 1)
table_data3[["Invasive mechanical ventilation"]] <- getT3Stat("s_intubation", 1)
table_data3[["Inhaled nitric oxide"]] <- getT3Stat("t_care_nitric___1", 1)
table_data3[["Parenteral nutrition"]] <- getT3Stat("t_care_nut___1", 1)
table_data3[["Hypoglycemia"]] <- getT3Stat("s_hypoglycemia", 1)
table_data3[["Neonatal death"]] <- getT3Stat("s_infdeath", 1)
table_data3[["Iatrogenic preterm*"]] <- getT3Stat("r_iaotrogenic", 1)
table_data3[["Non-Invasive Ventilation*"]] <- getT3Stat("t_care_noninv___1", 1)





# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_vacc <- NULL
for (varlabel in names(table_data3)) {
  output_data_vacc <- rbind(output_data_vacc, 
                            table_data3[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data3[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Omicron")
n.cgroup <- c(1, 4)
colnames(output_data_vacc) <- gsub("[ ]*Omicron", "", colnames(output_data_vacc))

htmlTable::htmlTable(output_data_vacc, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()



#### HOSPITALIZATION #####


getT1Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe[, varname], 
                        data.babe$hospitalization, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data <- list()

# Get the basic stats
table_data[["Gestational age"]] <- getT1Stat("ga_del_cat", 1)
table_data[["Birth weight"]] <- getT1Stat("BW_cat", 1)
table_data[["SGA"]] <- getT1Stat("SGA", 1)
table_data[["Any resuscitation"]] <- getT1Stat("s_resus_any", 1)
table_data[["Invasive mechanical ventilation"]] <- getT1Stat("s_intubation", 1)
table_data[["Inhaled nitric oxide"]] <- getT1Stat("t_care_nitric___1", 1)
table_data[["Parenteral nutrition"]] <- getT1Stat("t_care_nut___1", 1)
table_data[["Hypoglycemia"]] <- getT1Stat("s_hypoglycemia", 1)
table_data[["Neonatal death"]] <- getT1Stat("s_infdeath", 1)
table_data[["ECMO"]] <- getT1Stat("ecmo", 1)
table_data[["Positive Infant COVID diagnosis"]] <- getT1Stat("cov_test_inf_res", 1)
table_data[["Iatrogenic preterm*"]] <- getT1Stat("r_iaotrogenic", 1)
table_data[["Non-Invasive Ventilation*"]] <- getT1Stat("t_care_noninv___1", 1)
table_data[["Use of surfactant*"]] <- getT1Stat("t_care_surf___1", 1)
table_data[["Gastrointestinal Surgery*"]] <- getT1Stat("t_care_surg___1", 1)
table_data[["Therapeutic Hypothermia*"]] <- getT1Stat("t_care_hypo___1", 1)






# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_hosp <- NULL
for (varlabel in names(table_data)) {
  output_data_hosp <- rbind(output_data_hosp, 
                            table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Maternal hospitalization")
n.cgroup <- c(1, 3)
colnames(output_data_hosp) <- gsub("[ ]*Maternal hospitalization", "", colnames(output_data_hosp))

htmlTable::htmlTable(output_data_hosp, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()










getT1Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe.pre[, varname], 
                        data.babe.pre$hospitalization, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data <- list()

# Get the basic stats
table_data[["Gestational age"]] <- getT1Stat("ga_del_cat", 1)
table_data[["Birth weight"]] <- getT1Stat("BW_cat", 1)
table_data[["SGA"]] <- getT1Stat("SGA", 1)
table_data[["Any resuscitation"]] <- getT1Stat("s_resus_any", 1)
table_data[["Invasive mechanical ventilation"]] <- getT1Stat("s_intubation", 1)
table_data[["Inhaled nitric oxide"]] <- getT1Stat("t_care_nitric___1", 1)
table_data[["Parenteral nutrition"]] <- getT1Stat("t_care_nut___1", 1)
table_data[["Hypoglycemia"]] <- getT1Stat("s_hypoglycemia", 1)
table_data[["Neonatal death"]] <- getT1Stat("s_infdeath", 1)
table_data[["ECMO"]] <- getT1Stat("ecmo", 1)
table_data[["Positive Infant COVID diagnosis"]] <- getT1Stat("cov_test_inf_res", 1)
table_data[["Iatrogenic preterm*"]] <- getT1Stat("r_iaotrogenic", 1)
table_data[["Non-Invasive Ventilation*"]] <- getT1Stat("t_care_noninv___1", 1)
table_data[["Use of surfactant*"]] <- getT1Stat("t_care_surf___1", 1)
table_data[["Gastrointestinal Surgery*"]] <- getT1Stat("t_care_surg___1", 1)
table_data[["Therapeutic Hypothermia*"]] <- getT1Stat("t_care_hypo___1", 1)



# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_hosp <- NULL
for (varlabel in names(table_data)) {
  output_data_hosp <- rbind(output_data_hosp, 
                            table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Pre-Omicron")
n.cgroup <- c(1, 3)
colnames(output_data_hosp) <- gsub("[ ]*Pre-Omicron", "", colnames(output_data_hosp))

htmlTable::htmlTable(output_data_hosp, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()




getT1Stat <- function(varname, digits=0, useNA = "no"){
  getDescriptionStatsBy(data.babe.omicron[, varname], 
                        data.babe.omicron$hospitalization, 
                        add_total_col=TRUE,
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics = TRUE,
                        html=TRUE, 
                        useNA = useNA,
                        digits=digits,
                        header_count = TRUE)
}

table_data <- list()

# Get the basic stats
table_data[["Gestational age"]] <- getT1Stat("ga_del_cat", 1)
table_data[["Birth weight"]] <- getT1Stat("BW_cat", 1)
table_data[["SGA"]] <- getT1Stat("SGA", 1)
table_data[["Any resuscitation"]] <- getT1Stat("s_resus_any", 1)
table_data[["Invasive mechanical ventilation"]] <- getT1Stat("s_intubation", 1)
table_data[["Inhaled nitric oxide"]] <- getT1Stat("t_care_nitric___1", 1)
table_data[["Parenteral nutrition"]] <- getT1Stat("t_care_nut___1", 1)
table_data[["Hypoglycemia"]] <- getT1Stat("s_hypoglycemia", 1)
table_data[["Neonatal death"]] <- getT1Stat("s_infdeath", 1)
table_data[["ECMO"]] <- getT1Stat("ecmo", 1)
table_data[["Positive Infant COVID diagnosis"]] <- getT1Stat("cov_test_inf_res", 1)
table_data[["Iatrogenic preterm*"]] <- getT1Stat("r_iaotrogenic", 1)
table_data[["Non-Invasive Ventilation*"]] <- getT1Stat("t_care_noninv___1", 1)
table_data[["Use of surfactant*"]] <- getT1Stat("t_care_surf___1", 1)
table_data[["Gastrointestinal Surgery*"]] <- getT1Stat("t_care_surg___1", 1)
table_data[["Therapeutic Hypothermia*"]] <- getT1Stat("t_care_hypo___1", 1)



# Now merge everything into a matrix
# and create the rgroup & n.rgroup variabels
rgroup <- c()
n.rgroup <- c()
output_data_hosp <- NULL
for (varlabel in names(table_data)) {
  output_data_hosp <- rbind(output_data_hosp, 
                            table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

# Add a column spanner for the columns
cgroup <- c("", "Omicron")
n.cgroup <- c(1, 3)
colnames(output_data_hosp) <- gsub("[ ]*Omicron", "", colnames(output_data_hosp))

htmlTable::htmlTable(output_data_hosp, align = "rrrr",
                     rgroup = rgroup, n.rgroup = n.rgroup,
                     css.rgroup.sep = "",
                     cgroup = cgroup,
                     n.cgroup = n.cgroup,
                     rowlabel = "",
                     caption = "",
                     ctable = TRUE)  %>% htmltools::html_print()

