#report 6 date fixes
getwd()

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

####### BC DATA DATE FIXES ########
### load data ####
BC <- read.csv(here("COVID_PREG_cleaned_BC_2023-02-26.csv"))

####  ALSO NOTE CAN FILL IN MISSING DATES USING E-DIAGNOSIS FOR NASO1 AND VICE VERSA - but then some have to be updated below because their positive test was noso2 or naso3 etc

BC$e_diagnosis <- BC$e_diagnosis.x

BC[which(BC$a_record_id %in% c("CCP-01-145","CCP-01-424","CCP-01-1691","CCP-01-173")),c("a_record_id","e_diagnosis","e_hosp_date","r_dob")]

data <- BC


# remove this participant for having ga at diagnosis more than 8 weeks before lmp
data <- data[-which(data$a_record_id == "CCP-01-145"), ]
# remove this participant for having ga at diagnosis more than 3 weeks before lmp
# data <- data[-which(data$a_record_id == "CCP-01-298"), ]


# no diagnosis information or dates
data <- data[-which(data$a_record_id == "CCP-01-424"), ]
data <- data[-which(data$a_record_id == "CCP-01-1691"), ]
data <- data[-which(data$a_record_id == "CCP-01-173"), ]

## fix some diagnosis dates
data$e_diagnosis <- as.Date(data$e_diagnosis)
data$r_dob <- as.Date(data$r_dob)


## fix time_del (time between diagnosis and delivery)
summary(data$e_diagnosis)
# replace the 9999-09-09
data$e_diagnosis[which(data$e_diagnosis == "9999-09-09")] <- NA
data$r_dob[which(data$r_dob == "9999-09-09")] <- NA
data$time_del <- as.numeric(as.Date(data$r_dob) - data$e_diagnosis)
summary(data$time_del)
## look at the impossible ones
# more than 40 weeks
View(data[which(data$time_del > 280), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
## less than 0 days (diagnosis after delivery)
View(data[which(data$time_del <0), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# # 
View(data[which(data$a_record_id == "CCP-01-604"), c("a_record_id", "e_diagnosis", "d_naso1_collect", "d_other_collect", "d_naso1_result", "d_naso2_collect", "d_naso2_result", "r_dob", "i_deliverydate_est", "i_lmp", "time_del", "p_outcome", "o_loss", "r_discharge")])

## some of these are obvious typos
## fixing DOB
data$r_dob[which(data$a_record_id == "CCP-01-006")] <- "2020-07-12"
data$r_dob[which(data$a_record_id == "CCP-01-1146")] <- "2021-12-01"
data$r_dob[which(data$a_record_id == "CCP-01-1368")] <- "2022-01-11" ## month and day reversed
data$r_dob[which(data$a_record_id == "CCP-01-1382")] <- "2021-11-14"
data$r_dob[which(data$a_record_id == "CCP-01-2424")] <- "2022-11-03"
data$r_dob[which(data$a_record_id == "CCP-01-518")] <- "2021-07-25"
data$r_dob[which(data$a_record_id == "CCP-01-598")] <- "2021-06-24"
data$r_dob[which(data$a_record_id == "CCP-01-624")] <- "2021-11-05"  ## DOUBLE CHECK THIS ONE
data$r_dob[which(data$a_record_id == "CCP-01-822")] <- "2021-10-30" 
data$r_dob[which(data$a_record_id == "CCP-01-877")] <- "2021-06-02"
data$r_dob[which(data$a_record_id == "CCP-01-923")] <- "2021-09-08"
data$r_dob[which(data$a_record_id == "CCP-01-2424")] <- "2022-11-03"
data$r_dob[which(data$a_record_id == "CCP-01-1011")] <- "2021-06-12"
data$r_dob[which(data$a_record_id == "CCP-01-1364")] <- "2021-11-03"
data$r_dob[which(data$a_record_id == "CCP-01-1546")] <- "2022-05-06"
data$r_dob[which(data$a_record_id == "CCP-01-1665")] <- "2022-08-27"
data$r_dob[which(data$a_record_id == "CCP-01-1777")] <- "2022-02-08"  ## double check this one
data$r_dob[which(data$a_record_id == "CCP-01-1934")] <- "2021-07-01" 
data$r_dob[which(data$a_record_id == "CCP-01-2292")] <- "2022-04-13" 
data$r_dob[which(data$a_record_id == "CCP-01-401")] <- "2021-02-21" 
data$r_dob[which(data$a_record_id == "CCP-01-503")] <- "2021-12-14"  ## no idea what's going on here
data$r_dob[which(data$a_record_id == "CCP-01-584")] <- "2021-04-01" 
data$r_dob[which(data$a_record_id == "CCP-01-800")] <- "2021-05-28" 
data$r_dob[which(data$a_record_id == "CCP-01-956")] <- "2021-06-14" 



## dx date
data$e_diagnosis[which(data$a_record_id == "CCP-01-1756")] <- "2021-12-28"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1933")] <- "2022-01-10"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1941")] <- "2022-01-10"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1963")] <- "2022-01-14"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1966")] <- "2022-01-15"
data$e_diagnosis[which(data$a_record_id == "CCP-01-2064")] <- "2022-01-12"
data$e_diagnosis[which(data$a_record_id == "CCP-01-2511")]  <- "2022-05-21" ## double check most of these
data$e_diagnosis[which(data$a_record_id == "CCP-01-750")] <- "2021-04-15"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1501")] <- "2021-10-22"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1813")] <- "2021-05-10"
data$e_diagnosis[which(data$a_record_id == "CCP-01-1846")] <- "2021-11-26"
data$e_diagnosis[which(data$a_record_id == "CCP-01-2348")] <- "2021-10-28"
data$e_diagnosis[which(data$a_record_id == "CCP-01-2494")] <- "2021-12-27"
data$e_diagnosis[which(data$a_record_id == "CCP-01-508")] <- "2021-02-09"
data$e_diagnosis[which(data$a_record_id == "CCP-01-604")] <- "2021-03-07"
data$e_diagnosis[which(data$a_record_id == "CCP-01-648")] <- "2021-03-18"


##delivery date est

data$i_deliverydate_est[which(data$a_record_id == "CCP-01-800")] <-  "2021-06-02"


# CCP-01-503
# 
# CCP-01-1605
# ## lmp
# 
# CCP-01-1211
# CCP-01-1253
# CCP-01-1432
 
 
# data$r_dob[which(data$a_record_id == "CCP-01-1364")] <- "2021-11-03"
# data$r_dob[which(data$a_record_id == "CCP-01-584")] <-  "2021-04-01"
# data$r_dob[which(data$a_record_id == "CCP-01-800")] <-  "2021-05-29"
# data$r_dob[which(data$a_record_id == "CCP-01-516")] <-  "2021-02-21"
# data$r_dob[which(data$a_record_id == "CCP-01-401")] <-  "2021-01-21"
# # data$r_dob[which(data$a_record_id == "CCP-01-380")] <-  "2021-11-20"
# data$r_dob[which(data$a_record_id == "CCP-01-401")] <-  "2021-02-11"
# data$r_dob[which(data$a_record_id == "CCP-01-1146")] <- "2021-12-01"
# data$r_dob[which(data$a_record_id == "CCP-01-2300")] <- "2021-08-27"
# data$r_dob[which(data$a_record_id == "CCP-01-1426")] <- "2021-10-19"
# data$r_dob[which(data$a_record_id == "CCP-01-598")] <- "2021-06-24"
# 
# ## fix dx date
# data$e_diagnosis[which(data$a_record_id == "CCP-01-632")] <-  "2021-04-06"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-636")] <-  "2021-03-06"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-648")] <-  "2021-03-18"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1379")] <- "2020-09-24"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-022")] <-  "2020-04-25"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-026")] <-  "2020-07-09"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-080")] <-  "2021-04-15"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1014")] <- "2021-06-11"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1044")] <- "2021-05-13"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1090")] <- "2021-05-28"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1444")] <- "2021-10-06"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-829")] <-  "2021-01-14"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-912")] <-  "2021-04-21"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-948")] <-  "2021-04-26"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1337")] <- "2021-11-09"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-975")] <-  "2021-11-09"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1813")] <- "2021-05-10"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-508")] <-  "2020-11-09"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1756")] <- "2021-12-28"
# data$e_diagnosis[which(data$a_record_id == "CCP-01-1448")] <- "2021-10-05"
# 
# ## mislabelled hospitalization and fix some dates
# 
# data$e_hosp_date[which(data$a_record_id == "CCP-01-1345")] <- "2021-09-23"
# 
# ## estimated date of delivery
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-800")] <-  "2021-06-02"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-145")] <-  "2021-07-15"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-298")] <-  "2021-10-04"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-200")] <-  "2021-03-17"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-450")] <-  "2021-04-03"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1081")] <- "2021-08-12"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1154")] <- "2022-02-15"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1159")] <- "2021-08-28"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1196")] <- "2021-10-13"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1395")] <- "2022-05-10"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1415")] <- "2022-02-02"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1465")] <- "2021-10-16"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1477")] <- "2022-02-20"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1508")] <- "2022-04-16"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-899")] <-  "2021-09-27"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-904")] <-  "2021-05-20"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-600")] <-  "2021-05-09"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-281")] <-  "2021-07-23"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-062")] <-  "2021-06-07"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-546")] <-  "2021-10-25"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-912")] <-  "2020-11-26"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1520")] <- "2021-12-25"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1654")] <- "2022-07-12"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1698")] <- "2022-07-18"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1703")] <- "2022-07-23"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1704")] <- "2022-05-31"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-912")] <-  "2021-11-26"
# data$i_deliverydate_est[which(data$a_record_id == "CCP-01-1750")] <- "2022-06-25"

data$i_lmp <- ifelse(data$i_lmp == "9999-09-09", NA, as.character(data$i_lmp))

data$i_deliverydate_est <- as.Date(data$i_deliverydate_est)

data$i_deliverydate_est[which(is.na(data$i_deliverydate_est) & !is.na(data$i_lmp))] <- as.Date(data$i_lmp[which(is.na(data$i_deliverydate_est) & !is.na(data$i_lmp))]) + 280

data$time_del <- as.numeric(as.Date(data$r_dob) - data$e_diagnosis)
summary(data$time_del)

# within 2 weeks of delivery?
# data <- data[-which(data$time_del < -14), ]
# 


## how many from each province


write.csv(data, here("BC_CLEANED_2023-02-28.csv"))







############## QC DATE FIXES ################

### load data ####
QC <- read.csv(here("COVID_PREG_cleaned_QC_2023-02-26.csv"))

####  ALSO NOTE CAN FILL IN MISSING DATES USING E-DIAGNOSIS FOR NASO1 AND VICE VERSA - but then some have to be updated below because their positive test was noso2 or naso3 etc
QC$e_diagnosis[which(is.na(QC$e_diagnosis))] <- QC$d_naso1_collect.x[which(is.na(QC$e_diagnosis))]


# QC$n_covid_date1 <- QC$n_covid_date
# QC$n_covid_type1 <- QC$n_covid_type
# 
# QC$e_hosp_dur
# 
# QC$e_hosp_date

data <- QC


data$i_lmp <- ifelse(data$i_lmp == "9999-09-09", NA, as.character(data$i_lmp))
data$i_deliverydate_est <- ifelse(data$i_deliverydate_est == "9999-09-09", NA, as.character(data$i_deliverydate_est))
data$e_diagnosis <- ifelse(data$e_diagnosis == "9999-09-09", NA, as.character(data$e_diagnosis))
data$r_dob <- ifelse(data$r_dob == "9999-09-09", NA, as.character(data$r_dob))

data$i_deliverydate_est <- as.Date(data$i_deliverydate_est)

data$i_deliverydate_est[which(is.na(data$i_deliverydate_est) & !is.na(data$i_lmp))] <- as.Date(data$i_lmp[which(is.na(data$i_deliverydate_est) & !is.na(data$i_lmp))]) + 280


# 
data$time_del <- as.numeric(as.Date(data$r_dob) - as.Date(data$e_diagnosis))
summary(data$time_del)



View(data[which(data$time_del > 280), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
## less than 0 days (diagnosis after delivery)
View(data[which(data$time_del <0), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# # 
View(data[which(data$a_record_id == "188-40"), c("a_record_id", "e_diagnosis", "d_naso1_collect", "d_other_collect", "d_naso1_result", "d_naso2_collect", "d_naso2_result", "r_dob", "i_deliverydate_est", "i_lmp", "time_del", "p_outcome", "o_loss", "r_discharge")])

## r_dob fixes
data$r_dob[which(data$a_record_id == "188-66")] <- "2021-12-27"
data$r_dob[which(data$a_record_id == "186-277")] <-  "2022-02-07"
data$r_dob[which(data$a_record_id == "186-371")] <-  "2022-11-08"
data$r_dob[which(data$a_record_id == "187-98")] <- "2022-01-01"
data$r_dob[which(data$a_record_id == "187-148")] <- "2022-05-25"
data$r_dob[which(data$a_record_id == "187-267")] <- "2022-06-07"
data$r_dob[which(data$a_record_id == "187-268")] <- "2022-06-02"
data$r_dob[which(data$a_record_id == "189-141")] <- "2022-03-31"
data$r_dob[which(data$a_record_id == "189-174")] <- "2022-01-11"
data$r_dob[which(data$a_record_id == "189-179")] <- "2022-01-15"
data$r_dob[which(data$a_record_id == "190-115")] <- "2022-03-03"
data$r_dob[which(data$a_record_id == "190-136")] <- "2022-03-03"
data$r_dob[which(data$a_record_id == "190-222")]  <- "2022-01-07"
data$r_dob[which(data$a_record_id == "190-241")]  <- "2022-01-31"
data$r_dob[which(data$a_record_id == "191-52")]  <- "2021-02-21"



## dx date fixes
data$e_diagnosis[which(data$a_record_id == "188-4")] <- "2021-08-22"
data$e_diagnosis[which(data$a_record_id == "189-205")] <- "2021-12-25"
data$e_diagnosis[which(data$a_record_id == "187-02")] <- "2020-03-26"
data$e_diagnosis[which(data$a_record_id == "187-42")] <- "2021-05-07"
data$e_diagnosis[which(data$a_record_id == "187-55")] <- "2020-12-16"
data$e_diagnosis[which(data$a_record_id == "189-163")] <- "2020-12-29"
data$e_diagnosis[which(data$a_record_id == "190-41")] <- "2020-11-11"
data$e_diagnosis[which(data$a_record_id == "190-58")] <- "2021-04-03"
data$e_diagnosis[which(data$a_record_id == "191-35")] <- "2020-11-12"
data$e_diagnosis[which(data$a_record_id == "191-36")] <- "2020-11-26"
data$e_diagnosis[which(data$a_record_id == "193-22")] <- "2020-11-15"
data$e_diagnosis[which(data$a_record_id == "204-247")] <- "2021-05-03"


## remove because diagnosis date is way before
data <- data[-which(data$a_record_id == "189-282"), ]
data <- data[-which(data$a_record_id == "190-233"), ]
data <- data[-which(data$a_record_id == "187-21"), ]
data <- data[-which(data$a_record_id == "188-13"), ]

data <- data[-which(data$a_record_id == "188-40"), ]
data <- data[-which(data$a_record_id == "188-49"), ]
data <- data[-which(data$a_record_id == "188-58"), ]
data <- data[-which(data$a_record_id == "190-231"), ]
data <- data[-which(data$a_record_id == "204-74"), ]




## delete bc confusing
data <- data[-which(data$a_record_id == "191-38"), ]
data <- data[-which(data$a_record_id == "191-83"), ]
data <- data[-which(data$a_record_id == "191-110"), ]
data <- data[-which(data$a_record_id == "204-759"), ]


# ## other fixes
# 
# # seems to have been fixed? editing diagnosis date - gal
# # # remove this participant for having ga at diagnosis more than 8 weeks before lmp
# # data <- data[-which(data$a_record_id == "188-4"), ]
# 
# # more than 140 days pp ## still true - Gal
# data <- data[-which(data$a_record_id == "188-49"), ]
# 
# # no diagnosis information or dates
# data <- data[-which(data$a_record_id == "186-5"), ]
# data <- data[-which(data$a_record_id == "193-11"), ]
# data <- data[-which(data$a_record_id == "193-28"), ]
# data <- data[-which(data$a_record_id == "193-38"), ]
# # data <- data[-which(data$a_record_id == "204-13"), ] ## missing
# data <- data[-which(data$a_record_id == "204-223"), ]
# # data <- data[-which(data$a_record_id == "204-90"), ] ## no longer missing
# # data <- data[-which(data$a_record_id == "204-72"), ] ## no longer missing
# # data <- data[-which(data$a_record_id == "204-6"), ] ## no longer missing
# data <- data[-which(data$a_record_id == "188-13"), ] ## diagnosis date long after date of birth
# # data <- data[-which(data$a_record_id == "188-18"), ] ## pregnancy loss
# data <- data[-which(data$a_record_id == "188-33"), ] ## very confusing, likely error in lmp( off by one year), but also discharge date is after delivery date? revisit this
# data <- data[-which(data$a_record_id == "337-16"), ] # contradictory dates and no way to resolve
# data <- data[-which(data$a_record_id == "188-40"), ] # contradictory dates no way to resolve
# 
# ## fix some diagnosis dates
# data$e_diagnosis <- as.Date(data$e_diagnosis)
# 
# 
# ## fix time_del
# data$time_del <- as.numeric(as.Date(data$r_dob) - data$e_diagnosis)
# summary(data$time_del)
# # more than 2 weeks after delivery
# # View(data[which(data$time_del < -14), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# 
# # more than 40 weeks before delivery
# # View(data[which(data$time_del > 280), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# # 
# # View(data[which(data$a_record_id == "187-30"), c("a_record_id", "e_diagnosis", "d_naso1_collect", "d_other_collect", "d_other_collect", "d_naso1_result", "d_naso2_collect", "d_naso2_result", "r_dob", "i_deliverydate_est", "i_lmp", "time_del", "p_outcome", "o_loss", "r_discharge")])
# 
# # View(data[which(data$a_record_id == "187-30"), ])
# 
# ## some of these are obvious typos
# ## fixing some other dates
# 
# # 
# data$r_dob[which(data$a_record_id == "189-38")] <- "2021-01-28"
# data$r_dob[which(data$a_record_id == "190-41")] <- "2021-11-21"
# data$r_dob[which(data$a_record_id == "204-74")] <- "2021-11-08"
# data$r_dob[which(data$a_record_id == "204-127")] <- "2021-03-23"
# data$r_dob[which(data$a_record_id == "204-130")] <- "2021-02-04"
# data$r_dob[which(data$a_record_id == "193-2")] <- "2020-04-22"
# data$r_dob[which(data$a_record_id == "189-38")] <- "2021-01-28"
# data$r_dob[which(data$a_record_id == "193-29")] <- "2020-12-04"
# data$r_dob[which(data$a_record_id == "187-30")] <- "2021-06-23"
# data$r_dob[which(data$a_record_id == "189-141")] <- "2022-03-31"
# data$r_dob[which(data$a_record_id == "187-98")] <- "2022-01-01"
# data$r_dob[which(data$a_record_id == "189-174")] <- "2022-01-11"
# data$r_dob[which(data$a_record_id == "189-179")] <- "2022-01-15"
# data$r_dob[which(data$a_record_id == "337-20")] <- "2021-10-21"
# 
# data$e_diagnosis[which(data$a_record_id == "193-19")] <- "2020-07-26"
# data$e_diagnosis[which(data$a_record_id == "193-22")] <- "2020-11-16"
# data$e_diagnosis[which(data$a_record_id == "192-1")] <- "2020-04-16"
# data$e_diagnosis[which(data$a_record_id == "190-27")] <- "2020-09-20"
# data$e_diagnosis[which(data$a_record_id == "189-2")] <- "2020-04-04"
# data$e_diagnosis[which(data$a_record_id == "189-103")] <- "2021-03-11"
# data$e_diagnosis[which(data$a_record_id == "204-102")] <- "2020-12-06"
# data$e_diagnosis[which(data$a_record_id == "186-92")] <- "2021-04-18"
# data$e_diagnosis[which(data$a_record_id == "186-42")] <- "2020-12-09"
# data$e_diagnosis[which(data$a_record_id == "186-101")] <- "2020-12-13"
# data$e_diagnosis[which(data$a_record_id == "192-47")] <- "2021-01-03"
# data$e_diagnosis[which(data$a_record_id == "186-97")] <- "2021-04-08"
# data$e_diagnosis[which(data$a_record_id == "192-67")] <- "2021-04-27"
# data$e_diagnosis[which(data$a_record_id == "187-2")] <- "2020-03-26"
# data$e_diagnosis[which(data$a_record_id == "192-59")] <- "2020-05-03"
# data$e_diagnosis[which(data$a_record_id == "189-33")] <- "2020-09-26"
# data$e_diagnosis[which(data$a_record_id == "186-46")] <- "2020-12-23"
# data$e_diagnosis[which(data$a_record_id == "186-118")] <- "2021-09-19"
# data$e_diagnosis[which(data$a_record_id == "186-75")] <- "2021-02-01"
# data$e_diagnosis[which(data$a_record_id == "189-28")] <- "2020-12-03"
# data$e_diagnosis[which(data$a_record_id == "189-72")] <- "2020-10-26"
# data$e_diagnosis[which(data$a_record_id == "192-107")] <- "2021-01-04"
# data$e_diagnosis[which(data$a_record_id == "204-101")] <- "2020-12-05"
# data$e_diagnosis[which(data$a_record_id == "204-104")] <- "2020-12-09"
# data$e_diagnosis[which(data$a_record_id == "193-2")] <- "2020-04-22"
# data$e_diagnosis[which(data$a_record_id == "204-126")] <- "2020-09-22"
# data$e_diagnosis[which(data$a_record_id == "188-5")] <- "2021-06-22"
# data$e_diagnosis[which(data$a_record_id == "189-104")] <- "2021-03-01"
# data$e_diagnosis[which(data$a_record_id == "189-108")] <- "2021-08-07"
# data$e_diagnosis[which(data$a_record_id == "189-109")] <- "2021-06-08"
# data$e_diagnosis[which(data$a_record_id == "190-14")] <- "2020-05-01"
# data$e_diagnosis[which(data$a_record_id == "193-6")] <- "2020-04-22"
# data$e_diagnosis[which(data$a_record_id == "193-7")] <- "2020-04-01"
# data$e_diagnosis[which(data$a_record_id == "193-8")] <- "2020-03-16"
# data$e_diagnosis[which(data$a_record_id == "193-29")] <- "2020-12-03"
# data$e_diagnosis[which(data$a_record_id == "204-17")] <- "2020-05-08"
# data$e_diagnosis[which(data$a_record_id == "204-79")] <- "2020-09-22"
# data$e_diagnosis[which(data$a_record_id == "187-21")] <- "2021-01-08"
# data$e_diagnosis[which(data$a_record_id == "186-166")] <- "2021-12-31"
# data$e_diagnosis[which(data$a_record_id == "186-122")] <- "2021-09-16"
# data$e_diagnosis[which(data$a_record_id == "188-58")] <- "2020-12-17"

## mislabelled hospitalization and fix some dates

# 
data$e_hosp_date[which(data$a_record_id == "189-4")] <- "2020-04-23"
data$e_hosp_date[which(data$a_record_id == "192-33")] <- "2020-12-29"
data$e_hosp_date[which(data$a_record_id == "189-103")] <- NA

## estimated date of delivery
View(data[which(data$a_record_id == "189-160"), c("a_record_id", "e_diagnosis", "d_naso1_collect", "d_other_collect", "d_naso1_result", "d_naso2_collect", "d_naso2_result", "r_dob", "i_deliverydate_est", "i_lmp", "time_del", "p_outcome", "o_loss", "r_discharge")])

# 
# data$i_deliverydate_est[which(data$a_record_id == "186-46")] <- "2021-03-23"
data$i_deliverydate_est[which(data$a_record_id == "192-30")] <- "2020-11-15"
data$i_lmp[which(data$a_record_id == "190-45")] <- "2020-05-19"
data$i_deliverydate_est[which(data$a_record_id == "204-30")] <- "2020-11-03"
# data$i_deliverydate_est[which(data$a_record_id == "204-86")] <- "2021-04-06"
data$i_deliverydate_est[which(data$a_record_id == "337-2")] <- "2020-12-29"
# data$i_deliverydate_est[which(data$a_record_id == "187-1")] <- "2021-01-02"
# data$i_deliverydate_est[which(data$a_record_id == "190-22")] <- "2020-05-22"
data$i_deliverydate_est[which(data$a_record_id == "190-41")] <- "2020-11-26"
# data$i_deliverydate_est[which(data$a_record_id == "204-74")] <- "2021-11-06"
# data$i_deliverydate_est[which(data$a_record_id == "204-120")] <- "2021-06-28"
# data$i_deliverydate_est[which(data$a_record_id == "204-131")] <- "2021-07-13"
# data$i_deliverydate_est[which(data$a_record_id == "189-31")] <- "2020-10-06"
# data$i_deliverydate_est[which(data$a_record_id == "191-10")] <- "2020-11-21"
# data$i_deliverydate_est[which(data$a_record_id == "193-21")] <- "2020-11-26"
# data$i_deliverydate_est[which(data$a_record_id == "189-141")] <- "2021-04-02"
# data$i_deliverydate_est[which(data$a_record_id == "188-23")] <- "2022-03-16"
# data$i_deliverydate_est[which(data$a_record_id == "189-38")] <- "2021-02-07"
# data$i_deliverydate_est[which(data$a_record_id == "189-127")] <- "2022-05-08"
# data$i_deliverydate_est[which(data$a_record_id == "189-130")] <- "2022-04-27"
# data$i_deliverydate_est[which(data$a_record_id == "189-144")] <- "2022-06-03"
# data$i_deliverydate_est[which(data$a_record_id == "337-6")] <- "2021-02-01"
# data$i_deliverydate_est[which(data$a_record_id == "189-111")] <- "2021-10-24"
# data$i_deliverydate_est[which(data$a_record_id == "1204-217")] <- "2021-06-06"
# data$i_deliverydate_est[which(data$a_record_id == "186-141")] <- "2021-12-18"
# data$i_deliverydate_est[which(data$a_record_id == "186-155")] <- "2022-01-13"
# data$i_deliverydate_est[which(data$a_record_id == "189-157")] <- "2022-01-31"
# data$i_deliverydate_est[which(data$a_record_id == "189-160")] <- "2022-01-20"
# data$i_deliverydate_est[which(data$a_record_id == "189-169")] <- "2022-01-09"
# data$i_deliverydate_est[which(data$a_record_id == "204-307")] <- "2022-04-06"
# data$i_deliverydate_est[which(data$a_record_id == "189-115")] <- "2022-01-20"
# data$i_deliverydate_est[which(data$a_record_id == "337-15")] <- "2022-03-21"



# within 2 weeks of delivery?
# data <- data[-which(data$time_del < -14), ]
# 

## 

write.csv(data, here("QC_CLEANED_2023-02-27.csv"))


############ REST OF PROVINCES ###############

### load data ####
MB <- read.csv(here("COVID_PREG_cleaned_MB_NB_PE_2023-02-26.csv"))

####  ALSO NOTE CAN FILL IN MISSING DATES USING E-DIAGNOSIS FOR NASO1 AND VICE VERSA - but then some have to be updated below because their positive test was noso2 or naso3 etc

MB$e_diagnosis <- MB$e_diagnosis.x
MB$e_hosp_dur


MB$e_hosp_date

data <- MB

# remove this participant for having ga at diagnosis more than 8 weeks before lmp

# remove this participant for having ga at diagnosis more than 3 weeks before lmp


# no diagnosis information or dates
data <- data[-which(data$a_record_id == "MB0097"), ]


## fix some diagnosis dates
data$e_diagnosis <- as.Date(data$e_diagnosis)
data$r_dob <- as.Date(data$r_dob)

## fix time_del
data$time_del <- as.numeric(data$r_dob - data$e_diagnosis)
summary(data$time_del)

# data$d_naso1_collect

View(data[which(data$time_del > 280), c("a_record_id", "e_diagnosis", "d_naso1_collect.x", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# 
View(data[which(data$time_del < -10), c("a_record_id", "e_diagnosis", "d_naso1_collect.x", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])

View(data[which(data$a_record_id == "CCP-01-173"), c("a_record_id", "e_diagnosis", "d_naso1_collect", "r_dob", "i_deliverydate_est", "i_lmp", "time_del", "p_outcome", "o_loss", "r_discharge")])

## some of these are obvious typos
#r_dob
data$r_dob[which(data$a_record_id == "MB0376")] <- "2021-08-01"
data$r_dob[which(data$a_record_id == "MB0602")] <- "2021-11-30"
data$r_dob[which(data$a_record_id == "MB0664")] <- "2021-06-12"
data$r_dob[which(data$a_record_id == "MB0046")] <- "2021-02-06"
data$r_dob[which(data$a_record_id == "MB0326")] <- "2021-09-11"
data$r_dob[which(data$a_record_id == "MB0543")] <- "2022-02-04"
data$r_dob[which(data$a_record_id == "MB0599")] <- "2021-07-23"
data$r_dob[which(data$a_record_id == "MB0620")] <- "2021-10-29"
data$r_dob[which(data$a_record_id == "MB0621")] <- "2021-11-15"
data$r_dob[which(data$a_record_id == "MB0624")] <- "2021-03-04"
data$r_dob[which(data$a_record_id == "MB0638")] <- "2021-06-01"
data$r_dob[which(data$a_record_id == "MB0720")] <- "2021-10-14"
data$r_dob[which(data$a_record_id == "YT - 0065")] <- "2022-03-24"


#e_diagnosis
data$e_diagnosis[which(data$a_record_id == "MB0249")] <- "2021-04-20"
data$e_diagnosis[which(data$a_record_id == "MB0513")] <- "2021-12-01"
data$e_diagnosis[which(data$a_record_id == "MB0036")] <- "2020-11-15"
data$e_diagnosis[which(data$a_record_id == "MB0064")] <- "2020-11-28"
data$e_diagnosis[which(data$a_record_id == "MB0117")] <- "2020-11-21"
data$e_diagnosis[which(data$a_record_id == "MB0138")] <- "2020-09-04"
data$e_diagnosis[which(data$a_record_id == "MB0139")] <- "2020-09-23"
data$e_diagnosis[which(data$a_record_id == "MB0147")] <- "2020-09-22"
data$e_diagnosis[which(data$a_record_id == "MB0172")] <- "2021-10-10"
data$e_diagnosis[which(data$a_record_id == "MB0209")] <- "2020-11-16"
data$e_diagnosis[which(data$a_record_id == "MB0240")] <- "2020-12-24"
data$e_diagnosis[which(data$a_record_id == "MB0241")] <- "2020-12-04"
data$e_diagnosis[which(data$a_record_id == "MB0254")] <- "2020-10-29"
data$e_diagnosis[which(data$a_record_id == "MB0267")] <- "2021-11-28"
data$e_diagnosis[which(data$a_record_id == "MB0301")] <- "2020-11-14"
data$e_diagnosis[which(data$a_record_id == "MB0317")] <- "2021-05-23"
data$e_diagnosis[which(data$a_record_id == "MB0344")] <- "2020-11-08"
data$e_diagnosis[which(data$a_record_id == "MB0414")] <- "2020-12-07"
data$e_diagnosis[which(data$a_record_id == "MB0468")] <- "2021-09-03"
data$e_diagnosis[which(data$a_record_id == "MB0531")] <- "2021-12-25"
data$e_diagnosis[which(data$a_record_id == "MB0623")] <- "2021-09-02"
data$e_diagnosis[which(data$a_record_id == "MB0679")] <- "2021-09-09"
data$e_diagnosis[which(data$a_record_id == "MB0688")] <- "2021-03-29"
data$e_diagnosis[which(data$a_record_id == "MB0692")] <- "2021-07-30"
data$e_diagnosis[which(data$a_record_id == "NB-00010")] <- "2021-06-08"





##d_naso1_collect.x

## delete bc diagnosis date well after or well before
data <- data[-which(data$a_record_id == "MB0605"),]

data <- data[-which(data$a_record_id == "MB0165"),]
data <- data[-which(data$a_record_id == "MB0296"),]
data <- data[-which(data$a_record_id == "MB0546"),]
data <- data[-which(data$a_record_id == "MB0563"),]
data <- data[-which(data$a_record_id == "MB0630"),]







## just confusing may need to be deleted

data <- data[-which(data$a_record_id == "MB0595"),]
data <- data[-which(data$a_record_id == "MB0685"),]
data <- data[-which(data$a_record_id == "YT - 0066"),]
data <- data[-which(data$a_record_id == "MB0621"),]


# MB0024

# MB0621 # double check this one




# data$i_lmp[which(data$a_record_id == "MB0301")] <- "2020-06-21"

data$i_lmp[which(data$a_record_id == "MB0643")] <- "2020-11-203"






## fixing some other dates
data$d_naso1_collect.x[which(data$a_record_id == "MB0022")] <- "2020-11-23"
data$d_naso1_collect.x[which(data$a_record_id == "MB0122")] <- "2020-11-30"
data$d_naso1_collect.x[which(data$a_record_id == "MB0164")] <- "2020-08-09"
# data$d_naso1_collect.x[which(data$a_record_id == "MB0165")] <- "2020-11-27"

# data$r_dob[which(data$a_record_id == "MB-0043")] <- "2020-12-01"
# data$r_dob[which(data$a_record_id == "MB0326")] <- "2021-09-11"
#

#### DOUBLE CHECK ALL OF THESE LATER
# data$e_diagnosis[which(data$a_record_id == "MB0047")] <- "2020-10-28"
# data$e_diagnosis[which(data$a_record_id == "MB0051")] <- "2020-11-19"
# data$e_diagnosis[which(data$a_record_id == "MB0218")] <- "2020-11-09"
# data$e_diagnosis[which(data$a_record_id == "MB0225")] <- "2020-11-20"
# data$e_diagnosis[which(data$a_record_id == "MB0296")] <- "2020-11-07"
# data$e_diagnosis[which(data$a_record_id == "MB0315")] <- "2021-07-13"
# data$e_diagnosis[which(data$a_record_id == "MB0034")] <- "2020-10-31"
# data$e_diagnosis[which(data$a_record_id == "MB0117")] <- "2020-11-21"
# data$e_diagnosis[which(data$a_record_id == "MB0138")] <- "2020-09-04"
# data$e_diagnosis[which(data$a_record_id == "MB0139")] <- "2020-09-23"
# data$e_diagnosis[which(data$a_record_id == "MB0147")] <- "2020-09-22"
# data$e_diagnosis[which(data$a_record_id == "MB0064")] <- "2020-11-28"
# data$e_diagnosis[which(data$a_record_id == "MB0310")] <- "2020-11-26"
# data$e_diagnosis[which(data$a_record_id == "MB0320")] <- "2020-11-22"
# data$e_diagnosis[which(data$a_record_id == "MB0022")] <- "2020-11-23"
# data$e_diagnosis[which(data$a_record_id == "MB0122")] <- "2020-11-30"
# data$e_diagnosis[which(data$a_record_id == "MB0164")] <- "2020-08-09"
# data$e_diagnosis[which(data$a_record_id == "MB0165")] <- "2020-11-27"
# data$e_diagnosis[which(data$a_record_id == "MB0324")] <- "2021-07-02"
# data$e_diagnosis[which(data$a_record_id == "MB0119")] <- "2020-10-01"
# data$e_diagnosis[which(data$a_record_id == "MB0036")] <- "2020-11-15"
# data$e_diagnosis[which(data$a_record_id == "MB0172")] <- "2020-10-10"
# data$e_diagnosis[which(data$a_record_id == "MB0209")] <- "2020-11-16"
# data$e_diagnosis[which(data$a_record_id == "MB0240")] <- "2020-12-24"
# data$e_diagnosis[which(data$a_record_id == "MB0241")] <- "2020-12-04"
# data$e_diagnosis[which(data$a_record_id == "MB0254")] <- "2020-10-29"
# data$e_diagnosis[which(data$a_record_id == "MB0267")] <- "2020-11-28"
# data$e_diagnosis[which(data$a_record_id == "MB0301")] <- "2020-11-14"
# data$e_diagnosis[which(data$a_record_id == "MB0317")] <- "2021-05-23"
# data$e_diagnosis[which(data$a_record_id == "MB0344")] <- "2020-11-05"
# data$e_diagnosis[which(data$a_record_id == "MB0414")] <- "2020-12-07"
# data$e_diagnosis[which(data$a_record_id == "MB0249")] <- "2021-04-20"
# data$e_diagnosis[which(data$a_record_id == "MB0030")] <- "2020-11-13"



## mislabelled hospitalization and fix some dates
data$e_hosp[which(data$a_record_id == "MB0108")] <- 0
data$e_hosp_date[which(data$a_record_id == "MB0108")] <- NA

data$e_hosp_date[which(data$a_record_id == "MB0129")] <- "2020-11-25"
data$e_hosp_date[which(data$a_record_id == "MB0055")] <- "2020-11-14"






## estimated date of delivery
 data$i_deliverydate_est[which(data$a_record_id == "MB0024")] <- "2020-11-04"
# data$i_deliverydate_est[which(data$a_record_id == "MB0188")] <- "2021-01-23"
data$i_deliverydate_est[which(data$a_record_id == "MB-0010")] <- "2021-04-12"
data$i_deliverydate_est[which(data$a_record_id == "MB0130")] <- "2021-02-16"
data$i_deliverydate_est[which(data$a_record_id == "MB0157")] <- "2021-01-25"
data$i_deliverydate_est[which(data$a_record_id == "MB0207")] <- "2021-03-30"
data$i_deliverydate_est[which(data$a_record_id == "MB0297")] <- "2022-02-21"
data$i_deliverydate_est[which(data$a_record_id == "MB0313")] <- "2021-07-22"
data$i_deliverydate_est[which(data$a_record_id == "MB0373")] <- "2021-09-22"
data$i_deliverydate_est[which(data$a_record_id == "MB0391")] <- "2021-12-11"
# data$i_deliverydate_est[which(data$a_record_id == "MB0188")] <- "2022-01-23"
data$i_deliverydate_est[which(data$a_record_id == "MB0363")] <- "2021-10-05"
data$i_deliverydate_est[which(data$a_record_id == "MB0406")] <- "2021-06-16"
data$i_deliverydate_est[which(data$a_record_id == "MB0434")] <- "2021-08-06"
data$i_deliverydate_est[which(data$a_record_id == "MB0500")] <- "2022-04-25"
data$i_deliverydate_est[which(data$a_record_id == "MB0495")] <- "2022-01-15"
data$i_deliverydate_est[which(data$a_record_id == "MB0570")] <- "2022-06-29"
data$i_deliverydate_est[which(data$a_record_id == "MB0573")] <- "2022-02-28"
data$i_deliverydate_est[which(data$a_record_id == "MB0575")] <- "2022-02-09"
data$i_deliverydate_est[which(data$a_record_id == "MB0593")] <- "2021-11-25"
data$i_deliverydate_est[which(data$a_record_id == "MB0594")] <- "2022-02-08"
data$i_deliverydate_est[which(data$a_record_id == "MB0615")] <- "2021-02-11"
data$i_deliverydate_est[which(data$a_record_id == "MB0625")] <- "2022-04-11"
# data$i_deliverydate_est[which(data$a_record_id == "MB0643")] <- as.Date(data$i_lmp[which(data$a_record_id == "MB0643")])  + 280
data$i_deliverydate_est[which(data$a_record_id == "Mb0721")] <- "2022-03-30"
data$i_deliverydate_est[which(data$a_record_id == "NB-00012")] <- "2022-03-31"
data$i_deliverydate_est[which(data$a_record_id == "NB-00094")] <- "2022-09-02"
data$i_deliverydate_est[which(data$a_record_id == "YT - 0059")] <- "2022-03-12"




data <- data[-which(data$a_record_id == "MB0643"),]
data <- data[-which(data$a_record_id == "NB-00001"),]
data <- data[-which(data$a_record_id == "NB-00095"),]
data <- data[-which(data$a_record_id == "NB-00195"),]
data <- data[-which(data$a_record_id == "NB-00206"),]
data <- data[-which(data$a_record_id == "YT - 0036"),]
data <- data[-which(data$a_record_id == "YT - 0076"),]
data <- data[-which(data$a_record_id == "PE-0325"),]


# MB0643 #delete this one
# NB-00001
# NB-00095
# NB-00195	
# NB-00206
# YT - 0036
# YT - 0076
# data$i_lmp <- ifelse(data$i_lmp == "9999-09-09", NA, as.character(data$i_lmp))
# 
# data$i_deliverydate_est[which(is.na(data$i_deliverydate_est) & !is.na(data$i_lmp))] <- as.Date(data$i_lmp[which(is.na(data$i_deliverydate_est) & !is.na(data$i_lmp))]) + 280

data$time_diff <- as.Date(data$i_deliverydate_est) - as.Date(data$r_dob)
View(data[which(abs(as.Date(data$i_deliverydate_est) - as.Date(data$r_dob)) > 60), c("a_record_id", "e_diagnosis", "d_naso1_collect.x", "d_other_collect", "d_naso1_result", "d_naso2_collect", "d_naso2_result", "r_dob", "i_deliverydate_est", "i_lmp", "time_del", "p_outcome", "o_loss", "r_discharge","time_diff")])


# within 2 weeks of delivery?
# data <- data[-which(data$time_del < -14), ]
# 
# length(which(is.na(data$e_diagnosis)))
# 
# ## to Nov 30th
# data <- data[which(data$e_diagnosis <= "2021-11-30"), ]

## how many from each province
# describeFactors(data$province)

## 

write.csv(data, here("MB_NB_PE_YT_CLEANED_2023-02-27.csv"))


