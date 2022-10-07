# This file contains the code that produces the analysis located at:
# https://www.linkedin.com/pulse/health-plan-quality-improvement-cost-containment-versus-jeff-passaro/?trackingId=G01LQMNGQWS%2FK7Xp6e09kw%3D%3D
# Health Plan Quality Improvement & Cost Containment Investment Versus Growth


#--------------------------------------------------------------------
# This code develops an analysis that shows growth vs investment in 
# QI activities for health plans from 2015 to 2019.
#--------------------------------------------------------------------

setwd("~/ds/data/mlr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr") 
library(dplyr) 
library(tidyverse)
library(readxl)
library(sqldf)
library(ggplot2)
library(tidyr)
options(scipen = 999)

#This step brings in the header information for the Health Plan

header_in <- read_csv("MR_Submission_Template_Header_2019.csv")
names(header_in) <- tolower(names(header_in))
header <- subset(header_in, select =  c(mr_submission_template_id,
                                     business_state,
                                     group_affiliation,
                                     hios_issuer_id,
                                     company_name,
                                     domiciliary_state,
                                     federal_ein,
                                     dba_marketing_name,
                                     not_for_profit,
                                     merge_markets_ind_small_grp,
                                     fit_exempt)) %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

hios <- subset(header_in, select =  c(mr_submission_template_id,hios_issuer_id)) %>%
  rename(hios_id = hios_issuer_id)

#This step brings in the summary financial information for Health Plan 2019

summary_in <- read_csv("Part1_2_Summary_Data_Premium_Claims_2019.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
summary <- summary_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

rebate_in <- read_csv("Part3_MLR_Rebate_Calculation_2019.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
rebate <- rebate_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

year = "19"

membership <- summary %>%
  subset(row_lookup_code =="NUMBER_OF_COVERED_LIVES" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
            cmm_small_group_yearly >= 1 |
            cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mbr = cmm_individual_q1,
         sg_mbr = cmm_small_group_q1,
         lg_mbr = cmm_large_group_q1)

  
member_months <- summary %>%
  subset(row_lookup_code =="MEMBER_MONTHS" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mm = cmm_individual_q1,
         sg_mm = cmm_small_group_q1,
         lg_mm = cmm_large_group_q1)

cc <- summary %>%
  subset(row_lookup_code == "COST_CONTAINMENT_EXP_NOT_INCL" & hios_id != 0 &
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_cc = cmm_individual_q1,
         sg_cc = cmm_small_group_q1,
         lg_cc = cmm_large_group_q1)


premium <- rebate %>%
  subset((row_lookup_code == "PREMIUM_EARNED_INCLUDING_FSHRP") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_rev = cmm_individual_cy,
         sg_rev = cmm_small_group_cy,
         lg_rev = cmm_large_group_cy)


taxfees <- rebate %>%
  subset((row_lookup_code == "FED_STATE_TAXES_LIC_OR_REG_FEE") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_tax = cmm_individual_cy,
         sg_tax = cmm_small_group_cy,
         lg_tax = cmm_large_group_cy)


claims <- rebate %>%
  subset((row_lookup_code == "ADJ_INCURRED_CLAIMS_RESTATED_Q1") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_clm = cmm_individual_cy,
         sg_clm = cmm_small_group_cy,
         lg_clm = cmm_large_group_cy)


qi <- rebate %>%
  subset((row_lookup_code == "QUALITY_IMPROVEMENT_EXPENSES") & hios_id != 0 &
           (cmm_individual_cy != 0 |
            cmm_small_group_cy != 0 |
            cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_qi = cmm_individual_cy,
         sg_qi = cmm_small_group_cy,
         lg_qi = cmm_large_group_cy)

ra <- rebate %>%
  subset((row_lookup_code == "FED_RISK_ADJ_NET_PAYMENTS_HHS") & hios_id != 0 & 
                  (cmm_individual_cy != 0 |
                  cmm_small_group_cy != 0 |
                  cmm_large_group_cy != 0)) %>%
           select(hios_id, 
                  cmm_individual_cy, 
                  cmm_small_group_cy, 
                  cmm_large_group_cy) %>%
           rename(ind_ra = cmm_individual_cy,
                  sg_ra = cmm_small_group_cy,
                  lg_ra = cmm_large_group_cy)



fullset_2019 <- membership %>%
  left_join(member_months, by=c("hios_id")) %>%
  left_join(premium, by=c("hios_id")) %>%
  left_join(taxfees, by=c("hios_id")) %>%
  left_join(claims, by=c("hios_id")) %>%
  left_join(qi, by=c("hios_id")) %>%
  left_join(cc, by=c("hios_id")) %>%
  left_join(ra, by=c("hios_id")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate(ind_mrg = ind_rev - ind_tax - ind_clm + ind_ra - ind_qi - ind_cc,
         sg_mrg  = sg_rev - sg_tax - sg_clm + sg_ra - sg_qi - sg_cc,
         lg_mrg  = lg_rev - lg_tax - lg_clm + lg_ra - lg_qi - sg_cc,
         ind_mlr = (ind_clm - ind_ra + ind_qi)/(ind_rev - ind_tax),
         sg_mlr  = (sg_clm - sg_ra + sg_qi)/(sg_rev - sg_tax),
         lg_mlr  = (lg_clm - lg_ra + lg_qi)/(lg_rev - lg_tax),
         ind_inv = ind_qi + ind_cc,
         sg_inv  = sg_qi + sg_cc,
         lg_inv  = lg_qi + lg_cc) %>%
  arrange(-ind_mm) %>%
  rename_at(vars(-hios_id),function(x) paste0(x, sep = "_" , year)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 

## Here we bring in 2018 Data

header_in <- read_csv("MR_Submission_Template_Header_2018.csv")
names(header_in) <- tolower(names(header_in))
header <- subset(header_in, select =  c(mr_submission_template_id,
                                        business_state,
                                        group_affiliation,
                                        hios_issuer_id,
                                        company_name,
                                        domiciliary_state,
                                        federal_ein,
                                        dba_marketing_name,
                                        not_for_profit,
                                        merge_markets_ind_small_grp,
                                        fit_exempt)) %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

hios <- subset(header_in, select =  c(mr_submission_template_id,hios_issuer_id)) %>%
  rename(hios_id = hios_issuer_id)

summary_in <- read_csv("Part1_2_Summary_Data_Premium_Claims_2018.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
summary <- summary_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

rebate_in <- read_csv("Part3_MLR_Rebate_Calculation_2018.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
rebate <- rebate_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

year = "18"

membership <- summary %>%
  subset(row_lookup_code =="NUMBER_OF_COVERED_LIVES" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mbr = cmm_individual_q1,
         sg_mbr = cmm_small_group_q1,
         lg_mbr = cmm_large_group_q1)


member_months <- summary %>%
  subset(row_lookup_code =="MEMBER_MONTHS" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mm = cmm_individual_q1,
         sg_mm = cmm_small_group_q1,
         lg_mm = cmm_large_group_q1)

cc <- summary %>%
  subset(row_lookup_code == "COST_CONTAINMENT_EXP_NOT_INCL" & hios_id != 0 &
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_cc = cmm_individual_q1,
         sg_cc = cmm_small_group_q1,
         lg_cc = cmm_large_group_q1)


premium <- rebate %>%
  subset((row_lookup_code == "PREMIUM_EARNED_INCLUDING_FSHRP") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_rev = cmm_individual_cy,
         sg_rev = cmm_small_group_cy,
         lg_rev = cmm_large_group_cy)


taxfees <- rebate %>%
  subset((row_lookup_code == "FED_STATE_TAXES_LIC_OR_REG_FEE") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_tax = cmm_individual_cy,
         sg_tax = cmm_small_group_cy,
         lg_tax = cmm_large_group_cy)


claims <- rebate %>%
  subset((row_lookup_code == "ADJ_INCURRED_CLAIMS_RESTATED_Q1") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_clm = cmm_individual_cy,
         sg_clm = cmm_small_group_cy,
         lg_clm = cmm_large_group_cy)


qi <- rebate %>%
  subset((row_lookup_code == "QUALITY_IMPROVEMENT_EXPENSES") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_qi = cmm_individual_cy,
         sg_qi = cmm_small_group_cy,
         lg_qi = cmm_large_group_cy)

ra <- rebate %>%
  subset((row_lookup_code == "FED_RISK_ADJ_NET_PAYMENTS_HHS") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_ra = cmm_individual_cy,
         sg_ra = cmm_small_group_cy,
         lg_ra = cmm_large_group_cy)



fullset_2018 <- membership %>%
  left_join(member_months, by=c("hios_id")) %>%
  left_join(premium, by=c("hios_id")) %>%
  left_join(taxfees, by=c("hios_id")) %>%
  left_join(claims, by=c("hios_id")) %>%
  left_join(qi, by=c("hios_id")) %>%
  left_join(cc, by=c("hios_id")) %>%
  left_join(ra, by=c("hios_id")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate(ind_mrg = ind_rev - ind_tax - ind_clm + ind_ra - ind_qi - ind_cc,
         sg_mrg  = sg_rev - sg_tax - sg_clm + sg_ra - sg_qi - sg_cc,
         lg_mrg  = lg_rev - lg_tax - lg_clm + lg_ra - lg_qi - sg_cc,
         ind_mlr = (ind_clm - ind_ra + ind_qi)/(ind_rev - ind_tax),
         sg_mlr  = (sg_clm - sg_ra + sg_qi)/(sg_rev - sg_tax),
         lg_mlr  = (lg_clm - lg_ra + lg_qi)/(lg_rev - lg_tax),
         ind_inv = ind_qi + ind_cc,
         sg_inv  = sg_qi + sg_cc,
         lg_inv  = lg_qi + lg_cc) %>%
  arrange(-ind_mm) %>%
  rename_at(vars(-hios_id),function(x) paste0(x, sep = "_" , year)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 



## Here we bring in 2017 Data

header_in <- read_csv("MR_Submission_Template_Header_2017.csv")
names(header_in) <- tolower(names(header_in))
header <- subset(header_in, select =  c(mr_submission_template_id,
                                        business_state,
                                        group_affiliation,
                                        hios_issuer_id,
                                        company_name,
                                        domiciliary_state,
                                        federal_ein,
                                        dba_marketing_name,
                                        not_for_profit,
                                        merge_markets_ind_small_grp,
                                        fit_exempt)) %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

hios <- subset(header_in, select =  c(mr_submission_template_id,hios_issuer_id)) %>%
  rename(hios_id = hios_issuer_id)

summary_in <- read_csv("Part1_2_Summary_Data_Premium_Claims_2017.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
summary <- summary_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

rebate_in <- read_csv("Part3_MLR_Rebate_Calculation_2017.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
rebate <- rebate_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

year = "17"

membership <- summary %>%
  subset(row_lookup_code =="NUMBER_OF_COVERED_LIVES" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mbr = cmm_individual_q1,
         sg_mbr = cmm_small_group_q1,
         lg_mbr = cmm_large_group_q1)


member_months <- summary %>%
  subset(row_lookup_code =="MEMBER_MONTHS" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mm = cmm_individual_q1,
         sg_mm = cmm_small_group_q1,
         lg_mm = cmm_large_group_q1)

cc <- summary %>%
  subset(row_lookup_code == "COST_CONTAINMENT_EXP_NOT_INCL" & hios_id != 0 &
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_cc = cmm_individual_q1,
         sg_cc = cmm_small_group_q1,
         lg_cc = cmm_large_group_q1)


premium <- rebate %>%
  subset((row_lookup_code == "PREMIUM_EARNED_INCLUDING_FSHRP") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_rev = cmm_individual_cy,
         sg_rev = cmm_small_group_cy,
         lg_rev = cmm_large_group_cy)


taxfees <- rebate %>%
  subset((row_lookup_code == "FED_STATE_TAXES_LIC_OR_REG_FEE") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_tax = cmm_individual_cy,
         sg_tax = cmm_small_group_cy,
         lg_tax = cmm_large_group_cy)


claims <- rebate %>%
  subset((row_lookup_code == "ADJ_INCURRED_CLAIMS_RESTATED_Q1") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_clm = cmm_individual_cy,
         sg_clm = cmm_small_group_cy,
         lg_clm = cmm_large_group_cy)


qi <- rebate %>%
  subset((row_lookup_code == "QUALITY_IMPROVEMENT_EXPENSES") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_qi = cmm_individual_cy,
         sg_qi = cmm_small_group_cy,
         lg_qi = cmm_large_group_cy)

ra <- rebate %>%
  subset((row_lookup_code == "FED_RISK_ADJ_NET_PAYMENTS_HHS") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_ra = cmm_individual_cy,
         sg_ra = cmm_small_group_cy,
         lg_ra = cmm_large_group_cy)



fullset_2017 <- membership %>%
  left_join(member_months, by=c("hios_id")) %>%
  left_join(premium, by=c("hios_id")) %>%
  left_join(taxfees, by=c("hios_id")) %>%
  left_join(claims, by=c("hios_id")) %>%
  left_join(qi, by=c("hios_id")) %>%
  left_join(cc, by=c("hios_id")) %>%
  left_join(ra, by=c("hios_id")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate(ind_mrg = ind_rev - ind_tax - ind_clm + ind_ra - ind_qi - ind_cc,
         sg_mrg  = sg_rev - sg_tax - sg_clm + sg_ra - sg_qi - sg_cc,
         lg_mrg  = lg_rev - lg_tax - lg_clm + lg_ra - lg_qi - sg_cc,
         ind_mlr = (ind_clm - ind_ra + ind_qi)/(ind_rev - ind_tax),
         sg_mlr  = (sg_clm - sg_ra + sg_qi)/(sg_rev - sg_tax),
         lg_mlr  = (lg_clm - lg_ra + lg_qi)/(lg_rev - lg_tax),
         ind_inv = ind_qi + ind_cc,
         sg_inv  = sg_qi + sg_cc,
         lg_inv  = lg_qi + lg_cc) %>%
  arrange(-ind_mm) %>%
  rename_at(vars(-hios_id),function(x) paste0(x, sep = "_" , year)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 

## Here we bring in 2016 Data

header_in <- read_csv("MR_Submission_Template_Header_2016.csv")
names(header_in) <- tolower(names(header_in))
header <- subset(header_in, select =  c(mr_submission_template_id,
                                        business_state,
                                        group_affiliation,
                                        hios_issuer_id,
                                        company_name,
                                        domiciliary_state,
                                        federal_ein,
                                        dba_marketing_name,
                                        not_for_profit,
                                        merge_markets_ind_small_grp,
                                        fit_exempt)) %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

hios <- subset(header_in, select =  c(mr_submission_template_id,hios_issuer_id)) %>%
  rename(hios_id = hios_issuer_id)

summary_in <- read_csv("Part1_2_Summary_Data_Premium_Claims_2016.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
summary <- summary_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

rebate_in <- read_csv("Part3_MLR_Rebate_Calculation_2016.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
rebate <- rebate_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

year = "16"

membership <- summary %>%
  subset(row_lookup_code =="NUMBER_OF_COVERED_LIVES" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mbr = cmm_individual_q1,
         sg_mbr = cmm_small_group_q1,
         lg_mbr = cmm_large_group_q1)


member_months <- summary %>%
  subset(row_lookup_code =="MEMBER_MONTHS" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mm = cmm_individual_q1,
         sg_mm = cmm_small_group_q1,
         lg_mm = cmm_large_group_q1)

cc <- summary %>%
  subset(row_lookup_code == "COST_CONTAINMENT_EXP_NOT_INCL" & hios_id != 0 &
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_cc = cmm_individual_q1,
         sg_cc = cmm_small_group_q1,
         lg_cc = cmm_large_group_q1)


premium <- rebate %>%
  subset((row_lookup_code == "PREMIUM_EARNED_INCLUDING_FSHRP") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_rev = cmm_individual_cy,
         sg_rev = cmm_small_group_cy,
         lg_rev = cmm_large_group_cy)


taxfees <- rebate %>%
  subset((row_lookup_code == "FED_STATE_TAXES_LIC_OR_REG_FEE") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_tax = cmm_individual_cy,
         sg_tax = cmm_small_group_cy,
         lg_tax = cmm_large_group_cy)


claims <- rebate %>%
  subset((row_lookup_code == "ADJ_INCURRED_CLAIMS_RESTATED_Q1") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_clm = cmm_individual_cy,
         sg_clm = cmm_small_group_cy,
         lg_clm = cmm_large_group_cy)


qi <- rebate %>%
  subset((row_lookup_code == "QUALITY_IMPROVEMENT_EXPENSES") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_qi = cmm_individual_cy,
         sg_qi = cmm_small_group_cy,
         lg_qi = cmm_large_group_cy)

ra <- rebate %>%
  subset((row_lookup_code == "FED_RISK_ADJ_NET_PAYMENTS_HHS") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_ra = cmm_individual_cy,
         sg_ra = cmm_small_group_cy,
         lg_ra = cmm_large_group_cy)



fullset_2016 <- membership %>%
  left_join(member_months, by=c("hios_id")) %>%
  left_join(premium, by=c("hios_id")) %>%
  left_join(taxfees, by=c("hios_id")) %>%
  left_join(claims, by=c("hios_id")) %>%
  left_join(qi, by=c("hios_id")) %>%
  left_join(cc, by=c("hios_id")) %>%
  left_join(ra, by=c("hios_id")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate(ind_mrg = ind_rev - ind_tax - ind_clm + ind_ra - ind_qi - ind_cc,
         sg_mrg  = sg_rev - sg_tax - sg_clm + sg_ra - sg_qi - sg_cc,
         lg_mrg  = lg_rev - lg_tax - lg_clm + lg_ra - lg_qi - sg_cc,
         ind_mlr = (ind_clm - ind_ra + ind_qi)/(ind_rev - ind_tax),
         sg_mlr  = (sg_clm - sg_ra + sg_qi)/(sg_rev - sg_tax),
         lg_mlr  = (lg_clm - lg_ra + lg_qi)/(lg_rev - lg_tax),
         ind_inv = ind_qi + ind_cc,
         sg_inv  = sg_qi + sg_cc,
         lg_inv  = lg_qi + lg_cc) %>%
  arrange(-ind_mm) %>%
  rename_at(vars(-hios_id),function(x) paste0(x, sep = "_" , year)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 

## Here we bring in 2015 Data

header_in <- read_csv("MR_Submission_Template_Header_2015.csv")
names(header_in) <- tolower(names(header_in))
header <- subset(header_in, select =  c(mr_submission_template_id,
                                        business_state,
                                        group_affiliation,
                                        hios_issuer_id,
                                        company_name,
                                        domiciliary_state,
                                        federal_ein,
                                        dba_marketing_name,
                                        not_for_profit,
                                        merge_markets_ind_small_grp,
                                        fit_exempt)) %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

hios <- subset(header_in, select =  c(mr_submission_template_id,hios_issuer_id)) %>%
  rename(hios_id = hios_issuer_id)

summary_in <- read_csv("Part1_2_Summary_Data_Premium_Claims_2015.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
summary <- summary_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

rebate_in <- read_csv("Part3_MLR_Rebate_Calculation_2015.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
rebate <- rebate_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

year = "15"

membership <- summary %>%
  subset(row_lookup_code =="NUMBER_OF_COVERED_LIVES" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mbr = cmm_individual_q1,
         sg_mbr = cmm_small_group_q1,
         lg_mbr = cmm_large_group_q1)


member_months <- summary %>%
  subset(row_lookup_code =="MEMBER_MONTHS" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mm = cmm_individual_q1,
         sg_mm = cmm_small_group_q1,
         lg_mm = cmm_large_group_q1)

cc <- summary %>%
  subset(row_lookup_code == "COST_CONTAINMENT_EXP_NOT_INCL" & hios_id != 0 &
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_cc = cmm_individual_q1,
         sg_cc = cmm_small_group_q1,
         lg_cc = cmm_large_group_q1)


premium <- rebate %>%
  subset((row_lookup_code == "PREMIUM_EARNED_INCLUDING_FSHRP") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_rev = cmm_individual_cy,
         sg_rev = cmm_small_group_cy,
         lg_rev = cmm_large_group_cy)


taxfees <- rebate %>%
  subset((row_lookup_code == "FED_STATE_TAXES_LIC_OR_REG_FEE") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_tax = cmm_individual_cy,
         sg_tax = cmm_small_group_cy,
         lg_tax = cmm_large_group_cy)


claims <- rebate %>%
  subset((row_lookup_code == "ADJ_INCURRED_CLAIMS_RESTATED_Q1") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_clm = cmm_individual_cy,
         sg_clm = cmm_small_group_cy,
         lg_clm = cmm_large_group_cy)


qi <- rebate %>%
  subset((row_lookup_code == "QUALITY_IMPROVEMENT_EXPENSES") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_qi = cmm_individual_cy,
         sg_qi = cmm_small_group_cy,
         lg_qi = cmm_large_group_cy)

ra <- rebate %>%
  subset((row_lookup_code == "FED_RISK_ADJ_NET_PAYMENTS_HHS") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_ra = cmm_individual_cy,
         sg_ra = cmm_small_group_cy,
         lg_ra = cmm_large_group_cy)



fullset_2015 <- membership %>%
  left_join(member_months, by=c("hios_id")) %>%
  left_join(premium, by=c("hios_id")) %>%
  left_join(taxfees, by=c("hios_id")) %>%
  left_join(claims, by=c("hios_id")) %>%
  left_join(qi, by=c("hios_id")) %>%
  left_join(cc, by=c("hios_id")) %>%
  left_join(ra, by=c("hios_id")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate(ind_mrg = ind_rev - ind_tax - ind_clm + ind_ra - ind_qi - ind_cc,
         sg_mrg  = sg_rev - sg_tax - sg_clm + sg_ra - sg_qi - sg_cc,
         lg_mrg  = lg_rev - lg_tax - lg_clm + lg_ra - lg_qi - sg_cc,
         ind_mlr = (ind_clm - ind_ra + ind_qi)/(ind_rev - ind_tax),
         sg_mlr  = (sg_clm - sg_ra + sg_qi)/(sg_rev - sg_tax),
         lg_mlr  = (lg_clm - lg_ra + lg_qi)/(lg_rev - lg_tax),
         ind_inv = ind_qi + ind_cc,
         sg_inv  = sg_qi + sg_cc,
         lg_inv  = lg_qi + lg_cc) %>%
  arrange(-ind_mm) %>%
  rename_at(vars(-hios_id),function(x) paste0(x, sep = "_" , year)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 

## Here we bring in 2014 Data

header_in <- read_csv("MR_Submission_Template_Header_2014.csv")
names(header_in) <- tolower(names(header_in))
header <- subset(header_in, select =  c(mr_submission_template_id,
                                        business_state,
                                        group_affiliation,
                                        hios_issuer_id,
                                        company_name,
                                        domiciliary_state,
                                        federal_ein,
                                        dba_marketing_name,
                                        not_for_profit,
                                        merge_markets_ind_small_grp,
                                        fit_exempt)) %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

hios <- subset(header_in, select =  c(mr_submission_template_id,hios_issuer_id)) %>%
  rename(hios_id = hios_issuer_id)

summary_in <- read_csv("Part1_2_Summary_Data_Premium_Claims_2014.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
summary <- summary_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

rebate_in <- read_csv("Part3_MLR_Rebate_Calculation_2014.csv", col_types = cols (.default = "d", ROW_LOOKUP_CODE = "c", CREATED_DATE = "c" ))
rebate <- rebate_in %>%
  rename_with(tolower) %>%
  left_join(hios, by=c("mr_submission_template_id")) %>%
  mutate_all(~replace(., is.na(.), 0))

year = "14"

membership <- summary %>%
  subset(row_lookup_code =="NUMBER_OF_COVERED_LIVES" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mbr = cmm_individual_q1,
         sg_mbr = cmm_small_group_q1,
         lg_mbr = cmm_large_group_q1)


member_months <- summary %>%
  subset(row_lookup_code =="MEMBER_MONTHS" & hios_id != 0 & 
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_mm = cmm_individual_q1,
         sg_mm = cmm_small_group_q1,
         lg_mm = cmm_large_group_q1)

cc <- summary %>%
  subset(row_lookup_code == "COST_CONTAINMENT_EXP_NOT_INCL" & hios_id != 0 &
           (cmm_individual_yearly >= 1 |
              cmm_small_group_yearly >= 1 |
              cmm_large_group_yearly >= 1 )) %>%
  select(hios_id, 
         cmm_individual_q1, 
         cmm_small_group_q1,
         cmm_large_group_q1) %>%
  rename(ind_cc = cmm_individual_q1,
         sg_cc = cmm_small_group_q1,
         lg_cc = cmm_large_group_q1)


premium <- rebate %>%
  subset((row_lookup_code == "PREMIUM_EARNED_INCLUDING_FSHRP") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_rev = cmm_individual_cy,
         sg_rev = cmm_small_group_cy,
         lg_rev = cmm_large_group_cy)


taxfees <- rebate %>%
  subset((row_lookup_code == "FED_STATE_TAXES_LIC_OR_REG_FEE") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_tax = cmm_individual_cy,
         sg_tax = cmm_small_group_cy,
         lg_tax = cmm_large_group_cy)


claims <- rebate %>%
  subset((row_lookup_code == "ADJ_INCURRED_CLAIMS_RESTATED_Q1") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_clm = cmm_individual_cy,
         sg_clm = cmm_small_group_cy,
         lg_clm = cmm_large_group_cy)


qi <- rebate %>%
  subset((row_lookup_code == "QUALITY_IMPROVEMENT_EXPENSES") & hios_id != 0 &
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_qi = cmm_individual_cy,
         sg_qi = cmm_small_group_cy,
         lg_qi = cmm_large_group_cy)

ra <- rebate %>%
  subset((row_lookup_code == "FED_RISK_ADJ_NET_PAYMENTS_HHS") & hios_id != 0 & 
           (cmm_individual_cy != 0 |
              cmm_small_group_cy != 0 |
              cmm_large_group_cy != 0)) %>%
  select(hios_id, 
         cmm_individual_cy, 
         cmm_small_group_cy, 
         cmm_large_group_cy) %>%
  rename(ind_ra = cmm_individual_cy,
         sg_ra = cmm_small_group_cy,
         lg_ra = cmm_large_group_cy)



fullset_2014 <- membership %>%
  left_join(member_months, by=c("hios_id")) %>%
  left_join(premium, by=c("hios_id")) %>%
  left_join(taxfees, by=c("hios_id")) %>%
  left_join(claims, by=c("hios_id")) %>%
  left_join(qi, by=c("hios_id")) %>%
  left_join(cc, by=c("hios_id")) %>%
  left_join(ra, by=c("hios_id")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate(ind_mrg = ind_rev - ind_tax - ind_clm + ind_ra - ind_qi - ind_cc,
         sg_mrg  = sg_rev - sg_tax - sg_clm + sg_ra - sg_qi - sg_cc,
         lg_mrg  = lg_rev - lg_tax - lg_clm + lg_ra - lg_qi - sg_cc,
         ind_mlr = (ind_clm - ind_ra + ind_qi)/(ind_rev - ind_tax),
         sg_mlr  = (sg_clm - sg_ra + sg_qi)/(sg_rev - sg_tax),
         lg_mlr  = (lg_clm - lg_ra + lg_qi)/(lg_rev - lg_tax),
         ind_inv = ind_qi + ind_cc,
         sg_inv  = sg_qi + sg_cc,
         lg_inv  = lg_qi + lg_cc) %>%  
  arrange(-ind_mm) %>%
  rename_at(vars(-hios_id),function(x) paste0(x, sep = "_" , year)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 

#--------------------------------------------------------------------
# Here we start the analysis
#--------------------------------------------------------------------

header_in <- read_csv("MR_Submission_Template_Header_2019.csv")
names(header_in) <- tolower(names(header_in))
headera <- subset(header_in, select =  c(mr_submission_template_id,
                                        business_state,
                                        group_affiliation,
                                        hios_issuer_id,
                                        company_name,
                                        domiciliary_state,
                                        federal_ein,
                                        dba_marketing_name,
                                        not_for_profit,
                                        merge_markets_ind_small_grp,
                                        fit_exempt)) %>%
  filter(business_state != "GU" & business_state != "PR" & business_state != "MP" & 
         business_state != "AS" & business_state != "VI") %>%
  rename(template_id = mr_submission_template_id, hios_id = hios_issuer_id)

plan_names <- read_csv("hpnames.csv")
header <- headera %>%
  left_join(plan_names, by="hios_id") 

# Add later
withdemo <- header %>%
  inner_join(fullset_2014, by="hios_id") %>%
  inner_join(fullset_2015, by="hios_id") %>%
  inner_join(fullset_2016, by="hios_id") %>%
  inner_join(fullset_2017, by="hios_id") %>%
  inner_join(fullset_2018, by="hios_id") %>%
  inner_join(fullset_2019, by="hios_id") %>%
  mutate_all(~replace(., is.na(.), 0))


  
individual <- withdemo%>%
  filter(ind_mbr_15>1000 & ind_mbr_16>1000 & ind_mbr_17>1000 & ind_mbr_18>1000 & ind_mbr_19>1000 & 
         ind_rev_15>1000 & ind_rev_16>1000 & ind_rev_17>1000 & ind_rev_18>1000 & ind_rev_19>1000)%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         ind_cc_14, ind_cc_15, ind_cc_16, ind_cc_17, ind_cc_18, ind_cc_19, 
         ind_clm_14, ind_clm_15, ind_clm_16, ind_clm_17, ind_clm_18, ind_clm_19, 
         ind_inv_14, ind_inv_15, ind_inv_16, ind_inv_17, ind_inv_18, ind_inv_19, 
         ind_mbr_14, ind_mbr_15, ind_mbr_16, ind_mbr_17, ind_mbr_18, ind_mbr_19, 
         ind_mlr_14, ind_mlr_15, ind_mlr_16, ind_mlr_17, ind_mlr_18, ind_mlr_19, 
         ind_mm_14, ind_mm_15, ind_mm_16, ind_mm_17, ind_mm_18, ind_mm_19, 
         ind_mrg_14, ind_mrg_15, ind_mrg_16, ind_mrg_17, ind_mrg_18, ind_mrg_19, 
         ind_qi_14, ind_qi_15, ind_qi_16, ind_qi_17, ind_qi_18, ind_qi_19, 
         ind_ra_14, ind_ra_15, ind_ra_16, ind_ra_17, ind_ra_18, ind_ra_19, 
         ind_rev_14, ind_rev_15, ind_rev_16, ind_rev_17, ind_rev_18, ind_rev_19, 
         ind_tax_14, ind_tax_15, ind_tax_16, ind_tax_17, ind_tax_18, ind_tax_19)%>%
  mutate(mem_growth_15 = ind_mbr_15-ind_mbr_14,
         rev_growth_15 = ind_rev_15-ind_rev_14,
         qi_growth_15  = ind_qi_15-ind_qi_14,
         invest_growth_15 = ind_inv_15-ind_inv_14,
         om_growth_15 = ind_mrg_15-ind_mrg_14,
         mem_growth_16 = ind_mbr_16-ind_mbr_15,
         rev_growth_16 = ind_rev_16-ind_rev_15,
         qi_growth_16  = ind_qi_16-ind_qi_15,
         invest_growth_16 = ind_inv_16-ind_inv_15,
         om_growth_16 = ind_mrg_16-ind_mrg_15,
         mem_growth_17 = ind_mbr_17-ind_mbr_16,
         rev_growth_17 = ind_rev_17-ind_rev_16,
         qi_growth_17  = ind_qi_17-ind_qi_16,
         invest_growth_17 = ind_inv_17-ind_inv_16,
         om_growth_17 = ind_mrg_17-ind_mrg_16,
         mem_growth_18 = ind_mbr_18-ind_mbr_17,
         rev_growth_18 = ind_rev_18-ind_rev_17,
         qi_growth_18  = ind_qi_18-ind_qi_17,
         invest_growth_18 = ind_inv_18-ind_inv_17,
         om_growth_18 = ind_mrg_18-ind_mrg_17,
         mem_growth_19 = ind_mbr_19-ind_mbr_18,
         rev_growth_19 = ind_rev_19-ind_rev_18,
         qi_growth_19  = ind_qi_19-ind_qi_18,
         invest_growth_19 = ind_inv_19-ind_inv_18,
         om_growth_19 = ind_mrg_19-ind_mrg_18,
         total_growth = ind_mbr_19-ind_mbr_15,
         total_inv_growth = ind_inv_19-ind_inv_15,
         total_rev_growth = ind_rev_19-ind_rev_15,
         total_margin_growth = ind_mrg_19-ind_mrg_15,
         ra_growth = ind_ra_19 - ind_ra_15)


ind15 <- individual%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         ind_cc_15, ind_clm_15, ind_inv_15, ind_mbr_15, ind_mm_15, ind_mrg_15, qi_growth_15, om_growth_15,
         ind_qi_15, ind_ra_15, ind_rev_15, ind_tax_15, mem_growth_15, rev_growth_15, invest_growth_15)%>%
  mutate(year = "2015")%>%
  rename(members = ind_mbr_15,
         member_months = ind_mm_15,
         revenue = ind_rev_15,
         claims = ind_clm_15,
         costc = ind_cc_15,
         qi = ind_qi_15,
         invest = ind_inv_15,
         risk_adj = ind_ra_15,
         taxes = ind_tax_15,
         margin = ind_mrg_15,
         mem_growth = mem_growth_15,
         rev_growth = rev_growth_15,
         qi_growth = qi_growth_15,
         invest_growth = invest_growth_15,
         om_growth = om_growth_15)
         
         
         

ind16 <- individual%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name,  
         ind_cc_16, ind_clm_16, ind_inv_16, ind_mbr_16, ind_mm_16, ind_mrg_16, qi_growth_16, om_growth_16,
         ind_qi_16, ind_ra_16, ind_rev_16, ind_tax_16, mem_growth_16, rev_growth_16, invest_growth_16)%>%
  mutate(year = "2016")%>%
  rename(members = ind_mbr_16,
         member_months = ind_mm_16,
         revenue = ind_rev_16,
         claims = ind_clm_16,
         costc = ind_cc_16,
         qi = ind_qi_16,
         invest = ind_inv_16,
         risk_adj = ind_ra_16,
         taxes = ind_tax_16,
         margin = ind_mrg_16,
         mem_growth = mem_growth_16,
         rev_growth = rev_growth_16,
         qi_growth = qi_growth_16,
         invest_growth = invest_growth_16,
         om_growth = om_growth_16)

ind17 <- individual%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name,  
         ind_cc_17, ind_clm_17, ind_inv_17, ind_mbr_17, ind_mm_17, ind_mrg_17, qi_growth_17, om_growth_17,
         ind_qi_17, ind_ra_17, ind_rev_17, ind_tax_17, mem_growth_17, rev_growth_17, invest_growth_17)%>%
  mutate(year = "2017")%>%
  rename(members = ind_mbr_17,
         member_months = ind_mm_17,
         revenue = ind_rev_17,
         claims = ind_clm_17,
         costc = ind_cc_17,
         qi = ind_qi_17,
         invest = ind_inv_17,
         risk_adj = ind_ra_17,
         taxes = ind_tax_17,
         margin = ind_mrg_17,
         mem_growth = mem_growth_17,
         rev_growth = rev_growth_17,
         qi_growth = qi_growth_17,
         invest_growth = invest_growth_17,
         om_growth = om_growth_17)

ind18 <- individual%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name,  
         ind_cc_18, ind_clm_18, ind_inv_18, ind_mbr_18, ind_mm_18, ind_mrg_18, qi_growth_18, om_growth_18,
         ind_qi_18, ind_ra_18, ind_rev_18, ind_tax_18, mem_growth_18, rev_growth_18, invest_growth_18)%>%
  mutate(year = "2018")%>%
  rename(members = ind_mbr_18,
         member_months = ind_mm_18,
         revenue = ind_rev_18,
         claims = ind_clm_18,
         costc = ind_cc_18,
         qi = ind_qi_18,
         invest = ind_inv_18,
         risk_adj = ind_ra_18,
         taxes = ind_tax_18,
         margin = ind_mrg_18,
         mem_growth = mem_growth_18,
         rev_growth = rev_growth_18,
         qi_growth = qi_growth_18,
         invest_growth = invest_growth_18,
         om_growth = om_growth_18)

ind19 <- individual%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         ind_cc_19, ind_clm_19, ind_inv_19, ind_mbr_19, ind_mm_19, ind_mrg_19, qi_growth_19, om_growth_19,
         ind_qi_19, ind_ra_19, ind_rev_19, ind_tax_19, mem_growth_19, rev_growth_19, invest_growth_19)%>%
  mutate(year = "2019")%>%
  rename(members = ind_mbr_19,
         member_months = ind_mm_19,
         revenue = ind_rev_19,
         claims = ind_clm_19,
         costc = ind_cc_19,
         qi = ind_qi_19,
         invest = ind_inv_19,
         risk_adj = ind_ra_19,
         taxes = ind_tax_19,
         margin = ind_mrg_19,
         mem_growth = mem_growth_19,
         rev_growth = rev_growth_19,
         qi_growth = qi_growth_19,
         invest_growth = invest_growth_19,
         om_growth = om_growth_19)

ind <- rbind(ind15, ind16, ind17, ind18, ind19)

ind_metrics <- ind%>%
  mutate(claims_pmpm = claims/member_months,
         revenue_pmpm = revenue/member_months,
         qi_pmpm = qi/member_months,
         invest_pmpm = invest/member_months,
         ra_pmpm = risk_adj/member_months,
         taxes_pmpm = taxes/member_months,
         om_pmpm = margin/member_months,
         mem_change = round((mem_growth/members*100), digits=1),
         rev_change = round((rev_growth/revenue*100), digits=1),
         qi_change = round((qi_growth/qi*100), digits=1),
         invest_change = round((invest_growth/invest*100), digits=1),
         om_change = round((om_growth/margin*100), digits=1))%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))


#---------------------------------------------------
# Test Investment against Member Growth 
#---------------------------------------------------

Q_m <- quantile(ind_metrics$mem_change, probs=c(.25, .75), na.rm = FALSE)
iqr_m <- IQR(ind_metrics$mem_change)
mm_up <-  Q_m[2]+1.5*iqr_m # Upper Range  
mm_low<- Q_m[1]-1.5*iqr_m # Lower Range

Q_x   <- quantile(ind_metrics$invest_change, probs=c(.25, .75), na.rm = FALSE)
iqr_x <- IQR(ind_metrics$invest_change)
x_up  <-Q_x[2]+1.5*iqr_x # Upper Range  
x_low <- Q_x[1]-1.5*iqr_x # Lower Range

pairs_no <- filter(ind_metrics, mem_change > mm_low & mem_change < mm_up & 
                     invest_change > x_low & invest_change < x_up)%>%
  filter(invest_change !=0)

cor.test(ind_metrics$mem_change, ind_metrics$invest_change)
cor.test(ind_metrics$mem_change, ind_metrics$invest_change, method = "pearson",)

ggplot(data = pairs_no, aes(x=invest_change, y=mem_change))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  theme_classic() +
  labs(title = "Individual LOB Raw Plot",
       #title = "QI & CC Investment Growth Vs. Membership Growth",
       #subtitle = "Individual LOB Raw Plot\n",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n")  +
  annotate("text", x=25, y=-90, label = "Correlation = .80", size=5)


bins <- pairs_no%>%
  mutate(bins= ntile(invest_change, 50))%>%
  group_by(bins)%>%
  summarize(members = sum(members),
            mem_growth = sum(mem_growth),
            invest = sum(invest),
            invest_growth = sum(invest_growth))%>%
  mutate(invest_change = round((invest_growth/invest*100), digits=1),
         mem_change = round((mem_growth/members*100), digits=1))%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

cor.test(bins$mem_change, bins$invest_change)

ggplot(data = bins, aes(x=invest_change, y=mem_change))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-50, 50), breaks = c(-50, -25, 0, 25, 50)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  theme_classic() +
  labs(title ="Individual LOB Binned(50)",
       #title = "QI & CC Investment Growth Vs. Membership Growth",
       #subtitle = "Individual LOB Binned(50)\n ",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n") + 
  annotate("text", x=25, y=-45, label = "Corr = .89, p = .00", size=5)


indass <- pairs_no %>%
  mutate(qibin = ntile(invest_change, 10)) 

select_bins1 <- indass %>%
  filter(qibin == 1 | qibin == 5) %>%
  group_by(qibin) %>% 
  sample_n(75) 

t.test(mem_change ~ qibin, data = select_bins1, paired = TRUE)

select_bins2 <- indass %>%
  filter(qibin == 5 | qibin == 10) %>%
  group_by(qibin) %>% 
  sample_n(75)

t.test(mem_change ~ qibin, data = select_bins2, paired = TRUE)


indass$qibin <- as.factor(indass$qibin)

bp <- ggplot(indass, aes(x=qibin, y=mem_change)) +
  geom_boxplot(outlier.shape = NA, coef = 0, color="blue", fill="light blue", alpha=0.8) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  theme_classic() +
  labs(title = "Individual Growth",
       #title = "QI & CC Investment Growth Vs Membership Growth",
       #subtitle = "Individual Growth\n",
       x = "\n Investment Growth Decile\n",
       caption = "5 vs 1 : t(74) = 3.3, p = .00, Avg Mbr Change Mean Diff = 13.9 \n
                 10 vs 5: t(74) = 14.1, p = .00, Avg Mbr Change Mean Diff = 54.8", size=4)
bp + geom_smooth(se=FALSE, aes(group=1), color="red", size=1)

meanbin <- indass %>%
  group_by(qibin) %>%
  summarize(mean_growth = round(mean(mem_change), digits=1), 
            mean_invest = round(mean(invest_change), digits=1))


meanbin$qibin <- as.factor(meanbin$qibin)


ggplot(meanbin, aes(x=qibin, y=mean_invest)) +
  geom_bar(stat='identity', color="white", fill="light blue") +
  coord_flip() +
  scale_y_continuous(name="\n Investment Growth", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  geom_text(aes(label=mean_invest), hjust=1.4, color="Black", size=3.5)+
  theme_classic() +
  labs(title = "QI & Cost Containment Investment Growth",
       subtitle = "Individual Investment Growth\n",
       x = "Investment Growth Decile\n") 

ggplot(meanbin, aes(x=qibin, y=mean_growth)) +
  geom_bar(stat='identity', color="white", fill="light blue") +
  coord_flip() +
  scale_y_continuous(name="\n Investment Growth", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  geom_text(aes(label=mean_growth), hjust=1.4, color="Black", size=3.5)+
  theme_classic() +
  labs(title = "Member Growth By Investment Decile",
       subtitle = "Individual Member Growth\n",
       x = "Investment Growth Decile\n") 

# ID Top 10 Individual Growth Plans
top10_ind <- sqldf('select hios_id, business_state, plan_name, ind_mbr_15, ind_mbr_19, total_growth, 
                    ind_inv_15, ind_inv_19, total_inv_growth, ind_rev_15, ind_rev_19, total_rev_growth, 
                    ind_mlr_15, ind_mlr_19
                    from individual
                    where hios_id != 59763
                    order by total_growth desc limit 10')

write.csv(top10_ind,"top10_ind.csv")

#--------------------------------------------------------------------
# Here we evaluate Small Group
#--------------------------------------------------------------------

limit = 1000

smallgroup <- withdemo%>%
  filter(sg_mbr_14>limit & sg_mbr_15>limit & sg_mbr_16>limit & sg_mbr_17>limit & sg_mbr_18>limit & sg_mbr_19>limit)%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         sg_cc_14, sg_cc_15, sg_cc_16, sg_cc_17, sg_cc_18, sg_cc_19, 
         sg_clm_14, sg_clm_15, sg_clm_16, sg_clm_17, sg_clm_18, sg_clm_19, 
         sg_inv_14, sg_inv_15, sg_inv_16, sg_inv_17, sg_inv_18, sg_inv_19, 
         sg_mbr_14, sg_mbr_15, sg_mbr_16, sg_mbr_17, sg_mbr_18, sg_mbr_19, 
         sg_mlr_14, sg_mlr_15, sg_mlr_16, sg_mlr_17, sg_mlr_18, sg_mlr_19, 
         sg_mm_14, sg_mm_15, sg_mm_16, sg_mm_17, sg_mm_18, sg_mm_19, 
         sg_mrg_14, sg_mrg_15, sg_mrg_16, sg_mrg_17, sg_mrg_18, sg_mrg_19, 
         sg_qi_14, sg_qi_15, sg_qi_16, sg_qi_17, sg_qi_18, sg_qi_19, 
         sg_ra_14, sg_ra_15, sg_ra_16, sg_ra_17, sg_ra_18, sg_ra_19, 
         sg_rev_14, sg_rev_15, sg_rev_16, sg_rev_17, sg_rev_18, sg_rev_19, 
         sg_tax_14, sg_tax_15, sg_tax_16, sg_tax_17, sg_tax_18, sg_tax_19)%>%
  mutate(mem_growth_15 = sg_mbr_15-sg_mbr_14,
         rev_growth_15 = sg_rev_15-sg_rev_14,
         qi_growth_15  = sg_qi_15-sg_qi_14,
         invest_growth_15 = sg_inv_15-sg_inv_14,
         om_growth_15 = sg_mrg_15-sg_mrg_14,
         mem_growth_16 = sg_mbr_16-sg_mbr_15,
         rev_growth_16 = sg_rev_16-sg_rev_15,
         qi_growth_16  = sg_qi_16-sg_qi_15,
         invest_growth_16 = sg_inv_16-sg_inv_15,
         om_growth_16 = sg_mrg_16-sg_mrg_15,
         mem_growth_17 = sg_mbr_17-sg_mbr_16,
         rev_growth_17 = sg_rev_17-sg_rev_16,
         qi_growth_17  = sg_qi_17-sg_qi_16,
         invest_growth_17 = sg_inv_17-sg_inv_16,
         om_growth_17 = sg_mrg_17-sg_mrg_16,
         mem_growth_18 = sg_mbr_18-sg_mbr_17,
         rev_growth_18 = sg_rev_18-sg_rev_17,
         qi_growth_18  = sg_qi_18-sg_qi_17,
         invest_growth_18 = sg_inv_18-sg_inv_17,
         om_growth_18 = sg_mrg_18-sg_mrg_17,
         mem_growth_19 = sg_mbr_19-sg_mbr_18,
         rev_growth_19 = sg_rev_19-sg_rev_18,
         qi_growth_19  = sg_qi_19-sg_qi_18,
         invest_growth_19 = sg_inv_19-sg_inv_18,
         om_growth_19 = sg_mrg_19-sg_mrg_18,
         total_growth = sg_mbr_19-sg_mbr_15,
         total_inv_growth = sg_inv_19-sg_inv_15,
         total_rev_growth = sg_rev_19-sg_rev_15,
         total_margin_growth = sg_mrg_19-sg_mrg_15,
         ra_growth = sg_ra_19 - sg_ra_15)


sg_15 <- smallgroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         sg_cc_15, sg_clm_15, sg_inv_15, sg_mbr_15, sg_mm_15, sg_mrg_15, qi_growth_15, om_growth_15,
         sg_qi_15, sg_ra_15, sg_rev_15, sg_tax_15, mem_growth_15, rev_growth_15, invest_growth_15)%>%
  mutate(year = "2015")%>%
  rename(members = sg_mbr_15,
         member_months = sg_mm_15,
         revenue = sg_rev_15,
         claims = sg_clm_15,
         costc = sg_cc_15,
         qi = sg_qi_15,
         invest = sg_inv_15,
         risk_adj = sg_ra_15,
         taxes = sg_tax_15,
         margin = sg_mrg_15,
         mem_growth = mem_growth_15,
         rev_growth = rev_growth_15,
         qi_growth = qi_growth_15,
         invest_growth = invest_growth_15,
         om_growth = om_growth_15)

sg_16 <- smallgroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         sg_cc_16, sg_clm_16, sg_inv_16, sg_mbr_16, sg_mm_16, sg_mrg_16, qi_growth_16, om_growth_16,
         sg_qi_16, sg_ra_16, sg_rev_16, sg_tax_16, mem_growth_16, rev_growth_16, invest_growth_16)%>%
  mutate(year = "2016")%>%
  rename(members = sg_mbr_16,
         member_months = sg_mm_16,
         revenue = sg_rev_16,
         claims = sg_clm_16,
         costc = sg_cc_16,
         qi = sg_qi_16,
         invest = sg_inv_16,
         risk_adj = sg_ra_16,
         taxes = sg_tax_16,
         margin = sg_mrg_16,
         mem_growth = mem_growth_16,
         rev_growth = rev_growth_16,
         qi_growth = qi_growth_16,
         invest_growth = invest_growth_16,
         om_growth = om_growth_16)

sg_17 <- smallgroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         sg_cc_17, sg_clm_17, sg_inv_17, sg_mbr_17, sg_mm_17, sg_mrg_17, qi_growth_17, om_growth_17,
         sg_qi_17, sg_ra_17, sg_rev_17, sg_tax_17, mem_growth_17, rev_growth_17, invest_growth_17)%>%
  mutate(year = "2017")%>%
  rename(members = sg_mbr_17,
         member_months = sg_mm_17,
         revenue = sg_rev_17,
         claims = sg_clm_17,
         costc = sg_cc_17,
         qi = sg_qi_17,
         invest = sg_inv_17,
         risk_adj = sg_ra_17,
         taxes = sg_tax_17,
         margin = sg_mrg_17,
         mem_growth = mem_growth_17,
         rev_growth = rev_growth_17,
         qi_growth = qi_growth_17,
         invest_growth = invest_growth_17,
         om_growth = om_growth_17)

sg_18 <- smallgroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         sg_cc_18, sg_clm_18, sg_inv_18, sg_mbr_18, sg_mm_18, sg_mrg_18, qi_growth_18, om_growth_18,
         sg_qi_18, sg_ra_18, sg_rev_18, sg_tax_18, mem_growth_18, rev_growth_18, invest_growth_18)%>%
  mutate(year = "2018")%>%
  rename(members = sg_mbr_18,
         member_months = sg_mm_18,
         revenue = sg_rev_18,
         claims = sg_clm_18,
         costc = sg_cc_18,
         qi = sg_qi_18,
         invest = sg_inv_18,
         risk_adj = sg_ra_18,
         taxes = sg_tax_18,
         margin = sg_mrg_18,
         mem_growth = mem_growth_18,
         rev_growth = rev_growth_18,
         qi_growth = qi_growth_18,
         invest_growth = invest_growth_18,
         om_growth = om_growth_18)

sg_19 <- smallgroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         sg_cc_19, sg_clm_19, sg_inv_19, sg_mbr_19, sg_mm_19, sg_mrg_19, qi_growth_19, om_growth_19,
         sg_qi_19, sg_ra_19, sg_rev_19, sg_tax_19, mem_growth_19, rev_growth_19, invest_growth_19)%>%
  mutate(year = "2019")%>%
  rename(members = sg_mbr_19,
         member_months = sg_mm_19,
         revenue = sg_rev_19,
         claims = sg_clm_19,
         costc = sg_cc_19,
         qi = sg_qi_19,
         invest = sg_inv_19,
         risk_adj = sg_ra_19,
         taxes = sg_tax_19,
         margin = sg_mrg_19,
         mem_growth = mem_growth_19,
         rev_growth = rev_growth_19,
         qi_growth = qi_growth_19,
         invest_growth = invest_growth_19,
         om_growth = om_growth_19)

small_group <- rbind(sg_15, sg_16, sg_17, sg_18, sg_19)

sg_metrics <- small_group%>%
  mutate(claims_pmpm = claims/member_months,
         revenue_pmpm = revenue/member_months,
         qi_pmpm = qi/member_months,
         invest_pmpm = invest/member_months,
         ra_pmpm = risk_adj/member_months,
         taxes_pmpm = taxes/member_months,
         om_pmpm = margin/member_months,
         mem_change = round((mem_growth/members*100), digits=1),
         rev_change = round((rev_growth/revenue*100), digits=1),
         qi_change = round((qi_growth/qi*100), digits=1),
         invest_change = round((invest_growth/invest*100), digits=1),
         om_change = round((om_growth/margin*100), digits=1))%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

#---------------------------------------------------
# Test Investment against Member Growth 
#---------------------------------------------------

Q_m <- quantile(sg_metrics$mem_change, probs=c(.25, .75), na.rm = FALSE)
iqr_m <- IQR(sg_metrics$mem_change)
mm_up <-  Q_m[2]+1.5*iqr_m # Upper Range  
mm_low<- Q_m[1]-1.5*iqr_m # Lower Range

Q_x   <- quantile(sg_metrics$invest_change, probs=c(.25, .75), na.rm = FALSE)
iqr_x <- IQR(sg_metrics$invest_change)
x_up  <-Q_x[2]+1.5*iqr_x # Upper Range  
x_low <- Q_x[1]-1.5*iqr_x # Lower Range

pairs_no <- filter(sg_metrics, mem_change > mm_low & mem_change < mm_up & 
                     invest_change > x_low & invest_change < x_up)%>%
  filter(invest_change !=0)

cor.test(sg_metrics$mem_change, sg_metrics$invest_change)

ggplot(data = pairs_no, aes(x=invest_change, y=mem_change))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  theme_classic() +
  labs(title = "Small Group LOB Raw Plot",
       #title = "QI & CC Investment Growth Vs. Membership Growth",
       #subtitle = "Small Group LOB Raw Plot\n",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n")  +
  annotate("text", x=25, y=-90, label = "Correlation = .42", size=5)



bins <- pairs_no%>%
  mutate(bins= ntile(invest_change, 50))%>%
  group_by(bins)%>%
  summarize(members = sum(members),
            mem_growth = sum(mem_growth),
            invest = sum(invest),
            invest_growth = sum(invest_growth))%>%
  mutate(invest_change = round((invest_growth/invest*100), digits=1),
         mem_change = round((mem_growth/members*100), digits=1))%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

cor.test(bins$mem_change, bins$invest_change)

ggplot(data = bins, aes(x=invest_change, y=mem_change))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-50, 50), breaks = c(-50, -25, 0, 25, 50)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  theme_classic() +
  labs(title = "Small Group LOB Binned(50)",
       #subtitle = "Small Group LOB Binned(50)\n ",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n") + 
  annotate("text", x=25, y=-45, label = "Corr = .89, p = .00", size=5)



indass <- pairs_no %>%
  mutate(qibin = ntile(invest_change, 10)) 

select_bins1 <- indass %>%
  filter(qibin == 1 | qibin == 5) %>%
  group_by(qibin) %>% 
  sample_n(75) 

t.test(mem_change ~ qibin, data = select_bins1, paired = TRUE)

select_bins2 <- indass %>%
  filter(qibin == 5 | qibin == 10) %>%
  group_by(qibin) %>% 
  sample_n(75)

t.test(mem_change ~ qibin, data = select_bins2, paired = TRUE)


indass$qibin <- as.factor(indass$qibin)

bp <- ggplot(indass, aes(x=qibin, y=mem_change)) +
  geom_boxplot(outlier.shape = NA, coef = 0, color="blue", fill="light blue", alpha=0.8) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  theme_classic() +
  labs(title = "Small Group Growth",
       #title = "QI & CC Investment Growth Vs Membership Growth",
       #subtitle = "Small Group Growth\n",
       x = "Investment Growth Decile\n ",
       caption = "5 vs 1 : t(74) = 5.4, p = .00, Avg Mbr Change Mean Diff = 16.2 \n
                 10 vs 5: t(74) = 5.9, p = .00, Avg Mbr Change Mean Diff = 16.4", size=4)
bp + geom_smooth(se=FALSE, aes(group=1), color="red", size=1)


meanbin <- indass %>%
  group_by(qibin) %>%
  summarize(mean_growth = round(mean(mem_change), digits=1), 
            mean_invest = round(mean(invest_change), digits=1))


meanbin$qibin <- as.factor(meanbin$qibin)

ggplot(meanbin, aes(x=qibin, y=mean_invest)) +
  geom_bar(stat='identity', color="white", fill="light blue") +
  coord_flip() +
  scale_y_continuous(name="Investment Growth\n", limits=c(-60, 60), breaks = c(-60, -30, 0, 30, 60)) +
  geom_text(aes(label=mean_invest), hjust=1.4, color="Black", size=3.5)+
  theme_classic() +
  labs(title = "QI & CC Investment Growth by Decile",
       subtitle = "Small Group Investment Growth\n",
       x = "Investment Growth Decile\n ") 

ggplot(meanbin, aes(x=qibin, y=mean_growth)) +
  geom_bar(stat='identity', color="white", fill="light blue") +
  coord_flip() +
  scale_y_continuous(name="Member Growth\n", limits=c(-60, 60), breaks = c(-60, -30, 0, 30, 60)) +
  geom_text(aes(label=mean_growth), hjust=1.4, color="Black", size=3.5)+
  theme_classic() +
  labs(title = "Member Growth By Investment Decile",
       subtitle = "Small Group Member Growth\n",
       x = "Investment Growth Decile\n ") 

# ID Top 10 Individual Growth Plans
top10_sg <- sqldf('select hios_id, business_state, plan_name, sg_mbr_15, sg_mbr_19, total_growth, 
                    sg_inv_15, sg_inv_19, total_inv_growth, sg_rev_15, sg_rev_19, total_rev_growth, 
                    sg_mlr_15, sg_mlr_19
                    from smallgroup
                    order by total_growth desc limit 10')

write.csv(top10_sg,"top10_sg.csv")


#--------------------------------------------------------------------
# Here we evaluate Small Group
#--------------------------------------------------------------------

limit = 1000

largegroup <- withdemo%>%
  filter(lg_mbr_14>limit & lg_mbr_15>limit & lg_mbr_16>limit & lg_mbr_17>limit & lg_mbr_18>limit & lg_mbr_19>limit)%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         lg_cc_14, lg_cc_15, lg_cc_16, lg_cc_17, lg_cc_18, lg_cc_19, 
         lg_clm_14, lg_clm_15, lg_clm_16, lg_clm_17, lg_clm_18, lg_clm_19, 
         lg_inv_14, lg_inv_15, lg_inv_16, lg_inv_17, lg_inv_18, lg_inv_19, 
         lg_mbr_14, lg_mbr_15, lg_mbr_16, lg_mbr_17, lg_mbr_18, lg_mbr_19, 
         lg_mlr_14, lg_mlr_15, lg_mlr_16, lg_mlr_17, lg_mlr_18, lg_mlr_19, 
         lg_mm_14, lg_mm_15, lg_mm_16, lg_mm_17, lg_mm_18, lg_mm_19, 
         lg_mrg_14, lg_mrg_15, lg_mrg_16, lg_mrg_17, lg_mrg_18, lg_mrg_19, 
         lg_qi_14, lg_qi_15, lg_qi_16, lg_qi_17, lg_qi_18, lg_qi_19, 
         lg_ra_14, lg_ra_15, lg_ra_16, lg_ra_17, lg_ra_18, lg_ra_19, 
         lg_rev_14, lg_rev_15, lg_rev_16, lg_rev_17, lg_rev_18, lg_rev_19, 
         lg_tax_14, lg_tax_15, lg_tax_16, lg_tax_17, lg_tax_18, lg_tax_19)%>%
  mutate(mem_growth_15 = lg_mbr_15-lg_mbr_14,
         rev_growth_15 = lg_rev_15-lg_rev_14,
         qi_growth_15  = lg_qi_15-lg_qi_14,
         invest_growth_15 = lg_inv_15-lg_inv_14,
         om_growth_15 = lg_mrg_15-lg_mrg_14,
         mem_growth_16 = lg_mbr_16-lg_mbr_15,
         rev_growth_16 = lg_rev_16-lg_rev_15,
         qi_growth_16  = lg_qi_16-lg_qi_15,
         invest_growth_16 = lg_inv_16-lg_inv_15,
         om_growth_16 = lg_mrg_16-lg_mrg_15,
         mem_growth_17 = lg_mbr_17-lg_mbr_16,
         rev_growth_17 = lg_rev_17-lg_rev_16,
         qi_growth_17  = lg_qi_17-lg_qi_16,
         invest_growth_17 = lg_inv_17-lg_inv_16,
         om_growth_17 = lg_mrg_17-lg_mrg_16,
         mem_growth_18 = lg_mbr_18-lg_mbr_17,
         rev_growth_18 = lg_rev_18-lg_rev_17,
         qi_growth_18  = lg_qi_18-lg_qi_17,
         invest_growth_18 = lg_inv_18-lg_inv_17,
         om_growth_18 = lg_mrg_18-lg_mrg_17,
         mem_growth_19 = lg_mbr_19-lg_mbr_18,
         rev_growth_19 = lg_rev_19-lg_rev_18,
         qi_growth_19  = lg_qi_19-lg_qi_18,
         invest_growth_19 = lg_inv_19-lg_inv_18,
         om_growth_19 = lg_mrg_19-lg_mrg_18,
         total_growth = lg_mbr_19-lg_mbr_15,
         total_inv_growth = lg_inv_19-lg_inv_15,
         total_rev_growth = lg_rev_19-lg_rev_15,
         total_margin_growth = lg_mrg_19-lg_mrg_15,
         ra_growth = lg_ra_19 - lg_ra_15)


lg_15 <- largegroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         lg_cc_15, lg_clm_15, lg_inv_15, lg_mbr_15, lg_mm_15, lg_mrg_15, qi_growth_15, om_growth_15,
         lg_qi_15, lg_ra_15, lg_rev_15, lg_tax_15, mem_growth_15, rev_growth_15, invest_growth_15)%>%
  mutate(year = "2015")%>%
  rename(members = lg_mbr_15,
         member_months = lg_mm_15,
         revenue = lg_rev_15,
         claims = lg_clm_15,
         costc = lg_cc_15,
         qi = lg_qi_15,
         invest = lg_inv_15,
         risk_adj = lg_ra_15,
         taxes = lg_tax_15,
         margin = lg_mrg_15,
         mem_growth = mem_growth_15,
         rev_growth = rev_growth_15,
         qi_growth = qi_growth_15,
         invest_growth = invest_growth_15,
         om_growth = om_growth_15)

lg_16 <- largegroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         lg_cc_16, lg_clm_16, lg_inv_16, lg_mbr_16, lg_mm_16, lg_mrg_16, qi_growth_16, om_growth_16,
         lg_qi_16, lg_ra_16, lg_rev_16, lg_tax_16, mem_growth_16, rev_growth_16, invest_growth_16)%>%
  mutate(year = "2016")%>%
  rename(members = lg_mbr_16,
         member_months = lg_mm_16,
         revenue = lg_rev_16,
         claims = lg_clm_16,
         costc = lg_cc_16,
         qi = lg_qi_16,
         invest = lg_inv_16,
         risk_adj = lg_ra_16,
         taxes = lg_tax_16,
         margin = lg_mrg_16,
         mem_growth = mem_growth_16,
         rev_growth = rev_growth_16,
         qi_growth = qi_growth_16,
         invest_growth = invest_growth_16,
         om_growth = om_growth_16)

lg_17 <- largegroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         lg_cc_17, lg_clm_17, lg_inv_17, lg_mbr_17, lg_mm_17, lg_mrg_17, qi_growth_17, om_growth_17,
         lg_qi_17, lg_ra_17, lg_rev_17, lg_tax_17, mem_growth_17, rev_growth_17, invest_growth_17)%>%
  mutate(year = "2017")%>%
  rename(members = lg_mbr_17,
         member_months = lg_mm_17,
         revenue = lg_rev_17,
         claims = lg_clm_17,
         costc = lg_cc_17,
         qi = lg_qi_17,
         invest = lg_inv_17,
         risk_adj = lg_ra_17,
         taxes = lg_tax_17,
         margin = lg_mrg_17,
         mem_growth = mem_growth_17,
         rev_growth = rev_growth_17,
         qi_growth = qi_growth_17,
         invest_growth = invest_growth_17,
         om_growth = om_growth_17)

lg_18 <- largegroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         lg_cc_18, lg_clm_18, lg_inv_18, lg_mbr_18, lg_mm_18, lg_mrg_18, qi_growth_18, om_growth_18,
         lg_qi_18, lg_ra_18, lg_rev_18, lg_tax_18, mem_growth_18, rev_growth_18, invest_growth_18)%>%
  mutate(year = "2018")%>%
  rename(members = lg_mbr_18,
         member_months = lg_mm_18,
         revenue = lg_rev_18,
         claims = lg_clm_18,
         costc = lg_cc_18,
         qi = lg_qi_18,
         invest = lg_inv_18,
         risk_adj = lg_ra_18,
         taxes = lg_tax_18,
         margin = lg_mrg_18,
         mem_growth = mem_growth_18,
         rev_growth = rev_growth_18,
         qi_growth = qi_growth_18,
         invest_growth = invest_growth_18,
         om_growth = om_growth_18)

lg_19 <- largegroup%>%
  select(business_state, group_affiliation, hios_id, company_name, plan_name, 
         lg_cc_19, lg_clm_19, lg_inv_19, lg_mbr_19, lg_mm_19, lg_mrg_19, qi_growth_19, om_growth_19,
         lg_qi_19, lg_ra_19, lg_rev_19, lg_tax_19, mem_growth_19, rev_growth_19, invest_growth_19)%>%
  mutate(year = "2019")%>%
  rename(members = lg_mbr_19,
         member_months = lg_mm_19,
         revenue = lg_rev_19,
         claims = lg_clm_19,
         costc = lg_cc_19,
         qi = lg_qi_19,
         invest = lg_inv_19,
         risk_adj = lg_ra_19,
         taxes = lg_tax_19,
         margin = lg_mrg_19,
         mem_growth = mem_growth_19,
         rev_growth = rev_growth_19,
         qi_growth = qi_growth_19,
         invest_growth = invest_growth_19,
         om_growth = om_growth_19)

large_group <- rbind(lg_15, lg_16, lg_17, lg_18, lg_19)

lg_metrics <- large_group%>%
  mutate(claims_pmpm = claims/member_months,
         revenue_pmpm = revenue/member_months,
         qi_pmpm = qi/member_months,
         invest_pmpm = invest/member_months,
         ra_pmpm = risk_adj/member_months,
         taxes_pmpm = taxes/member_months,
         om_pmpm = margin/member_months,
         mem_change = round((mem_growth/members*100), digits=1),
         rev_change = round((rev_growth/revenue*100), digits=1),
         qi_change = round((qi_growth/qi*100), digits=1),
         invest_change = round((invest_growth/invest*100), digits=1),
         om_change = round((om_growth/margin*100), digits=1))%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

#---------------------------------------------------
# Test Investment against Member Growth 
#---------------------------------------------------

Q_m <- quantile(lg_metrics$mem_change, probs=c(.25, .75), na.rm = FALSE)
iqr_m <- IQR(lg_metrics$mem_change)
mm_up <-  Q_m[2]+1.5*iqr_m # Upper Range  
mm_low<- Q_m[1]-1.5*iqr_m # Lower Range

Q_x   <- quantile(lg_metrics$invest_change, probs=c(.25, .75), na.rm = FALSE)
iqr_x <- IQR(lg_metrics$invest_change)
x_up  <-Q_x[2]+1.5*iqr_x # Upper Range  
x_low <- Q_x[1]-1.5*iqr_x # Lower Range

pairs_no <- filter(lg_metrics, mem_change > mm_low & mem_change < mm_up & 
                     invest_change > x_low & invest_change < x_up)%>%
  filter(invest_change !=0)

cor.test(lg_metrics$mem_change, lg_metrics$invest_change)

ggplot(data = pairs_no, aes(x=invest_change, y=mem_change))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  theme_classic() +
  labs(title = "Large Group LOB Raw Plot",
       #title = "QI & CC Investment Growth Vs. Membership Growth",
       #subtitle = "Large Group LOB Raw Plot\n",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n")  +
  annotate("text", x=25, y=-90, label = "Correlation = .20", size=5)

bins <- pairs_no%>%
  mutate(bins= ntile(invest_change, 50))%>%
  group_by(bins)%>%
  summarize(members = sum(members),
            mem_growth = sum(mem_growth),
            invest = sum(invest),
            invest_growth = sum(invest_growth))%>%
  mutate(invest_change = round((invest_growth/invest*100), digits=1),
         mem_change = round((mem_growth/members*100), digits=1))%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

cor.test(bins$mem_change, bins$invest_change)

ggplot(data = bins, aes(x=invest_change, y=mem_change))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-50, 50), breaks = c(-50, -25, 0, 25, 50)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  theme_classic() +
  labs(title = "Large Group LOB Binned(50)",
       #title = "QI & CC Investment Growth Vs. Membership Growth",
       #subtitle = "Large Group LOB Binned(50)\n ",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n") + 
  annotate("text", x=25, y=-45, label = "Corr = .79, p = .00", size=5)


indass <- pairs_no %>%
  mutate(qibin = ntile(invest_change, 10)) 

select_bins1 <- indass %>%
  filter(qibin == 1 | qibin == 5) %>%
  group_by(qibin) %>% 
  sample_n(75) 

t.test(mem_change ~ qibin, data = select_bins1, paired = TRUE)

select_bins2 <- indass %>%
  filter(qibin == 5 | qibin == 10) %>%
  group_by(qibin) %>% 
  sample_n(75)

t.test(mem_change ~ qibin, data = select_bins2, paired = TRUE)


indass$qibin <- as.factor(indass$qibin)

bp <- ggplot(indass, aes(x=qibin, y=mem_change)) +
  geom_boxplot(outlier.shape = NA, coef = 0, color="blue", fill="light blue", alpha=0.8) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  theme_classic() +
  labs(title = "Large Group Growth",
       #title = "QI & CC Investment Growth Vs Membership Growth",
       #subtitle = "Large Group Investment Growth\n",
       x = "Investment Growth Decile\n",
       caption = "5 vs 1 : t(74) = 4.6, p = .00, Avg Mbr Change Mean Diff = 9.2 \n
                 10 vs 5: t(74) = 3.6, p = .00, Avg Mbr Change Mean Diff = 9.2", size=4)
bp + geom_smooth(se=FALSE, aes(group=1), color="red", size=1)

meanbin <- indass %>%
  group_by(qibin) %>%
  summarize(mean_growth = round(mean(mem_change), digits=1), 
            mean_invest = round(mean(invest_change), digits=1))


meanbin$qibin <- as.factor(meanbin$qibin)

ggplot(meanbin, aes(x=qibin, y=mean_invest)) +
  geom_bar(stat='identity', color="white", fill="light blue") +
  coord_flip() +
  scale_y_continuous(name="\n Investment Growth", limits=c(-50, 50), breaks = c(-50, -25, 0, 25, 50)) +
  geom_text(aes(label=mean_invest), hjust=1.4, color="Black", size=3.5)+
  theme_classic() +
  labs(title = "QI & Cost Containment Investment Growth",
       subtitle = "Large Group Investment Growth\n",
       x = "Investment Growth Decile\n") 

ggplot(meanbin, aes(x=qibin, y=mean_growth)) +
  geom_bar(stat='identity', color="white", fill="light blue") +
  coord_flip() +
  scale_y_continuous(name="\n Member Growth", limits=c(-50, 50), breaks = c(-50, -25, 0, 25, 50)) +
  geom_text(aes(label=mean_growth), hjust=1.4, color="Black", size=3.5)+
  theme_classic() +
  labs(title = "Member Growth By Investment Decile",
       subtitle = "Large Group Member Growth\n",
       x = "Investment Growth Decile\n") 

# ID Top 10 Individual Growth Plans
top10_lg <- sqldf('select hios_id, business_state, plan_name, lg_mbr_15, lg_mbr_19, total_growth, 
                    lg_inv_15, lg_inv_19, total_inv_growth, lg_rev_15, lg_rev_19, total_rev_growth, 
                    lg_mlr_15, lg_mlr_19
                    from largegroup
                    order by total_growth desc limit 10')

write.csv(top10_lg,"top10_lg.csv")



member_profile <- withdemo%>%
  select(hios_id, plan_name, plan_group, business_state, ind_mbr_14, ind_mbr_15, ind_mbr_16, ind_mbr_17, ind_mbr_18, ind_mbr_19,
         sg_mbr_14, sg_mbr_15, sg_mbr_16, sg_mbr_17, sg_mbr_18, sg_mbr_19,
         lg_mbr_14, lg_mbr_15, lg_mbr_16, lg_mbr_17, lg_mbr_18, lg_mbr_19)%>%
  group_by(plan_group)%>%
  summarize(ind_mbr_14=sum(ind_mbr_14), 
            ind_mbr_15=sum(ind_mbr_15), 
            ind_mbr_16=sum(ind_mbr_16), 
            ind_mbr_17=sum(ind_mbr_17), 
            ind_mbr_18=sum(ind_mbr_18), 
            ind_mbr_19=sum(ind_mbr_19),
            sg_mbr_14=sum(sg_mbr_14), 
            sg_mbr_15=sum(sg_mbr_15), 
            sg_mbr_16=sum(sg_mbr_16), 
            sg_mbr_17=sum(sg_mbr_17), 
            sg_mbr_18=sum(sg_mbr_18), 
            sg_mbr_19=sum(sg_mbr_19),
            lg_mbr_14=sum(lg_mbr_14), 
            lg_mbr_15=sum(lg_mbr_15), 
            lg_mbr_16=sum(lg_mbr_16), 
            lg_mbr_17=sum(lg_mbr_17), 
            lg_mbr_18=sum(lg_mbr_18), 
            lg_mbr_19=sum(lg_mbr_19))%>%
  mutate(total_14 = ind_mbr_14+sg_mbr_14+lg_mbr_14,
         total_15 = ind_mbr_15+sg_mbr_15+lg_mbr_15,
         total_16 = ind_mbr_16+sg_mbr_16+lg_mbr_16,
         total_17 = ind_mbr_17+sg_mbr_17+lg_mbr_17,
         total_18 = ind_mbr_18+sg_mbr_18+lg_mbr_18,
         total_19 = ind_mbr_19+sg_mbr_19+lg_mbr_19)%>%
  arrange(-total_19)

write.csv(member_profile,"member_profile.csv")

ind_profile <- withdemo%>%
  select(hios_id, plan_name, plan_group, business_state, ind_mbr_14, ind_mbr_15, ind_mbr_16, ind_mbr_17, ind_mbr_18, ind_mbr_19)%>%
  group_by(plan_group)%>%
  summarize(plans = n_distinct(hios_id),
            ind_mbr_15=sum(ind_mbr_15), 
            ind_mbr_16=sum(ind_mbr_16), 
            ind_mbr_17=sum(ind_mbr_17), 
            ind_mbr_18=sum(ind_mbr_18), 
            ind_mbr_19=sum(ind_mbr_19))%>%
  arrange(-ind_mbr_19)
write.csv(ind_profile,"ind_profile.csv")

sg_profile <- withdemo%>%
  select(hios_id, plan_name, plan_group, business_state, sg_mbr_14, sg_mbr_15, sg_mbr_16, sg_mbr_17, sg_mbr_18, sg_mbr_19)%>%
  group_by(plan_group)%>%
  summarize(plans = n_distinct(hios_id),
            sg_mbr_15=sum(sg_mbr_15), 
            sg_mbr_16=sum(sg_mbr_16), 
            sg_mbr_17=sum(sg_mbr_17), 
            sg_mbr_18=sum(sg_mbr_18), 
            sg_mbr_19=sum(sg_mbr_19))%>%
  arrange(-sg_mbr_19)
write.csv(sg_profile,"sg_profile.csv")

lg_profile <- withdemo%>%
  select(hios_id, plan_name, plan_group, business_state, lg_mbr_14, lg_mbr_15, lg_mbr_16, lg_mbr_17, lg_mbr_18, lg_mbr_19)%>%
  group_by(plan_group)%>%
  summarize(plans = n_distinct(hios_id),
            lg_mbr_15=sum(lg_mbr_15), 
            lg_mbr_16=sum(lg_mbr_16), 
            lg_mbr_17=sum(lg_mbr_17), 
            lg_mbr_18=sum(lg_mbr_18), 
            lg_mbr_19=sum(lg_mbr_19))%>%
  arrange(-lg_mbr_19)
write.csv(lg_profile,"lg_profile.csv")

#--------------------------------------------------------
# Here we look at relative price position
#--------------------------------------------------------

working <- withdemo%>%
  filter(group_affiliation != "UNITEDHEALTH GRP" & ind_mbr_15>1000 & ind_mbr_19>1000 & ind_rev_15>0 & ind_rev_19>0)%>%
  select(hios_id, plan_name, plan_group, business_state, ind_mm_15, ind_mm_16, ind_mm_17, ind_mm_18, ind_mm_19,
         ind_rev_15, ind_rev_16, ind_rev_17, ind_rev_18, ind_rev_19)%>%
  mutate(ind_pmpm_15 = round(ind_rev_15/ind_mm_15),
         ind_pmpm_16 = round(ind_rev_16/ind_mm_16),
         ind_pmpm_17 = round(ind_rev_17/ind_mm_17),
         ind_pmpm_18 = round(ind_rev_18/ind_mm_18),
         ind_pmpm_19 = round(ind_rev_19/ind_mm_19),
         pmpm_increase_17 = round((ind_pmpm_17-ind_pmpm_16), digits=2),
         pmpm_increase_18 = round((ind_pmpm_18-ind_pmpm_17), digits=2),
         pmpm_increase_19 = round((ind_pmpm_19-ind_pmpm_18), digits=2),
         increase_17 = round(((ind_pmpm_17/ind_pmpm_16-1)*100), digits=2),
         increase_18 = round(((ind_pmpm_18/ind_pmpm_17-1)*100), digits=2),
         increase_19 = round(((ind_pmpm_19/ind_pmpm_18-1)*100), digits=2)) %>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

state <- working%>%
  group_by(business_state)%>%
  summarize(ind_mm_15 = sum(ind_mm_15),
            ind_mm_16 = sum(ind_mm_16),
            ind_mm_17 = sum(ind_mm_17),
            ind_mm_18 = sum(ind_mm_18),
            ind_mm_19 = sum(ind_mm_19),
            ind_rev_15 = sum(ind_rev_15),
            ind_rev_16 = sum(ind_rev_16),
            ind_rev_17 = sum(ind_rev_17),
            ind_rev_18 = sum(ind_rev_18),
            ind_rev_19 = sum(ind_rev_19))%>%
  mutate(ind_pmpm_15 = round(ind_rev_15/ind_mm_15),
         ind_pmpm_16 = round(ind_rev_16/ind_mm_16),
         ind_pmpm_17 = round(ind_rev_17/ind_mm_17),
         ind_pmpm_18 = round(ind_rev_18/ind_mm_18),
         ind_pmpm_19 = round(ind_rev_19/ind_mm_19),
         st_pmpm_increase_17 = round((ind_pmpm_17-ind_pmpm_16), digits=2),
         st_pmpm_increase_18 = round((ind_pmpm_18-ind_pmpm_17), digits=2),
         st_pmpm_increase_19 = round((ind_pmpm_19-ind_pmpm_18), digits=2),
         st_increase_17 = round(((ind_pmpm_17/ind_pmpm_16-1)*100), digits=2),
         st_increase_18 = round(((ind_pmpm_18/ind_pmpm_17-1)*100), digits=2),
         st_increase_19 = round(((ind_pmpm_19/ind_pmpm_18-1)*100), digits=2)) %>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))%>%
  select(business_state, st_pmpm_increase_17, st_pmpm_increase_18, st_pmpm_increase_19)
  
joinstate <- working %>%
  inner_join(state, by="business_state")%>%
  mutate(state_rel_17 = round(((pmpm_increase_17/st_pmpm_increase_17-1)*100), digits=2),
         state_rel_18 = round(((pmpm_increase_18/st_pmpm_increase_18-1)*100), digits=2),
         state_rel_19 = round(((pmpm_increase_19/st_pmpm_increase_19-1)*100), digits=2))%>%
  select(hios_id, state_rel_17, state_rel_18, state_rel_19)



addpos <- top10_ind %>%
  inner_join(joinstate, by="hios_id")






Q_m <- quantile(relpos$ind_mbr_growth, probs=c(.25, .75), na.rm = FALSE)
iqr_m <- IQR(relpos$ind_mbr_growth)
mm_up <-  Q_m[2]+1.5*iqr_m # Upper Range  
mm_low<- Q_m[1]-1.5*iqr_m # Lower Range

Q_x   <- quantile(relpos$ind_rel_imp, probs=c(.25, .75), na.rm = FALSE)
iqr_x <- IQR(relpos$ind_rel_imp)
x_up  <-Q_x[2]+1.5*iqr_x # Upper Range  
x_low <- Q_x[1]-1.5*iqr_x # Lower Range

pairs_no <- filter(relpos, ind_mbr_growth > mm_low & ind_mbr_growth < mm_up & 
                     ind_rel_imp > x_low & ind_rel_imp < x_up)%>%
  filter(ind_rel_imp !=0)

cor.test(pairs_no$ind_mbr_growth, pairs_no$ind_rel_imp)

ggplot(data = pairs_no, aes(x=ind_rel_imp, y=ind_mbr_growth))+
  geom_point(color = "blue") +
  geom_smooth(method=lm, se=FALSE, color = "red", size=1) +
  scale_y_continuous(name="Membership Growth \n", limits=c(-50, 50), breaks = c(-50, -25, 0, 50, 25)) +
  scale_x_continuous(name="Investment Growth \n", limits=c(-75, 75), breaks = c(-75, -50, -25, 0, 50, 25, 75)) +
  theme_classic() +
  labs(title = "QI & CC Investment Growth Vs. Membership Growth",
       subtitle = "Large Group LOB Raw Plot\n",
       x = "\n % QI & CC Investment Growth",
       y = " % Membership Growth\n")  +
  annotate("text", x=25, y=-45, label = "Correlation = .20", size=5)



working <- withdemo%>%
  filter(ind_mbr_15>1000 & ind_mbr_19>1000 & sg_mbr_15>1000 & sg_mbr_19>1000 & lg_mbr_15>1000 & lg_mbr_19>1000)%>%
  select(hios_id, plan_name, plan_group, business_state, ind_mbr_15, ind_mbr_19, sg_mbr_15, sg_mbr_19,
         lg_mbr_15, lg_mbr_19, ind_mm_15, ind_mm_19, sg_mm_15, sg_mm_19, lg_mm_15, lg_mm_19,
         ind_rev_15, ind_rev_19, sg_rev_15, sg_rev_19, lg_rev_15, lg_rev_19)%>%
  mutate(ind_pmpm_15 = round(ind_rev_15/ind_mm_15),
         sg_pmpm_15 = round(sg_rev_15/sg_mm_15),
         lg_pmpm_15 = round(lg_rev_15/lg_mm_15),
         ind_pmpm_19 = round(ind_rev_19/ind_mm_19),
         sg_pmpm_19 = round(sg_rev_19/sg_mm_19),
         lg_pmpm_19 = round(lg_rev_19/lg_mm_19)) %>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 

state <- working%>%
  group_by(business_state)%>%
  summarize(ind_mm_15 = sum(ind_mm_15), 
            ind_mm_19 = sum(ind_mm_19), 
            sg_mm_15 = sum(sg_mm_15), 
            sg_mm_19 = sum(sg_mm_19), 
            lg_mm_15 = sum(lg_mm_15), 
            lg_mm_19 = sum(lg_mm_19),
            ind_rev_15 = sum(ind_rev_15), 
            ind_rev_19 = sum(ind_rev_19), 
            sg_rev_15 = sum(sg_rev_15), 
            sg_rev_19 = sum(sg_rev_19), 
            lg_rev_15 = sum(lg_rev_15), 
            lg_rev_19 = sum(lg_rev_19))%>%
  mutate(ind_st_pmpm_15 = round(ind_rev_15/ind_mm_15),
         sg_st_pmpm_15 = round(sg_rev_15/sg_mm_15),
         lg_st_pmpm_15 = round(lg_rev_15/lg_mm_15),
         ind_st_pmpm_19 = round(ind_rev_19/ind_mm_19),
         sg_st_pmpm_19 = round(sg_rev_19/sg_mm_19),
         lg_st_pmpm_19 = round(lg_rev_19/lg_mm_19))%>%
  select(business_state, ind_st_pmpm_15, sg_st_pmpm_15, lg_st_pmpm_15,
         ind_st_pmpm_19, sg_st_pmpm_19, lg_st_pmpm_19)




