if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)

in_dir <- "clean data"

files <- list.files(in_dir, pattern = "\\.xlsx$", full.names = TRUE)

sections <- lapply(files, read.xlsx)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)

#SEX OF HOUSEHOLD HEAD 

hh_head <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  filter(v107 == 1) %>%
  group_by(hhid) %>%
  slice(1) %>%
  select(hhid, uniq_id, v107, v103, v109) %>%
  rename(hh_head_sex = v103)

#INSURANCE STATUS OF HOUSEHOLD

section0 <- section0 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    insured = case_when(
      enrollment %in% c(1, 3) ~ 1, 
      TRUE ~ 0
    )
  )

#EDUCATION OF HOUSEHOLD HEAD 

section1b <- section1b %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

hh_head <- merge(
  hh_head,
  section1b[, c("uniq_id", "v115", "v116")], 
  by = "uniq_id",
  all = FALSE 
)

hh_head <- merge(
  hh_head, 
  section0[, c("ID", "hhid", "hhld_member_t", "insured")],
  by = "hhid", 
  all = FALSE
)

hh_head <- hh_head %>%
  mutate(
    v115 = case_when(
      is.na(v116)        ~ 1,
      v116 >= 1          ~ 2,
      TRUE               ~ v115   
    )
  )

#CALCULATING CONSUMPTION PER HOUSEHOLD

food_at_home <- section3a %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    home_production   = sum(as.numeric(v303), na.rm = TRUE),
    food_purchases    = sum(as.numeric(v304), na.rm = TRUE),
    received_in_kind  = sum(as.numeric(v305), na.rm = TRUE)
  ) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)),
    athome_consumption = rowSums(across(c(home_production, food_purchases, received_in_kind)), na.rm = TRUE),
    foodathome_annual  = athome_consumption * 52
  ) %>%
  ungroup()

food_away_from_home <- section3b %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    week_spend = sum(as.numeric(v308), na.rm = TRUE),
    week_receive = sum(as.numeric(v309), na.rm = TRUE)
  ) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)),
    awayhome_consumption = rowSums(across(c(week_spend, week_receive)), na.rm = TRUE),
    foodawayhome_annual = awayhome_consumption * 52
  ) %>%
  ungroup()

non_food_expenditure <- section4a %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    non_food_annual = sum(as.numeric(v403a), na.rm = TRUE),
    non_food_month = sum(as.numeric(v403b), na.rm = TRUE)
  ) %>%
  ungroup()

expenditure_abroad <- section4b %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    abroad_annual = sum(as.numeric(v407a), na.rm = TRUE),
    abroad_month = sum(as.numeric(v407b), na.rm = TRUE)
  ) %>%
  ungroup() 

consumption_of_goods <- section4d %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    goods_annual = sum(as.numeric(v416a), na.rm = TRUE),
    goods_month = sum(as.numeric(v416b), na.rm = TRUE)
  ) %>%
  ungroup()

consumption_dfs <- list(
  food_at_home,
  food_away_from_home, 
  non_food_expenditure, 
  expenditure_abroad,
  consumption_of_goods 
)

consumption_hh <- reduce(consumption_dfs, full_join, by = "hhid") %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_food_annual = foodathome_annual + foodawayhome_annual, 
    total_consumption = total_food_annual + non_food_annual + abroad_annual + goods_annual
  ) %>%
  select(hhid, total_consumption, total_food_annual, foodathome_annual, foodawayhome_annual, non_food_annual, abroad_annual, goods_annual)

hh_head <- merge(
  hh_head, 
  consumption_hh[, c("hhid", "total_food_annual")]
)

#CHRONIC ILLNESS STATUS 

chronic_household <- section6b1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  filter(v603 == 1) %>%
  group_by(hhid) %>%
  slice(1) %>%
  ungroup()

hh_head <- hh_head %>%
  mutate(
    chronic_illness = if_else(
      hhid %in% chronic_household$hhid,
      1, 0
    )
  )

#ACUTE ILLNESS STATUS 

acute_household <- section6c1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  filter(v629 == 1) %>%
  group_by(hhid) %>%
  slice(1) %>%
  ungroup()

hh_head <- hh_head %>%
  mutate(
    acute_illness = if_else(
      hhid %in% acute_household$hhid,
      1, 0
    )
  )

#OUT-OF-POCKET EXPENDITURE

hh_head <- merge(
  hh_head, 
  expenditure_hhld[, c("hhid", "reported_oop", "copay_amount", "total_health_cost")],
  by = "hhid",
  all = FALSE
)

hh_head <- merge(
  hh_head, 
  section2b[, c("hhid", "v249")]
)

#TOTAL HOUSEHOLD INCOME 

hh_head <- merge(
  hh_head, 
  income_hhld[, c("hhid", "total_income")],
  by = "hhid"
)

#MODEL

hh_head <- hh_head %>%
  mutate(
    log_oop = log(out_of_pocket + 1),
    log_income = log(total_income + 1),
    hh_head_sex = ifelse(hh_head_sex == 2, 0, hh_head_sex),
    across(everything(), ~ replace_na(.x, 0))
  ) 
  

model <- lm(log_oop ~ log_income + hh_head_sex + v116 + insured + chronic_illness + acute_illness, data = hh_head)

summary(model)
