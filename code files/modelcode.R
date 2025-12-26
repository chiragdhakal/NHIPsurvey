if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)

in_dir <- "stata_data1"

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

sections <- lapply(files, read_dta)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)

#MAKING THE ENTIRE DATASET NUMERIC

section1a <- section1a %>%
  mutate(
    ID = as.numeric(ID),
    psu = as.numeric(psu), 
    ward = as.numeric(ward),
    hhld = as.numeric(hhld),
    v101 = as.numeric(v101),
    v103 = as.numeric(v103), 
    v104a = as.numeric(gsub("[^0-9]", "", v104a)),
    v105 = as.numeric(v105), 
    v106 = as.numeric(v106),
    v107 = as.numeric(v107),
    v108 = as.numeric(v108),
    v109 = as.numeric(v109), 
    v110 = as.numeric(v110)
  ) 

for (i in setdiff(1:ncol(section1b), c(2, 6, 7, 8, 21, 22, 23))) {
  section1b[[i]] <- as.numeric(section1b[[i]])
}

for (i in setdiff(1:ncol(section2a1), c(2, 7, 8, 13, 15, 17, 19, 20))) {
  section2a1[[i]] <- as.numeric(gsub("[^0-9]", "", section2a1[[i]]))
}

for (i in setdiff(1:ncol(section2a2), c(2, 7, 8, 16))) {
  section2a2[[i]] <- as.numeric(gsub("[^0-9]", "", section2a2[[i]]))
}

for (i in setdiff(1:ncol(section2a3), c(2, 7, 8, 11, 14, 24, 33))) {
  section2a3[[i]] <- as.numeric(gsub("[^0-9]", "", section2a3[[i]]))
}

for (i in setdiff(1:ncol(section2b), c(2, 7, 8, 14:21, 23, 35, 45, 82))) {
  section2b[[i]] <- as.numeric(gsub("[^0-9]", "", section2b[[i]]))
}

for (i in setdiff(1:ncol(section2c), c(2, 8, 7, 13, 17, 19))) {
  section2c[[i]] <- as.numeric(gsub("[^0-9]", "", section2c[[i]]))
}

for (i in setdiff(1:ncol(section3a), c(2, 8, 7))) {
  section3a[[i]] <- as.numeric(gsub("[^0-9]", "", section3a[[i]]))
}

for (i in setdiff(1:ncol(section3b), c(2, 8, 7))) {
  section3b[[i]] <- as.numeric(gsub("[^0-9]", "", section3b[[i]]))
}

for (i in setdiff(1:ncol(section4a), c(2, 8, 7))) {
  section4a[[i]] <- as.numeric(gsub("[^0-9]", "", section4a[[i]]))
}

for (i in setdiff(1:ncol(section4b), c(2, 7, 8))) {
  section4b[[i]] <- as.numeric(gsub("[^0-9]", "", section4b[[i]]))
}

for (i in setdiff(1:ncol(section4c), c(2, 7, 8))) {
  section4c[[i]] <- as.numeric(gsub("[^0-9]", "", section4c[[i]]))
}

for (i in setdiff(1:ncol(section4d), c(2, 7, 8))) {
  section4d[[i]] <- as.numeric(gsub("[^0-9]", "", section4d[[i]]))
}

for (i in setdiff(1:ncol(section5), c(2, 7, 8))) {
  section5[[i]] <- as.numeric(gsub("[^0-9]", "", section5[[i]]))
}

for (i in setdiff(1:ncol(section6a), c(2, 7, 8))) {
  section6a[[i]] <- as.numeric(gsub("[^0-9]", "", section6a[[i]]))
}

for (i in setdiff(1:ncol(section6b1), c(2, 7, 8, 14, 21, 22, 23, 38))) {
  section6b1[[i]] <- as.numeric(gsub("[^0-9]", "", section6b1[[i]]))
}

for (i in setdiff(1:ncol(section6b2), c(2, 7, 8, 13:32))) {
  section6b2[[i]] <- as.numeric(gsub("[^0-9]", "", section6b2[[i]]))
}

for (i in setdiff(1:ncol(section6b3), c(2, 7, 8, 29))) {
  section6b3[[i]] <- as.numeric(gsub("[^0-9]", "", section6b3[[i]]))
}

for (i in setdiff(1:ncol(section6b4), c(2, 7, 8, 11))) {
  section6b4[[i]] <- as.numeric(gsub("[^0-9]", "", section6b4[[i]]))
}

for (i in setdiff(1:ncol(section6b5), c(2, 7, 8, 17))) {
  section6b5[[i]] <- as.numeric(gsub("[^0-9]", "", section6b5[[i]]))
}

for (i in setdiff(1:ncol(section6c1), c(2, 7, 8, 14, 15, 16))) {
  section6c1[[i]] <- as.numeric(gsub("[^0-9]", "", section6c1[[i]]))
}

for (i in setdiff(1:ncol(section6c2), c(2, 7, 8, 13, 14, 16:22))) {
  section6c2[[i]] <- as.numeric(gsub("[^0-9]", "", section6c2[[i]]))
}

for (i in setdiff(1:ncol(section6c3), c(2, 7, 8, 14:29))) {
  section6c3[[i]] <- as.numeric(gsub("[^0-9]", "", section6c3[[i]]))
}

for (i in setdiff(1:ncol(section6c4), c(2, 7, 8, 29, 34:36))) {
  section6c4[[i]] <- as.numeric(gsub("[^0-9]", "", section6c4[[i]]))
}

for (i in setdiff(1:ncol(section6d), c(2, 7, 8, 13, 24))) {
  section6d[[i]] <- as.numeric(gsub("[^0-9]", "", section6d[[i]]))
}

for (i in setdiff(1:ncol(section7), c(2, 7, 8, 19, 25, 34))) {
  section7[[i]] <- as.numeric(gsub("[^0-9]", "", section7[[i]]))
}

for (i in setdiff(1:ncol(section8), c(2, 7, 8, 13, 16))) { 
  section8[[i]] <- as.numeric(gsub("[^0-9]", "", section8[[i]]))
}

for (i in setdiff(1:ncol(section9a), c(2, 7, 8, 13, 20, 22))) { 
  section9a[[i]] <- as.numeric(gsub("[^0-9]", "", section9a[[i]]))
}

for (i in setdiff(1:ncol(section9b), c(2, 7, 8))) { 
  section9b[[i]] <- as.numeric(gsub("[^0-9]", "", section9b[[i]]))
}

for (i in setdiff(1:ncol(section9c), c(2, 7, 8, 11))) { 
  section9c[[i]] <- as.numeric(gsub("[^0-9]", "", section9c[[i]]))
}

for (i in setdiff(1:ncol(section9d), c(2, 7, 8))) { 
  section9d[[i]] <- as.numeric(gsub("[^0-9]", "", section9d[[i]]))
}

for (i in setdiff(1:ncol(section9e), c(2, 7, 8))) { 
  section9e[[i]] <- as.numeric(gsub("[^0-9]", "", section9e[[i]]))
}

for (i in setdiff(1:ncol(section9f1), c(2, 7, 8))) { 
  section9f1[[i]] <- as.numeric(gsub("[^0-9]", "", section9f1[[i]]))
}

for (i in setdiff(1:ncol(section9f2), c(2, 7, 8))) { 
  section9f2[[i]] <- as.numeric(gsub("[^0-9]", "", section9f2[[i]]))
}

for (i in setdiff(seq_len(ncol(section10)), c(2, 7, 8, 13, 15))) {
    section10[[i]] <- as.numeric(gsub("[^0-9]", "", section10[[i]]))
}

for (i in setdiff(1:ncol(section11a), c(2, 7, 8, 11, 14, 19, 22))) { 
  section11a[[i]] <- as.numeric(gsub("[^0-9]", "", section11a[[i]]))
}

for (i in setdiff(1:ncol(section11b), c(2, 7, 8, 11, 14, 19))) { 
  section11b[[i]] <- as.numeric(gsub("[^0-9]", "", section11b[[i]]))
}

for (i in setdiff(1:ncol(section11c), c(2, 7, 8))) { 
  section11c[[i]] <- as.numeric(gsub("[^0-9]", "", section11c[[i]])) 
}

for (i in setdiff(1:ncol(section12a), c(2, 7, 8, 16))) { 
  section12a[[i]] <- as.numeric(gsub("[^0-9]", "", section12a[[i]]))
}

for (i in setdiff(1:ncol(section12b), c(2, 7, 8))) { 
  section12b[[i]] <- as.numeric(gsub("[^0-9]", "", section12b[[i]]))
}

for (i in setdiff(1:ncol(section13a), c(2, 7, 8))) { 
  section13a[[i]] <- as.numeric(gsub("[^0-9]", "", section13a[[i]]))
}

for (i in setdiff(1:ncol(section13b), c(2, 7, 8))) { 
  section13b[[i]] <- as.numeric(gsub("[^0-9]", "", section13b[[i]]))
}

for (i in setdiff(1:ncol(section13c), c(2, 7, 8))) { 
  section13c[[i]] <- as.numeric(gsub("[^0-9]", "", section13c[[i]]))
}

#SEX OF HOUSEHOLD HEAD 

hh_head <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  filter(v107 == 1) %>%
  group_by(hhid) %>%
  select(ID, hhid, uniq_id, v107, v106, v105, v103, v109) %>%
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
  section0[, c("hhid", "hhld_member_t", "insured")],
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


section1a <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    mis_hh = if_else(v107 == 1 & v109 %in% c(3, 4), 1, 0)
  ) 

section1a <- section1a %>%
  group_by(hhid) %>%
  mutate(
    bad_head_flag = any(v107 == 1 & v109 %in% c(3, 4)),
    

    max_res_age = max(v104a[v109 == 1], na.rm = TRUE)
  ) %>%
  mutate(
    v107 = case_when(
    
      bad_head_flag == TRUE & v107 == 1 ~ 3, 
      
    
      bad_head_flag == TRUE & v109 == 1 & v104a == max_res_age ~ 1,
      

      TRUE ~ v107
    )
  ) %>%

  select(-bad_head_flag, -max_res_age) %>%
  ungroup()
