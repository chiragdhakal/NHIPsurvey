if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)

section0 <- read.xlsx("dataset/cover page.xlsx")
section1a <- read.xlsx("dataset/section 1.xlsx")
section1b <- read.xlsx("dataset/Part 1_1 Household Roster-1.xlsx")
section2a1 <- read.xlsx("dataset/Household Characteristics.xlsx")
section2a2 <- read.xlsx("dataset/Section 2_1 Housing Expenses.xlsx")
section2a3 <- read.xlsx("dataset/Utilities and Amenities.xlsx")
section2b <- read.xlsx("dataset/Section 2_2_ National health insurence.xlsx")
section2c <- read.xlsx("dataset/PART 2_3_ Mortality (Death) Information.xlsx")
section3a <- read.xlsx("dataset/Section 3_ Consumption of Food.xlsx")
section3b <- read.xlsx("dataset/Part 3_1_ Food away from home.xlsx")
section4a <- read.xlsx("dataset/section 4.xlsx")
section4b <- read.xlsx("dataset/Part 4_2_ Expenditure Abroad.xlsx")
section4c <- read.xlsx("dataset/Part 4_3_ Inventory of Durable Goods.xlsx")
section4d <- read.xlsx("dataset/Part 4_4_ Own Account Consumption of Goods.xlsx")
section5 <- read.xlsx("dataset/Section 5_ Expense in Education.xlsx")
section6a <- read.xlsx("dataset/section 6.xlsx")
section6b1 <- read.xlsx("dataset/Part 6_2_1_ Chronic Illness and Health Seeking Behaviour.xlsx")
section6b2 <- read.xlsx("dataset/Part 6_2_2_ Chronic Illness and Expenditure Tracking.xlsx")
section6b3 <- read.xlsx("dataset/Part 6_2_3_ Chronic Illness and Expenditure Tracking – Outpatient (Regular Checkups).xlsx")
section6b4 <- read.xlsx("dataset/Part 6_2_4_ Chronic Illness and Expenditure Tracking – Inpatient.xlsx")
section6b5 <- read.xlsx("dataset/section 6_2_5.xlsx")
section6c1 <- read.xlsx("dataset/Part 6_3_1_ Acute Illness and health seeking behaviour.xlsx")
section6c2 <- read.xlsx("dataset/Part 6.3.2_ Acute illness and health screening.xlsx")
section6c3 <- read.xlsx("dataset/Part 6_3_3_ Acute Illness health seeking and expenditure tracking.xlsx")
section6c4 <- read.xlsx("dataset/Part 6_3_4_ Acute Illness health seeking and expenditure tracking.xlsx")
section6d <- read.xlsx("dataset/PART 6_4_ Household Health Care Seeking.xlsx")
section7 <- read.xlsx("dataset/Swction 7_ Labor and Employment.xlsx")
section8 <- read.xlsx("dataset/Section 8_ Wage Jobs.xlsx")
section9a <- read.xlsx("dataset/section 9.xlsx")
section9b <- read.xlsx("dataset/Part 9_2_ Landholding  Increase Decrease.xlsx")
section9c <- read.xlsx("dataset/Part 9_3_ Production and Uses.xlsx")
section9d <- read.xlsx("dataset/Part 9_4_ Expenditure.xlsx")
section9e <- read.xlsx("dataset/Part 9_5_ Livestock.xlsx")
section9f2 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure (1).xlsx")
section9f1 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure.xlsx")
section10 <- read.xlsx("dataset/Income from Non - Agricultural Enterprises.xlsx")
section11a <- read.xlsx("dataset/section 11.xlsx")
section11b <- read.xlsx("dataset/Part 11_2_ Lending and Outstanding Loans.xlsx")
section11c <- read.xlsx("dataset/Part 11_3_ Other Assets.xlsx")
section12a <- read.xlsx("dataset/Remittance and transfer.xlsx")
section12b <- read.xlsx("dataset/Part 12_2. Other Remittances.xlsx")
section13a <- read.xlsx("dataset/section 13.xlsx")
section13b <- read.xlsx("dataset/Part 13_2_ Social Assistance.xlsx")
section13c <- read.xlsx("dataset/Part 13_3_ Other Income.xlsx")

#MAKING HOUSEHOLD ID UNIQUE ACROSS THE ENTIRE DATASET

section0 <- section0 %>%
  group_by(psu) %>%
  mutate(
    hhld = row_number(),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  ungroup()

sections <- list(
  section1a, section1b, section2a1, section2a2, section2a3, section2b, section2c,
  section3a, section3b, section4a, section4b, section4c, section4d,
  section5, section6a, section6b1, section6b2, section6b3, section6b4,
  section6b5, section6c1, section6c2, section6c3, section6c4,
  section6d, section7, section8, section9a, section9b, section9c,
  section9d, section9e, section9f1, section9f2, section10,
  section11a, section11b, section11c, section12a, section12b,
  section13a, section13b, section13c
)

sections <- lapply(sections, function(df) {
  df <- df %>%
    select(-any_of("hhld")) %>%                   
    left_join(section0 %>% select(uid, hhld), by = "uid")  
  return(df)
})

names(sections) <- c(
  "section1a", "section1b", "section2a1", "section2a2", "section2a3", "section2b", "section2c",
  "section3a", "section3b", "section4a", "section4b", "section4c", "section4d",
  "section5", "section6a", "section6b1", "section6b2", "section6b3", "section6b4",
  "section6b5", "section6c1", "section6c2", "section6c3", "section6c4",
  "section6d", "section7", "section8", "section9a", "section9b", "section9c",
  "section9d", "section9e", "section9f1", "section9f2", "section10",
  "section11a", "section11b", "section11c", "section12a", "section12b",
  "section13a", "section13b", "section13c"
)

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
