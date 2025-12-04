if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)
library(labelled)
library(officer)
library(flextable)
library(stringr)

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

#DESCRIPTIVE TABLE BASED ON SIZE  

desc_sec0 <- section0 %>%
  mutate(
    household_size = case_when(
      hhld_member_t >= 1 & hhld_member_t <= 2 ~ "1-2 persons",
      hhld_member_t >= 3 & hhld_member_t <= 4 ~ "3-4 persons", 
      hhld_member_t >= 5 & hhld_member_t <= 6 ~ "5-6 persons",
      hhld_member_t >= 7 ~ "7 or more persons"
    ) 
  ) %>%
  select(household_size)

table_sec0 <- map_df(names(desc_sec0), function(v) {
  var <- desc_sec0[[v]]
  fvar <- as_factor(var)
  freq <- table(fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
  )
})

ft_sec0 <- flextable(table_sec0) 
doc_sec0 <- read_docx()
doc_sec0 <- body_add_flextable(doc_sec0, ft_sec0)
print(doc_sec0, target = "descriptive tables/doc_sec0.docx")


#DESCRIPTIVE TABLE BASED ON SEX, ENTHICITY, RELIGION AND TYPE OF HOUSEHOLD MEMBER

section1a <- section1a %>%
  mutate(
    person = paste0(psu, "-", hhld, "-", v102),
    uniq_id = paste0(psu,"-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld),
    age_group = case_when(
    v104a >= 0 & v104a <= 14 ~ "0-14 years",
    v104a >= 15 & v104a <= 59 ~ "15-59 years", 
    v104a >= 60 ~ "60 years and above"
    )
  ) %>%
  distinct(person, .keep_all = TRUE) 

desc_sec1a <- section1a %>%
  select(v103, v105, v106, v109, age_group) %>%
  rename(
    sex = v103, 
    ethnicity = v105, 
    religion = v106, 
    category = v109
  )

table_sec1a <- map_df(names(desc_sec1a), function(v) {
  var <- desc_sec1a[[v]]
  fvar <- as_factor(var)
  freq <- table(fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
)
}) 

ft_sec1a <- flextable(table_sec1a) 
doc_sec1a <- read_docx()
doc_sec1a <- body_add_flextable(doc_sec1a, ft_sec1a)
print(doc_sec1a, target = "descriptive tables/desc_sec1a.docx")

#DESCRIPTIVE TABLE BASED ON EDUCATION 

section1b <- section1b %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld)
  )

section0 <- section0 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

desc_sec1b <- merge(
  section1b, 
  section0[, c("hhid", "province")],
  by = "hhid"
)

desc_sec1b <- merge(
  desc_sec1b, 
  section1a[, c("uniq_id", "v103")],
  by = "uniq_id"
)

desc_sec1b <- desc_sec1b %>%
  select(v114, v115, v116, v103, province) %>%
  rename(
    literate = v114, 
    attended_school = v115, 
    highest_edu = v116,
    gender = v103
  )

table_sec1b <- map_df(names(desc_sec1b), function(v) {
  var <- desc_sec1b[[v]]
  fvar <- as_factor(var)
  freq <- table (fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
  )
})

ft_sec1b <- flextable(table_sec1b) 

make_wide_table <- function(df, varname) {
  tmp <- df %>%
    mutate(
      value = as_factor(.data[[varname]]),
      gender = as_factor(gender),
      province = as_factor(province),
      col = paste0(province, " - ", gender)
    )

  totals <- tmp %>%
    group_by(col) %>%
    summarise(total = n(), .groups = "drop")

  counts <- tmp %>%
    group_by(value, col) %>%
    summarise(count = n(), .groups = "drop")

  counts <- counts %>%
    left_join(totals, by = "col") %>%
    mutate(percent = round(100 * count / total, 2)) %>%
    select(-total)

  counts %>%
    pivot_wider(
      names_from = col,
      values_from = c(count, percent),
      values_fill = 0
    ) %>%
    rename(category = value)
}
tbl_literate  <- make_wide_table(desc_sec1b, "literate")
tbl_attend    <- make_wide_table(desc_sec1b, "attended_school")
tbl_highest   <- make_wide_table(desc_sec1b, "highest_edu")

wb <- createWorkbook()

addWorksheet(wb, "original_table")
writeData(wb, "original_table", table_sec1b)

addWorksheet(wb, "literate_pg")
writeData(wb, "literate_pg", tbl_literate)

addWorksheet(wb, "attend_pg")
writeData(wb, "attend_pg", tbl_attend)

addWorksheet(wb, "highest_pg")
writeData(wb, "highest_pg", tbl_highest)

saveWorkbook(wb, "descriptive tables/sec1b_full_tables.xlsx", overwrite = TRUE)

#DESCRIPTIVE TABLE FOR TYPE OF DWELLING 

desc_sec2a1 <- section2a1 %>%
  select(v203, v204, v205, v206) %>%
  rename(
    house_foundation = v203, 
    outer_wall = v204, 
    roof = v205, 
    floor = v206
  )

table_sec2a1 <- map_df(names(desc_sec2a1), function(v) {
  var <- desc_sec2a1[[v]]
  fvar <- as_factor(var)
  freq <- table(fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
)
}) 

ft_sec2a1 <- flextable(table_sec2a1) 
doc_sec2a1 <- read_docx()
doc_sec2a1 <- body_add_flextable(doc_sec2a1, ft_sec2a1)
print(doc_sec2a1, target = "descriptive tables/doc_sec2a1.docx")

#DESCRIPTIVE TABLE FOR UTILITIES AND AMENITIES

desc_sec2a3 <- section2a3 %>%
  select(v216, v218, v220, v223, v225) %>%
  rename(
    drinking_water = v216, 
    cooking_fuel = v218, 
    lighting_source = v220, 
    garbage_dispose = v223, 
    toilet_type = v225
  )

table_sec2a3 <- map_df(names(desc_sec2a3), function(v) {
  var <- desc_sec2a3[[v]]
  fvar <- as_factor(var)
  freq <- table(fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
)
}) 

ft_sec2a3 <- flextable(table_sec2a3) 
doc_sec2a3 <- read_docx()
doc_sec2a3 <- body_add_flextable(doc_sec2a3, ft_sec2a3)
print(doc_sec2a3, target = "descriptive tables/doc_sec2a3.docx")

#DESCRIPTIVE TABLE FOR FARM INCOME 

desc_farmincome <- merge(
  farm_hh, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_farmincome <- desc_farmincome %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, total_production_sale, total_livestock_income, total_farm_income, total_farm_expenditure)

desc_farmincome <- desc_farmincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_farmincome, "descriptive tables/desc_farmincome.xlsx")

#DESCRIPTIVE TABLE FOR WAGE INCOME 

desc_wageincome <- merge(
  household_wage_income, 
  section0[, c("hhid", "province", "hhld_member_t")],
  by = "hhid", 
  all = FALSE
)

desc_wageincome <- desc_wageincome %>%
  select(-hhid) %>%
  mutate(percapita_wage = total_hh_income / hhld_member_t)

p1  <- quantile(desc_wageincome$percapita_wage, 0.01, na.rm = TRUE)
p99 <- quantile(desc_wageincome$percapita_wage, 0.99, na.rm = TRUE)

a1  <- quantile(desc_wageincome$total_hh_income, 0.01, na.rm = TRUE)
a99 <- quantile(desc_wageincome$total_hh_income, 0.99, na.rm = TRUE)

desc_wageincome <- desc_wageincome %>%
  filter(
    percapita_wage >= p1, 
    percapita_wage <= p99, 
    total_hh_income >= a1, 
    total_hh_income <= a99
  ) 
  group_by(province) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = mean,
      na.rm = TRUE
    )
  ) %>%
  select(province, total_hh_income, percapita_wage)

desc_wageincome <- desc_wageincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_wageincome, "descriptive tables/desc_wageincome.xlsx")

#DESCRIPTIVE TABLE FOR NON FARM ENTERPRISE INCOME

desc_nonagri <- merge(
  non_agri_income, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_nonagri <- desc_nonagri %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, total_non_agri_income) 

desc_nonagri <- desc_nonagri %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_nonagri, "descriptive tables/desc_nonagri.xlsx")

#DESCRIPTIVE TABLE FOR RENT INCOME 

desc_rent <- merge(
  rent_income, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_rent <- desc_rent %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, rent_annual) 

desc_rent <- desc_rent %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_rent, "descriptive tables/desc_rent.xlsx")

#DESCRIPTIVE TABLE FOR CASH TRANSFER INCOME

desc_cashtransfer <- merge(
  cash_transfer_program, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_cashtransfer <- desc_cashtransfer %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, cash_assistance_received) 

desc_cashtransfer <- desc_cashtransfer %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_cashtransfer, "descriptive tables/desc_cashtransfer.xlsx")

#DESCRIPTIVE TABLE FOR REMITTANCE INCOME 

desc_remittance <- merge(
  remittance_income, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

remittance_households <- desc_remittance %>%
  filter(!is.na(province)) %>%
  group_by(province) %>%
  summarise(hhlds = n()) %>%
  ungroup() %>%
  mutate(
    percent = (hhlds/nrow(section0)) * 100
  )

desc_remittance <- desc_remittance %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  select(province, total_amount_received, total_sent_abroad) 

desc_remittance <- desc_remittance %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(remittance_households, "descriptive tables/remittance_households.xlsx")

write.xlsx(desc_remittance, "descriptive tables/desc_remittance.xlsx")

#DESCRIPTIVE TABLE FOR TOTAL INCOME 

desc_totalincome <- merge(
  income_hhld, 
  section0[, c("hhid", "province", "hhld_member_t")],
  by = "hhid", 
  all = FALSE
)

desc_totalincome <- desc_totalincome %>%
  select(-hhid) %>%
  mutate(percapita_income = total_income / hhld_member_t)

p1  <- quantile(desc_totalincome$percapita_income, 0.01, na.rm = TRUE)
p99 <- quantile(desc_totalincome$percapita_income, 0.99, na.rm = TRUE)

desc_totalincome <- desc_totalincome %>%
  filter(percapita_income >= p1, percapita_income <= p99) %>%
  group_by(province) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = mean,
      na.rm = TRUE
    )
  ) %>%
  select(province, total_income, percapita_income)

desc_totalincome <- desc_totalincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_totalincome, "descriptive tables/desc_totalincome.xlsx")

#DESCRIPTIVE TABLE FOR CONSUMPTION EXPENDITURE 

desc_consumption <- merge(
  consumption_hh,
  section0[, c("hhid", "province", "hhld_member_t")],
  by = "hhid", 
  all = FALSE
)

desc_consumption <- desc_consumption %>%
  mutate(
    total_consumption = total_food_annual + non_food_annual + abroad_annual + goods_annual, 
    percapita_foodconsumption = total_food_annual / hhld_member_t,
    percapita_consumption = total_consumption / hhld_member_t
  )

summary(desc_consumption)

desc_consumption <- desc_consumption %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, total_food_annual, non_food_annual, goods_annual, percapita_foodconsumption, total_consumption, percapita_consumption) 

desc_consumption <- desc_consumption %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_consumption, "descriptive tables/desc_consumption.xlsx")

#DESCRIPTIVE TABLE FOR EDUCATION EXPENSES

desc_edu <- merge(
  education_expenses,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

summary(desc_edu)

desc_edu <- desc_edu %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, tuition_fee, other_fee, dress_expense, books_expense, transportation_expense, private_tuition, other_expense) 

desc_edu <- desc_edu %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_edu, "descriptive tables/desc_edu.xlsx")

#DESCRIPTIVE TABLE FOR OTHER INCOME

desc_otherincome <- merge(
  other_income,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

summary(desc_otherincome)

desc_otherincome <- desc_otherincome %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, other_income_annual) 

desc_otherincome <- desc_otherincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_otherincome, "descriptive tables/desc_otherincome.xlsx")

#DESCRIPTIVE TABLE FOR RATIO OF INCOME COMPONENTS 

ratio_income <- income_hhld %>%
  mutate(across(
    c(
      total_hh_income, rent_annual, net_remittance_received, 
      total_farm_income, cash_assistance_received,
      other_income_annual, total_non_agri_income
    ),
    ~ replace_na(as.numeric(.), 0)
  ))

ratio_income <- income_hhld %>%
  mutate(
    wage_ratio = total_hh_income / total_income,
    rent_ratio = rent_annual / total_income, 
    remittance_ratio = net_remittance_received / total_income, 
    farm_income_ratio = total_farm_income / total_income, 
    cash_assistance_ratio = cash_assistance_received / total_income, 
    other_income_ratio = other_income_annual / total_income, 
    non_agri_ratio = total_non_agri_income / total_income
  ) %>%
  select(hhid, wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio, cash_assistance_ratio, other_income_ratio, non_agri_ratio)

ratio_income <- merge(
  ratio_income,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

summary(ratio_income, na.rm = TRUE)

ratio_income <- ratio_income %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio, cash_assistance_ratio, other_income_ratio, non_agri_ratio) 

ratio_income <- ratio_income %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(ratio_income, "descriptive tables/ratio_income.xlsx")

#DESCRIPTIVE TABLE FOR LABOUR AND EMPLOYMENT

section1a <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

labor_force <- section1a %>%
  filter(v104a >= 10 & v104a <= 65)

labor_force <- labor_force %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(hhid, uniq_id, v103)

labor_force <- merge(
  labor_force, 
  section0[, c("hhid", "province")],
  by = "hhid"
)

section7_1 <- section7 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  select(uniq_id, v702, v705, v708, v710b)

labor_force <- merge(
  labor_force, 
  section7_1, 
  by = "uniq_id"
)

labor_force <- labor_force %>%
  mutate(lf_participant = ifelse(
  (v702 == 1 | v705 == 1) %in% TRUE,
  1,
  0
))

lfpr_province <- labor_force %>%
  group_by(province) %>%
  summarise(
    working_age = n(),                           
    labor_force = sum(lf_participant, na.rm=TRUE), 
    lfpr = round(labor_force / working_age * 100, 2)  
  )

lfpr_province_gender <- labor_force %>%
  group_by(province, v103) %>%
  summarise(
    working_age = n(), 
    labor_force = sum(lf_participant, na.rm = TRUE),
    lfpr = round(labor_force / working_age * 100, 2),
    .groups = "drop"
  )

work_population <- section7 %>%
  filter(v702 == 1 | v703 == 1 | v704 == 1) %>%
  mutate(
    v714a = str_extract(v714a, "^[0-9]+") %>% as.numeric(),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v708) %>%
  filter(!is.na(v708))

work_population <- merge(
  work_population, 
  section0[, c("hhid", "province")],
  by = "hhid"
)

work_population <- merge(
  work_population, 
  section1a[, c("uniq_id", "v103")],
  by = "uniq_id"
)

work_type_nepal <- work_population %>%
  group_by(v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

work_type_province <- work_population %>%
  group_by(province, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

work_type_gender <- work_population %>%
  group_by(v103, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))


write.xlsx(lfpr_province, "descriptive tables/lfpr_province.xlsx")
write.xlsx(lfpr_province_gender, "descriptive tables/lfpr_province_gender.xlsx")
write.xlsx(work_type_nepal, "descriptive tables/work_type_nepal.xlsx")
write.xlsx(work_type_province, "descriptive tables/work_type_province.xlsx")
write.xlsx(work_type_gender, "descriptive tables/work_type_gender.xlsx")


#DESCRIPTIVE TABLE FOR CHRONIC ILLNESSES 

chronic_qualified <- section1a %>%
  filter(
    v109 %in% c(1, 2) & v104a >= 5
  ) %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)

chronic_qualified <- merge(
  chronic_qualified, 
  section0[, c("hhid", "province")], 
  by = "hhid"
)

section6b1 <- section6b1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

chronic_qualified <- merge(
  chronic_qualified,
  section6b1[, c("uniq_id", "v604")],
  by = "uniq_id"
)

chronic_nepal <- chronic_qualified %>%
  filter(!is.na(v604)) %>%   
  count(v604) %>%
  mutate(percent = round(n/sum(n) * 100, 2))

chronic_province <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(province, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

chronic_gender <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(v103, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

write.xlsx(chronic_nepal, "descriptive tables/chronic_nepal.xlsx")
write.xlsx(chronic_province, "descriptive tables/chronic_province.xlsx")
write.xlsx(chronic_gender, "descriptive tables/chronic_gender.xlsx")

#DESCRIPTIVE TABLE FOR ACUTE ILLNESSES 

acute_qualified <- section1a %>%
  filter(
    v109 %in% c(1, 2) & v104a >= 5
  ) %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)

acute_qualified <- merge(
  acute_qualified, 
  section0[, c("hhid", "province")], 
  by = "hhid"
)

section6c1 <- section6c1 %>%
  mutate(
    v630 = as.numeric(v630),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

acute_qualified <- merge(
  acute_qualified,
  section6c1[, c("uniq_id", "v630")],
  by = "uniq_id"
)

acute_nepal <- acute_qualified %>%
  filter(!is.na(v630)) %>%   
  count(v630) %>%
  mutate(percent = round(n/sum(n) * 100, 2))

acute_province <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(province, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

acute_gender <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(v103, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

write.xlsx(acute_nepal, "descriptive tables/acute_nepal.xlsx")
write.xlsx(acute_province, "descriptive tables/acute_province.xlsx")
write.xlsx(acute_gender, "descriptive tables/acute_gender.xlsx")

#DESCRIPTIVE TABLE FOR EMPLOYMENT TYPES

section7 <- section7 %>%
  filter(v702 == 1 || v703 == 1 || v704 == 1) %>%
  mutate(
    v714a = str_extract(v714a, "^[0-9]+") %>% as.numeric(),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)
