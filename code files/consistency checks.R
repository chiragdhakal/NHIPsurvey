if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)

#IMPORTING DATASET

in_dir <- "data"

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

sections <- lapply(files, read_dta)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)


#SUMMARISING SECTION 3A PER HOUSEHOLD

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


#SUMMARISING SECTION 3B PER HOUSEHOLD

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

#SUMMARISING SECTION 4A PER HOUSEHOLD 

non_food_expenditure <- section4a %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    non_food_annual = sum(as.numeric(v403a), na.rm = TRUE),
    non_food_month = sum(as.numeric(v403b), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 4B PER HOUSEHOLD


expenditure_abroad <- section4b %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    abroad_annual = sum(as.numeric(v407a), na.rm = TRUE),
    abroad_month = sum(as.numeric(v407b), na.rm = TRUE)
  ) %>%
  ungroup() 

#SUMMARISING SECTION 4D PER HOUSEHOLD

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
  select(hhid, total_food_annual, foodathome_annual, foodawayhome_annual, non_food_annual, abroad_annual, goods_annual, total_consumption)

#SUMMARISING SECTION 5 PER HOUSEHOLD

education_expenses <- section5 %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    tuition_fee = sum(as.numeric(v502a), na.rm = TRUE), 
    other_fee = sum(as.numeric(v502b), na.rm = TRUE), 
    dress_expense = sum(as.numeric(v502c), na.rm = TRUE), 
    books_expense = sum(as.numeric(v502d), na.rm = TRUE), 
    transportation_expense = sum(as.numeric(v502e), na.rm = TRUE), 
    private_tuition = sum(as.numeric(v502f), na.rm = TRUE), 
    other_expense = sum(as.numeric(v502g), na.rm = TRUE),
    scholarship = sum(as.numeric(v504), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_expense_education = rowSums(across(
      c(tuition_fee, other_fee, dress_expense, books_expense, transportation_expense, private_tuition, other_expense),
    ), na.rm = TRUE),
    net_expense_education = total_expense_education - scholarship 
  ) %>%
  ungroup()


#SUMMARISING SECTION 6.2.3 PER HOUSEHOLD                     

chronic_outpatient_expenditure <- section6b3 %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense = sum(as.numeric(v614a), na.rm = TRUE), 
    opd_expense = sum(as.numeric(v614b), na.rm = TRUE), 
    laboratory_expense = sum(as.numeric(v614c), na.rm = TRUE), 
    imaging_expense = sum(as.numeric(v614d), na.rm = TRUE), 
    medicine_expense = sum(as.numeric(v614e), na.rm = TRUE), 
    med_device_expense = sum(as.numeric(v614f), na.rm = TRUE), 
    transportation_expense = sum(as.numeric(v614g), na.rm = TRUE), 
    food_accom_expense = sum(as.numeric(v614h), na.rm = TRUE), 
    care_giver_expense = sum(as.numeric(v614i), na.rm = TRUE), 
    other_cost = sum(as.numeric(v614j), na.rm = TRUE), 
    total_cost = sum(as.numeric(v614k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_chronic_outpatient = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense, medicine_expense, med_device_expense, transportation_expense, food_accom_expense, care_giver_expense, other_cost),
    ), na.rm = TRUE),
    cost_discrepancy = total_cost - total_cost_chronic_outpatient
  )

#SUMMARISING SECTION 6.2.4 PER HOUSEHOLD

chronic_inpatient_expenditure <- section6b4 %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense = sum(as.numeric(v618a), na.rm = TRUE), 
    opd_expense = sum(as.numeric(v618b), na.rm = TRUE), 
    laboratory_expense = sum(as.numeric(v618c), na.rm = TRUE), 
    imaging_expense = sum(as.numeric(v618d), na.rm = TRUE), 
    medicine_expense = sum(as.numeric(v618e), na.rm = TRUE), 
    med_device_expense = sum(as.numeric(v618f), na.rm = TRUE), 
    transportation_expense = sum(as.numeric(v618g), na.rm = TRUE), 
    food_accom_expense = sum(as.numeric(v618h), na.rm = TRUE), 
    care_giver_expense = sum(as.numeric(v618i), na.rm = TRUE), 
    other_cost = sum(as.numeric(v618j), na.rm = TRUE), 
    total_cost = sum(as.numeric(v618k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_chronic_inpatient = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense, medicine_expense, med_device_expense),
    ), na.rm = TRUE),
    cost_discrepancy = total_cost - total_cost_chronic_inpatient
  )

#SUMMARISING SECTION 6.3.4 PER HOUSEHOLD

acute_expenditure <- section6c4 %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense = sum(as.numeric(v651a), na.rm = TRUE), 
    opd_expense = sum(as.numeric(v651b), na.rm = TRUE), 
    laboratory_expense = sum(as.numeric(v651c), na.rm = TRUE), 
    imaging_expense = sum(as.numeric(v651d), na.rm = TRUE), 
    medicine_expense = sum(as.numeric(v651e), na.rm = TRUE), 
    med_device_expense = sum(as.numeric(v651f), na.rm = TRUE), 
    transportation_expense = sum(as.numeric(v651g), na.rm = TRUE), 
    food_accom_expense = sum(as.numeric(v651h), na.rm = TRUE), 
    care_giver_expense = sum(as.numeric(v651i), na.rm = TRUE), 
    other_cost = sum(as.numeric(v651j), na.rm = TRUE), 
    total_cost = sum(as.numeric(v651k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_acute = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense, medicine_expense, med_device_expense),
    ), na.rm = TRUE
    ),
    cost_discrepancy = total_cost - total_cost_acute
  )

#SUMMARISING SECTION 6.4 

household_health <- section6d %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  select(hhid, v662, v663) %>%
  rename(
    reported_oop = v662,
    copay_amount = v663
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0))
  )

#AGGREGATING EXPENDITURE OF HOUSEHOLDS

expenditure_dfs <- list(
  consumption_hh, 
  education_expenses, 
  chronic_outpatient_expenditure, 
  chronic_inpatient_expenditure, 
  acute_expenditure,
  household_health
)

expenditure_hhld <- Reduce(function(x, y) full_join(x, y, by = "hhid"), expenditure_dfs)

expenditure_hhld <- expenditure_hhld %>%
  select(
    hhid, total_food_annual, net_expense_education, total_cost_chronic_inpatient, total_cost_chronic_outpatient, total_cost_acute, reported_oop, copay_amount
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_health_cost = rowSums(across(
      c(total_cost_chronic_inpatient, total_cost_chronic_outpatient, total_cost_acute)
    ), na.rm = TRUE),
    total_expenditure = rowSums(across(
      c(total_food_annual, net_expense_education, total_cost_chronic_inpatient, total_cost_chronic_outpatient, total_cost_acute)
    ), na.rm = TRUE)
  ) %>%
  distinct(hhid, .keep_all = TRUE)

#SUMMARISING SECTION 2 PER HOUSEHOLD 

rent_income <- section2a2 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    v212 = as.numeric(gsub("[^0-9]", "", v212)),
    rent_annual = v212 * 12
  ) %>%
  select(hhid, rent_annual)

rent_income <- rent_income %>%
  group_by(hhid) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

#SUMMARISING SECTION 8 PER HOUSEHOLD 

household_wage_income <- section8 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    day_income = (v805 * v806) + v807
  ) %>%
  group_by(hhid) %>%
  summarise(
    total_wage_income = sum(day_income, na.rm = TRUE), 
    total_salary = sum(v808a, na.rm = TRUE), 
    total_transport_allowance = sum(v808b, na.rm = TRUE), 
    total_bonus = sum(v808c, na.rm = TRUE),                       
    total_uniform_allowance = sum(v808d, na.rm = TRUE), 
    total_other_allowance = sum(v808e, na.rm = TRUE),
    total_salary_inkind = sum(v809, na.rm = TRUE), 
    total_contract_wage = sum(v810a, na.rm = TRUE), 
    total_contract_inkind = sum(v810b, na.rm = TRUE)
  ) %>%
  mutate(
    total_hh_salary = total_salary + total_transport_allowance + total_bonus + total_uniform_allowance + total_other_allowance + total_salary_inkind,
    hh_income = pmax(total_wage_income, total_hh_salary, na.rm = TRUE),
    total_hh_income = hh_income + total_contract_wage + total_contract_inkind
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.1 PER HOUSEHOLD 

landholding_agri <- section9a %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    expected_land_prices = sum(as.numeric(v906), na.rm = TRUE),                                       
    land_rent_received_cash = sum(as.numeric(v907a), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.2 PER HOUSEHOLD 

landholding_buysell <- section9b %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    land_sell = sum(as.numeric(v910), na.rm = TRUE),
    land_buy = sum(as.numeric(v913), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.3 PER HOUSEHOLD

land_production <- section9c %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    total_production_sale = sum(as.numeric(v918d), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.4 

agri_expenditure <- section9d %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  rename(
    seeds_price = v920, 
    seeds_transportation = v921,
    fertilizers_price = v923, 
    fertilizers_transportation = v924,
    farm_labour_expenditure = v927,
    irrigation_charges = v928, 
    land_improvements = v929, 
    equipment_repair = v930, 
    crop_insurance = v931, 
    animal_rent = v932a, 
    tractor_rent = v932b, 
    thrasher_rent = v932c, 
    other_expenditure = v932d
  ) %>%
  select(hhid, seeds_price, seeds_transportation, fertilizers_price, fertilizers_transportation,
         farm_labour_expenditure, irrigation_charges, land_improvements, equipment_repair,
         crop_insurance, animal_rent, tractor_rent, thrasher_rent, other_expenditure) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    agri_expense = seeds_price + seeds_transportation + fertilizers_price + fertilizers_transportation +
                   farm_labour_expenditure + irrigation_charges + land_improvements + equipment_repair +
                   crop_insurance + animal_rent + tractor_rent + thrasher_rent + other_expenditure
  )


#SUMMARISING SECTION 9.5 PER HOUSEHOLD

livestock_ownership <- section9e %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    own_livestock_price = sum(as.numeric(v936b), na.rm = TRUE),
    livestock_sell = sum(as.numeric(v938b), na.rm = TRUE), 
    livestock_buy = sum(as.numeric(v939b), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.6.1 PER HOUSEHOLD 

livestock_income <- section9f1 %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    total_livestock_income = sum(as.numeric(v941), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.6.2 PER HOUSEHOLD 

livestock_expenditure <- section9f2 %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    total_expenditure_livestock = sum(as.numeric(v943), na.rm = TRUE)
  ) %>%
  ungroup()

farm_dfs <- list(
  landholding_agri,
  landholding_buysell,
  land_production,
  livestock_ownership,
  livestock_income,
  livestock_expenditure,
  agri_expenditure
)

farm_hh <- Reduce(function(x, y) full_join(x, y, by = "hhid"), farm_dfs) 

farm_hh <- farm_hh %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)),
    total_farm_income = total_livestock_income + total_production_sale + land_rent_received_cash + livestock_sell,
    total_farm_expenditure = total_expenditure_livestock + agri_expense
  )

farm_hh <- farm_hh %>%
  distinct(hhid, .keep_all = TRUE)

#SUMMARISING SECTION 10 PER HOUSEHOLD 

non_agri_income <- section10 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  ) %>%
  group_by(hhid) %>%
  summarise(
    total_non_agri_income = sum(as.numeric(v1011))
  ) %>%
  ungroup() 

#SUMMARISING SECTION 12.1 PER HOUSEHOLD 

remittance_income <- section12a %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    total_money_received       = sum(as.numeric(v1210), na.rm = TRUE), 
    total_valueofgoods_received = sum(as.numeric(v1211), na.rm = TRUE),
    total_sent_abroad  = sum(as.numeric(v1212), na.rm = TRUE)
  ) %>%
  mutate(
    total_money_received       = replace_na(total_money_received, 0),
    total_valueofgoods_received = replace_na(total_valueofgoods_received, 0),
    total_sent_abroad  = replace_na(total_sent_abroad, 0),
    total_amount_received = rowSums(across(
      c(total_money_received, total_valueofgoods_received)
    )),
    net_remittance_received = total_amount_received
  ) %>%
  ungroup()


#SUMMARISING SECTION 13.1 PER HOUSEHOLD 

cash_transfer_program <- section13a %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    cash_assistance_received = sum(as.numeric(v1305), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 13.3 PER HOUSEHOLD

other_income <- section13c %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  group_by(hhid) %>%
  summarise(
    other_income_annual = sum(as.numeric(v1312), na.rm = TRUE)
  ) %>%
  ungroup()

#AGGREGATING INCOME OF HOUSEHOLDS

income_dfs <- list(
  household_wage_income, farm_hh, remittance_income, cash_transfer_program, non_agri_income, rent_income, other_income
)

income_hhld <- Reduce(function(x, y) full_join(x, y, by = "hhid"), income_dfs)

income_hhld <- income_hhld %>%
  select(hhid, total_hh_income, total_farm_income, net_remittance_received, cash_assistance_received, total_non_agri_income, rent_annual, other_income_annual) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)),
    total_income = rowSums(across(
      c(total_hh_income, total_farm_income, net_remittance_received, cash_assistance_received, total_non_agri_income, rent_annual, other_income_annual)
    ), na.rm = TRUE)
  )


income_expenditure_hhld <- merge.data.frame(income_hhld, expenditure_hhld, by.x = "hhid", by.y = "hhid", all = FALSE)

income_expenditure_hhld <- income_expenditure_hhld %>%
  mutate(
    income_expenditure_ratio = total_income / total_expenditure
  ) %>%
  select(hhid, total_income, total_expenditure, income_expenditure_ratio)

write.csv(income_expenditure_hhld, "hh_income_expenditure.csv")
