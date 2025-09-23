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
section2a2 <- read.xlsx("dataset/Section 2_1 Housing Expenses.xlsx")
section2a3 <- read.xlsx("dataset/Utilities and Amenities.xlsx")
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
section9f1 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure (1).xlsx")
section9f2 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure.xlsx")
section10 <- read.xlsx("dataset/Income from Non - Agricultural Enterprises.xlsx")
section11a <- read.xlsx("dataset/section 11.xlsx")
section11b <- read.xlsx("dataset/Part 11_2_ Lending and Outstanding Loans.xlsx")
section11c <- read.xlsx("dataset/Part 11_3_ Other Assets.xlsx")
section12a <- read.xlsx("dataset/Remittance and transfer.xlsx")
section12b <- read.xlsx("dataset/Part 12_2. Other Remittances.xlsx")
section13a <- read.xlsx("dataset/section 13.xlsx")
section13b <- read.xlsx("dataset/Part 13_2_ Social Assistance.xlsx")
section13c <- read.xlsx("dataset/Part 13_3_ Other Income.xlsx")

#SUMMARISING SECTION 3A PER HOUSEHOLD

food_at_home <- section3a %>%
  group_by(ID) %>%
  summarise(
    home_production = sum(as.numeric(v303), na.rm = TRUE),
    food_purchases = sum(as.numeric(v304), na.rm = TRUE),
    received_in_kind = sum(as.numeric(v305), na.rm = TRUE)
  ) %>%
  mutate(
    athome_consumption = home_production + food_purchases + received_in_kind,      
    foodathome_annual = athome_consumption * 52
  ) %>%
  ungroup()

#SUMMARISING SECTION 3B PER HOUSEHOLD

food_away_from_home <- section3b %>%
  group_by(ID) %>%
  summarise(
    week_spend = sum(as.numeric(v308), na.rm = TRUE),
    week_receive = sum(as.numeric(v309), na.rm = TRUE)
  ) %>%
  mutate(
    awayhome_consumption = week_spend + week_receive,
    foodawayhome_annual = awayhome_consumption * 52
  ) %>%
  ungroup()

#SUMMARISING SECTION 4A PER HOUSEHOLD 

non_food_expenditure <- section4a %>%
  group_by(ID) %>%
  summarise(
    year_spend_nonfood = sum(as.numeric(v403a), na.rm = TRUE),
    month_spend_nonfood = sum(as.numeric(v403b), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 4B PER HOUSEHOLD

expenditure_abroad <- section4b %>%
  group_by(ID) %>%
  summarise(
    year_spend_abroad = sum(as.numeric(v407a), na.rm = TRUE),
    month_spend_abroad = sum(as.numeric(v407b), na.rm = TRUE)
  ) %>%
  ungroup() 

#SUMMARISING SECTION 4D PER HOUSEHOLD

consumption_of_goods <- section4d %>%
  group_by(ID) %>%
  summarise(
    year_spend_goods = sum(as.numeric(v416a), na.rm = TRUE),
    month_spend_goods = sum(as.numeric(v416b), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 5 PER HOUSEHOLD

education_expenses <- section5 %>%
  group_by(ID) %>%
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
    total_expense_education = tuition_fee + other_fee + dress_expense + books_expense + transportation_expense + private_tuition + other_expense,
    net_expense_education = total_expense_education - scholarship 
  ) %>%
  ungroup()


#SUMMARISING SECTION 6.2.3 PER HOUSEHOLD                     

chronic_outpatient_expenditure <- section6b3 %>%
  group_by(ID) %>%
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
    calculated_total_cost_chronic_outpatient = emergency_expense + opd_expense + laboratory_expense + imaging_expense + medicine_expense + med_device_expense + transportation_expense + food_accom_expense + care_giver_expense + other_cost,
    cost_discrepancy = total_cost - calculated_total_cost_chronic_outpatient
  )

#SUMMARISING SECTION 6.2.4 PER HOUSEHOLD

chronic_inpatient_expenditure <- section6b4 %>%
  group_by(ID) %>%
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
    calculated_total_cost_chronic_inpatient = emergency_expense + opd_expense + laboratory_expense + imaging_expense + medicine_expense + med_device_expense + transportation_expense + food_accom_expense + care_giver_expense + other_cost,
    cost_discrepancy = total_cost - calculated_total_cost_chronic_inpatient
  )

#SUMMARISING SECTION 6.3.4 PER HOUSEHOLD

acute_expenditure <- section6c4 %>%
  group_by(ID) %>%
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
    calculated_total_cost_acute = emergency_expense + opd_expense + laboratory_expense + imaging_expense + medicine_expense + med_device_expense + transportation_expense + food_accom_expense + care_giver_expense + other_cost,
    cost_discrepancy = total_cost - calculated_total_cost_acute
  )

#SUMMARISING SECTION 8 PER HOUSEHOLD 

household_wage_income <- section8 %>%
  mutate(
    day_income = (as.numeric(v805) * as.numeric(v806)) + as.numeric(v807)
  ) %>%
  group_by(ID) %>%
  summarise(
    total_day_income = sum(as.numeric(day_income), na.rm = TRUE), 
    total_salary = sum(as.numeric(v808a), na.rm = TRUE), 
    total_transport_allowance = sum(as.numeric(v808b), na.rm = TRUE), 
    total_bonus = sum(as.numeric(v808c), na.rm = TRUE),                       
    total_uniform_allowance = sum(as.numeric(v808d), na.rm = TRUE), 
    total_other_allowance = sum(as.numeric(v808e), na.rm = TRUE),
    total_salary_inkind = sum(as.numeric(v809), na.rm = TRUE), 
    total_contract_wage = sum(as.numeric(v810a), na.rm = TRUE), 
    total_contract_inkind = sum(as.numeric(v810b), na.rm = TRUE)
  ) %>%
  mutate(
    total_hh_income = total_day_income + total_salary + total_transport_allowance + total_bonus + total_uniform_allowance + total_other_allowance + total_salary_inkind + total_contract_wage + total_contract_inkind
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.1 PER HOUSEHOLD 

landholding_agri <- section9a %>%
  group_by(ID) %>%
  summarise(
    expected_land_prices = sum(as.numeric(v906), na.rm = TRUE),                                       
    land_rent_received_cash = sum(as.numeric(v907a), na.rm = TRUE),
    land_rent_received_inkind = sum(as.numeric(v907b), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.2 PER HOUSEHOLD 

landholding_buysell <- section9b %>%
  group_by(ID) %>%
  summarise(
    land_sell = sum(as.numeric(v910), na.rm = TRUE),
    land_buy = sum(as.numeric(v913), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.3 PER HOUSEHOLD

land_production <- section9c %>%
  group_by(ID) %>%
  summarise(
    total_production_sale = sum(as.numeric(v918d), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.5 PER HOUSEHOLD

livestock_ownership <- section9e %>%
  group_by(ID) %>%
  summarise(
    own_livestock_price = sum(as.numeric(v936b), na.rm = TRUE),
    livestock_sell = sum(as.numeric(v938b), na.rm = TRUE), 
    livestock_buy = sum(as.numeric(v939b), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.6.1 PER HOUSEHOLD 

livestock_income <- section9f1 %>%
  group_by(ID) %>%
  summarise(
    total_livestock_income = sum(as.numeric(v941), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 9.6.2 PER HOUSEHOLD 

livestock_expenditure <- section9f2 %>%
  group_by(ID) %>%
  summarise(
    total_expenditure_livestock = sum(as.numeric(v943), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 11.1 PER HOUSEHOLD 

borrowing_outstanding_loan <- section11a %>%
  group_by(ID) %>%
  summarise(
    total_loan = sum(as.numeric(v1106), na.rm = TRUE),
    total_interest = sum(as.numeric(v1107a), na.rm = TRUE),
    total_paidback = sum(as.numeric(v1110), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 11.2 PER HOUSEHOLD 

lending_outstanding_loan <- section11b %>%
  group_by(ID) %>%
  summarise(
    total_lended = sum(as.numeric(v1116), na.rm = TRUE), 
    total_interest = sum(as.numeric(v1117a), na.rm = TRUE), 
    total_repaid = sum(as.numeric(v1120), na.rm = TRUE)
  ) %>%
  ungroup()

#SUMMARISING SECTION 12.1 PER HOUSEHOLD 

remittance_income <- section12a %>%
  group_by(ID) %>%
  summarise(
    total_money_received = sum(as.numeric(v1210), na.rm = TRUE), 
    total_valueofgoods_received = sum(as.numeric(v1211), na.rm = TRUE),
    total_cash_inkind_received = sum(as.numeric(v1212), na.rm = TRUE)
  ) %>%
  ungroup() 

#SUMMARISING SECTION 13.1 PER HOUSEHOLD 

cash_transfer_program <- section13a %>%
  group_by(ID) %>%
  summarise(
    cash_assistance_received = sum(as.numeric(v1305), na.rm = TRUE)
  )


expenditure_dfs <- list(
  food_at_home, food_away_from_home, non_food_expenditure, expenditure_abroad, 
  consumption_of_goods, education_expenses, chronic_outpatient_expenditure, 
  chronic_inpatient_expenditure, acute_expenditure, livestock_expenditure
)

expenditure_hhld <- Reduce(function(x, y) left_join(x, y, by = "ID"), expenditure_dfs)

expenditure_hhld <- expenditure_hhld %>%
  select(
    ID, foodathome_annual, foodawayhome_annual, year_spend_abroad, year_spend_goods, net_expense_education,
    calculated_total_cost_chronic_inpatient, calculated_total_cost_chronic_outpatient, calculated_total_cost_acute, total_expenditure_livestock
  ) %>%
  mutate(
    total_expenditure = rowSums(across(
      c(foodathome_annual, foodawayhome_annual, year_spend_abroad, year_spend_goods, net_expense_education,
        calculated_total_cost_chronic_inpatient, calculated_total_cost_chronic_outpatient, calculated_total_cost_acute, total_expenditure_livestock)
    ), na.rm = TRUE)
  )

income_dfs <- list(
  household_wage_income, landholding_agri, landholding_buysell, land_production, livestock_income,
  remittance_income, cash_transfer_program
)

income_hhld <- Reduce(function(x, y) left_join(x, y, by = "ID"), income_dfs)

income_hhld <- income_hhld %>%
  select(ID, total_hh_income, total_production_sale, land_rent_received_cash, total_money_received, cash_assistance_received) %>%
  mutate(
    total_income = rowSums(across(
      c(total_hh_income, total_production_sale, land_rent_received_cash, total_money_received, cash_assistance_received)
    ), na.rm = TRUE)
  )


income_expenditure_hhld <- merge.data.frame(income_hhld, expenditure_hhld, by.x = "ID", by.y = "ID", all = FALSE)

income_expenditure_hhld <- income_expenditure_hhld %>%
  select(ID, total_expenditure, total_income) %>%
  mutate(
    income_expenditure_ratio = total_income / total_expenditure
  )

hh_flagged <- income_expenditure_hhld %>%
  filter(income_expenditure_ratio < 0.5 | income_expenditure_ratio > 1.5)
