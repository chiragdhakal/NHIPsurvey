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
section9f1 <- read.xlsx("dataset/Part 9_6_ Livestock  Expenditure.xlsx")
section9f2 <- read.xlsx("dataset/Part 9_6_ Livestock Income.xlsx")
section10 <- read.xlsx("dataset/Income from Non - Agricultural Enterprises.xlsx")
section11a <- read.xlsx("dataset/section 11.xlsx")
section11b <- read.xlsx("dataset/Part 11_2_ Lending and Outstanding Loans.xlsx")
section11c <- read.xlsx("dataset/Part 11_3_ Other Assets.xlsx")
section12a <- read.xlsx("dataset/Remittance and transfer.xlsx")
section12b <- read.xlsx("dataset/Part 12_2. Other Remittances.xlsx")
section13a <- read.xlsx("dataset/section 13.xlsx")
section13b <- read.xlsx("dataset/Part 13_2_ Social Assistance.xlsx")
section13c <- read.xlsx("dataset/Part 13_3_ Other Income.xlsx")

#COUNTING NUMBER OF HOUSEHOLDS IN EACH PSU  
psu_counts <- section0 %>%
  group_by(psu) %>%
  summarise(n_hhlds = n()) %>%
  ungroup()

NHIP_insured <- c(1101:1112, 2101:2110, 3101:3115, 4101:4109, 5101:5111, 6101:6108, 7101:7109)
NHIP_non_insured <- c(1201:1212, 2201:2210, 3201:3215, 4201:4209, 5201:5211, 6201:6208, 7201:7209)
SSF_insured <- c(1301:1318, 2301:2315, 3301:3390, 4301:4322, 5301:5323, 6301:6316, 7301:7311)
SSF_non_insured <- c(1401:1418, 2401:2415, 3401:3490, 4401:4422, 5401:5423, 6401:6416, 7401:7411)

total_interviewed <- psu_counts %>%
  mutate(
    category = case_when(
      psu %in% NHIP_insured    ~ "NHIP_insured",
      psu %in% NHIP_non_insured ~ "NHIP_non_insured",
      psu %in% SSF_insured     ~ "SSF_insured",
      psu %in% SSF_non_insured ~ "SSF_non_insured",
      TRUE                     ~ "Other"
    )
  ) %>%
  group_by(category) %>%
  summarise(total_hhlds = sum(n_hhlds))

#VALIDATING PALIKA AND DISTRICT
nhip_check_geoinputs <- section0 %>%
  filter(
    #KOSHI
    (psu %in% c(1101, 1201) & !(province == 1 & district == 102 & palika == 10207)) |
    (psu %in% c(1102, 1202) & !(province == 1 & district == 103 & palika == 10304)) |
    (psu %in% c(1103, 1203) & !(province == 1 & district == 106 & palika == 10602)) |
    (psu %in% c(1104, 1204) & !(province == 1 & district == 108 & palika == 10804)) |
    (psu %in% c(1105, 1205) & !(province == 1 & district == 108 & palika == 10805)) |
    (psu %in% c(1106, 1206) & !(province == 1 & district == 110 & palika == 11005)) |
    (psu %in% c(1107, 1207) & !(province == 1 & district == 111 & palika == 11102)) |
    (psu %in% c(1108, 1208) & !(province == 1 & district == 112 & palika == 11211)) |
    (psu %in% c(1109, 1209) & !(province == 1 & district == 112 & palika == 11214)) |
    (psu %in% c(1110, 1210) & !(province == 1 & district == 113 & palika == 11303)) |
    (psu %in% c(1111, 1211) & !(province == 1 & district == 113 & palika == 11306)) |
    (psu %in% c(1112, 1212) & !(province == 1 & district == 113 & palika == 11309)) |
    #MADHESH
    (psu %in% c(2101, 2201) & !(province == 2 & district == 201 & palika == 20110)) |
    (psu %in% c(2102, 2202) & !(province == 2 & district == 201 & palika == 20111)) |
    (psu %in% c(2103, 2203) & !(province == 2 & district == 202 & palika == 20206)) |
    (psu %in% c(2104, 2204) & !(province == 2 & district == 202 & palika == 20214)) |
    (psu %in% c(2105, 2205) & !(province == 2 & district == 203 & palika == 20302)) |
    (psu %in% c(2106, 2206) & !(province == 2 & district == 205 & palika == 20514)) |
    (psu %in% c(2107, 2207) & !(province == 2 & district == 206 & palika == 20613)) |
    (psu %in% c(2108, 2208) & !(province == 2 & district == 206 & palika == 20614)) |
    (psu %in% c(2109, 2209) & !(province == 2 & district == 207 & palika == 20703)) |
    (psu %in% c(2110, 2210) & !(province == 2 & district == 208 & palika == 20807)) |
    #BAGMATI
    (psu %in% c(3101, 3201) & !(province == 3 & district == 301 & palika == 30102)) |
    (psu %in% c(3102, 3202) & !(province == 3 & district == 302 & palika == 30207)) |
    (psu %in% c(3103, 3203) & !(province == 3 & district == 302 & palika == 30212)) |
    (psu %in% c(3104, 3204) & !(province == 3 & district == 305 & palika == 30512)) |
    (psu %in% c(3105, 3205) & !(province == 3 & district == 306 & palika == 30601)) |
    (psu %in% c(3106, 3206) & !(province == 3 & district == 306 & palika == 30604)) |
    (psu %in% c(3107, 3207) & !(province == 3 & district == 306 & palika == 30608)) |
    (psu %in% c(3108, 3208) & !(province == 3 & district == 308 & palika == 30802)) |
    (psu %in% c(3109, 3209) & !(province == 3 & district == 308 & palika == 30803)) |
    (psu %in% c(3110, 3210) & !(province == 3 & district == 309 & palika == 30909)) |
    (psu %in% c(3111, 3211) & !(province == 3 & district == 311 & palika == 31101)) |
    (psu %in% c(3112, 3212) & !(province == 3 & district == 312 & palika == 31206)) |
    (psu %in% c(3113, 3213) & !(province == 3 & district == 313 & palika == 31301)) |
    (psu %in% c(3114, 3214) & !(province == 3 & district == 313 & palika == 31303)) |
    (psu %in% c(3115, 3215) & !(province == 3 & district == 313 & palika == 31304)) |
    #GANDAKI
    (psu %in% c(4101, 4201) & !(province == 4 & district == 402 & palika == 40204)) |
    (psu %in% c(4102, 4202) & !(province == 4 & district == 405 & palika == 40504)) |
    (psu %in% c(4103, 4203) & !(province == 4 & district == 406 & palika == 40602)) |
    (psu %in% c(4104, 4204) & !(province == 4 & district == 406 & palika == 40605)) |
    (psu %in% c(4105, 4205) & !(province == 4 & district == 407 & palika == 40704)) |
    (psu %in% c(4106, 4206) & !(province == 4 & district == 408 & palika == 40803)) |
    (psu %in% c(4107, 4207) & !(province == 4 & district == 408 & palika == 40806)) |
    (psu %in% c(4108, 4208) & !(province == 4 & district == 410 & palika == 41003)) |
    (psu %in% c(4109, 4209) & !(province == 4 & district == 410 & palika == 41007)) |
    #LUMBINI
    (psu %in% c(5101, 5201) & !(province == 5 & district == 504 & palika == 50403)) |
    (psu %in% c(5102, 5202) & !(province == 5 & district == 506 & palika == 50601)) |
    (psu %in% c(5103, 5203) & !(province == 5 & district == 506 & palika == 50609)) |
    (psu %in% c(5104, 5204) & !(province == 5 & district == 507 & palika == 50703)) |
    (psu %in% c(5105, 5205) & !(province == 5 & district == 509 & palika == 50901)) |
    (psu %in% c(5106, 5206) & !(province == 5 & district == 509 & palika == 50903)) |
    (psu %in% c(5107, 5207) & !(province == 5 & district == 510 & palika == 51007)) |
    (psu %in% c(5108, 5208) & !(province == 5 & district == 510 & palika == 51008)) |
    (psu %in% c(5109, 5209) & !(province == 5 & district == 511 & palika == 51105)) |
    (psu %in% c(5110, 5210) & !(province == 5 & district == 511 & palika == 51106)) |
    (psu %in% c(5111, 5211) & !(province == 5 & district == 511 & palika == 51108)) |
    #KARNALI
    (psu %in% c(6101, 6201) & !(province == 6 & district == 603 & palika == 60303)) |
    (psu %in% c(6102, 6202) & !(province == 6 & district == 604 & palika == 60404)) |
    (psu %in% c(6103, 6203) & !(province == 6 & district == 606 & palika == 60602)) |
    (psu %in% c(6104, 6204) & !(province == 6 & district == 606 & palika == 60608)) |
    (psu %in% c(6105, 6205) & !(province == 6 & district == 607 & palika == 60703)) |
    (psu %in% c(6106, 6206) & !(province == 6 & district == 608 & palika == 60806)) |
    (psu %in% c(6107, 6207) & !(province == 6 & district == 609 & palika == 60902)) |
    (psu %in% c(6108, 6208) & !(province == 6 & district == 610 & palika == 61004)) |
    #FAR-WEST
    (psu %in% c(7101, 7201) & !(province == 7 & district == 701 & palika == 70103)) |
    (psu %in% c(7102, 7202) & !(province == 7 & district == 702 & palika == 70210)) |
    (psu %in% c(7103, 7203) & !(province == 7 & district == 704 & palika == 70403)) |
    (psu %in% c(7104, 7204) & !(province == 7 & district == 704 & palika == 70410)) |
    (psu %in% c(7105, 7205) & !(province == 7 & district == 706 & palika == 70601)) |
    (psu %in% c(7106, 7206) & !(province == 7 & district == 708 & palika == 70811)) |
    (psu %in% c(7107, 7207) & !(province == 7 & district == 708 & palika == 70813)) |
    (psu %in% c(7108, 7208) & !(province == 7 & district == 709 & palika == 70901)) |
    (psu %in% c(7109, 7209) & !(province == 7 & district == 709 & palika == 70909))
  )

write.csv(nhip_check_geoinputs, "nhipincorrect_geoinputs.csv")

ssf_check_geopoints <- section0 %>%
  filter(
    #KOSHI
    psu == 1301 & !(province == 1 & district == 102 & palika == 10206) |
    psu == 1302 & !(province == 1 & district == 110 & palika == 11003) |
    psu == 1303 & !(province == 1 & district == 112 & palika == 11203) |
    psu == 1306 & !(province == 1 & district == 111 & palika == 11101) |
    psu == 1307 & !(province == 1 & district == 111 & palika == 11112) |
    psu == 1308 & !(province == 1 & district == 113 & palika == 11306) |
    psu == 1309 & !(province == 1 & district == 113 & palika == 11306) |
    psu == 1310 & !(province == 1 & district == 110 & palika == 11003) |
    psu == 1314 & !(province == 1 & district == 113 & palika == 11307) |
    psu == 1315 & !(province == 1 & district == 112 & palika == 11213) |
    psu == 1316 & !(province == 1 & district == 111 & palika == 11101) |
    psu == 1317 & !(province == 1 & district == 113 & palika == 11307) |
    psu == 1318 & !(province == 1 & district == 112 & palika == 11214) |
    #MADHESH
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107) 
  )

#CHECKING FOR AGE <= 5 IN SECTION 6.1

age_mishap <- section1a %>%
  inner_join(section6a, by = "personid") %>%
  mutate(v104a = as.integer(v104a)) %>%
  select(ID = ID.x, palika = palika.x, ward = ward.x,
         hhld = hhld.x, version = version.x, verified = verified.x,
         v101 = v101.x, v104a, v601) %>%
  filter(is.na(v104a) | v104a <= 5) 

write.csv(age_mishap, "section6_lessthan5.csv")

#ENUMERATOR WISE NUMBERS OF NO HEALTH RECORDS 

chronic_households <- section6b1 %>% 
  mutate(v603 = as.integer(v603)) %>%
  group_by(ID) %>%
  summarise(
    hh_has_sick = any(v603 == 1),
    .groups = "drop"
  )

chronic_households <- merge.data.frame(chronic_households, section0, by.x = "ID", by.y = "ID")

chronic_households <- chronic_households %>%
  select(ID, hh_has_sick, Name.of.enumerator) %>%
  group_by(hh_has_sick, Name.of.enumerator) %>%
  summarise(
    n_households = n(),
    .groups = "drop"
  )

write.csv(chronic_households, "chronic_counts.csv")

acute_households <- section6c1 %>% 
  mutate(v629 = as.integer(v629)) %>%
  group_by(ID) %>%
  summarise(
    hh_has_sick = any(v629 == 1),
    .groups = "drop"
  )

acute_households <- merge.data.frame(acute_households, section0, by.x = "ID", by.y = "ID")

acute_households <- acute_households %>%
  select(ID, hh_has_sick, Name.of.enumerator) %>%
  group_by(hh_has_sick, Name.of.enumerator) %>%
  summarise(
    n_households = n(),
    .groups = "drop"
  )

write.csv(acute_households, "acute_counts.csv")

#CHECKING FOR HOUSEHOLDS WITH NO HEALTH EXPENDITURE

sec4_healthex <- section4a %>%
  filter(v401 == 22)

health_expenditure <- section2b %>%
  inner_join(section6d, by = "ID") %>%
  inner_join(sec4_healthex, by = "ID")

health_expenditure <- health_expenditure %>%
  select(ID, psu = psu.x, palika = palika.x, ward = ward.x,
         hhld = hhld.x, version = version.x, verified = verified.x,
         v249, v662, v403a, v403b)

no_healthexpense <- merge.data.frame(health_expenditure, section0, by.x = "ID", by.y = "ID")

no_healthexpense <- no_healthexpense %>%
  filter(v249 == 0) %>%
  select(ID, psu = psu.x, palika = palika.x, ward = ward.x, hhld = hhld.x, version = version.x, verified = verified.x, v249, respondent, Name.of.enumerator)

no_expense_enumerator <- no_healthexpense %>%
  group_by(Name.of.enumerator) %>%
  summarise(
    n_hhlds = n()
  )

health_expenditure <- health_expenditure %>%
  mutate(
    v249 = as.integer(v249), 
    v662 = as.integer(v662)
  )
  
boxplot(
  list(`SECTION 2` = health_expenditure$v249,
       `SECTION 6` = health_expenditure$v662),
  na.rm = TRUE
)

ruma_healthexpense <- no_healthexpense %>%
  filter(Name.of.enumerator == "RUMA LINGTHEP")

pooja_healthexpense <- no_healthexpense %>%
  filter(Name.of.enumerator == "POOJA RAWAT")

devika_healthexpense <- no_healthexpense %>%
  filter(Name.of.enumerator == "DEVIKA ACHARYA")

siraj_healthexpense <- no_healthexpense %>%
  filter(Name.of.enumerator == "SIRAJ POKHREL")

#Section8 discrepency 

outside_household <- section1a %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(v109 %in% c(3, 4)) %>%
  select(personid, v109)

outside_household <- merge.data.frame(section8, outside_household, by.x = "personid", by.y = "personid", all = FALSE)

outside_household <- outside_household %>%
  select(personid, ID, psu, palika, ward, hhld, version, verified, v101, v802, v109)

write.csv(outside_household, "section8_discrepency.csv")

#UNIQUE ID 
section0 <- section0 %>%
  group_by(psu) %>%
  arrange(psu, hhld, ID) %>%      
  mutate(
    hhld_serial = row_number(),     
    uniq_id = paste0(psu, "-", hhld)
  ) %>%
  ungroup()
