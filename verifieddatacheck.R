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

#COMPLETENESS CHECKS

#CHECKING WHETHER THERE ARE ALL HOUSEHOLD RESIDENTS IN SECTION1B

section1a_v109 <- section1a %>%
  filter(verified == "Y") %>%
  select(personid, v109)
  
section1a[duplicated(section1a$personid), "personid"]

section1a_hhresident <- section1a %>%
  filter(
    trimws(v109) %in% c(1, 2), 
    verified == "Y"  
  ) 

section1b[duplicated(section1b$personid), "personid"]

section1b_hhstatus <- merge.data.frame(section1b, section1a_v109, by.x = "personid", by.y = "personid", all = FALSE)

section1b_miscategorized <- section1b_hhstatus %>%
  filter(trimws(v109) %in% c(3, 4),
         verified == "Y"
)

#CHECKING FOR EDUCATION CONSISTENCY

section1b_education <- section1b %>%
  mutate(v115 = as.integer(v115)) %>%
  filter(
    v115 == 3, 
    verified == "Y"
  )

section1b_education[duplicated(section1b_education$personid), "personid"]

section5 <- section5 %>%
  filter(verified == "Y")

section5[duplicated(section5$personid), "personid"]

section5_missing <- section1b_education %>%
  anti_join(section5, by = "personid")

#CHECKING FOR AGE <= 5 IN SECTION 6.1

age_mishap_6.1 <- section1a %>%
  inner_join(section6a, by = "personid") %>%
  mutate(v104a = as.integer(v104a)) %>%
  select(ID = ID.x, palika = palika.x, ward = ward.x,
         hhld = hhld.x, version = version.x, verified = verified.x,
         v101 = v101.x, v104a, v601) %>%
  filter(is.na(v104a) | v104a <= 5) 

write.csv(age_mishap_6.1, "section6.1_lessthan5.csv")

#CHECKING FOR ALL PEOPLE IN SECTION 6.2.1

section1a_chronic_qualified <- section1a %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(
    v109 %in% c(1, 2),
    verified == "Y"
)

section6b1_verified <- section6b1 %>%
  filter(
    verified == "Y"
  )

section6b1_missing <-  section1a_chronic_qualified %>%
  anti_join(section6b1_verified, by = "personid")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.2.2

section6b1_chronic_checks <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  filter(v603 == "1",
         verified == "Y" 
  )

section6b1_chronic_checks[duplicated(section6b1_chronic_checks$personid), "personid"]

section6b2_verified <- section6b2 %>%
  filter(verified == "Y")

section6b2_qualified_missing <- section6b1_chronic_checks %>%
  anti_join(section6b2_verified, by = "personid")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.2.3

section6b1_chronic_inpatients <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  filter(
    v603 == 1, 
    v605a > 0,
    verified == "Y"
  )

section6b3_verified <- section6b3 %>%
  filter(verified == "Y")

section6b3_qualified_missing <- section6b1_chronic_inpatients %>%
  anti_join(section6b3_verified, by = "personid")
