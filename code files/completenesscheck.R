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

#REMOVING COMMAS IN UNNECESSARY SECTIONS

section1a <- section1a %>%
  mutate(across(where(~ any(grepl(",", .))), ~ sub(",.*", "", .)))

section5 <- section5 %>%
  mutate(across(where(~ any(grepl(",", .))), ~ sub(",.*", "", .)))

section7 <- section7 %>%
  mutate(across(
    .cols = -c(v709a, v710b, v714a, v716),  
    .fns = ~ ifelse(grepl(",", .), sub(",.*", "", .), .)
  ))

section8 <- section8 %>%
  mutate(across(
    .cols = -c(v803, v803b),  
    .fns = ~ ifelse(grepl(",", .), sub(",.*", "", .), .)
  ))

section9a <- section9a %>%
  mutate(across(
    .cols = -c(v902b), 
    .fns = ~ ifelse(grepl(",", .), sub(",.*", "", .), .)
  ))

#MAKING HHLD UNIQUE ACROSS THE ENTIRE DATABASE 

section0 <- section0 %>%
  group_by(psu) %>%
  mutate(
    hhld = row_number(),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  ungroup()

sections <- list(
  section0, section1a, section1b, section2a1, section2a2, section2a3, section2b, section2c,
  section3a, section3b, section4a, section4b, section4c, section4d,
  section5, section6a, section6b1, section6b2, section6b3, section6b4,
  section6b5, section6c1, section6c2, section6c3, section6c4,
  section6d, section7, section8, section9a, section9b, section9c,
  section9d, section9e, section9f1, section9f2, section10,
  section11a, section11b, section11c, section12a, section12b,
  section13a, section13b, section13c
)


names(sections) <- c(
  "section0", "section1a", "section1b", "section2a1", "section2a2", "section2a3", "section2b", "section2c",
  "section3a", "section3b", "section4a", "section4b", "section4c", "section4d",
  "section5", "section6a", "section6b1", "section6b2", "section6b3", "section6b4",
  "section6b5", "section6c1", "section6c2", "section6c3", "section6c4",
  "section6d", "section7", "section8", "section9a", "section9b", "section9c",
  "section9d", "section9e", "section9f1", "section9f2", "section10",
  "section11a", "section11b", "section11c", "section12a", "section12b",
  "section13a", "section13b", "section13c"
)

list2env(sections, .GlobalEnv) 

#MAKING COMPLETENESS TABLE ACROSS SECTIONS PER HOUSEHOLD 

#ACTUAL NUMBER OF HOUSEHOLD MEMBERS 

hh_members <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hh_status = case_when(
      v109 %in% c(1, 2) ~ "inside_hhmember",
      v109 %in% c(3, 4) ~ "outside_hhmember"
    )
  ) %>%
  group_by(hhid, hh_status) %>%
  summarise(
    members = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = hh_status,
    values_from = members,
    values_fill = 0,
    names_prefix = ""
  )

hh_members <- hh_members %>%
  mutate(total_members = inside_hhmember + outside_hhmember)

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 1.2

hhmembers_s1b <- section1b %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  ) %>%
  group_by(hhid) %>%
  summarise(
    hh_members_s1b = n()
  ) %>%
  ungroup()

hh_members <- merge(
  hh_members, 
  hhmembers_s1b, 
  by = "hhid", 
  all = TRUE
)

hh_members <- merge(
  hh_members, 
  section0[, c("ID", "hhid")],
  by = "hhid", 
  all = TRUE
)

write.xlsx(hh_members, "completeness checks/hh_members.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 5

s5_qualified <- section1b %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu,"-", hhld)
  ) %>%
  filter(
    trimws(v115) == 3
  ) 
  
section1a_v109 <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld, "-"),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  filter(v109 == 1) 

s5_qualified <- merge(
  s5_qualified, 
  section1a_v109[, c("uniq_id", "v109")],
  by = "uniq_id",
  all = FALSE
)

s5_qualified <- s5_qualified %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s5 <- section5 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s5 <- merge(
  hhmembers_s5, 
  s5_qualified, 
  by = "hhid", 
  all = TRUE
)

hhmembers_s5 <- merge(
  hhmembers_s5, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s5, "completeness checks/section5_discrepency.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.1 

s6_qualified <- section1a %>% 
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v104a = as.numeric(v104a)
  ) %>%
  filter(
    v109 %in% c(1, 2),
    v104a >= 5
  ) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  )

hhmembers_s6a <- section6a %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6a <- merge(
  hhmembers_s6a, 
  s6_qualified, 
  by = "hhid", 
  all = TRUE
)

hhmembers_s6a <- merge(
  hhmembers_s6a, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6a, "completeness checks/section6a_discrepency.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.2.1

s6b1_qualified <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld),
    v104a = as.numeric(v104a), 
    v109 = as.numeric(v109)
  ) %>%
  filter(
    v109 %in% c(1, 2)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6b1 <- section6b1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6b1 <- merge( 
  hhmembers_s6b1,
  s6b1_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6b1 <- merge(
  hhmembers_s6b1, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6b1, "completeness checks/section6b1_discrepency.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.2.2

s6b2_qualified <- section6b1 %>%
  mutate(
    v603 = as.integer(v603),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  filter(v603 == "1"
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6b2 <- section6b2 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6b2 <- merge(
  hhmembers_s6b2,
  s6b2_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6b2 <- merge(
  hhmembers_s6b2, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6b2, "completeness checks/section6b2_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.2.3

s6b3_qualified <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld),
    v603 = as.integer(v603)) %>%
  filter(
    v603 == 1, 
    v605a > 0
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6b3 <- section6b3 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    hh_members_s6b3 = n()
  ) %>%
  ungroup()

hhmembers_s6b3 <- merge(
  hhmembers_s6b3,
  s6b3_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6b3 <- merge(
  hhmembers_s6b3, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6b3, "completeness checks/section6b3_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.2.4

s6b4_qualified <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v605b = as.integer(v605b),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(trimws(v605b) > 0) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6b4 <- section6b4 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6b4 <- merge(
  hhmembers_s6b4,
  s6b4_qualified, 
  by = "hhid", 
  all = TRUE
)

hhmembers_s6b4 <- merge(
  hhmembers_s6b4, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6b4, "completeness checks/section6b4_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.3.1

s6c1_qualified <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    v109 = as.integer(v109),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(
    v109 %in% c(1, 2)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()
  

hhmembers_s6c1 <- section6c1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6c1 <- merge(
  hhmembers_s6c1,
  s6c1_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6c1 <- merge(
  hhmembers_s6c1, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6c1, "completeness checks/section6c1_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.3.2

s6c2_qualified <- section6c1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v629 = as.integer(v629),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(v629 == "1"
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6c2 <- section6c2 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6c2 <- merge(
  hhmembers_s6c2,
  s6c2_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6c2 <- merge(
  hhmembers_s6c2, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6c2, "completeness checks/section6c2_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.3.3

s6c3_qualified <- section6c1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v629 = as.integer(v629),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(v629 == "1"
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6c3 <- section6c3 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6c3 <- merge( 
  hhmembers_s6c3,
  s6c3_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6c3 <- merge(
  hhmembers_s6c3, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6c3, "completeness checks/section6c3_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 6.3.4

s6c4_qualified <- section6c1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v629 = as.integer(v629),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(v629 == "1"
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    qualified_members = n()
  ) %>%
  ungroup()

hhmembers_s6c4 <- section6c4 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(
    actual_members = n()
  ) %>%
  ungroup()

hhmembers_s6c4 <- merge(
  hhmembers_s6c4,
  s6c4_qualified,
  by = "hhid", 
  all = TRUE
)

hhmembers_s6c4 <- merge(
  hhmembers_s6c4, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s6c4, "completeness checks/section6c4_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 7

s7_qualified <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v104a = as.integer(v104a),
    v109 = as.integer(v109),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(
    v104a >= 10, 
    v109 %in% c(1, 2)
  ) %>%
  group_by(hhid) %>%
  summarise(qualified_members = n()) %>%
  ungroup()

hhmembers_s7 <- section7 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(actual_members = n()) %>%
  ungroup() 

hhmembers_s7 <- merge(
  hhmembers_s7, 
  s7_qualified, 
  by = "hhid", 
  all = TRUE
)  

hhmembers_s7 <- merge(
  hhmembers_s7, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s7, "completeness checks/section7_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 8 

s8_qualified <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v104a = as.integer(v104a),
    v109 = as.integer(v109),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  filter(
    v104a >= 10, 
    v109 %in% c(1, 2)
  ) %>%
  group_by(hhid) %>%
  summarise(qualified_members = n()) %>%
  ungroup()

hhmembers_s8 <- section8 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
  ) %>%
  distinct(uniq_id, .keep_all = TRUE) %>%
  group_by(hhid) %>%
  summarise(actual_members = n()) %>%
  ungroup() 

hhmembers_s8 <- merge(
  hhmembers_s8, 
  s8_qualified, 
  by = "hhid", 
  all = TRUE
)  

hhmembers_s8 <- merge(
  hhmembers_s8, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s8, "completeness checks/section8_discrepancy.xlsx")

#NUMBER OF HOUSEHOLD MEMBERS IN SECTION 12.1

s12a_qualified <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    v104a = as.integer(v104a),
    v109 = as.integer(v109)
  ) %>%
  filter(
    v104a > 10, 
    v109 %in% c(3, 4)
  ) %>%
  group_by(hhid) %>%
  summarise(qualified_members = n()) %>%
  ungroup()

hhmembers_s12a <- section12a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
  ) %>%
  group_by(hhid) %>%
  summarise(actual_members = n()) %>%
  ungroup() 
 
hhmembers_s12a <- merge(
  hhmembers_s12a, 
  s12a_qualified, 
  by = "hhid", 
  all = TRUE
)  

hhmembers_s12a <- merge(
  hhmembers_s12a, 
  section0[, c("hhid", "ID")], 
  by = "hhid", 
  all = FALSE
)

write.xlsx(hhmembers_s12a, "completeness checks/section12a_discrepancy.xlsx")


