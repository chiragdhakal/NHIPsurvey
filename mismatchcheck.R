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

#MAKING PSU AND HHID UNIQUE 

psu_counts <- section0 %>%
  group_by(psu) %>%
  summarise(n_hhlds = n()) %>%
  ungroup()

section0 <- section0 %>%
  mutate(
  enrollment = as.integer(enrollment),
  psu = case_when(
    enrollment == 2 & psu %in% c(
      1101:1112, 2101:2110, 3101:3115, 4101:4109, 5101:5111, 6101:6108, 7101:7109
    ) ~ psu + 100, 
    enrollment == 1 & psu %in% c(
      1201:1212, 2201:2210, 3201:3215, 4201:4209, 5201:5211, 6201:6208, 7201:7209
    ) ~ psu - 100,
    TRUE ~ psu
  )
  ) %>%
  group_by(psu) %>%
  mutate(
    hhld = row_number(),
    uniq_id = paste0(psu, "-", hhld), 
    uniq_id1 = paste0(ID, "-", enrollment, "-", palika),
  ) %>% 
  ungroup()

psu_counts <- section0 %>%
  group_by(psu) %>%
  summarise(n_hhlds = n()) %>%
  ungroup()

any(duplicated(section0$uniq_id))
section0[duplicated(section0$uniq_id), "uniq_id"]
section0[duplicated(section0$uniq_id), "uniq_id1"]

#CHECKING FOR ANY MISALLOCATED PSU 
psu_counts1 <- section0 %>%
  group_by(psu) %>%
  summarise(n_hhlds = n()) %>%
  ungroup()

psu_counts <- merge.data.frame(psu_counts, psu_counts1, by.x = "psu", by.y = "psu", all = TRUE)

section0_issues <- section0 %>%
  filter(psu %in% psu_issues$psu) %>%
  select(version, verified, ID, enrollment, psu, province, district, palika, ward, hhld, interview_date, Name.of.enumerator)

duplicates <- section0 %>%
  filter(duplicated(uniq_id) | duplicated(uniq_id, fromLast = TRUE)) %>%
  arrange(uniq_id)

duplicates <- duplicates %>% 
  select(uniq_id, ID, enrollment, psu, province, district, palika, ward, hhld, Name.of.enumerator, interview_date, version)

write_xlsx(duplicates, "duplicated_hhld.xlsx")
write_xlsx(psu_counts, "psu_counts.xlsx")
write_xlsx(section0_issues, "section0_issues.xlsx")

#CHECKING FOR PALIKA ERROR 



#CHECKING FOR CELLS WITH INCONSISTENT DATATYPE
keep_rows_with_commas <- function(df, skip_cols = NULL) {
  cols <- setdiff(names(df), skip_cols)
  df %>% 
  filter(if_any(all_of(cols), ~ grepl(",", .)))
}

section0_multi <- keep_rows_with_commas(
  section0, 
  skip_cols = NULL
)

section1b_multi <- keep_rows_with_commas(
  section1b, 
  skip_cols = c(6, 16, 17, 18, 22, 24, 26)
)

section2a1_multi <- keep_rows_with_commas(
  section2a1, 
  skip_cols = c(9, 11, 13, 15, 16)
)

section2a2_multi <- keep_rows_with_commas(
  section2a2, 
  skip_cols = c(12)
)

section2a3_multi <- keep_rows_with_commas(
  section2a3, 
  skip_cols = c(7, 10, 20, 29)
)

section2b_multi <- keep_rows_with_commas(
  section2b, 
  skip_cols = NULL 
)

section2c_multi <- keep_rows_with_commas(
  section2c, 
  skip_cols = c(9, 13, 15)
)

section3a_multi <- keep_rows_with_commas(
  section3a, 
  skip_cols = NULL  
)

section3b_multi <- keep_rows_with_commas(
  section3b, 
  skip_cols = NULL 
)

#CHECKING EDUCATION DATA CONSISTENCY
section1a_edu <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    uniq_id1 = paste0(ID, "-", v101),
    v109 = as.integer(v109) 
  ) %>%
  filter(v109 == 1) %>%
  filter(verified == "Y")
any(duplicated(section1a_edu$uniq_id))
any(duplicated(section1a_edu$uniq_id1))
section1a_edu[duplicated(section1a_edu$uniq_id), "uniq_id"]
section1a_edu[duplicated(section1a_edu$uniq_id1), "uniq_id1"]

  
section1b_edu <- section1b %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    uniq_id1 = paste0(ID, "-", v101),
    v115 = as.integer(v115) 
  ) %>%
  filter(v115 == 3) %>%
  filter(verified == "Y") %>%
  filter(uniq_id1 %in% section1a_edu$uniq_id1)
any(duplicated(section1b_edu$uniq_id))
any(duplicated(section1b_edu$uniq_id1))
section1b_edu[duplicated(section1b_edu$uniq_id), "uniq_id"]
section1b_edu[duplicated(section1b_edu$uniq_id1), "uniq_id1"]


edu_running <- merge.data.frame(
  section1a_edu, section1b_edu,
  by.x = "uniq_id1", 
  by.y = "uniq_id1", 
  all = FALSE
)

section5 <- section5 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    uniq_id1 = paste0(ID, "-", v101)
  ) %>% 
  filter(verified == "Y")
any(duplicated(section5$uniq_id))
any(duplicated(section5$uniq_id1))
section5[duplicated(section5$uniq_id), "uniq_id"]
section5[duplicated(section5$uniq_id1), "uniq_id1"]

edu_consistent <- merge.data.frame(
  edu_running, section5,
  by.x = "uniq_id1",
  by.y = "uniq_id1"
)

missing_ids <- anti_join(
  edu_running, education_consistent,
  by = "uniq_id1"
)

missing_ids$uniq_id1

nrow(section1b)       
nrow(education_consistent)  



#REMITTANCE CONSISTENCY CHECK
section1a_remit <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    uniq_id1 = paste0(ID, "-", v101),
    v109 = as.integer(v109) 
  ) %>%
  filter(v109 == 4) %>%
  filter(verified == "Y")
any(duplicated(section1a_remit$uniq_id1))
section1a_remit[duplicated(section1a_remit$uniq_id1), "uniq_id1"]

section12a <- section12a %>%
  mutate(
    uniq_id1 = paste0(ID, "-", v1201)
  ) %>%
  filter(verified == "Y") 
any(duplicated(section12a$uniq_id1))
section12a[duplicated(section12a$uniq_id1), "uniq_id1"]

remit_consistent <- merge.data.frame(
  section1a_remit, section12a, 
  by.x = "personid",
  by.y = "personid"
)

missing_remit <- anti_join(
  section1a_remit, remit_consistent,
  by = "personid"
)


#HEALTH EXPENDITURE AND OUT OF POCKET EXPENDITURE RATIO 
section2b_hexpense <- section2b %>%
  mutate(
    v249 = as.double(v249)
  ) %>%
  filter(verified == "Y")
  #filter(!is.na(v249))

section6d_hexpense <- section6d %>%
  mutate(
    v662 = as.double(v662)
  ) %>%
  filter(verified == "Y") 
  #filter(!is.na(v662))

healthexp_oop <- merge.data.frame(
  section2b_hexpense, section6d_hexpense, 
  by.x = "ID",
  by.y = "ID",
  all = FALSE
)

healthexp_oop_inconsistent <- healthexp_oop %>%
  filter( (is.na(v662) & !is.na(v249)) | (is.na(v249) & !is.na(v662)) ) %>%
  mutate(
    missing_case = case_when(
      is.na(v662) & !is.na(v249) ~ "v662 missing",
      is.na(v249) & !is.na(v662) ~ "v249 missing"
    )
  ) %>% 
  select(ID, v249, v662)

healthexp_oop <- healthexp_oop %>%
  mutate(
    ratio1 = v249 / v662,
    ratio2 = v662 / v249
  )
sum(healthexp_oop$ratio1 > 1, na.rm = TRUE)
sum(healthexp_oop$ratio2 > 1, na.rm = TRUE)
healthexp_oop$uniq_id[!is.na(healthexp_oop$ratio1) & healthexp_oop$ratio1 == 1]

sum(healthexp_oop$)

healthexp_oop_inconsistent <- healthexp_oop %>%
  filter( (is.na(v662) & !is.na(v249)) | (is.na(v249) & !is.na(v662)) ) %>%
  mutate(
    missing_case = case_when(
      is.na(v662) & !is.na(v249) ~ "v662 missing",
      is.na(v249) & !is.na(v662) ~ "v249 missing"
    )
  )

#CHECKING MISMATCHES WHILE SELECTING HOUSEHOLDS 

section0_mismatch <- merge.data.frame(section0, section2b,
by.x = "ID",
by.y = "ID",
all = FALSE)

section0_mismatch <- section0_mismatch %>%
  select(ID, psu.x, enrollment, province, district, palika.x, ward.x, hhld.x, v228, v229)

section0_mismatch <- section0_mismatch %>% 
  mutate(
    enrollment = as.integer(enrollment),
    v228 = as.integer(v228),
    v229 = as.integer(v229)
  ) %>%
  filter(
    (enrollment %in% c(1, 3) & v228 == 2) |
    (enrollment %in% c(2, 4) & v228 == 1 & v229 %in% c(1, 2)) 
  )

section0_mismatch <- section0_mismatch %>%
  filter(is.na(v228)) %>%
  mutate(enrollment = as.integer(enrollment)) %>%
  filter(enrollment == 1)
  
#CHECKING FOR NUMBER OF ACUTE AND CHRONIC ILLNESS IN THE SAMPLE

section6b1 <- read.xlsx("dataset/Part 6_2_1_ Chronic Illness and Health Seeking Behaviour.xlsx")
section6c1 <- read.xlsx("dataset/Part 6_3_1_ Acute Illness and health seeking behaviour.xlsx")

sum(is.na(section6b1$v603))
sum(is.na(section6c1$v629))

chronic_na <- section6b1 %>%
  filter(is.na(v603)) 

write.csv(chronic_na, "v603_na.csv")

acute_na <- section6c1 %>%
  filter(is.na(v629))

write.csv(acute_na, "v629_na.csv")


section6b1 <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  filter(!is.na(v603))

section6c1 <- section6c1 %>%
  mutate(v629 = as.integer(v629)) %>%
  filter(!is.na(v629))

sum(section6b1$v603 == 1, na.rm = TRUE)
sum(section6b1$v603 == 2, na.rm = TRUE)
sum(section6c1$v629 == 1, na.rm = TRUE)
sum(section6c1$v629 == 2, na.rm = TRUE)

#HOUSEHOLDS WITH ACUTE ILLNESS
acute <- section6b1 %>%
  group_by(ID) %>%
  filter(all(v603 == 1)) %>%
  distinct(ID)


#HOUSEHOLDS WITH CHRONIC ILLNESS
chronic <- section6c1 %>%
  group_by(ID) %>%
  filter(all(v629 == 1)) %>%
  distinct(ID)

#NO ACUTE ILLNESS HOUSEHOLDS
no_acute <- section6b1 %>%
  group_by(ID) %>%
  filter(all(v603 == 2)) %>%
  distinct(ID)

#NO CHRONIC ILLNESS HOUSEHOLDS
no_chronic <- section6c1 %>%
  group_by(ID) %>%
  filter(all(v629 == 2)) %>%
  distinct(ID)

#HOUSEHOLDS WITH NO CHRONIC AND NO ACUTE ILLNESSES
common_id <- intersect(acute$ID, chronic$ID)

common_id

#CHECKING FOR DUPLICATES IN ACUTE DISEASE LIST
acute_duplicates <- section6b1 %>%
  filter(duplicated(paste(personid, v604)) | duplicated(paste(personid, v604), fromLast = TRUE)) %>%
  select(ID, psu, palika, ward, hhld, version, verified, interviewer_id, v101, v603, personid, v604) 

write.csv(acute_duplicates, "acute_duplicates.csv")


#CHECKING FOR DUPLICATES IN 
chronic_duplicates <- section6c1 %>%
  filter(duplicated(paste(personid, v630)) | duplicated(paste(personid, v630), fromLast = TRUE)) %>%
  select(ID, psu, palika, ward, hhld, version, verified, interviewer_id, v101, v629, personid, v630)

write.csv(chronic_duplicates, "chronic_duplicates.csv")







