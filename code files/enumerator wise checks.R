library(dplyr)
library(readxl)
library(purrr)

sections <- list(
  section0, section1a, section1b, section2a1, section2a2, section2a3, section2b, section2c,
  section3a, section3b, section4a, section4b, section4c, section4d, section5, section6a,
  section6b1, section6b2, section6b3, section6b4, section6b5, section6c1, section6c2,
  section6c3, section6c4, section6d, section7, section8, section9a, section9b, section9c,
  section9d, section9e, section10, section11a, section11b, section11c,
  section12a, section12b, section13a, section13b, section13c
)

section_names <- c(
  "section0", "section1a", "section1b", "section2a1", "section2a2", "section2a3", "section2b", "section2c",
  "section3a", "section3b", "section4a", "section4b", "section4c", "section4d", "section5", "section6a",
  "section6b1", "section6b2", "section6b3", "section6b4", "section6b5", "section6c1", "section6c2",
  "section6c3", "section6c4", "section6d", "section7", "section8", "section9a", "section9b", "section9c",
  "section9d", "section9e", "section10", "section11a", "section11b", "section11c",
  "section12a", "section12b", "section13a", "section13b", "section13c"
)

sections_25 <- map(sections, ~ filter(.x, interviewer_id == 25))

# Optionally, assign back to individual variables
names(sections_25) <- paste0(section_names, "_25")
list2env(sections_25, envir = .GlobalEnv)

#CHECKING FOR AGE <= 5 IN SECTION 6.1

age_mishap <- section1a_25 %>%
  inner_join(section6a_25, by = "personid") %>%
  mutate(v104a = as.integer(v104a)) %>%
  select(ID = ID.x, palika = palika.x, ward = ward.x,
         hhld = hhld.x, version = version.x, verified = verified.x,
         v101 = v101.x, v104a, v601) %>%
  filter(is.na(v104a) | v104a <= 5) 

sec4_healthex <- section4a_25 %>%
  filter(v401 == 22)

health_expenditure <- section2b_25 %>%
  inner_join(section6d_25, by = "ID") %>%
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
    v662 = as.integer(v662),
    health_expense_ratio = v249 / v662
  )


outside_household <- section1a_25 %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(v109 %in% c(3, 4)) %>%
  select(personid, v109, v101, ID, interviewer_id)

outside_household_25 <- merge.data.frame(section12a_25, outside_household, by.x = "personid", by.y = "personid", all = FALSE)

outside_household_25 <- outside_household_25 %>%
  select(personid, ID.x, psu, palika, ward, hhld, version, verified, v101, v1201, v1202, v109)

outside_household <- outside_household %>%
  anti_join(outside_household_25, by = "personid")

write.csv(outside_household, "remmitancemissing_pasang.csv")

section6b1_25 <- section6b1_25 %>%
  select(ID, personid, v603)

section6c1_25 <- section6c1_25 %>%
  select(ID, personid, v629)

section6_merged_25 <- merge.data.frame(section6b1_25, section6c1_25, by.x = "personid", by.y = "personid", all = TRUE)
  
section6_merged_25 <- section6_merged_25 %>%
  group_by(ID.x) %>%
  summarise(count = sum(v603 == 2 & v629 == 2, na.rm = TRUE)) %>%
  ungroup()

#ENUMERATOR AND ENROLLMENT WISE HOUSEHOLDS

section0 <- section0 %>%
  mutate(
    supervisor_name = case_when(
      Name.of.enumerator %in% c("SHEKHAR SHRESTHA", "PASANG SHERPA", "DEVIKA ACHARYA", "PREEJA BASNET") ~ "SHEKHAR SHRESTHA",
      Name.of.enumerator %in% c("MINA MAYA PAKHRIN", "KESA MAYA DAHAL", "ASMITA WAIBA", "RANJANA KUMARI MAINALI") ~ "MINA MAYA PAKHRIN",
      Name.of.enumerator %in% c("PRAMILA ACHARYA", "ADITI KUMARI SHRESTHA", "NEELIMA UPADHYAY", "RAM SHANKER SHAH") ~ "PRAMILA ACHARYA",
      Name.of.enumerator %in% c("KAMALA SHARMA", "ISHWARI SHRESTHA", "SALINA ADHIKARI", "RUMA LINGTHEP", "KAMAL TIMSINA") ~ "KAMALA SHARMA",
      Name.of.enumerator %in% c("MANOJ KUMAR BHATTARAI", "AKSHYA ACHARYA", "SWECHCHHA RANJIT", "MANJU KC") ~ "MANOJ KUMAR BHATTARAI",
      Name.of.enumerator %in% c("BADRI PRASAD MAINALI", "GIRIRAJ ADHIKARI", "BARSHA THAPA MAGAR", "AMBIKA PANDEY") ~ "BADRI PRASAD MAINALI",
      Name.of.enumerator %in% c("ASMITA DAHAL", "GITA DAHAL", "KRITI PAUDEL", "SAURI GHIMIRE", "SHANKAR PRASAD NEUPANE") ~ "ASMITA DAHAL",
      Name.of.enumerator %in% c("TIKA RAM ACHARYA", "SANTANA GAUTAM", "SARITA CHHETRI", "SUJATA NEPAL") ~ "TIKA RAM ACHARYA",
      Name.of.enumerator %in% c("NETRA RAJ DHITAL", "ELIJA SHAHI", "HARIHAR JOSHI", "POOJA RAWAT") ~ "NETRA RAJ DHITAL",
      Name.of.enumerator %in% c("SIRAJ POKHREL", "SUSHAN SHRESTHA", "URMILA PARAJULI", "SUSHMITA PATHAK") ~ "SIRAJ POKHREL",
      TRUE ~ "UNASSIGNED"
    ), 
    type_of_enrollment = case_when(
      enrollment == 1 ~ "NHIP",
      enrollment == 2 ~ "Non_NHIP", 
      enrollment == 3 ~ "SSF", 
      enrollment == 4 ~ "Non_SSF"
    )
  )

enumerator_wise <- section0 %>%
  group_by(Name.of.enumerator, type_of_enrollment) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = type_of_enrollment,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(total = NHIP + Non_NHIP + SSF + Non_SSF)

#ACUTE DISEASES YES/NO HOUSEHOLDS PER ENUMERATORS

chronic <- section6b1 %>%
  filter(v603 == 1) %>%
  group_by(uid) %>%
  slice(1) %>%
  ungroup()

chronic <- merge(
  chronic,
  section0[, c("uid", "Name.of.enumerator")],
  by = "uid",
  all.x = TRUE
)  

chronic <- chronic %>%
  group_by(Name.of.enumerator) %>%
  summarise(chronic_hh = n())

enumerator_wise <- merge.data.frame(enumerator_wise, chronic, by.x = "Name.of.enumerator", by.y = "Name.of.enumerator")

#CHRONIC DISEASED YES/NO HOUSEHOLDS PER ENUMERATORS

acute <- section6c1 %>%
  filter(v629 == 1) %>%
  group_by(uid) %>%
  slice(1) %>%
  ungroup()

acute <- merge(
  acute,
  section0[, c("uid", "Name.of.enumerator")],
  by = "uid",
  all.x = TRUE
)  

acute <- acute %>%
  group_by(Name.of.enumerator) %>%
  summarise(acute_hh = n())

enumerator_wise <- merge.data.frame(enumerator_wise, acute, by.x = "Name.of.enumerator", by.y = "Name.of.enumerator")

#LANDS SECTION FILLED HOUSEHOLDS PER ENUMERATORS 

land_holding <- section9a %>%
  filter(v901 == 1) %>%
  group_by(uid) %>%
  slice(1) %>%
  ungroup()

land_holding <- merge(
  land_holding, 
  section0[, c("uid", "Name.of.enumerator")],
  by = "uid", 
  all.x = TRUE
)

land_holding <- land_holding %>%
  group_by(Name.of.enumerator) %>%
  summarise(landholding_hh = n())

enumerator_wise <- merge.data.frame(enumerator_wise, land_holding, by.x = "Name.of.enumerator", by.y = "Name.of.enumerator")

#SECTION 5 EDUCATION MISSING PER ENUMERATOR

section1b_education <- section1b %>%
  mutate(uniq_id = paste0(psu, "-", hhld, "-", v101)) %>%
  filter(
    trimws(v115) == 3
  )

section1b_education <- merge.data.frame(section1b_education, section1a_v109, by.x = "uniq_id", by.y = "uniq_id")

section1b_education <- section1b_education %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(v109 == 1)

section5 <- section5 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

section5_missing <- section1b_education %>%
  anti_join(section5, by = "uniq_id")

section5_missing <- merge(
  section5_missing, 
  section0[, c("uid", "Name.of.enumerator")], 
  by = "uid", 
  all.x = TRUE
)

education_missing <- section5_missing %>%
  group_by(Name.of.enumerator) %>%
  summarise(education_missing = n()) %>%
  ungroup()

enumerator_wise <- merge.data.frame(enumerator_wise, education_missing, by.x = "Name.of.enumerator", by.y = "Name.of.enumerator", all = TRUE)

#SECTION 6.2.1 MISSING PER ENUMERATOR 

section1a_chronic_qualified <- section1a %>%
  mutate(
    v109 = as.numeric(v109), 
    v104a = as.numeric(v104a)
  ) %>%
  filter(
    v109 %in% c(1, 2) & v104a > 5
  )

section6b1_missing <-  section1a_chronic_qualified %>%
  anti_join(section6b1, by = "personid")

section6b1_missing <- merge(
  section6b1_missing, 
  section0[, c("uid", "Name.of.enumerator")],
  by = "uid",
  all.x = TRUE
)

section6b1_missing_enum <- section6b1_missing %>%
  group_by(Name.of.enumerator) %>%
  summarise(chronic_missing = n()) %>%
  ungroup()

enumerator_wise <- merge.data.frame(enumerator_wise, section6b1_missing_enum, by.x = "Name.of.enumerator", by.y = "Name.of.enumerator", all = TRUE)

write.xlsx(enumerator_wise, "enumerator_wise.xlsx")
