#####ENUMERATOR NO 12

library(dplyr)
library(readxl)
library(purrr)

# List of all your datasets
sections <- list(
  section0, section1a, section1b, section2a1, section2a2, section2a3, section2b, section2c,
  section3a, section3b, section4a, section4b, section4c, section4d, section5, section6a,
  section6b1, section6b2, section6b3, section6b4, section6b5, section6c1, section6c2,
  section6c3, section6c4, section6d, section7, section8, section9a, section9b, section9c,
  section9d, section9e, section10, section11a, section11b, section11c,
  section12a, section12b, section13a, section13b, section13c
)

# Names of datasets
section_names <- c(
  "section0", "section1a", "section1b", "section2a1", "section2a2", "section2a3", "section2b", "section2c",
  "section3a", "section3b", "section4a", "section4b", "section4c", "section4d", "section5", "section6a",
  "section6b1", "section6b2", "section6b3", "section6b4", "section6b5", "section6c1", "section6c2",
  "section6c3", "section6c4", "section6d", "section7", "section8", "section9a", "section9b", "section9c",
  "section9d", "section9e", "section10", "section11a", "section11b", "section11c",
  "section12a", "section12b", "section13a", "section13b", "section13c"
)

# Filter all sections for interviewer_id == 12
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

