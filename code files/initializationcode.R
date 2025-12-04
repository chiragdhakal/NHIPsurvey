if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)

#SECTION0

section0 <- read.xlsx("dataset/cover page.xlsx")

#labelling section-0

section0 <- section0 %>%
  rename(
    nhip_enrollment_date = Health.Insurence.Program,
    ssf_enrollment_date = Social.Health.Insurence,
    employer_name = name
  ) %>%
  mutate(
  ID = labelled(
    ID, 
    label = "system id code"
  ), 
  enrollment = labelled(
    enrollment, 
    label = "type of enrollment"
  ), 
  psu = labelled(
    psu, 
    label = "psu number"
  ), 
  province = labelled(
    province, 
    label = "province"
  ), 
  district = labelled(
    district, 
    label = "district"
  ), 
  palika = labelled(
    palika, 
    label = "local level"
  ), 
  ward = labelled(
    ward, 
    label = "ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "household number"
  ), 
  latitude = labelled(
    latitude, 
    label = "latitude"
  ), 
  longitude = labelled(
    longitude, 
    label = "longitude"
  ), 
  elevation = labelled(
    elevation, 
    label = "elevation"
  ), 
  device_id = labelled(
    device_id, 
    label = "device id"
  ), 
  consent = labelled(
    consent, 
    label = "informed consent"
  ), 
  respondent = labelled(
    respondent, 
    label = "name of respondent"
  ), 
  signature_res = labelled(
    signature_res, 
    label = "signature of respondent"
  ), 
  signature_date_res = labelled(
    signature_date_res, 
    label = "respondent's date of signature"
  ), 
  interviewer_id = labelled(
    interviewer_id, 
    label = "id code of interviewer"
  ), 
  signature_enu = labelled(
    signature_enu, 
    label = "signature of enumerator"
  ), 
  signature_date_enu = labelled(
    signature_date_enu, 
    label = "enumerator's date of signature"
  ), 
  nhip_enrollment_date = labelled(
    nhip_enrollment_date, 
    label = "date of enrollment - nhip"
  ), 
  ssf_enrollment_date = labelled(
    ssf_enrollment_date, 
    label = "date of enrollment - ssf"
  ), 
  nhip_target_group = labelled(
    nhip_target_group, 
    label = "nhip targeted family"
  ),
  interview_date = labelled(
    interview_date, 
    label = "Date of interview"
  ),
  interview_result = labelled(
    interview_result, 
    label = "Result of interview"
  ),
  village = labelled(
    village, 
    label = "name of respondent's village"
  ), 
  phone = labelled(
    phone, 
    label = "contact number of respondent"
  ), 
  hhld_head = labelled(
    hhld_head, 
    label = "is the respondent hh head?"
  ), 
  stay_duration = labelled(
    stay_duration, 
    label = "how long have you been residing in this locality"
  ), 
  hhld_member_t = labelled(
    hhld_member_t, 
    label = "usual resident: total"
  ), 
  hhld_member_m = labelled(
    hhld_member_m, 
    label = "usual resident: male"
  ), 
  hhld_member_f = labelled(
    hhld_member_f, 
    label = "usual resident: female"
  ), 
  hhld_member_o = labelled(
    hhld_member_o, 
    label = "usual resident: other"
  ), 
  absentee_country_t = labelled(
    absentee_country_t, 
    label = "absentee within the country: total"
  ), 
  absentee_country_m = labelled(
    absentee_country_m, 
    label = "absentee within the country: male"
  ), 
  absentee_country_f = labelled(
    absentee_country_f, 
    label = "absentee within the country: female"
  ), 
  absentee_country_o = labelled(
    absentee_country_o, 
    label = "absentee within the country: other"
  ), 
  absentee_abroad_t = labelled(
    absentee_abroad_t, 
    label = "absentee abroad: total"
  ), 
  absentee_abroad_m = labelled(
    absentee_abroad_m, 
    label = "absentee abroad: male"
  ), 
  absentee_abroad_f = labelled(
    absentee_abroad_f, 
    label = "absentee abroad: female"
  ), 
  absentee_abroad_o = labelled(
    absentee_abroad_o, 
    label = "absentee abroad: other"
  ), 
  returnee_migrant_t = labelled(
    returnee_migrant_t, 
    label = "returnee migrants (≤ 12 months): total"
  ), 
  returnee_migrant_m = labelled(
    returnee_migrant_m, 
    label = "returnee migrants (≤ 12 months): male"
  ), 
  returnee_migrant_f = labelled(
    returnee_migrant_f, 
    label = "returnee migrants (≤ 12 months): female"
  ), 
  returnee_migrant_o = labelled(
    returnee_migrant_o, 
    label = "returnee migrants (≤ 12 months): other"
  ), 
  deceased_persons_t = labelled(
    deceased_persons_t, 
    label = "deceased persons (≤ 12 months): total"
  ), 
  deceased_m = labelled(
    deceased_m, 
    label = "deceased persons (≤ 12 months): male"
  ), 
  deceased_f = labelled(
    deceased_f, 
    label = "deceased persons (≤ 12 months): female"
  ), 
  deceased_o = labelled(
    deceased_o, 
    label = "deceased persons (≤ 12 months): other"
  ), 
  designation = labelled(
    designation, 
    label = "designation of the respondent"
  ), 
  employer_name = labelled(
    employer_name, 
    label = "name of the employer"
  ), 
  employer_sector = labelled(
    employer_sector, 
    label = "sector of the employer"
  ), 
  employer_size = labelled(
    employer_size, 
    label = "size of the employer"
  ), 
  address_province = labelled(
    address_province, 
    label = "address of the employee: province"
  ), 
  address_district = labelled(
    address_district, 
    label = "address of the employee: district"
  ), 
  address_palika = labelled(
    address_palika, 
    label = "address of the employee: local level"
  ), 
  address_ward = labelled(
    address_ward, 
    label = "address of the employee: ward number"
  )
)


#SECTION 1 - HOUSEHOLD ROSTER

section1a <- read.xlsx("dataset/section 1.xlsx")
section1b <- read.xlsx("dataset/Part 1_1 Household Roster-1.xlsx")

#Section1a

section1a <- section1a %>%
  mutate(
    v105 = case_when(
      grepl("KUMHAL", v105, ignore.case = TRUE) ~ "3",
      grepl("NEWAR|SHRESTHA|THARU|SIMANTRAKIT|SIMANTAKRIT", v105, ignore.case = TRUE) ~ "2",
      TRUE ~ v105
    ),
    v107 = as.numeric(gsub("[^0-9]", "", v107))
  ) %>% 
  mutate(
    ID = as.integer(ID),
    psu = as.integer(psu), 
    ward = as.integer(ward),
    hhld = as.integer(hhld),
    v101 = as.integer(v101),
    v103 = as.integer(v103), 
    v104a = as.integer(v104a),
    v105 = as.integer(v105), 
    v106 = as.integer(v106),
    v107 = as.integer(v107),
    v108 = as.integer(v108),
    v109 = as.integer(v109), 
    v110 = as.integer(v110)
  ) %>% 
  mutate(
    psu = labelled(
      psu, 
      label = "primary sampling unit"
    ), 
    palika = labelled(
      palika, 
      label = "local level"
    ),
    ward = labelled(
      ward, 
      label = "ward number"
    ), 
    hhld = labelled(
      hhld, 
      label = "household number"
    ), 
    v101 = labelled(
      v101, 
      label = "identification code"
    ), 
    v102 = labelled(
      v102, 
      label = "name of the family member"
    ), 
    v103 = labelled(
      v103, 
      label = "sex", 
      labels = c(Male = 1, Female = 2, Others = 96)
    ), 
    v104a = labelled(
      v104a, 
      label = "age in years"
    ), 
    v104b = labelled(
      v104b,
      label = "age in months"
    ),
    v105 = labelled(
      v105, 
      label = "ethnicity", 
      labels = c(Aryan = 1, Janajati = 2, Madhesi = 3, Dalit = 4, Muslim = 5)
    ), 
    v106 = labelled(
      v106, 
      label = "religion", 
      labels = c(Hindu = 1, Buddhist = 2, Islam = 3, Kirat = 4, Christian = 5, Others = 6)
    ), 
    v106a = labelled(
      v106a, 
      label = "others (specify)"
    ),
    v107 = labelled(
      v107, 
      label = "what is your relationship with the household head?",
      labels = c(Householdhead = 1, Spouse = 2, Children = 3, Grandchildren = 4, Parents = 5, Siblings = 6, BhatijBhatiji = 7, JwaiBuhari = 8, DajuBhauju = 9, Inlaws = 10, Others = 11)
    ), 
    v108 = labelled(
      v108,
      label = "during the past 12 months, how many months did you live here?"
    ), 
    v109 = labelled(
      v109,
      label = "category of household member",
      labels = c(Familymember = 1, AbroadReturnee = 2, AbsentNepal = 3, AbsentForeign = 4)
    ), 
    v110 = labelled(
      v110,
      label = "what is your current marital status?",
      labels = c(Unmarried = 1, Married = 2, Separated = 3, Divorced = 4, Widowed = 5)
    )
  )


#Section1b

for (i in setdiff(1:ncol(section1b), c(6, 7, 20, 21, 22))) {
  section1b[[i]] <- as.integer(section1b[[i]])
}

section1b <- section1b %>%
  mutate(
    psu = labelled(
      psu, 
      label = "primary sampling unit"
    ), 
    palika = labelled(
      palika, 
      label = "local level"
    ),
    ward = labelled(
      ward, 
      label = "ward number"
    ), 
    hhld = labelled(
      hhld, 
      label = "household number"
    ), 
    v101 = labelled(
      v101, 
      label = "identification code"
    ), 
    v111 = labelled(
      v111,
      label = "is currently covered by any type of health insurance?", 
      labels = c(Yes = 1, No = 2)
    ),
    v112a = labelled(
      v112a, 
      label = "Health Insurance Board (HIB)",
      labels = c(Yes = 1, No = 0)
    ),
    v112b = labelled(
      v112b, 
      label = "Social Security Fund (SSF)",
      labels = c(Yes = 1, No = 0)
    ), 
    v112c = labelled(
      v112c, 
      label = "Insured through employer", 
      labels = c(Yes = 1, No = 0)
    ),
    v112d = labelled(
      v112d, 
      label = "Private Insurance", 
      labels = c(Yes = 1, No = 0)
    ),
    v112e = labelled(
      v112e, 
      label = "Insured through BFIs", 
      labels = c(Yes = 1, No = 0)
    ),
    v112f = labelled(
      v112f, 
      label = "Hospital/Health institution insurance", 
      labels = c(Yes = 1, No = 0)
    ),
    v112g = labelled(
      v112g, 
      label = "Community health insurance", 
      labels = c(Yes = 1, No = 0)
    ), 
    v112h = labelled(
      v112h, 
      label = "Others", 
      labels = c(Yes = 1, No = 0)
    ),
    v112h_1 = labelled(
      v112h_1, 
      label = "Other type of health insurance"
    ),
    v113a = labelled(
      v113a, 
      label = "Membership or ID number (health insurance board)"
    ),
    v113b = labelled(
      v113b, 
      label = "Membership or ID number (social security fund)"
    ),
    v114 = labelled(
      v114, 
      label = "Can the respondent read and write in any language?", 
      labels = c(Both =  1, "Read only" = 2, Neither = 3)
    ), 
    v115 = labelled(
      v115, 
      label = "Is respondent currently attending school/college?", 
      labels = c(Never = 1, "Used to" = 2, "Currently going" = 3)
    ),
    v116 = labelled(
      v116, 
      label = "What is the highest level of education respondent has completed?", 
      labels = c(Kindergarten = 0, "Class 1" = 1, "Class 2" = 2, "Class 3" = 3, "Class 4" = 4, "Class 5" = 5, "Class 6" = 6,
      "Class 7" = 7, "Class 8" = 8, "Class 9" = 9, "Class 10" = 10, "SEE/SLC" = 11, "+2 or equivalent" = 12, Bachelors = 13, 
      Masters = 14, PhD = 15, "Literate - Level less" = 16, Illiterate = 17)
    ),
    v117 = labelled(
      v117, 
      label = "Spouse's ID code"
    ), 
    v118 = labelled(
      v118, 
      label = "Does your father live in this household?", 
      labels = c(Yes = 1, No = 2, Death = 3)
    ), 
    v119 = labelled(
      v119, 
      label = "Father ID code"
    ), 
    v120 = labelled(
      v120, 
      label = "Does your mother live in this household?", 
      labels = c(Yes = 1, No = 2, Death = 3)
    ), 
    v121 = labelled(
      v121, 
      label = "Mother ID code"
    )
  )

#SECTION 2 (Household Characteristics)
section2a1 <- read.xlsx("dataset/Household Characteristics.xlsx")
section2a2 <- read.xlsx("dataset/Section 2_1 Housing Expenses.xlsx")
section2a3 <- read.xlsx("dataset/Utilities and Amenities.xlsx")
section2b <- read.xlsx("dataset/Section 2_2_ National health insurence.xlsx")
section2c <- read.xlsx("dataset/PART 2_3_ Mortality (Death) Information.xlsx")

#Part 2.1 - Housing

#Part 2.1.1 - Type of dwelling

for (i in setdiff(1:ncol(section2a1), c(7, 8, 13, 15, 17, 19))) {
  section2a1[[i]] <- as.integer(section2a1[[i]])
}

section2a1 <- section2a1 %>%
  mutate(
  psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household number"
  ), 
  v201 = labelled(
    v201, 
    label = "Is this dwelling unit occupied by your household only?",
    labels = c(Yes = 1, No = 2)
  ), 
  v202 = labelled(
    v202, 
    label = "How many rooms does your household use?"
  ), 
  v203 = labelled(
    v203, 
    label = "What is the foundation of the house?",
    labels = c("Mud bonded bricks/stone" = 1, "Cement bonded bricks/stones" = 2, "Concrete with pillar" = 3, "Wooden pillar" = 4, Others = 5)
  ), 
  v203a = labelled(
    v203a, 
    label = "Other foundation type"
  ), 
  v204 = labelled(
    v204, 
    label = "What is the outer wall of the house?",
    labels = c("Mud bonded bricks/stone" = 1, "Cement bonded bricks/stone" = 2, "Wood/planks" = 3, "Bamboo" = 4, "Unbaked bricks" = 5, "Galvanized sheet" = 6, Others = 7)
  ), 
  v204a = labelled(
    v204a, 
    label = "Other outer wall type"
  ), 
  v205 = labelled(
    v205, 
    label = "What is the roof of the house?", 
    labels = c("Galvanized sheet" = 1, "Cement Concrete (RCC)" = 2, Tile = 3, "Stone/Slate" = 4, "Wood/planks" = 5, "Straw/thatch" = 6, Other = 7)
  ), 
  v205a = labelled(
    v205a, 
    label = "Other roof type"
  ), 
  v206 = labelled(
    v206, 
    label = "What type of material does the floor made of?",
    labels = c("Earth/mud" = 1, "Cement" = 2, "Ceramic tile" = 3, "Wood planks/bamboo" = 4, "Parquet" = 5, Other = 6)
  ), 
  v206a = labelled(
    v206a, 
    label = "Other floor type"
  ), 
  v207 = labelled(
    v207, 
    label = "In which year (in BS) was the house you are living in built?"
  )
)

#Part 2.1.2 - Housing Expenses

for (i in setdiff(1:ncol(section2a2), c(6, 7, 15))) {
  section2a2[[i]] <- as.integer(section2a2[[i]])
}

section2a2 <- section2a2 %>%
  mutate(
    psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household number"
  ), 
  v208 = labelled(
    v208, 
    label = "Does this dwelling belong to your family?", 
    labels = c(Yes = 1, No = 2)
  ), 
  v209 = labelled(
    v209, 
    label = "If you wanted to buy a dwelling just like this today, how much money would you have to pay?"
  ), 
  v210 = labelled(
    v210, 
    label = "If someone wanted to rent this dwelling today, how much money would they have to pay each month?"
  ), 
  v211 = labelled(
    v211, 
    label = "Do you rent out part of this dwelling unit?",
    labels = c(Yes = 1, No = 2)
  ), 
  v212 = labelled(
    v212, 
    label = "How much rent do you receive per month?"
  ), 
  v213a = labelled(
    v213a, 
    label = "What is your present occupancy status?",
    labels = c(Renter = 1, "Provided free of charge" = 2, Squatting = 3, Other = 4)
  ), 
  v213b = labelled(
    v213b, 
    label = "Other (specify)"
  ), 
  v214 = labelled(
    v214, 
    label = "If someone wanted to rent this dwelling (only the unit occupied by the household) today, how much money would they have to pay each month?"
  ), 
  v215 = labelled(
    v215, 
    label = "What is the rent per month? (include cash + value of in-kind payments)"
  )
)

#Part 2.1.3 - Utilities and Amenities

for (i in setdiff(1:ncol(section2a3), c(6, 7, 10, 13, 23, 32))) {
  section2a3[[i]] <- as.integer(section2a3[[i]])
}

section2a3 <- section2a3 %>%
  mutate(
  psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household number"
  ), 
  v216 = labelled(
    v216, 
    label = "Which is the main source of your drinking water? (drinking and cooking)",
    labels = c("Piped water (Within compound)" = 1, "Piped water (outside compound)" = 2, "Tubewell/Hand pump" = 3, "Covered well/Kuwa" = 4, "Uncovered well/Kuwa" = 5, "Spout water" = 6, "River/Stream" = 7, Others = 8)
  ), 
  v216a = labelled(
    v216a, 
    label = "Other source of drinking water"
  ), 
  v217 = labelled(
    v217, 
    label = "How much did you pay for water over the last 12 months? (excluding irrigation)"
  ), 
  v218 = labelled(
    v218, 
    label = "What is the main type of fuel used for cooking in your household?",
    labels = c("Wood/Firewood" = 1, "LP Gas" = 2, Biogas = 3, Kerosene = 4, "Dung Cake" = 5, Electricity = 6, Others = 7)
  ), 
  v218a = labelled(
    v218a, 
    label = "Other type of fuel used for cooking"
  ), 
  v219a = labelled(
    v219a, 
    label = "Have you spent on firewood over the past 12 months?", 
    labels = c(Yes = 1, No = 2)
  ), 
  v219b = labelled(
    v219b, 
    label = "Have you spent on LPG over the past 12 months?",
    labels = c(Yes = 1, No = 2)
  ), 
  v219c = labelled(
    v219c, 
    label = "Have you spent on kerosene over the past 12 months?",
    labels = c(Yes = 1, No = 2)
  ), 
  v219d = labelled(
    v219d, 
    label = "Have you spent on dung cake over the past 12 months?",
    labels = c(Yes = 1, No = 2)
  ), 
  v219a1 = labelled(
    v219a1, 
    label = "How much did you spend on firewood over the past 12 months?"
  ), 
  v219b1 = labelled(
    v219b1, 
    label = "How much did you spend on LPG over the past 12 months?"
  ), 
  v219c1 = labelled(
    v219c1, 
    label = "How much did you spend on kerosene over the past 12 months?"
  ), 
  v219d1 = labelled(
    v219d1, 
    label = "How much did you spend on dung cake over the past 12 months?"
  ), 
  v220 = labelled(
    v220, 
    label = "What is the main source of lighting in your household?",
    labels = c(Electricity = 1, Solar = 2, Kerosene = 3, Biogas = 4, Others = 5)
  ), 
  v220a = labelled(
    v220a, 
    label = "Other source of lighting"
  ), 
  v221 = labelled(
    v221, 
    label = "How much did you spend on electricity over the past 12 months?"
  ), 
  v222a = labelled(
    v222a, 
    label = "Have you spent on smart/phones?", 
    labels = c(Yes = 1, No = 2)
  ), 
  v222b = labelled(
    v222b, 
    label = "Have you spent on cable/satellite TV?",
    labels = c(Yes = 1, No = 2)
  ), 
  v222c = labelled(
    v222c, 
    label = "Have you spent on internet facility?",
    labels = c(Yes = 1, No = 2)
  ), 
  v222a1 = labelled(
    v222a1, 
    label = "How much did you pay for the phone facility over the past 12 months?"
  ), 
  v222b1 = labelled(
    v222b1, 
    label = "How much did you pay for the cable/satellite TV facility over the past 12 months?"
  ), 
  v222c1 = labelled(
    v222c1, 
    label = "How much did you pay for the internet facility over the past 12 months?"
  ), 
  v223 = labelled(
    v223, 
    label = "How does your household dispose of its garbage mainly?",
    labels = c("Collected by municipality" = 1, "Private/community collector" = 2, "Dumped/thrown away" = 3, "Burned/buried" = 4, "Used for fertilizer/compost" = 5, Other = 6)
  ), 
  v223a = labelled(
    v223a, 
    label = "Other means of garbage disposals"
  ), 
  v224 = labelled(
    v224, 
    label = "How much did your household pay for garbage disposal over the last 12 months? (write 0 if nothing)"
  ), 
  v225 = labelled(
    v225, 
    label = "What type of toilet facility are you using in your household?",
    labels = c("Flush toilet (Public Sewage)" = 1, "Flush toilet (Septic tank)" = 2, "Ordinary toilet" = 3, "Public toilet" = 4, "No toilet" = 5)
  )
)

#Part 2.2 - Awarebess about and Affiliation with Health Insurance Program

section2b <- section2b %>% 
  rename(
  v227h_1 = v227a
  ) %>%
  mutate(
  v227_new = ifelse(grepl("\\b1\\b", v227), 1, 0),
  v227b = ifelse(grepl("\\b2\\b", v227), 1, 0),
  v227c = ifelse(grepl("\\b3\\b", v227), 1, 0),
  v227d = ifelse(grepl("\\b4\\b", v227), 1, 0),
  v227e = ifelse(grepl("\\b5\\b", v227), 1, 0),
  v227f = ifelse(grepl("\\b6\\b", v227), 1, 0),
  v227g = ifelse(grepl("\\b7\\b", v227), 1, 0),
  v227h = ifelse(grepl("\\b96\\b", v227), 1, 0)
  ) %>%
  select(-v227) %>%
  rename(
  v227a = v227_new
  ) %>%
  select(-v227h_1, everything(), v227h_1)

for (i in setdiff(1:ncol(section2b), c(6, 7, 22, 34, 44, 81))) {
  section2b[[i]] <- as.integer(section2b[[i]])
}

section_2b <- section2b %>%
  mutate(
  psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household number"
  ), 
  v226 = labelled(
    v226, 
    label = "Are you aware of any health insurance programme that are available in Nepal?",
    labels = c(Yes = 1, No = 2)
  ), 
  v227a = labelled(
    v227a, 
    label = "National Health Insurance Programme"
  ),
  v227b = labelled(
    v227b, 
    label = "Health Insurance - SSF"
  ),
  v227c = labelled(
    v227c, 
    label = "Private health insurance"
  ),
  v227d = labelled(
    v227d, 
    label = "Employer provided health insurance"
  ),
  v227e = labelled(
    v227e, 
    label = "Bank/cooperative health insurance"
  ),
  v227f = labelled(
    v227f, 
    label = "Hospital/health facility insurance"
  ),
  v227g = labelled(
    v227g, 
    label = "Community-based health insurance"
  ),
  v227h = labelled(
    v227h, 
    label = "Other health insurance"
  ),
  v227h_1 = labelled(
    v227h_1, 
    label = "Others (Specify)"
  ),
  v228 = labelled(
    v228, 
    label = "Are you enrolled in any of these programmes?",
    labels = c(Yes = 1, No = 2)
  ), 
  v229 = labelled(
    v229, 
    label = "If yes, could you name the programme?"
  ), 
  v229a = labelled(
    v229a, 
    label = "Could you tell the year of enrollment in the program?"
  ), 
  v230 = labelled(
    v230, 
    label = "If you received free enrollment, who paid your amount?",
    labels = c(
      Self = 1, 
      "Health Insurance Board" = 2, 
      "Provincial Government" = 3, 
      "Local Government" = 4, 
      Employer = 5, 
      Other = 6)
  ), 
  v230a = labelled(
    v230a, 
    label = "Others (specify)"
  ), 
  v231 = labelled(
    v231, 
    label = "Are you aware that the annual NHIP premium is NPR 3,500 and the benefit package is NPR 100,000 per household per year?",
    labels = c(Yes = 1, No = 2)
  ), 
  v232a = labelled(
    v232a, 
    label = "Cannot afford to pay the insurance premium or contribution"
  ), 
  v232b = labelled(
    v232b, 
    label = "Not aware of the health insurance scheme or its benefits"
  ), 
  v232c = labelled(
    v232c, 
    label = "Do not trust the quality of services provided under the insurance"
  ), 
  v232d = labelled(
    v232d, 
    label = "Do not frequently need health care (family is generally healthy)"
  ), 
  v232e = labelled(
    v232e, 
    label = "Health facilities nearby do not offer good insurance-covered services or are not well equipped"
  ), 
  v232f = labelled(
    v232f, 
    label = "The enrolment process is difficult, unclear, or not accessible in my area or workplace"
  ), 
  v232g = labelled(
    v232g, 
    label = "Prefer to pay directly for health services when needed (do not see the value of insurance)"
  ), 
  v232h = labelled(
    v232h, 
    label = "Previously enrolled but dropped out due to dissatisfaction (poor service, delays, pay for medicines)"
  ), 
  v232i = labelled(
    v232i, 
    label = "The employer has not supported or initiated the SSF enrolment process (for those who should be enrolled through the workplace)"
  ), 
  v232j = labelled(
    v232j, 
    label = "I prefer to remain in other private health insurance or existing employer-provided schemes"
  ), 
  v232k = labelled(
    v232k, 
    label = "Other (please specify)"
  ), 
  v233a = labelled(
    v233a, 
    label = "To reduce out-of-pocket health expenses"
  ), 
  v233b = labelled(
    v233b, 
    label = "To protect the family from unexpected or large health costs (e.g. accidents, major illnesses)"
  ), 
  v233c = labelled(
    v233c, 
    label = "To cover ongoing treatment for chronic conditions (e.g. diabetes, hypertension, heart disease, asthma)"
  ), 
  v233d = labelled(
    v233d, 
    label = "To access affordable health services and medicines"
  ), 
  v233e = labelled(
    v233e, 
    label = "Because the insurance was mandatory (e.g. through the Social Security Fund)"
  ), 
  v233f = labelled(
    v233f, 
    label = "Because the premium was affordable for the household"
  ), 
  v233g = labelled(
    v233g, 
    label = "Because the enrolment process was easy or facilitated by the employer/local health authority"
  ), 
  v233h = labelled(
    v233h, 
    label = "Based on advice or recommendation from family, friends, health workers, or local leaders"
  ), 
  v233i = labelled(
    v233i, 
    label = "Past experience with high health costs pushed the household to enrol"
  ), 
  v233j = labelled(
    v233j, 
    label = "Others (please specify)"
  ), 
  v234 = labelled(
    v234, 
    label = "In the past 12 months, how many times did your household use NHIP-covered health services?"
  ), 
  v235 = labelled(
    v235, 
    label = "In past 12 months, did you get referral to receive your treatment under NHIP?",
    labels = c(Yes = 1, No = 2)
  ), 
  v236 = labelled(
    v236, 
    label = "Did NHIP cover most of your health expenses during these visits?",
    labels = c("Yes - Fully" = 1, "Yes - Partially" = 2, "No" = 3)
  ), 
  v237a = labelled(
    v237a, 
    label = "If partially paid, how much did you pay OOP in total despite having NHIP coverage?"
  ), 
  v237b = labelled(
    v237b, 
    label = "If partially paid, how much did you pay OOP in hospital costs despite having NHIP coverage?"
  ), 
  v237c = labelled(
    v237c, 
    label = "If partially paid, how much did you pay OOP in medicines despite having NHIP coverage?"
  ), 
  v237d = labelled(
    v237d, 
    label = "If partially paid, how much did you pay OOP in radiology despite having NHIP coverage?"
  ), 
  v237e = labelled(
    v237e, 
    label = "If partially paid, how much did you pay OOP in referrals despite having NHIP coverage?"
  ), 
  v238 = labelled(
    v238, 
    label = "In the past 12 months, how many times did your household use SSF-covered health services?"
  ), 
  v239 = labelled(
    v239, 
    label = "Did SSF cover most of your health expenses during these visits?", 
    labels = c(
      "Yes - Fully" = 1, 
      "Yes - Partially" = 2, 
      "No" = 3)
  ), 
  v240a = labelled(
    v240a, 
    label = "If partially paid, how much did you pay OOP in total despite having SSF coverage?"
  ), 
  v240b = labelled(
    v240b, 
    label = "If partially paid, how much did you pay OOP in hospital costs despite having SSF coverage?"
  ), 
  v240c = labelled(
    v240c, 
    label = "If partially paid, how much did you pay OOP in medicines despite having SSF coverage?"
  ), 
  v240d = labelled(
    v240d, 
    label = "If partially paid, how much did you pay OOP in lab tests despite having SSF coverage?"
  ), 
  v240e = labelled(
    v240e, 
    label = "If partially paid, how much did you pay OOP in referrals despite having SSF coverage?"
  ), 
  v241 = labelled(
    v241, 
    label = "Do you find the NPR 3,500 annual premium affordable under NHIP?",
    labels = c("Very affordable" = 1, 
    "Somewhat affordable" = 2, 
    "Not affordable" = 3)
  ), 
  v242 = labelled(
    v242, 
    label = "Would you be willing to continue paying NPR 3,500 next year under NHIP?",
    labels = c(Yes = 1, No = 2)
  ), 
  v243 = labelled(
    v243, 
    label = "Would you be willing to pay a slightly higher amount if the benefit package improved?",
    labels = c(Yes = 1, No = 2)
  ), 
  v244 = labelled(
    v244, 
    label = "If yes: what is the maximum amount you would pay?"
  ), 
  v245 = labelled(
    v245, 
    label = "Would you pay NPR 3,500 for insurance with 100,000 ceiling? (for non-insured)",
    labels = c(Yes = 1, No = 2)
  ), 
  v246 = labelled(
    v246, 
    label = "If no, what is the maximum amount you would be willing to pay?"
  ), 
  v247 = labelled(
    v247, 
    label = "What is your household's total monthly income?"
  ), 
  v248 = labelled(
    v248, 
    label = "What is your household's total monthly expenditure?"
  ), 
  v249 = labelled(
    v249, 
    label = "How much did your household spend on health services in the past 12 months?"
  ), 
  v250 = labelled(
    v250, 
    label = "Could you afford your current or proposed premium without reducing essentials?",
    labels = c(Yes = 1, No = 2, "Not sure" = 3)
  ), 
  v251 = labelled(
    v251, 
    label = "In past 12 months, did you borrow, sell assets, or reduce essentials to pay for health care?",
    labels = c(Yes = 1, No = 2)
  ), 
  v252 = labelled(
    v252, 
    label = "If yes, how much did you borrow or sell in assets?"
  ), 
  v253 = labelled(
    v253, 
    label = "If a family member falls seriously ill, what is the max you could afford in a year?"
  )
)

#Part 2.3 - Mortality (Death) Information

for (i in setdiff(1:ncol(section2c), c(6, 7, 12, 16, 17, 18))) {
  section2c[[i]] <- as.integer(section2c[[i]])
}

section2c <- section2c %>%
  mutate(
  psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household number"
  ), 
  v254 = labelled(
    v254, 
    label = "In the past 12 months, has there been a death in your family?", 
    labels = c(Yes = 1, No = 2)
  ), 
  v255 = labelled(
    v255, 
    label = "If yes, how many deaths occurred?"
  ), 
  v256 = labelled(
    v256, 
    label = "Deceased ID"
  ), 
  v257 = labelled(
    v257, 
    label = "Name of the deceased person"
  ), 
  v258 = labelled(
    v258, 
    label = "Sex of the deceased person", 
    labels = c(Male = 1, Female = 2)
  ), 
  v259 = labelled(
    v259, 
    label = "Age at the time of death"
  ), 
  v260 = labelled(
    v260, 
    label = "Main cause of death", 
    labels = c(
      "Communicable disease" = 1, 
      "Non-communicable disease" = 2, 
      "Traffic accident" = 3, 
      "Other accident" = 4, 
      "Reproductive and obstetric complications" = 5, 
      "Homicide" = 6, 
      "Suicide" = 7, 
      "Natural Disaster" = 8, 
      Others = 9)
  ), 
  v260a = labelled(
    v260a, 
    label = "Others (specify)"
  ), 
  v261 = labelled(
    v261a, 
    label = "If the deceased was a woman aged 15 to 49, what was her condition at the time of death?",
    labels = c(
      Pregnant = 1, 
      "In labour" = 2, 
      "Postpartum (<= 6 weeks after childbirth)" = 3, 
      Other = 4)
  ), 
  v261a = labelled(
    v261a, 
    label = "Other (specify)"
  ), 
  v262a = labelled(
    v262a, 
    label = "How much cost occurred for treatment (hospital cost) in the last 12 months?"
  ), 
  v262b = labelled(
    v262b, 
    label = "How much cost occurred for medicine in the last 12 months?"
  ), 
  v262c = labelled(
    v262c, 
    label = "How much cost occurred for tests (radiology/pathology) in the last 12 months?"
  ), 
  v262d = labelled(
    v262d, 
    label = "How much cost occurred for transportation (in-country and outside) in the last 12 months?"
  ), 
  v262e = labelled(
    v262e, 
    label = "How much cost occurred for care giver (transport, hotel, fooding) in the last 12 months?"
  ), 
  v262f = labelled(
    v262f, 
    label = "How much wage loss occurred for the care giver in the last 12 months?"
  )
)

#SECTION 3: FOOD CONSUMPTION

section3a <- read.xlsx("dataset/Section 3_ Consumption of Food.xlsx")
section3b <- read.xlsx("dataset/Part 3_1_ Food away from home.xlsx")

#Part 3.1: Food at Home

for (i in setdiff(1:ncol(section3a), c(6, 7))) {
  section3a[[i]] <- as.integer(section3a[[i]])
}

section3a <- section3a %>%
  mutate(
  psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward number"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household number"
  ), 
  v301 = labelled(
    v301, 
    label = "Food items",
    labels = c(
      "Grains and Cereals" = 1, 
      "Pulses and Lentils" = 2, 
      "Meats and Fish" = 3, 
      "Eggs and Milk Products" = 4, 
      "Ghee (Butter, lard and other animal-based oils and fats)" = 5, 
      "Cooking (Vegetable) Oils" = 6, 
      "Fruits and Nuts (fresh, dried, dehydrated, frozen)" = 7, 
      "Vegetables (fresh, dried, dehydrated, frozen)" = 8, 
      "Sweets and confectionary" = 9, 
      "Spices and Condiments" = 10, 
      "Tea and Coffee" = 11, 
      "Non-alcoholic beverages" = 12, 
      "Alcoholic Beverages (local or imported)" = 13, 
      "Tobacco and Tobacco produces" = 14, 
      "Prepared food products" = 15
    )
  ), 
  v302 = labelled(
    v302, 
    label = "In the last week, did your household consume [item]?",
    labels = c(Yes = 1, No = 2)
  ), 
  v303 = labelled(
    v303, 
    label = "Estimated value of [item] consumed from home production (NPR)"
  ), 
  v304 = labelled(
    v304, 
    label = "Estimated value of [item] purchased for consumption (NPR)"
  ), 
  v305 = labelled(
    v305, 
    label = "Estimated value of [item] received in-kind (NPR)"
  )
)

#Part 3.2: Food Away from Home

for (i in setdiff(1:ncol(section3b), c(6, 7))) {
  section3b[[i]] <- as.integer(section3b[[i]])
}

section3b <- section3b %>%
  mutate(
  psu = labelled(
    psu, 
    label = "PSU number"
  ), 
  palika = labelled(
    palika, 
    label = "Local level"
  ), 
  ward = labelled(
    ward, 
    label = "Ward #"
  ), 
  hhld = labelled(
    hhld, 
    label = "Household #"
  ), 
  v306 = labelled(
    v306, 
    label = "Food items (away from home)",
    labels = c("
    Tea/coffee, juice/lassi or bottled water" = 1, 
    "breakfast" = 2, 
    "lunch" = 3, 
    "afternoon snack" = 4, 
    "dinner" = 5, 
    "carbonated/soft drinks" = 6, 
    "spirits, wine, beer or other alcoholic drinks" = 7, 
    "Other food items" = 8)
  ), 
  v307 = labelled(
    v307, 
    label = "In the last week did you or your household member consume [item] outside, either by paying yourself or as a guest?",
    labels = c(Yes = 1, No = 2)
  ), 
  v308 = labelled(
    v308, 
    label = "Amount spent to purchase [item] outside the household in the last week (NPR)"
  ), 
  v309 = labelled(
    v309, 
    label = "Estimated value of [item] received as guest outside household in the last week (NPR)"
  )
)

#SECTION 4: NON-FOOD EXPENDITURE AND INVENTORY OF DURABLE GOODS 

section4a <- read.xlsx("dataset/section 4.xlsx")
section4b <- read.xlsx("dataset/Part 4_2_ Expenditure Abroad.xlsx")
section4c <- read.xlsx("dataset/Part 4_3_ Inventory of Durable Goods.xlsx")
section4d <- read.xlsx("dataset/Part 4_4_ Own Account Consumption of Goods.xlsx")

#Part 4.1 - Non-Food Expenditures
for (i in setdiff(1:ncol(section4a), c(6, 7))) {
  section4a[[i]] <- as.integer(section4a[[i]])
}

section4a <- section4a %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ), 
    palika = labelled(
      palika,
      label = "Local level"
    ), 
    ward = labelled(
      ward,
      label = "Ward number"
    ), 
    hhld = labelled(
      hhld,
      label = "Household number"
    ), 
    v401 = labelled(
      v401,
      label = "Items",
      labels = c(
        "Clothing and apparel" = 1, 
        "Shoes and Slippers" = 2, 
        "Repair and Minor Repair of House" = 3, 
        "Fuel" = 4, 
        "Furniture and Furnishings" = 5, 
        "Purchase and Maintenance of Textiles for Household Use" = 6, 
        "Purchase and Maintenance of Household Equipment and Appliances" = 7, 
        "Purchase and Maintainence of House and Kitchen-garden" = 8, 
        "Purchase and Maintenance of House and Kitchen-garden" = 9, 
        "Expenses on Regular House Cleaning" = 10, 
        "Purchase of Personal Vehicle" = 11, 
        "Repair and Maintenance of Vehicle" = 12, 
        "Public Transportation Expenses" = 13, 
        "Communication Cost" = 14, 
        "Audio-Visual, Photographic and Information Processing Equipment Expenses" = 15, 
        "Music and Entertainment Related Goods" = 16, 
        "Sports and Hobby Related Expenses" = 17, 
        "Amusement and Cultural Services" = 18, 
        "Books, Magazines and Stationery" = 19, 
        "Domestic Holiday Package" = 20, 
        "Education Expenses" = 21, 
        "Preventive Health Care Expenses" = 22, 
        "Lodging and Hostel Costs" = 23, 
        "Other Non-Electronic Personal Use Items" = 24, 
        "Social Security Expenses" = 25, 
        "Insurance costs" = 26, 
        "Banking services" = 27, 
        "Administrative and Legal Costs" = 28, 
        "Festival and parties" = 29, 
        "Other Non-Food Consumption" = 30)
    ), 
    v402 = labelled(
      v402,
      label = "Were any of the following items purchased or received in-kind by your household over the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ), 
    v403a = labelled(
      v403a,
      label = "Money value of item purchased or received in-kind (12 months)"
    ), 
    v403b = labelled(
      v403b,
      label = "Money value of item purchased or received in-kind (30 days)"
    )
  )

#Part 4.2: Expenditure Abroad

for (i in setdiff(1:ncol(section4b), c(6, 7))) {
  section4b[[i]] <- as.integer(section4b[[i]])
}

section4b <- section4b %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ), 
    palika = labelled(
      palika,
      label = "Local level"
    ), 
    ward = labelled(
      ward,
      label = "Ward number"
    ), 
    hhld = labelled(
      hhld,
      label = "Household number"
    ), 
    v404 = labelled(
      v404,
      label = "Did you or any of the household members travel to a foreign country in the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ), 
    v405 = labelled(
      v405,
      label = "Tourism expenditure items",
      labels = c(
        "Tour packages" = 1, 
        "Food & Beverages" = 2, 
        "Accomodation" = 3, 
        "Transportation" = 4, 
        "Health-related expenses" = 5, 
        "Leisure & entertainment activities" = 6, 
        "Shopping and goods" = 7, 
        "Travel essentials" = 8, 
        "Services & Fees" = 9, 
        "Other expenses" = 10
      )
    ), 
    v406 = labelled(
      v406,
      label = "Were any of the following items purchased or received in-kind by your household over the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ), 
    v407a = labelled(
      v407a,
      label = "Money value of the amount purchased or received in-kind (12 months)"
    ), 
    v407b = labelled(
      v407b,
      label = "Money value of the amount purchased or received in-kind (30 days)"
    )
  )

#Part 4.3 - Inventory of Durable Goods

for (i in setdiff(1:ncol(section4c), c(6, 7))) {
  section4c[[i]] <- as.integer(section4c[[i]])
}

section4c <- section4c %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v408 = labelled(
      v408,
      label = "Household items", 
      labels = c(
        "Radio/Player" = 1, 
        "Camera (Still/Movie)" = 2, 
        Bicycle = 3, 
        "Rikshaw/e-Rikshaw" = 4, 
        "Motorcycle/Scooter" = 5, 
        "Tractor/Power Tiller" = 6, 
        "Car, Jeep, Van, etc." = 7, 
        "Bus/Truck" = 8, 
        "Refrigerator or Freezer" = 9, 
        "Microwave Oven" = 10, 
        "Geyser (Gas/Electricity)" = 11, 
        "Washing Machine" = 12, 
        Fans = 13, 
        "Heater(gas/kerosene/electric)" = 14, 
        Television = 15, 
        "Air conditioner/Cooler" = 16, 
        "Vacuum cleaner" = 17, 
        Inverter = 18, 
        "Solar panel (for electricity)" = 19, 
        "Solar heater" = 20, 
        "Electric Iron" = 21, 
        "Telephone sets (Fixed/Mobile)" = 22, 
        "Sewing machine" = 23, 
        "Computer/Laptop" = 24, 
        "Wrist watch" = 25, 
        "Furniture (Sofa set, dining, rack, etc.)" = 26, 
        "LPG Stove/Cooking device" = 27)
    ),
    v409 = labelled(
      v409,
      label = "Does your household own any of the following items?",
      labels = c(Yes = 1, No = 2)
    ),
    v410 = labelled(
      v410,
      label = "How many ..[item].. Does your household own?"
    ),
    v411a = labelled(
      v411a,
      label = "How many years ago did you last acquire ..[item]? Years"
    ),
    v411b = labelled(
      v411b,
      label = "How many years ago did you last acquire ..[item]? NPR"
    ),
    v412a = labelled(
      v412a,
      label = "Value paid or estimated value of gift for the last acquired ..[item]?  No. of items"
    ),
    v412b = labelled(
      v412b,
      label = "Value paid or estimated value of gift for the last acquired ..[item]? NPR"
    ),
    v413 = labelled(
      v413,
      label = "If you wanted to sell this ..[item].. Today, how much would you receive?"
    )
  )

#Part 4.4 - Own Account Consumption of Goods

for (i in setdiff(1:ncol(section4d), c(6, 7))) {
  section4d[[i]] <- as.integer(section4d[[i]])
}

section4d <- section4d %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v414 = labelled(
      v414,
      label = "Self-produced and consumed items or services",
      labels = c(
        "Bamboo & Cane Products" = 1, 
        "Straw & Grass Products" = 2, 
        "Textiles & Clothing" = 3, 
        "Wooden Products & Furniture" = 4, 
        "Metal Tools & Implements" = 5, 
        "Processed Foods & Preserves" = 6, 
        "Household Services & MAintenance" = 7, 
        "Other Handicrafts & Items" = 8)
    ),
    v415 = labelled(
      v415,
      label = "Were any of the following items produced and consumed by your household over the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v416a = labelled(
      v416a,
      label = "Monetary value of items self-produced and consumed during the past 12 months"
    ),
    v416b = labelled(
      v416b,
      label = "Monetary value of items self-produced and consumed during the past 30 days?"
    )
  )

#SECTION 5: EXPENSE IN EDUCATION

section5 <- read.xlsx("dataset/Section 5_ Expense in Education.xlsx")

for (i in setdiff(1:ncol(section5), c(6, 7))) {
  section5[[i]] <- as.integer(section5[[i]])
}

section5 <- section5 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v501 = labelled(
      v501,
      label = "How do you go to school/college?",
      labels = c(
        Walk = 1, 
        "School/College Vehicle" = 2, 
        "Private Vehicle" = 3, 
        "Public Vehicle" = 4, 
        "Other" = 5, 
        "Not Applicable" = 6)
    ),
    v502a = labelled(
      v502a,
      label = "Tuition fee spent in the past 12 months"
    ),
    v502b = labelled(
      v502b,
      label = "Other fees (exam, admission, events, etc.) Spent in the past 12 months"
    ),
    v502c = labelled(
      v502c,
      label = "Uniform/school dress and shoes spent in the past 12 months"
    ),
    v502d = labelled(
      v502d,
      label = "Textbook/supplies spent in the past 12 months"
    ),
    v502e = labelled(
      v502e,
      label = "Transportation costs spent in the past 12 months"
    ),
    v502f = labelled(
      v502f,
      label = "Private tuition costs spent in the past 12 months"
    ),
    v502g = labelled(
      v502g,
      label = "Other school-related expenses (snacks, tea, etc.) In the past 12 months"
    ),
    v503 = labelled(
      v503,
      label = "Did you receive a scholarship in the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v504 = labelled(
      v504,
      label = "Amount received as scholarship over past 12 months"
    )
  )


#SECTION 6:EXPENSES IN HEALTH

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

#Part 6.1 - Screening for General Health Status 

for (i in setdiff(1:ncol(section6a), c(6, 7))) {
  section6a[[i]] <- as.integer(section6a[[i]])
}

section6a <- section6a %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v601a = labelled(
      v601a,
      label = "How would you describe your ability to walk or move around today?",
      labels = c(
        "No problem" = 1, 
        "Slight problems" = 2, 
        "Moderate problems" = 3, 
        "Severe problems" = 4, 
        "Unable to walk" = 5)
    ),
    v601b = labelled(
      v601b,
      label = "How would you describe your ability to wash, dress, or care for yourself today?",
      labels = c(
        "No problem" = 1, 
        "Slight problems" = 2, 
        "Moderate problems" = 3, 
        "Severe problems" = 4, 
        "Unable to walk" = 5)
    ),
    v601c = labelled(
      v601c,
      label = "How would you describe your ability to perform daily activities (work, study, housework) today?",
      labels = c(
        "No problem" = 1, 
        "Slight problems" = 2, 
        "Moderate problems" = 3, 
        "Severe problems" = 4, 
        "Unable to walk" = 5)
    ),
    v601d = labelled(
      v601d,
      label = "How would you describe any pain or discomfort you feel today?",
      labels = c(
        "No problem" = 1, 
        "Slight problems" = 2, 
        "Moderate problems" = 3, 
        "Severe problems" = 4, 
        "Unable to walk" = 5)
    ),
    v601e = labelled(
      v601e,
      label = "How would you describe any feelings of anxiety or depression today?",
      labels = c(
        "No problem" = 1, 
        "Slight problems" = 2, 
        "Moderate problems" = 3, 
        "Severe problems" = 4, 
        "Unable to walk" = 5)
    ),
    v602 = labelled(
      v602,
      label = "Please mark your health condition today on this scale, where 100 is the best health, and 0 is the worst (You can imagine) My health today"
    )
  )


#Part 6.2.1 - Chronic Illness and Health Seeking Behavior 

section6b1 <- section6b1 %>%
  mutate(v610 = v610a) %>%
  rename(
    v610n_1 = v610b
  ) %>%
  mutate(
    v610_new = ifelse(grepl("\\b1\\b", v610a), 1, 0), 
    v610b = ifelse(grepl("\\b2\\b", v610a), 1, 0),
    v610c = ifelse(grepl("\\b3\\b", v610a), 1, 0),
    v610d = ifelse(grepl("\\b4\\b", v610a), 1, 0),
    v610e = ifelse(grepl("\\b5\\b", v610a), 1, 0),
    v610f = ifelse(grepl("\\b6\\b", v610a), 1, 0),
    v610g = ifelse(grepl("\\b7\\b", v610a), 1, 0),
    v610h = ifelse(grepl("\\b8\\b", v610a), 1, 0), 
    v610i = ifelse(grepl("\\b9\\b", v610a), 1, 0),
    v610j = ifelse(grepl("\\b10\\b", v610a), 1, 0), 
    v610k = ifelse(grepl("\\b11\\b", v610a), 1, 0), 
    v610l = ifelse(grepl("\\b12\\b", v610a), 1, 0),
    v610m = ifelse(grepl("\\b13\\b", v610a), 1, 0), 
    v610n = ifelse(grepl("\\b14\\b", v610a), 1, 0)
  ) %>%
  select(-v610a) %>%
  rename(
    v610a = v610_new
  )

section6b1 <- section6b1 %>% 
  mutate(v611_1 = v611) %>%
  rename(
  v611m_1 = v611a
  ) %>%
  mutate(
  v611_new = ifelse(grepl("\\b1\\b", v611), 1, 0),
  v611b = ifelse(grepl("\\b2\\b", v611), 1, 0),
  v611c = ifelse(grepl("\\b3\\b", v611), 1, 0),
  v611d = ifelse(grepl("\\b4\\b", v611), 1, 0),
  v611e = ifelse(grepl("\\b5\\b", v611), 1, 0),
  v611f = ifelse(grepl("\\b6\\b", v611), 1, 0),
  v611g = ifelse(grepl("\\b7\\b", v611), 1, 0),
  v611h = ifelse(grepl("\\b8\\b", v611), 1, 0), 
  v611i = ifelse(grepl("\\b9\\b", v611), 1, 0),
  v611j = ifelse(grepl("\\b10\\b", v611), 1, 0), 
  v611k = ifelse(grepl("\\b11\\b", v611), 1, 0), 
  v611l = ifelse(grepl("\\b12\\b", v611), 1, 0),
  v611m = ifelse(grepl("\\b13\\b", v611), 1, 0),
  v604 = as.integer(trimws(v604)) 
  ) %>%
  select(-v611) %>%
  rename(
  v611a = v611_new,
  v611 = v611_1
  )

for (i in setdiff(1:ncol(section6b1), c(6, 7, 12, 13, 20, 21, 22, 37))) {
  section6b1[[i]] <- as.integer(section6b1[[i]])
}

section6b1 <- section6b1 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v603 = labelled(
      v603,
      label = "Do you have any chronic diseases needing regular medicines/checkups?",
      labels = c(Yes = 1, No = 2)
    ),
    v604 = labelled(
      v604,
      label = "Chronic health conditions", 
      labels = c(
        "Heart Diseases" = 1, 
        Hypertension = 2, 
        Diabetes = 3, 
        "Asthma/COPD" = 4, 
        "Rheumatism/Arthritis" = 5, 
        "Kidney Diseases" = 6, 
        "Liver Diseases" = 7, 
        "Cancer" = 8, 
        "Epilepsy" = 9, 
        Tuberculosis = 10, 
        "HIV/AIDS" = 11, 
        "Thyroid Disorders" = 12, 
        "Chronic Gastrointestinal Diseases" = 13, 
        "Gynaecological Problems" = 14, 
        "Chronic Orthopaedic Problems" = 15, 
        "Neurological Conditions" = 16, 
        "Alzheimer's/Parkinson's" = 17, 
        "Mental Illness" = 18, 
        "Others (Specify)" = 19)
    ),
    v604a = labelled(
      v604a,
      label = "Other chronic health conditions (specify)"
    ),
    v605a = labelled(
      v605a,
      label = "If yes, how many times have you visited a health facility for regular check-ups"
    ),
    v605b = labelled(
      v605b,
      label = "If yes, how many times have you visited a health facility for hospitalised"
    ),
    v606 = labelled(
      v606,
      label = "When were you diagnosed with this condition?"
    ),
    v607 = labelled(
      v607,
      label = "Are you currently receiving any treatment for this condition(s)?",
      labels = c(Yes = 1, No = 2)
    ),
    v608 = labelled(
      v608,
      label = "Did you use any insurance scheme/ government programme  for the treatment of this chronic illness?",
      labels = c(Yes = 1, No = 2)
    ),
    v609 = labelled(
      v609,
      label = "Which insurance scheme/government program was used for the past 12 months to pay for treatment?",
      labels = c(
        NHIP = 1, 
        SSF = 2, 
        "Employer provided" = 3, 
        "Privately purchased" = 4, 
        "Bank/Cooperative" = 5, 
        "Health/health facility" = 6, 
        "Community-based health insurance" = 7, 
        "Hospitals (army/police/civil service" = 8, 
        "Free health" = 9, 
        "Bipanna Nagarik" = 10, 
        "Aama Surakshya Programme" = 11, 
        "Social Security Unit/OCMC" = 12, 
        "5000 cash support" = 13, 
        Others = 14)
    ),
    v610 = labelled(
      v610,
      label = "If no, why are you not currently receiving treatment?",
    ),
    v610a = labelled(
      v610a, 
      label = "Cannot afford lifelong medications (eg. diabetes/hypertension drugs"
    ),
    v610b = labelled(
      v610b, 
      label = "Essential medicines frequently out of stock at local health facilities"
    ),
    v610c = labelled(
      v610c, 
      label = "Costs too high for regular hospital visits"
    ),
    v610d = labelled(
      v610d, 
      label = "No family member available to assist with clinic visits"
    ),
    v610e = labelled(
      v610e, 
      label = "Treatment showed no noticeable improvement over time"
    ),
    v610f = labelled(
      v610f, 
      label = "Local health centre lacks chronic disease specialists/services"
    ),
    v610g = labelled(
      v610g, 
      label = "Fear of side effects from long-term medication use"
    ),
    v610h = labelled(
      v610h, 
      label = "Long queues discourage repeat visit"
    ),
    v610i = labelled(
      v610i, 
      label = "Stopped treatment after consultation"
    ),
    v610j = labelled(
      v610j, 
      label = "Never sought formal treatment"
    ),
    v610k = labelled(
      v610k, 
      label = "Switched to alternate/traditional care"
    ),
    v610l = labelled(
      v610l, 
      label = "Ashamed to discuss illness (eg. mental health)"
    ),
    v610m = labelled(
      v610m, 
      label = "Clinic hours conflict with work"
    ),
    v610n = labelled(
      v610n, 
      label = "Others"
    ),
    v610n_1 = labelled(
      v610n_1,
      label = "Others (specify)"
    ),
    v611 = labelled(
      v611,
      label = "Where do you usually go for consultation in relation to this illness?",
    ), 
    v611a = labelled(
      v611a, 
      label = "Health Post"
    ), 
    v611b = labelled(
      v611b, 
      label = "Primary health centre"
    ),
    v611c = labelled(
      v611c,
      label = "Govermental hospital"
    ), 
    v611d = labelled(
      v611d, 
      label = "Government outreach clinic"
    ), 
    v611e = labelled(
      v611e, 
      label = "Government ayurveda clinic"
    ), 
    v611f = labelled(
      v611f, 
      label = "Pharmacy/Drug seller"
    ), 
    v611g = labelled(
      v611g, 
      label = "Private clinic"
    ), 
    v611h = labelled(
      v611h, 
      label = "Private/Community hospital"
    ), 
    v611i = labelled(
      v611i, 
      label = "Private Ayurveda Centre"
    ),
    v611j = labelled(
      v611j, 
      label = "Health worker's home"
    ), 
    v611k = labelled(
      v611k, 
      label = "Alternative/Traditional Healer"
    ),
    v611l = labelled(
      v611l, 
      label = "Abroad (India/Other)"
    ),
    v611m = labelled(
      v611m, 
      label= "Others"
    ),
    v611m_1 = labelled(
      v611m, 
      label = "Others (specify)"
    )
  )

#Part 6.2.2 - Chronic Illness and Medication Use

for (i in setdiff(1:ncol(section6b2), c(6, 7, 12, 13, 20, 21, 22, 37))) {
  section6b2[[i]] <- as.integer(section6b2[[i]])
}

section6b2 <- section6b2 %>%
  section6b2 <- section6b2 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v612 = labelled(
      v612,
      label = "How many different kinds of medicine do you have in a day?"
    ),
    v612a = labelled(
      v612a,
      label = "Medications: Heart diseases",
      labels = c(
        Aspirin = 11, 
        Atenolol = 12, 
        Atorvastain = 13
      )
    ),  
    v612b = labelled(
      v612b,
      label = "Medications: Hypertension",
      labels - c(
        Amlodipine = 21, 
        Losartan = 22, 
        Hydrochlorothiazide = 23, 
        Enalapril = 24
      )
    ),
    v612c = labelled(
      v612c,
      label = "Medications: Diabetes",
      labels = c(
        Metformin = 31, 
        Gliclazide = 32, 
        Insulin = 33
      )
    ),
    v612d = labelled(
      v612d,
      label = "Medications: Asthma/COPD",
      labels = c(
        Salbutamol = 41, 
        Budesonide = 42, 
        Montelukast = 43, 
        Ipratropium = 44, 
        Tiotropium = 45
      )
    ),
    v612e = labelled(
      v612e,
      label = "Medications: Rheumatism/arthritis",
      labels = c(
        Ibuprofen = 51, 
        Diclofenac = 52, 
        Methotrexate = 53
      )
    ),
    v612f = labelled(
      v612f,
      label = "Medications: Kidney diseases",
      labels = c(
        Furosemide = 61, 
        "Calcium Carbonate" = 62, 
        "Sodium Bicarbonate" = 63
      )
    ),
    v612g = labelled(
      v612g,
      label = "Medications: Liver diseases",
      labels = c(
        "Ursodeoxycholic Acid" = 71, 
        "Silmarin Tenofovir" = 72, 
        "Ursodeoxycholic Acid" = 73
      )
    ),
    v612h = labelled(
      v612h,
      label = "Medications: Cancer",
      labels = c(
        Paracetamol = 81, 
        Tramadol = 82, 
        Ondansetron = 83
      )
    ),
    v612i = labelled(
      v612i,
      label = "Medications: Epilepsy",
      labels = c(
        Phenytoin = 91, 
        Carbamazepine = 92, 
        "Valproic Acid" = 93
      )
    ),
    v612j = labelled(
      v612j,
      label = "Medications: Tuberculosis",
      labels = c(
        Rifampicin = 101, 
        Isoniazid = 102, 
        Pyrazinamide= 103, 
        Ethambutol = 104
      )
    ),
    v612k = labelled(
      v612k,
      label = "Medications: HIV/AIDS",
      labels = c(
        Tenofovir = 111, 
        Lamivudine = 112, 
        Efavirenz = 113
      )
    ),
    v612l = labelled(
      v612l,
      label = "Medications: Thyroid disorders",
      labels = c(
        Levothyroxine = 121, 
        Carbimazole = 122
      )
    ),
    v612m = labelled(
      v612m,
      label = "Medications: Chronic gastrointestinal diseases",
      labels = c(
        Omeprazole = 131, 
        Ranitide = 132, 
        Pantoprazole = 133
      )
    ),
    v612n = labelled(
      v612n,
      label = "Medications: Gynaecological problems",
      labels = c(
        "Tranexamic Acid" = 141, 
        Metronidazole = 142, 
        Clotrimazole = 143
      )
    ),
    v612o = labelled(
      v612o,
      label = "Medications: Chronic orthopaedic problems",
      labels = c(
        Paracetamol = 151, 
        Diclofenac = 152, 
        "Calcium Carbonate" = 153, 
        Other = 154
      )
    ),
    v612p = labelled(
      v612p,
      label = "Medications: Neurological conditions",
      labels = c(
        Amitriptyline = 161, 
        Gabapentin = 162, 
        Sumatripatan = 163
      )
    ),
    v612q = labelled(
      v612q,
      label = "Medications: Alzheimer's/Parkinson's",
      labels = c(
        Levodopa = 171, 
        Donepezil = 172
      )
    ),
    v612r = labelled(
      v612r,
      label = "Medications: Mental illness",
      labels = c(
        Fluoxetine = 181, 
        Sertraline = 182, 
        Diazepam = 183
      )
    ),
    v612s = labelled(
      v612s,
      label = "Medications: Other diseases",
      labels = c(
        Multivitamins = 191, 
        Calcium = 192, 
        "Vitamin D" = 193
      )
    ),
    v613 = labelled(
      v613,
      label = "How are you covering the cost of [name] medicine? (select all that apply for each condition)",
      labels = c(
        "Fully paid out of pocket" = 1, 
        "Received free of cost" = 2, 
        "Fully paid by NHIP" = 3, 
        "Partially paid through NHIP" = 4, 
        "Partially paid through SSF" = 5, 
        "Others" = 6
      )
    )
  )

#Part 6.2.3 - Chronic Illness and Expenditure Tracking - Outpatient (Regular Checkups)

for (i in (1:ncol(section6b3))) {
  section6b3[[i]] <- as.integer(section6b3[[i]])
}

section6b3 <- section6b3 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v604 = labelled(
      v604,
      label = "Health conditions",
      labels = c(
        "Heart Diseases" = 1, 
        Hypertension = 2, 
        Diabetes = 3, 
        "Asthma/COPD" = 4, 
        "Rheumatism/Arthritis" = 5, 
        "Kidney Diseases" = 6, 
        "Liver Diseases" = 7, 
        "Cancer" = 8, 
        "Epilepsy" = 9, 
        Tuberculosis = 10, 
        "HIV/AIDS" = 11, 
        "Thyroid Disorders" = 12, 
        "Chronic Gastrointestinal Diseases" = 13, 
        "Gynaecological Problems" = 14, 
        "Chronic Orthopaedic Problems" = 15, 
        "Neurological Conditions" = 16, 
        "Alzheimer's/Parkinson's" = 17, 
        "Mental Illness" = 18, 
        "Others (Specify)" = 19)
    ),
    v614 = labelled(
      v614,
      label = "How much have you spent in the past 12 months on the treatment of this illness?"
    ),
    v614a = labelled(
      v614a,
      label = "OOP-OPD: Emergency care"
    ),
    v614b = labelled(
      v614b,
      label = "OOP-OPD: Outpatient visits (OPD)"
    ),
    v614c = labelled(
      v614c,
      label = "OOP-OPD: Laboratory tests"
    ),
    v614d = labelled(
      v614d,
      label = "OOP-OPD: Imaging (e.g., x-ray, MRI, CT)"
    ),
    v614e = labelled(
      v614e,
      label = "OOP-OPD: Medicines"
    ),
    v614f = labelled(
      v614f,
      label = "OOP-OPD: Medical supplies/devices"
    ),
    v614g = labelled(
      v614g,
      label = "OOP-OPD: Transportation"
    ),
    v614h = labelled(
      v614h,
      label = "OOP-OPD: Food & accommodation"
    ),
    v614i = labelled(
      v614i,
      label = "OOP-OPD: Caregiver cost"
    ),
    v614j = labelled(
      v614j,
      label = "OOP-OPD: Other costs"
    ),
    v614k = labelled(
      v614k,
      label = "OOP-OPD: Total costs"
    ),
    v615 = labelled(
      v615,
      label = "What was your main source of funds for healthcare and treatment?",
      labels = c(
        "Own savings" = 1, 
        "Loan" = 2, 
        "Borrowing" = 3, 
        "Selling assets" = 4, 
        "From family" = 5,
        "Others" = 6
      )
    ),
    v615a = labelled(
      v615a,
      label = "Other main source of funds for healthcare and treatment"
    ),
    v616 = labelled(
      v616,
      label = "Did you have to stop doing your usual activity due to this illness during the past 12 months?",
      labels = c(
        Yes = 1,
        No = 2
      )
    ),
    v617 = labelled(
      v617,
      label = "How many days did you have to stop doing your usual activity due to this illness during the past 12 months?"
    )
  )
  
#Part 6.2.4 - Chronic Illness and Expenditure Tracking - Inpatient

for (i in (1:ncol(section6b4))) {
  section6b4[[i]] <- as.integer(section6b4[[i]])
}

section6b4 <- section6b4 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v604 = labelled(
      v604,
      label = "Health conditions",
      labels = c(
        "Heart Diseases" = 1, 
        Hypertension = 2, 
        Diabetes = 3, 
        "Asthma/COPD" = 4, 
        "Rheumatism/Arthritis" = 5, 
        "Kidney Diseases" = 6, 
        "Liver Diseases" = 7, 
        "Cancer" = 8, 
        "Epilepsy" = 9, 
        Tuberculosis = 10, 
        "HIV/AIDS" = 11, 
        "Thyroid Disorders" = 12, 
        "Chronic Gastrointestinal Diseases" = 13, 
        "Gynaecological Problems" = 14, 
        "Chronic Orthopaedic Problems" = 15, 
        "Neurological Conditions" = 16, 
        "Alzheimer's/Parkinson's" = 17, 
        "Mental Illness" = 18, 
        "Others (Specify)" = 19)
    ),
    v618 = labelled(
      v618,
      label = "How much have you spent in the past 12 months on the treatment of this illness?"
    ),
    v618a = labelled(
      v618a,
      label = "OOP-IPD: Emergency care"
    ),
    v618b = labelled(
      v618b,
      label = "OOP-IPD: Inpatient: bed charges"
    ),
    v618b1 = labelled(
      v618b1,
      label = "OOP-IPD: Inpatient: # of days"
    ),
    v618c = labelled(
      v618c,
      label = "OOP-IPD: Laboratory"
    ),
    v618d = labelled(
      v618d,
      label = "OOP-IPD: Imaging"
    ),
    v618e = labelled(
      v618e,
      label = "OOP-IPD: Medicines"
    ),
    v618f = labelled(
      v618f,
      label = "OOP-IPD: Medical supplies/ devices"
    ),
    v618g = labelled(
      v618g,
      label = "OOP-IPD: Transportation"
    ),
    v618h = labelled(
      v618h,
      label = "OOP-IPD: Food & accommodation"
    ),
    v618i = labelled(
      v618i,
      label = "OOP-IPD: Care giver cost"
    ),
    v618j = labelled(
      v618j,
      label = "OOP-IPD: Other costs"
    ),
    v618k = labelled(
      v618k,
      label = "OOP-IPD: Total cost"
    ),
    v619 = labelled(
      v619,
      label = "What was your main source of funds for healthcare and treatment?",
      labels = c(
        "Own savings" = 1, 
        "Loan" = 2, 
        "Borrowing" = 3, 
        "Selling asset" = 4, 
        "From family" = 5, 
        "Others" = 6
      )
    ),
    v619a = labelled(
      v619a,
      label = "Other main source of funds for healthcare and treatment"
    ),
    v620 = labelled(
      v620,
      label = "Did you have to stop doing your usual activity due to this illness during the past 12 months?",
      labels = c(
        Yes = 1, 
        No = 2
      )
    ),
    v621 = labelled(
      v621,
      label = "How many days did you have to stop doing your usual activity due to this illness during the past 12 months?"
    )
  )

#Part 6.2.5 - Chronic Illness and Care Giver Burden

for (i in (1:ncol(section6b5))) {
  section6b5[[i]] <- as.integer(section6b5[[i]])
}

section6b5 <- section6b5 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v622 = labelled(
      v622,
      label = "In the past 12 months, did …[name]… stop regular activities at any time to take care of a sick household member?",
      labels = c(
        Yes = 1, 
        No = 2
      )
    ),
    v623 = labelled(
      v623,
      label = "Who did take care of (id code)?"
    ),
    v624 = labelled(
      v624,
      label = "Who did …[name] … care for", 
      labels = c(
        "Elderly (>= 60 years)" = 1, 
        "Person with disability" = 2, 
        "Chronic patient" = 3, 
        "Other" = 4
      )
    ),
    v625 = labelled(
      v625,
      label = "In the past 12 months, how many days of regular activities were missed to take care of sick members?"
    ),
    v626 = labelled(
      v626,
      label = "What activities were affected?",
      labels = c(
        "Paid work" = 1, 
        "Farming/household chores" = 2, 
        "Children's education" = 3, 
        "Social/community activities" = 4
      )
    ),
    v627 = labelled(
      v627,
      label = "Estimated income loss due to missed paid work (npr)"
    ),
    v628 = labelled(
      v628,
      label = "Do you [Name] feel your [disease condition] is well-controlled? (Chronic Disease Control Perception)",
      labels = c(Yes = 1, No = 2)
    )
  )

#Part 6.3.1 - Acute Illness and Health Seeking Behavior

for (i in (1:ncol(section6c1))) {
  section6c1[[i]] <- as.integer(section6c1[[i]])
}

section6c1 <- section6c1 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v629 = labelled(
      v629,
      label = "Have …[name] experienced any illness, injury other than the chronic condition? (note all health condition)",
      labels = c(Yes = 1, No = 2)
    ),
    v630 = labelled(
      v630,
      label = "Which of the following health conditions has [name] been diagnosed with?",
      labels = c(
        Diarrhoea = 1, 
        Typhoid = 2, 
        Dengue = 3, 
        Malaria = 4, 
        "Acute Respiratory Infection" = 5, 
        "Cold/Flu/Fever" = 6, 
        Pneumonia = 7, 
        Measles = 8, 
        Jaundice = 9, 
        "Infection/UTI" = 10, 
        "Dental Problem" = 11, 
        "Acute Eye Infection" = 12, 
        "Acute Ear Infection" = 13, 
        "Skin Disease" = 14, 
        "Injury" = 15, 
        "Accident" = 16, 
        "Other fever" = 17, 
        "Others" = 18
      )
    ),
    v630a = labelled(
      v630a,
      label = "Others: (specify)"
    ),
    v631a = labelled(
      v631a,
      label = "How long ago did the illness or injury start (start date)"
    ),
    v631b = labelled(
      v631b,
      label = "How long did the illness or injury last for? (write end date; if still continuing select today’s date)"
    ),
    v632 = labelled(
      v632,
      label = "Did you go to a health facility/pharmacy or consult a health worker for treatment?", 
      labels = c(Yes = 1, No = 2)
    ),
    v633 = labelled(
      v633,
      label = "Did you use any insurance scheme/ government programme for the treatment of this acute illness?",
      labels = c(Yes = 1, No = 2)
    ),
    v634 = labelled(
      v634,
      label = "Which insurance scheme/government program was used to pay for treatment of this acute illness",
      labels = c(
        NHIP = 1, 
        SSF = 2, 
        "Employer provided" = 3, 
        "Privately purchased" = 4, 
        "Bank/Cooperative" = 5, 
        "Hospital/Health facility" = 6, 
        "Community-based health insurance" = 7, 
        "Hospital (army/police/civil service)" = 8,
        "Free health" = 9, 
        "Aama surakshya programme" = 10, 
        "Social Security Unit/OCMC" = 11, 
        "Others" = 12
      )
    ),
    v634a = labelled(
      v634a,
      label = "Other types of health insurance schemes/government programs was used to pay"
    ),
    v635 = labelled(
      v635,
      label = "Why did you not go to a health facility/pharmacy or consult a health worker for treatment?",
      labels = c(
        "Cannot afford treatment costs" = 1, 
        "Transportation too expensive" = 2, 
        "Medicines out of stock" = 3, 
        "Health facility too far" = 4, 
        "Long queues discourage care" = 5, 
        "Clinic hours conflict with work" = 6, 
        "Illness seemed mild" = 7, 
        "Expected self recovery" = 8, 
        "Previous treatment didn't help" = 9, 
        "No family member to accompany" = 10, 
        "Preferred traditional healers" = 11, 
        "Shamed to discuss symptoms" = 12, 
        "Other" = 13
      )
    ),
    v635a = labelled(
      v635a,
      label = "Other reasons for not going to a health facility/pharmacy or consulting a health worker for treatment?",
      labels = c(
        "Health Post" = 1, 
        "Primary Health Centre" = 2, 
        "Government Hospital" = 3, 
        "Government Outreach Clinic" = 4, 
        "Government Ayurveda Centre" = 5, 
        "Pharmacy/Drug Seller" = 6, 
        "Private clinic" = 7, 
        "Private/Community Hospital" = 8, 
        "Private Ayurveda Centre" = 9,
        "Health Worker's Home" = 10, 
        "Alternative/Traditional Healer" = 11, 
        "Abroad (India/Other)" = 12,
        "Others" = 13
      )
    ),
    v636 = labelled(
      v636,
      label = "Where did you go for consultation in relation to this illness?"
    ),
    v636a = labelled(
      v636a,
      label = "Other places to go for consultation about this illness?"
    ),
    v637 = labelled(
      v637,
      label = "In the last 1 month, when did you first seek care for each illness or injury?",
      labels = c(
        "Instantly" = 1, 
        "Within 24 hours" = 2, 
        "Within 2-3 days" = 3, 
        "Within 1 week" = 4, 
        "Within 1 to 2 weeks" = 5, 
        "More than 2 weeks" = 6
      )
    ),
    v638 = labelled(
      v638,
      label = "How did you go for consultation in relation to this illness?",
      labels = c(
        "Public transport" = 1, 
        "Taxi/Cab" = 2, 
        "Ambulance" = 3, 
        "Bicycle" = 4, 
        "Private vehicle" = 5, 
        "Walked" = 6, 
        "Aeroplane" = 7, 
        "Others" = 8
      )
    ),
    v639 = labelled(
      v639,
      label = "In the last 30 days, how long did it take you to travel (two-way) to the care provider?"
    ),
    v639a = labelled(
      v639a,
      label = "Two-way travel time: in minutes"
    ),
    v639b = labelled(
      v639b,
      label = "Two-way travel time: in hours"
    ),
    v639c = labelled(
      v639c,
      label = "Two-way travel time: in days"
    ),
    v640 = labelled(
      v640,
      label = "In the last 30 days, whom did you consult or treat with?",
      labels = c(
        "Doctor" = 1, 
        "Nurse/Mid-wife" = 2, 
        "Paramedic" = 3, 
        "Pharmacist" = 4,
        "Homeopathic/Ayurvedic" = 5, 
        "Traditional/Faith Healer" = 6, 
        "FCHV" = 7, 
        "Self/family members" = 8, 
        "Others" = 9
      )
    ),
    v640a = labelled(
      v640a,
      label = "Other personnel were consulted to treat the illness"
    ),
    v641 = labelled(
      v641,
      label = "During the health facility visit did the provider ask you any of the following. (Select all that apply)",
      labels = c(
        "Ask your feelings or symptoms" = 1, 
        "Conduct physical exam" = 2, 
        "Explain cause of illness in a way you understood" = 3, 
        "Explain the treatment or medications prescribed" = 4, 
        "Ask if you have any questions or concerns" = 5, 
        "Discuss any follow-up visits or referrals" = 6
      )
    )
  )

#Part 6.3.2 - Acute Illness and Diagnostic Tests

for (i in (1:ncol(section6c2))) {
  section6c2[[i]] <- as.integer(section6c2[[i]])
}

section6c2 <- section6c2 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v642 = labelled(
      v642,
      label = "What kind of service did you receive at the health facility for illness or injury? (select all that apply)",
      labels = c(
        "Emergency" = 1, 
        "OPD" = 2, 
        "Childbirth" = 3, 
        "Physiotherapy" = 4, 
        "Dressing" = 5, 
        "Follow up: General" = 6, 
        "Fellow up: Chronic" = 7, 
        "Immunization" = 8, 
        "Laboratory test" = 9, 
        "Diagnostic test" = 10, 
        "Others" = 11
      )
    ),
    v642a = labelled(
      v642a,
      label = "Other kinds of services received at the health facility"
    ),
    v643 = labelled(
      v643,
      label = "Did this health care provider order any of the following tests?",
      labels = c(Yes = 1, No = 2)
    ),
    v644 = labelled(
      v644,
      label = "If yes, which of the following tests prescribed?",
      labels = c(
        "Blood tests" = 1, 
        "Urine tests" = 2, 
        "Stool test" = 3, 
        "X-ray" = 4, 
        "Ultrasound" = 5, 
        "ECG" = 6, 
        "CT scan/MRI" = 7, 
        "Echo" = 8, 
        "Others" = 9
      )
    ),
    v644a = labelled(
      v644a,
      label = "Other types of tests prescribed?"
    ),
    v645 = labelled(
      v645,
      label = "Did you do the test as prescribed?",
      labels = c(Yes = 1, No = 2)
    ),
    v646 = labelled(
      v646,
      label = "Did you receive the results?",
      labels = c(
        "Blood tests" = 1, 
        "Urine tests" = 2, 
        "Stool test" = 3, 
        "X-ray" = 4, 
        "Ultrasound" = 5, 
        "ECG" = 6, 
        "CT scan/MRI" = 7, 
        "Echo" = 8, 
        "Others" = 9
      )
    ),
    v647 = labelled(
      v647,
      label = "Why did you not perform the prescribed test?",
      labels = c(
        "Could not afford the cost of the test" = 1, 
        "Test not available at local health facility" = 2, 
        "Transportation costs or distance too high" = 3, 
        "No family members available to assist" = 4, 
        "Felt the test was not necessary" = 5, 
        "Fear of test results or procedure" = 6, 
        "Long waiting times at facility" = 7, 
        "Lack of trust in healthcare provider" = 8, 
        "Equipment or supplies out of stock" = 9, 
        "Other reason" = 10
      )
    ),
    v647a = labelled(
      v647a,
      label = "Other type of reason for not performing the prescribed test"
    )
  )

#Part 6.3.3 - Acute Illness and Medication Use

for (i in (1:ncol(section6c3))) {
  section6c3[[i]] <- as.integer(section6c3[[i]])
}

section6c3 <- section6c3 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v648 = labelled(
      v648,
      label = "Health conditions (for selected disease)", 
      labels = c(
        Diarrhoea = 1, 
        "Cold/Flu/Fever" = 2, 
        "Acute Respiratory Infection" = 3, 
        Pneumonia = 4, 
        Measles = 5, 
        Jaundice = 6, 
        "Infection/UTI" = 7, 
        "Dental Problem" = 8, 
        "Acute Eye Infection" = 9, 
        "Acute Ear Infection" = 10, 
        "Skin Disease" = 11, 
        "Injury/Accident" = 12, 
        "Acute Gastritis" = 13, 
        "Other" = 14
      )
    ),
    v649 = labelled(
      v649,
      label = "How many different kinds of medicine do you have in a day?"
    ),
    v649a = labelled(
      v649a,
      label = "Medications: Diarrhoea",
      labels = c(
        ORS = 11, 
        "Zinc tablets" = 12
      )
    ),
    v649b = labelled(
      v649b,
      label = "Medications: Cold/Flu/Fever",
      labels = c(
        Paracetamol = 21, 
        Ibuprofen = 22, 
        Cetirizine = 23
      )
    ),
    v649c = labelled(
      v649c,
      label = "Medications: Acute Respiratory Infection",
      labels = c(
        Salbutamol = 31
      )
    ),
    v649d = labelled(
      v649d,
      label = "Medications: Pneumonia",
      labels = c(
        Amoxicillin = 41, 
        Azithromycin = 42, 
        Paracetamol = 43, 
        Ceftriaxone = 44
      )
    ),
    v649e = labelled(
      v649e,
      label = "Medications: Measles",
      labels = c(
        "Vitamin A" = 51, 
        Paracetamol = 52
      )
    ),
    v649f = labelled(
      v649f,
      label = "Medications: Jaundice",
      labels = c(
        Ondansetron = 61, 
        Others = 62
      )
    ),
    v649g = labelled(
      v649g,
      label = "Medications: Infection/ UTI",
      labels = c(
        Nitrofurantoin = 71, 
        Ciprofloxacin = 72, 
        Amoxicillin = 73, 
        Azithromycin = 74,
        Metronidazole = 75
      )
    ),
    v649h = labelled(
      v649h,
      label = "Medications: Dental Problem",
      labels = c(
        Paracetamol = 81, 
        Amoxicillin = 82, 
        Metronidazole = 83
      )
    ),
    v649i = labelled(
      v649i,
      label = "Medications: Acute Eye Infection",
      labels = c(
        "Chloramphenicol (eye drops)" = 91, 
        "Erythromycin (eye ointment)" = 92
      )
    ),
    v649j = labelled(
      v649j,
      label = "Medications: Acute Ear Infection",
      labels = c(
        "Ciprofloxacin (ear drops)" = 101
      )
    ),
    v649k = labelled(
      v649k,
      label = "Medications: Skin Disease",
      labels = c(
        "Mupirocin Clotrimazole" = 111
      )
    ),
    v649l = labelled(
      v649l,
      label = "Medications: Injury/ Accident", 
      labels = c(
        "Paracetamol" = 121, 
        "Diclofenac" = 122, 
        "Tetanus Toxoid" = 123, 
        "Antibiotic ointments" = 124
      )
    ),
    v649m = labelled(
      v649m,
      label = "Medications: Acute Gastritis",
      labels = c(
        Omeprazole = 131, 
        Ranitidine = 132, 
        Digen = 133
      )
    ),
    v649n = labelled(
      v649n,
      label = "Medications: Other illness",
      labels = c(
        Albendazole = 141, 
        Antihistamine = 142
      )
    ),
    v650 = labelled(
      v650,
      label = "How are you covering the cost of medicines?",
      labels = c(
        "Fully paid" = 1, 
        "Received free of cost" = 2, 
        "Fully paid through NHIP" = 3, 
        "Partially paid through NHIP" = 4, 
        "Partially paid through SSF" = 5, 
        "Other" = 6
      )
    ),
    v650a = labelled(
      v650a,
      label = "Other means of covering costs of medicines"
    )
  )


#Part 6.3.4 - Acute Illness Health Seeking and Expenditure Tracking

for (i in (1:ncol(section6c4))) {
  section6c4[[i]] <- as.integer(section6c4[[i]])
}

section6c4 <- section6c4 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v630 = labelled(
      v630,
      label = "Acute health conditions",
      labels = c(
        "Diarrhoea" = 1, 
        "Typhoid" = 2, 
        "Dengue" = 3, 
        "Malaria" = 4, 
        "Acute Respiratory Infection" = 5, 
        "Cold/Flu/Fever" = 6, 
        "Pneumonia" = 7, 
        "Measles" = 8, 
        "Jaundice" = 9, 
        "UTI" = 10, 
        "Dental Problem" = 11, 
        "Acute Eye Infection" = 12, 
        "Acute Ear Infection" = 13, 
        "Skin Disease" = 14, 
        "Injury" = 15, 
        "Accident" = 16, 
        "Other Fever" = 17, 
        "Other" = 18
      )
    ),
    v651a = labelled(
      v651a,
      label = "OOP-OPD/IPD: Emergency"
    ),
    v651b = labelled(
      v651b,
      label = "OOP-OPD/IPD: OPD/IPD"
    ),
    v651c = labelled(
      v651c,
      label = "OOP-OPD/IPD: Laboratory"
    ),
    v651d = labelled(
      v651d,
      label = "OOP-OPD/IPD: Imaging"
    ),
    v651e = labelled(
      v651e,
      label = "OOP-OPD/IPD: Medicines"
    ),
    v651f = labelled(
      v651f,
      label = "OOP-OPD/IPD: Medical supplies/ devices"
    ),
    v651g = labelled(
      v651g,
      label = "OOP-OPD/IPD: Transportation"
    ),
    v651h = labelled(
      v651h,
      label = "OOP-OPD/IPD: Food & accommodation"
    ),
    v651i = labelled(
      v651i,
      label = "OOP-OPD/IPD: Care-giver cost"
    ),
    v651j = labelled(
      v651j,
      label = "OOP-OPD/IPD: Other costs"
    ),
    v651k = labelled(
      v651k,
      label = "OOP-OPD/IPD: Total cost"
    ),
    v652 = labelled(
      v652,
      label = "What was your main source of funds for healthcare and treatment?",
      labels = c(
        "Own savings" = 1, 
        "Loan" = 2, 
        "Borrowing" = 3, 
        "From family" = 4, 
        "Selling assets" = 5,
        "Others" = 6
      )
    ),
    v652a = labelled(
      v652a,
      label = "Other main sources of funds for healthcare and treatment"
    ),
    v653 = labelled(
      v653,
      label = "Did you have to stop doing your usual activity due to this illness during the past 30 days?",
      labels = c(Yes = 1, No = 2)
    ),
    v654 = labelled(
      v654,
      label = "How many days did you have to stop doing your usual activity due to this illness during the past 30 days?"
    )
  )

#Part 6.3.5 - Acute Illness and Care Giver Burden

for (i in (1:ncol(section6c5))) {
  section6c5[[i]] <- as.integer(section6c5[[i]])
}

section6c5 <- section6c5 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v655 = labelled(
      v655,
      label = "In the past 30 days, did [NAME] stop regular activities at any time to take care of a sick household member?",
      labels = c(Yes = 1, No = 2)
    ),
    v656 = labelled(
      v656,
      label = "Do you [Name] feel your [disease condition] is well-controlled? (Acute Disease Control Perception)",
      labels = c(Yes = 1, No = 2)
    ),
    v657 = labelled(
      v657,
      label = "Who did [NAME] take care of? (ID code)"
    ),
    v658 = labelled(
      v658,
      label = "Who did [NAME] care for",
      labels = c(
        "Elderly (>=60 years)" = 1, 
        "Person with disability" = 2, 
        "Chronic patient" = 3, 
        "Others" = 4
      )
    ),
    v659 = labelled(
      v659,
      label = "In the past 30 days, how many days of regular activities did [NAME] miss to take care of sick household members?"
    ),
    v660 = labelled(
      v660,
      label = "What activities were affected?",
      labels = c(
        "Paid work" = 1, 
        "Farming/Household chores" = 2, 
        "Children's education" = 3, 
        "Social/community activities" = 4
      )
    ),
    v661 = labelled(
      v661,
      label = "Estimated income loss"
    )
  )

#Part 6.4 - Household Health Care Seeking Behavior

for (i in (1:ncol(section6d))) {
  section6d[[i]] <- as.integer(section6d[[i]])
}

section6d <- section6d %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v662 = labelled(
      v662,
      label = "In the last 12 months, how much in total did your household spend out-of-pocket for healthcare?"
    ),
    v663 = labelled(
      v663,
      label = "Of this amount, how much was reimbursed by any insurance scheme (nhip, ssf, private, etc.)?"
    ),
    v664 = labelled(
      v664,
      label = "Did your household sell any land, livestock, jewellery, or other assets to pay for healthcare in the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v665 = labelled(
      v665,
      label = "If yes: which item(s) were sold?"
    ),
    v666 = labelled(
      v666,
      label = "Did any household member take on additional work or migrate temporarily to pay for healthcare?",
      labels = c(Yes = 1, No = 2)
    ),
    v667 = labelled(
      v667,
      label = "Did your household reduce spending on food, education, or other essentials to cover health expenses?",
      labels = c(Yes = 1, No = 2)
    ),
    v668 = labelled(
      v668,
      label = "How satisfied are you with the insurance scheme in terms of coverage?"
    ),
    v668a = labelled(
      v668a,
      label = "Getting emergency care when you need it",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668b = labelled(
      v668b,
      label = "Getting doctor visits when you need them",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668c = labelled(
      v668c,
      label = "Getting tests (e.g., blood tests, X-rays) when you need them",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668d = labelled(
      v668d,
      label = "Getting medicines covered by the scheme",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668e = labelled(
      v668e,
      label = "Getting medical supplies (e.g., bandages, crutches) when you need",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668f = labelled(
      v668f,
      label = "The amount of healthcare costs the scheme pays for (e.g., how much it covers)",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668g = labelled(
      v668g,
      label = "The types of healthcare services included in the scheme (e.g., treatments you need)",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v668h = labelled(
      v668h,
      label = "Other types of coverage category",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669 = labelled(
      v669,
      label = "How satisfied are you with the insurance scheme you just identified in terms of ease of access?"
    ),
    v669a = labelled(
      v669a,
      label = "How easy it is to join or renew the NHIP/SSF scheme?",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669b = labelled(
      v669b,
      label = "How easy it is to find and reach health facilities that accept NHIP/SSF",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669c = labelled(
      v669c,
      label = "How clear and easy it is to understand information about what NHIP/SSF offers",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669d = labelled(
      v669d,
      label = "Getting help from NHIP/SSF staff or local offices when you need it",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669e = labelled(
      v669e,
      label = "Getting information or services in your local language",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669f = labelled(
      v669f,
      label = "How easy it is to travel to health facilities that accept NHIP/SSF",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669g = labelled(
      v669g,
      label = "How long you wait to get care at health facilities that accept NHIP",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669h = labelled(
      v669h,
      label = "Getting services that respect your culture, caste, or gender",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669i = labelled(
      v669i,
      label = "Being able to afford travel or other costs to use NHIP/SSF services",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v669j = labelled(
      v669j,
      label = "Getting the specific healthcare services you need at NHIP facilities",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670 = labelled(
      v670,
      label = "How satisfied are you with the NHIP/SSF in terms of responsiveness?",
    ),
    v670a = labelled(
      v670a,
      label = "Getting clear information about how much NHIP/SSF coverage you have left",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670b = labelled(
      v670b,
      label = "How easy it is to contact NHIP/SSF staff for help",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670c = labelled(
      v670c,
      label = "How quickly and helpfully NHIP/SSF responds to your questions",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670d = labelled(
      v670d,
      label = "How effectively NHIP/SSF resolves your complaints or problems",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670e = labelled(
      v670e,
      label = "How fairly and respectfully you are treated by NHIP/SSF staff and health facility staff",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670f = labelled(
      v670f,
      label = "Getting timely reminders or updates about NHIP/SSF (e.g., renewal deadlines)",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670g = labelled(
      v670g,
      label = "Getting responses from NHIP/SSF that respect your language and culture",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670h = labelled(
      v670h,
      label = "Not having to pay upfront for healthcare services covered by NHIP/SSF",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670i = labelled(
      v670i,
      label = "NHIP/SSF covering most of your healthcare costs without extra payments",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    ),
    v670j = labelled(
      v670j,
      label = "Getting support from NHIP/SSF in rural or remote areas",
      labels = c(
        "Satisfied" = 1,
        "Neutral" = 2, 
        "Not satisfied" = 3
      )
    )
  )

#SECTION 7: LABOUR AND EMPLOYMENT

section7 <- read.xlsx("dataset/Swction 7_ Labor and Employment.xlsx")

section7 <- section7 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v702 = labelled(
      v702,
      label = "During the last 7 days, did you do any work for a wage, salary, commission, tips or any other pay, even if only for one hour?",
      labels = c(Yes = 1, No = 2)
    ),
    v703 = labelled(
      v703,
      label = "During the last 7 days, did you run or do any kind of business, farming or other activity to generate income, even if only for one hour?",
      labels = c(Yes = 1, No = 2)
    ),
    v704 = labelled(
      v704,
      label = "During the last 7 days, did you help unpaid in a business owned by a household member, even if only for one hour?",
      labels = c(Yes = 1, No = 2)
    ),
    v705 = labelled(
      v705,
      label = "During the last 7 days, did you have a paid job or a business from which you were temporary absent and to which you expect to return?",
      labels = c(Yes = 1, No = 2)
    ),
    v706 = labelled(
      v706,
      label = "Including the time that you have been absent, when will you return to that same job/business that you had?",
      labels = c(
        "<= 3 months" = 1, 
        "After 3 months" = 2, 
        "Not sure when" = 3
      )
    ),
    v707 = labelled(
      v707,
      label = "Do you continue receiving an income or other returns from a job or business during this absence?",
      labels = c(Yes = 1, No = 2)
    ),
    v708 = labelled(
      v708,
      label = "In the main job/business that you had during the last days (occupation)?"
    ),
    v709a = labelled(
      v709a,
      label = "Description of main tasks or duties"
    ),
    v709b = labelled(
      v709b,
      label = "NSCO Code (Occupation)",
      labels = c(
        "Legislators, Officials & Managers" = 1, 
        "Professionals" = 2, 
        "Technicians and Associate Professionals" = 3, 
        "Clerical Support Workers" = 4, 
        "Services and Sales Workers" = 5, 
        "Skilled, Agricultural, Forestry and Fishery Workers" = 6, 
        "Craft and Related Trades Workers" = 7, 
        "Plant and Machine Operators and Assemblers" = 8, 
        "Elementary Occupations" = 9, 
        "Armed Forces Occupations" = 10
      )
    ),
    v710a = labelled(
      v710a,
      label = "In this job, what is the status of your involvement?",
      labels = c(
        Employee = 1, 
        "Paid apprentice/Intern" = 2, 
        "Employer (with regular employees)" = 3, 
        "Own-account worker (without regular employees)" = 4,
        "Contributing family worker (Helping without pay)" = 5, 
        "Others" = 6
      )
    ),
    v710b = labelled(
      v710b,
      label = "Others (specify)"
    ),
    v711 = labelled(
      v711,
      label = "Does your employer pay contributions for social security (insurance, provident fund, etc.) On your behalf?",
      labels = c(Yes = 1, No = 2)
    ),
    v712 = labelled(
      v712,
      label = "Do you get paid annual leave or payment for leave not taken?",
      labels = c(Yes = 1, No = 2)
    ),
    v713 = labelled(
      v713,
      label = "Do you get paid sick leave or compensation in case of illness or injury?",
      labels = c(Yes = 1, No = 2)
    ),
    v714a = labelled(
      v714a,
      label = "What are the main goods or services produced at your place of work or its main function (Description)?"
    ),
    v714b = labelled(
      v714b,
      label = "NSIC Code (Economic Sector)", 
      labels = c(
        "Agriculture, forestry and fishing" = 1, 
        "Mining and quarrying" = 2, 
        "Manufacturing" = 3, 
        "Electricty, gas, steam and air conditioning supply" = 4, 
        "Water supply, sewerage, waste management and remediation activities" = 5,
        "Construction" = 6, 
        "Wholesale and retail trade" = 7, 
        "Transportation and storage" = 8, 
        "Accomodation and food service activities" = 9, 
        "Publishing, broadcasting, and content production and distribution activities" = 10,
        "Telecommunications, computer programming, consultancy, computing infrastructure, and other information service activities" = 11, 
        "Financial and insurance activities" = 12, 
        "Real estate activities" = 13, 
        "Professional, scientific and technical activities" = 14,
        "Administrative and support service activities" = 15, 
        "Public administration and defence; compulsory social security" = 16, 
        "Education" = 17, 
        "Human health and social work activities" = 18, 
        "Arts, sports and recreation" = 19, 
        "Other service activities" = 20, 
        "Activities of households as employers" = 21, 
        "Activities of extraterritorial organizations and bodies" = 22
      )
    ),
    v715 = labelled(
      v715,
      label = "What kind of sector was your main activity carried out in?",
      labels = c(
        "Government" = 1, 
        "Govt. Financial Institution" = 2, 
        "Govt. Non-financial Institution" = 3, 
        "Govt. Non-Profit Making Institution" = 4, 
        "Private Financial Institution" = 5, 
        "Private Non-Financial Institution" = 6, 
        "Non-Govt., Non-Profit Making Institution" = 7, 
        "Household Sector" = 8, 
        "Other" = 9
      )
    ),
    v716 = labelled(
      v716,
      label = "What is the type of enterprise/business where you work?", 
      labels = c(
        "An incorporated company" = 1, 
        "Private sector" = 2
      )
    ),
    v717 = labelled(
      v717,
      label = "Is the business registered with (relevant authority)?",
      labels = c(Yes = 1, No = 2)
    ),
    v718 = labelled(
      v718,
      label = "During the last 30 days, did you look for any kind of paid job or try to start any kind of business?",
      labels = c(Yes = 1, No = 2)
    ),
    v719 = labelled(
      v719,
      label = "Have you already found a job or arranged to start a business in the near future?",
      labels = c(Yes = 1, No = 2)
    ),
    v720 = labelled(
      v720,
      label = "What did you do in the last 30 days to find a job or start a business?",
      labels = c(
        "Applied to prospective employers" = 1, 
        "Placed/answered job advertisements" = 2, 
        "Registered with employment centre" = 3, 
        "Regsitered with private recruitment offices" = 4, 
        "Took a test or an interview" = 5, 
        "Sought help from relatives, friends, others" = 6, 
        "Checked at factories, work sites" = 7, 
        "Waited on the streets to be recruited" = 8, 
        "Sought financial help to start a business" = 9, 
        "Looked for land, building, etc. to start a business" = 10, 
        "Applied for permit/license to start a business" = 11, 
        "Other" = 12
      )
    ),
    v721 = labelled(
      v721,
      label = "Would you want to work if a job or business opportunity became available?",
      labels = c(Yes = 1, No = 2)
    ),
    v722 = labelled(
      v722,
      label = "If (a/the) job or business opportunity became available, when could you start working or have started working?", 
      labels = c(
        "During the last 7 days" = 1, 
        "Within the next 15 days" = 2, 
        "Not available" = 3 
      )
    )
  )

#SECTION 8 : WAGE JOBS

section8 <- read.xlsx("dataset/Section 8_ Wage Jobs.xlsx")

section8 <- section8 %>% 
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v801 = labelled(
      v801,
      label = "Id code from household roster"
    ),
    v802 = labelled(
      v802,
      label = "Did you work for salary or wages in the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v803a = labelled(
      v803a,
      label = "Job ID"
    ),
    v803b = labelled(
      v803b,
      label = "Description of main tasks or duties"
    ),
    v803c = labelled(
      v803c,
      label = "NSCO Code (Occupation)",
      labels = c(
        "Legislators, Officials & Managers" = 1, 
        "Professionals" = 2, 
        "Technicians and Associate Professionals" = 3, 
        "Clerical Support Workers" = 4, 
        "Services and Sales Workers" = 5, 
        "Skilled, Agricultural, Forestry and Fishery Workers" = 6, 
        "Craft and Related Trades Workers" = 7, 
        "Plant and Machine Operators and Assemblers" = 8, 
        "Elementary Occupations" = 9, 
        "Armed Forces Occupations" = 10
      )
    ),
    v804 = labelled(
      v804,
      label = "On what basis are you working/worked in this job?",
      labels = c(
        "Daily basis" = 1, 
        "Long term basis" = 2, 
        "Contract/Piece-rate" = 3
      )
    ),
    v805 = labelled(
      v805,
      label = "How long did you work for daily wages in the last 12 months (in days)?"
    ),
    v806 = labelled(
      v806,
      label = "How much did you get in cash per day for this job (in NPR)?"
    ),
    v807 = labelled(
      v807,
      label = "What was the value of what you received per day in-kind for this job? NPR Per Day"
    ),
    v808a = labelled(
      v808a,
      label = "How much did you get for this job? Take-home pay or Salary"
    ),
    v808b = labelled(
      v808b,
      label = "Transportation allowance"
    ),
    v808c = labelled(
      v808c,
      label = "Bonuses, tips, festival allowances"
    ),
    v808d = labelled(
      v808d,
      label = "Uniform / clothing"
    ),
    v808e = labelled(
      v808e,
      label = "Other allowances"
    ),
    v809 = labelled(
      v809,
      label = "What was the value of what you received in kind in the past 12 months?"
    ),
    v810a = labelled(
      v810a,
      label = "During the past 12 months, having worked on a contract, how much did you receive in cash?"
    ),
    v810b = labelled(
      v810b,
      label = "During the past 12 months, having worked on a contract, how much did you receive in-kind (value)?"
    )
  )

#SECTION 9: FARMING AND LIVESTOCK

section9a <- read.xlsx("dataset/section 9.xlsx")
section9b <- read.xlsx("dataset/Part 9_2_ Landholding  Increase Decrease.xlsx")
section9c <- read.xlsx("dataset/Part 9_3_ Production and Uses.xlsx")
section9d <- read.xlsx("dataset/Part 9_4_ Expenditure.xlsx")
section9e <- read.xlsx("dataset/Part 9_5_ Livestock.xlsx")
section9f1 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure (1).xlsx")
section9f2 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure.xlsx")

#Part 9.1: Land Holding 

section9a <- section9a %>% 
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v901 = labelled(
      v901,
      label = "Do you or does any of your household member (even if absent) own or share cropped in/out any agricultural land?",
      labels = c(Yes = 1, No = 2)
    ),
    v902a = labelled(
      v902a,
      label = "Parcel ID"
    ),
    v902b = labelled(
      v902b,
      label = "Name of parcels of agricultural land that the household/family member operates"
    ),
    v903 = labelled(
      v903,
      label = "Over the past agricultural year what did you do with the [parcel]?",
      labels = c(
        "Owned and cropped yourself" = 1, 
        "Sharecropped out" = 2, 
        "Fixed rent out" = 3, 
        "Mortgaged-out" = 4, 
        "Left fallow" = 5, 
        "Sharecropped-in" = 6, 
        "Rented-in" = 7, 
        "Mortgaged-in" = 8
      )
    ),
    v904a = labelled(
      v904a,
      label = "Unit of [parcel] area",
      labels = c(Ropani = 1, Bigha = 2)
    ),
    v904b = labelled(
      v904b,
      label = "Area of the [parcel] - Ropani/Bigha"
    ),
    v904c = labelled(
      v904c,
      label = "Area of the [parcel] - Aana/Kattha"
    ),
    v904d = labelled(
      v904d,
      label = "Area of the [parcel] - Paisa/Dhur"
    ),
    v905 = labelled(
      v905,
      label = "Where is this [parcel] located (district)?"
    ),
    v906 = labelled(
      v906,
      label = "If you wanted to buy/sell a [parcel] exactly like this, how much would it cost/fetch you?"
    ),
    v907a = labelled(
      v907a,
      label = "For the parcels which you did not crop yourself, what net rent did you receive/pay from/to the tenant? (In cash NPR)"
    ),
    v907b = labelled(
      v907b,
      label = "For the parcels which you did not crop yourself, what net rent did you receive/pay from/to the tenant? (In-Kind NPR)"
    )
  )

#Part 9.2 - Landholding - Increase/Decrease

section9b <- section9b %>% 
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v908 = labelled(
      v908,
      label = "Did your household sell/transfer any farmland over the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v909 = labelled(
      v909,
      label = "How much land did your household sell/transfer?"
    ),
    v909a = labelled(
      v909a,
      label = "Unit of land sell/transfer", 
      labels = c(Ropani = 1, Bigha = 2)
    ),
    v909b = labelled(
      v909b,
      label = "Area of land sell/transfer - Ropani/Bigha"
    ),
    v909c = labelled(
      v909c,
      label = "Area of land sell/transfer - Aana/Kattha"
    ),
    v909d = labelled(
      v909d,
      label = "Area of land sell/transfer - Paisa/Dhur"
    ),
    v910 = labelled(
      v910,
      label = "How much did your household receive from the sales?"
    ),
    v911 = labelled(
      v911,
      label = "Did your household buy/get any agricultural land over the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v912a = labelled(
      v912a,
      label = "Unit of land buy/get",
      labels = c(Ropani = 1, Bigha = 2)
    ),
    v912b = labelled(
      v912b,
      label = "Area of land buy/get - Ropani/Bigha"
    ),
    v912c = labelled(
      v912c,
      label = "Area of land buy/get - Aana/Kattha"
    ),
    v912d = labelled(
      v912d,
      label = "Area of land buy/get - Paisa/Dhur"
    ),
    v913 = labelled(
      v913,
      label = "How much did your household pay for this land?"
    )
  )

#Part 9.3 - Production and Uses

section9c <- section9c %>% 
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v914a = labelled(
      v914a,
      label = "Crop description"
    ),
    v914b = labelled(
      v914b,
      label = "Crop code",
      labels = c(
        "Early Paddy" = 1, 
        "Main Paddy" = 2, 
        "Upland Paddy" = 3, 
        "Wheat" = 4, 
        "Spring/Winter Maize" = 5, 
        "Summer Maize" = 6, 
        "Millet" = 7, 
        "Barley" = 8, 
        "Buckwheat" = 9, 
        "Other cereals" = 10,
        "Soybeans" = 11, 
        "Black Gram" = 12, 
        "Red Gram" = 13, 
        "Grass Pea" = 14, 
        "Lentil" = 15, 
        "Gram" = 16, 
        "Pea" = 17, 
        "Green Gram" = 18, 
        "Coarse Gram" = 19, 
        "Cow Pea" = 20, 
        "Other legumes" = 21, 
        "Winter Potato" = 22, 
        "Summer Potato" = 23,
        "Sweet Potato" = 24, 
        "Colocasia" = 25, 
        "Other tubers" = 26, 
        "Mustard" = 27, 
        "Ground nut" = 28, 
        "Linseed" = 29, 
        "Sesame" = 30, 
        "Other oilseed" = 31, 
        "Sugarcane" = 32, 
        "Jute" = 33, 
        "Tobacco" = 34, 
        "Other cash crop" = 35,
        "Chillies" = 36, 
        "Onions" = 37, 
        "Garlic" = 38, 
        "Ginger" = 39, 
        "Turmeric" = 40, 
        "Cardamom" = 41, 
        "Coriander seed" = 42, 
        "Other Spices" = 43, 
        "Winter vegetables" = 44, 
        "Summer vegetables" = 45, 
        "Orange" = 46, 
        "Lemon" = 47, 
        "Lime" = 48, 
        "Sweet Lime" = 49, 
        "Other citrus" = 50, 
        "Mango" = 51, 
        "Banana" = 52, 
        "Guava" = 53, 
        "Jackfruit" = 54, 
        "Pineapple" = 55, 
        "Lichee" = 56, 
        "Pear" = 57, 
        "Apple" = 58, 
        "Plum" = 59, 
        "Papaya" = 60,
        "Pomegranate" = 61, 
        "Other Fruit" = 62, 
        "Tea" = 63, 
        "Straw/Grass" = 64, 
        "Fodder Trees" = 65, 
        "Bamboo" = 66, 
        "Other trees" = 67, 
        "Floriculture" = 68, 
        "Seed production" = 69, 
        "Plant/Saplings" = 70
      )
    ),
    v915 = labelled(
      v915,
      label = "What is the main purpose of cultivation [crop]?",
      labels = c(
        "Own consumption" = 1, 
        "For sale" = 2
      )
    ),
    v916 = labelled(
      v916,
      label = "Did you use an improved variety of seed of [crop]?",
      labels = c(Yes = 1, No = 2)
    ),
    v917a = labelled(
      v917a,
      label = "Unit of production of [crop]",
      labels = c(
        "Kilogram" = 1, 
        "Maund" = 2, 
        "Muri" = 3, 
        "Quintal" = 4, 
        "Gota (Piece)" = 5
      )
    ),
    v917b = labelled(
      v917b,
      label = "Total Quantity Harvested"
    ),
    v917c = labelled(
      v917c,
      label = "Quantity given to landlord"
    ),
    v917d = labelled(
      v917d,
      label = "Quantity sold (or expected to sell)"
    ),
    v918a = labelled(
      v918a,
      label = "Unit of sales reported for [crop]",
      labels = c(
        "Kilogram" = 1, 
        "Maund" = 2, 
        "Muri" = 3, 
        "Quintal" = 4, 
        "Gota (Piece)" = 5
      )
    ),
    v918b = labelled(
      v918b,
      label = "Total Quantity sold"
    ),
    v918c = labelled(
      v918c,
      label = "Price per unit in NPR"
    ),
    v918d = labelled(
      v918d,
      label = "Total Sales (NPR)"
    )
  )

#Part 9.4 - Expenditure on Agriculture 

section9d <- section9d %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v919 = labelled(
      v919,
      label = "Did you purchase or receive any seeds or young plants over the past agriculture year?",
      labels = c(Yes = 1, No = 2)
    ),
    v920 = labelled(
      v920,
      label = "How much did you spend on buying seeds or young plants?"
    ),
    v921 = labelled(
      v921,
      label = "How much did you spend on transportation costs for seeds/young plants?"
    ),
    v922 = labelled(
      v922,
      label = "Did you use any fertilizers or insecticides you purchased/received over the past agriculture year?",
      labels = c(Yes = 1, No = 2)
    ),
    v923 = labelled(
      v923,
      label = "How much did you spend on buying fertilizers or insecticides?"
    ),
    v924 = labelled(
      v924,
      label = "How much did you spend on transportation costs for fertilizers/insecticides?"
    ),
    v925 = labelled(
      v925,
      label = "Did you hire/exchange any casual farm workers over the past agriculture year?",
      labels = c(Yes = 1, No = 2)
    ),
    v926 = labelled(
      v926,
      label = "How many such workers (including permanent farm workers) did you hire in total over the past agriculture year (total man-days)?"
    ),
    v927 = labelled(
      v927,
      label = "Total expenditure on hiring farm labour"
    ),
    v928 = labelled(
      v928,
      label = "Irrigation charges/maintenance of water source, etc."
    ),
    v929 = labelled(
      v929,
      label = "Improvements on land or buildings"
    ),
    v930 = labelled(
      v930,
      label = "Repair and maintenance of equipment"
    ),
    v931 = labelled(
      v931,
      label = "Crop insurance (premium)"
    ),
    v932a = labelled(
      v932a,
      label = "Expenditure on renting in: Draft animals, carts, etc."
    ),
    v932b = labelled(
      v932b,
      label = "Expenditure on renting in: Tractor/power tiller/combined harvester"
    ),
    v932c = labelled(
      v932c,
      label = "Expenditure on renting in: Thrasher/other machinery"
    ),
    v932d = labelled(
      v932d,
      label = "Expenditure on renting in: Other expenditure"
    )
  )

#Part 9.5 -  Livestock - Ownership 

section9e <- section9e %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v933 = labelled(
      v933,
      label = "Has your household owned any livestock over the past 12 months?", 
      labels = c(Yes = 1, No = 2)
    ),
    v934a = labelled(
      v934a,
      label = "Livestock code", 
      labels = c(
        "Bullocks/Cows" = 1, 
        "He/she Buffaloes" = 2, 
        "Goats/Mountain goats" = 3, 
        "He/She sheep" = 4, 
        "Yaks/Naks" = 5, 
        "Pigs/Boards" = 6, 
        "Horses/Donkeys/Mules" = 7, 
        "Poultry/Ducks/Pigeons" = 8, 
        "Other livestock" = 9, 
        "Fish" = 10
      )
    ),
    v934 = labelled(
      v934,
      label = "Did you own any ..[animals].. over the past 12 months?"
    ),
    v935 = labelled(
      v935,
      label = "What is the main purpose of raising livestock?", 
      labels = c(
        "Own consumption" = 1, 
        "For Sale" = 2
      )
    ),
    v936a = labelled(
      v936a,
      label = "How many do you own now? (Number)"
    ),
    v936b = labelled(
      v936b,
      label = "For how much could you buy them all today? (NPR)"
    ),
    v937a = labelled(
      v937a,
      label = "How many did you have 12 months ago? (Number)"
    ),
    v937b = labelled(
      v937b,
      label = "For how much could you have bought them all then? (NPR)"
    ),
    v938a = labelled(
      v938a,
      label = "How many did you sell over the past 12 months? (Number)"
    ),
    v938b = labelled(
      v938b,
      label = "How much did you sell them for? (NPR)"
    ),
    v939a = labelled(
      v939a,
      label = "How many did you buy over the past 12 months? (Number)"
    ),
    v939b = labelled(
      v939b,
      label = "How much did you pay for them? (NPR)"
    )
  )

#Part 9.6.1 - Livestock Income

section9f1 <- section9f1 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v940 = labelled(
      v940, 
      label = "Livestock code",
      labels = c(
        "Bullocks/Cows" = 1, 
        "He/she Buffaloes" = 2, 
        "Goats/Mountain goats" = 3, 
        "He/She sheep" = 4, 
        "Yaks/Naks" = 5, 
        "Pigs/Boards" = 6, 
        "Horses/Donkeys/Mules" = 7, 
        "Poultry/Ducks/Pigeons" = 8, 
        "Other livestock" = 9, 
        "Fish" = 10
      )
    ), 
    v941 = labelled(
      v941, 
      label = "Total income over past 12 months (NPR)"
    )
  )

#Part 9.6.2 - Livestock Expenditure

section9f2 <- section9f2 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v942 = labelled(
      v942, 
      label = "Livestock code",
      labels = c(
        "Bullocks/Cows" = 1, 
        "He/she Buffaloes" = 2, 
        "Goats/Mountain goats" = 3, 
        "He/She sheep" = 4, 
        "Yaks/Naks" = 5, 
        "Pigs/Boards" = 6, 
        "Horses/Donkeys/Mules" = 7, 
        "Poultry/Ducks/Pigeons" = 8, 
        "Other livestock" = 9, 
        "Fish" = 10
      )
    ), 
    v943 = labelled(
      v943, 
      label = "Total expenditure over past 12 months (NPR)"
    )
  )

#SECTION 10 : INCOME FROM NON-AGRICULTURAL ENTERPRISES

section10 <- read.xlsx("dataset/Income from Non - Agricultural Enterprises.xlsx")

section10 <- section10 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v1001 = labelled(
      v1001,
      label = "Did you or your family operate any non-agricultural enterprise in the past 12 months?"
    ),
    v1002a = labelled(
      v1002a,
      label = "Description"
    ),
    v1002b = labelled(
      v1002b,
      label = "ISIC"
    ),
    v1002c = labelled(
      v1002c,
      label = "Produced goods/services"
    ),
    v1003 = labelled(
      v1003,
      label = "What is the ownership of these enterprises?", 
      labels = c(
        Own = 1, 
        Joint = 2
      )
    ),
    v1004 = labelled(
      v1004,
      label = "What share of the profits is kept by your household?"
    ),
    v1005 = labelled(
      v1005,
      label = "Gross revenues over the past 12 months (from sales)"
    ),
    v1006 = labelled(
      v1006,
      label = "Did you hire on salary/ wage anyone over the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v1007 = labelled(
      v1007,
      label = "Expenditures on wages / salary both cash and in-kind"
    ),
    v1008 = labelled(
      v1008,
      label = "Expenditure on fuel (kerosene, electricity, coal, firewood, etc.)"
    ),
    v1009a = labelled(
      v1009a,
      label = "Expenditure on raw materials: Cash (NPR)"
    ),
    v1009b = labelled(
      v1009b,
      label = "Expenditure on raw materials: In-kind (NPR)"
    ),
    v1010 = labelled(
      v1010,
      label = "Other operating expenses (NPR)"
    ),
    v1011 = labelled(
      v1011,
      label = "Net revenues over past 12 months (NPR)"
    ),
    v1012 = labelled(
      v1012,
      label = "Expenditure on capital goods over past 12 months (NPR)"
    ),
    v1013 = labelled(
      v1013,
      label = "Sale of assets over past 12 months (NPR)"
    ),
    v1014 = labelled(
      v1014,
      label = "If someone wanted to buy this enterprise today, how much would s/he have to pay? (NPR)"
    ),
    v1015 = labelled(
      v1015,
      label = "If someone was to buy this business a year ago, how much would s/he had to pay? (NPR)"
    )
  )

#SECTION 11 : CREDIT AND SAVINGS

section11a <- read.xlsx("dataset/section 11.xlsx")
section11b <- read.xlsx("dataset/Part 11_2_ Lending and Outstanding Loans.xlsx")
section11c <- read.xlsx("dataset/Part 11_3_ Other Assets.xlsx")

#Part 11.1 - Borrowing and Outstanding Loans

section11 <- section11 %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v1101 = labelled(
      v1101,
      label = "Does anyone in your household currently have loans, or have you taken out any loans in the past 12 months (even if repaid)?",
      labels = c(Yes = 1, No = 2)
    ),
    v1102 = labelled(
      v1102,
      label = "Description of household loan"
    ),
    v1103 = labelled(
      v1103,
      label = "ID Code of primary borrower"
    ),
    v1104a = labelled(
      v1104a,
      label = "When did you get the loan (in months)?"
    ),
    v1104b = labelled(
      v1104b,
      label = "When did you get the loan (in years)?"
    ),
    v1105 = labelled(
      v1105,
      label = "From whom did you obtain the loan?",
      labels = c(
        "Commercial Bank" = 1, 
        "Development Bank" = 2, 
        "Financial Company" = 3, 
        "Micro-Finance" = 4, 
        "Cooperatives" = 5, 
        "Employees Provident Fund" = 6, 
        "Citizens' Investment Trust" = 7, 
        "NGO or Relief Agency" = 8, 
        "Landlord/Employer" = 9, 
        "Shopkeeper/Money Lender" = 10, 
        "Relatives/Friends/Neighbours" = 11, 
        "Other" = 12
      )
    ),
    v1106 = labelled(
      v1106,
      label = "How much in total did you borrow?"
    ),
    v1107a = labelled(
      v1107a,
      label = "What is/was the interest on the loan? (NPR)"
    ),
    v1107b = labelled(
      v1107b,
      label = "What is/was the interest rate on the loan? (percent per year)"
    ),
    v1108a = labelled(
      v1108a,
      label = "By when did / do you have to pay the loan? - Month"
    ),
    v1108b = labelled(
      v1108b,
      label = "By when did / do you have to pay the loan? - Year"
    ),
    v1109 = labelled(
      v1109,
      label = "Have you repaid the loan over the last 12 months?",
      labels = c(
        "Fully Paid" = 1, 
        "Partly Paid" = 2, 
        "Not Paid At All" = 3 
      )
    ),
    v1110 = labelled(
      v1110,
      label = "How much have you repaid in principal and interest? (NPR)"
    )
  )

#Part 11.2 - Lending and Outstanding Loans 

section11b <- section11b %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v1111 = labelled(
      v1111,
      label = "Does anyone outside your household currently owe money to your household, or have they repaid any loans to you in the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v1112 = labelled(
      v1112,
      label = "Description of Loan"
    ),
    v1113 = labelled(
      v1113,
      label = "ID Code of the primary lender"
    ),
    v1114a = labelled(
      v1114a,
      label = "When was the loan made? (Month)"
    ),
    v1114b = labelled(
      v1114b,
      label = "When was the loan made? (Year)"
    ),
    v1115 = labelled(
      v1115,
      label = "What is the relationship of the borrower to the primary lender?",
      labels = c(
        "Employee or Tenant Farmer" = 1, 
        "Business Customer" = 2, 
        "Other Business Associate" = 3, 
        "Friend/Neighbour" = 4, 
        "Relative" = 5, 
        "Other" = 6
      )
    ),
    v1116 = labelled(
      v1116,
      label = "How much in total did you lend? (NPR)"
    ),
    v1117a = labelled(
      v1117a,
      label = "What is/was the interest on the loan? (NPR)"
    ),
    v1117b = labelled(
      v1117b,
      label = "What is/was the interest rate on the loan? (percent per year)"
    ),
    v1118a = labelled(
      v1118a,
      label = "When is/was the borrower scheduled to finish repaying the loan? (Month)"
    ),
    v1118b = labelled(
      v1118b,
      label = "When is/was the borrower scheduled to finish repaying the loan? (Year)"
    ),
    v1119 = labelled(
      v1119,
      label = "Has the borrower finished repaying the loan?",
       labels = c(
        "Fully Paid" = 1, 
        "Partly Paid" = 2, 
        "Not Paid At All" = 3 
      )
    ),
    v1120 = labelled(
      v1120,
      label = "How much in total has been repaid on the loan? (NPR)"
    )
  )

#Part 11.3 - Other Assets 

section11c <- section11c %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v1121 = labelled(
      v1121,
      label = "Does your household own any land or property (dwelling)?",
      labels = c(Yes = 1, No = 2)
    ),
    v1122 = labelled(
      v1122,
      label = "How much money would it cost to buy property owned by your household?"
    ),
    v1123 = labelled(
      v1123,
      label = "How much money would it have cost a year ago to buy the property that your household now owns?"
    ),
    v1124 = labelled(
      v1124,
      label = "How much did your household spend in total over the past 12 months purchasing property?"
    ),
    v1125 = labelled(
      v1125,
      label = "How much did your household receive in total over the past 12 months from selling property?"
    ),
    v1126 = labelled(
      v1126,
      label = "How much did your household receive in total over the past 12 months from renting out property?"
    ),
    v1127 = labelled(
      v1127,
      label = "Does your household own any other real assets other than land and dwellings?",
      labels = c(Yes = 1, No = 2)
    ),
    v1128 = labelled(
      v1128,
      label = "How much money would it cost to buy assets owned by your household?"
    ),
    v1129 = labelled(
      v1129,
      label = "How much money would it have cost a year ago to buy the assets that your household now owns?"
    ),
    v1130 = labelled(
      v1130,
      label = "How much did your household spend in total over the past 12 months in purchasing these assets?"
    ),
    v1131 = labelled(
      v1131,
      label = "How much did your household receive in total over the past 12 months from selling these assets?"
    ),
    v1132 = labelled(
      v1132,
      label = "How much did your household receive in total over the past 12 months from renting these assets to others?"
    )
  )

#SECTION 12: REMITTANCES AND TRANSFER

section12a <- read.xlsx("dataset/Remittance and transfer.xlsx")
section12b <- read.xlsx("dataset/Part 12_2. Other Remittances.xlsx")

#Part 12.1 - Remittances and Transfer Income Received and Sent

section12a <- section12a %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v101 = labelled(
      v101,
      label = "Identification code"
    ),
    v1201 = labelled(
      v1201,
      label = "Are any former household members expected to rejoin your household?",
      labels = c(Yes = 1, No = 2)
    ),
    v1202 = labelled(
      v1202,
      label = "Name of Absentee"
    ),
    v1203 = labelled(
      v1203,
      label = "How old was the .. [PERSON].. when they last left the household?"
    ),
    v1204 = labelled(
      v1204,
      label = "How many years ago did ..[PERSON].. leave this household?"
    ),
    v1205 = labelled(
      v1205,
      label = "What was the main reason ..[PERSON].. left this household?",
      labels = c(
        "Together with family/relatives" = 1, 
        "Education" = 2, 
        "Looking for work" = 3, 
        "Start new job" = 4, 
        "Start new business" = 5, 
        "Other" = 6
      )
    ),
    v1206 = labelled(
      v1206,
      label = "Where does ..[PERSON].. live now?"
    ),
    v1207 = labelled(
      v1207,
      label = "What is ..[PERSON]'s primary occupation or activity now?",
      labels = c(
        "Wage job" = 1, 
        "Self-employed" = 2, 
        "Household work" = 3, 
        "Student" = 4, 
        "Not working" = 5
      )
    ),
    v1208 = labelled(
      v1208,
      label = "During the past 12 months, have the members of this household received money or goods from ..[PERSON]..?",
      labels = c(Yes = 1, No = 2)
    ),
    v1209 = labelled(
      v1209,
      label = "How many times did the members of this household receive money or goods from ..[PERSON].. during the past 12 months?"
    ),
    v1210 = labelled(
      v1210,
      label = "How much money did the household members receive from ..[PERSON].. during the past 12 months?"
    ),
    v1211 = labelled(
      v1211,
      label = "What is the value of all goods received by the household members from ..[PERSON].. during the past 12 months?"
    ),
    v1212 = labelled(
      v1212,
      label = "How much in total has been sent (cash & in-kind) by the household members to ..[PERSON].. during the past 12 months?"
    )
  )

#Part 12.2 - Other Remittances 

section12b <- section12b %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v1213 = labelled(
      v1213, 
      label = "During the past 12 months, did you or any member of your household send money or other in-kind gifts to other than absent members?"
    ),
    v1214 = labelled(
      v1214, 
      label = "During the past 12 months, have you received any money or in-kind gifts from any person who is not an absentee member of your household?"
    )
  )

#SECTION 13: TRANSFERS, SOCIAL ASSISTANCE AND OTHER INCOME 

section13a <- read.xlsx("dataset/section 13.xlsx")
section13b <- read.xlsx("dataset/Part 13_2_ Social Assistance.xlsx")
section13c <- read.xlsx("dataset/Part 13_3_ Other Income.xlsx")

#Part 13.1 - Cash Transfer Programs

section13a <- section13a %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
    ),
    v1301 = labelled(
      v1301,
      label = "Cash transfer programmes",
      labels = c(
        "Senior citizen allowance" = 1, 
        "Single woman allowance" = 2, 
        "Full disability allowance" = 3, 
        "Partial disability allowance" = 4, 
        "Endangered ethnicities allowance" = 5, 
        "Child grant" = 6, 
        "Aama Surakshya Programme" = 7, 
        "Martyr's family benefits" = 8, 
        "Conflict victims' benefits" = 9, 
        "Unemployment benefits" = 10, 
        "Earthquake disaster relief" = 11, 
        "Flood/landslide disaster relief" = 12, 
        "Other disaster relief" = 13, 
        "Agricultural Subsidy (cash)" = 14, 
        "Other cash assistance" = 15
      )
    ),
    v1302 = labelled(
      v1302,
      label = "Did any of the household members receive payment from ..[SOURCE].. during the past 12 months?",
      labels = c(
        Yes = 1, 
        No = 2, 
        "Not Applicable" = 3
      )
    ),
    v1303 = labelled(
      v1303,
      label = "How many household members are receiving the payments from ..[SOURCE]..?"
    ),
    v1304a = labelled(
      v1304a,
      label = "Which household members? - IDCODE1"
    ),
    v1304b = labelled(
      v1304b,
      label = "Which household members? - IDCODE2"
    ),
    v1304c = labelled(
      v1304c,
      label = "Which household members? - IDCODE3"
    ),
    v1304d = labelled(
      v1304d,
      label = "Which household members? - IDCODE4"
    ),
    v1305 = labelled(
      v1305,
      label = "Amount received by the household members in last 12 months"
    ),
    v1306 = labelled(
      v1306,
      label = "What was the mode of payment for ..[SOURCE]..?",
      labels = c(
        "Paid Cash" = 1, 
        "Bank Deposit" = 2
      )
    ),
    v1307 = labelled(
      v1307,
      label = "What was the source of the benefit? (for sources 11-14 and 15 only)",
      labels = c(
        "Government" = 1, 
        "Non-Profit (I/NGO)" = 2, 
        "Private" = 3, 
        "Don't know" = 4
      )
    )
  )

#Part 13.2 - Social Assistance

section13b <- section13b %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v1308 = labelled(
      v1308, 
      label = "In-kind transfer programs",
      labels = c(
        "Public Food Distribution System" = 1, 
        "Nutritional Supplement program for children" = 2, 
        "Nutritional supplement program for mothers" = 3, 
        "Midday meals" = 4, 
        "Earthquake disaster relief" = 5, 
        "Flood/landslide victims' relief" = 6, 
        "Other disaster relief" = 7, 
        "Agriculture subsidy (in-kind)" = 8, 
        "Other in-kind assistance" = 9,
        "Prime Minister's Employment Program (PMEP)" = 10, 
        "Other public works program" = 11
      )
    ), 
    v1309 = labelled(
      v1309, 
      label = "Did any of the household members participate in or receive any benefits from ..[PROGRAM].. during the past 12 months?",
      labels = c(Yes = 1, No = 2)
    ),
    v1310 = labelled(
      v1310, 
      label = "What was the source of the benefits program?",
      labels = c(
        "Government" = 1, 
        "Non-Profit (I/NGO)" = 2, 
        "Private" = 3, 
        "Don't know" = 4 
      )
    )
  )

#Part 13.3 - Other Income

section13c <- section13c %>%
  mutate(
    psu = labelled(
      psu,
      label = "PSU number"
    ),
    palika = labelled(
      palika,
      label = "Local level"
    ),
    ward = labelled(
      ward,
      label = "Ward number"
    ),
    hhld = labelled(
      hhld,
      label = "Household number"
    ),
    v1311a = labelled(
      v1311a, 
      label = "Income source",
      labels = c(
        "Savings account" = 1, 
        "Fixed deposit account" = 2, 
        "Stocks, shares, treasury bills, etc." = 3, 
        "Employee provident fund/citizen investment trust" = 4, 
        "Pension received from within country" = 5, 
        "Pension received from abroad" = 6, 
        "Commission fee, royalties, etc." = 7,
        "Gratuity, separation payment, retirement benefits" = 8, 
        "Insurance (life and non-life) income" = 9, 
        "Income from rent of property (equipment/machinery)" = 10, 
        "Other income" = 11
      )
    ),
    v1311b = labelled(
      v1311b, 
      label = "Does any of your household member has income ..[ITEM].. source?",
      labels = c(Yes = 1, No = 2)
    ),
    v1312 = labelled(
      v1312, 
      label = "How much has the household received from ..[ITEM].. in the past 12 months?"
    )
  )
