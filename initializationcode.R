if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)

#importing all the datasets
section0 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet")
section1a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 1")
section1b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 2")
section2a1 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 3")
section2a2 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 4")
section2a3 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 5")
section2b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 6")
section2c <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 7")
section3a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 8")
section3b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 9")
section4a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 10")
section4b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 11")
section4c <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 12")
section4d <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 13")
section5 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 14")
section6a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 15")
section6b1 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 16")
section6b2 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 17")
section6b3 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 18")
section6b4 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 19")
section6b5 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 20")
section6c1 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 21")
section6c2 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 22")
section6c3 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 23")
section6d <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 24")
section7a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 25")
section7b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 26")
section8 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 27")
section9a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 28")
section9b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 29")
section9c <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 30")
section9d <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 31")
section9e <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 32")
section9f1 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 33")
section9f2 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 34")
section10 <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 35")
section11a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 36")
section11b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 37")
section11c <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 38")
section12a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 39")
section12b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 40")
section13a <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 41")
section13b <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 42")
section13c <- read.xlsx("dataset/compileddataset.xlsx", sheet = "Worksheet 43")

#SECTION0

#labelling section-0
section0 <- section0 %>%
  mutate(
  id = labelled(
    id, 
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
      labels = c(Male = 1, Female = 2, Others = 3)
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


#section1b
for (i in setdiff(1:ncol(section1b), c(8, 18, 19, 20, 24, 26, 28))) {
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

#Part 2.1 - Housing

#Part 2.1.1 - Type of dwelling

section2a1 <- read.xlsx("dataset/section2a1.xlsx")as

for (i in setdiff(1:ncol(section2a1), c(9, 11, 13, 15, 16))) {
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

for (i in setdiff(1:ncol(section2a2), c(7, 12))) {
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

for (i in setdiff(1:ncol(section2a3), c(7, 10, 20, 29))) {
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
section2b <- read.xlsx("dataset/section2b.xlsx")

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

for (i in setdiff(1:ncol(section2b), c(19, 31, 41, 78))) {
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
  v232 = labelled(
    v232, 
    label = "What is your main reason for not enrolling? (MCQ - up to three applicable)",
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
  v233 = labelled(
    v233, 
    label = "What is your primary reason for enrolling? (MCQ - up to three applicable)"
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

for (i in setdiff(1:ncol(section2c), c(9, 13, 15))) {
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
  v261a = labelled(
    v261a, 
    label = "If the deceased was a woman aged 15 to 49, what was her condition at the time of death?",
    labels = c(
      Pregnant = 1, 
      "In labour" = 2, 
      "Postpartum (<= 6 weeks after childbirth)" = 3, 
      Other = 4)
  ), 
  v261b = labelled(
    v261b, 
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

#Part 3.1: Food at Home

for (i in (1:ncol(section3a))) {
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
      "Prepared food products" = 15)
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

for (i in (1:ncol(section3b))) {
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

#Part 4.1 - Non-Food Expenditures
for (i in (1:ncol(section4a))) {
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
        "Books, Magazines and Stationery" - 19, 
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

for (i in (1:ncol(section4b))) {
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
      label = "Did you or any of the household members travel to a foreign country in the past 12 months?"
    ), 
    v405 = labelled(
      v405,
      label = "Tourism expenditure items"
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

for (i in (1:ncol(section4c))) {
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
      label = "Ward #"
    ),
    hhld = labelled(
      hhld,
      label = "Household #"
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

for (i in (1:ncol(section4d))) {
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

for (i in (1:ncol(section5))) {
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

#Part 6.1 - Screening for General Health Status 

for (i in (1:ncol(section6a))) {
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

for (i in (1:ncol(section6b1))) {
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
    v610a = labelled(
      v610a,
      label = "If no, why are you not currently receiving treatment?",
      labels = c(
        "Cannot afford lifelong medications (eg. diabetes/hypertension drugs" = 1, 
        "Essential medicines frequently out of stock at local health facilities" = 2, 
        "Costs too high for regular hospital visits" = 3, 
        "No family member available to assist with clinic visits" = 4, 
        "Treatment showed no noticeable improvement over time" = 5, 
        "Local health centre lacks chronic disease specialists/services" = 6, 
        "Fear of side effects from long-term medication use" = 7, 
        "Long queues discourage repeat visit" = 8, 
        "Stopped treatment after consultation" = 9, 
      )
    ),
    v610b = labelled(
      v610b,
      label = "Others (specify)"
    ),
    v611 = labelled(
      v611,
      label = "Where do you usually go for consultation in relation to this illness?",
      labels = c(
        "Health Post" = 1, 
        "Primary Health Centre" = 2, 
        "Governmental Hospital" = 3, 
        "Government Outreach Clinic" = 4, 
        "Government Ayurveda Centre" = 5, 
        "Pharmacy/Drug Seller" = 6, 
        "Private Clinic" = 7, 
        "Private/Community Hospital" = 8, 
        "Private Ayurveda Centre" = 9, 
        "Health Worker's Home" = 10, 
        "Alternative/Traditional Healer" = 11, 
        "Abroad (India/Other)" = 12, 
        "Others" = 13
      )
    )
  )

#Part 6.2.2 - Chronic Illness and Medication Use

for (i in (1:ncol(section6b2))) {
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
