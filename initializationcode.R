if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)

#importing dataset
section1a <- read.xlsx("dataset/aug14_data.xlsx", sheet = "Worksheet 1")

#cleaning section-1a 
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
    personid = as.integer(personid),
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
    ID = labelled(ID, 
      label = "identification code"
    ), 
    psu = labelled(
      psu, 
      label = "primary sampling unit"
    ), 
    ward = labelled(
      ward, 
      label = "ward number"
    ), 
    hhld = labelled(
      hhld, 
      label = "household number"
    ), 
    personid = labelled(
      personid, 
      label = "unique id"
    ), 
    v101 = labelled(
      v101, 
      label = "family member code"
    ), 
    v102 = labelled(
      v102, 
      label = "name of the family member"
    ), 
    v103 = labelled(
      v103, 
      label = "gender", 
      labels = c(Male = 1, Female = 2, Others = 3)
    ), 
    v104a = labelled(
      v104a, 
      label = "age"
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
    v107 = labelled(
      v107, 
      label = "relation to household head",
      labels = c(Householdhead = 1, Spouse = 2, Children = 3, Grandchildren = 4, Parents = 5, Siblings = 6, BhatijBhatiji = 7, JwaiBuhari = 8, DajuBhauju = 9, Inlaws = 10, Others = 11)
    ), 
    v108 = labelled(
      v108,
      label = "duration stayed with the family"
    ), 
    v109 = labelled(
      v109,
      label = "type of family member",
      labels = c(Familymember = 1, AbroadReturnee = 2, AbsentNepal = 3, AbsentForeign = 4)
    ), 
    v110 = labelled(
      v110,
      label = "marital status",
      labels = c(Unmarried = 1, Married = 2, Separated = 3, Divorced = 4, Widowed = 5)
    )
  )

#importing section1b
section1b <- read.xlsx("dataset/aug14_data.xlsx", sheet = "Worksheet 2")

#cleaning section1b
section1b <- section1b %>%
  mutate(
  v112a_new = ifelse(grepl("\\b1\\b", v112a), 1, 0),
  v112b = ifelse(grepl("\\b2\\b", v112a), 1, 0),
  v112c = ifelse(grepl("\\b3\\b", v112a), 1, 0),
  v112d = ifelse(grepl("\\b4\\b", v112a), 1, 0),
  v112e = ifelse(grepl("\\b5\\b", v112a), 1, 0),
  v112f = ifelse(grepl("\\b6\\b", v112a), 1, 0),
  v112g = ifelse(grepl("\\b7\\b", v112a), 1, 0),
  v112h = ifelse(grepl("\\b8\\b", v112a), 1, 0)
  ) %>%
  select(-v112a) %>%
  rename(v112a = v112a_new)

for (i in setdiff(1:ncol(section1b), c(12, 14, 16))) {
  section1b[[i]] <- as.integer(section[[i]])
}

section1b <- section1b %>%
  mutate(
    ID = labelled(
      ID, 
      label = "identification code"
    ), 
    psu = labelled(
      psu, 
      label = "primary sampling unit"
    ), 
    ward = labelled(
      ward, 
      label = "ward number"
    ), 
    hhld = labelled(
      hhld, 
      label = "household number"
    ), 
    personid = labelled(
      personid, 
      label = "unique id"
    ), 
    v111 = labelled(
      v111,
      label = "insured status", 
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
    v113 = labelled(
      v113, 
      label = "Insurance Number (Only HIB and SSF)"
    ),
    v114 = labelled(
      v114, 
      label = "Can the respondent read and write", 
      labels = c(Both =  1, "Read only" = 2, Neither = 3)
    ), 
    v115 = labelled(
      v115, 
      label = "If respondent has ever gone to school", 
      labels = c(Never = 1, "Used to" = 2, "Currently going" = 3)
    ),
    v116 = labelled(
      v116, 
      label = "Level of Education Completed", 
      labels = c(Kindergarten = 0, "Class 1" = 1, "Class 2" = 2, "Class 3" = 3, "Class 4" = 4, "Class 5" = 5, "Class 6" = 6,
      "Class 7" = 7, "Class 8" = 8, "Class 9" = 9, "Class 10" = 10, "SEE/SLC" = 11, "+2 or equivalent" = 12, Bachelors = 13, 
      Masters = 14, PhD = 15, "Literate - Level less" = 16, Illiterate = 17)
    )
    v117 = labelled(
      v117, 
      label = "Name of the Spouse"
    ), 
    v118 = labelled(
      v118, 
      label = "Does your father live with the family?", 
      labels = c(Yes = 1, No = 2, Death = 3)
    ), 
    v119 = labelled(
      v119, 
      label = "Name of the father"
    ), 
    v120 = labelled(
      v120, 
      label = "Does your mother live with the family?", 
      labels = c(Yes = 1, No = 2, Death = 3)
    ), 
    v121 = labelled(
      v121, 
      label = "Name of the mother"
    )
  )
