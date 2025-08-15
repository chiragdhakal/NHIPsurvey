if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)

#importing dataset
section1 <- read.xlsx("dataset/aug14_data.xlsx", sheet = "Worksheet 1")

#cleaning section-1 
section1 <- section1 %>%
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
      labels = c(Arya = 1, Janajati = 2, Madhesi = 3, Dalit = 4, Muslim = 5)
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

  
  

