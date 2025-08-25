if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)

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


##############CHECKING FOR CELLS WITH INCONSISTENT DATATYPE##################
keep_rows_with_commas <- function(df, skip_cols = NULL) {
  cols <- setdiff(names(df), skip_cols)
  df %>% 
  filter(if_any(all_of(cols), ~ grepl(",", .)))
}

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
  skip_cols = NULL  # you can add skipped columns if needed
)

section2c_multi <- keep_rows_with_commas(
  section2c, 
  skip_cols = c(9, 13, 15)
)

section3a_multi <- keep_rows_with_commas(
  section3a, 
  skip_cols = NULL  # or add columns to skip if necessary
)

section3b_multi <- keep_rows_with_commas(
  section3b, 
  skip_cols = NULL  # or add columns to skip if necessary
)


################CHECKING EDUCATION DATA CONSISTENCY####################
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



#############REMITTANCE CONSISTENCY CHECK############
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


############ HEALTH EXPENDITURE AND OUT OF POCKET EXPENDITURE RATIO ###############
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


