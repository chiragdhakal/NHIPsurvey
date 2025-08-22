if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)

section0 <- read.xlsx("dataset/section0.xlsx")
section1a <- read.xlsx("dataset/section1a.xlsx")
section1b <- read.xlsx("dataset/section1b.xlsx")
section2a1 <- read.xlsx("dataset/section2a1.xlsx")
section2a2 <- read.xlsx("dataset/section2a2.xlsx")
section2a3 <- read.xlsx("dataset/section2a3.xlsx")
section2b <- read.xlsx("dataset/section2b.xlsx")
section2c <- read.xlsx("dataset/section2c.xlsx")
section3a <- read.xlsx("dataset/section3a.xlsx")
section3b <- read.xlsx("dataset/section3b.xlsx")



section5 <- read.xlsx("dataset/section5.xlsx")


library(dplyr)

# Helper function: keep rows with commas in any column except skipped
keep_rows_with_commas <- function(df, skip_cols = NULL) {
  cols <- setdiff(names(df), skip_cols)
  df %>% filter(if_any(all_of(cols), ~ grepl(",", .)))
}

# --- Section 1b ---
section1b_multi <- keep_rows_with_commas(
  section1b, 
  skip_cols = c(6, 16, 17, 18, 22, 24, 26)
)

# --- Section 2a1 ---
section2a1_multi <- keep_rows_with_commas(
  section2a1, 
  skip_cols = c(9, 11, 13, 15, 16)
)

# --- Section 2a2 ---
section2a2_multi <- keep_rows_with_commas(
  section2a2, 
  skip_cols = c(12)
)

# --- Section 2a3 ---
section2a3_multi <- keep_rows_with_commas(
  section2a3, 
  skip_cols = c(7, 10, 20, 29)
)

# --- Section 2b ---
section2b_multi <- keep_rows_with_commas(
  section2b, 
  skip_cols = NULL  # you can add skipped columns if needed
)

# --- Section 2c ---
section2c_multi <- keep_rows_with_commas(
  section2c, 
  skip_cols = c(9, 13, 15)
)

# --- Section 3a ---
section3a_multi <- keep_rows_with_commas(
  section3a, 
  skip_cols = NULL  # or add columns to skip if necessary
)

section3b_multi <- keep_rows_with_commas(
  section3b, 
  skip_cols = NULL  # or add columns to skip if necessary
)







section1a <- read.xlsx("dataset/section1a.xlsx")
section1b <- read.xlsx("dataset/section1b.xlsx")
section5 <- read.xlsx("dataset/section5.xlsx")
section12a <- read.xlsx("dataset/section12a.xlsx")

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

  
section1b_edu <- section1b_edu %>%
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

