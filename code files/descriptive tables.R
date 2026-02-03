if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)
library(labelled)
library(officer)
library(flextable)
library(stringr)

in_dir <- "stata_data"

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

sections <- lapply(files, read_dta)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)

#PALIKA TYPE IN SECTION0 

metro_codes <- c(11214, 20807, 30608, 30802, 31304, 40504)

sub_metro_codes <- c(11301, 11306, 20315, 20703, 20708, 31206, 50802, 51002, 51003, 51106, 70813)

municipality_codes <- c(10106, 10206, 10207, 10208, 10209, 10210, 10307, 10402, 
  10504, 10505, 10601, 10604, 10701, 10702, 10704, 10804, 10805, 10904, 11003, 
  11004, 11008, 11009, 11101, 11103, 11104, 11105, 11107, 11108, 11112, 11114, 
  11202, 11204, 11205, 11207, 11208, 11209, 11210, 11211, 11302, 11305, 11307, 
  11309, 11401, 11402, 11403, 11407, 20101, 20102, 20105, 20106, 20107, 20109, 
  20110, 20113, 20116, 20201, 20202, 20203, 20204, 20205, 20206, 20210, 20217, 
  20301, 20302, 20303, 20305, 20307, 20308, 20309, 20310, 20311, 20313, 20317, 
  20401, 20402, 20404, 20405, 20406, 20407, 20408, 20410, 20414, 20415, 20501, 
  20502, 20503, 20504, 20505, 20506, 20507, 20511, 20516, 20517, 20520, 20601, 
  20602, 20603, 20604, 20605, 20606, 20607, 20608, 20609, 20610, 20611, 20612, 
  20613, 20616, 20617, 20618, 20701, 20702, 20712, 20713, 20714, 20806, 20808, 
  20809, 30105, 30109, 30205, 30207, 30209, 30406, 30413, 30504, 30508, 30601, 
  30602, 30603, 30604, 30605, 30606, 30607, 30609, 30610, 30611, 30701, 30702, 
  30703, 30704, 30801, 30803, 30903, 30904, 30905, 30906, 30908, 30909, 31004, 
  31005, 31101, 31105, 31202, 31301, 31302, 31305, 31306, 31307, 40108, 40109, 
  40406, 40604, 40605, 40606, 40607, 40701, 40702, 40704, 40705, 40801, 40805, 
  40806, 40807, 40901, 40905, 40908, 40909, 40910, 41003, 41004, 41101, 41105, 
  41108, 41110, 50207, 50304, 50305, 50404, 50409, 50503, 50504, 50506, 50601, 
  50605, 50701, 50702, 50703, 50801, 50803, 50808, 50811, 50813, 50901, 50902, 
  50903, 50905, 50906, 50907, 51007, 51102, 51201, 51202, 51203, 51205, 51206, 
  51207, 60105, 60106, 60202, 60404, 60503, 60506, 60507, 60605, 60606, 60607, 
  60608, 60704, 60706, 60707, 60801, 60804, 60806, 60903, 60905, 60907, 61003, 
  61004, 61005, 61006, 61008, 70103, 70106, 70108, 70109, 70202, 70206, 70303, 
  70307, 70403, 70405, 70408, 70409, 70502, 70505, 70604, 70605, 70701, 70704, 
  70706, 70708, 70803, 70804, 70805, 70807, 70810, 70811, 70901, 70902, 70903, 
  70904, 70905, 70907, 70908)

rural_codes <- c(10101, 10102, 10103, 10104, 10105, 10107, 10108, 10109, 10201, 
  10202, 10203, 10204, 10205, 10301, 10302, 10303, 10304, 10305, 10306, 10308, 
  10401, 10403, 10404, 10405, 10406, 10407, 10408, 10501, 10502, 10503, 10506, 
  10507, 10508, 10509, 10510, 10602, 10603, 10605, 10606, 10607, 10608, 10609, 
  10703, 10705, 10706, 10707, 10801, 10802, 10803, 10806, 10901, 10902, 10903, 
  10905, 10906, 10907, 10908, 11001, 11002, 11005, 11006, 11007, 11010, 11102, 
  11106, 11109, 11110, 11111, 11113, 11115, 11201, 11203, 11206, 11212, 11213, 
  11215, 11216, 11217, 11303, 11304, 11308, 11310, 11311, 11312, 11404, 11405, 
  11406, 11408, 20103, 20104, 20108, 20111, 20112, 20114, 20115, 20117, 20118, 
  20207, 20208, 20209, 20211, 20212, 20213, 20214, 20215, 20216, 20304, 20306, 
  20312, 20314, 20316, 20318, 20403, 20409, 20411, 20412, 20413, 20508, 20509, 
  20510, 20512, 20513, 20514, 20515, 20518, 20519, 20614, 20615, 20704, 20705, 
  20706, 20707, 20709, 20710, 20711, 20715, 20716, 20801, 20802, 20803, 20804, 
  20805, 20810, 20811, 20812, 20813, 20814, 30101, 30102, 30103, 30104, 30106, 
  30107, 30108, 30201, 30202, 30203, 30204, 30206, 30208, 30210, 30211, 30212, 
  30301, 30302, 30303, 30304, 30305, 30401, 30402, 30403, 30404, 30405, 30407, 
  30408, 30409, 30410, 30411, 30412, 30501, 30502, 30503, 30505, 30506, 30507, 
  30509, 30510, 30511, 30512, 30804, 30805, 30806, 30901, 30902, 30907, 30910, 
  30911, 30912, 30913, 31001, 31002, 31003, 31006, 31007, 31008, 31102, 31103, 
  31104, 31106, 31107, 31108, 31109, 31201, 31203, 31204, 31205, 31207, 31208, 
  31209, 31210, 31303, 40101, 40102, 40103, 40104, 40105, 40106, 40107, 40110, 
  40111, 40201, 40202, 40203, 40204, 40301, 40302, 40303, 40304, 40305, 40401, 
  40402, 40403, 40404, 40405, 40501, 40502, 40503, 40505, 40601, 40602, 40603, 
  40608, 40703, 40706, 40707, 40708, 40709, 40710, 40802, 40803, 40804, 40808, 
  40902, 40903, 40904, 40906, 40907, 40911, 41001, 41002, 41005, 41006, 41007, 
  41102, 41103, 41104, 41106, 41107, 41109, 50101, 50102, 50103, 50201, 50202, 
  50203, 50204, 50205, 50206, 50208, 50209, 50210, 50301, 50302, 50303, 50306, 
  50307, 50308, 50309, 50401, 50402, 50403, 50405, 50406, 50407, 50408, 50410, 
  50411, 50412, 50501, 50502, 50505, 50602, 50603, 50604, 50606, 50607, 50608, 
  50609, 50610, 50704, 50705, 50706, 50707, 50804, 50805, 50806, 50807, 50809, 
  50810, 50812, 50814, 50815, 50816, 50904, 50908, 50909, 50910, 51001, 51004, 
  51005, 51006, 51008, 51009, 51010, 51101, 51103, 51104, 51105, 51107, 51108, 
  51204, 51208, 60101, 60102, 60103, 60104, 60107, 60108, 60201, 60203, 60204, 
  60301, 60302, 60303, 60304, 60305, 60306, 60307, 60401, 60402, 60403, 60405, 
  60406, 60407, 60408, 60501, 60502, 60504, 60505, 60508, 60509, 60601, 60602, 
  60603, 60604, 60609, 60610, 60611, 60701, 60702, 60703, 60705, 60802, 60803, 
  60805, 60901, 60902, 60904, 60906, 60908, 60909, 60910, 61001, 61002, 61007, 
  61009, 70101, 70102, 70104, 70105, 70107, 70201, 70203, 70204, 70205, 70207, 
  70208, 70209, 70210, 70211, 70212, 70301, 70302, 70304, 70305, 70306, 70308, 
  70309, 70401, 70402, 70404, 70406, 70407, 70410, 70501, 70503, 70504, 70506, 
  70507, 70601, 70602, 70603, 70606, 70607, 70608, 70609, 70702, 70703, 70705, 
  70707, 70709, 70710, 70801, 70802, 70806, 70808, 70809, 70812, 70906, 70909)

section0 <- section0 %>%
  mutate(
    palika_type = case_when(
      palika %in% metro_codes ~ 1, 
      palika %in% sub_metro_codes ~ 2, 
      palika %in% municipality_codes ~ 3, 
      palika %in% rural_codes ~ 4
    ),
    domain = case_when(
      province == 1 & palika_type %in% c(1, 2, 3) ~ "KOSHI URBAN",
      province == 1 & palika_type == 4 ~ "KOSHI RURAL",
      province == 2 & palika_type %in% c(1, 2, 3) ~ "MADHESH URBAN",
      province == 2 & palika_type == 4 ~ "MADHESH RURAL",
      province == 3 & palika_type %in% c(1, 2, 3) ~ "BAGMATI URBAN",
      province == 3 & palika_type == 4 ~ "BAGMATI RURAL",
      province == 4 & palika_type %in% c(1, 2, 3) ~ "GANDAKI URBAN",
      province == 4 & palika_type == 4 ~ "GANDAKI RURAL",
      province == 5 & palika_type %in% c(1, 2, 3) ~ "LUMBINI URBAN",
      province == 5 & palika_type == 4 ~ "LUMBINI RURAL",
      province == 6 & palika_type %in% c(1, 2, 3) ~ "KARNALI URBAN",
      province == 6 & palika_type == 4 ~ "KARNALI RURAL",
      province == 7 & palika_type %in% c(1, 2, 3) ~ "SUDURPASCHIM URBAN",
      province == 7 & palika_type == 4 ~ "SUDURPASCHIM RURAL"
    ), 
    enrollment_type = case_when(
      enrollment == 1 ~ "NHIP", 
      enrollment == 2 ~ "Non NHIP",
      enrollment == 3 ~ "SSF", 
      enrollment == 4 ~ "Non SSF"
    )
  )

section0 <- section0 %>%
  mutate(
    rural_urban = case_when(
      palika_type %in% c(1, 2, 3) ~ 1, 
      palika_type %in% c(4) ~ 2
    )
  )

#DESCRIPTIVE TABLE BASED ON SIZE  

desc_sec0 <- section0 %>%
  mutate(
    household_size = case_when(
      hhld_member_t >= 1 & hhld_member_t <= 2 ~ "1-2 persons",
      hhld_member_t >= 3 & hhld_member_t <= 4 ~ "3-4 persons", 
      hhld_member_t >= 5 & hhld_member_t <= 6 ~ "5-6 persons",
      hhld_member_t >= 7 ~ "7 or more persons"
    )
  ) %>%
  select(household_size)


table_sec0 <- map_df(names(desc_sec0), function(v) {
  var_data <- desc_sec0[[v]]
  fvar <- as_factor(var_data)
  freq <- table(fvar)
  
  tibble(
    Variable = "Household Size", 
    Label = names(freq),
    Count = as.integer(freq), 
    Percentage = round(100 * as.integer(freq) / length(var_data), 2)
  )
})

ft_sec0 <- flextable(table_sec0) %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5, color = "black"), part = "header") %>%
  hline_bottom(border = fp_border(width = 1, color = "black"), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5, color = "black"), part = "body") %>%
  
  merge_v(j = ~ Variable) %>%
  valign(j = 1, valign = "top") %>%
  
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(j = 1:2, align = "left", part = "all") %>%
  align(j = 3:4, align = "right", part = "all") %>%
  
  padding(padding = 5, part = "all") %>%
  autofit()

if(!dir.exists("descriptive tables")) dir.create("descriptive tables")

doc_sec0 <- read_docx() %>%
  body_add_flextable(ft_sec0)

print(doc_sec0, target = "descriptive tables/doc_sec0_scientific.docx")


#DESCRIPTIVE TABLE BASED ON SEX, ENTHICITY, RELIGION AND TYPE OF HOUSEHOLD MEMBER

section1a_clean <- section1a %>%
  mutate(
    person = paste0(psu, "-", hhld, "-", v102),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld),
    age_group = case_when(
      v104a >= 0 & v104a <= 14 ~ "0-14 years",
      v104a >= 15 & v104a <= 59 ~ "15-59 years", 
      v104a >= 60 ~ "60 years and above"
    )
  ) %>%
  distinct(person, .keep_all = TRUE) 

desc_sec1a <- section1a_clean %>%
  select(v103, v105, v106, v109, age_group) %>%
  rename(
    `Sex` = v103, 
    `Ethnicity` = v105, 
    `Religion` = v106, 
    `Category` = v109,
    `Age Group` = age_group
  )

table_sec1a_data <- map_df(names(desc_sec1a), function(v) {
  var_vector <- desc_sec1a[[v]]
  fvar <- as_factor(var_vector)
  freq <- table(fvar)
  
  tibble(
    Variable = v,
    Label = names(freq),
    Count = as.integer(freq), 
    Percentage = round(100 * as.integer(freq) / length(var_vector), 2)
  )
})

ft_sec1a <- flextable(table_sec1a_data) %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  
  merge_v(j = ~ Variable) %>%
  valign(j = 1, valign = "top") %>%
  
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  align(j = 1:2, align = "left", part = "all") %>%
  align(j = 3:4, align = "right", part = "all") %>%
  
  bold(part = "header") %>%
  
  padding(padding = 4, part = "all") %>%
  autofit()

doc_output <- read_docx() %>%
  body_add_flextable(ft_sec1a)

print(doc_output, target = "descriptive tables/desc_sec1a_scientific.docx")

#DESCRIPTIVE TABLE BASED ON EDUCATION 

section1b <- section1b %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld)
  )

section0 <- section0 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

desc_sec1b <- merge(
  section1b, 
  section0[, c("hhid", "province")],
  by = "hhid"
)

desc_sec1b <- merge(
  desc_sec1b, 
  section1a[, c("uniq_id", "v103", "v105")],
  by = "uniq_id"
)

desc_sec1b <- desc_sec1b %>%
  select(v114, v115, v116, v103, v105, province) %>%
  rename(
    literate = v114, 
    attended_school = v115, 
    highest_edu = v116,
    gender = v103, 
    ethnicity = v105
  )

table_sec1b <- map_df(names(desc_sec1b), function(v) {
  var <- desc_sec1b[[v]]
  fvar <- as_factor(var)
  freq <- table (fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
  )
})

ft_sec1b <- flextable(table_sec1b) 

make_wide_table <- function(df, varname) {
  tmp <- df %>%
    mutate(
      value = as_factor(.data[[varname]]),
      gender = as_factor(gender),
      province = as_factor(province),
      col = paste0(province, " - ", gender)
    )

  totals <- tmp %>%
    group_by(col) %>%
    summarise(total = n(), .groups = "drop")

  counts <- tmp %>%
    group_by(value, col) %>%
    summarise(count = n(), .groups = "drop")

  counts <- counts %>%
    left_join(totals, by = "col") %>%
    mutate(percent = round(100 * count / total, 2)) %>%
    select(-total)

  counts %>%
    pivot_wider(
      names_from = col,
      values_from = c(count, percent),
      values_fill = 0
    ) %>%
    rename(category = value)
}
tbl_literate  <- make_wide_table(desc_sec1b, "literate")
tbl_attend    <- make_wide_table(desc_sec1b, "attended_school")
tbl_highest   <- make_wide_table(desc_sec1b, "highest_edu")

ethnicity_literacy <- desc_sec1b %>%
  mutate(
    literate_bin = literate == 1,
    ethnicity = as_factor(ethnicity)
  ) %>%
  group_by(ethnicity) %>%
  summarise(
    total = n(),
    literate = sum(literate_bin, na.rm = TRUE),
    literacy_rate = round(100 * literate / total, 2),
    .groups = "drop"
  )

ethnicity_gender_literacy <- desc_sec1b %>%
  mutate(
    literate_bin = literate == 1,
    ethnicity = as_factor(ethnicity),
    gender = as_factor(gender)
  ) %>%
  group_by(ethnicity, gender) %>%
  summarise(
    total = n(),
    literate = sum(literate_bin, na.rm = TRUE),
    literacy_rate = round(100 * literate / total, 2),
    .groups = "drop"
  )

wb <- createWorkbook()

addWorksheet(wb, "original_table")
writeData(wb, "original_table", table_sec1b)

addWorksheet(wb, "literate_pg")
writeData(wb, "literate_pg", tbl_literate)

addWorksheet(wb, "attend_pg")
writeData(wb, "attend_pg", tbl_attend)

addWorksheet(wb, "highest_pg")
writeData(wb, "highest_pg", tbl_highest)

addWorksheet(wb, "ethnicity_literacy")
writeData(wb, "ethnicity_literacy", ethnicity_literacy)

addWorksheet(wb, "ethnicity_gender_lit")
writeData(wb, "ethnicity_gender_lit", ethnicity_gender_literacy)

saveWorkbook(wb, "descriptive tables/sec1b_full_tables.xlsx", overwrite = TRUE)

#DESCRIPTIVE TABLE FOR TYPE OF DWELLING 

desc_sec2a1 <- section2a1 %>%
  select(v203, v204, v205, v206) %>%
  rename(
    house_foundation = v203, 
    outer_wall = v204, 
    roof = v205, 
    floor = v206
  )

table_sec2a1 <- map_df(names(desc_sec2a1), function(v) {
  var <- desc_sec2a1[[v]]
  fvar <- as_factor(var)
  freq <- table(fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
)
}) 

ft_sec2a1 <- flextable(table_sec2a1) 
doc_sec2a1 <- read_docx()
doc_sec2a1 <- body_add_flextable(doc_sec2a1, ft_sec2a1)
print(doc_sec2a1, target = "descriptive tables/doc_sec2a1.docx")

#DESCRIPTIVE TABLE FOR UTILITIES AND AMENITIES

desc_sec2a3 <- section2a3 %>%
  select(v216, v218, v220, v223, v225) %>%
  rename(
    drinking_water = v216, 
    cooking_fuel = v218, 
    lighting_source = v220, 
    garbage_dispose = v223, 
    toilet_type = v225
  )

table_sec2a3 <- map_df(names(desc_sec2a3), function(v) {
  var <- desc_sec2a3[[v]]
  fvar <- as_factor(var)
  freq <- table(fvar)

  tibble(
  variable = v,
  variable_label = var_label(var) %||% NA,
  value_label = names(freq),
  count = as.integer(freq), 
  percent = round(100*count / length(var), 2)
)
}) 

ft_sec2a3 <- flextable(table_sec2a3) 
doc_sec2a3 <- read_docx()
doc_sec2a3 <- body_add_flextable(doc_sec2a3, ft_sec2a3)
print(doc_sec2a3, target = "descriptive tables/doc_sec2a3.docx")

#DESCRIPTIVE TABLE FOR FARM INCOME 

desc_farmincome <- merge(
  farm_hh, 
  section0[, c("hhid", "province", "")],
  by = "hhid", 
  all = FALSE
)

desc_farmincome <- desc_farmincome %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, total_production_sale, total_livestock_income, total_farm_income, total_farm_expenditure)

desc_farmincome <- desc_farmincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_farmincome, "descriptive tables/desc_farmincome.xlsx")

#DESCRIPTIVE TABLE FOR WAGE INCOME 

desc_wageincome <- merge(
  household_wage_income, 
  section0[, c("hhid", "province", "hhld_member_t")],
  by = "hhid", 
  all = FALSE
)

desc_wageincome <- desc_wageincome %>%
  select(-hhid) %>%
  mutate(
    hhld_member_t = as.numeric(hhld_member_t),
    percapita_wage = total_hh_income / hhld_member_t
  )

p1  <- quantile(desc_wageincome$percapita_wage, 0.01, na.rm = TRUE)
p99 <- quantile(desc_wageincome$percapita_wage, 0.99, na.rm = TRUE)

a1  <- quantile(desc_wageincome$total_hh_income, 0.01, na.rm = TRUE)
a99 <- quantile(desc_wageincome$total_hh_income, 0.99, na.rm = TRUE)

desc_wageincome <- desc_wageincome %>%
  filter(
    percapita_wage >= p1, 
    percapita_wage <= p99, 
    total_hh_income >= a1, 
    total_hh_income <= a99
  ) %>%
  group_by(province) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = mean,
      na.rm = TRUE
    )
  ) %>%
  select(province, total_hh_income, percapita_wage)

desc_wageincome <- desc_wageincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_wageincome, "descriptive tables/desc_wageincome.xlsx")

#DESCRIPTIVE TABLE FOR NON FARM ENTERPRISE INCOME

desc_nonagri <- merge(
  non_agri_income, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_nonagri <- desc_nonagri %>%
  mutate(total_non_agri_income = ifelse(is.na(total_non_agri_income), 0, total_non_agri_income)) %>%
  select(-hhid) 
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, total_non_agri_income) 

desc_nonagri <- desc_nonagri %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_nonagri, "descriptive tables/desc_nonagri.xlsx")

#DESCRIPTIVE TABLE FOR RENT INCOME 

desc_rent <- merge(
  rent_income, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_rent <- desc_rent %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, rent_annual) 

desc_rent <- desc_rent %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_rent, "descriptive tables/desc_rent.xlsx")

#DESCRIPTIVE TABLE FOR CASH TRANSFER INCOME

desc_cashtransfer <- merge(
  cash_transfer_program, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_cashtransfer <- desc_cashtransfer %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, cash_assistance_received) 

desc_cashtransfer <- desc_cashtransfer %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_cashtransfer, "descriptive tables/desc_cashtransfer.xlsx")

#DESCRIPTIVE TABLE FOR REMITTANCE INCOME 

desc_remittance <- merge(
  remittance_income, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

remittance_households <- desc_remittance %>%
  filter(!is.na(province)) %>%
  group_by(province) %>%
  summarise(hhlds = n()) %>%
  ungroup() %>%
  mutate(
    percent = (hhlds/nrow(section0)) * 100
  )

desc_remittance <- desc_remittance %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  select(province, total_amount_received, total_sent_abroad) 

desc_remittance <- desc_remittance %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(remittance_households, "descriptive tables/remittance_households.xlsx")

write.xlsx(desc_remittance, "descriptive tables/desc_remittance.xlsx")

#DESCRIPTIVE TABLE FOR TOTAL INCOME 

desc_totalincome <- merge(
  income_hhld, 
  section0[, c("hhid", "province", "hhld_member_t", "distinction")],
  by = "hhid", 
  all = FALSE
)

desc_totalincome <- desc_totalincome %>%
  mutate(hhld_member_t = as.numeric(hhld_member_t)) %>%
  select(-hhid) %>%
  mutate(percapita_income = total_income / hhld_member_t)

p1  <- quantile(desc_totalincome$percapita_income, 0.01, na.rm = TRUE)
p99 <- quantile(desc_totalincome$percapita_income, 0.99, na.rm = TRUE)

desc_totalincome_province <- desc_totalincome %>%
  filter(percapita_income >= p1, percapita_income <= p99) %>%
  group_by(province) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = mean,
      na.rm = TRUE
    )
  ) %>%
  select(province, total_income, percapita_income)

desc_totalincome_urban <- desc_totalincome %>%
  filter(percapita_income >= p1, percapita_income <= p99) %>%
  group_by(distinction) %>%
  summarise(
    across(
      .cols = where(is.numeric), 
      .fns = mean, 
      na.rm = TRUE
    )
  ) %>%
  select(distinction, total_income, percapita_income)

desc_totalincome_province <- desc_totalincome_province %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

desc_totalincome_urban <- desc_totalincome_urban %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_totalincome, "descriptive tables/desc_totalincome.xlsx")

#DESCRIPTIVE TABLE FOR CONSUMPTION EXPENDITURE 

desc_consumption <- merge(
  consumption_hh,
  section0[, c("hhid", "province", "hhld_member_t")],
  by = "hhid", 
  all = FALSE
)

desc_consumption <- desc_consumption %>%
  mutate(
    hhld_member_t = as.numeric(hhld_member_t),
    total_consumption = total_food_annual + non_food_annual + abroad_annual + goods_annual, 
    percapita_foodconsumption = total_food_annual / hhld_member_t,
    percapita_consumption = total_consumption / hhld_member_t
  )

summary(desc_consumption)

desc_consumption <- desc_consumption %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, total_food_annual, non_food_annual, goods_annual, percapita_foodconsumption, total_consumption, percapita_consumption) 

desc_consumption <- desc_consumption %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_consumption, "descriptive tables/desc_consumption.xlsx")

#DESCRIPTIVE TABLE FOR EDUCATION EXPENSES

desc_edu <- merge(
  education_expenses,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

summary(desc_edu)

desc_edu <- desc_edu %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, tuition_fee, other_fee, dress_expense, books_expense, transportation_expense, private_tuition, other_expense) 

desc_edu <- desc_edu %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_edu, "descriptive tables/desc_edu.xlsx")

#DESCRIPTIVE TABLE FOR OTHER INCOME

desc_otherincome <- merge(
  other_income,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

summary(desc_otherincome)

desc_otherincome <- desc_otherincome %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, other_income_annual) 

desc_otherincome <- desc_otherincome %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(desc_otherincome, "descriptive tables/desc_otherincome.xlsx")

#DESCRIPTIVE TABLE FOR RATIO OF INCOME COMPONENTS 

ratio_income <- income_hhld %>%
  mutate(across(
    c(
      total_hh_income, rent_annual, net_remittance_received, 
      total_farm_income, cash_assistance_received,
      other_income_annual, total_non_agri_income
    ),
    ~ replace_na(as.numeric(.), 0)
  ))

ratio_income <- income_hhld %>%
  mutate(
    wage_ratio = total_hh_income / total_income,
    rent_ratio = rent_annual / total_income, 
    remittance_ratio = net_remittance_received / total_income, 
    farm_income_ratio = total_farm_income / total_income, 
    cash_assistance_ratio = cash_assistance_received / total_income, 
    other_income_ratio = other_income_annual / total_income, 
    non_agri_ratio = total_non_agri_income / total_income
  ) %>%
  select(hhid, wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio, cash_assistance_ratio, other_income_ratio, non_agri_ratio)

ratio_income <- merge(
  ratio_income,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

summary(ratio_income, na.rm = TRUE)

ratio_income <- ratio_income %>%
  select(-hhid) %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  select(province, wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio, cash_assistance_ratio, other_income_ratio, non_agri_ratio) 

ratio_income <- ratio_income %>%
  pivot_longer(
    cols = -province,
    names_to = "variable",
    values_to = "mean_value"
  )  

write.xlsx(ratio_income, "descriptive tables/ratio_income.xlsx")

#DESCRIPTIVE TABLE FOR LABOUR AND EMPLOYMENT

section1a <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

labor_force <- section1a %>%
  filter(v104a >= 10 & v104a <= 65)

labor_force <- labor_force %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(hhid, uniq_id, v103, v105)

labor_force <- merge(
  labor_force, 
  section0[, c("hhid", "province")],
  by = "hhid"
)

section7_1 <- section7 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld)
  ) %>%
  select(uniq_id, v702, v705, v708, v710)

labor_force <- merge(
  labor_force, 
  section7_1, 
  by = "uniq_id"
)

labor_force <- labor_force %>%
  mutate(lf_participant = ifelse(
  (v702 == 1 | v705 == 1) %in% TRUE,
  1,
  0
))

lfpr_province <- labor_force %>%
  group_by(province) %>%
  summarise(
    working_age = n(),                           
    labor_force = sum(lf_participant, na.rm=TRUE), 
    lfpr = round((labor_force / working_age) * 100, 2)  
  )

lfpr_ethnicity <- labor_force %>%
  group_by(v105) %>%
  summarise(
    working_age = n(), 
    labor_force = sum(lf_participant, na.rm = TRUE), 
    lfpr = round(labor_force / working_age * 100, 2)
  )

lfpr_ethnicity_gender <- labor_force %>%
  group_by(v103, v105) %>%
  summarise(
    working_age = n(), 
    labor_force = sum(lf_participant, na.rm = TRUE),
    lfpr = round(labor_force / working_age * 100, 2),
    .groups = "drop"
  )

lfpr_province_gender <- labor_force %>%
  group_by(province, v103) %>%
  summarise(
    working_age = n(), 
    labor_force = sum(lf_participant, na.rm = TRUE),
    lfpr = round(labor_force / working_age * 100, 2),
    .groups = "drop"
  )

work_population <- section7 %>%
  filter(v702 == 1 | v703 == 1 | v704 == 1) %>%
  mutate(
    v714a = str_extract(v714a, "^[0-9]+") %>% as.numeric(),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v708) %>%
  filter(!is.na(v708))

work_population <- merge(
  work_population, 
  section0[, c("hhid", "province")],
  by = "hhid"
)

work_population <- merge(
  work_population, 
  section1a[, c("uniq_id", "v103", "v105")],
  by = "uniq_id"
)

work_type_nepal <- work_population %>%
  group_by(v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

work_type_province <- work_population %>%
  group_by(province, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

work_type_ethnicity <- work_population %>%
  group_by(v105, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

work_type_gender <- work_population %>%
  group_by(v103, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))


write.xlsx(lfpr_province, "descriptive tables/lfpr_province.xlsx")
write.xlsx(lfpr_province_gender, "descriptive tables/lfpr_province_gender.xlsx")
write.xlsx(lfpr_ethnicity, "descriptive tables/lfpr_ethnicity.xlsx")
write.xlsx(lfpr_ethnicity_gender, "descriptive tables/lfpr_ethnicity_gender.xlsx")
write.xlsx(work_type_nepal, "descriptive tables/work_type_nepal.xlsx")
write.xlsx(work_type_ethnicity, "descriptive tables/work_type_ethnicity.xlsx")
write.xlsx(work_type_province, "descriptive tables/work_type_province.xlsx")
write.xlsx(work_type_gender, "descriptive tables/work_type_gender.xlsx")


#DESCRIPTIVE TABLE FOR CHRONIC ILLNESSES 

chronic_qualified <- section1a %>%
  filter(
    v109 %in% c(1, 2) & v104a >= 5
  ) %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)

chronic_qualified <- merge(
  chronic_qualified, 
  section0[, c("hhid", "province", "domain")], 
  by = "hhid"
)

section6b1 <- section6b1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

chronic_qualified <- merge(
  chronic_qualified,
  section6b1[, c("uniq_id", "v604")],
  by = "uniq_id"
)

chronic_nepal <- chronic_qualified %>%
  filter(!is.na(v604)) %>%   
  count(v604) %>%
  mutate(percent = round(n/sum(n) * 100, 2))

chronic_province <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(province, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

chronic_gender <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(v103, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

chronic_domain <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(domain, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

write.xlsx(chronic_nepal, "descriptive tables/chronic_nepal.xlsx")
write.xlsx(chronic_province, "descriptive tables/chronic_province.xlsx")
write.xlsx(chronic_gender, "descriptive tables/chronic_gender.xlsx")
write.xlsx(chronic_domain, "descriptive tables/chronic_domain.xlsx")

#DESCRIPTIVE TABLE FOR ACUTE ILLNESSES 

acute_qualified <- section1a %>%
  filter(
    v109 %in% c(1, 2) & v104a >= 5
  ) %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)

acute_qualified <- merge(
  acute_qualified, 
  section0[, c("hhid", "province", "domain")], 
  by = "hhid"
)

section6c1 <- section6c1 %>%
  mutate(
    v630 = as.numeric(v630),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

acute_qualified <- merge(
  acute_qualified,
  section6c1[, c("uniq_id", "v630")],
  by = "uniq_id"
)

acute_nepal <- acute_qualified %>%
  filter(!is.na(v630)) %>%   
  count(v630) %>%
  mutate(percent = round(n/sum(n) * 100, 2))

acute_province <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(province, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

acute_gender <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(v103, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

acute_domain <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(domain, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n/sum(n) * 100, 2))

write.xlsx(acute_nepal, "descriptive tables/acute_nepal.xlsx")
write.xlsx(acute_province, "descriptive tables/acute_province.xlsx")
write.xlsx(acute_gender, "descriptive tables/acute_gender.xlsx")
write.xlsx(acute_domain, "descriptive tables/acute_domain.xlsx")

#DESCRIPTIVE TABLES FOR CHRONIC INPATIENT HEALTH COSTS

chronic_inpatient_expenditure <- section6b4 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    across(v618a:v618k, ~ replace_na(., 0))
  ) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense = sum(as.numeric(v618a), na.rm = TRUE), 
    opd_expense = sum(as.numeric(v618b), na.rm = TRUE), 
    laboratory_expense = sum(as.numeric(v618c), na.rm = TRUE), 
    imaging_expense = sum(as.numeric(v618d), na.rm = TRUE), 
    medicine_expense = sum(as.numeric(v618e), na.rm = TRUE), 
    med_device_expense = sum(as.numeric(v618f), na.rm = TRUE), 
    transportation_expense = sum(as.numeric(v618g), na.rm = TRUE), 
    food_accom_expense = sum(as.numeric(v618h), na.rm = TRUE), 
    care_giver_expense = sum(as.numeric(v618i), na.rm = TRUE), 
    other_cost = sum(as.numeric(v618j), na.rm = TRUE), 
    total_cost = sum(as.numeric(v618k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_chronic_inpatient = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense, medicine_expense, med_device_expense),
    ), na.rm = TRUE)
  )

chronic_inpatient_expenditure <- merge(
  chronic_inpatient_expenditure, 
  section0[, c("hhid", "province", "domain")],
  by = "hhid"
)

summary(chronic_inpatient_expenditure)

chronic_inpatient_expenditure_province <- chronic_inpatient_expenditure %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop") 

chronic_inpatient_expenditure_domain <- chronic_inpatient_expenditure %>%
  group_by(domain) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

write.xlsx(chronic_inpatient_expenditure_province, "descriptive tables/chronic_inpatient_expenditure_province.xlsx")
write.xlsx(chronic_inpatient_expenditure_domain, "descriptive tables/chronic_inpatient_expenditure_domain.xlsx")

#DESCRIPTIVE TABLES FOR CHRONIC OUTPATIENT HEALTH COSTS

chronic_outpatient_expenditure <- section6b3 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    across(v614a:v614k, ~ replace_na(., 0))
  ) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense = sum(as.numeric(v614a), na.rm = TRUE), 
    opd_expense = sum(as.numeric(v614b), na.rm = TRUE), 
    laboratory_expense = sum(as.numeric(v614c), na.rm = TRUE), 
    imaging_expense = sum(as.numeric(v614d), na.rm = TRUE), 
    medicine_expense = sum(as.numeric(v614e), na.rm = TRUE), 
    med_device_expense = sum(as.numeric(v614f), na.rm = TRUE), 
    transportation_expense = sum(as.numeric(v614g), na.rm = TRUE), 
    food_accom_expense = sum(as.numeric(v614h), na.rm = TRUE), 
    care_giver_expense = sum(as.numeric(v614i), na.rm = TRUE), 
    other_cost = sum(as.numeric(v614j), na.rm = TRUE), 
    total_cost = sum(as.numeric(v614k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_chronic_outpatient = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense, medicine_expense, med_device_expense, transportation_expense, food_accom_expense, care_giver_expense, other_cost),
    ), na.rm = TRUE)
  )

summary(chronic_outpatient_expenditure)

chronic_outpatient_expenditure <- merge(
  chronic_outpatient_expenditure, 
  section0[, c("hhid", "province", "domain")],
  by = "hhid"
)

chronic_outpatient_expenditure_province <- chronic_outpatient_expenditure %>%
  group_by(province) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop") 

chronic_outpatient_expenditure_domain <- chronic_outpatient_expenditure %>%
  group_by(domain) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

write.xlsx(chronic_outpatient_expenditure_province, "descriptive tables/chronic_outpatient_expenditure_province.xlsx")
write.xlsx(chronic_outpatient_expenditure_domain, "descriptive tables/chronic_outpatient_expenditure_domain.xlsx")

#HEALTH TABLES

section6b3 <- section6b3 %>%
  mutate(
    v604 = case_when(
      # Cholesterol
      v604a %in% c(
        "कोलेस्ट्रोल", "COLSTORE", "CHOLESTEROL", "COLSTRORE", "CHOLOSTREL", "CHLOSTROAL",
        "COLSTROL", "CHLOSTROL", "COLESTROME", "COLESTER", "COLDSTORAL",
        "COLESTEROL", "CHOLESTEROLPROSTATE", "CHOLESTROL", "CHOLEDTEROL",
        "COLDSTORE", "COL", "CHORESTEROL", "COL STORE", "CHOLESTEROL PROSTATE", " CHOLESTEROL", "CHLORESTROL"
      ) ~ 19,

      # Uric Acid
      v604a %in% c(
        "URIC ACID", "URIK ACID", "URIK ASID",
        "URIC ACID RA PROSTHETICS", "URIQE ACID"
      ) ~ 20,

      # Diabetes / Sugar
      v604a %in% c("SUGAR", "SUGAR BLOOD PRESSURE", "DIABETIC") ~ 3,

      # Piles / Ulcer
      v604a %in% c(
        "PILES", "PAYALS", "ALSAR", "ULCER", "ULCERS",
        "PILES KO LAI SHE SOMETIMES USES OINTMENT BUT MOSTLY TAKES AYURVEDIC MEDICINE",
        "GASTRIC", "GATRIC", "APPENDIX"
      ) ~ 13,

      # Prostate
      v604a %in% c(
        "PROSTATE", "PROSTED", "PROSTRATE", "PROTEST", "POSTATE", "PROTESTED KO SAMASYA", "POSTERT",
        "PROSTATE PROBLEM", "POSTED", "POSTERD", "PROSTHETIC", "PROSTRATE", "PROSTATE", "POSTATE"
      ) ~ 21,

      # Blood Pressure
      v604a %in% c("PRESSURE", "PRESSURE LOW", "BP LOW") ~ 2,

      # Migraine
      v604a %in% c("MIGRAINE", "MIGRANE", "MIGRAIN") ~ 22,

      # Joint / Knee pain
      v604a %in% c("KNEE PAIN", "BATH") ~ 5,

      # Cancer
      v604a %in% c("CANCER", "TONGUE CANCER") ~ 8,

      # Skin diseases
      v604a %in% c(
        "CHHALAKO ROG", "CHALA ROG", "SKIN PROBLEM", "SKIN ALLERGY",
        "SKIN ALLERGIES", "SKIN ELERGY", "SKIN", "SKIN CONDITION", "XALA SAMBANDHI",
        "छालाको समस्या छाला रोग", "BRAIN PROBLEM - SCARS", "ACNE ISSUES"
      ) ~ 23,

      # Anxiety
      v604a %in% c("ANZITY", "ANJEITY", "ANXIETY") ~ 18,

      # Epilepsy
      v604a %in% c("MIRGI", "SEIZURE", "SIJAR") ~ 9,

      # Thyroid
      v604a %in% c("THYROID", "THOYRED") ~ 12,

      # Neurological
      v604a %in% c("NEURO", "CEREBRAL PAIN", "BRAIN TUMOR", "BRAIN PROBLEM") ~ 16,

      #KIDNEY 
      v604a %in% c(
        "KIDNEY MA PATHALI", "KIDNEY STONES", "KIDNEY MA PATHALI", "KIDNEY STONES KO UPRESAN GAREKO"
      ) ~ 6,

      #EYE PROBLEMS
      v604a %in% c(
        "RETINA PROBLEM JALBINDU", "EYE ISSUES", "AKHA SAMBANDI", "JALBINDU VAYEKO", "JALBINDU VAYEKO REGULAR MEDICINE LAGAUNE PARXA", "EYE PROBLEMS", "JALBINDU","EYE PROBLEM",
        "AAKHAKO SAMASYA", "AAKHA SAMBANDHI SAMASYA", "AAKHAKO - MOTIBIDNU SAMASYA", "AAKHAKO SAMASYA", "MOTIBINDU", "EYE INFECTION", "EYE"
    ) ~24,

    #TUBERCULOSIS
    v604a %in% c(
      "TB ROG"
    ) ~ 10,

    #PARALYSIS 
    v604a %in% c(
      "PARALYZED", "PARALYSIS", "PARALICSES"
    ) ~ 25,

      TRUE ~ v604
    )
  )

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101))

section6b1 <- merge(
  section6b1,
  section1a[, c("uniq_id", "v104a")],
  by = "uniq_id"
)

chronic_condition <- chronic_condition %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b4 <- section6b4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )


section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)  )

section6b1 <- section6b1 %>%
  mutate(
    v606 = if_else(
      v606 >= v104a & !is.na(v606) & !is.na(v104a),
      as.numeric(
        str_extract_all(as.character(v606), "\\d")[[1]] %>% max()
      ),
      v606
    )
  )

chronic_condition <- health %>%
  left_join(
    section6b3 %>% select(disease_id, 15:28),
    by = "disease_id"
  )

chronic_condition <- chronic_condition %>%
  left_join(
    section6b4 %>% select(disease_id, 16:28),
    by = "disease_id"
  )


section6c1 <- section6c1 %>%
  mutate(
    v630a = str_trim(v630a),
    v630a = str_squish(v630a),
    v630a = toupper(v630a),

    v630 = case_when(

      # Cold / cough
      v630a %in% c(
        "रुघाखोकी", "खोकी", "RUGHA KHOKI", "RUGAKHOKI", "ROUGHA KHOLA",
        "KHOKI", "COLD ALLERGY", "COLD", "CHISO RUGHA"
      ) ~ 6,

      # Abdominal / gastric / appendix
      v630a %in% c(
        "एपेन्डिसाइड", "PETKO SAMSYA", "PETKO SAMASYAA",
        "PETKO OPERATION GAREKO", "PETDUKHERA", "PETDUKHEKO",
        "CPETA DUKHE KO", "PETA DUKHEKO",
        "PET KO SAMASYAA VAAKO BU K HO VANERA THA NAVAAKO HOSPITAL MA.",
        "PET KO SAMASAYA", "PET KO OPERATION", "PET KAMAR DUKHNA",
        "PET DUKNE", "PET DUKHYAKO", "PET DUKHNA", "PET DUKHERW",
        "PET DUKHERA FOOD POISON BHAKO", "PET DUKHERA",
        "PET DUKHEKO", "PET DUKHANE", "PET DHUKERA",
        "ABDOMINAL PAIN", "ABDOMEN PAIN",
        "GASTRITIS", "GASTRIC PROBLEM", "GASTRIC INFECTION",
        "GASTRIC", "GASTIK",
        "DOCTOR DOESN'T KNOW ABOUT THEIR CONDITION THEY SAY HE HAS GASTRITIS.",
        "APPENDIX KO OPTION", "APPENDIX", "APPENDICITIS",
        "STOMACH PAIN", "STOMACH ACHE"
      ) ~ 18,

      # Tonsils
      v630a %in% c("TONSILS", "TONSILLITIS", "TONSIL") ~ 19,

      # Pregnancy / gynecological
      v630a %in% c(
        "PREGNANT", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW",
        "PREGNANCY CHECK UP", "SUTKERI",
        "PREGNANT NA BHAYERA CHECK UP",
        "PERIOD PAIN", "PERIOD ANIYAMIT",
        "PATHEGHAR KO SAMASYA", "PATHEGHAR KO OPERATION"
      ) ~ 20,

      # Headache / head injury
      v630a %in% c(
        "THAUKO DUKHANE", "TAUKO MA GHAU", "TAUKO DUKHNE",
        "TAUKO DUKHEKO VYERW", "TAUKO DUKHEKO",
        "TAU KO DUKHNA",
        "HEADACHE", "HEADACE", "HEAD INJURIES"
      ) ~ 21,

      # Urinary problems
      v630a %in% c(
        "PISABMA KHARABI", "PISAB THAILIKO PATHARI", "PISAB ROKIYAKO"
      ) ~ 10,

      # ENT / ear
      v630a %in% c("ENT", "EAR PROBLEM") ~ 13,

      TRUE ~ v630
    )
  )



acute_condition <- acute_condition %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
)
  
acute_condition <- acute_condition %>%
  mutate(
    v630a = if_else(v630 != 96, "", v630a)
  )


section6c4 <- section6c4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
)

acute_condition <- acute_condition %>%
  left_join(
    section6c2 %>% select(disease_id, 13:21),
    by = "disease_id"
  )

acute_condition <- acute_condition %>%
  left_join(
    section6c4 %>% select(disease_id, 13:33),
    by = "disease_id"
  )

acute_condition <- merge(
  acute_condition, 
  section1a[, c("uniq_id", "v102")],
  by = "uniq_id", 
  all = FALSE
)

chronic_diseases <- merge(
  chronic_diseases, 
  section1a[, c("uniq_id", "v103", "enrollment")],
  by = "uniq_id", 
  all = FALSE
)



chronic_diseases <- chronic_diseases %>%
  mutate(
    chronic_condition = case_when(
      chronic_condition == 1  ~ "Heart Diseases",
      chronic_condition == 2  ~ "Hypertension",
      chronic_condition == 3  ~ "Diabetes",
      chronic_condition == 4  ~ "Asthma/COPD",
      chronic_condition == 5  ~ "Rheumatism/Arthritis",
      chronic_condition == 6  ~ "Kidney Diseases",
      chronic_condition == 7  ~ "Liver Diseases",
      chronic_condition == 8  ~ "Cancer",
      chronic_condition == 9  ~ "Epilepsy",
      chronic_condition == 10 ~ "Tuberculosis",
      chronic_condition == 11 ~ "HIV/AIDS",
      chronic_condition == 12 ~ "Thyroid Disorders",
      chronic_condition == 13 ~ "Chronic Gastrointestinal Diseases",
      chronic_condition == 14 ~ "Gynaecological Problems",
      chronic_condition == 15 ~ "Chronic Orthopaedic Problems",
      chronic_condition == 16 ~ "Neurological Conditions",
      chronic_condition == 17 ~ "Alzheimer's/Parkinson's",
      chronic_condition == 18 ~ "Mental Illness",
      chronic_condition == 96 ~ "Others (Specify)",
      chronic_condition == 19 ~ "Cholestrol",
      chronic_condition == 20 ~ "Uric Acid",
      chronic_condition == 21 ~ "Prostate", 
      chronic_condition == 22 ~ "Migrane", 
      chronic_condition == 23 ~ "Skin diseases", 
      chronic_condition == 24 ~ "Eye related", 
      chronic_condition == 25 ~ "Paralysis",
      TRUE ~ NA_character_
    ),
    v103 = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female", 
      TRUE ~ NA_character_
    ),
    enrollment = case_when(
      enrollment == 1 ~ "NHIP", 
      enrollment == 2 ~ "Non NHIP", 
      enrollment == 3 ~ "SSF", 
      enrollment == 4 ~ "Non SSF", 
      TRUE ~ NA_character_
    )
  )

chronic_checkups <- merge(
  chronic_checkups, 
  section6b1[, c("disease_id", "v611")],
  by = "disease_id"
)

chronic_inpatient_costs <- section6b3 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_inpatient_costs <- merge(
  chronic_inpatient_costs, 
  section6b1[, c("disease_id", "v605a", "v605b", "v611")],
  by = "disease_id"
)