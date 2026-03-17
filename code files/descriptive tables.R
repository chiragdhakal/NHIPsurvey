library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)
library(labelled)
library(officer)
library(flextable)
library(stringr)

in_dir <- "data"

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
    urban_rural = case_when(
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


#DESCRIPTIVE TABLE BASED ON SEX, ETHNICITY, RELIGION AND TYPE OF HOUSEHOLD MEMBER

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
  mutate(hhid = paste0(psu, "-", hhld))

section1a <- section1a %>%
  mutate(uniq_id = paste0(psu, "-", hhld, "-", v101))

desc_sec1b <- section1b

desc_sec1b <- merge(
  desc_sec1b, 
  section1a[, c("personid", "v103", "v105")],
  by = "personid"
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
  freq <- table(fvar)

  tibble(
    variable = v,
    variable_label = var_label(var) %||% NA,
    value_label = names(freq),
    count = as.integer(freq), 
    percent = round(100 * count / length(var), 2)
  )
})

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
addWorksheet(wb, "original_table");  writeData(wb, "original_table", table_sec1b)
addWorksheet(wb, "literate_pg");     writeData(wb, "literate_pg", tbl_literate)
addWorksheet(wb, "attend_pg");       writeData(wb, "attend_pg", tbl_attend)
addWorksheet(wb, "highest_pg");      writeData(wb, "highest_pg", tbl_highest)
addWorksheet(wb, "ethnicity_literacy"); writeData(wb, "ethnicity_literacy", ethnicity_literacy)
addWorksheet(wb, "ethnicity_gender_lit"); writeData(wb, "ethnicity_gender_lit", ethnicity_gender_literacy)
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
    percent = round(100 * count / length(var), 2)
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
    percent = round(100 * count / length(var), 2)
  )
})

ft_sec2a3 <- flextable(table_sec2a3)
doc_sec2a3 <- read_docx()
doc_sec2a3 <- body_add_flextable(doc_sec2a3, ft_sec2a3)
print(doc_sec2a3, target = "descriptive tables/doc_sec2a3.docx")


#DESCRIPTIVE TABLE FOR FARM INCOME

desc_farmincome <- merge(
  farm_hh, 
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_farmincome <- desc_farmincome %>%
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    across(
      c(total_production_sale, total_livestock_income, total_farm_income, total_farm_expenditure),
      list(
        p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    total_production_sale    >= total_production_sale_p1,    total_production_sale    <= total_production_sale_p99,
    total_livestock_income   >= total_livestock_income_p1,   total_livestock_income   <= total_livestock_income_p99,
    total_farm_income        >= total_farm_income_p1,        total_farm_income        <= total_farm_income_p99,
    total_farm_expenditure   >= total_farm_expenditure_p1,   total_farm_expenditure   <= total_farm_expenditure_p99
  ) %>%
  summarise(
    across(
      c(total_production_sale, total_livestock_income, total_farm_income, total_farm_expenditure),
      mean, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  select(province, total_production_sale, total_livestock_income, total_farm_income, total_farm_expenditure) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

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
    percapita_wage  >= p1,  percapita_wage  <= p99,
    total_hh_income >= a1,  total_hh_income <= a99
  ) %>%
  group_by(province) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(province, total_hh_income, percapita_wage) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

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
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    p1  = quantile(total_non_agri_income, 0.01, na.rm = TRUE),
    p99 = quantile(total_non_agri_income, 0.99, na.rm = TRUE)
  ) %>%
  filter(total_non_agri_income >= p1, total_non_agri_income <= p99) %>%
  summarise(
    total_non_agri_income = mean(total_non_agri_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

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
  mutate(
    p1  = quantile(rent_annual, 0.01, na.rm = TRUE),
    p99 = quantile(rent_annual, 0.99, na.rm = TRUE)
  ) %>%
  filter(rent_annual >= p1, rent_annual <= p99) %>%
  summarise(
    rent_annual = mean(rent_annual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(province, rent_annual) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

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
  mutate(
    p1  = quantile(cash_assistance_received, 0.01, na.rm = TRUE),
    p99 = quantile(cash_assistance_received, 0.99, na.rm = TRUE)
  ) %>%
  filter(cash_assistance_received >= p1, cash_assistance_received <= p99) %>%
  summarise(
    cash_assistance_received = mean(cash_assistance_received, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(province, cash_assistance_received) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

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
  mutate(percent = (hhlds / nrow(section0)) * 100)

desc_remittance <- desc_remittance %>%
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    p1_recv  = quantile(total_amount_received, 0.01, na.rm = TRUE),
    p99_recv = quantile(total_amount_received, 0.99, na.rm = TRUE),
    p1_sent  = quantile(total_sent_abroad,     0.01, na.rm = TRUE),
    p99_sent = quantile(total_sent_abroad,     0.99, na.rm = TRUE)
  ) %>%
  filter(
    total_amount_received >= p1_recv, total_amount_received <= p99_recv,
    total_sent_abroad     >= p1_sent, total_sent_abroad     <= p99_sent
  ) %>%
  summarise(
    total_amount_received = mean(total_amount_received, na.rm = TRUE),
    total_sent_abroad     = mean(total_sent_abroad,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(province, total_amount_received, total_sent_abroad) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

write.xlsx(remittance_households, "descriptive tables/remittance_households.xlsx")
write.xlsx(desc_remittance,       "descriptive tables/desc_remittance.xlsx")


#DESCRIPTIVE TABLE FOR TOTAL INCOME

desc_totalincome <- merge(
  income_hhld, 
  section0[, c("hhid", "province", "hhld_member_t", "urban_rural")],
  by = "hhid", 
  all = FALSE
)

desc_totalincome <- desc_totalincome %>%
  mutate(
    hhld_member_t  = as.numeric(hhld_member_t),
    percapita_income = total_income / hhld_member_t
  ) %>%
  select(-hhid)

desc_totalincome_province <- desc_totalincome %>%
  mutate(
    total_income = total_hh_income + total_farm_income +
                   net_remittance_received + cash_assistance_received +
                   total_non_agri_income + rent_annual + other_income_annual
  ) %>%
  group_by(province) %>%
  mutate(
    p1_income  = quantile(total_income,      0.01, na.rm = TRUE),
    p99_income = quantile(total_income,      0.99, na.rm = TRUE),
    p1_pc      = quantile(percapita_income,  0.01, na.rm = TRUE),
    p99_pc     = quantile(percapita_income,  0.99, na.rm = TRUE)
  ) %>%
  filter(
    total_income     >= p1_income, total_income     <= p99_income,
    percapita_income >= p1_pc,     percapita_income <= p99_pc
  ) %>%
  summarise(
    total_income     = mean(total_income,     na.rm = TRUE),
    percapita_income = mean(percapita_income, na.rm = TRUE),
    .groups = "drop"
  )

desc_totalincome_urban <- desc_totalincome %>%
  group_by(urban_rural, province) %>%
  mutate(
    p1_income  = quantile(total_income,      0.01, na.rm = TRUE),
    p99_income = quantile(total_income,      0.99, na.rm = TRUE),
    p1_pc      = quantile(percapita_income,  0.01, na.rm = TRUE),
    p99_pc     = quantile(percapita_income,  0.99, na.rm = TRUE)
  ) %>%
  filter(
    total_income     >= p1_income, total_income     <= p99_income,
    percapita_income >= p1_pc,     percapita_income <= p99_pc
  ) %>%
  summarise(
    total_income     = mean(total_income,     na.rm = TRUE),
    percapita_income = mean(percapita_income, na.rm = TRUE),
    .groups = "drop"
  )

desc_totalincome_province <- desc_totalincome_province %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

desc_totalincome_urban <- desc_totalincome_urban %>%
  pivot_longer(cols = -c(urban_rural, province), names_to = "variable", values_to = "mean_value")

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
    hhld_member_t            = as.numeric(hhld_member_t),
    total_consumption        = total_food_annual + non_food_annual + abroad_annual + goods_annual,
    percapita_foodconsumption = total_food_annual / hhld_member_t,
    percapita_consumption    = total_consumption / hhld_member_t
  )

desc_consumption <- desc_consumption %>%
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    across(
      c(total_food_annual, non_food_annual, goods_annual,
        percapita_foodconsumption, total_consumption, percapita_consumption),
      list(
        p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    total_food_annual             >= total_food_annual_p1,             total_food_annual             <= total_food_annual_p99,
    non_food_annual               >= non_food_annual_p1,               non_food_annual               <= non_food_annual_p99,
    goods_annual                  >= goods_annual_p1,                  goods_annual                  <= goods_annual_p99,
    percapita_foodconsumption     >= percapita_foodconsumption_p1,     percapita_foodconsumption     <= percapita_foodconsumption_p99,
    total_consumption             >= total_consumption_p1,             total_consumption             <= total_consumption_p99,
    percapita_consumption         >= percapita_consumption_p1,         percapita_consumption         <= percapita_consumption_p99
  ) %>%
  summarise(
    across(
      c(total_food_annual, non_food_annual, goods_annual,
        percapita_foodconsumption, total_consumption, percapita_consumption),
      mean, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  select(province, total_food_annual, non_food_annual, goods_annual,
         percapita_foodconsumption, total_consumption, percapita_consumption) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

write.xlsx(desc_consumption, "descriptive tables/desc_consumption.xlsx")


#DESCRIPTIVE TABLE FOR EDUCATION EXPENSES

desc_edu <- merge(
  education_expenses,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_edu <- desc_edu %>%
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    across(
      c(tuition_fee, other_fee, dress_expense, books_expense,
        transportation_expense, private_tuition, other_expense),
      list(
        p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    tuition_fee            >= tuition_fee_p1,            tuition_fee            <= tuition_fee_p99,
    other_fee              >= other_fee_p1,              other_fee              <= other_fee_p99,
    dress_expense          >= dress_expense_p1,          dress_expense          <= dress_expense_p99,
    books_expense          >= books_expense_p1,          books_expense          <= books_expense_p99,
    transportation_expense >= transportation_expense_p1, transportation_expense <= transportation_expense_p99,
    private_tuition        >= private_tuition_p1,        private_tuition        <= private_tuition_p99,
    other_expense          >= other_expense_p1,          other_expense          <= other_expense_p99
  ) %>%
  summarise(
    across(
      c(tuition_fee, other_fee, dress_expense, books_expense,
        transportation_expense, private_tuition, other_expense),
      mean, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  select(province, tuition_fee, other_fee, dress_expense, books_expense,
         transportation_expense, private_tuition, other_expense) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

write.xlsx(desc_edu, "descriptive tables/desc_edu.xlsx")


#DESCRIPTIVE TABLE FOR OTHER INCOME

desc_otherincome <- merge(
  other_income,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

desc_otherincome <- desc_otherincome %>%
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    p1  = quantile(other_income_annual, 0.01, na.rm = TRUE),
    p99 = quantile(other_income_annual, 0.99, na.rm = TRUE)
  ) %>%
  filter(other_income_annual >= p1, other_income_annual <= p99) %>%
  summarise(
    other_income_annual = mean(other_income_annual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(province, other_income_annual) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

write.xlsx(desc_otherincome, "descriptive tables/desc_otherincome.xlsx")


#DESCRIPTIVE TABLE FOR RATIO OF INCOME COMPONENTS

ratio_income <- income_hhld %>%
  mutate(across(
    c(total_hh_income, rent_annual, net_remittance_received,
      total_farm_income, cash_assistance_received,
      other_income_annual, total_non_agri_income),
    ~ replace_na(as.numeric(.), 0)
  )) %>%
  mutate(
    wage_ratio             = total_hh_income          / total_income,
    rent_ratio             = rent_annual               / total_income,
    remittance_ratio       = net_remittance_received   / total_income,
    farm_income_ratio      = total_farm_income         / total_income,
    cash_assistance_ratio  = cash_assistance_received  / total_income,
    other_income_ratio     = other_income_annual       / total_income,
    non_agri_ratio         = total_non_agri_income     / total_income
  ) %>%
  select(hhid, wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio,
         cash_assistance_ratio, other_income_ratio, non_agri_ratio)

ratio_income <- merge(
  ratio_income,
  section0[, c("hhid", "province")],
  by = "hhid", 
  all = FALSE
)

ratio_income <- ratio_income %>%
  select(-hhid) %>%
  group_by(province) %>%
  mutate(
    across(
      c(wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio,
        cash_assistance_ratio, other_income_ratio, non_agri_ratio),
      list(
        p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    wage_ratio            >= wage_ratio_p1,            wage_ratio            <= wage_ratio_p99,
    rent_ratio            >= rent_ratio_p1,            rent_ratio            <= rent_ratio_p99,
    remittance_ratio      >= remittance_ratio_p1,      remittance_ratio      <= remittance_ratio_p99,
    farm_income_ratio     >= farm_income_ratio_p1,     farm_income_ratio     <= farm_income_ratio_p99,
    cash_assistance_ratio >= cash_assistance_ratio_p1, cash_assistance_ratio <= cash_assistance_ratio_p99,
    other_income_ratio    >= other_income_ratio_p1,    other_income_ratio    <= other_income_ratio_p99,
    non_agri_ratio        >= non_agri_ratio_p1,        non_agri_ratio        <= non_agri_ratio_p99
  ) %>%
  summarise(
    across(
      c(wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio,
        cash_assistance_ratio, other_income_ratio, non_agri_ratio),
      mean, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  select(province, wage_ratio, rent_ratio, remittance_ratio, farm_income_ratio,
         cash_assistance_ratio, other_income_ratio, non_agri_ratio) %>%
  pivot_longer(cols = -province, names_to = "variable", values_to = "mean_value")

write.xlsx(ratio_income, "descriptive tables/ratio_income.xlsx")


#DESCRIPTIVE TABLE FOR LABOUR AND EMPLOYMENT

section1a <- section1a %>%
  mutate(
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

labor_force <- section1a %>%
  filter(v104a >= 10 & v104a <= 65) %>%
  mutate(
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(hhid, uniq_id, v103, v105)

labor_force <- merge(labor_force, section0[, c("hhid", "province")], by = "hhid")

section7_1 <- section7 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid    = paste0(psu, "-", hhld)
  ) %>%
  select(uniq_id, v702, v705, v708, v710)

labor_force <- merge(labor_force, section7_1, by = "uniq_id")

labor_force <- labor_force %>%
  mutate(lf_participant = ifelse((v702 == 1 | v705 == 1) %in% TRUE, 1, 0))

lfpr_province <- labor_force %>%
  group_by(province) %>%
  summarise(
    working_age  = n(),
    labor_force  = sum(lf_participant, na.rm = TRUE),
    lfpr         = round((labor_force / working_age) * 100, 2)
  )

lfpr_ethnicity <- labor_force %>%
  group_by(v105) %>%
  summarise(
    working_age = n(),
    labor_force = sum(lf_participant, na.rm = TRUE),
    lfpr        = round(labor_force / working_age * 100, 2)
  )

lfpr_ethnicity_gender <- labor_force %>%
  group_by(v103, v105) %>%
  summarise(
    working_age = n(),
    labor_force = sum(lf_participant, na.rm = TRUE),
    lfpr        = round(labor_force / working_age * 100, 2),
    .groups     = "drop"
  )

lfpr_province_gender <- labor_force %>%
  group_by(province, v103) %>%
  summarise(
    working_age = n(),
    labor_force = sum(lf_participant, na.rm = TRUE),
    lfpr        = round(labor_force / working_age * 100, 2),
    .groups     = "drop"
  )

work_population <- section7 %>%
  filter(v702 == 1 | v703 == 1 | v704 == 1) %>%
  mutate(
    v714a   = str_extract(v714a, "^[0-9]+") %>% as.numeric(),
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v708) %>%
  filter(!is.na(v708))

work_population <- merge(work_population, section0[, c("hhid", "province")], by = "hhid")
work_population <- merge(work_population, section1a[, c("uniq_id", "v103", "v105")], by = "uniq_id")

work_type_nepal <- work_population %>%
  group_by(v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

work_type_province <- work_population %>%
  group_by(province, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

work_type_ethnicity <- work_population %>%
  group_by(v105, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

work_type_gender <- work_population %>%
  group_by(v103, v708) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

write.xlsx(lfpr_province,         "descriptive tables/lfpr_province.xlsx")
write.xlsx(lfpr_province_gender,  "descriptive tables/lfpr_province_gender.xlsx")
write.xlsx(lfpr_ethnicity,        "descriptive tables/lfpr_ethnicity.xlsx")
write.xlsx(lfpr_ethnicity_gender, "descriptive tables/lfpr_ethnicity_gender.xlsx")
write.xlsx(work_type_nepal,       "descriptive tables/work_type_nepal.xlsx")
write.xlsx(work_type_ethnicity,   "descriptive tables/work_type_ethnicity.xlsx")
write.xlsx(work_type_province,    "descriptive tables/work_type_province.xlsx")
write.xlsx(work_type_gender,      "descriptive tables/work_type_gender.xlsx")


#DESCRIPTIVE TABLE FOR CHRONIC ILLNESSES

chronic_qualified <- section1a %>%
  filter(v109 %in% c(1, 2) & v104a >= 5) %>%
  mutate(
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)

chronic_qualified <- merge(chronic_qualified, section0[, c("hhid", "province", "domain")], by = "hhid")

section6b1 <- section6b1 %>%
  mutate(
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

chronic_qualified <- merge(chronic_qualified, section6b1[, c("uniq_id", "v604")], by = "uniq_id")

chronic_nepal <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  count(v604) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

chronic_province <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(province, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

chronic_gender <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(v103, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

chronic_domain <- chronic_qualified %>%
  filter(!is.na(v604)) %>%
  group_by(domain, v604) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

write.xlsx(chronic_nepal,    "descriptive tables/chronic_nepal.xlsx")
write.xlsx(chronic_province, "descriptive tables/chronic_province.xlsx")
write.xlsx(chronic_gender,   "descriptive tables/chronic_gender.xlsx")
write.xlsx(chronic_domain,   "descriptive tables/chronic_domain.xlsx")


#DESCRIPTIVE TABLE FOR ACUTE ILLNESSES

acute_qualified <- section1a %>%
  filter(v109 %in% c(1, 2) & v104a >= 5) %>%
  mutate(
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) %>%
  select(uniq_id, hhid, v103)

acute_qualified <- merge(acute_qualified, section0[, c("hhid", "province", "domain")], by = "hhid")

section6c1 <- section6c1 %>%
  mutate(
    v630    = as.numeric(v630),
    hhid    = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

acute_qualified <- merge(acute_qualified, section6c1[, c("uniq_id", "v630")], by = "uniq_id")

acute_nepal <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  count(v630) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

acute_province <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(province, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

acute_gender <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(v103, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

acute_domain <- acute_qualified %>%
  filter(!is.na(v630)) %>%
  group_by(domain, v630) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

write.xlsx(acute_nepal,    "descriptive tables/acute_nepal.xlsx")
write.xlsx(acute_province, "descriptive tables/acute_province.xlsx")
write.xlsx(acute_gender,   "descriptive tables/acute_gender.xlsx")
write.xlsx(acute_domain,   "descriptive tables/acute_domain.xlsx")

#DESCRIPTIVE TABLES FOR CHRONIC INPATIENT HEALTH COSTS

chronic_inpatient_expenditure <- section6b4 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    across(v618a:v618k, ~ replace_na(., 0))
  ) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense      = sum(as.numeric(v618a), na.rm = TRUE),
    opd_expense            = sum(as.numeric(v618b), na.rm = TRUE),
    laboratory_expense     = sum(as.numeric(v618c), na.rm = TRUE),
    imaging_expense        = sum(as.numeric(v618d), na.rm = TRUE),
    medicine_expense       = sum(as.numeric(v618e), na.rm = TRUE),
    med_device_expense     = sum(as.numeric(v618f), na.rm = TRUE),
    transportation_expense = sum(as.numeric(v618g), na.rm = TRUE),
    food_accom_expense     = sum(as.numeric(v618h), na.rm = TRUE),
    care_giver_expense     = sum(as.numeric(v618i), na.rm = TRUE),
    other_cost             = sum(as.numeric(v618j), na.rm = TRUE),
    total_cost             = sum(as.numeric(v618k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_chronic_inpatient = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense,
        medicine_expense, med_device_expense)
    ), na.rm = TRUE)
  )

chronic_inpatient_expenditure <- merge(
  chronic_inpatient_expenditure,
  section0[, c("hhid", "province", "domain")],
  by = "hhid"
)

cost_cols_inpatient <- c("emergency_expense", "opd_expense", "laboratory_expense",
                         "imaging_expense", "medicine_expense", "med_device_expense",
                         "transportation_expense", "food_accom_expense", "care_giver_expense",
                         "other_cost", "total_cost", "total_cost_chronic_inpatient")

trim_by_group <- function(df, group_var, cost_cols) {
  df_bounds <- df %>%
    group_by(across(all_of(group_var))) %>%
    mutate(across(
      all_of(cost_cols),
      list(
        p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    ungroup()
  
  for (col in cost_cols) {
    df_bounds <- df_bounds %>%
      filter(.data[[col]] >= .data[[paste0(col, "_p1")]],
             .data[[col]] <= .data[[paste0(col, "_p99")]])
  }
  
  df_bounds %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(all_of(cost_cols), mean, na.rm = TRUE),
      .groups = "drop"
    )
}

chronic_inpatient_expenditure_province <- trim_by_group(
  chronic_inpatient_expenditure, "province", cost_cols_inpatient
)

chronic_inpatient_expenditure_domain <- trim_by_group(
  chronic_inpatient_expenditure, "domain", cost_cols_inpatient
)

write.xlsx(chronic_inpatient_expenditure_province, "descriptive tables/chronic_inpatient_expenditure_province.xlsx")
write.xlsx(chronic_inpatient_expenditure_domain,   "descriptive tables/chronic_inpatient_expenditure_domain.xlsx")


#DESCRIPTIVE TABLES FOR CHRONIC OUTPATIENT HEALTH COSTS

chronic_outpatient_expenditure <- section6b3 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    across(v614a:v614k, ~ replace_na(., 0))
  ) %>%
  group_by(hhid) %>%
  summarise(
    emergency_expense      = sum(as.numeric(v614a), na.rm = TRUE),
    opd_expense            = sum(as.numeric(v614b), na.rm = TRUE),
    laboratory_expense     = sum(as.numeric(v614c), na.rm = TRUE),
    imaging_expense        = sum(as.numeric(v614d), na.rm = TRUE),
    medicine_expense       = sum(as.numeric(v614e), na.rm = TRUE),
    med_device_expense     = sum(as.numeric(v614f), na.rm = TRUE),
    transportation_expense = sum(as.numeric(v614g), na.rm = TRUE),
    food_accom_expense     = sum(as.numeric(v614h), na.rm = TRUE),
    care_giver_expense     = sum(as.numeric(v614i), na.rm = TRUE),
    other_cost             = sum(as.numeric(v614j), na.rm = TRUE),
    total_cost             = sum(as.numeric(v614k), na.rm = TRUE)
  ) %>%
  mutate(
    across(-hhid, ~ replace_na(as.numeric(.x), 0)),
    total_cost_chronic_outpatient = rowSums(across(
      c(emergency_expense, opd_expense, laboratory_expense, imaging_expense,
        medicine_expense, med_device_expense, transportation_expense,
        food_accom_expense, care_giver_expense, other_cost)
    ), na.rm = TRUE)
  )

chronic_outpatient_expenditure <- merge(
  chronic_outpatient_expenditure,
  section0[, c("hhid", "province", "domain")],
  by = "hhid"
)

cost_cols_outpatient <- c("emergency_expense", "opd_expense", "laboratory_expense",
                          "imaging_expense", "medicine_expense", "med_device_expense",
                          "transportation_expense", "food_accom_expense", "care_giver_expense",
                          "other_cost", "total_cost", "total_cost_chronic_outpatient")

chronic_outpatient_expenditure_province <- trim_by_group(
  chronic_outpatient_expenditure, "province", cost_cols_outpatient
)

chronic_outpatient_expenditure_domain <- trim_by_group(
  chronic_outpatient_expenditure, "domain", cost_cols_outpatient
)

write.xlsx(chronic_outpatient_expenditure_province, "descriptive tables/chronic_outpatient_expenditure_province.xlsx")
write.xlsx(chronic_outpatient_expenditure_domain,   "descriptive tables/chronic_outpatient_expenditure_domain.xlsx")


#HEALTH TABLES

section6b1 <- merge(
  section6b1, 
  section1a[, c("v103", "personid")], 
  by = "personid"
)

chronic_diseases <- section6b1 %>%
  filter(!is.na(v604) & v604 != 96) %>%
  mutate(
    chronic_condition = case_when(
      v604 == 1  ~ "Heart Diseases",
      v604 == 2  ~ "Hypertension",
      v604 == 3  ~ "Diabetes",
      v604 == 4  ~ "Asthma/COPD",
      v604 == 5  ~ "Rheumatism/Arthritis",
      v604 == 6  ~ "Kidney Diseases",
      v604 == 7  ~ "Liver Diseases",
      v604 == 8  ~ "Cancer",
      v604 == 9  ~ "Epilepsy",
      v604 == 10 ~ "Tuberculosis",
      v604 == 11 ~ "HIV/AIDS",
      v604 == 12 ~ "Thyroid Disorders",
      v604 == 13 ~ "Chronic Gastrointestinal Diseases",
      v604 == 14 ~ "Gynaecological Problems",
      v604 == 15 ~ "Chronic Orthopaedic Problems",
      v604 == 16 ~ "Neurological Conditions",
      v604 == 17 ~ "Alzheimer's/Parkinson's",
      v604 == 18 ~ "Mental Illness",
      v604 == 20 ~ "Uric Acid",
      v604 == 21 ~ "Male Reproductive Disease",
      v604 == 22 ~ "ENT",
      v604 == 23 ~ "Skin Diseases",
      v604 == 24 ~ "Eye diseases",
      v604 == 26 ~ "Trauma/Injury",
      v604 == 27 ~ "Lung Diseases",
      v604 == 28 ~ "Blood and Blood Vessel Diseases",
      v604 == 29 ~ "Infectious Diseases",
      v604 == 30 ~ "Disability",
      v604 == 31 ~ "Geriatric Problems",
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

chronic_condition_count <- chronic_diseases %>%
  group_by(chronic_condition) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

chronic_condition_count_gender <- chronic_diseases %>%
  group_by(v103, chronic_condition) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

chronic_condition_count_provnice <- chronic_diseases %>%
  group_by(province, chronic_condition) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))

chronic_condition_enrollment <- chronic_diseases %>%
  group_by(enrollment, chronic_condition) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 2))


#LABELING CHRONIC CONDITIONS 

label_chronic <- function(x) {
  case_when(
    x == 1  ~ "Heart Diseases",         x == 2  ~ "Hypertension",
    x == 3  ~ "Diabetes",               x == 4  ~ "Asthma/COPD",
    x == 5  ~ "Rheumatism/Arthritis",   x == 6  ~ "Kidney Diseases",
    x == 7  ~ "Liver Diseases",         x == 8  ~ "Cancer",
    x == 9  ~ "Epilepsy",               x == 10 ~ "Tuberculosis",
    x == 11 ~ "HIV/AIDS",               x == 12 ~ "Thyroid Disorders",
    x == 13 ~ "Chronic Gastrointestinal Diseases",
    x == 14 ~ "Gynaecological Problems",
    x == 15 ~ "Chronic Orthopaedic Problems",
    x == 16 ~ "Neurological Conditions",
    x == 17 ~ "Alzheimer's/Parkinson's",
    x == 18 ~ "Mental Illness",         x == 20 ~ "Uric Acid",
    x == 21 ~ "Male Reproductive Disease", x == 22 ~ "ENT",
    x == 23 ~ "Skin Diseases",          x == 24 ~ "Eye diseases",
    x == 26 ~ "Trauma/Injury",          x == 27 ~ "Lung Diseases",
    x == 28 ~ "Blood and Blood Vessel Diseases",
    x == 29 ~ "Infectious Diseases",    x == 30 ~ "Disability",
    x == 31 ~ "Geriatric Problems",
    TRUE ~ NA_character_
  )
}

#LABELING ACUTE CONDITIONS 

label_acute <- function(x) {
  case_when(
    x == 1  ~ "Diarrhoea",              x == 2  ~ "Typhoid",
    x == 3  ~ "Dengue",                 x == 4  ~ "Malaria",
    x == 5  ~ "Acute Respiratory Infection",
    x == 6  ~ "Cold/Flu/Fever",         x == 7  ~ "Pneumonia",
    x == 8  ~ "Measles",                x == 9  ~ "Jaundice",
    x == 10 ~ "UTI",                    x == 11 ~ "Dental Problem",
    x == 12 ~ "Acute Eye Infection",    x == 13 ~ "Acute Ear Infection",
    x == 14 ~ "Skin Disease",           x == 15 ~ "Injury",
    x == 16 ~ "Accident",               x == 17 ~ "Other Fever",
    x == 18 ~ "Animal Bite",            x == 19 ~ "Arthritis",
    x == 20 ~ "Blood Diseases",         x == 21 ~ "Cancer",
    x == 22 ~ "Gastrointestinal Diseases",
    x == 23 ~ "Congenital Anomaly",     x == 24 ~ "COPD",
    x == 25 ~ "Pregnancy/Postpartum",   x == 26 ~ "Disability",
    x == 27 ~ "ENT",                    x == 28 ~ "Eye Problems",
    x == 29 ~ "Fungal Infections",      x == 30 ~ "Geriatric Problem",
    x == 31 ~ "Gynecological Problem",  x == 32 ~ "Heart Disease",
    x == 33 ~ "Hernia",                 x == 34 ~ "HIV",
    x == 35 ~ "Infectious Disease",     x == 36 ~ "Kidney Disease",
    x == 37 ~ "Liver Disease",          x == 38 ~ "Lungs Disease",
    x == 39 ~ "Male Reproductive Diseases",
    x == 40 ~ "Mental Illness",         x == 41 ~ "Neurological Conditions",
    x == 42 ~ "Uric Acid",              x == 43 ~ "Warts",
    x == 44 ~ "Worms",
    TRUE ~ NA_character_
  )
}

#DESCRIPTIVE TABLES FOR CHRONIC CONSTS

chronic_outpatient_average <- section6b3 %>%
  select(enrollment, province, v604, v614a:v614k) %>%
  mutate(
    across(v614a:v614k, ~na_if(.x, 0))
  ) %>%
  rename(
    chronic_condition = v604,
    emergency_costs          = v614a,
    opd_charges              = v614b, 
    laboratory_costs         = v614c, 
    imaging_costs            = v614d, 
    medicine_costs           = v614e, 
    medical_supplies_costs   = v614f, 
    transportation_costs     = v614g, 
    accomodation_costs       = v614h,
    care_giver_costs         = v614i, 
    other_costs              = v614j, 
    total_costs              = v614k
  )

chronic_outpatient_average <- chronic_outpatient_average %>%
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
      chronic_condition == 20 ~ "Uric Acid",
      chronic_condition == 21 ~ "Male Reproductive Disease", 
      chronic_condition == 22 ~ "ENT", 
      chronic_condition == 23 ~ "Skin Diseases", 
      chronic_condition == 24 ~ "Eye diseases", 
      chronic_condition == 26 ~ "Trauma/Injury",
      chronic_condition == 27 ~ "Lung Diseases", 
      chronic_condition == 28 ~ "Blood and Blood Vessel Diseases", 
      chronic_condition == 29 ~ "Infectious Diseases", 
      chronic_condition == 30 ~ "Disability",
      chronic_condition == 31 ~ "Geriatric Problems",
      TRUE ~ NA_character_
    )
  )

chronic_outpatient_summary <- chronic_outpatient_average %>%
  group_by(chronic_condition) %>%
  summarise(
    across(
      emergency_costs:total_costs,
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        min  = ~ min(.x, na.rm = TRUE),
        max  = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
  )


chronic_outpatient_enrollment_summary <- chronic_outpatient_average %>%
  group_by(enrollment, chronic_condition) %>%
  summarise(
    across(
      emergency_costs:total_costs, 
      list(
        mean = ~ mean(.x, na.rm = TRUE), 
        min = ~ min(.x, na.rm = TRUE), 
        max = ~ max(.x, na.rm = TRUE)
      ), 
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
  )

write.xlsx(chronic_outpatient_summary, "chronic_outpatient_summary.xlsx")
write.xlsx(chronic_outpatient_enrollment_summary, "chronic_outpatient_enrollment_summary.xlsx")

chronic_inpatient_cost_cols <- c("emergency_costs", "bed_charges", "laboratory_costs",
                                 "imaging_costs", "medicine_costs", "medical_supplies_costs",
                                 "transportation_costs", "accomodation_costs",
                                 "care_giver_costs", "other_costs", "total_costs")

chronic_inpatient_average <- section6b4 %>%
  select(enrollment, v604, v618a, v618b, v618b1, v618c:v618k) %>%
  mutate(across(v618a:v618k, ~ na_if(.x, 0))) %>%
  rename(
    chronic_condition      = v604,
    emergency_costs        = v618a,
    bed_charges            = v618b,
    days                   = v618b1,
    laboratory_costs       = v618c,
    imaging_costs          = v618d,
    medicine_costs         = v618e,
    medical_supplies_costs = v618f,
    transportation_costs   = v618g,
    accomodation_costs     = v618h,
    care_giver_costs       = v618i,
    other_costs            = v618j,
    total_costs            = v618k
  ) %>%
  mutate(chronic_condition = label_chronic(chronic_condition))

chronic_inpatient_summary <- chronic_inpatient_average %>%
  group_by(chronic_condition) %>%
  summarise(
    across(
      emergency_costs:total_costs,
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        min  = ~ min(.x, na.rm = TRUE),
        max  = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
  )

chronic_inpatient_enrollment_summary <- chronic_inpatient_average %>%
  group_by(enrollment, chronic_condition) %>%
  summarise(
    across(
      emergency_costs:total_costs, 
      list(
        mean = ~ mean(.x, na.rm = TRUE), 
        min = ~ min(.x, na.rm = TRUE), 
        max = ~ max(.x, na.rm = TRUE)
      ), 
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
  )

write.xlsx(chronic_inpatient_summary, "chronic_inpatient_summary.xlsx")


acute_cost_cols <- c("emergency_costs", "opd_ipd_charges", "laboratory_costs",
                     "imaging_costs", "medicine_costs", "medical_supplies_costs",
                     "transportation_costs", "accomodation_costs",
                     "care_giver_costs", "other_costs", "total_costs")

acute_average <- section6c4 %>%
  select(v630, v651a:v651k) %>%
  mutate(across(v651a:v651k, ~ na_if(.x, 0))) %>%
  rename(
    emergency_costs        = v651a,
    opd_ipd_charges        = v651b,
    laboratory_costs       = v651c,
    imaging_costs          = v651d,
    medicine_costs         = v651e,
    medical_supplies_costs = v651f,
    transportation_costs   = v651g,
    accomodation_costs     = v651h,
    care_giver_costs       = v651i,
    other_costs            = v651j,
    total_costs            = v651k
  ) %>%
  mutate(v630 = label_acute(v630))

acute_average_summary <- trim_summarise(
  acute_average, "v630", acute_cost_cols
)

write.xlsx(acute_average_summary, "acute_average_summary.xlsx")

# FOOD AND EXPENDITURE SUMMARIES

trim_summarise_ungrouped <- function(df, group_var, cost_cols) {
  df %>%
    group_by(across(all_of(group_var))) %>%
    mutate(across(
      all_of(cost_cols),
      list(
        p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    filter(if_all(
      all_of(cost_cols),
      ~ .x >= get(paste0(cur_column(), "_p1")) &
        .x <= get(paste0(cur_column(), "_p99"))
    )) %>%
    summarise(
      across(
        all_of(cost_cols),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          min  = ~ min(.x,  na.rm = TRUE),
          max  = ~ max(.x,  na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      across(starts_with("n_"), ~ sum(!is.na(.x))),
      .groups = "drop"
    )
}

food_at_home_summary <- section3a %>%
  mutate(
    across(v303:v305, ~ na_if(.x, 0)),
    food_category = case_when(
      v301 == 1  ~ "Grains and Cereals",
      v301 == 2  ~ "Pulses and Lentils",
      v301 == 3  ~ "Meats and Fish",
      v301 == 4  ~ "Eggs and Milk Products (excluding butter)",
      v301 == 5  ~ "Ghee (Butter, lard, and other animal-based oils and fats)",
      v301 == 6  ~ "Cooking (Vegetable) Oils",
      v301 == 7  ~ "Fruits and Nuts (fresh, dried, dehydrated, frozen)",
      v301 == 8  ~ "Vegetables (fresh, dried, dehydrated, frozen)",
      v301 == 9  ~ "Sweets and Confectionery",
      v301 == 10 ~ "Spices and Condiments",
      v301 == 11 ~ "Tea and Coffee",
      v301 == 12 ~ "Non-alcoholic beverages",
      v301 == 13 ~ "Alcoholic Beverages (local or imported)",
      v301 == 14 ~ "Tobacco and Tobacco Products",
      v301 == 15 ~ "Prepared Food Products",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(home_production = v303, food_purchases = v304, inkind_received = v305) %>%
  select(-v301) %>%
  group_by(food_category) %>%
  mutate(
    across(
      c(home_production, food_purchases, inkind_received),
      list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    home_production >= home_production_p1, home_production <= home_production_p99,
    food_purchases  >= food_purchases_p1,  food_purchases  <= food_purchases_p99,
    inkind_received >= inkind_received_p1, inkind_received <= inkind_received_p99
  ) %>%
  summarise(
    across(
      c(home_production, food_purchases, inkind_received),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n_home_production = sum(!is.na(home_production)),
    n_food_purchases  = sum(!is.na(food_purchases)),
    n_inkind_received = sum(!is.na(inkind_received)),
    .groups = "drop"
  )

food_away_home <- section3b %>%
  mutate(
    across(v308:v309, ~ na_if(.x, 0)),
    food_category = case_when(
      v306 == 1 ~ "Tea/Coffee",        v306 == 2 ~ "Breakfast",
      v306 == 3 ~ "Lunch",             v306 == 4 ~ "Afternoon Snack",
      v306 == 5 ~ "Dinner",            v306 == 6 ~ "Soft Drink",
      v306 == 7 ~ "Alcoholic Drink",   v306 == 8 ~ "Other foods",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(household_expenditure = v308, received_as_guest = v309) %>%
  select(-v306) %>%
  group_by(food_category) %>%
  mutate(
    across(
      c(household_expenditure, received_as_guest),
      list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    household_expenditure >= household_expenditure_p1, household_expenditure <= household_expenditure_p99,
    received_as_guest     >= received_as_guest_p1,     received_as_guest     <= received_as_guest_p99
  ) %>%
  summarise(
    across(
      c(household_expenditure, received_as_guest),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n_household_expenditure = sum(!is.na(household_expenditure)),
    n_received_as_guest     = sum(!is.na(received_as_guest)),
    .groups = "drop"
  )

non_food_expenditure <- section4a %>%
  mutate(
    across(v403a:v403b, ~ na_if(.x, 0)),
    nonfood_category = case_when(
      v401 == 1  ~ "Clothing and apparel",
      v401 == 2  ~ "Shoes and Slippers",
      v401 == 3  ~ "Repair and Minor repair of house",
      v401 == 4  ~ "Fuel",
      v401 == 5  ~ "Furniture and Furnishings",
      v401 == 6  ~ "Purchase and Maintenance of Textiles for Household Use",
      v401 == 7  ~ "Purchase and Maintenance of Household Equipment and Appliances",
      v401 == 8  ~ "Purchase and Maintenance of Kitchen and Bathroom Items",
      v401 == 9  ~ "Purchase and Maintenance of House and Kitchen-garden",
      v401 == 10 ~ "Expenses on Regular House Cleaning",
      v401 == 11 ~ "Purchase of Personal Vehicle",
      v401 == 12 ~ "Repair and Maintenance of Vehicle",
      v401 == 13 ~ "Public Transportation Expenses",
      v401 == 14 ~ "Communication Cost",
      v401 == 15 ~ "Audio-Visual, Photographic and Information Processing Equipment Expenses",
      v401 == 16 ~ "Music and Entertainment Related Goods",
      v401 == 17 ~ "Sports and Hobby Related Expenses",
      v401 == 18 ~ "Amusement and Cultural Services",
      v401 == 19 ~ "Books, Magazines, and Stationery",
      v401 == 20 ~ "Domestic Holiday Package",
      v401 == 21 ~ "Education Expenses",
      v401 == 22 ~ "Health Expenses (Preventive Health Care)",
      v401 == 23 ~ "Lodging and Hostel Costs",
      v401 == 24 ~ "Other Non-Electronic Personal Use Items",
      v401 == 25 ~ "Social Security Expenses",
      v401 == 26 ~ "Insurance Costs (life and non-life)",
      v401 == 27 ~ "Banking services",
      v401 == 28 ~ "Administrative and Legal Costs",
      v401 == 29 ~ "Festival and parties (wedding, birthday, etc.)",
      v401 == 30 ~ "Other Non-Food Consumption",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(yearly = v403a, monthly = v403b) %>%
  select(-v401) %>%
  group_by(nonfood_category) %>%
  mutate(
    across(
      c(yearly, monthly),
      list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    yearly  >= yearly_p1,  yearly  <= yearly_p99,
    monthly >= monthly_p1, monthly <= monthly_p99
  ) %>%
  summarise(
    across(
      c(yearly, monthly),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n_yearly  = sum(!is.na(yearly)),
    n_monthly = sum(!is.na(monthly)),
    .groups = "drop"
  )

travel_expense <- section4b %>%
  mutate(
    across(v407a:v407b, ~ na_if(.x, 0)),
    travel_expense_category = case_when(
      v405 == 1  ~ "Tour packages (Pre-paid tours, guided excursions, cruise packages)",
      v405 == 2  ~ "Food & Beverages (restaurants, cafes, groceries, street food, alcohol)",
      v405 == 3  ~ "Accommodation (hotels, hostels, vacation rentals, camping fees)",
      v405 == 4  ~ "Transportation (airfare, roads)",
      v405 == 5  ~ "Health-related Expenses (medicines, medical equipment, health services, and insurance co-pays)",
      v405 == 6  ~ "Leisure & entertainment activities (museums, movies, parks, and exhibitions, etc.)",
      v405 == 7  ~ "Shopping and goods (personal items, electronics, luxury goods, etc.)",
      v405 == 8  ~ "Travel essentials (Visas, travel insurance, SIM cards, roaming charges)",
      v405 == 9  ~ "Services & Fees (Laundry, communications, banking fees, currency conversion, tips)",
      v405 == 10 ~ "Other expenses (gifts, donations, unclassifiable spending)",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(yearly = v407a, monthly = v407b) %>%
  select(-v405) %>%
  group_by(travel_expense_category) %>%
  mutate(
    across(
      c(yearly, monthly),
      list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    yearly  >= yearly_p1,  yearly  <= yearly_p99,
    monthly >= monthly_p1, monthly <= monthly_p99
  ) %>%
  summarise(
    across(
      c(yearly, monthly),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n_yearly  = sum(!is.na(yearly)),
    n_monthly = sum(!is.na(monthly)),
    .groups = "drop"
  )

own_account <- section4d %>%
  mutate(
    v416a = as.numeric(v416a),
    v416b = as.numeric(v416b),
    across(v416a:v416b, ~ na_if(.x, 0)),
    product_type = case_when(
      v414 == 1 ~ "Bamboo & Cane Products",
      v414 == 2 ~ "Straw & Grass Products",
      v414 == 3 ~ "Textiles & Clothing",
      v414 == 4 ~ "Wooden Products & Furniture",
      v414 == 5 ~ "Metal Tools & Implements",
      v414 == 6 ~ "Processed Foods & Preserves",
      v414 == 7 ~ "Household Services & Maintenance",
      v414 == 8 ~ "Other Handicrafts & Items",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(yearly = v416a, monthly = v416b) %>%
  select(-v414) %>%
  group_by(product_type) %>%
  mutate(
    across(
      c(yearly, monthly),
      list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  filter(
    yearly  >= yearly_p1,  yearly  <= yearly_p99,
    monthly >= monthly_p1, monthly <= monthly_p99
  ) %>%
  summarise(
    across(
      c(yearly, monthly),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n_yearly  = sum(!is.na(yearly)),
    n_monthly = sum(!is.na(monthly)),
    .groups = "drop"
  )

wb <- createWorkbook()
addWorksheet(wb, "Food at Home");        writeData(wb, "Food at Home",        food_at_home_summary)
addWorksheet(wb, "Food Away Home");      writeData(wb, "Food Away Home",      food_away_home)
addWorksheet(wb, "Non-Food Expenditure"); writeData(wb, "Non-Food Expenditure", non_food_expenditure)
addWorksheet(wb, "Travel Expenses");     writeData(wb, "Travel Expenses",     travel_expense)
addWorksheet(wb, "Own Account Production"); writeData(wb, "Own Account Production", own_account)
saveWorkbook(wb, "household_expenditure_summary.xlsx", overwrite = TRUE)


# INCOME SUMMARIES

wage_income <- section8 %>%
  mutate(
    occupation = case_when(
      v803c == 1  ~ "Legislators, Officials & Managers",
      v803c == 2  ~ "Professionals",
      v803c == 3  ~ "Technicians and Associate Professionals",
      v803c == 4  ~ "Clerical Support Workers",
      v803c == 5  ~ "Services and Sales Workers",
      v803c == 6  ~ "Skilled Agricultural, Forestry and Fishery Workers",
      v803c == 7  ~ "Craft and Related Trades Workers",
      v803c == 8  ~ "Plant and Machine Operators and Assemblers",
      v803c == 9  ~ "Elementary Occupations",
      v803c == 10 ~ "Armed Forces Occupations",
      TRUE ~ NA_character_
    ),
    across(v808a:v808e, ~ na_if(.x, 0))
  ) %>%
  filter(!is.na(occupation)) %>%
  group_by(occupation) %>%
  mutate(across(
    c(v808a, v808b, v808c, v808d, v808e),
    list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  )) %>%
  filter(
    v808a >= v808a_p1, v808a <= v808a_p99,
    v808b >= v808b_p1, v808b <= v808b_p99,
    v808c >= v808c_p1, v808c <= v808c_p99,
    v808d >= v808d_p1, v808d <= v808d_p99,
    v808e >= v808e_p1, v808e <= v808e_p99
  ) %>%
  summarise(
    across(
      c(v808a, v808b, v808c, v808d, v808e),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
  )

crop_production_income <- section9c %>%
  mutate(
    crop = case_when(
      v914a == 1 ~ "Cereals",          v914a == 2 ~ "Pulses/Legumes",
      v914a == 3 ~ "Tuber & Bulb Crops", v914a == 4 ~ "Oilseed Crops",
      v914a == 5 ~ "Cash Crops",       v914a == 6 ~ "Spices",
      v914a == 7 ~ "Vegetables",       v914a == 8 ~ "Citrus Fruits",
      v914a == 9 ~ "Non Citrus Fruits",
      TRUE ~ NA_character_
    ),
    across(c(v918a:v918d), ~ na_if(.x, 0))
  ) %>%
  rename(total_sales = v918d) %>%
  group_by(crop) %>%
  mutate(
    p1  = quantile(total_sales, 0.01, na.rm = TRUE),
    p99 = quantile(total_sales, 0.99, na.rm = TRUE)
  ) %>%
  filter(total_sales >= p1, total_sales <= p99) %>%
  summarise(
    across(
      total_sales,
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = sum(!is.na(total_sales)),
    .groups = "drop"
  )

livestock_income <- section9e %>%
  filter(!is.na(v938b)) %>%
  mutate(
    livestock_type = case_when(
      v934a == 1  ~ "Bullocks/Cows",     v934a == 2  ~ "Buffaloes",
      v934a == 3  ~ "Goats/Mountain goats", v934a == 4  ~ "Sheep",
      v934a == 5  ~ "Yaks/Naks",         v934a == 6  ~ "Pigs/Boars",
      v934a == 7  ~ "Horses/Donkeys/Mules", v934a == 8  ~ "Poultry/Ducks/Pigeons",
      v934a == 9  ~ "Other livestock",   v934a == 10 ~ "Fish",
      TRUE ~ NA_character_
    ),
    across(v938b, ~ na_if(.x, 0))
  ) %>%
  rename(sell_income = v938b) %>%
  select(livestock_type, sell_income) %>%
  group_by(livestock_type) %>%
  mutate(
    p1  = quantile(sell_income, 0.01, na.rm = TRUE),
    p99 = quantile(sell_income, 0.99, na.rm = TRUE)
  ) %>%
  filter(sell_income >= p1, sell_income <= p99) %>%
  summarise(
    across(
      sell_income,
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = sum(!is.na(sell_income)),
    .groups = "drop"
  )

livestock_item_income <- section9f1 %>%
  filter(!is.na(v941), v941 > 0) %>%
  mutate(
    item = case_when(
      v940 == 1 ~ "Milk",   v940 == 2 ~ "Ghee",
      v940 == 3 ~ "Curd/Chhurpi", v940 == 4 ~ "Eggs",
      v940 == 5 ~ "Meat",   v940 == 6 ~ "Animal Hides",
      v940 == 7 ~ "Fish",   v940 == 8 ~ "Other income"
    ),
    across(v941, ~ na_if(.x, 0))
  ) %>%
  rename(sell_income = v941) %>%
  group_by(item) %>%
  mutate(
    p1  = quantile(sell_income, 0.01, na.rm = TRUE),
    p99 = quantile(sell_income, 0.99, na.rm = TRUE)
  ) %>%
  filter(sell_income >= p1, sell_income <= p99) %>%
  summarise(
    across(
      sell_income,
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = sum(!is.na(sell_income)),
    .groups = "drop"
  )

non_agri_income <- section10 %>%
  mutate(
    industry = as_factor(v1002b),
    raw_material_expenditure = v1009a + v1009b
  ) %>%
  rename(
    net_revenue           = v1011,
    gross_revenue         = v1005,
    wage_expenditure      = v1007,
    fuel_expenditure      = v1008
  ) %>%
  mutate(across(
    c(net_revenue, gross_revenue, wage_expenditure, fuel_expenditure, raw_material_expenditure),
    ~ na_if(.x, 0)
  )) %>%
  group_by(industry) %>%
  mutate(across(
    c(net_revenue, gross_revenue, wage_expenditure, fuel_expenditure, raw_material_expenditure),
    list(p1 = ~ quantile(.x, 0.01, na.rm = TRUE), p99 = ~ quantile(.x, 0.99, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  )) %>%
  filter(
    net_revenue              >= net_revenue_p1,              net_revenue              <= net_revenue_p99,
    gross_revenue            >= gross_revenue_p1,            gross_revenue            <= gross_revenue_p99,
    wage_expenditure         >= wage_expenditure_p1,         wage_expenditure         <= wage_expenditure_p99,
    fuel_expenditure         >= fuel_expenditure_p1,         fuel_expenditure         <= fuel_expenditure_p99,
    raw_material_expenditure >= raw_material_expenditure_p1, raw_material_expenditure <= raw_material_expenditure_p99
  ) %>%
  summarise(
    across(
      c(net_revenue, gross_revenue, wage_expenditure, fuel_expenditure, raw_material_expenditure),
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n_net_revenue              = sum(!is.na(net_revenue)),
    n_gross_revenue            = sum(!is.na(gross_revenue)),
    n_wage_expenditure         = sum(!is.na(wage_expenditure)),
    n_fuel_expenditure         = sum(!is.na(fuel_expenditure)),
    n_raw_material_expenditure = sum(!is.na(raw_material_expenditure)),
    .groups = "drop"
  )

cash_assistance <- section13a %>%
  filter(!is.na(v1305), v1305 > 0) %>%
  mutate(
    ssp = as_factor(v1301),
    across(v1305, ~ na_if(.x, 0))
  ) %>%
  rename(ssp_income = v1305) %>%
  group_by(ssp) %>%
  mutate(
    p1  = quantile(ssp_income, 0.01, na.rm = TRUE),
    p99 = quantile(ssp_income, 0.99, na.rm = TRUE)
  ) %>%
  filter(ssp_income >= p1, ssp_income <= p99) %>%
  summarise(
    across(
      ssp_income,
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = sum(!is.na(ssp_income)),
    .groups = "drop"
  )

other_income <- section13c %>%
  filter(!is.na(v1312), v1312 > 0) %>%
  mutate(
    income_item = as_factor(v1311a),
    across(v1311a, ~ na_if(.x, 0))
  ) %>%
  rename(other_income = v1312) %>%
  group_by(income_item) %>%
  mutate(
    p1  = quantile(other_income, 0.01, na.rm = TRUE),
    p99 = quantile(other_income, 0.99, na.rm = TRUE)
  ) %>%
  filter(other_income >= p1, other_income <= p99) %>%
  summarise(
    across(
      other_income,
      list(mean = ~ mean(.x, na.rm = TRUE), min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = sum(!is.na(other_income)),
    .groups = "drop"
  )

wb <- createWorkbook()
addWorksheet(wb, "Wage Income");           writeData(wb, "Wage Income",           wage_income)
addWorksheet(wb, "Livestock Income");      writeData(wb, "Livestock Income",      livestock_income)
addWorksheet(wb, "Livestock Item Income"); writeData(wb, "Livestock Item Income", livestock_item_income)
addWorksheet(wb, "Non Agricultural Income"); writeData(wb, "Non Agricultural Income", non_agri_income)
addWorksheet(wb, "Cash Assistance");       writeData(wb, "Cash Assistance",       cash_assistance)
addWorksheet(wb, "Other Income");          writeData(wb, "Other Income",          other_income)
saveWorkbook(wb, "income_summary.xlsx", overwrite = TRUE)


# WAGE INCOME FINAL SUMMARY

section8_wages <- section8 %>%
  mutate(
    v804 = as.numeric(v804),
    v804 = case_when(
      (is.na(v804) | v804 == 0) & v803c == 10 ~ 2,
      (is.na(v804) | v804 == 0) & v803c == 1  ~ 2,
      TRUE ~ v804
    ),
    total_wage = case_when(
      is.na(v808a)  ~ v805 * v806,
      !is.na(v808a) ~ rowSums(across(c(v808a, v808b, v808c, v808d, v808e)), na.rm = TRUE),
      v804 == 3     ~ rowSums(across(c(v810a, v810b)), na.rm = TRUE),
      TRUE          ~ NA_real_
    ),
    occupation_label = case_when(
      v803c == 1  ~ "Legislators, Officials & Managers",
      v803c == 2  ~ "Professionals",
      v803c == 3  ~ "Technicians and Associate Professionals",
      v803c == 4  ~ "Clerical Support Workers",
      v803c == 5  ~ "Services and Sales Workers",
      v803c == 6  ~ "Skilled Agricultural, Forestry and Fishery Workers",
      v803c == 7  ~ "Craft and Related Trades Workers",
      v803c == 8  ~ "Plant and Machine Operators and Assemblers",
      v803c == 9  ~ "Elementary Occupations",
      v803c == 10 ~ "Armed Forces Occupations",
      TRUE ~ NA_character_
    )
  )

section8_clean <- section8_wages %>%
  filter(!is.na(total_wage), total_wage > 0, !is.na(occupation_label))

print(paste("Original rows:", nrow(section8_wages)))
print(paste("Rows with valid wage:", nrow(section8_clean)))

final_summary <- section8_clean %>%
  group_by(occupation = occupation_label) %>%
  mutate(
    p1  = quantile(total_wage, 0.01, na.rm = TRUE),
    p99 = quantile(total_wage, 0.99, na.rm = TRUE)
  ) %>%
  filter(total_wage >= p1, total_wage <= p99) %>%
  summarise(
    mean_wage = mean(total_wage, na.rm = TRUE),
    min_wage  = min(total_wage,  na.rm = TRUE),
    max_wage  = max(total_wage,  na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )