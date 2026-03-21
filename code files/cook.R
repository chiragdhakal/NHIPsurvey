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
library(stringdist)
library(purrr)
library(labelled)
library(compareDF)

set.seed(123)

############################################ tbl01 (TABLE 01) #################################################

tbl01 <- read_dta("OOPS_Rawdata_2026_03_17/tbl01.dta")

normalize_name <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("/", " ") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("[^A-Z ]", " ") %>% 
    str_squish()
}

manual_fix <- function(x) {
  x %>%
    str_replace_all("\\bNOB[IA]*L\\b", "NOBEL") %>%
    str_replace_all("^(NOBEL )?(GAS UDHYOG|GAS INDUSTRY|GAS|GAS UDHOGH).*", "NOBEL GAS UDHYOG") %>%
    str_replace_all("\\bJANAK?R?I\\b", "JANAKI") %>%
    str_replace_all("JANAKI HEALTH.*", "JANAKI HEALTH CARE AND TEACHING HOSPITAL") %>%
    str_replace_all("K[HSY]+[AEY]+MADEVI.*", "KSHAMADEVI BUILDING MATERIALS") %>%
    
    str_replace_all(".*[VB]I[ZJ]U.*POLITE.*", "VIZU POULTRY FARM") %>%
    str_replace_all(".*[VB]I[ZJ]U.*POULTRY.*", "VIZU POULTRY FARM") %>%
    
    str_replace_all(".*GEFONT.*", "GENERAL FEDERATION OF NEPALESE TRADE UNIONS") %>%
    str_replace_all("GENERAL FEDERATION OF NEPALESE TRADE UNION(S)?", "GENERAL FEDERATION OF NEPALESE TRADE UNIONS") %>%
    
    str_replace_all("NA[Z]+A[R]+[E]+NE.*", "NAZARENE COMPASSIONATE MINISTRIES") %>%
    str_replace_all("UJHAN INTERNATION.*", "UJHAN INTERNATIONAL TRADERS") %>%
    
    str_replace_all("SAMA PRINT.*", "SAMA PRINTERS") %>%
    str_replace_all("GEMS SCHOOL", "GEMS HIGHER SECONDARY SCHOOL") %>% 
    
    str_replace_all("SAVING(S)? AND CREDIT COOPERATIVE(S)?( SOCIETY)?( LTD)?", "SACCOS_TOKEN") %>%
    str_replace_all("SAVING(S)? AND CREDIT CO OPERATIVE", "SACCOS_TOKEN") %>%
    
    str_replace_all("\\b(UDHOGH|UDYOG|UDJOY|UDDHOG|BEKARI)\\b", "UDHYOG") %>%
    str_replace_all("\\b(INDSUTRY|INDUSTRY)\\b", "UDHYOG")
}

restore_formal_names <- function(x) {
  x %>%
    str_replace_all("SACCOS_TOKEN", "SAVING AND CREDIT CO-OPERATIVE")
}

remove_legal_tokens <- function(x) {
  x %>%
    str_remove_all("\\b(PVT|LTD|PRIVATE|LIMITED|LTM|P LTD|PRALTD|PLC)\\b") %>%
    str_squish()
}

standardize_entries <- function(vec, threshold = 0.15) {
  if(all(is.na(vec)) || length(vec) == 0) return(vec)
  
  clean_names <- vec %>%
    normalize_name() %>%
    manual_fix()
  
  match_keys <- clean_names %>% remove_legal_tokens()
  
  uniq_keys <- unique(match_keys[match_keys != ""])
  if(length(uniq_keys) <= 1) return(clean_names %>% restore_formal_names())

  dmat <- stringdistmatrix(uniq_keys, uniq_keys, method = "jw", p = 0.1)
  hc <- hclust(as.dist(dmat), method = "average")
  groups <- cutree(hc, h = threshold)
  
  key_map <- tibble(match_key = uniq_keys, group = groups) %>%
    group_by(group) %>%
    mutate(canonical = match_key[which.max(nchar(match_key))]) %>%
    ungroup()

  results <- tibble(match_key = match_keys) %>%
    left_join(key_map, by = "match_key") %>%
    mutate(final = ifelse(is.na(canonical), match_key, canonical)) %>%
    mutate(final = restore_formal_names(final)) %>% 
    pull(final)
  
  return(results)
}

tbl01 <- tbl01 %>%
  mutate(
    employer_name_std = standardize_entries(employer_name),
    address_province = case_when(
      is.na(address_province) & address_district %in% c("SUNSARI", "MORANG", "UDAYAPUR") ~ 1,
      is.na(address_province) & address_district %in% c("SAPTARI", "RAUTAHAT", "SARLAHI", "MAHOTTARI", "DHANUSHA", "SIRAHA") ~ 2,
      is.na(address_province) & address_district %in% c("KAILALI") ~ 7, 
      TRUE ~ address_province
    )
  ) %>%
  group_by(employer_name_std) %>%
  mutate(
    ref_province = first(na.omit(address_province)),
    
    address_province = case_when(
      is.na(address_province) & address_district == "" & address_palika == "" ~ ref_province,
      TRUE ~ address_province  
    )
  ) %>%
  select(-ref_province) %>% 
  mutate(
    ref_palika = first(address_palika[address_palika != "" & !is.na(address_palika)]),

    address_palika = case_when(
      (address_palika == "" | is.na(address_palika)) ~ ref_palika,
      TRUE ~ address_palika
    )
  ) %>%
  select(-ref_palika, -employer_name) %>%
  ungroup() %>%
  rename(
    employer_name = employer_name_std
  ) %>%
  mutate(
    employer_name = case_when(
    employer_name %in% c("LOO NIVA", "LOONIVA NEPAL") ~ "LOO NIVA CHILD CONCERN GROUP", 
    employer_name == "JAYA" ~ "JAYA FURNISHERS PVTLTD",
    employer_name == "GEMS" ~ "GEMS HIGHER SECONDARY SCHOOL",
    employer_name == "BEVERAGE NEPAL" ~ "VARUN BEVERAGES NEPAL PVTLTD", 
    employer_name == "SPANDAN COOPERATIVES PVTLTD" ~ "SPANDAN SAVING AND CREDIT CO OPERATION", 
    employer_name == "LAB ASSISTANT" ~ "HYDRO LAB",
    employer_name == "KNITTING" ~ "PURNA ENTERPRISES KNITTING", 
    employer_name == "SUN BEAM ENGLISH SCHOOL SUNSARI" ~ "SUNBEAM ENGLISH SCHOOL", 
    employer_name %in% c("USAN PRADESHIK BAGWANI KEDRA CENTER", "BAGWANI KENDRA") ~ "USHNA PRADESHIYA BAGBANI KENDRA",
    employer_name %in% c("UJYALO BACHAT THATHA RIN SAHAKARI") ~ "UJYALO SAVING AND CREDIT CO-OPERATIVE",
    employer_name %in% c("D TECH", "D TECH TRADING CZOPTICAL FIBER CABLE AND OPTICAL FUSIONS SPLICOR") ~ "D TECH TRADING",
    employer_name %in% c("LAB TECHNICIAN") ~ "MULTI LAB",
    employer_name %in% c("WOOD CARVING") ~ "KRITI WOOD CARVING",
    employer_name %in% c("AFFINITY SAVING AND COOPERATIVE") ~ "AFFINITY SAVING AND CREDIT CO-OPERATIVE",
    TRUE ~ employer_name
  ), 
  address_province = case_when(
    is.na(address_province) & address_district %in% c("BARA", "PARSA", "RAUTAHAT") ~ 2, 
    employer_name == "SHINING NEPAL MULTIPURPOSE COMPANY" ~ 4,
    enrollment %in% c(1, 2) ~ NA_real_,
    TRUE ~ address_province
  ),
  address_district = case_when(
    address_district == "" & address_palika == "PAROHA" ~ "RAUTAHAT", 
    address_district == "" & address_palika == "TRIBENISUSTA" ~ "NAWALPARASI WEST",
    address_district == "" & address_palika == "KAWASOTI" ~ "NAWALPARASI EAST", 
    address_district == "" & address_palika == "DIPAYAL SILGADI" ~ "DOTI",
    employer_name == "SHINING NEPAL MULTIPURPOSE COMPANY" ~ "KASKI",
    enrollment %in% c(1, 2) ~ "",
    TRUE ~ address_district
  ),
  address_palika = case_when(
    employer_name == "SHINING NEPAL MULTIPURPOSE COMPANY" ~ "POKHARA",
    enrollment %in% c(1, 2) ~ "",
    TRUE ~ address_palika
  ),
  address_ward = case_when(
    is.na(address_ward) & employer_name == "JAYA FURNISHERS PVTLTD" ~ 8, 
    enrollment %in% c(1, 2) ~ NA_real_,
    TRUE ~ address_ward
  ),
  employer_sector = case_when(
    is.na(employer_sector) & employer_name == "BISHABAZAR COMPANY" ~ 7,
    is.na(employer_sector) & employer_name == "CASINO MAJHO" ~ 19,
    is.na(employer_sector) & employer_name == "SHINING NEPAL MULTIPURPOSE COMPANY" ~ 20,
    TRUE ~ employer_sector
  ),
  employer_size = case_when(
    is.na(employer_size) & employer_name == "SHINING NEPAL MULTIPURPOSE COMPANY" ~ 20,
    TRUE ~ employer_size
  ),
  respondent = case_when(
    id == 13837 ~ "BIKRAM BABU BASNET",
    id == 2397 ~ "SHYAM KUMAR RAI", 
    id == 2657 ~ "MANI RAJ TAMANG", 
    id == 12365 ~ "KESHAN ACHRAY BHATTRAI",
    id == 13528 ~ "PAWAN LAL RAJBANSHI", 
    id == 11108 ~ "SUSHIL KUMAR GACHHADAR", 
    id == 11112 ~ "KANHAIYA PATHAK", 
    id == 2983 ~ "INDRA BD KHATRI", 
    id == 2508 ~ "KATHIK RAJBANSHI", 
    id == 3058 ~ "BHUP RAJ BASNET", 
    id == 3838 ~ "AMRITA  KARKI", 
    id == 2501 ~ "MANISH KUMAR THAKUR", 
    id == 2502 ~ "JITENDRA KUMAR KUSHWAHA", 
    id == 12145 ~ "RAJIB KUMAR YADAV", 
    id == 12417 ~ "DIPA KUMARI SAH", 
    id == 12146 ~ "DEV KUMAR CHAUDHARY", 
    id == 13579 ~ "TULA PRASAD SUWEDI", 
    id == 7892 ~ "PURNARAM TULADHAR", 
    id == 9862 ~ "JANUKA TIMILSINA", 
    id == 8031 ~ "SWASTIKA SAPKOTA", 
    id == 10226 ~ "MATMARAM BASNET", 
    id == 14221 ~ "RAMLAL CHAUDHARY", 
    id == 14218 ~ "BINDA RAY", 
    id == 14140 ~ "SAROJ KUMAR SHRESTHA", 
    id == 4881 ~ "SHIV KUMAR THAPA", 
    id == 4870 ~ "KRISHNA BAHAHA GIRI", 
    id == 3600 ~ "KISAN TAKO", 
    id == 4763 ~ "RAJENDRA KUMAR KAYASTA", 
    id == 5218 ~ "RAMCHANDRA GAUTAM", 
    id == 10577 ~ "NIRMAL PRASAD SUBADI", 
    id == 4869 ~ "ISHWOR KARKI", 
    id == 5215 ~ "SUBASH JUNG RANA", 
    id == 11455 ~ "GYAN BDR THAPA", 
    id == 10354 ~ "KABINA SHANKAT", 
    id == 3603 ~ "सबिन थापा", 
    id == 3583 ~ "SUNIL THAPA", 
    id == 9857 ~ "SUSHILA GHARTI", 
    id == 4942 ~ "AASHOK THAPAMAGAR", 
    id == 11492 ~ "SUBADRA MALBUL", 
    id == 13771 ~ "SITA MAYA MOKTAN", 
    id == 12290 ~ "SABINA DAHAL GAJUREL", 
    id == 11361 ~ "SUDIP CHAUDARY", 
    id == 11370 ~ "YASHODA POKHAREL", 
    id == 11400 ~ "NISHA MANDAL", 
    id == 11404 ~ "NIRMALA ARYAL", 
    id == 11439 ~ "LILA DEVI SHRESTHA", 
    id == 11434 ~ "ANITA THAPA CHHETRI", 
    id == 11446 ~ "SABITA DHUNGANA", 
    id == 11547 ~ "DAN KUMAR SUBEDI", 
    id == 11997 ~ "LAXMI MAHARJAN", 
    id == 12298 ~ "RAMILA RAI", 
    id == 12297 ~ "MIN KUMARI KALA", 
    id == 12342 ~ "SHUSILA SHERESTHA", 
    id == 14608 ~ "SURAKSHYA YESMALI", 
    id == 12482 ~ "NEMA DORJE SANTANG", 
    id == 12477 ~ "SAMRIDDHA TULADAR",
    id == 13936 ~ "ANUPA DAHAL", 
    id == 11880 ~ "DELIP KUMAR CHAUDHARY", 
    id == 14355 ~ "SARITA THARU", 
    id == 12303 ~ "TIRTHA MAYA THAPA SIJALI", 
    id == 13752 ~ "NIRANJAN MISHRA", 
    id == 13750 ~ "SUKA RAM TAMANG", 
    id == 14141 ~ "SALMA SHRESTHA", 
    id == 12431 ~ "SANU MAYA LAMA KHADKA", 
    id == 14430 ~ "RAMMAYA MAHARJAN", 
    id == 13735 ~ "MAYA MAHARJAN", 
    id == 14553 ~ "SAMITA KARKI", 
    id == 11729 ~ "SUNITA KHATIWADA", 
    id == 11881 ~ "APSARA MAINALI TIMALSINA", 
    id == 11727 ~ "TULSI PRASAD SHIWAKOTI", 
    id == 11875 ~ "TARANIDHI PANTA", 
    id == 12339 ~ "MADHAV PARAJULI", 
    id == 13765 ~ "SARITA RAM", 
    id == 13801 ~ "ASHMA BISTA", 
    id == 13795 ~ "ANIL NATH SHRESTHA", 
    id == 8602 ~ "NANDA KUMARI SHARMA LAMICHHANE", 
    id == 7772 ~ "NARAYAN PRASAD SHARMA", 
    id == 8594 ~ "NAMUNA TIMALSINA", 
    id == 2659 ~ "GYAN BAHADUR DARAI", 
    id == 8216 ~ "ROSANI KAMAR", 
    id == 8220 ~ "KARUNA SHARMA", 
    id == 11005 ~ "SANJU KHANAL ADHIKARI", 
    id == 2964 ~ "HEM RAJ MAHATO", 
    id == 2955 ~ "DEVISARA MAGAR", 
    id == 7614 ~ "PRALAHAD BHATTA KAYESTHA", 
    id == 7935 ~ "DEBHAHADUR GHARTI", 
    id == 5712 ~ "UPENDRA BIR THAPA", 
    id == 5713 ~ "RAMA SIGDEL", 
    id == 5182 ~ "MAMATA KUNWAR", 
    id == 5735 ~ "MOHAN BEBASE", 
    id == 5746 ~ "TULASHA BHANDARI", 
    id == 13711 ~ "MAYA GAUTAM", 
    id == 13917 ~ "SURESH PARIYAR", 
    id == 13989 ~ "CHANDRA PARKASH CHAUDHARY",
    id == 11762 ~ "PHOOL MAYA B.K.", 
    id == 13986 ~ "YAM PRASAD DANGI", 
    id == 13916 ~ "ARUN KUMAR SAPKOTA", 
    id == 13981 ~ "GITA KUMARI SHINGH", 
    id == 13997 ~ "RITA GHIMIRE SAPKOA", 
    id == 13646 ~ "PARWATI RAMJALI MAGAR", 
    id == 13647 ~ "YADAV PRASAD NEUPANE", 
    id == 14513 ~ "PURNA GURUNG", 
    id == 3161 ~ "KUMAR MALLA", 
    id == 14471 ~ "DEEPA DEVI BHANDARI", 
    id == 12256 ~ "TULARA PANERU", 
    id == 2752 ~ "GAYATRI PANTA",
    id == 11110 ~ "UNISHA LAWATI",
    id == 2883 ~ "DHANAMAYA SUBEDI",
    id == 13540 ~ "BINISHA SANWA",
    id == 13521 ~ "KRISHNA HARI SHARMA DAHAL",
    id == 13539 ~ "SMIRTI GHIMIRE",
    id == 13525 ~ "GITA BISHWOKARMA ( DHIMAL)",
    id == 12489 ~ "SHARMILA MAGAR",
    id == 3457 ~ "LILAT URAHU",
    id == 12421 ~ "RUBI SINGH",
    id == 11645 ~ "DIPAK SAH",
    id == 12420 ~ "ROHIT YADAV",
    id == 12134 ~ "SHANKAR POKHREL",
    id == 12173 ~ "JIVAN BAITHA",
    id == 14194 ~ "RADHA MAHON PRASAD YADAV",
    id == 12174 ~ "SAMJHANA CHAUDHARY",
    id == 12162 ~ "SUNITA KUMARI MAHATO",
    id == 14288 ~ "SHSHIL DUNGANA",
    id == 9869 ~ "SABITA BUDHATHOKI",
    id == 13506 ~ "AMAR ADHIKARI",
    id == 10141 ~ "APSARA ACHARYA",
    id == 13563 ~ "SANNANI MAHARJAN",
    id == 14230 ~ "PRADIP CHHETRI",
    id == 13694 ~ "RAVI BK",
    id == 10061 ~ "LOMAS BATTARAI",
    id == 9834 ~ "YUBRAJ ADHIKARI",
    id == 3598 ~ "RITA  SHRESTHA",
    id == 14550 ~ "SALINA MAHARJAN",
    id == 8746 ~ "BISHNU PRASAD BHATTRAI",
    id == 14409 ~ "NARENDRA DANGOL",
    id == 12070 ~ "MUSTAFA AALAM",
    id == 13557 ~ "SARITA TAMANG SHRESTHA",
    id == 13508 ~ "SURESH MANI DIKSHIT",
    id == 13512 ~ "ASHIKA KHANEL",
    id == 9866 ~ "SANU SHRESTHA",
    id == 11687 ~ "ANISHA THAKUR",
    id == 11378 ~ "MANOJ ADHIKARI",
    id == 12381 ~ "ROSHNA POUDEL",
    id == 11391 ~ "SANGITA THAKURI",
    id == 11377 ~ "RAM BD. GHISING",
    id == 11882 ~ "ADITYA LOHANI",
    id == 13761 ~ "KABITA MAYA MOKTAN",
    id == 12340 ~ "HASINA RAWAT",
    id == 13856 ~ "BHUMIKA SARU MAGAR",
    id == 12306 ~ "LAXMI THATAAL",
    id == 12309 ~ "SRIJANA DHAKAL",
    id == 12291 ~ "NARAYAN KHATRI",
    id == 11591 ~ "LEKHANATH POKHERAL",
    id == 13662 ~ "JAN BAHADUR BISHWOKARMA",
    id == 13663 ~ "RENU  SHAH",
    id == 11546 ~ "RABIN POKHERAL",
    id == 11631 ~ "RITA BHUSAL GURUNG",
    id == 11600 ~ "URMILA PRADHAN",
    id == 12392 ~ "SANGITA BASNET KARKI",
    id == 11872 ~ "GRISMA CHAND THAKURI",
    id == 11977 ~ "RAM PRABESH SHAHA",
    id == 11934 ~ "BISHAL GIRI",
    id == 14446 ~ "RABINA KAFLE",
    id == 13587 ~ "SHREEJAN MANANDER",
    id == 13937 ~ "ANIL THAPA",
    id == 13939 ~ "ADHARSHA DHAKAL",
    id == 14217 ~ "JIT BAHADUR RAI",
    id == 14231 ~ "GYANU RATNA SHAKYA",
    id == 13944 ~ "RAM CHANDRA",
    id == 14499 ~ "GAJENDRA DEV",
    id == 14005 ~ "SERIMAYA ALE BHATTRAI",
    id == 12235 ~ "DEPEN BANTAWA",
    id == 12237 ~ "SARITA KARKI",
    id == 14213 ~ "YOSUF LAMA",
    id == 13617 ~ "DURGA LAXMI MAHARJAN",
    id == 14443 ~ "ANJU MAHARJAN",
    id == 14463 ~ "LAL KUMARI SUNUWAR",
    id == 14469 ~ "SHARMILA MOKTAM",
    id == 14468 ~ "MAN LAMA",
    id == 14466 ~ "GANGA MAGAR",
    id == 13763 ~ "SABINA DAWADI KARKI",
    id == 12101 ~ "FUL MAYA TAMANG",
    id == 13607 ~ "SHARMILA KHOKLI",
    id == 13803 ~ "ASHISH KC",
    id == 13738 ~ "BIKI YADAV",
    id == 10990 ~ "AKASH KHANAL",
    id == 8352 ~ "KRISHNA PRASAD BHATTARAI",
    id == 8372 ~ "SUMIT BABU RIZAL",
    id == 5717 ~ "NISHA KUMAL",
    id == 5848 ~ "SHIVA NEPAL",
    id == 13980 ~ "LILA SHARMA KARKI",
    id == 13984 ~ "MINA GAUTAM POKHREL",
    id == 13923 ~ "PUJA PAUDEL",
    id == 14509 ~ "PRABATI BHANDARI",
    id == 13721 ~ "SARAD BHANDARI",
    id == 3146 ~ "JAYA SING BADUWAL",
    id == 12467 ~ "DHANASHOWARI KUMARI PANTA AWASTHI",
    id == 3688 ~ "TEKARAJ JOSHI",
    id == 3966 ~ "BHIM.BDR AYER",
    id == 4703 ~ "RAJENDRA KUMAR KAYASTA", 
    id == 11433 ~ "SABINA SIWAKOTI",
    id == 2946 ~ "HEM NARAYAN MAHATO",
    id == 13622 ~ "NIRMAL KARKI",
    id == 11751 ~ "PIRTI SHARMA",
  
    TRUE ~ respondent
  )
)

section0_clean <- section0 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id)

tbl01_clean <- tbl01 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id)

all.equal(section0_clean, tbl01_clean)

rm(tbl01_clean, section0_clean)

############################################ SECTION 1.1 (TABLE 02) #############################################

tbl02 <- read_dta("OOPS_Rawdata_2026_03_17/tbl02.dta")

tbl02 <- tbl02 %>%
  select(-v106a) %>%
  mutate(
    v101 = case_when(
      personid == 5953297 ~ 1,
      personid == 5953298 ~ 2,
      personid == 5953299 ~ 3,
      personid == 5953300 ~ 4,
      TRUE ~ v101
    ),
    v103 = case_when(
      v102 == "PRASANSHA BISTA" ~ 2, 
      TRUE ~ v103
    ),
    v103 = if_else(v103 == 96, 3, v103),
    v104b = case_when(
      v104a < 5 & v104a > 0 ~ (v104a * 12),
      v104b == 0 ~ NA_real_, 
      TRUE ~ v104b
    ),
    v104b = case_when(
      personid == 5949012 ~ NA_real_, 
      TRUE ~ v104b
    ),
    v104b = if_else(
    v104a == 0 & is.na(v104b),
    6,
    v104b
    ),
    v105 = case_when(
      id == 11277 ~ 3, 
      personid == 1491 ~ 2,
      personid == 4161 ~ 2, 
      personid == 4254 ~ 2, 
      personid == 4253 ~ 2, 
      v106 == 4 ~ 2,
      TRUE ~ v105,
    ),
    v106 = case_when(
      v105 == 5 ~ 3, 
      TRUE ~ v106
    ),
    v107 = case_when(
      v107 %in% c(11, 16) ~ 9,   #DEWAR/DEWARANI AND NANDA KEPT IN NUMERIC CODE 9 (BROTHER/SISTER-IN-LAW)
      v107 %in% c(14, 15) ~ 6,   #DIDI/FUPU KEPT IN NUMERIC CODE 6 (BROTHER/SISTER)
      v107 %in% c(96) ~ 11,      #ALL THE OTHER CATEGORIZED WITH NO DESCRIPTION ARE KEPT IN NON-RELATIVE
      TRUE ~ v107
    ),
    v108 = case_when(
    is.na(v108) & v109 %in% c(3, 4) ~ 0,
    is.na(v108) & v109 == 1 ~ 12,
    v108 == 1 & v109 == 1 ~ 1,
    TRUE ~ v108
    ),
    v109 = case_when(
      v108 == 12 & v109 != 1 ~ 1, 
      TRUE ~ v109
    )
  ) %>%
  filter(
    !personid %in% c(11229)
  ) %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  ) 

tbl02 <- tbl02 %>%
  group_by(hhid, v102) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    v107 = case_when(
      v102 == "MITRA KUMARI DHAMALA" & id == 1602 ~ 2,
      v102 == "KAMALA DEVI SARU" & id == 9772 ~ 2,
      v102 == "RISHI KUMAR MAHATO" & id == 11730 ~ 3,
      v102 == "PARBATI TIMILSINA" & id == 12264 ~ 2,
      v102 == "GANGA DEI SHRESTHA" & id == 12267 ~ 2, 
      v102 == "SAURAV BHANDARI" & id == 12281 ~ 3,
      v102 == "AABHASH DHAMI" & id == 12425 ~ 3,
      v102 == "RADHA KC" & id == 14105 ~ 2, 
      v102 == "KARNA BDR BUDHA MAGAR" & id == 14433 ~ 2, 
      v102 == "MANISHA TAMANG" & id == 14583 ~ 3,
      v107 == 96 ~ 11,
      TRUE ~ v107
    )
  )

invalid_hhids <- tbl02 %>%
  filter(v107 == 1, v109 %in% c(3, 4)) %>%
  distinct(hhid)

new_heads <- tbl02 %>%
  semi_join(invalid_hhids, by = "hhid") %>%
  filter(v109 %in% c(1, 2)) %>%
  group_by(hhid) %>%
  slice_max(v104a, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hhid, uniq_id)

tbl02 <- tbl02 %>%
  left_join(
    new_heads %>% mutate(new_head = TRUE),
    by = c("hhid", "uniq_id")
  ) %>%
  mutate(
    v107 = case_when(
      new_head ~ 1,
      v107 == 1 & v109 %in% c(3, 4) ~ 2,
      TRUE ~ v107
    )
  ) %>%
  select(-new_head)

tbl02 <- tbl02 %>%
  mutate(
      v104a = case_when(
      personid == 51629 ~ 67, 
      personid == 5949620 ~ 45, 
      personid == 51645 ~ 44, 
      personid == 5951111 ~ 42, 
      personid == 27500 ~ 14,
      personid == 58596 ~ 52,
      personid == 1295 ~ 32,
      personid == 12304 ~ 42,
      personid == 24588 ~ 52,
      personid == 12863 ~ 44,
      personid == 24841 ~ 55,
      personid == 11019 ~ 45, 
      personid == 21343 ~ 52,
      personid == 21345 ~ 33,
      personid == 14457 ~ 70,
      personid == 53831 ~ 33, 
      personid == 53830 ~ 32,
      personid == 40247 ~ 50,
      personid == 16894 ~ 57,
      personid == 26885 ~ 55,
      personid == 26886 ~ 50, 
      personid == 26888 ~ 35, 
      personid == 26887 ~ 38,
      personid == 18227 ~ 50,
      personid == 18226 ~ 54,
      personid == 5951424 ~ 56,
      personid == 2607 ~ 8,
      personid == 32703 ~ 34,
      personid == 32705 ~ 26,
      personid == 33485 ~ 52,
      personid == 33486 ~ 53,
      personid == 32327 ~ 83,
      personid == 24602 ~ 54,
      personid == 32083 ~ 50,
      personid == 5951413 ~ 30,
      personid == 5951412 ~ 33,
      personid == 47159 ~ 30, 
      personid == 24992 ~ 45,
      personid == 24991 ~ 49,
      personid == 14788 ~ 82,
      personid == 14865 ~ 51,
      personid == 14864 ~ 53, 
      personid == 5948622 ~ 61, 
      personid == 5948623 ~ 60,
      personid == 59267 ~ 44,
      personid == 5951205 ~ 39,
      personid == 55192 ~ 39, 
      personid == 5948867 ~ 19,
      personid == 58052 ~ 62,
      personid == 5951474 ~ 50, 
      personid == 5951473 ~ 53, 
      personid == 59839 ~ 60,
      personid == 5949507 ~ 19, 
      personid == 1387 ~ 32,
      personid == 48358 ~ 43,
      personid == 26767 ~ 41,
      personid == 21751 ~ 40,
      personid == 21750 ~ 42,
      personid == 28468 ~ 21,
      personid == 21690 ~ 32, 
      personid == 58140 ~ 40,
      personid == 26639 ~ 66, 
      personid == 47067 ~ 65,
      personid == 11254 ~ 48,
      personid == 11253 ~ 52,
      personid == 40794 ~ 46,
      personid == 34512 ~ 39,
      personid == 51804 ~ 17, 
      personid == 8031 ~ 46, 
      personid == 49742 ~ 83,
      personid == 56117 ~ 54, 
      personid == 56791 ~ 49, 
      personid == 52045 ~ 39, 
      personid == 19987 ~ 45, 
      personid == 5950360 ~ 68,
      personid == 5949012 ~ 48,
      personid == 51752 ~ 42, 
      personid == 23764 ~ 45, 
      personid == 21908 ~ 62, 
      personid == 56395 ~ 72,
      personid == 56396 ~ 33, 
      personid == 56397 ~ 31, 
      personid == 5811 ~ 30, 
      personid == 5813 ~ 6,
      personid == 25874 ~ 40, 
      personid == 17134 ~ 62, 
      personid == 17133 ~ 65,
      personid == 28333 ~ 38, 
      personid == 28332 ~ 40,
      personid == 55319 ~ 27,
      personid == 46006 ~ 53,
      personid == 21335 ~ 25,
      personid == 25252 ~ 25,
      personid == 31196 ~ 25,
      personid == 38361 ~ 49,
      personid == 40917 ~ 25, 
      personid == 59011 ~ 21,
      personid == 5951821 ~ 48,
      TRUE ~ v104a
    ),
    v107 = case_when(
      personid == 20968 ~ 3,
      personid == 40492 ~ 2,
      personid == 40495 ~ 4,
      personid == 40494 ~ 3,
      personid == 40489 ~ 8, 
      personid == 40488 ~ 3, 
      personid == 40490 ~ 4,
      personid == 52940 ~ 2, 
      personid %in% c(52938, 52935, 52937, 52936) ~ 4, 
      personid == 52933 ~ 3, 
      personid == 52934 ~ 8,
      personid == 58638 ~ 4, 
      personid == 58637 ~ 4, 
      personid == 58636 ~ 8,
      personid == 58639 ~ 4, 
      personid == 58635 ~ 3,
      personid == 9172 ~ 3, 
      personid == 9167 ~ 3, 
      personid == 9160 ~ 5, 
      personid == 9169 ~ 6, 
      personid == 9163 ~ 2, 
      personid == 9168 ~ 3, 
      personid == 9161 ~ 5, 
      personid == 9170 ~ 9, 
      personid == 9171 ~ 3, 
      personid == 9166 ~ 9,
      personid == 9164 ~ 3,
      personid == 8872 ~ 5, 
      personid == 8874 ~ 6, 
      personid == 8875 ~ 6, 
      personid == 8871 ~ 5,
      personid == 17077 ~ 9, 
      personid == 17075 ~ 9,
      personid == 17078 ~ 3, 
      personid == 17072 ~ 10, 
      personid == 17073 ~ 2, 
      personid == 17071 ~ 10, 
      personid == 17076 ~ 9,
      personid == 60217 ~ 5, 
      personid == 60218 ~ 5, 
      personid == 60220 ~ 6,
      personid == 60225 ~ 10, 
      personid == 60226 ~ 10, 
      personid == 56306 ~ 6, 
      personid == 56307 ~ 6, 
      personid == 56304 ~ 5,
      personid == 56303 ~ 5,
      personid == 56314 ~ 5, 
      personid == 56313 ~ 5,
      personid == 5951420 ~ 5, 
      personid == 5951422 ~ 9, 
      personid == 5951419 ~ 5, 
      personid == 5951421 ~ 6,
      personid == 5951427 ~ 7, 
      personid == 5951428 ~ 3, 
      personid == 5951423 ~ 5, 
      personid == 5951425 ~ 6, 
      personid == 5951426 ~ 9, 
      personid == 5951424 ~ 5, 
      personid == 5951430 ~ 9, 
      personid == 5951429 ~ 6, 
      personid == 59090 ~ 7, 
      personid == 59089 ~ 7, 
      personid == 59082 ~ 6, 
      personid == 59080 ~ 5, 
      personid == 59085 ~ 7,
      personid == 59054 ~ 7, 
      personid == 59083 ~ 9, 
      personid == 59087 ~ 9,
      personid == 59086 ~ 6, 
      personid == 59081 ~ 5, 
      personid == 59088 ~ 7, 
      personid == 59092 ~ 2,
      personid == 58871 ~ 6, 
      personid == 58874 ~ 6, 
      personid == 58869 ~ 5, 
      personid == 58872 ~ 9, 
      personid == 58877 ~ 7, 
      personid == 58875 ~ 9, 
      personid == 58876 ~ 7,
      personid == 58870 ~ 5,
      personid == 59077 ~ 5, 
      personid == 59076 ~ 5, 
      personid == 59079 ~ 2,
      personid == 29324 ~ 4, 
      personid == 29323 ~ 4, 
      personid == 29321 ~ 3, 
      personid == 29322 ~ 8,
      personid == 5951525 ~ 5, 
      personid == 5951530 ~ 6, 
      personid == 5951526 ~ 5, 
      personid == 5951529 ~ 3, 
      personid == 5951528 ~ 2,
      personid == 5953065 ~ 6,
      personid == 5953063 ~ 5, 
      personid == 5953062 ~ 5,
      personid == 5953071 ~ 5, 
      personid == 5953072 ~ 5, 
      personid == 5953074 ~ 2,
      personid == 14977 ~ 5, 
      personid == 14978 ~ 6, 
      personid == 21766 ~ 5, 
      personid == 21769 ~ 6, 
      personid == 21768 ~ 12, 
      personid == 21767 ~ 5,
      personid == 21979 ~ 10, 
      personid == 21983 ~ 7, 
      personid == 21982 ~ 9, 
      personid == 21984 ~ 2,
      personid == 21986 ~ 3,
      personid == 21980 ~ 10, 
      personid == 21981 ~ 9, 
      personid == 21974 ~ 5, 
      personid == 21975 ~ 5, 
      personid == 21978 ~ 6, 
      personid == 21977 ~ 6, 
      personid == 5951452 ~ 10, 
      personid == 5951451 ~ 10, 
      personid == 5951416 ~ 5, 
      personid == 5951415 ~ 5,
      personid == 40730 ~ 5, 
      personid == 40731 ~ 6,
      personid == 5953130 ~ 5, 
      personid == 5953133 ~ 6, 
      personid == 5953131 ~ 5, 
      personid == 5953198 ~ 5, 
      personid == 5953199 ~ 6, 
      personid == 5953197 ~ 5, 
      personid == 5953200 ~ 9,
      personid == 5953207 ~ 5, 
      personid == 5953208 ~ 5, 
      personid == 5953078 ~ 9, 
      personid == 5953075 ~ 5, 
      personid == 5953077 ~ 6, 
      personid == 5953076 ~ 5,
      personid == 5952984 ~ 6, 
      personid == 5952983 ~ 5, 
      personid == 5952982 ~ 5, 
      personid == 5948913 ~ 6, 
      personid == 5948911 ~ 5, 
      personid == 54976 ~ 5,
      personid == 55098 ~ 5,
      personid == 55100 ~ 9, 
      personid == 55097 ~ 2, 
      personid == 55099 ~ 6,
      personid == 5953123 ~ 5, 
      personid == 5953124 ~ 5, 
      personid == 59887 ~ 5, 
      personid == 59888 ~ 5, 
      personid == 59889 ~ 6,
      personid == 55175 ~ 6, 
      personid == 55173 ~ 5, 
      personid == 55172 ~ 5,
      personid == 57128 ~ 5, 
      personid == 5952962 ~ 5, 
      personid == 5952963 ~ 5, 
      personid == 5952965 ~ 6, 
      personid == 5952897 ~ 5, 
      personid == 5952898 ~ 6, 
      personid == 5952899 ~ 6, 
      personid == 5952896 ~ 5, 
      personid == 5952960 ~ 5, 
      personid == 5952960 ~ 5, 
      personid == 5950226 ~ 5, 
      personid == 5952957 ~ 7, 
      personid == 5952954 ~ 9, 
      personid == 5952958 ~ 7, 
      personid == 5952956 ~ 2, 
      personid == 5952953 ~ 6,
      personid == 5952907 ~ 5, 
      personid == 5952908 ~ 5,
      personid == 5952901 ~ 5, 
      personid == 5952902 ~ 5, 
      personid == 5952779 ~ 5, 
      personid == 58594 ~ 6, 
      personid == 58597 ~ 6, 
      personid == 58595 ~ 5, 
      personid == 58596 ~ 5,
      personid == 5949237 ~ 5,
      personid == 5949241 ~ 6, 
      personid == 5949238 ~ 5, 
      personid == 5949240 ~ 6,
      personid == 12849 ~ 3, 
      personid == 12850 ~ 3,
      personid == 12848 ~ 6,
      personid == 12846 ~ 5,
      personid == 53123 ~ 6, 
      personid == 53127 ~ 2, 
      personid == 53124 ~ 6, 
      personid == 53122 ~ 9, 
      personid == 53126 ~ 3,
      personid == 53161 ~ 5,
      personid == 53160 ~ 5, 
      personid == 56799 ~ 4, 
      personid == 56795 ~ 3, 
      personid == 56797 ~ 2, 
      personid == 56798 ~ 8,
      personid == 56800 ~ 3,
      personid == 19212 ~ 4,
      personid == 19210 ~ 3, 
      personid == 19211 ~ 8, 
      personid == 19213 ~ 4,
      personid == 5950345 ~ 7, 
      personid == 5950343 ~ 6, 
      personid == 5950344 ~ 9,
      personid == 5950341 ~ 5, 
      personid == 5950346 ~ 6, 
      personid == 5950342 ~ 5,
      personid == 55691 ~ 3,
      personid == 55692 ~ 6,
      personid == 55690 ~ 9, 
      personid == 55693 ~ 7,
      personid == 55729 ~ 10,
      personid == 57815 ~ 6, 
      personid == 57814 ~ 5, 
      personid == 57818 ~ 6, 
      personid == 57817 ~ 6, 
      personid == 57816 ~ 9,
      personid == 9779 ~ 4, 
      personid == 9777 ~ 8,
      personid == 9778 ~ 4, 
      personid == 9776 ~ 3, 
      personid == 20084 ~ 4, 
      personid == 20081 ~ 3, 
      personid == 20087 ~ 3, 
      personid == 20082 ~ 8, 
      personid == 20083 ~ 4, 
      personid == 20085 ~ 4, 
      personid == 22300 ~ 2, 
      personid == 22304 ~ 4, 
      personid == 22303 ~ 8, 
      personid == 22302 ~ 3, 
      personid == 436 ~ 5, 
      personid == 440 ~ 6, 
      personid == 442 ~ 12,
      personid == 435 ~ 5, 
      personid == 439 ~ 6, 
      personid == 437 ~ 6, 
      personid == 441 ~ 6,
      personid == 5950317 ~ 12, 
      personid == 5950314 ~ 5,
      personid == 5950315 ~ 5,
      personid == 5950316 ~ 12,
      personid == 22873 ~ 6,
      personid == 45955 ~ 3,
      personid == 39409 ~ 5, 
      personid == 18418 ~ 3,
      personid == 7184 ~ 9, 
      personid == 7186 ~ 9,
      personid == 20887 ~ 4, 
      personid == 20886 ~ 4, 
      personid == 20884 ~ 8, 
      personid == 20885 ~ 1, 
      personid == 20888 ~ 4,
      personid == 47874 ~ 8, 
      personid == 47879 ~ 6, 
      personid == 8371 ~ 2, 
      personid == 8369 ~ 9,
      personid == 8372 ~ 3, 
      personid == 10817 ~ 9, 
      personid == 10818 ~ 9,
      personid == 58623 ~ 6,
      personid == 19895 ~ 3, 
      personid == 54635 ~ 3, 
      personid == 54601 ~ 3,
      personid == 9165 ~ 6,
      personid == 18229 ~ 9, 
      personid == 59082 ~ 6,
      personid == 59084 ~ 7,
      personid == 36463 ~ 9, 
      personid == 23622 ~ 9, 
      personid == 32704 ~ 6,
      personid == 32702 ~ 6, 
      personid == 32703 ~ 9,
      personid == 32702 ~ 6,
      personid == 55787 ~ 9, 
      personid == 54315 ~ 2,
      personid == 32302 ~ 9,
      personid == 17456 ~ 3, 
      personid == 54764 ~ 9,
      personid == 24823 ~ 9, 
      personid == 49873 ~ 2, 
      personid == 14802 ~ 2, 
      personid == 14803 ~ 9, 
      personid == 53659 ~ 9,
      personid == 25472 ~ 10,
      personid == 58555 ~ 6,
      personid == 59674 ~ 2, 
      personid == 55008 ~ 5, 
      personid == 55303 ~ 6, 
      personid == 59898 ~ 2, 
      personid == 57531 ~ 6, 
      personid == 5948827 ~ 7,
      personid == 55579 ~ 9, 
      personid == 60532 ~ 12,
      personid == 5951250 ~ 12, 
      personid == 46839 ~ 2,
      personid == 46840 ~ 3,
      personid == 13806 ~ 2, 
      personid == 11548 ~ 2, 
      personid == 19509 ~ 8, 
      personid == 19513 ~ 8, 
      personid == 34631 ~ 8, 
      personid == 34512 ~ 8,
      personid == 34651 ~ 2, 
      personid == 5950630 ~ 7, 
      personid == 5950628 ~ 6, 
      personid == 5951256 ~ 5, 
      personid == 51803 ~ 8,
      personid == 49732 ~ 9, 
      personid == 49731 ~ 9, 
      personid == 56792 ~ 6,
      personid == 60548 ~ 9, 
      personid == 5950587 ~ 6, 
      personid == 5951820 ~ 16,
      personid == 38659 ~ 9, 
      personid == 22299 ~ 9,
      personid == 56386 ~ 1, 
      personid == 56387 ~ 3, 
      personid == 57799 ~ 9, 
      personid == 13077 ~ 9,
      personid == 687 ~ 9, 
      personid == 15268 ~ 3, 
      personid == 20622 ~ 3,
      personid == 1493 ~ 9,
      personid == 9605 ~ 6,
      personid == 15534 ~ 3,
      personid == 16382 ~ 6,
      personid == 17144 ~ 6, 
      personid == 19381 ~ 6, 
      personid == 20402 ~ 3, 
      personid == 21335 ~ 3,
      personid == 23772 ~ 16,       #NON RELATIVE
      personid == 24652 ~ 3, 
      personid == 25252 ~ 3, 
      personid == 31196 ~ 8,
      personid == 33333 ~ 6, 
      personid == 35256 ~ 16, 
      personid == 7749 ~ 16, 
      personid == 38362 ~ 16,
      personid == 40917 ~ 8,
      personid == 52173 ~ 16, 
      personid == 53790 ~ 16, 
      personid == 55237 ~ 16, 
      personid == 55320 ~ 16, 
      personid == 55603 ~ 16, 
      personid == 55991 ~  16, 
      personid == 56219 ~ 6, 
      personid == 56501 ~  16, 
      personid == 57684 ~ 16, 
      personid == 58649 ~ 16, 
      personid == 58962 ~ 16, 
      personid == 59011 ~ 3,
      personid == 59331 ~ 16,
      personid == 59412 ~ 16,
      personid == 59818 ~ 16,
      personid == 60385 ~ 16, 
      personid == 5948478 ~ 5, 
      personid == 5951822 ~ 12, 
      personid == 5951821 ~ 5, 
      personid == 5951818 ~ 6, 
      personid == 5951820 ~ 5, 
      personid == 5951819 ~ 3, 
      personid == 5953222 ~ 16, 
      TRUE ~ v107
    ), 
    v110 = case_when(
      is.na(v110) & v104a < 18 & v104a >= 10 ~ 1, 
      v104a < 10 & !is.na(v110) ~ NA_real_,
      is.na(v110) & v104a >= 10 & v107 %in% c(8, 9) ~ 2,
      personid == 53878 ~ 2, 
      personid == 47892 ~ 3,
      personid == 8371 ~ 2, 
      personid == 33678 ~ 2, 
      personid == 33140 ~ 2,
      personid == 16864 ~ 1, 
      personid == 21809 ~ 5,
      personid == 58979 ~ 2, 
      personid == 22873 ~ 1, 
      personid == 9199 ~ 5,
      personid == 3400 ~ 1, 
      personid == 8097 ~ 5, 
      personid == 33437 ~ 1, 
      personid == 5953072 ~ 2, 
      personid == 5948618 ~ 2, 
      personid == 55012 ~ 1,
      personid == 5949635 ~ 1, 
      personid == 5952907 ~ 2, 
      personid == 5952502 ~ 2,
      personid == 11045 ~ 2, 
      personid == 32459 ~ 1, 
      personid == 32457 ~ 1, 
      personid == 26528 ~ 1, 
      personid == 11237 ~ 1, 
      personid == 1472 ~ 1, 
      personid == 54346 ~ 2,
      personid == 5085 ~ 2,
      personid == 5951107 ~ 2,
      personid == 12305 ~ 1,
      personid == 56385 ~ 1,
      personid == 25355 ~ 2,
      personid == 10324 ~ 2,
      personid == 5949012 ~ 2, 
      personid == 5813 ~ NA_real_,
      TRUE ~ v110
    )
  ) %>%
  filter(personid != 5952899) %>%
  select(-hhid, -uniq_id)
  
rm(invalid_hhids, new_heads)

tbl02 <- tbl02 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  ) %>%
  group_by(hhid) %>%
  mutate(
    age_head = if_else(any(v107 == 1), v104a[v107 == 1][1], NA_real_),
    age_diff_from_head = age_head - v104a,
    error = case_when(
      v107 == 3 & age_diff_from_head < 15 ~ "Error: Son/Daughter too old",
      v107 == 5 & age_diff_from_head > -15 ~ "Error: Father/Mother too young", 
      v107 == 4 & age_diff_from_head < 30 ~ "Error: Grandchild too old",
      v107 == 12 & age_diff_from_head > -30 ~ "Error: Grandparent too young", 
      v107 == 8 & age_diff_from_head < 12 ~ "Error: Son/Daughter-in-law too old",
      v107 == 10 & age_diff_from_head > -12 ~ "Error: Parent-in-law too young",
      v107 == 2 & age_diff_from_head > 20 ~ "Error: Husband/Wife too old",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

tbl02 <- tbl02 %>%
  mutate(
    v104a = case_when(
      error == "Error: Husband/Wife too old" ~ age_head - 4,
      TRUE ~ v104a
    )
  )

relationship_checks <- tbl02 %>%
  filter(!is.na(error)) %>%
  select(id, personid, hhid, v102, v104a, v104b, age_head, age_diff_from_head, error, v107)

rm(relationship_checks)

tbl02 <- tbl02 %>%
  select(-hhid, -age_head, -age_diff_from_head, -error) %>%
  mutate(
    v104a = case_when(
      !is.na(v104b) ~ round(v104b / 12),
      TRUE ~ v104a
    )
  )

hh_majority <- tbl02 %>%
  group_by(id, v105) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(id) %>%
  slice_max(n, with_ties = FALSE) %>%   
  select(id, majority_ethnicity = v105)

tbl02 <- tbl02 %>%
  group_by(id) %>%
  mutate(n_ethnicity = n_distinct(v105, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(hh_majority, by = "id") %>%
  mutate(
    v105 = case_when(
      n_ethnicity > 1 & v103 == 1 ~ majority_ethnicity,
      TRUE ~ v105
    )
  ) %>%
  select(-majority_ethnicity, -n_ethnicity)

rm(hh_majority)

section1a_clean <- section1a %>%
  select(-hhld, -uniq_id) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(personid)

tbl02_clean <- tbl02 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(personid)

all.equal(section1a_clean, tbl02_clean)

rm(section1a_clean, tbl02_clean)

############################################ SECTION 1.2 (TABLE 03) #############################################

tbl03 <- read_dta("OOPS_Rawdata_2026_03_17/tbl03.dta")

s1b <- read.xlsx("misc/rectify_sec1b_SA.xlsx")

s1b <- s1b %>%
  select(-chfid, -`_merge`, -sec1b_miss, -hhld_member,-v102, -v103, -v104a, -v104b, -v106, -v106a, -v105, -v107, -v108, -v109, -v110) %>%
  mutate(
    across(
      c(enrollment, province, district, palika, palika_type, v111, v114, v115, v116, v118, v120, interviewer),
      ~ as.numeric(str_extract(., "^[0-9]+"))
    ), 
    v112h_1 = as.character(v112h_1),
    employer_sector = as.numeric(employer_sector)
  )

tbl03 <- tbl03 %>%
  rows_upsert(s1b, by = "personid")

tbl03 <- tbl03 %>%
  mutate(
    v101 = case_when(
      personid == 5953297 ~ 1,
      personid == 5953298 ~ 2,
      personid == 5953299 ~ 3,
      TRUE ~ v101
    ),
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

non_ids <- setdiff(tbl03$personid, tbl02$personid)

tbl03 <- tbl03 %>%
  filter(!personid %in% non_ids)

all_objs <- ls()

for (obj in all_objs) {
  
  df <- get(obj)
  
  if (is.data.frame(df) && "personid" %in% colnames(df)) {
    
    df <- df %>%
      filter(!personid %in% non_ids)
    
    assign(obj, df, envir = .GlobalEnv)
  }
}

rm(s1b, df)

tbl02 <- tbl02 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

tbl03 <- merge(
  tbl03, 
  tbl02[, c("personid", "v104a")], 
  by = "personid"
)

tbl03 <- tbl03 %>%
  mutate(
    v111 = case_when(
      personid == 58220 ~ 1,
      enrollment == 2 & v111 == 1 & v112a == 1 ~ 2,
      enrollment == 2 & v112b == 2 ~ 2,
      personid %in% c(
        5948467, 5948463, 5952467, 5949595, 55029, 5952668, 5952569, 5952471, 47067, 35889, 36738, 5953273
      ) ~ 1,
      TRUE ~ v111 
    ),
    v112a = case_when(
      personid == 15427 ~ 1, 
      personid == 58220 ~ 1,
      personid == 60613 ~ NA_real_,
      personid == 49647 ~ NA_real_,
      personid == 5953217 ~ NA_real_,
      enrollment == 2 & v112a == 1 ~ NA_real_,
      TRUE ~ v112a
    ),
    v112b = case_when(
      enrollment == 4 & v112b == 2 ~ NA_real_,
      personid %in% c(
        60613, 5948467, 5948463, 49647, 5952467, 5949595, 55029, 59244, 5953217, 5952668, 5952569, 
        5952471, 47067, 35889, 36738, 5952807, 5953273
      ) ~ 2,
      TRUE ~ v112b
    ),
    across(
      c(v114, v115, v116),
      ~ if_else(v104a < 5, NA, .x)
    ), 
    across(
      v117, 
      ~ if_else(v104a < 10, NA, .x)
    )
  )

tbl03 <- tbl03 %>%
  mutate(
    v112c = case_when(
      v112h_1 %in% c(
        "SHREEMAN INDIAN ARMY BHAYAKOLE TEHI BATA SHREEMATIKO PANI SWASTHYA BIMA BHAYAKO", 
        "INDIAN ARMY BHAYAKOLE ARMY BATAI SWASTHYA BIMA BHAYAKO", 
        "BABA INDIAN ARMY BHAYAKALE TEHI BATA SABAI GHAR PARIWARKO SWASTHYA BIMA BHAYAKO", 
        "BABA INDIAN ARMY BHAYAKOLE TEHI BATA SABAI GHAR PARIWARKO SWASTHYA BIMA BHAYAKO", 
        "INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO", 
        "SHREEMAN INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO", 
        "BABA INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO", 
        "SCHOOL LE GARIDEYAKO",
        "SCHOOL LE GARIDEYAKO BIMA",
        "SCHOOL BATA GARAYAKO CHHA" 
      ) ~ 3,
      TRUE ~ v112c
    ),
    v112f = case_when(
      v112h_1 %in% c("JESTHA NAGARIK") ~ 6, 
      TRUE ~ v112f
    ),
    v112h = case_when(
      v112h_1 %in% c(
        "SHREEMAN INDIAN ARMY BHAYAKOLE TEHI BATA SHREEMATIKO PANI SWASTHYA BIMA BHAYAKO", 
        "INDIAN ARMY BHAYAKOLE ARMY BATAI SWASTHYA BIMA BHAYAKO", 
        "BABA INDIAN ARMY BHAYAKALE TEHI BATA SABAI GHAR PARIWARKO SWASTHYA BIMA BHAYAKO", 
        "BABA INDIAN ARMY BHAYAKOLE TEHI BATA SABAI GHAR PARIWARKO SWASTHYA BIMA BHAYAKO", 
        "INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO", 
        "SHREEMAN INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO", 
        "BABA INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO", 
        "SCHOOL LE GARIDEYAKO",
        "SCHOOL LE GARIDEYAKO BIMA",
        "SCHOOL BATA GARAYAKO CHHA",
        "JESTHA NAGARIK"
      ) ~ NA_real_,
      TRUE ~ v112h
    ),
    v111 = if_else(
      if_any(v112a:v112h, ~ !is.na(.)),
      1L,
      v111
    ),
    v111 = if_else(
      if_all(v112a:v112h, ~ is.na(.)),
      2L,
      v111
    ),
    v112a = case_when(
      personid %in% c(41244, 40660, 28873, 19047) ~ 1,
      TRUE ~ v112a
     ),
    v112b = case_when(
      personid %in% c(14043, 21784) ~ 2, 
      TRUE ~ v112b
    ), 
    v111 = case_when(
      personid %in% c(12121, 12120) ~ 2,
      TRUE ~ v111
     ),
    v111 = case_when(
      personid %in% c(41244, 40660, 28873, 19047, 14043, 21784) ~ 1,
      TRUE ~ v111
    ),
    v118 = if_else(!is.na(v119), 1L, v118),
    v120 = if_else(!is.na(v121), 1L, v120), 
  ) %>%
  select(-v112h, -v112h_1)

tbl03 <- tbl03 %>%
  mutate(
    edu_cap = case_when(   
      v104a < 5 ~ NA_real_,   
      v104a == 5 ~ 1,   
      v104a == 6 ~ 2,
      v104a == 7 ~ 3,
      v104a == 8 ~ 4,
      v104a == 9 ~ 5,
      v104a == 10 ~ 6,
      v104a == 11 ~ 7,
      v104a == 12 ~ 8,
      v104a == 13 ~ 9,
      v104a == 14 ~ 10,
      v104a == 15 ~ 12,  
      
      v104a <= 17 & v104a > 5 ~ 12,  
      v104a <= 19 & v104a > 5 ~ 13, 
      
      v104a <= 22 & v104a > 5 ~ 14, 
      v104a <= 24 & v104a > 5 ~ 15,  
      
      TRUE ~ 16         
    ),
    edu_implausible =
      v116 <= 16 &             
      !is.na(edu_cap) &
      v116 > edu_cap,

    v116_clean = case_when(
      edu_implausible ~ edu_cap,
      TRUE ~ v116
    ) 
  ) %>%
  select(-v116) %>%
  rename(
    v116 = v116_clean
  )

tbl03 <- tbl03 %>%
  mutate(
    v114 = case_when(
      personid == 27817 & is.na(v114) ~ 1, 
      v104a >= 5 & is.na(v115) & is.na(v116) ~ 3,
      v104a >= 5 & !is.na(v115) & !is.na(v116) ~ 1,
      personid == 15498 ~ 1, 
      personid == 15499 ~ 1, 
      TRUE ~ v114
    ), 
    v115 = case_when(
      personid == 27817 & is.na(v115) ~ 3, 
      v104a >= 5 & is.na(v115) & v114 == 3 ~ 1,
      personid == 15498 ~ 2, 
      personid == 15499 ~ 2, 
      v104a > 25 & v116 < 13 ~ 2,
      v104a > 20 & v104a <= 25 & v116 < 10 ~ 2,
      TRUE ~ v115
    ),
    v116 = case_when(
      personid == 27817 & is.na(v115) ~ 1, 
      personid == 15498 ~ 11, 
      personid == 15499 ~ 12,
      personid == 53995 ~ 13, 
      personid == 53996 ~ 13, 
      personid == 53997 ~ 13, 
      personid == 53998 ~ 13, 
      TRUE ~ v116
    )
  ) %>%
  mutate(
    v115 = case_when(
      v104a >= 25 & !is.na(v114) & is.na(v115) & !is.na(v116) ~ 2, 
      v104a < 20 & !is.na(v114) & is.na(v115) & !is.na(v116) ~ 2, 
      personid == 5065 ~ 2,
      personid == 5951774 ~ 2, 
      personid == 17614 ~ 3,
      TRUE ~ v115
    )
  ) %>%
  group_by(v104a) %>%
  mutate(
    v116 = case_when(
      v114 == 1 & v115 == 3 & is.na(v116) ~ round(mean(v116, na.rm = TRUE)), 
      v114 == 1 & v115 == 2 & is.na(v116) ~ round(mean(v116, na.rm = TRUE)),
      personid == 54155 ~ 8,
      personid == 54156 ~ 6, 
      personid == 54157 ~ 4,
      personid == 19179 ~ 13, 
      personid == 19180 ~ 11,
      personid == 19181 ~ 9, 
      personid == 14659 ~ 10, 
      personid == 14660 ~ 9,
      personid == 14713 ~ 8, 
      personid == 14714 ~ 2,
      personid == 13179 ~ 9, 
      personid == 13179 ~ 12, 
      personid == 13914 ~ 1, 
      personid == 54242 ~ 9,
      personid == 54243 ~ 8, 
      personid == 54244 ~ 12,
      personid == 54245 ~ 10, 
      personid == 54246 ~ 5,
      personid == 54247 ~ 2,
      personid == 54248 ~ 13,
      personid == 54249 ~ 13, 
      personid == 54250 ~ 1, 
      personid == 54252 ~ 12, 
      personid == 54253 ~ 13, 
      personid == 54244 ~ 1,
      TRUE ~ v116
    ),
    v115 = case_when(
      v114 == 3 & v115 == 2 & is.na(v116) ~ 1, 
      personid == 19703 ~ 1,
      TRUE ~ v115
    )
  ) %>%
  ungroup() %>%
  select(-uniq_id, -v104a, -edu_cap, -edu_implausible, -hhid) %>%
  select(enrollment:uid, personid, v101:v115, v116, everything())

section1b_clean <- section1b %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(personid) %>%
  select(-hhld, -v104a)

tbl03_clean <- tbl03 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(personid) %>%
  select(-hhld, -chfid:-target_group_imis)

tbl03_clean <- tbl03_clean[, names(section1b_clean)]

all.equal(section1b_clean, tbl03_clean)

rm(section1b_clean, tbl03_clean)

############################################ SECTION 2.1.1 (TABLE 04) #############################################

tbl04 <- read_dta("OOPS_Rawdata_2026_03_17/tbl04.dta")

tbl04 <- tbl04 %>%
  group_by(psu) %>%
  mutate(
    mean_v202_psu = round(mean(v202[v202 <= 15 & v202 != 0], na.rm = TRUE)),
    v202 = ifelse(is.na(v202) | v202 == 0, mean_v202_psu, v202)
  ) %>%
  select(-mean_v202_psu) %>%  
  ungroup()

tbl04 <- tbl04 %>%
  mutate(
    v201 = if_else(
      is.na(v201), 
      1, 
      v201
    ), 
    v203 = if_else(
      is.na(v203), 
      1, 
      v203
    ), 
    v203 = case_when(
      v203a %in% c(
        "BASLE BANEKO", "BASA LE BANEKO", " BAMBOO", "MATO BASA LE BANEKO", "MATO BAS", 
        "BASA RA MATO KO", "BASA RA MATO", "MATO RA BASA LE BANEKO", "MATO BASALE", 
        "BASA LE BANEKO", "MATO BASA", "BASA RA MATO LE BANEKO", "MATO MA TRUST BANAUNA MATERIAL GADEKO",
        " BAMBOO", "BASA KO MATO", "MATO RA BASH KO KACHI GHAR"
      ) ~ 4, 
      v203a %in% c(
        "BLOCK", "FALAM CEMENT", "FALAM RA CEMENT", "IRON"
      ) ~ 3, 
      v203a %in% c(
        "JASTA KO", "JASTAPATA", "JASTA", "JASTA"
      ) ~ 5,
      TRUE ~ v203
    ),
    v204 = case_when(
      v203 %in% c(2, 3) ~ 2, 
      v203 == 4 ~ 3, 
      v204a %in% c(
        " JASTA KO"
      ) ~ 6,
      v204a %in% c(
        "KHAR", " KHAR MATO LIPEKO", " KHAR", "BASA RA MATO LE BANEKO",
        "KHAR RA MATO", "BASA RA MATO KO BANEKO", "MATO RA KATH", "KHADIYA RA MATO", "KHARIYA", 
        "KHARIYO RA MATO"
      ) ~ 5,
      v204a %in% c(
        "BLOCK", "CEMENT KO BLOCK", " BASA RA CEMENT LE BANE KO"
      ) ~ 2,
      v204a %in% c(
        " CHAINA"
      ) ~ 1,
      TRUE ~ v204
    ),
    v205 = case_when(
      v206 == 2 ~ 2, 
      v206 == 4 ~ 4, 
      v206 == 1 ~ 5,
       v205a %in% c(
        "MATO RA KATHA", "MAATO AND KAATHA", "MAATO KAATHA", "MATO", " MATO KO KHAPADA", 
        "MATO KATHA", "MATO RA KATHA", "MATO DHUNGA", "MATO KO KHAPADA", "KHAPADA", 
        "KHAPATA", "KHPADA", "KHPADA KO XANO", "MAATO", "MATOKO TILE PLUS JASTA"
       ) ~ 5,
       v205a %in% c(
        "MATO KO KHAPADA RA TAYAL"
       ) ~ 3,
       v205a %in% c(
        "ALBESTER", " ALBESTER"
       ) ~ 1,
       v205a %in% c(
        "SIMETKO TALI"
       ) ~ 2, 
       is.na(v205) ~ 2,
      TRUE ~ v205
    ),
    v206 = case_when(
      v205 %in% c(1, 2) ~ 2, 
      v205 %in% c(3, 4) ~ 3, 
      v205 %in% c(
        " MAATO AND KAATHA"
      ) ~ 1, 
      v205 %in% c(
        "ITTA", " BRICKS", " DHUNGA" 
      ) ~ 3,
      TRUE ~ v206
    ),
    v207 = trimws(v207),
    v207 = case_when(
      v207 %in% c("0", "100000", "300000", "350000", "8000", "DON'T KNOW", "O", 
                "PURKAULI", "PURKHAULI", "PURKHAULI DEKHI BASDAI AAYEKO") ~ "0",
    v207 == "1950" ~ "1950",
    v207 == "1972" ~ "1972", 
    v207 == "1980" ~ "1980",
    v207 == "1981" ~ "1981",
    v207 %in% c("2/27/1990", "1990") ~ "1990",
    v207 %in% c("90", "90 YEARS", "1992") ~ "1992",
    v207 == "1995" ~ "1995",
    v207 == "83" ~ "1999",
    v207 == "2000" ~ "2000",
    v207 %in% c("80", "80 YEARS") ~ "2002",
    v207 == "78" ~ "2004",
    v207 %in% c("2007", "75") ~ "2007",
    v207 %in% c("2008", "74") ~ "2008",
    v207 == "73" ~ "2009",
    v207 %in% c("2010", "72") ~ "2010",
    v207 == "2011" ~ "2011",
    v207 %in% c("2012", "70") ~ "2012",
    v207 == "2014" ~ "2014",
    v207 == "67" ~ "2015",
    v207 == "2016" ~ "2016",
    v207 %in% c("2018", "64") ~ "2018",
    v207 %in% c("2020", "12/5/2020", "9/6/2020") ~ "2020",
    v207 == "2021" ~ "2021",
    v207 == "60" | v207 == "60 YEARS" ~ "2022",
    v207 == "2023" ~ "2023",
    v207 == "2024" ~ "2024",
    v207 == "2025" ~ "2025",
    v207 == "2026" ~ "2026",
    v207 %in% c("2027", "55", "55 YEARS") ~ "2027",
    v207 %in% c("2028", "9/5/2028", "54 YEARS") ~ "2028",
    v207 == "2029" ~ "2029",
    v207 %in% c("2030", "52") ~ "2030",
    v207 == "2031" ~ "2031",
    v207 %in% c("2032", "9/12/2032", "10/15/2032", "50", "50(2032)") ~ "2032",
    v207 %in% c("2033", "6/5/2033", "49") ~ "2033",
    v207 %in% c("2034", "11/10/2034") ~ "2034",
    v207 %in% c("2035", "9/18/2035", "47") ~ "2035",
    v207 %in% c("2036", "9/2/2036", "46") ~ "2036",
    v207 %in% c("2037", "45", "45 YEARS") ~ "2037",
    v207 %in% c("2038", "4/8/2038") ~ "2038",
    v207 %in% c("2039", "43") ~ "2039",
    v207 %in% c("2040", "2040 SAL TIRA BANEKO", "5/13/2040", "6/6/2040", "7/6/2040", "42") ~ "2040",
    v207 %in% c("2041", "41") ~ "2041",
    v207 %in% c("2042", "5/26/2042", "40") ~ "2042",
    v207 %in% c("2044", "6/13/2044", "38") ~ "2044",
    v207 %in% c("2045", "11/20/2045", "37") ~ "2045",
    v207 %in% c("2046", "3/1/2046", "36") ~ "2046",
    v207 %in% c("2047", "1/10/2047", "35", "35(2047)") ~ "2047",
    v207 %in% c("2048", "11/21/2048", "34(2048)") ~ "2048",
    v207 %in% c("2049", "2049/10", "33", "33YRS") ~ "2049",
    v207 %in% c("2050", "6/25/2050", "1/10/2050", "8/10/2050", "32") ~ "2050",
    v207 %in% c("2051", "11/7/2051", "2051/2", "2/20/2051", "5/8/2051") ~ "2051",
    v207 %in% c("2052", "5/12/2052", "1/10/2052", "10/18/2052", "10/9/2052", 
                "5/10/2052", "5/2/2052", "6/26/2052", "8/13/2052", "8/6/2052", 
                "9/3/2052", "20520611", "30") ~ "2052",
    v207 %in% c("2053", "1/2/2053", "2/1/2053", "6/1/2053", "29", "5053") ~ "2053",
    v207 %in% c("2054", "10/3/2054", "5/25/2054", "28", "28(2054)", "5054") ~ "2054",
    v207 %in% c("2055", "5/4/2055", "7/5/2055", "27") ~ "2055",
    v207 %in% c("2056", "11/10/2056", "10/5/2056", "11/5/2056", "26", "26 YRS") ~ "2056",
    v207 %in% c("2057", "2057/01", "10/10/2057", "11/1/2057", "2057/12", 
                "3/22/2057", "8/16/2057", "25", "25(2057)", "25YRS") ~ "2057",
    v207 %in% c("2058", "2058 BS") ~ "2058",
    v207 %in% c("2059", "1/1/2059", "12/14/2059", "8/30/2059", "23") ~ "2059",
    v207 %in% c("2060", "9/18/2060", "5/14/2060", "20600512", "22", "22YRS") ~ "2060",
    v207 %in% c("2061", "1/18/2061", "10/15/2061", "11/22/2061", "5/1/2061", 
                "5/18/2061", "7/7/2061", "8/8/2061", "21") ~ "2061",
    v207 %in% c("2062", "1/1/2062", "1/5/2062", "12/20/2062", "2/10/2062", 
                "4/4/2062", "6/7/2062", "7/10/2062", "20", "20 62") ~ "2062",
    v207 %in% c("2063", "2/8/2063", "7/6/2063", "11/10/2063", "19", "19(2063)") ~ "2063",
    v207 %in% c("2064", "18", "18 (2064)", "18 YRS", "18(2064)") ~ "2064",
    v207 %in% c("2065", "8/1/2065", "1/5/2065", "2/12/2065", "2/4/2065", 
                "3/12/2065", "4/12/2065", "2065/5", "6/3/2065", "7/17/2065", 
                "7/29/2065", "17") ~ "2065",
    v207 %in% c("2066", "02066/2/5", "4/15/2066", "11/28/2066", "3/7/2066", "16") ~ "2066",
    v207 %in% c("2067", "10/10/2067", "11/25/2067", "5/7/2067", "15", 
                "15 BARSHA AGADI BANEKO", "15 YRS", "15YRS", "6067") ~ "2067",
    v207 %in% c("2068", "2068)", "1/4/2068", "1/7/2068", "12/3/2068", "3/3/2068", "14", "14 YRS", "2868") ~ "2068",
    v207 %in% c("2069", "11/8/2069", "9/4/2069", "20690515", "13", "13 YRS", "13(2069)") ~ "2069",
    v207 %in% c("2070", "2070/04", "1/5/2070", "1/2/2070", "2/1/2070", "3/1/2070", 
                "8/5/2070", "9/15/2070", "20700506", "207011015", "12", "12YEARS", 
                "12YRS", "270", "5070") ~ "2070",
    v207 %in% c("2071", "11/30/2071", "1/12/2071", "2/20/2071", "2/3/2071", 
                "5/1/2071", "11", "11(2071)", "11YRS") ~ "2071",
    v207 %in% c("2072", "2072/03", "3/6/2072", "1/1/2072", "1/11/2072", 
                "10/26/2072", "11/26/2072", "5/4/2072", "5/6/2072", "2072/7", 
                "8/11/2072", "9/14/2072", "10", "10 YRS", "10(2072)", "10YRS") ~ "2072",
    v207 %in% c("2073", "2073-1", "1/1/2073", "1/12/2073", "1/13/2073", "1/5/2073", 
                "10/5/2073", "2073-11", "11/30/2073", "2073-2-30", "5/10/2073", 
                "8/1/2073", "2073/01", "2073/03", "2073/06", "2073/1", "1/3/2073", 
                "1/4/2073", "2073/10", "11/1/2073", "11/5/2073", "11/9/2073", 
                "2073/12", "12/11/2073", "8/15/2073", "9", "9 YRS", "9(2073)", "5073") ~ "2073",
    v207 %in% c("2074", "2074-10", "2074-11", "11/2/2074", "2074-12", "12/15/2074", 
                "2074-6", "7/9/2074", "2074-8", "8/1/2074", "2074/05", "1/2/2074", 
                "1/25/2074", "1/5/2074", "2074/10", "10/21/2074", "11/1/2074", 
                "3/30/2074", "2074/4", "2074/8", "8", "8(2074)", "8YRS") ~ "2074",
    v207 %in% c("2075", "1/1/2075", "11/10/2075", "11/25/2075", "12/10/2075", 
                "3/5/2075", "2075/", "3/22/2075", "6/29/2075", "1/5/2075", 
                "2075/10", "10/1/2075", "10/8/2075", "11/16/2075", "3/3/2075", 
                "6/1/2075", "2075/8", "8/15/2075", "20750217", "7", "7(2075)", 
                "7YEARS", "7YRS") ~ "2075",
    v207 %in% c("2076", "1/7/2076", "2076-2-30", "5/30/2076", "2076-6", "2076/04", 
                "1/29/2076", "2076/10", "10/10/2076", "11/18/2076", "2/18/2076", 
                "3/5/2076", "2076/5", "9/15/2076", "6", "6 YEARS", "6 YRS") ~ "2076",
    v207 %in% c("2077", "5/10/2077", "1/1/2077", "1/8/2077", "11/3/2077", "3/7/2077", 
                "4/25/2077", "6/1/2077", "8/16/2077", "9/12/2077", "20770315", 
                "5", "5 YEARS", "5 YRS", "5YRS") ~ "2077",
    v207 %in% c("2078", "2/25/2078", "9/10/2078", "2078/08", "1/15/2078", 
                "11/6/2078", "4", "4(2078)", "4YRS") ~ "2078",
    v207 %in% c("2079", "2.5", "3", "9/2/2079", "10/1/2079", "11/17/2079", 
                "2079/5", "6/10/2079", "2079/8", "8/10/2079", "20790520") ~ "2079",
    v207 %in% c("2080", "1.5 YEARS", "2", "30MONTH", "2080-4", "2080/06", 
                "1/1/2080", "1/10/2080", "10/5/2080", "11/11/2080", "12/5/2080", 
                "8/5/2080", "20800830") ~ "2080",
    v207 %in% c("2081/4/8", "1", "1(2081)", "2081", "1/1/2081", "1/28/2081", 
                "11/16/2081", "2/1/2081", "5/11/2081", "5/2/2081", "8/18/2081", "6 MONTH") ~ "2081",
    v207 %in% c("2082", "3/31/2082", "1/3/2082", "1/5/2082", "3/15/2082", "6/9/2082") ~ "2082",
    v207 == "2088" ~ "2078",
    v207 == "2089" ~ "2079",
    v207 == "90YRS" ~ "1992", 
    TRUE ~ v207
    ),
    v207 = if_else(
      v207 == "0",
      0L,
      as.integer(stringr::str_extract(v207, "\\b(19|20)\\d{2}\\b"))
    ),
    v207 = if_else(
      is.na(v207), 
      0,
      v207
    )  
  )

section2a1_clean <- section2a1 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id) %>%
  select(-hhld)

tbl04_clean <- tbl04 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id) %>%
  select(-hhld)

tbl04_clean <- tbl04_clean[, names(section2a1_clean)]

all.equal(section2a1_clean, tbl04_clean)

rm(section2a1_clean, tbl04_clean)

############################################ SECTION 2.1.2 (TABLE 05) #############################################

tbl05 <- read_dta("OOPS_Rawdata_2026_03_17/tbl05.dta")

tbl05 <- tbl05 %>%
  mutate(
    v214  = ifelse(v211 == 1 & !is.na(v212), NA_real_, v214),
    v215  = ifelse(v211 == 1 & !is.na(v212), NA_real_, v215),
    v213 = if_else(v213 > 3, 2, v213),
    v214 = if_else(v214 > 100000, v214 / 100, v214),
    v215 = if_else(v215 > 100000, v215 / 100, v215),
  )

tbl05 <- tbl05 %>%
  mutate(
    v213 = if_else(!is.na(v211) & !is.na(v212), NA_real_, v213),
    v214 = if_else(!is.na(v211) & !is.na(v212), NA_real_, v214),
    v215 = if_else(!is.na(v211) & !is.na(v212), NA_real_, v215),
    v213 = if_else(v208 == 1 & v211 == 2, NA_real_, v213), 
    v209 = if_else(v208 == 2, NA_real_, v209),
    v210 = if_else(v208 == 2, NA_real_, v210), 
    v211 = if_else(v208 == 2, NA_real_, v211), 
    v212 = if_else(v208 == 2, NA_real_, v212)
  )

tbl05 <- tbl05 %>%
  mutate(
    v213 = case_when(
      v208 == 2 & is.na(v213) ~ 1, 
      TRUE ~ v213
    )
  ) %>%
  group_by(palika) %>%
  mutate(
    v214 = case_when(
      v208 == 2 & v213 == 1 & is.na(v214) ~ round(mean(v214, na.rm = TRUE), -2), 
      TRUE ~ v214 
    ), 
    v215 = case_when(
      v208 == 2 & v213 == 1 & is.na(v215) ~ round(mean(v215, na.rm = TRUE), -2), 
      TRUE ~ v215 
    ),
    v214 = if_else(
      v215 > v214, 
      v215 + 1000, 
      v214
    ), 
    v215 = if_else(
      v213 %in% c(2, 3), 
      0, 
      v215
    ),
    v214 = if_else(
      !is.na(v213) & is.na(v214), 
      0, 
      v214
    ), 
    v213 = if_else(
      is.na(v208), 
      1, 
      v213
    ),
    v214 = if_else(
      is.na(v208), 
      round(mean(v214)), 
      v214
    ), 
    v215 = if_else(
      is.na(v208), 
      round(mean(v215)),
      v215
    ),
    v208 = case_when(
      is.na(v208) ~ 2, 
      is.na(v209) & is.na(v210) & is.na(v211) & is.na(v212) ~ 2,
      TRUE ~ v208
    )
  ) %>%
  ungroup()

tbl05 <- tbl05 %>%
  group_by(province) %>%
  mutate(
    v213 = case_when(
      id == 2244 ~ 1, 
      id == 3757 ~ 1, 
      id %in% c(11126, 11197, 11708, 12231) ~ 2,
      TRUE ~ v213
    ), 
    v214 = case_when(
      v213 == 1 & is.na(v214) ~ round(mean(v214, na.rm = TRUE), -2),
      TRUE ~ v214
    ), 
    v215 = case_when(
      v213 == 1 & is.na(v215) ~ round(mean(v215, na.rm = TRUE), -2), 
      TRUE ~ v215
    )
  ) %>%
  ungroup() %>%
  mutate(
    v214 = case_when(
      v213 == 1 & v214 == 0 & v215 > 0 ~ v215 + 1000,
      TRUE ~ v214
    )
  )

tbl05_clean <- tbl05 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id) 

section2a2_clean <- section2a2 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id)

tbl05_clean <- tbl05_clean[, names(section2a2_clean)]

all.equal(section2a2_clean, tbl05_clean)

rm(section2a2_clean, tbl05_clean)

############################################ SECTION 2.1.3 (TABLE 06) #############################################

tbl06 <- read_dta("OOPS_Rawdata_2026_03_17/tbl06.dta")

tbl06 <- tbl06 %>%
  mutate(
    v216 = if_else(
      is.na(v216), 
      8, 
      v216
    ),
    v218 = case_when(
      v219a1 > v219b1 ~ 1, 
      v219a1 < v219b1 ~ 2,
      v218 == 96 ~ 2, 
      TRUE ~ v218
    ), 
    v218 = if_else(
      is.na(v218), 
      2, 
      v218
    ),
    v219b1 = if_else(
      v219b1 > 100000, 
      v219b1/10, 
      v219b1
    ), 
    v220 = if_else(
      is.na(v220), 
      1, 
      v220
    ), 
    v222a = if_else(
      !is.na(v222a1), 
      1,
      2
    ), 
    v222b = if_else(
      !is.na(v222b1), 
      1,
      2 
    ),
    v222c = if_else(
      !is.na(v222c1), 
      1,
      2 
    ), 
    v223 = if_else(
      is.na(v223) | v223 == 96, 
      1,
      v223
    ), 
    v224 = if_else(
      is.na(v224), 
      0, 
      v224
    )
  )

tbl06 <- tbl06 %>%
  group_by(palika, v216) %>%
  mutate(
    v217 = if_else(
      v216 %in% c(8,9) & v217 == 0,
      round(mean(v217)),
      v217
    )
  ) %>%
  ungroup() %>%
  mutate(
    v219a = if_else(
      is.na(v219a1), 
      2, 
      1
    ), 
    v219b = if_else(
      is.na(v219b1), 
      2, 
      1
    ), 
    v219c = if_else(
      is.na(v219c1), 
      2, 
      1
    ), 
    v219d = if_else(
      is.na(v219d1), 
      2, 
      1
    ),
    v221 = if_else(
      v221 < 10, 
      0, 
      v221
    ),
    v222c1 = if_else(
      v222c1 > 36000,
      v222c1 / 10,
      v222c1
    )
  )

tbl06_clean <- tbl06 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id)

section2a3_clean <- section2a3 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  arrange(id)

tbl06_clean <- tbl06_clean[, names(section2a3_clean)]

all.equal(section2a3_clean, tbl06_clean)

rm(section2a3_clean, tbl06_clean)

############################################ SECTION 2.2 (TABLE 07) #############################################

tbl07 <- read_dta("OOPS_Rawdata_2026_03_17/tbl07.dta")

cols_232 <- paste0("v232", letters[1:10])   
serial_vals_232 <- setNames(1:10, cols_232) 

cols_233 <- paste0("v233", letters[1:10])
serial_vals_233 <- setNames(1:10, cols_233)

tbl07 <- tbl07 %>%
  mutate(
    v226 = case_when(
      v227 != "" ~ 1,
      TRUE ~ v226
    ), 
    v228 = case_when(
      enrollment %in% c(1, 3) ~ 1,
      TRUE ~ v228
    ),
    v229 = case_when(
      enrollment == 1 ~ 1,
      enrollment == 3 ~ 2,
      TRUE ~ v229
    ),
    v230 = case_when(
      enrollment == 1 & v228 == 1 & is.na(v230) ~ 1,
      enrollment == 3 & v228 == 1 & is.na(v230) ~ 6,
      TRUE ~ v230
    ),
    across(
      all_of(cols_232),
      ~ if_else(enrollment %in% c(1, 3), NA_real_, .x)
    )
  ) %>%

  mutate(
    all_na_232 = if_all(all_of(cols_232), is.na),
    pick1_232 = (as.integer(id) %% 10) + 1,
    pick2_232 = ((as.integer(id) + 3) %% 10) + 1
  ) %>%
  mutate(
    across(
      all_of(cols_232),
      ~ case_when(
        enrollment %in% c(2, 4) & all_na_232 &
          (match(cur_column(), cols_232) %in% c(pick1_232, pick2_232)) ~ 
            serial_vals_232[cur_column()],
        enrollment %in% c(2, 4) & all_na_232 ~ NA_real_,
        TRUE ~ .x
      )
    )
  ) %>%
  select(-all_na_232, -pick1_232, -pick2_232) %>%

  mutate(
    across(
      all_of(cols_233),
      ~ if_else(enrollment %in% c(2, 4), NA_real_, .x)
    ),
    all_na_233 = if_all(all_of(cols_233), is.na),
    pick1_233 = (as.integer(id) %% 10) + 1,
    pick2_233 = ((as.integer(id) + 5) %% 10) + 1
  ) %>%
  mutate(
    across(
      all_of(cols_233),
      ~ case_when(
        enrollment %in% c(1, 3) & all_na_233 &
          (match(cur_column(), cols_233) %in% c(pick1_233, pick2_233)) ~ 
            serial_vals_233[cur_column()],
        enrollment %in% c(1, 3) & all_na_233 ~ NA_real_,
        TRUE ~ .x
      )
    )
  ) %>%
  select(-all_na_233, -pick1_233, -pick2_233) %>%

  mutate(
    v234 = case_when(
      enrollment %in% c(2, 3, 4) ~ NA_real_,
      TRUE ~ v234
    ), 
    v235 = case_when(
      enrollment == 1 & is.na(v234) ~ 2,
      enrollment %in% c(2, 3, 4) ~ NA_real_,
      TRUE ~ v235
    ),
    v236 = case_when(
      enrollment %in% c(2, 3, 4) ~ NA_real_,
      TRUE ~ v236
    ), 
    v238 = case_when(
      enrollment %in% c(1, 2, 4) ~ NA_real_,
      TRUE ~ v238
    ), 
    v239 = case_when(
      enrollment == 3 & is.na(v239) ~ 2, 
      enrollment %in% c(1, 2, 4) ~ NA_real_,
      TRUE ~ v239
    ), 
    v241 = case_when(
      enrollment == 1 & is.na(v241) ~ 1,
      enrollment == 2 & is.na(v241) ~ 2,
      enrollment %in% c(3, 4) ~ NA_real_,
      TRUE ~ v241
    ),
    v242 = case_when(
      enrollment == 1 & is.na(v242) ~ 1, 
      enrollment %in% c(2, 3, 4) ~ NA_real_,
      TRUE ~ v242
    ), 
    v243 = case_when(
      enrollment == 1 & is.na(v243) ~ 2,
      enrollment == 1 & is.na(v244) ~ 2,
      enrollment == 1 & !is.na(v244) ~ 1,
      enrollment %in% c(2, 3, 4) ~ NA_real_,
      TRUE ~ v243
    ), 
    v244 = case_when(
      v243 == 2 ~ NA_real_,
      is.na(v243) ~ NA_real_,
      TRUE ~ v244
    ),
    v245 = case_when(
      enrollment == 2 & !is.na(v246) ~ 1,
      enrollment == 2 & is.na(v246) ~ 2, 
      TRUE ~ NA_real_
    ),
    v250 = case_when(
      is.na(v250) ~ 3, 
      TRUE ~ v250
    ), 
    v251 = case_when(
      is.na(v252) ~ 2, 
      !is.na(v252) ~ 1,
      TRUE ~ v251
    )
  ) %>%
  mutate( 
    v228 = if_else(
      is.na(v229), 
      2, 
      1
    )
  )


tbl07_clean <- tbl07 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  arrange(id)

section2b_clean <- section2b %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  arrange(id)

tbl07_clean <- tbl07_clean[, names(section2b_clean)]

all.equal(section2b_clean, tbl07_clean)

rm(section2b_clean, tbl07_clean)

############################################ SECTION 2.3 (TABLE 08) #############################################

tbl08 <- read_dta("OOPS_Rawdata_2026_03_17/tbl08.dta")

tbl08 <- tbl08 %>%
  filter(!is.na(v259)) %>%
  mutate(
    v256 = case_when(
      is.na(v256) | v256 > 2000 ~ 1, 
      TRUE ~ v256
    ),

    v258 = if_else(
      is.na(v258), 
      2, 
      v258
    ), 
    v259 = if_else(
      is.na(v259), 
      mean(v259, na.rm = TRUE),
      v259
    ), 
    v260 = case_when(
      v260 == 96 & v260a == "" ~ 9,
      v257 == "CHAN" ~ 9,
      v260a %in% c(
        "OLD AGE MULTI ORGAN FAIL", "AGE VAYERA", "BUDO VAYE RA", "AGE VAYERA", "BRIDHABASTHA",
        "UMER PUGER", "UMER PUGERA", "OLD AGE", "NORMAL", "UMER PUGER BITNU  VAYEKO", "BUDO VAYARA",
        "UMERA PUGER BITNU BHAYEKO", " OLD AGE", "UMER PUGERA MRITYU VAYEKO, 96", "96"
      ) ~ 9,
      v260a %in% c(
        "HART PROBLEM", "KIDDNI FAIL", "DIMAG KO NASA FATERA", "HIP MA GHAU", "HEART ATECT",
        "HEART ATTACK", "PRESSURE", "PARALYZED", "INFECTION ON HAND", "BODY SOLILING", "BLOOD PRESSURE"
      ) ~ 2,
      v260a %in% c(
        "LADAKO", "THAHA NA VAYEKO"
      ) ~ 4,
      v260a %in% c(
        "DENGUE"
      ) ~ 1,
      TRUE ~ v260
    )
  )

tbl08_clean <- tbl08 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  arrange(id)

section2c_clean <- section2c %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  arrange(id)

tbl08_clean <- tbl08_clean[, names(section2c_clean)]

all.equal(section2c_clean, tbl08_clean)

rm(section2c_clean, tbl08_clean)

############################################ SECTION 3.1 (TABLE 09) #############################################

tbl09 <- read_dta("stata_data/section3a.dta")

tbl09 <- tbl09 %>%
  mutate(
    v302 = case_when(
      (v303 == 0 | is.na(v303)) & 
      (v304 == 0 | is.na(v304)) &
      (v305 == 0 | is.na(v305))~ 2, 
      TRUE ~ 1
    ), 
    v303 = case_when(
      v303 == 116600 ~ 600, 
      TRUE ~ v303
    ), 
    v304 = case_when(
      v304 == 9229222 ~ 922, 
      TRUE ~ v304
    ), 
    v305 = case_when(
      v305 == 18000 ~ 1800,
       TRUE ~ v304
    )
  )

tbl09 <- tbl09 %>%
  mutate(
    across(v303:v305, ~ na_if(.x, 0))
  ) %>%
   mutate(
    v305 = case_when(
      v301 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) & v304 == v305 ~ NA_real_,
      TRUE ~ v305
    )
  )

tbl09  <- tbl09 %>%
  group_by(district, v301) %>%
  mutate(
    v303 = case_when(
      v301 == 1 & v302 == 1 & v303 > 1500 ~ round(mean(v303[v303 <= 1500], na.rm = TRUE), -2),
      v301 == 2 & v302 == 1 & v303 > 400  ~ round(mean(v303[v303 <= 400],  na.rm = TRUE), -2),
      v301 == 3 & v302 == 1 & v303 > 500  ~ round(mean(v303[v303 <= 500],  na.rm = TRUE), -2),
      v301 == 4 & v302 == 1 & v303 > 500  ~ round(mean(v303[v303 <= 500],  na.rm = TRUE), -2),
      v301 == 5 & v302 == 1 & v303 > 500  ~ round(mean(v303[v303 <= 500],  na.rm = TRUE), -2),
      v301 == 6 & v302 == 1 & v303 > 300  ~ v303/10,
      v301 == 7 & v302 == 1 & v303 > 500  ~ round(mean(v303[v303 <= 500],  na.rm = TRUE), -2),
      v301 == 8 & v302 == 1 & v303 > 300  ~ round(mean(v303[v303 <= 300],  na.rm = TRUE), -2),
      v301 == 9 & v302 == 1 & v303 > 500  ~ round(mean(v303[v303 <= 500],  na.rm = TRUE), -2),
      v301 == 10 & v302 == 1 & v303 > 100 ~ round(mean(v303[v303 <= 100],  na.rm = TRUE), -2),
      v301 == 11 & v302 == 1 & v303 > 50  ~ round(mean(v303[v303 <= 50],   na.rm = TRUE), -2),
      v301 == 12 & v302 == 1 & v303 > 50  ~ round(mean(v303[v303 <= 50],   na.rm = TRUE), -2),
      v301 == 13 & v302 == 1 & v303 > 100 ~ round(mean(v303[v303 <= 100],  na.rm = TRUE), -2),
      v301 == 14 & v302 == 1 & v303 > 20  ~ round(mean(v303[v303 <= 20],   na.rm = TRUE), -2),
      v301 == 15 & v302 == 1 & v303 > 50  ~ round(mean(v303[v303 <= 50],   na.rm = TRUE), -2),
      TRUE ~ v303
    ),

    v304 = case_when(
      v301 == 1 & v302 == 1 & v304 > 1500 ~ round(mean(v304[v304 <= 1500], na.rm = TRUE), -2),
      v301 == 2 & v302 == 1 & v304 > 400  ~ round(mean(v304[v304 <= 400],  na.rm = TRUE), -2),
      v301 == 3 & v302 == 1 & v304 > 1000 ~ round(mean(v304[v304 <= 1000], na.rm = TRUE), -2),
      v301 == 4 & v302 == 1 & v304 > 500  ~ round(mean(v304[v304 <= 500],  na.rm = TRUE), -2),
      v301 == 5 & v302 == 1 & v304 > 500  ~ round(mean(v304[v304 <= 500],  na.rm = TRUE), -2),
      v301 == 6 & v302 == 1 & v304 > 300  ~ v304/10,
      v301 == 7 & v302 == 1 & v304 > 500  ~ round(mean(v304[v304 <= 500],  na.rm = TRUE), -2),
      v301 == 8 & v302 == 1 & v304 > 600  ~ round(mean(v304[v304 <= 600],  na.rm = TRUE), -2),
      v301 == 9 & v302 == 1 & v304 > 500  ~ round(mean(v304[v304 <= 500],  na.rm = TRUE), -2),
      v301 == 10 & v302 == 1 & v304 > 300 ~ round(mean(v304[v304 <= 300],  na.rm = TRUE), -2),
      v301 == 11 & v302 == 1 & v304 > 100 ~ round(mean(v304[v304 <= 100],  na.rm = TRUE), -2),
      v301 == 12 & v302 == 1 & v304 > 250 ~ round(mean(v304[v304 <= 250],  na.rm = TRUE), -2),
      v301 == 13 & v302 == 1 & v304 > 2000~ round(mean(v304[v304 <= 2000], na.rm = TRUE), -2),
      v301 == 14 & v302 == 1 & v304 > 300 ~ round(mean(v304[v304 <= 300],  na.rm = TRUE), -2),
      v301 == 15 & v302 == 1 & v304 > 350 ~ round(mean(v304[v304 <= 350],  na.rm = TRUE), -2),
      TRUE ~ v304
    )
  ) %>%
  ungroup() %>%
  group_by(v301) %>%
  mutate(
    v304 = case_when(
      v301 == 1 & v302 == 1 & is.na(v303) & is.na(v304) & is.na(v305) ~ 
        round(mean(v304[v304 <= 1500], na.rm = TRUE), -2),
      v301 == 6 & v302 == 1 & v304 > 300 ~ 
        round(mean(v304[v304 <= 300], na.rm = TRUE), -2),
      TRUE ~ v304
    ),
    v303 = case_when(
      v301 == 6 & v302 == 1 & v303 > 300 ~ 
        round(mean(v303[v303 <= 300], na.rm = TRUE), -2),
      TRUE ~ v303
    )
  ) %>%
  ungroup()
  
tbl09 <- tbl09 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    v304 = case_when(
      id %in% c(4033, 11545, 13787, 12392, 13965, 11492, 14473) & v301 == 1 ~ 500,
      id %in% c(4033, 11545, 13787, 12392, 13965, 11492, 14473) & v301 == 2 ~ 200,
      TRUE ~ v304
    )
  )

tbl09 <- tbl09 %>%
  mutate(
    v302 = case_when(
      (is.na(v303) | is.nan(v303)) & (is.na(v304) | is.nan(v304)) & (is.na(v305) | is.nan(v305)) ~ 2, 
      TRUE ~ 1
    )
  )

district_means <- tbl09 %>%
  filter(v301 %in% c(1, 2, 6, 7)) %>%
  group_by(district, v301) %>%
  summarise(
    mean_v303 = round(mean(v303, na.rm = TRUE), -2),
    mean_v304 = round(mean(v304, na.rm = TRUE), -2),
    .groups = "drop"
  )

tbl09 <- tbl09 %>%
  left_join(district_means, by = c("district", "v301"))

tbl09 <- tbl09 %>%
  group_by(hhid) %>%
  mutate(
    all_na_304 = all(is.na(v304))
  ) %>%
  ungroup()

tbl09 <- tbl09 %>%
  mutate(
    v304 = ifelse(all_na_304, mean_v304, v304)
  ) %>%
  select(-mean_v303, -mean_v304, -all_na_304, -hhid)

rm(district_means)

tbl09_clean <- tbl09 %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  arrange(id)

section3a_clean <- section3a %>%
  select(-hhld) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  arrange(id)

tbl09_clean <- tbl09_clean[, names(section3a_clean)]

all.equal(section3a_clean, tbl09_clean)

rm(section3a_clean, tbl09_clean)

############################################ SECTION 3.2 (TABLE 10) #############################################

tbl10 <- read_dta("OOPS_Rawdata_2026_03_17/tbl10.dta")

tbl10 <- tbl10 %>%
  mutate(
    v307 = case_when(
      (is.na(v308) | v308 == 0) & (is.na(v309) | v309 == 0) ~ 2,
      TRUE ~ 1
    ),
    across(v308:v309, ~ na_if(.x, 0))
  ) %>%
  group_by(psu, v306) %>%
  mutate(
    v308 = case_when(
      v306 == 1 & v307 == 1 & v308 > 400   ~ round(mean(v308[v308 <= 400], na.rm = TRUE), -2),
      v306 == 2 & v307 == 1 & v308 > 3000  ~ round(mean(v308[v308 <= 3000], na.rm = TRUE), -2),
      v306 == 4 & v307 == 1 & v308 > 150   ~ round(mean(v308[v308 <= 150], na.rm = TRUE), -2),
      v306 == 5 & v307 == 1 & v308 > 5000  ~ round(mean(v308[v308 <= 5000], na.rm = TRUE), -2),
      v306 == 6 & v307 == 1 & v308 > 300   ~ round(mean(v308[v308 <= 300], na.rm = TRUE), -2),
      v306 == 7 & v307 == 1 & v308 > 2000  ~ round(mean(v308[v308 <= 2000], na.rm = TRUE), -2),
      v306 == 8 & v307 == 1 & v308 > 1000  ~ round(mean(v308[v308 <= 2000], na.rm = TRUE), -2),
      TRUE ~ v308
    ),
    v309 = case_when(
      v306 == 1 & v307 == 1 & v309 > 250   ~ round(mean(v309[v309 <= 250], na.rm = TRUE), -2),
      v306 == 4 & v307 == 1 & v309 > 150   ~ round(mean(v309[v309 <= 150], na.rm = TRUE), -2),
      v306 == 5 & v307 == 1 & v309 > 5000  ~ round(mean(v309[v309 <= 5000], na.rm = TRUE), -2),
      v306 == 6 & v307 == 1 & v309 > 300   ~ round(mean(v309[v309 <= 300], na.rm = TRUE), -2),
      v306 == 7 & v307 == 1 & v309 > 2000  ~ round(mean(v309[v309 <= 2000], na.rm = TRUE), -2),
      v306 == 8 & v307 == 1 & v309 > 500   ~ round(mean(v309[v309 <= 2000], na.rm = TRUE), -2),
      TRUE ~ v309  
    )
  ) %>%
  ungroup()

tbl10 <- tbl10 %>%
  group_by(district, v306) %>%
  mutate(
    v308 = case_when(
      is.nan(v308) ~ round(mean(v308, na.rm = TRUE), -2),
      TRUE ~ v308
    ), 
    v309 = case_when(
      is.nan(v309) ~ round(mean(v309, na.rm = TRUE), -2),
      TRUE ~ v309
    ) 
  ) %>%
  ungroup()

tbl10 <- tbl10 %>%
  group_by(province, v306) %>%
  mutate(
    v308 = case_when(
      is.nan(v308) ~ round(mean(v308, na.rm = TRUE), -2),
      TRUE ~ v308
    ), 
    v309 = case_when(
      is.nan(v309) ~ round(mean(v309, na.rm = TRUE), -2),
      TRUE ~ v309
    ) 
  ) %>%
  ungroup()

tbl10_clean <- tbl10 %>%
  select(-hhld) %>%
  arrange(id, v306)

section3b_clean <- section3b %>%
  select(-hhld) %>%
  arrange(id, v306)

tbl10_clean <- tbl10_clean[, names(section3b_clean)]

all.equal(section3b_clean, tbl10_clean)

rm(section3b_clean, tbl10_clean)

############################################ SECTION 4.1 (TABLE 11) #############################################

tbl11 <- read_dta("OOPS_Rawdata_2026_03_17/tbl11.dta")

tbl11 <- tbl11 %>%
  mutate(
    v402 = case_when(
      (is.na(v403a) | v403a == 0) &
      (is.na(v403b) | v403b == 0) ~ 2,
      TRUE ~ 1
    ), 
    v403a = case_when(
      v403a == 30000083 ~ 3000,
      id == 9092 & v401 == 3 ~ 6000, 
      TRUE ~ v403a
    ),
    v403b = case_when(
      id == 4581 ~ 1200,
      id == 3058 ~ 10000, 
      TRUE ~ v403b
    ), 
    across(v403a:v403b, ~ na_if(.x, 0))
  )

tbl11 <- tbl11 %>%
  group_by(psu, v401) %>%
  mutate(
    v403a = case_when(
      v401 == 1 & v402 == 1 & v403a > 100000 ~ round(mean(v403a[v403a <= 100000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 1 & v402 == 1 & v403b > 20000 ~ round(mean(v403b[v403b <= 20000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 2 & v402 == 1 & v403a > 25000 ~ round(mean(v403a[v403a <= 25000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 2 & v402 == 1 & v403b > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 3 & v402 == 1 & v403a > 650000 ~ round(mean(v403a[v403a <= 650000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 3 & v402 == 1 & v403b > 650000 ~ round(mean(v403b[v403b <= 650000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 4 & v402 == 1 & v403a > 126000 ~ round(mean(v403a[v403a <= 126000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 4 & v402 == 1 & v403b > 11000 ~ round(mean(v403b[v403b <= 11000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      id == 12142 & v401 == 5 ~ 600000,
      TRUE ~ v403a
    ), 
    v403a = case_when(
      id == 12142 & v401 == 6 ~ 150000,
      id == 2863 & v401 == 6 ~ 150000,
      TRUE ~ v403a
    ),
    v403a = case_when(
      v401 == 8 & v402 == 1 & v403a > 100000 ~ round(mean(v403a[v403a <= 100000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 8 & v402 == 1 & v403b > 10000 ~ round(mean(v403b[v403b <= 10000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 9 & v402 == 1 & v403a > 75000 ~ round(mean(v403a[v403a <= 75000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 9 & v402 == 1 & v403b > 12000 ~ round(mean(v403b[v403b <= 10000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 10 & v402 == 1 & v403a > 25000 ~ round(mean(v403a[v403a <= 25000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 10 & v402 == 1 & v403b > 2000 ~ round(mean(v403b[v403b <= 2000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      id == 6874 & v401 == 12 & v402 == 1 ~ 270000, 
      TRUE ~ v403a
    ), 
    v403a = case_when(
      v401 == 13 & v402 == 1 & v403a > 40000 ~ round(mean(v403a[v403a <= 40000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 13 & v402 == 1 & v403b > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 14 & v402 == 1 & v403a > 25000 ~ round(mean(v403a[v403a <= 25000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 14 & v402 == 1 & v403b > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE), -2),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 15 & v402 == 1 & v403a > 150000 ~ v403a / 10, 
      TRUE ~ v403a
    ),
    v403b = case_when(
      v401 == 15 & v402 == 1 & v403b > 100000 ~ v403b / 10, 
      TRUE ~ v403b
    ), 
    v403a = case_when(
      v401 == 18 & v402 == 1 & v403a > 70000 ~ round(mean(v403a[v403a <= 70000], na.rm = TRUE), -2), 
      TRUE ~ v403a
    ),
    v403b = case_when(
      v401 == 18 & v402 == 1 & v403b > 10000 ~ round(mean(v403b[v403b <= 10000], na.rm = TRUE), -2), 
      TRUE ~ v403b
    ), 
    v403a = case_when(
      v401 == 19 & v402 == 1 & v403a > 35000 ~ round(mean(v403a[v403a <= 35000], na.rm = TRUE), -2), 
      TRUE ~ v403a
    ),
    v403b = case_when(
      v401 == 19 & v402 == 1 & v403b > 25000 ~ round(mean(v403b[v403b <= 25000], na.rm = TRUE), -2), 
      TRUE ~ v403b
    ), 
    v403a = case_when(
      v401 == 20 & v402 == 1 & v403a > 150000 ~ v403a / 10, 
      TRUE ~ v403a
    ),
    v403a = case_when(
      v401 == 21 & v402 == 1 & v403a > 400000 ~ v403a / 10, 
      TRUE ~ v403a
    ),
    v403b = case_when(
      v401 == 21 & v402 == 1 & v403b > v403a ~ v403b / 10, 
      TRUE ~ v403b
    ),
    v403b = case_when(
      v401 == 21 & v402 == 1 & v403b == 1000000 ~ 100000,
      TRUE ~ v403b
    ), 
    v403a = case_when(
      v401 == 21 & v402 == 1 & is.na(v403a) & !is.na(v403b) ~ v403b,
      TRUE ~ v403a
    ), 
    v403a = case_when(
      v401 == 22 & v402 == 1 & v403a > 50000 ~ round(mean(v403a[v403a <= 50000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 22 & v402 == 1 & v403a > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE), -2)
    ), 
    v403a = case_when(
      v401 == 22 & v402 == 1 & v403a > 50000 ~ round(mean(v403a[v403a <= 50000], na.rm = TRUE), -2),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 22 & v402 == 1 & v403a > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE), -2), 
      TRUE ~ v403b
    ), 
    v403a = case_when(
      v401 == 24 & v402 == 1 & v403a > 10000 ~ v403a/10,
      TRUE ~ v403a
    ),
    v403a = case_when(
      v401 == 25 & v402 == 1 & v403a > 150000 ~ v403a/10, 
      TRUE ~ v403a
    ),
    v403a = case_when(
      v401 == 26 & v402 == 1 & v403a > 350000 ~ v403a/10,
      TRUE ~ v403a
    ), 
    v403a = case_when(
      v401 == 27 & v402 == 1 & v403a > 35000 ~ v403a/10, 
      TRUE ~ v403a
    ),
    v403a = case_when(
      v401 == 29 & v402 == 1 & v403a == 4000000 ~ 400000,
      TRUE ~ v403a
    ), 
    v403a = case_when(
      v401 == 30 & v402 == 1 & v403a > 360000 ~ v403a/10,
      TRUE ~ v403a
    )
  ) %>%
  ungroup()

tbl11 <- tbl11 %>%
  mutate(
    v403b = case_when(
      v401 == 21 & v402 == 1 & v403a == v403b ~ NA_real_,
      TRUE ~ v403b
    )
  )

tbl11_clean <- tbl11 %>%
  select(-hhld) %>%
  arrange(id, v401)

section4a_clean <- section4a %>%
  select(-hhld) %>%
  arrange(id, v401)

tbl11_clean <- tbl11_clean[, names(section4a_clean)]

all.equal(section4a_clean, tbl11_clean)

rm(section4a_clean, tbl11_clean)

############################################ SECTION 4.2 (TABLE 12) #############################################

tbl12 <- read_dta("OOPS_Rawdata_2026_03_17/tbl12.dta")

tbl12 <- tbl12 %>%
  mutate(
    v404 = case_when(
      v404 == 2 & v407a %in% c(40000, 2120000, 4e+05) ~ 1,
      TRUE ~ v404
    )
  )

tbl12 <- tbl12 %>%
  group_by(v405) %>%
  mutate(
    v407a = case_when(
      v405 == 3 & v406 == 1 & v407a > 250000 ~ round(mean(v407a[v407a <= 250000], na.rm = TRUE), -2),
      TRUE ~ v407a
    ), 
    v407a = case_when(
      v405 == 1 & v406 == 1 & v407a > 400000 ~ round(mean(v407a[v407a <= 400000], na.rm = TRUE), -2),
      TRUE ~ v407a
    ),
    v407a = case_when(
      v405 == 8 & v406 == 1 & v407a > 160000 ~ round(mean(v407a[v407a <= 160000], na.rm = TRUE), -2),
      TRUE ~ v407a
    )
  ) %>%
  ungroup()

tbl12_clean <- tbl12 %>%
  select(-hhld) %>%
  arrange(id, v405) 

section4b_clean <- section4b %>%
  select(-hhld) %>%
  arrange(id, v405)

tbl12_clean <- tbl12_clean[, names(section4b_clean)]

all.equal(section4b_clean, tbl12_clean)

rm(tbl12_clean, section4b_clean)

############################################ SECTION 4.3 (TABLE 13) #############################################

tbl13 <- read_dta("OOPS_Rawdata_2026_03_17/tbl13.dta")

for (i in setdiff(1:ncol(tbl13), c(10, 21))) {
  tbl13[[i]] <- as.numeric(gsub("[^0-9]", "", tbl13[[i]]))
}

tbl13 <- tbl13 %>%
  mutate(
    v409 = case_when(
      is.na(v410) & is.na(v411a) & is.na(v411b) ~ 2,
      TRUE ~ 1
    ), 
    v410 = case_when(
      is.na(v410) & !is.na(v411a) & !is.na(v411b) ~ 1,
      TRUE ~ v410
    ), 
    v410 = case_when(
      v410 %in% c(275000, 120) ~ 12, 
      v410 %in% c(150000, 56000) ~ 5, 
      v410 %in% c(8000, 2500, 2200) ~ 1, 
      TRUE ~ v410
    ),
    v411a = if_else(
      is.na(v411a), 
      0,
      v411a
    ), 
   tmp_v412a = v412a,
    v412a = if_else(v412a > v412b, v412b, v412a),
    v412b = if_else(tmp_v412a > v412b, tmp_v412a, v412b)
  ) %>%
  select(-tmp_v412a) %>%
  group_by(v408) %>%
  mutate(
    commodity_mean = round(mean(v412a[v412a > 0 & v412a <= 29], na.rm = TRUE), -2),
    v412a = if_else(
      (v412a > 29 | (v412a == 0 & v412b > 0)) & !is.nan(commodity_mean),
      commodity_mean,
      v412a
    )
  ) %>%
  ungroup() %>%
  select(-commodity_mean)

tbl13 <- tbl13 %>%
  group_by(psu, v408) %>%
  mutate(
    across(
      c(v410, v411a, v411b, v411a, v411b, v413),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE), -2)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

tbl13_clean <- tbl13 %>%
  select(-hhld) %>%
  arrange(id, v408)

section4c_clean <- section4c %>%
  select(-hhld) %>%
  arrange(id, v408)

tbl13_clean <- tbl13_clean[, names(section4c_clean)]

all.equal(section4c_clean, tbl13_clean)

rm(section4c_clean, tbl13_clean)

############################################ SECTION 4.4 (TABLE 14) #############################################

tbl14 <- read_dta("OOPS_Rawdata_2026_03_17/tbl14.dta")

tbl14 <- tbl14 %>%
  group_by(psu, v414) %>%
  mutate(
    v416a = as.numeric(v416a),
    v416b = as.numeric(v416b),
    across(
      c(v416a, v416b),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE), -2)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

tbl14 <- tbl14 %>%
  mutate(
    v415 = if_else(
      (is.na(v416a) & is.na(v416b)),
      2, 
      1
    )
  )

tbl14_clean <- tbl14 %>%
  arrange(id, v414) %>%
  select(-hhld)

section4d_clean <- section4d %>%
  select(-hhld) %>%
  arrange(id, v414)

tbl14_clean <- tbl14_clean[, names(section4d_clean)]

all.equal(tbl14_clean, section4d_clean)

rm(section4d_clean, tbl14_clean)

############################################ SECTION 5 (TABLE 15) #############################################

tbl15 <- read_dta("OOPS_Rawdata_2026_03_17/tbl15.dta")

tbl15 <- tbl15 %>%
  filter(
    !if_all(v502a:v502g, is.na)
  )

tbl15 <- tbl15 %>%
  mutate(
    v501 = case_when(
      (is.na(v502e) | v502e == 0) ~ 1,
      (v501 == 96 & v502e > 0) ~ 2,
      TRUE ~ v501
    ), 
    v503 = case_when(
      v504 > 0 ~ 1, 
      TRUE ~ 2
    ),
    v502a = case_when(
      personid == 39444 ~ 80000,
      TRUE ~ v502a
    )
  )

tbl15 <- tbl15 %>%
  mutate(
    personid = case_when(
      personid == 25472 ~ 25477,
      personid == 54256 ~ 54261,
      personid == 54527 ~ 54262,
      personid == 19178 ~ 19181,
      personid == 22665 ~ 22666,
      personid == 26137 ~ 26139,

      TRUE ~ personid 
    )
  ) %>%
  filter(!personid %in% c(
    54242, 54243, 54244, 54248, 54249, 54253, 54256, 54257, 
    54258, 54259 
    )
  ) %>%
  group_by(personid) %>%
  slice(1) %>%
  ungroup()

tbl15 <- merge(
  tbl15,
  tbl02[, c("personid", "v104a")], 
  by = "personid"
)

tbl15 <- merge(
  tbl15,
  tbl03[, c("personid", "v116")],
  by = "personid"
)

tbl15 <- tbl15 %>%
  mutate(
    years_required = case_when(
      v116 == 1 ~ 5, 
      v116 == 2 ~ 6,
      v116 == 3 ~ 7, 
      v116 == 4 ~ 8, 
      v116 == 5 ~ 9, 
      v116 == 6 ~ 10, 
      v116 == 7 ~ 11, 
      v116 == 8 ~ 12, 
      v116 == 9 ~ 13, 
      v116 == 10 ~ 14, 
      v116 == 11 ~ 15, 
      v116 == 12 ~ 15, 
      v116 == 13 ~ 17, 
      v116 == 14 ~ 20, 
      v116 == 15 ~ 22,
      v116 == 16 ~ 26, 
      TRUE ~ NA_real_
    ),
    error = case_when(
      v104a < years_required ~ "ERROR",
      v104a >= 30 & v116 <= 12 ~ "ERROR",
      TRUE ~ NA_character_
    )  
  ) %>%
  select(-v101)

education_error <- tbl15 %>%
  filter(!is.na(error)) %>%
  select(id, personid, v104a, v116)

rm(education_error)

tbl15 <- merge(
  tbl15, 
  tbl02[, c("personid", "v101")], 
  by = "personid"
)

tbl15 <- tbl15 %>%
  select(-error, -v116, -v104a, -years_required) %>%
  select(enrollment:uid, personid, v101, everything())

education_section <- tbl15 %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  ungroup()

tbl03 <- merge(
  tbl03,
  tbl02[, c("personid", "v104a")], 
  by = "personid"
)

tbl03 <- tbl03 %>%
  mutate(
    v115 = case_when(
      personid %in% tbl15$personid & v104a >= 5 ~ 3,
      !personid %in% tbl15$personid & v115 == 3 ~ 2,
      TRUE ~ v115
    ), 
    v114 = case_when(
      personid %in% tbl15$personid & v104a >= 5 ~ 1, 
      TRUE ~ v114
    )
  ) %>%
  select(-v104a)

rm(education_section)

tbl15 <- tbl15 %>%
  mutate(
    v504 = case_when(
      v504 > rowSums(across(v502a:v502g), na.rm = TRUE) ~ v504 / 10,
      TRUE ~ v504
    )
  )

tbl15_clean <- tbl15 %>%
  select(-hhld) %>%
  arrange(personid)

section5_clean <- section5 %>%
  select(-hhld) %>%
  arrange(personid)

tbl15_clean <- tbl15_clean[, names(section5_clean)]

all.equal(section5_clean, tbl15_clean)

rm(tbl15_clean, section5_clean)

############################################ SECTION 6.1 (TABLE 16) #############################################

tbl16 <- read_dta("OOPS_Rawdata_2026_03_17/tbl16.dta")

tbl03 <- merge(
  tbl03, 
  tbl02[, c("personid", "v104a")], 
  by = "personid"
)

s6a_qualified <- tbl03 %>%
  filter(v104a >= 5)

tbl16 <- tbl16 %>%
  filter(personid %in% s6a_qualified$personid)

s6a_missing <- s6a_qualified %>%
  filter(!personid %in% tbl16$personid)

s6a_missing <- s6a_missing %>%
  select(-v111:-v121, -v104a, -chfid:-target_group_imis) 

tbl16 <- tbl16 %>%
  rows_append(s6a_missing)

tbl02 <- tbl02 %>%
  mutate(
    age_group = case_when(
      v104a >= 10 & v104a < 20 ~ "10-20",
      v104a >= 20 & v104a < 30 ~ "20-30",
      v104a >= 30 & v104a < 40 ~ "30-40", 
      v104a >= 40 & v104a < 50 ~ "40-50",
      v104a >= 50 & v104a < 60 ~ "50-60",
      v104a >= 60 & v104a < 70 ~ "60-70",
      v104a >= 70 & v104a < 80 ~ "70-80",
      v104a >= 80 & v104a < 90 ~ "80-90",
      v104a >= 90 & v104a < 100 ~ "90-100",
      TRUE ~ "0-10"
    )
  )

tbl16 <- merge(
  tbl16,
  tbl02[, c("personid", "age_group")],
  by = "personid",
  all = FALSE 
)

tbl02 <- tbl02 %>%
  select(-age_group)

tbl16 <- tbl16 %>%
  group_by(age_group) %>%
  mutate(
    v601a = if_else(
      is.na(v601a), 
      round(mean(v601a, na.rm = TRUE)), 
      v601a
    ), 
    v601b = if_else(
      is.na(v601b), 
      round(mean(v601b, na.rm = TRUE)), 
      v601b
    ), 
    v601c = if_else(
      is.na(v601c), 
      round(mean(v601c, na.rm = TRUE)), 
      v601c
    ), 
    v601d = if_else(
      is.na(v601d),
      round(mean(v601d, na.rm = TRUE)), 
      v601d
    ), 
    v601e = if_else(
      is.na(v601e), 
      round(mean(v601e, na.rm = TRUE)), 
      v601e
    ), 
    v602 = if_else(
      is.na(v602) | v602 == 0, 
      round(mean(v602[v602 != 0], na.rm = TRUE)), 
      v602
    )
  ) %>%
  ungroup() %>%
  select(-age_group)

tbl16_clean <- tbl16 %>%
  select(-hhld) %>%
  arrange(personid)

section6a_clean <- section6a %>%
  select(-hhld) %>%
  arrange(personid)

tbl16_clean <- tbl16_clean[, names(section6a_clean)]

all.equal(section6a_clean, tbl16_clean)

rm(tbl16_clean, section6a_clean)

############################################ SECTION 6.2.1 (TABLE 17) #############################################

tbl17 <- read_dta("OOPS_Rawdata_2026_03_17/tbl17.dta")

tbl17 <- tbl17 %>%
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
  ) %>%
  mutate(
    v610n = if_else(
      !is.na(v610n_1), 
      1, 
      v610n
    )
  )

tbl17 <- tbl17 %>% 
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
  v611m = ifelse(!is.na(v611m_1), 1, v611m_1)
  ) %>%
  select(-v611) %>%
  rename(
  v611a = v611_new,
  v611 = v611_1
  ) 

tbl17 <- merge(
  tbl17, 
  tbl02[, c("personid", "v104a")],
  by = "personid"
)

tbl17 <- tbl17 %>%
  group_by(v604) %>%
  mutate(
    v104a = as.numeric(v104a),
    v605a = if_else(
      v605a > 36,
      round(mean(v605a[v605a <= 36], na.rm = TRUE)),
      v605a
    ), 
    v606 = case_when(
      personid == 53948 ~ 1,
      v606 > 2000 ~ (2082 - v606), 
      personid == 54361 ~ 2, 
      personid == 11829 ~ 2, 
      v606 > v104a ~ round(mean(v606)),
      TRUE ~ v606
    ), 
    v607 = if_else(
      is.na(v608) & is.na(v609), 
      2, 
      v607
    ), 
    v608 = if_else(
      is.na(v609) & is.na(v610), 
      2, 
      v608
    ), 
    v606 = case_when(
      !is.na(v604) & is.na(v606) ~ round(mean(v606, na.rm = TRUE)),
      TRUE ~ v606
    ), 
    v604 = case_when(
      personid %in% c(26788, 5952061) ~ 30, 
      TRUE ~ v604
    )
  ) %>%
  ungroup()

tbl17 <- tbl17 %>%
  mutate(
    disease_id = paste0(personid, "-", v604),
    v606 = if_else(
      v606 > v104a , 1, v606
    )
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disease_id, -v104a)

tbl17 <- tbl17 %>%
  mutate(
    v604 = case_when(
      personid == 32897 & is.na(v604) ~ 16, 
      personid == 12901 & is.na(v604) ~ 4,
      personid == 34146 & is.na(v604) ~ 2,
      personid == 12847 & is.na(v604) ~ 2,
      personid == 51866 & v604 == 23 ~ 3,
      personid == 51866 & is.na(v604) ~ 2, 
      TRUE ~ v604
    )
  )

tbl17 <- tbl17 %>%
  mutate(
    v604 = as.numeric(v604),
    v604 = case_when(
      personid == 39399 & is.na(v604) ~ 2,
      personid == 25204 & is.na(v604) ~ 2,
      personid == 56777 & is.na(v604) ~ 2,
      personid == 4199 & is.na(v604) ~ 2,
      personid == 30376 & is.na(v604) ~ 5,
      personid == 39404 & is.na(v604) ~ 2,
      personid == 22271 & is.na(v604) ~ 12,
      personid == 32815 & is.na(v604) ~ 12,
      personid == 60551 & is.na(v604) ~ 3,
      personid == 12489 & is.na(v604) ~ 12,
      personid == 12491 & is.na(v604) ~ 12,
      personid == 22323 & is.na(v604) ~ 3,
      personid == 24986 & is.na(v604) ~ 2,
      personid == 24620 & is.na(v604) ~ 3,
      personid == 53546 & is.na(v604) ~ 12,
      personid == 31928 & is.na(v604) ~ 1,
      personid == 54271 & is.na(v604) ~ 2,
      personid == 5953049 & is.na(v604) ~ 12,
      personid == 29544 & is.na(v604) ~ 2,
      personid == 5951573 & is.na(v604) ~ 2,
      personid == 54132 & is.na(v604) ~ 2,
      personid == 1394 & is.na(v604) ~ 13,
      personid == 11082 & is.na(v604) ~ 1,
      personid == 31676 & is.na(v604) ~ 2,
      personid == 22198 & is.na(v604) ~ 15,
      personid == 54199 & is.na(v604) ~ 2,
      personid == 35110 & is.na(v604) ~ 12,
      personid == 52024 & is.na(v604) ~ 4, 
      personid == 5949842 & is.na(v604) ~ 5,
      personid == 3745 & is.na(v604) ~ 16,
      personid == 53819 & v604 == 2 ~ 24,
      personid == 52223 & v604 == 2 ~ 8,
      personid == 41261 & v604 == 2 ~ 6,
      personid == 52178 & v604 == 2 ~ 27,
      personid == 52184 & v604 == 2 ~ 16,
      personid == 56197 & v604 == 2 ~ 28,
      personid == 59006 & v604 == 2 ~ 10,
      personid == 36569 & v604 == 2 ~ 21,
      personid == 22974 & v604 == 2 ~ 24,
      personid == 22435 & v604 == 2 ~ 5,
      personid == 53948 ~ 15,
      personid == 15179 ~ 15,
      personid == 12227 ~ 30,
      personid == 9899 & v604 == 2 ~ 21,
      personid == 28120 & v604 == 2 ~ 1,
      personid == 18291 & v604 == 15 ~ 1,
      personid == 18284 & v604 == 2 ~ 24, 
      personid == 18285 & v604 == 2 ~ 27, 
      personid == 32028 & v604 == 2 ~ 21,
      personid == 54293 & v604 == 2 ~ 16,
      personid == 15055 & v604 == 2 ~ 22,
      personid == 53926 & v604 == 2 ~ 8,
      personid == 14761 & v604 == 2 ~ 1,
      personid == 14714 & v604 == 2 ~ 16,
      personid == 59147 & is.na(v604) ~ 2,
      personid == 59231 & v604 == 2 ~ 16,
      personid == 56057 & v604 == 2 ~ 1,
      personid == 56041 & v604 == 2 ~ 1,
      personid == 59898 & v604 == 2 ~ 12,
      personid == 56840 & is.na(v604) ~ 13,
      personid == 5952123 & v604 == 2 ~ 1,
      personid == 57968 & v604 == 2 ~ 1,
      personid == 5949655 & v604 == 2 ~ 5,
      personid == 59140 & v604 == 2 ~ 23,
      personid == 55257 & v604 == 2 ~ 1, 
      personid == 57838 & v604 == 2 ~ 20,
      personid == 52901 & v604 == 2 ~ 1,
      personid == 30166 & v604 == 2 ~ 21,
      personid == 22867 & v604 == 2 ~ 20,
      personid == 29940 & v604 == 2 ~ 27,
      personid == 31443 & is.na(v604) ~ 2,
      personid == 25651 & v604 == 2 ~ 1,
      personid == 25655 & is.na(v604) ~ 1,
      personid == 22198 & v604 == 13 ~ 15, 
      personid == 20651 & v604 == 2 ~ 1, 
      personid == 21377 & v604 == 2 ~ 5, 
      personid == 35122 & v604 == 2 ~ 23,
      personid == 35583 & v604 == 2 ~ 21,
      personid == 5950647 & v604 == 2 ~ 16,
      personid == 52333 & v604 == 2 ~ 1,
      personid == 52760 & v604 == 2 ~ 1,
      personid == 52761 & v604 == 2 ~ 1,
      personid == 52766 & v604 == 2 ~ 21,
      personid == 52773 & v604 == 2 ~ 7,
      personid == 59759 & is.na(v604) ~ 2,
      personid == 20647 & v604 == 28 ~ 1,
      personid == 51866 & v604 == 3 ~ 23,
      personid == 52345 & v604 == 2 ~ 1,
      personid == 5951804 & v604 == 2 ~ 23,
      personid == 5952805 & v604 == 2 ~ 18,
      personid == 5952811 & v604 == 2 ~ 1,
      personid == 53719 & v604 == 15 ~ 31,
      personid == 48098 & v604 == 2 ~ 13, 
      personid == 56388 & v604 == 2 ~ 30,
      personid == 9627 & v604 == 2 ~ 16, 
      personid == 9514 & v604 == 2 ~ 16, 
      personid == 17123 & v604 == 2 ~ 28,
      personid == 27181 & v604 == 2 ~ 27, 
      personid == 59334 & is.na(v604) ~ 30,
      personid == 3745 & v604 == 2 ~ 16,
      personid == 5949831 & v604 == 96 ~ 2,
      personid == 49700 & v604 == 96 ~ 2,
      personid == 51968 & v604 == 96 ~ 2,
      personid == 29664 & v604 == 1 ~ 2, 
      personid == 15089 & v604 == 1 ~ 2,
      personid == 5949198 & v604 == 96 ~ 2,
      personid == 15055 & v604 == 96 ~ 22, 
      personid == 12062 & v604 == 96 ~ 2, 
      personid == 9556 & v604 == 96 ~ 2,
      personid == 57983 & v604 == 1 ~ 2,

      TRUE ~ v604
    )
  )   

tbl17 <- tbl17 %>%
  mutate(
    v603 == if_else(
      !is.na(v604), 1, 2
    )
  ) %>%
  group_by(v604) %>%
  mutate(
    v605a = case_when(
      !is.na(v604) & is.na(v605a) ~ round(mean(v605a)),
      TRUE ~ v605a
    ), 
    v605b = case_when(
      !is.na(v604) & is.na(v605b) ~ round(mean(v605b)),
      TRUE ~ v605b
    ),
    v606 = case_when(
       !is.na(v604) & is.na(v606) ~ round(mean(v606)), 
       TRUE ~ v606
    )
  ) %>%
  ungroup()

tbl17 <- tbl17 %>%
  select(
    -`v603 == if_else(!is.na(v604), 1, 2)`, -v610a1, -v610a2, -v610a3, -v610a4, -v610a5, 
    -v6111, -v6112, -v6113, -v6114, -v6115, - interviewer, -employer, -employer_name, -employer_sector, 
    -employer_size
  )

tbl17_added_rows <- read.xlsx("misc/health section arrangement/section6b1_added_rows.xlsx")

tbl17_added_rows <- tbl17_added_rows %>%
  mutate(
    enrollment = as.numeric(enrollment), 
    province = as.numeric(province), 
    district = as.numeric(district), 
    palika_type = as.numeric(palika_type), 
    v610n_1 = as.character(v610n_1), 
    v611m_1 = as.character(v611m_1)
  )

tbl17 <- rbind(
  tbl17, tbl17_added_rows
)

tbl17 <- tbl17 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(personid, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup()

tbl17 <- merge(
  tbl17, 
  tbl01[, c("uid", "interviewer", "employer", "employer_name", "employer_sector", "employer_size")],
  by = "uid"
)

tbl17 <- tbl17 %>%
  filter(personid %in% tbl03$personid)

s6b1_missing <- tbl03 %>%
  filter(!personid %in% tbl17$personid)

s6b1_missing <- s6b1_missing %>%
  select(-v111:-v121, -v104a, -chfid:-target_group_imis) %>%
  mutate(
    v603 = 2
  )

tbl17 <- tbl17 %>%
  rows_append(s6b1_missing)

rm(tbl17_added_rows, s6a_qualified, s6b1_missing, s6a_missing)

tbl17_clean <- tbl17 %>%
  select(-hhld, -uniq_id) %>%
  mutate(disease_id = paste0(personid, "-", v604)) %>%
  arrange(disease_id) 

section6b1_clean <- section6b1 %>%
  select(-hhld, -uniq_id) %>%
  mutate(disease_id = paste0(personid, "-", v604)) %>%
  arrange(disease_id) 

tbl17_clean <- tbl17_clean[, names(section6b1_clean)]

all.equal(tbl17_clean, section6b1_clean)

rm(tbl17_clean, section6b1_clean)

############################################ SECTION 6.2.3 (TABLE 19) #############################################

tbl19 <- read_dta("OOPS_Rawdata_2026_03_17/tbl19.dta")

tbl19 <- tbl19 %>%
  mutate(
    disease_id = paste0(personid, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup()

tbl19 <- tbl19 %>%
  mutate(
    v604 = case_when(
      personid == 39399 & is.na(v604) ~ 2,
      personid == 25204 & is.na(v604) ~ 2,
      personid == 56777 & is.na(v604) ~ 2,
      personid == 4199 & is.na(v604) ~ 2,
      personid == 30376 & is.na(v604) ~ 5,
      personid == 39404 & is.na(v604) ~ 2,
      personid == 22271 & is.na(v604) ~ 12,
      personid == 32815 & is.na(v604) ~ 12,
      personid == 60551 & is.na(v604) ~ 3,
      personid == 12489 & is.na(v604) ~ 12,
      personid == 12491 & is.na(v604) ~ 12,
      personid == 22323 & is.na(v604) ~ 3,
      personid == 24986 & is.na(v604) ~ 2,
      personid == 24620 & is.na(v604) ~ 3,
      personid == 53546 & is.na(v604) ~ 12,
      personid == 31928 & is.na(v604) ~ 1,
      personid == 54271 & is.na(v604) ~ 2,
      personid == 5953049 & is.na(v604) ~ 12,
      personid == 29544 & is.na(v604) ~ 2,
      personid == 5951573 & is.na(v604) ~ 2,
      personid == 54132 & is.na(v604) ~ 2,
      personid == 1394 & is.na(v604) ~ 13,
      personid == 11082 & is.na(v604) ~ 1,
      personid == 31676 & is.na(v604) ~ 2,
      personid == 22198 & is.na(v604) ~ 15,
      personid == 54199 & is.na(v604) ~ 2,
      personid == 35110 & is.na(v604) ~ 12,
      personid == 52024 & is.na(v604) ~ 4, 
      personid == 19283 ~ 30,
      personid == 25176 & v604 == 21 ~ 2,
      personid == 16335 & v604 == 3 ~ 5,
      personid == 5949831 & v604 == 21 ~ 2,
      personid == 4165 & v604 == 1 ~ 5,
      personid == 30313 & is.na(v604) ~ 13, 
      personid == 30323 & is.na(v604) ~ 12, 
      personid == 47972 & v604 == 4 ~ 13,
      personid == 24295 & is.na(v604) ~ 4, 
      personid == 3969 & v604 == 4 ~ 5,
      personid == 53818 & is.na(v604) ~ 16,
      personid == 21309 & is.na(v604) ~ 4, 
      personid == 55502 & v604 == 2 ~ 13,
      personid == 19883 ~ 12,
      personid == 24268 & is.na(v604) ~ 15, 
      personid == 12265 & is.na(v604) ~ 18, 
      personid == 54268 & v604 == 9 ~ 30,
      personid == 29664 & v604 == 1 ~ 2,
      personid == 35638 & is.na(v604) ~ 10,
      personid == 15089 & v604 == 1 ~ 2,  
      personid == 28122 & v604 == 12 ~ 1, 
      personid == 54271 & v604 == 2 ~ 16,
      personid == 54849 & v604 == 1 ~ 21,
      personid == 19576 & v604 == 1 ~ 16,
      personid == 51077 & v604 == 2 ~ 16,
      personid == 54132 & v604 == 2 & v614b == 14400 ~ 12,
      personid == 54191 & is.na(v604) ~ 8,
      personid == 26202 & is.na(v604) ~ 16,
      personid == 19438 & v604 == 16 ~ 18, 
      personid == 34458 & v604 == 13 ~ 7,
      personid == 25929 & v604 == 7 ~ 3,
      personid == 12062 & is.na(v604) ~ 2,
      personid == 60249 & v604 == 96 ~ 16,
      personid == 9607 & v604 == 2 ~ 28, 
      personid == 59821 & is.na(v604) ~ 2,
      personid == 5952200 & v604 == 13 ~ 3,
      personid == 12487 & is.na(v604) ~ 12,
      personid == 17644 & v604 == 1 ~ 20,
      personid == 25176 & v604 == 2 ~ 21,
      personid == 51957 & v604 == 2 ~ 16, 
      personid == 55501 & v604 == 26 ~ 15,
      personid == 54153 & v604 == 2 ~ 6, 
      personid == 59821 & v604 == 2 ~ 23,
      TRUE ~ v604
    )
  )

tbl19 <- tbl19 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(personid, "-", v604)
  ) %>%
  filter(
      !disease_id %in%  c("23262-2", "7960-2")
  )

tbl17 <- tbl17 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(personid, "-", v604)
  )

missing_outpatients <- anti_join(
  tbl19, 
  tbl17, 
  by = "disease_id"
)

rm(missing_outpatients)

s6b3_update <- read.xlsx("chronic_outpatient_costs 14 Feb edited.xlsx")

s6b3_update <- s6b3_update %>%
  filter(!is.na(disease_id))

tbl19 <- tbl19 %>%
  mutate(disease_id = paste0(personid, "-", v604)) %>%
  rows_update(
    s6b3_update,
    unmatched = "ignore",
    by = "disease_id"
  )

tbl19 <- tbl19 %>%
  mutate(
    v614k = rowSums(across(v614a:v614j), na.rm = TRUE)
  )

tbl19_clean <- tbl19 %>%
  arrange(disease_id) %>%
  select(-hhld)

section6b3_clean <- section6b3 %>%
  arrange(disease_id) %>%
  select(-hhld)

tbl19_clean <- tbl19_clean[, names(section6b3_clean)]

all.equal(tbl19_clean, section6b3_clean)

rm(tbl19_clean, section6b3_clean)

############################################ SECTION 6.2.4 (TABLE 20) #############################################

tbl20 <- read_dta("OOPS_Rawdata_2026_03_17/tbl20.dta")

tbl20 <- tbl20 %>%
  mutate(
    v604 = case_when(
      personid == 20655 & is.na(v604) ~ 16,
      personid == 35802 & is.na(v604) ~ 13, 
      personid == 54033 & is.na(v604) ~ 3,
      personid == 54116 & is.na(v604) ~ 6,
      TRUE ~ as.numeric(v604)
    ) 
  ) 

tbl20 <- tbl20 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(personid, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(v101))

tbl20 <- tbl20 %>%
  mutate(
    v604 = case_when(
      personid == 53541 ~ 2, 
      personid == 3758 ~ 12, 
      personid == 53999 ~ 2,
      personid == 54002 ~ 3, 
      personid == 31913 ~ 18, 
      personid == 35638 & v604 == 2 ~ 10,
      personid == 10840 ~ 7, 
      personid == 9403 ~ 6, 
      personid == 10599 & v604 == 4 ~ 12, 
      personid == 18291 ~ 1, 
      personid == 51077 ~ 16,
      personid == 1321 ~ 18,
      personid == 26524 ~ 2,
      personid == 22198 ~ 13,
      personid == 35583 ~ 21, 
      personid == 35603 ~ 12,
      personid == 5950615 ~ 15,    
      personid == 52310 ~ 2, 
      personid == 52331 ~ 2,
      personid == 52285 ~ 13, 
      personid == 52292 ~ 16, 
      personid == 52294 ~ 14,
      personid == 11225 ~ 2,
      personid == 55501 & v604 == 26 ~ 15,
      personid == 54153 & v604 == 2 ~ 6, 
      TRUE ~ v604
    )
  )

tbl20 <- tbl20 %>%
  mutate(
    disease_id = paste0(personid, "-", v604)
  )

missing_inpatients <- anti_join(
  tbl20, 
  tbl17, 
  by = "disease_id"
)

rm(missing_inpatients)

s6b4_updates <- read.xlsx("chronic_inpatient_costs Chirag 10 Feb.xlsx")

s6b4_updates <- s6b4_updates %>%
  filter(!is.na(disease_id)) 

tbl20$v618b2 <- NA_real_

tbl20 <- tbl20 %>%
  rows_update(
    s6b4_updates,
    by = "disease_id",
    unmatched = "ignore"
  ) %>%
  select(-disease_id)

tbl20 <- tbl20 %>%
  mutate(
    v618k = rowSums(across(v618a:v618j), na.rm = TRUE)
  )

tbl20_clean <- tbl20 %>%
  select(-hhld) %>%
  mutate(disease_id = paste0(personid, "-", v604)) %>%
  arrange(disease_id)

section6b4_clean <- section6b4 %>%
  select(-hhld) %>%
  mutate(disease_id = paste0(personid, "-", v604)) %>%
  arrange(disease_id)

tbl20_clean <- tbl20_clean[, names(section6b4_clean)]

all.equal(section6b4_clean, tbl20_clean)

rm(tbl20_clean, section6b4_clean)

############################################ SECTION 6.2.5 (TABLE 21) #############################################

tbl21 <- read_dta("OOPS_Rawdata_2026_03_17/tbl21.dta")

tbl21 <- tbl21 %>%
  mutate(
    v622 = case_when(
      v624 == "" ~ 2,
      is.na(personid1) & is.na(v623) ~ 2,
      TRUE ~ v622
    ),
    across(
      c(v623:v6263),
      ~ if_else(v622 == 2, NA, .x)
    ),
    v624 = case_when(
      str_detect(v624, "96") ~ "3",
      v624 %in% c("MUMMY KO HOSPITAL JADA, 3", "3, NIROGI", "3, , BIRAMI") ~ "3",
      TRUE ~ v624
    )
  )

tbl21 <- tbl21 %>%
  mutate(
    across(
      c(personid1:v6263),
      ~ if_else(v622 == 2, NA, .x)
    )
  ) %>%
  group_by(personid) %>%
  filter(!(v622 == 2 & any(v622 == 1))) %>%
  ungroup()

tbl21_clean <- tbl21 %>%
  filter(personid %in% section6b5$personid) %>%
  arrange(personid) %>%
  select(-hhld)

section6b5_clean <- section6b5 %>%
  arrange(personid) %>%
  select(-hhld)

tbl21_clean <- tbl21_clean[, names(section6b5_clean)]

all.equal(section6b5_clean, tbl21_clean)

rm(tbl21_clean, section6b5_clean)

############################################ SECTION 6.3.1 (TABLE 22) #############################################

tbl22 <- read_dta("OOPS_Rawdata_2026_03_17/tbl22.dta")

cols_after_v629 <- names(tbl22)[(match("v629", names(tbl22)) + 1):ncol(tbl22)]

tbl22 <- tbl22 %>%
  mutate(
    across(
      all_of(cols_after_v629),
      ~ ifelse(v629 == 2 & !is.na(v630), NA, .)
    ),
    disease_id = paste0(personid, "-", v630)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() 

tbl22 <- tbl22 %>%
  mutate(
    v630 = case_when(
      personid == 14238 ~ 30,
      personid == 20356 ~ 14,
      personid == 24940 ~ 15,
      personid == 27401 ~ 6,
      personid == 28495 & v630 == 31 ~ 17,
      personid == 34333 ~ 20,
      personid == 47002 ~ 12,
      personid == 53800 ~ 22, 
      personid == 54346 ~ 22,
      personid == 54641 ~ 36,
      personid == 56693 ~ 15,
      personid == 59916 ~ 41,
      personid == 9767 ~ 25,
      personid == 24627 ~ 15,
      personid == 24940 ~ 17,
      personid == 3398 ~ 19,
      v630 == 96 ~ 19,
      TRUE ~ v630
    )
  ) 

s6c1_add <- read.xlsx(
  "misc/s6c1_add.xlsx",
  detectDates = TRUE
)

s6c1_add <- s6c1_add %>%
  mutate(
    v630a = as.character(v630a), 
    v631a = as.character(v631a),
    v631b = as.character(v631b)
  )

for (i in setdiff(1:ncol(s6c1_add), c(11, 15, 16, 17, 20, 26, 27, 28, 32, 33, 38))) {
  s6c1_add[[i]] <- as.numeric(gsub("[^0-9]", "", s6c1_add[[i]]))
}

tbl22 <- tbl22 %>%
  rows_upsert(s6c1_add, by = "personid")

tbl22 <- tbl22 %>%
  filter(personid %in% tbl03$personid) %>%
  mutate(
  across(
    v630a:disease_id,
    ~ if_else(is.na(v630), NA, .x)
  ),
  v629 = case_when(
    is.na(v630) ~ 2, 
    TRUE ~ 1
  )
)

tbl22 <- tbl22 %>%
  mutate(
    disease_id = paste0(personid, "-", v630)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disease_id)

tbl22_clean <- tbl22 %>%
  select(-hhld) %>%
  mutate(disease_id = paste0(personid, "-", v630)) %>%
  arrange(disease_id)

section6c1_clean <- section6c1 %>%
  select(-hhld) %>%
  mutate(disease_id = paste0(personid, "-", v630)) %>%
  arrange(disease_id)

tbl22_clean <- tbl22_clean[, names(section6c1_clean)]

all.equal(section6c1_clean, tbl22_clean)

rm(tbl22_clean, section6c1_clean)

############################################ SECTION 6.3.2 (TABLE 23) #############################################

tbl23 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl23.dta")

tbl22 <- tbl22 %>%
  group_by(personid) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

tbl23 <- tbl23 %>%
  group_by(personid) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

tbl23 <- tbl23 %>%
  left_join(
    tbl22 %>% select(personid, row_id, v630),
    by = c("personid", "row_id"),
    suffix = c("_6c2", "_6c1")
  ) %>%
  filter(!is.na(v630_6c1)) %>%
  rename(v630 = v630_6c1) %>%
  select(-v630_6c2)

tbl22 <- tbl22 %>%
  select(-row_id)

s6c2_add <- read.xlsx("misc/s6c2_add.xlsx")

s6c2_add <- s6c2_add %>%
  select(-v629)

for (i in setdiff(1:ncol(s6c2_add), c(10, 16, 19, 20, 22, 24, 25, 27))) {
  s6c2_add[[i]] <- as.numeric(gsub("[^0-9]", "", s6c2_add[[i]]))
}

tbl23 <- tbl23 %>%
  rows_append(s6c2_add) %>%
  filter(personid %in% tbl22$personid)

tbl23 <- tbl23 %>%
  mutate(
    disease_id = paste0(personid, "-", v630)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disease_id)

tbl23_clean <- tbl23 %>%
  select(-hhld) %>%
  mutate(disease_id = paste0(personid, "-", v630)) %>%
  arrange(disease_id)

section6c2_clean <- section6c2 %>%
  select(-hhld) %>%
  mutate(disease_id = paste0(personid, "-", v630)) %>%
  arrange(disease_id)

tbl23_clean <- tbl23_clean[, names(section6c2_clean)]

all.equal(section6c2_clean, tbl23_clean)

rm(section6c2_clean, tbl23_clean)

############################################ SECTION 6.3.3 (TABLE 24) #############################################

tbl24 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl24.dta")

s6c3_missing <- read.xlsx("/Users/sobaakun/NHIPsurvey/s6c3_missing.xlsx")

for (i in setdiff(1:ncol(s6c3_missing), c(10, 15, 18:32))) {
  s6c3_missing[[i]] <- as.numeric(gsub("[^0-9]", "", s6c3_missing[[i]]))
}

s6c3_missing <- s6c3_missing %>%
  mutate(
    v649d = as.character(v649d)
  )

tbl24 <- tbl24 %>%
  rows_append(s6c3_missing) %>%
  filter(personid %in% tbl22$personid)

tbl24_clean <- tbl24 %>%
  arrange(personid) %>%
  select(-hhld) 

section6c3_clean <- section6c3 %>%
  arrange(personid) %>%
  select(-hhld)

tbl24_clean <- tbl24_clean[, names(section6c3_clean)]

all.equal(section6c3_clean, tbl24_clean)

rm(section6c3_clean, tbl24_clean)

############################################ SECTION 6.3.4 (TABLE 25) #############################################

tbl25 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl25.dta")

tbl25 <- tbl25 %>%
  mutate(
    v630 = case_when(
      personid == 777 ~ 8,
      personid == 3421 ~ 22,
      personid == 8150 ~ 19,
      personid == 8427 ~ 22,
      personid == 9616 ~ 41,
      personid == 16352 ~ 2,
      personid == 17818 ~ 1, 
      personid == 18176 ~ 15,
      personid == 19046 ~ 37,
      personid == 19684 ~ 22, 
      personid == 20888 ~ 6,
      personid == 24132 ~ 32,
      personid == 24286 ~ 10, 
      personid == 25246 ~ 41, 
      personid == 25322 ~ 25, 
      personid == 25356 ~ 24, 
      personid == 27238 ~ 28,
      personid == 27383 ~ 30,
      personid == 27484 ~ 10,
      personid == 27815 ~ 37,
      personid == 33458 ~ 9,
      personid == 38132 ~ 38,
      personid == 38711 ~ 32,
      personid == 47193 ~ 19,
      personid == 48072 ~ 22, 
      personid == 53953 ~ 19,
      personid == 55502 ~ 31,
      personid == 55737 ~ 22,
      personid == 59527 ~ 41,
      personid == 59529 ~ 22, 
      personid == 15320 ~ 36,
      TRUE ~ v630
    ),
    v630 = as.numeric(v630)
  ) %>%
  filter(
      !personid %in% c(
        "5093", "7630", "8027", "10459", "12116",
        "12256", "14617", "15266", "16818", "16821", 
        "16866", "17880", "19067", "19977", "25665", 
        "28030", "35113", "43017", "52897", "53896",
        "58823", "5949841", "5952193", "5952193", 
        "5952955"
      )
  )

tbl22 <- tbl22 %>%
  mutate(
    disease_id = paste0(personid, "-", v630)
  )

tbl25 <- tbl25 %>%
  mutate(
    v630 = as.numeric(v630),
    disease_id = paste0(personid, "-", v630)
  )

missing_acute <- anti_join(
  tbl25, 
  tbl22, 
  by = "disease_id"
)

missing_acute <- missing_acute %>%
  dplyr::left_join(
    tbl22 %>%
      dplyr::select(personid, v630) %>%
      dplyr::rename(v630_from_6c1 = v630),
    by = "personid"
  )

missing_acute <- missing_acute %>%
  select(personid, v630, v630_from_6c1)

v630_replacement <- tbl22 %>%
  select(personid, v630) %>%
  rename(v630_from_6c1 = v630) %>%
  semi_join(missing_acute, by = "personid")

tbl25 <- tbl25 %>%
  left_join(v630_replacement, by = "personid") %>%
  mutate(
    v630 = if_else(
      personid %in% missing_acute$personid,
      v630_from_6c1,
      v630
    )
  ) %>%
  select(-v630_from_6c1)

tbl22 <- tbl22 %>%
  mutate(
    disease_id = paste0(personid, "-", v630)
  )

tbl25 <- tbl25 %>%
  mutate(
    v630 = as.numeric(v630),
    disease_id = paste0(personid, "-", v630)
  )

missing_acute <- anti_join(
  tbl25, 
  tbl22, 
  by = "disease_id"
)

rm(missing_acute, v630_replacement)

tbl25 <- tbl25 %>%
  select(-disease_id)

s6c4_missing <- read.xlsx("misc/s6c4_missing.xlsx")

for (i in setdiff(1:ncol(s6c4_missing), c(10, 16))) {
  s6c4_missing[[i]] <- as.numeric(gsub("[^0-9]", "", s6c4_missing[[i]]))
}

tbl25 <- tbl25 %>%
  rows_append(s6c4_missing)

tbl25 <- tbl25 %>%
  filter(personid %in% tbl22$personid)

tbl25 <- tbl25 %>%
  mutate(
  disease_id = paste0(personid, "-", v630)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disease_id)

tbl25 <- tbl25 %>%
  mutate(
    v651k = rowSums(across(v651a:v651j), na.rm = TRUE)
  )

tbl25_clean <- tbl25 %>%
  mutate(disease_id = paste0(personid, "-", v630)) %>%
  arrange(disease_id) %>%
  select(-hhld)

section6c4_clean <- section6c4 %>%
  mutate(disease_id = paste0(personid, "-", v630)) %>%
  arrange(disease_id) %>%
  select(-hhld)

tbl25_clean <- tbl25_clean[, names(section6c4_clean)]

all.equal(section6c4_clean, tbl25_clean)

rm(section6c4_clean, tbl25_clean)

############################################ SECTION 6.3.5 (TABLE 26) #############################################

tbl26 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl26.dta")

tbl26 <- tbl26 %>%
  mutate(
    v655 = case_when(
      v657 == "" ~ 2, 
      TRUE ~ 1
    ), 
    across(
      c(v656:v661), 
      ~ if_else(v655 == 2, NA, .x)
    ),
    v656 = case_when(
      v655 == 1 & is.na(v656) ~ 1, 
      TRUE ~ v656
    ), 
    v658 = case_when(
      v655 == 1 & is.na(v658) ~ 3,
      TRUE ~ v658
    ), 
    v659 = case_when(
      v655 == 1 & is.na(v659) ~ median(v659, na.rm = TRUE),
      TRUE ~ v659
    ), 
    v660 = case_when(
      v655 == 1 & is.na(v660) ~ 4,
      TRUE ~ v660
    ), 
    v661 = case_when(
      v655 == 1 & is.na(v661) ~ median(v661, na.rm = TRUE), 
      TRUE ~ v661
    )
  )
  

tbl26_clean <- tbl26 %>%
  arrange(personid) %>%
  select(-hhld)

section6c5_clean <- section6c5 %>%
  arrange(personid) %>%
  select(-hhld)

tbl26_clean <- tbl26_clean[, names(tbl26_clean)]

all.equal(section6c5_clean, tbl26_clean)

rm(section6c5_clean, tbl26_clean)

############################################ SECTION 7 (TABLE 28) #############################################

tbl28 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl28.dta")

tbl28 <- tbl28 %>%
  mutate(
    ward = as.numeric(gsub("[^0-9]", "", ward)),
    v702 = as.numeric(gsub("[^0-9]", "", v702)), 
    v703 = as.numeric(gsub("[^0-9]", "", v703)),
    v721 = as.numeric(gsub("[^0-9]", "", v721)),
    v714a = as.numeric(gsub("[^0-9]", "", v714a)),
  ) %>%
  filter(
    personid != 56798
  )

tbl28 <- tbl28 %>%
  mutate(
    v708 = case_when(
      is.na(v708) &
      v709a %in% c(
        "PUROHIT"
      ) ~ 2,
      is.na(v708) &
      v709a %in% c(
        "QUALITY ASSURANCE ENGINEER", "AD", "FCHV KO TALIM LIYERA BHATTA"
      ) ~ 3,
      is.na(v708) &
      v709a %in% c(
        "LOAN KO KAMM MA GARNI", "JUNIOR ASSISTANT IN FINANCE DEPARTMENT", "RECEPTIONIST IN FINANCE",
        "MARKETER OR MONEY COLLECTOR"
      ) ~ 4,
      is.na(v708) &
      v709a %in% c(
        "BYAPAR", "SALES - KIRANA", "KHAJA PASAL", "CHEF", "FNB CAPTAIN(DINING HALL AND BAR)"
      ) ~ 5,     
      is.na(v708) &
      v709a %in% c(
        "AGRICULTURAL ACTIVITIES", "KRISHAK KHETIPATI GARNE", "KRISHI KAM GAREKO", "KRISHI", "KHETI"
      ) ~ 6, 
      is.na(v708) &
      v709a %in% c(
        "AAFNAI AUTA RIKSHA CHALAUNE"
      ) ~ 8,
      is.na(v708) &
      v709a %in% c(
        "PANI GHATTA CHALAUNA SAHAYOG GARNE", "KIRANA STORE MA SAGAUNE", "SAGAUNA",
        "CONDUCTOR", "PACKING", "WAITRESS", "VB", "AS"
      ) ~ 9,
      TRUE ~ v708    
    )
  )

tbl28 <- tbl28 %>%
  mutate(
    personid = case_when(
      id == 2946 ~ 11256, 
      TRUE ~ personid 
    ), 
    v101 = case_when(
      id == 2946 ~ 4, 
      TRUE ~ v101
    )
  )

s7_qualified <- tbl03 %>%
  filter(v104a >= 10)

tbl28 <- tbl28 %>%
  filter(personid %in% s7_qualified$personid)

ssf_respondents <- tbl01 %>%
  filter(enrollment %in% c(3, 4)) %>%
  mutate(
    ssf_id = paste0(id, "-", respondent)
  )

ssf_respondent_id <- tbl02 %>%
  filter(enrollment %in% c(3, 4)) %>%
  mutate(
    ssf_id = paste0(id, "-", v102)
  )

ssf_respondent_id <- ssf_respondent_id %>%
  filter(ssf_id %in% ssf_respondents$ssf_id)

tbl28 <- tbl28 %>%
  mutate(
    personid = case_when(
      personid == 53800 ~ 53799,
      personid == 9939 ~ 9936,
      personid == 11256 ~ 11255,
      personid == 5949528 ~ 5949530,
      personid == 5949530 & v101 == 3 ~ 5949528,
      TRUE ~ personid
    ),
    v101 = case_when(
      personid == 53799 ~ 1,
      personid == 9936 ~ 1,
      personid == 11255 ~ 3, 
      personid == 5949528 ~ 1, 
      personid == 5949530 ~ 3,
      TRUE ~ v101
    )
  )

s7_missing_import <- read.xlsx("misc/s7_missing.xlsx")

for (i in setdiff(1:ncol(s7_missing_import), c(11, 20, 38))) {
  s7_missing_import[[i]] <- as.numeric(gsub("[^0-9]", "", s7_missing_import[[i]]))
}

tbl28 <- bind_rows(tbl28, s7_missing_import)

s7_missing_ssf_respondents <- ssf_respondent_id %>%
  filter(!personid %in% tbl28$personid)

s7_missing <- s7_qualified %>%
  filter(!personid %in% tbl28$personid)

rm(s7_missing, s7_missing_ssf_respondents, s7_qualified, s7_missing_import)

ssf_s7 <- read.xlsx("misc/ssf_s7.xlsx")

for (i in setdiff(1:ncol(ssf_s7), c(10, 20, 38))) {
  ssf_s7[[i]] <- as.numeric(gsub("[^0-9]", "", ssf_s7[[i]]))
}

tbl28 <- tbl28 %>%
  rows_update(
    ssf_s7, 
    by = "personid",
    unmatched = "ignore"
  ) %>%
  mutate(
    across(
      c(v702:v705), 
      ~ if_else(is.na(v708) & .x == 2, NA_real_, .x)
    ),
    across(
      v718:v721,
      ~ if_else(!is.na(v708), NA_real_, .x)
    ),
    across(
      c(v706:v717, v709, v714),
      ~ if_else(
        v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2,
        NA,
        .x
      )
    ), 
    v702 = case_when(
      v703 == 1 ~ 2,
      v704 == 1 ~ 2,
      TRUE ~ v702
    ),
    v702 = case_when(
      v702 == 2 & v703 == 2 & v704 == 2 & !is.na(v708) ~ 1,
      TRUE ~ v702
    ),
    v703 = case_when(
      v702 == 1 ~ NA_real_,
      v704 == 1 ~ 2, 
      TRUE ~ v703
    ), 
    v704 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      TRUE ~ v704
    ), 
    v705 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v705 
    ),
    v706 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v706 
    ),
    v707 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v707 
    ),
    v709 = case_when(
      enrollment %in% c(3, 4) & !is.na(v708) ~ employer_sector,
      TRUE ~ v709
    ),
    v710 = if_else(
      !is.na(v708) & is.na(v710),
      sample(1:3, n(), replace = TRUE),
      v710
    ),
    v711 = case_when(
      !is.na(v708) & enrollment == 3 ~ 1, 
      !is.na(v708) & enrollment == 4 ~ 2, 
      TRUE ~ v711
    ),
    v712 = case_when(
      !is.na(v708) & is.na(v712) ~ 2, 
      TRUE ~ v712
    ), 
    v713 = case_when(
      !is.na(v708) & is.na(v713) ~ 2, 
      TRUE ~ v713
    ),
    v714 = case_when(
      enrollment %in% c(3, 4) & !is.na(v708) ~ employer_sector,
      TRUE ~ v714
    ),
    v715 = case_when(
      !is.na(v708) & is.na(v715) & is.na(employer) ~ sample(1:8, n(), replace = TRUE), 
      TRUE ~ v715
    ),
    v716 = case_when(
      !is.na(v715) & !is.na(v708) & is.na(employer) & is.na(v716) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ v716
    ), 
    v717 = case_when(
      v716 == 2 & !is.na(v708) & is.na(employer) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ NA_real_
    ),
    v718 = case_when(
      v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2 & is.na(v718) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ NA_real_ 
    ),
    v719 = case_when(
      v718 == 2 & is.na(v719) ~ sample(1:2, n(), replace = TRUE),
      TRUE ~ NA_real_
    ),
    v720 = case_when(
      v718 == 1 & is.na(v720) ~ sample(1:11, n(), replace = TRUE), 
      TRUE ~ NA_real_
    ), 
    v721 = case_when(
      v719 == 2 & is.na(v721) ~ sample(1:2, n(), replace = TRUE),
      TRUE ~ NA_real_
    ), 
    v722 = case_when(
      v721 == 1 & is.na(v722) ~ sample(1:3, n(), replace = TRUE), 
      TRUE ~ NA_real_
    )
  )

rm(ssf_s7, ssf_respondents)

ssf <- tbl28 %>%
  filter(personid %in% ssf_respondent_id$personid)

rm(ssf)

tbl28_clean <- tbl28 %>%
  arrange(personid) %>%
  select(-hhld)

section7_clean <- section7 %>%
  arrange(personid) %>%
  select(-hhld)

tbl28_clean <- tbl28_clean[, names(section7_clean)]

all.equal(section7_clean, tbl28_clean)

rm(section7_clean, tbl28_clean)

############################################ SECTION 8 (TABLE 29) ############################################# 

wages <- read.xlsx("misc/wages.xlsx")
ssf_s8_missing <- read.xlsx("misc/ssf_s8_missing.xlsx")

tbl29 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl29.dta")

for (i in setdiff(1:ncol(ssf_s8_missing), c(10, 17, 34))) {
  ssf_s8_missing[[i]] <- as.numeric(gsub("[^0-9]", "", ssf_s8_missing[[i]]))
}

tbl29 <- tbl29 %>%
  rows_update(
      wages, 
      by = "personid",
      unmatched = "ignore"
  ) %>%
  rows_append(
    ssf_s8_missing
  ) %>%
  mutate(
    personid = case_when(
      personid == 5952053 ~ 5952054,
      TRUE ~ personid
    ),
    v101 = case_when(
      personid == 5952054 ~ 3, 
      TRUE ~ v101
    ),
    v802 = case_when(
      v802 == 2 & !is.na(v803) ~ 1,
      is.na(v803) & is.na(v803c) ~ 2,
      personid %in% c(14210, 51558) ~ 2,
      TRUE ~ v802
    ),
    v803c = case_when(

      is.na(v803c) & v803 %in% c(
        "HOTEL ADMIN HR", "1"
      ) ~ 1,

      is.na(v803c) & v803 %in% c(
        "BOARDING SCHOOL", "IT SAMBANDHI", "TEACHER", "BACHALAI PADAUNE", "0, TEACHER"
      ) ~ 2,

      is.na(v803c) & v803 %in% c(
        "IT SUPPORT", "LEKHANDASI", "SCHOOL MA PADHAUNE", "LEKHA ADHIKRITH",
        "PRASASANIK SEWA MA SAHAYOG, PRASASANIK SEWA"
      ) ~ 3,

      is.na(v803c) & v803 %in% c(
        "4", "BANK MA TELLER", "HALKARA COUNTER MA BASNE",
        "TOP QUALITY POULTRY FEED MA ACCOUNTING KO KAM GARNU VAYO",
        "GAGA AGENT", "WARD OFFICE MA", "NGO MA KAAM GARNE",
        "SARKARI OFFICE MA KAAM GARNEY", "RECEPTIONIST", "ADMIN",
        "COOPERATIVE EMPLOYEE", "FF, PROCUREMENT OFFICER",
        "MARKETING IN FINANCE, ALUMINUM RELATED WORK JHYAL, DHOKA BANAUNE KAAM"
      ) ~ 4,

      is.na(v803c) & v803 %in% c(
        "MANPOWER AGENT", "PUROHIT", "PASAL", "BEAUTICIAN", "MRKETING",
        "COLLECTION KO KAM", "GG", "HOUSE KEEPING",
        "HOTEL MA SAFE", "AAFNAI JOB LINK"
      ) ~ 5,

      is.na(v803c) & v803 %in% c(
        "THEKKPATTA", "DHUP BANAUNE", "PARLOUR MA KAM GARNE",
        "BIDI BANAUNE", "BIDI BANAUNE KAM", "NIRMAN SAMBANDHI",
        "GHAR BANAUNE MISTREE", "ALUMINUM KO KAM", "KHAPADA SILAUNE",
        "ELECTRICIAN", "WELDING AND MAINTENANCE. MILL MECHANICAL",
        "AUTO MECHANIC", "WIRING KO KAMM", "GHAR KO GARO LAGAUNE",
        "DHUP BATTI BANAUNE", "AC MECHANIC", "MISTRI"
      ) ~ 7,

      is.na(v803c) & v803 %in% c(
        "THREAD MAKING, MACHINE OPERATOR", "AAFNAI AUTO CHALAUNE",
        "JCV", "BUS DRIVER", "PENTAR KO KAM",
        "AMBULANCE DRIVER", "TRIPPER DRIVER", "DRIVER"
      ) ~ 8,

      is.na(v803c) & v803 %in% c(
        "JYALA MA KHETIPATI SAMBANDHI KAAM GARNE",
        "ARU KO KHET MA DHAN ROPNE", "RGH", "LABOUR,MISTRI",
        "DHAN ROPNE KAM,", "JYALADRI", "HOUSE KEEPING",
        "ETA BHATAMA LABOUR KAM", "DHAN ROPNE", "DHAN GODNE,ROPNE",
        "DHAN ROPNE, GODNE", "KHETI KISANU", "JYAMI GHAR BANAUNE",
        "GARI RAHEKO", "KEHTI PATI", "GARIRHEKO", "JYALA MAJHDOORI",
        "DHAN GODNE, ROPNE", "LABOR", "JHYAL DHOKA BANAUNE SAHAYOG",
        "PANTIN", "PANCHKANYA PROFILE MA ALUMINUM SAHAYOGI KAM",
        "DAURA BOKNE/KATNE KAM, KRISHI KAMMA DAILY JYALADARI KAM GARNE.",
        "BHARI BOKNE KAM HARU, JYALADARI KAM",
        "GARO LAUNE DHUNGA MATO BOKNE SABAI KAAM, JYALA MA KHETIPATI SAMBANDHI SABAI KAAM",
        "BHAWAN NIRMAAN SAMBANDHI KAAM HARU DHUNGA MATO KO, KHANNE, GODMEL AADI BAARIKO SABAI KAAM GARNE",
        "GHARMA COLOUR LAGAUNEE KAM GARNEE, KHETIPATI MA JYALA KO KAM",
        "KHETIPATI MA JYALA, BATO BANAUNE KAM",
        "SADAKKO KULO SAFA GARNE KAM, DAURA KATNE, BARI KHANNE,JASTO KRISHI KAMMA",
        "FURNITUREKO SAMANHARU  BANAUNE, BHAWAN /BATO AADI BANAUNE",
        "MADHAV POUDEL JI WAS IN FOREIGN EMPLOYMENT AND HAS RETURNED TO NEPAL TWO MONTHS AGO.AND EVEN NOW HE IS PLANNING WHICH COUNTRY TO GO IN ORDER TO CONTINUE HIS FOREIGN  EMPLOYMENT AND HE ALSO INFORMED THAT HE USED TO SEND AN AVERAGE OF 25 THOUSAND RUPEES PER MONTH WHILE HE WAS IN FOREIGN EMPLOYMENT."
      ) ~ 9,

      TRUE ~ v803c
    )
  )

rm(ssf_s8_missing, wages)

tbl29 <- tbl29 %>%
  mutate(
    v802 = case_when(
      v802 == 2 & !is.na(v803) ~ 1,
      is.na(v803) & is.na(v803c) ~ 2,
      personid %in% c(14210, 51558) ~ 2,
      TRUE ~ v802
    ),
    across(
      v803:employer_size,
      ~ if_else(personid %in% c(8036, 7468, 7176, 14210, 5949841, 1350, 5949840, 10524, 8036, 25146), NA, .x)
    ), 
    v803 = case_when(
      personid == 9985 ~ 6, 
      personid == 10643 ~ 3, 
      personid == 27445 ~ 9,
      TRUE ~ v803
    )
  ) %>% 
  group_by(v803c) %>%
  mutate(
    v804 = case_when(
      is.na(v804) & v808a > 0 ~ 2,
      is.na(v804) & v810a > 0 ~ 3,
      TRUE ~ v804
    ),
    v805 = if_else(v804 == 2, NA_real_, v805),
    v806 = if_else(v804 == 2, NA_real_, v806),
    v807 = if_else(v804 == 2, NA_real_, v807),

    v805_trim_mean = mean(v805[v805 <= 365], na.rm = TRUE),
    v805 = if_else(
      v805 > 365 & !is.nan(v805_trim_mean),
      round(v805_trim_mean),
      v805
    ),
    v808a = case_when(
      v804 == 2 &
      (is.na(v808a) | v808a == 0) &
      v806 > 0 ~ v806,
      TRUE ~ v808a
    )
  ) %>%
  ungroup() %>%
  select(-v805_trim_mean)

tbl29 <- tbl29 %>%
  mutate(
    v802 = case_when(
      is.na(v805) & !is.na(v808a) & v808a > 0 ~ 2,
      TRUE ~ v802
    ),
    v803 = case_when(
      is.na(v803) & !is.na(v803c) ~ v803c, 
      TRUE ~ v803
    ),
    v803c = case_when(
      !is.na(v803) & is.na(v803c) ~ v803,
      TRUE ~ v803c
    ),
    v805 = if_else(v804 == 2, NA_real_, v805),
    v806 = if_else(v804 == 2, NA_real_, v806),
    v807 = if_else(v804 == 2, NA_real_, v807),
    v806   = if_else(personid == 9829, 1000, v806),
    v808a  = if_else(personid == 9829, 84000, v808a),
    v806   = if_else(personid == 29620, 1000, v806),
    v808a  = if_else(personid == 29620, 128000, v808a),
    v806   = if_else(personid == 8907, 1200, v806),
    v808a  = if_else(personid == 8907, 150000, v808a),
    v806   = if_else(personid == 8953, 700, v806),
    v808a  = if_else(personid == 8953, 90000, v808a),
    v806   = if_else(personid == 58151, 500, v806),
    v808a  = if_else(personid == 58151, 64000, v808a),
    v808a = if_else(
      v806 >= 10000 & (is.na(v808a) | v808a == 0), 
      v806, 
      v808a
    ),
    v804 = if_else(
      v806 == v808a, 
      2, 
      v804
    ),
    v805 = if_else(
      v806 == v808a, 
      NA_real_, 
      v805
    ),
    v807 = if_else(
      v806 == v808a, 
      NA_real_, 
      v807
    ),
    v806 = if_else(
      v806 == v808a, 
      NA_real_,
      v806
    ), 
    v804 = case_when(
      is.na(v804) & v803c == 10 ~ 2,
      is.na(v804) & v803c == 1 ~ 2, 
      TRUE ~ v804
    )
  )

tbl29 <- tbl29 %>%
  group_by(v803c) %>%
  mutate(
    v808a = if_else(
      v808a > 4200000,
      round(mean(v808a[v808a <= 4200000], na.rm = TRUE)),
      v808a
    ),
    v808c = if_else(
      v808c == 750040000, 
      40000, 
      v808c
    ),
    v808a = case_when(
      v803b %in% c(
        "LEBAR KO KAM GARNE GAS KO CYLINDER GADI MA  RAKHANE", 
        "SENA", 
        "GADI CHALAUNEE", 
        "SCHOOL MA ACCOUNT KO KAM", 
        "DHAN KO BORA BOKNE",
        "MALPOT MA LEKHAPADI KO KAM"
      ) ~ v808a/10,
      TRUE ~ v808a
    )
  ) %>%
  ungroup()

tbl29 <- tbl29 %>%
  group_by(v803c, province) %>%
  mutate(
    v808a = case_when(
      v803c == 4 & v808a > 300000 ~ round(mean(v808a[v808a <= 300000], na.rm = TRUE)),
      v803c == 7 & v808a > 600000 ~ round(mean(v808a[v808a <= 600000], na.rm = TRUE)),
      v803c == 9 & v808a > 300000 ~ round(mean(v808a[v808a <= 300000], na.rm = TRUE)), 
      v803c == 8 & v808a > 1200000 ~ round(mean(v808a[v808a <= 1200000], na.rm = TRUE)), 
      v803c == 2 & v808a > 2400000 ~ round(mean(v808a[v808a <= 2400000], na.rm = TRUE)),
      v803c == 5 & v808a > 600000 ~ round(mean(v808a[v808a <= 600000], na.rm = TRUE)), 
      v803c == 6 & v808a > 600000 ~ round(mean(v808a[v808a <= 600000], na.rm = TRUE)),
      v803c == 3 & v808a > 1300000 ~ round(mean(v808a[v808a <= 1300000], na.rm = TRUE)),
      TRUE ~ v808a
    ),
    v808b = case_when(
      v803c == 3 & v808b > 30000 ~ round(mean(v808b[v808b <= 30000], na.rm = TRUE)), 
      TRUE ~ v808b
    ),
    v808c = case_when(
      v803c == 4 & v808c > 50000 ~ round(mean(v808c[v808c <= 50000], na.rm = TRUE)), 
      v803c == 1 & v808c > 90000 ~ round(mean(v808c[v808c <= 90000], na.rm = TRUE)), 
      v803c == 8 & v808c > 50000 ~ round(mean(v808c[v808c <= 50000], na.rm = TRUE)),
      v803c == 5 & v808c > 40000 ~ round(mean(v808c[v808c <= 40000], na.rm = TRUE)), 
      v803c == 3 & v808c > 40000 ~ round(mean(v808c[v808c <= 40000], na.rm = TRUE)),
      TRUE ~ v808c
    ), 
    v808e = case_when(
      v803c == 9 & v808e > 50000 ~ round(mean(v808e[v808e <= 50000], na.rm = TRUE)), 
      v803c == 1 & v808e > 50000 ~ round(mean(v808e[v808e <= 50000], na.rm = TRUE)), 
      v803c == 2 & v808e > 30000 ~ round(mean(v808e[v808e <= 30000], na.rm = TRUE)),
      TRUE ~ v808e
    )
  ) %>%
  ungroup()

ssf_s8 <- tbl29 %>%
  filter(personid %in% ssf_respondent_id$personid)
  
ssf_s8 <- ssf_s8 %>%
  left_join(
    tbl28 %>% select(personid, v708),
    by = "personid"
  ) %>%
  mutate(
    v803 = coalesce(v708, v803)
  ) %>%
  select(-v708)

ssf_s8 <- ssf_s8 %>%
  group_by(province, v803) %>%
  mutate(
    v808a = case_when(
    is.na(v808a) ~ round(mean(v808a, na.rm = TRUE), -2),
    TRUE ~ v808a
    )
  ) %>%
  ungroup() %>%
  mutate(
    across(
      c(v808a:v810b), 
      ~ if_else(!is.na(v805) & !is.na(v806), NA_real_, .x)
    ),
    across(
      c(v805:v809), 
      ~ if_else(!is.na(v810a), NA_real_, .x)
    ),
    v808a = pmax(v808a, v808b, v808c, na.rm = TRUE),
    across(
      c(v805:v810b),
      ~ na_if(.x, 0)
    ),
    v804 = case_when(
      !is.na(v805) ~ 1, 
      !is.na(v808a) ~ 2, 
      !is.na(v810a) ~ 3,
      TRUE ~ v804
    )
  )

tbl29 <- tbl29 %>%
  rows_upsert(ssf_s8, by = "personid")

s8_missing <- read.xlsx("misc/s8_missing.xlsx")

for (i in setdiff(1:ncol(s8_missing), c(11, 16, 34))) { 
  s8_missing[[i]] <- as.numeric(gsub("[^0-9]", "", s8_missing[[i]]))
}

s8_missing$v803b <- as.character(s8_missing$v803b)

tbl29 <-tbl29 %>%
  rows_append(s8_missing)

s8_qualified <- tbl03 %>%
  filter(v104a >= 10)

tbl29 <- tbl29 %>%
  filter(personid %in% s8_qualified$personid) %>%
  mutate(
    across(
      c(v808a:v810b), 
      ~ if_else(!is.na(v805) & !is.na(v806), NA_real_, .x)
    ),
    across(
      c(v805:v809), 
      ~ if_else(!is.na(v810a), NA_real_, .x)
    ),
    v808a = pmax(v808a, v808b, v808c, na.rm = TRUE),
    across(
      c(v805:v810b),
      ~ na_if(.x, 0)
    ),
    v804 = case_when(
      !is.na(v805) ~ 1, 
      !is.na(v808a) ~ 2, 
      !is.na(v810a) ~ 3,
      TRUE ~ v804
    ),
    v802 = case_when(
      !is.na(v803) ~ 1,
      TRUE ~ 2 
    ), 
    across(
      c(v803:v810b),
      ~ if_else(v802 == 2, NA, .x)
    ),
    v802 = case_when(
      is.na(v804) ~ 2, 
      TRUE ~ v802
    ),
    across(
      c(v803:v810b),
      ~ if_else(is.na(v804), NA, .x)
    )
  )

wages <- tbl29 %>%
  filter(
    !is.na(v803)
  ) 
  
jobs <- tbl28 %>%
  filter(personid %in% wages$personid) 

jobs <- merge(
  jobs, 
  wages[, c("personid", "v803")],
  by = "personid"
)

jobs <- jobs %>%
  mutate(
    v803 = as.numeric(v803),
    v708 = case_when(
      is.na(v708) ~ v803, 
      TRUE ~ v708
    ),
    v702 = case_when(
      personid == 54165 ~ 1, 
      TRUE ~ v702
    ),
    v704 = case_when(
      personid == 60526 ~ 1,
      TRUE ~ v704
    )
  )

jobs <- jobs %>%
  mutate(
    v702 = case_when(
      is.na(v702) ~ 1, 
      v702 == 2 & v703 == 2 & v704 == 2 & is.na(v705) ~ 1, 
      v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2 ~ 1,
      TRUE ~ v702
    ),
    across(
      c(v703:v705),
      ~ if_else(v702 == 1, NA, .x)
    ), 
    across(
      c(v702:v705), 
      ~ if_else(is.na(v708) , 2, .x)
    ),
    across(
      v718:v721,
      ~ if_else(!is.na(v708), NA_real_, .x)
    ),
    across(
      c(v706:v717, v709, v714),
      ~ if_else(
        v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2,
        NA,
        .x
      )
    ), 
    v702 = case_when(
      v703 == 1 ~ 2,
      v704 == 1 ~ 2,
      TRUE ~ v702
    ),
    v702 = case_when(
      v702 == 2 & v703 == 2 & v704 == 2 & !is.na(v708) ~ 1,
      TRUE ~ v702
    ),
    v703 = case_when(
      v702 == 1 ~ NA_real_,
      v704 == 1 ~ 2, 
      TRUE ~ v703
    ), 
    v704 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      TRUE ~ v704
    ), 
    v705 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v705 
    ),
    v706 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v706 
    ),
    v707 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v707 
    ),
    v709 = case_when(
      enrollment %in% c(3, 4) & !is.na(v708) ~ employer_sector,
      TRUE ~ v709
    ),
    v710 = if_else(
      !is.na(v708) & is.na(v710),
      sample(1:3, n(), replace = TRUE),
      v710
    ),
    v711 = case_when(
      !is.na(v708) & enrollment == 3 ~ 1, 
      !is.na(v708) & enrollment == 4 ~ 2, 
      is.na(v711) ~ 2,
      TRUE ~ v711
    ),
    v712 = case_when(
      !is.na(v708) & is.na(v712) ~ 2, 
      TRUE ~ v712
    ), 
    v713 = case_when(
      !is.na(v708) & is.na(v713) ~ 2, 
      TRUE ~ v713
    ),
    v714 = case_when(
      enrollment %in% c(3, 4) & !is.na(v708) ~ employer_sector,
      is.na(v714) ~ sample(1:20, n(), replace = TRUE),
      TRUE ~ v714
    ),
    v715 = case_when(
      !is.na(v708) & is.na(v715) & is.na(employer) ~ sample(1:8, n(), replace = TRUE), 
      TRUE ~ v715
    ),
    v716 = case_when(
      !is.na(v715) & !is.na(v708) & is.na(employer) & is.na(v716) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ v716
    ), 
    v717 = case_when(
      v716 == 2 & !is.na(v708) & is.na(employer) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ NA_real_
    ),
    v718 = case_when(
      v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2 & is.na(v718) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ NA_real_ 
    ),
    v719 = case_when(
      v718 == 2 & is.na(v719) ~ sample(1:2, n(), replace = TRUE),
      TRUE ~ NA_real_
    ),
    v720 = case_when(
      v718 == 1 & is.na(v720) ~ sample(1:11, n(), replace = TRUE), 
      TRUE ~ NA_real_
    ), 
    v721 = case_when(
      v719 == 2 & is.na(v721) ~ sample(1:2, n(), replace = TRUE),
      TRUE ~ NA_real_
    ), 
    v722 = case_when(
      v721 == 1 & is.na(v722) ~ sample(1:3, n(), replace = TRUE), 
      TRUE ~ NA_real_
    )
  ) %>%
  select(-v803)

tbl28 <- tbl28 %>%
  rows_update(jobs, by = "personid") %>%
  mutate(
    v708 = case_when(
      !personid %in% jobs$personid ~ NA,
      TRUE ~ v708
    )
  )

tbl28 <- tbl28 %>%
  mutate(
    across(
      c(v702:v705), 
      ~ if_else(is.na(v708) & .x == 2, NA, .x)
    ),
    across(
      v718:v721,
      ~ if_else(!is.na(v708), NA, .x)
    ),
    across(
      c(v706:v717, v709, v714),
      ~ if_else(
        v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2,
        NA,
        .x
      )
    ), 
    v702 = case_when(
      v703 == 1 ~ 2,
      v704 == 1 ~ 2,
      TRUE ~ v702
    ),
    v702 = case_when(
      v702 == 2 & v703 == 2 & v704 == 2 & !is.na(v708) ~ 1,
      TRUE ~ v702
    ),
    v703 = case_when(
      v702 == 1 ~ NA_real_,
      v704 == 1 ~ 2, 
      TRUE ~ v703
    ), 
    v704 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      TRUE ~ v704
    ), 
    v705 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v705 
    ),
    v706 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v706 
    ),
    v707 = case_when(
      v702 == 1 ~ NA_real_, 
      v703 == 1 ~ NA_real_,
      v704 == 1 ~ NA_real_, 
      TRUE ~ v707 
    ),
    v709 = case_when(
      enrollment %in% c(3, 4) & !is.na(v708) ~ employer_sector,
      TRUE ~ v709
    ),
    v710 = if_else(
      !is.na(v708) & is.na(v710),
      sample(1:3, n(), replace = TRUE),
      v710
    ),
    v711 = case_when(
      !is.na(v708) & enrollment == 3 ~ 1, 
      !is.na(v708) & enrollment == 4 ~ 2, 
      TRUE ~ v711
    ),
    v712 = case_when(
      !is.na(v708) & is.na(v712) ~ 2, 
      TRUE ~ v712
    ), 
    v713 = case_when(
      !is.na(v708) & is.na(v713) ~ 2, 
      TRUE ~ v713
    ),
    v714 = case_when(
      enrollment %in% c(3, 4) & !is.na(v708) ~ employer_sector,
      TRUE ~ v714
    ),
    v715 = case_when(
      !is.na(v708) & is.na(v715) & is.na(employer) ~ sample(1:8, n(), replace = TRUE), 
      TRUE ~ v715
    ),
    v716 = case_when(
      !is.na(v715) & !is.na(v708) & is.na(employer) & is.na(v716) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ v716
    ), 
    v717 = case_when(
      v716 == 2 & !is.na(v708) & is.na(employer) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ NA_real_
    ),
    v718 = case_when(
      v702 == 2 & v703 == 2 & v704 == 2 & v705 == 2 & is.na(v718) ~ sample(1:2, n(), replace = TRUE), 
      TRUE ~ NA_real_ 
    ),
    v719 = case_when(
      v718 == 2 & is.na(v719) ~ sample(1:2, n(), replace = TRUE),
      TRUE ~ NA_real_
    ),
    v720 = case_when(
      v718 == 1 & is.na(v720) ~ sample(1:11, n(), replace = TRUE), 
      TRUE ~ NA_real_
    ), 
    v721 = case_when(
      v719 == 2 & is.na(v721) ~ sample(1:2, n(), replace = TRUE),
      TRUE ~ NA_real_
    ), 
    v722 = case_when(
      v721 == 1 & is.na(v722) ~ sample(1:3, n(), replace = TRUE), 
      TRUE ~ NA_real_
    )
  )

rm(jobs, wages, ssf_s8, s8_missing, s8_qualified)

tbl28 <- tbl28 %>%
  mutate(
    v708 = case_when(
      personid == 18223 ~ 6, 
      personid == 21500 ~ 3, 
      personid == 22235 ~ 9, 
      personid == 25994 ~ 3, 
      personid == 5953191 ~ 4,
      TRUE ~ v708
    )
  )

tbl29 <- tbl29 %>%
  mutate(
    v803 = case_when(
      personid == 18223 ~ 6, 
      personid == 21500 ~ 3, 
      personid == 22235 ~ 9, 
      personid == 25994 ~ 3, 
      personid == 5953191 ~ 4,
      TRUE ~ v803
    ), 
    v804 = case_when(
      personid == 18223 ~ 2, 
      personid == 21500 ~ 2, 
      personid == 22235 ~ 2, 
      personid == 25994 ~ 2, 
      personid == 5953191 ~ 2,
      TRUE ~ v804
    ), 
    v808a = case_when(
      personid == 18223 ~ 240000, 
      personid == 21500 ~ 260000, 
      personid == 22235 ~ 200000, 
      personid == 25994 ~ 180000, 
      personid == 5953191 ~ 270000,
      TRUE ~ v808a
    )
  )

tbl29_clean <- tbl29 %>%
  arrange(personid) %>%
  select(-hhld)

section8_clean <- section8 %>%
  arrange(personid) %>%
  select(-hhld)

tbl29_clean <- tbl29_clean[, names(section8_clean)]

all.equal(section8_clean, tbl29_clean)

rm(section8_clean, tbl29_clean)

############################################ SECTION 9.1 (TABLE 30) #############################################

tbl30 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl30.dta")

tbl30 <- tbl30 %>%
  filter(
    !(v902b == "" & is.na(v901) & is.na(v902a) & v905 == "")
  ) %>%
  filter(
    !(v902b == "" & is.na(v903) & is.na(v904a))
  ) %>%
  mutate(
    v903 = case_when(
      is.na(v903) & is.na(v907a) & is.na(v907b) ~ 1, 
      is.na(v903) & (!is.na(v907a) | !is.na(v907b)) ~ 2, 
      TRUE ~ v903
    )
  ) %>%
  filter(v902b != "") %>%
  mutate(
    v901 = 1
  ) %>%
  group_by(id) %>%
  mutate(
    v902a = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    v904a = case_when(
      v904b > 0 & is.na(v904a) ~ 1, 
      v904c > 0 & is.na(v904a) & v904b == 0 ~ 2,
      TRUE ~ v904a
    ),
    v905 = if_else(
      v905 == "" | is.na(v905),
      toupper(as.character(haven::as_factor(district))),
      toupper(as.character(v905))
    ),
    v907a = if_else(
      v907a > 1000000,
      v907a / 10,
      v907a
    )
  )

tbl30_clean <- tbl30 %>%
  mutate(uniq_id = paste0(id, "-", v902b)) %>%
  arrange(id, v902b) %>%
  select(-hhld)

section9a_clean <- section9a %>%
  mutate(uniq_id = paste0(id, "-", v902b)) %>%
  arrange(id, v902b) %>%
  select(-hhld)

tbl30_clean <- tbl30_clean[, names(section9a_clean)]

all.equal(section9a_clean, tbl30_clean)

rm(section9a_clean, tbl30_clean)

############################################ SECTION 9.2 (TABLE 31) #############################################

tbl31 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl31.dta")

tbl31 <- tbl31 %>%
  mutate(
    across(
      c(v909:v910),
      ~ if_else(is.na(v910) | v910 == 0, NA, .x)
    ),
    v908 = case_when(
      !is.na(v910) ~ 1, 
      TRUE ~ 2
    ),
    v909a = case_when(
      v908 == 1 & is.na(v909a) & (v909b > 0) ~ 1, 
      v908 == 1 & is.na(v909a) & (v909c > 0) ~ 2, 
      TRUE ~ v909a
    ),
    across(
      c(v912a:v913),
      ~ if_else(is.na(v913) | v913 == 0, NA, .x)
    ),
    v911 = case_when(
      !is.na(v913) ~ 1,
      TRUE ~ 2
    ),
    v912a = case_when(
      v911 == 1 & is.na(v912a) & (v912b > 0) ~ 1, 
      v911 == 1 & is.na(v912a) & (v912c > 0) ~ 2, 
      TRUE ~ v912a
    )
  )

tbl31_clean <- tbl31 %>%
  arrange(id) %>%
  select(-hhld)

section9b_clean <- section9b %>%
  arrange(id) %>%
  select(-hhld)

tbl31_clean <- tbl31_clean[, names(section9b_clean)]

all.equal(section9b_clean, tbl31_clean)

rm(section9b_clean, tbl31_clean)

############################################ SECTION 9.3 (TABLE 32) #############################################

tbl32 <- read_dta("/Users/sobaakun/NHIPsurvey/OOPS_Rawdata_2026_03_17/tbl32.dta")

tbl32 <- tbl32 %>%
  filter(!is.na(v914b)) %>%
  select(-v914a) %>%
  rename(
    v914a = v914b, 
    v914b = v914b_1
  ) %>%
  filter(!is.na(v914a)) %>%
  mutate(
    v915 = case_when(
      is.na(v915) &
      (is.na(v917a) | v917a == 0) &
      (is.na(v917b) | v917b == 0) &
      (is.na(v917c) | v917c == 0) &
      (v918a > 0 | v918b > 0 | v918c > 0) ~ 2,

      is.na(v915) &
      (is.na(v918a) | v918a == 0) &
      (is.na(v918b) | v918b == 0) &
      (is.na(v918c) | v918c == 0) &
      (v917a > 0 | v917b > 0 | v917c > 0) ~ 1,

      is.na(v915) &
      (v918a > 0 | v918b > 0 | v918c > 0) &
      (v917a > 0 | v917b > 0 | v917c > 0) ~ 3,

      TRUE ~ v915
    ),
    v914a = case_when(
      v914b %in% c(
        "GAHU", "KODO", "GAU BALI", "DHAN GAHU MASULI TORI", "DHAN GAHU MASULI BANGALA\nTARKARI", 
        "DHAN GAHU MASULI", "MAKAI BALI", "DHAN GAHU MASULI TORI", "DHAN GAHU MASULI",
        "DHAN 5 KUNTAL GHEHU 5 KUNTAL MASULI 1 KUNTAL", "DHAN GAHU MASULI", "MAKAI VATMAS BORI",
        "MAKAI VATMAS TORI KHURSANI", "MAKAI VATMAS KHODO", "MAKAI VATMAS", "DHAN GAHU DAL BALI",
        "MAIZE", "DHAN GAHU TORI TARKARI BALI", "DHAN GAHU AALU PIYAJ", "DHAN GAHU MASULI TARKARI",
        "DHAAN,", "DHAN GAHU MASULI", "TARKARI BALI DHAN GAHU", "MAKAI TORI"
      ) ~ 1, 
      v914b %in% c(
        "SILTUM", "DALHAN", "MATAR", "HARHR KO DAL RA MUSURO KO DAL", "DALHAN BALI"
      ) ~ 2,
      v914b %in% c(
        "OKHAR", "SAYAPATRI FUL", "CHIYA - LEMON GRASS", "RUDRAKXYA", "RUDRAKSHYA", "RUDRAKSHA",
        "RDRAKSHA", "RUDRAKSH", "GOLIYA", "RUDRAKSHYA", "LOG", "LOG(GOLILYA)", "RUDRAKSHYA 5",
        "WOOD", "COFFEE", "SUPARI"
      ) ~ 5,
      v914b %in% c(
        "TUMERIC", "TIMUR"
      ) ~ 6,
      v914b %in% c(
        "FARSI", "TOMATO", "TARKARI", "TARKARI BALI"
      ) ~ 7,
      v914b %in% c(
        "BHOGATE,RUKH KATAHAR,KERA,BHUIKATAHAR"
      ) ~ 8,
      v914b %in% c(
        "BANANA", "APPLE", "KERA", "SYAU", "BANANA,GUAVA", "FALFUL", "FALFUL/AAMBA",
        "NURSERY AAP,LITCHI,LAGAGAYAT PHUL HARUKO BIRUWA LAGAUNE RA BECHNE", "FALFUL KHETI",
        "SYAU KHETI", "FALFUL"
      ) ~ 9,
      TRUE ~ v914a
    ),
    v916 = case_when(
      v917d > 0 ~ 1,
      TRUE ~ 2
    ),
    across(
      v917a:v918d,
      ~ na_if(.x, 0)
    ), 
    v915 = case_when(
      !is.na(v917d) & !is.na(v918d) ~ 2, 
      TRUE ~ 1
    ),
    across(
      v917d:v918d,
      ~ if_else(v915 == 1, NA, .x)
    )
  ) 

tbl32 <- tbl32 %>%
  filter(!is.na(v915)) %>%
  filter(v914a != 96) 

tbl32 <- tbl32 %>%
  mutate(
    kg_equiv = case_when(
      v918a == 1 ~ v918b,           
      v918a == 2 ~ v918b * 40,     
      v918a == 3 ~ v918b * 20,     
      v918a == 4 ~ v918b * 100,     
      TRUE ~ NA_real_
    ),
    
    price_per_kg = case_when(
      v918a == 1 ~ v918c,
      v918a == 2 ~ v918c / 40,
      v918a == 3 ~ v918c / 20,
      v918a == 4 ~ v918c / 100,
      TRUE ~ NA_real_
    )
  ) %>%
  
  mutate(
    min_price = case_when(
      v914a == 1 ~ 20,   
      v914a == 2 ~ 60,    
      v914a == 3 ~ 15,    
      v914a == 4 ~ 80,    
      v914a == 5 ~ 100,   
      v914a == 6 ~ 200,  
      v914a == 7 ~ 10,    
      v914a == 8 ~ 30,   
      v914a == 9 ~ 40    
    ),
    
    max_price = case_when(
      v914a == 1 ~ 80,
      v914a == 2 ~ 200,
      v914a == 3 ~ 80,
      v914a == 4 ~ 250,
      v914a == 5 ~ 1500,
      v914a == 6 ~ 3000,
      v914a == 7 ~ 150,
      v914a == 8 ~ 250,
      v914a == 9 ~ 400
    )
  ) %>%
  
  mutate(
    flag_price = !is.na(price_per_kg) &
      (price_per_kg < min_price | price_per_kg > max_price),
    
    flag_qty = case_when(
      v918a != 5 & kg_equiv > 10000 ~ TRUE, 
      v918a == 5 & v918b > 20000 ~ TRUE,    
      v918b <= 0 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  
  group_by(v914a, v918a) %>%
  mutate(
    median_price = median(v918c[!flag_price & !is.na(v918c)], na.rm = TRUE),
    v918c = if_else(flag_price, median_price, v918c)
  ) %>%
  ungroup() %>%
  
  group_by(v914a, v918a) %>%
  mutate(
    median_qty = median(v918b[!flag_qty & !is.na(v918b)], na.rm = TRUE),
    v918b = if_else(flag_qty, median_qty, v918b)
  ) %>%
  ungroup() %>%
  
  mutate(
    v918d = v918b * v918c
  ) %>%
  
  select(-kg_equiv, -price_per_kg, -min_price, -max_price, -flag_price, -flag_qty, -median_price, -median_qty)

tbl32 <- tbl32 %>%
  mutate(
    v918c = if_else(v918d > 600000, v918c / 10, v918c),
    v918d = v918c * v918b
  )

tbl32_clean <- tbl32 %>%
  mutate(uniq_id = paste0(id, "-", v914a)) %>%
  arrange(uniq_id) %>%
  select(-hhld)

section9c_clean <- section9c %>%
  mutate(uniq_id = paste0(id, "-", v914a)) %>%
  arrange(uniq_id) %>%
  select(-hhld)

tbl32_clean <- tbl32_clean[, names(section9c_clean)]

all.equal(section9c_clean, tbl32_clean)

rm(section9c_clean, tbl32_clean)

############################################ SECTION 9.4 (TABLE 33) #############################################

tbl33 <- tbl33 %>%
  mutate(
    across(
      c(v920, v921, v923, v924, v926, v927, v928:v932d),
      ~ na_if(.x, 0)
    ),
    v927 = case_when(
      is.na(v926) & !is.na(v927) ~ NA, 
      TRUE ~ v927
    ),
    v919 = case_when(
      (is.na(v920) & is.na(v921)) ~ 2,
      TRUE ~ 1
    ),
    v922 = case_when(
      (is.na(v923) & is.na(v924)) ~ 2,
      TRUE ~ 1
    ),
    v925 = case_when(
      (is.na(v926) & is.na(v927)) ~ 2,
      TRUE ~ 1
    ),
    v920 = case_when(
      v919 == 1 & v920 > 80000 ~ median(v920, na.rm = TRUE), 
      v919 == 1 & is.na(v920) & !is.na(v921) ~ v921,
      TRUE ~ v920
    ),
    v921 = case_when(
      v919 == 1 & v921 == v920 ~ NA_real_,
      TRUE ~ v921
    ),
    v923 = case_when(
      v922 == 1 & v923 > 80000 ~ median(v923, na.rm = TRUE), 
      v922 == 1 & is.na(v923) & !is.na(v924) ~ v924,
      TRUE ~ v920
    ),
    v924 = case_when(
      v922 == 1 & v923 == v924 ~ NA_real_,
      TRUE ~ v924
    ),
    swap = v926 > v927,
    v926 = if_else(swap, pmin(v926, v927), v926),
    v927 = if_else(swap, pmax(v926, v927), v927), 
    ratio = v927 / v926, 
    v927 = case_when(
      ratio > 1000 ~ v926 * 1000,
      ratio < 300 ~ v926 * 300,
      TRUE ~ v927
    ),
    v928 = case_when(
      v928 > 30000 ~ median(v928, na.rm = TRUE),
      v928 < 10 ~ NA_real_,
      TRUE ~ v928
    ), 
    v929 = case_when(
      v929 > 50000 ~ median(v929, na.rm = TRUE), 
      v929 < 10 ~ NA_real_,
      TRUE ~ v929
    ), 
    v930 = case_when(
      v930 > 30000 ~ median(v930, na.rm = TRUE),
      v930 < 10 ~ NA_real_,
      TRUE ~ v930
    ), 
    v931 = case_when(
      v931 == 8 ~ NA_real_,
      TRUE ~ v931
    ), 
    v932a = case_when(
      v932a == 8 ~ NA_real_,
      TRUE ~ v932a
    ), 
    v932b = case_when(
      v932b > 60000 ~ median(v932b, na.rm = TRUE),
      v932b < 100 ~ NA_real_,
      TRUE ~ v932b
    ),
    v932c = case_when(
      v932c < 100 ~ NA_real_, 
      TRUE ~ v932c
    ),
    v932d = case_when(
      v932d > 30000 ~ v932d / 10,
      v932d < 100 ~ NA_real_, 
      TRUE ~ v932d
    )
  ) %>%
  select(-swap, -ratio)

############################################ SECTION 9.5 (TABLE 34) #############################################

tbl34 <- tbl34 %>%
  mutate(
    v934 = if_else(!is.na(v935), 1L, 2L)
  ) %>%
  group_by(v934a) %>%
  mutate(
    v936a = case_when(
      id == 840 & v936a == 8000 ~ 8, 
      v934a == 1 & v936a > 10 ~ round(v936a/10),
      !is.na(v936a) & is.na(v936b) ~ NA_real_,
      TRUE ~ v936a
    ),
    v938b = case_when(
      v934a == 9 & v938b > 150000 ~ round(mean(v938b[v938b <= 150000], na.rm = TRUE)),
      TRUE ~ v938b
    ),
    across(
      c(v936a:v939b),
      ~ na_if(.x, 0)
    ), 
    v936b = case_when(
      !is.na(v936b) & is.na(v936a) ~ NA_real_,
      TRUE ~ v936b
    ), 
    v937a = case_when(
      !is.na(v937a) & is.na(v937b) ~ NA_real_,
      TRUE ~ v937a
    ), 
    v937b = case_when(
      is.na(v937a) & !is.na(v937b) ~ NA_real_,
      TRUE ~ v937b
    ),
    v938a = case_when(
      !is.na(v938a) & is.na(v938b) ~ NA_real_,
      TRUE ~ v938a
    ), 
    v938b = case_when(
      is.na(v938a) & !is.na(v938b) ~ NA_real_,
      TRUE ~ v938b
    ),
    v939a = case_when(
      !is.na(v939a) & is.na(v939b) ~ NA_real_,
      TRUE ~ v939a
    ), 
    v939b = case_when(
      is.na(v939a) & !is.na(v939b) ~ NA_real_,
      TRUE ~ v939b
    )
  ) %>%
  ungroup() 

tbl34 <- tbl34 %>%
  mutate(
    v934 = if_else(
      if_all(v936a:v939b, is.na),
      2L,
      1L
    ),
    v935 = case_when(
      v934 == 2 ~ NA_real_,
      TRUE ~ v935
    )
  )

############################################ SECTION 9.6.1 (TABLE 35) #############################################

tbl35 <- tbl35 %>%
  mutate(
    across(
      v941, 
      ~ na_if(.x, 0)
    )
  )

tbl35 <- tbl35 %>%
  group_by(province, v940) %>%
  mutate(
    v941 = case_when(
      v940 == 2 & v941 > 40000 ~ round(mean(v941[v941 <= 40000], na.rm = TRUE)),
      v940 == 5 & v941 > 80000 ~ round(mean(v941[v941 <= 80000], na.rm = TRUE)),
      v940 == 1 & v941 > 600000 ~ round(mean(v941[v941 <= 600000], na.rm = TRUE)), 
      v941 < 100 ~ NA_real_,
      TRUE ~ v941
    )
  ) %>%
  ungroup()

############################################ SECTION 9.6.2 (TABLE 36) #############################################

tbl36 <- tbl36 %>%
  mutate(
    across(
      v943, 
      ~ na_if(.x, 0)
    ),
    v943 = case_when(
      v943 < 50 ~ NA_real_,
      v943 > 360000 ~ v943 / 10,
      TRUE ~ v943
    )
  )

############################################ SECTION 10 (TABLE 37) #############################################

s10 <- read.xlsx("misc/clean data1/section10.xlsx")

s10 <- s10 %>%
  rename(
    v1002c_1 = v1002c,
    v1002a_1 = v1002a
  )

tbl37 <- merge(
  tbl37, 
  s10[, c("uid", "v1002c_1", "v1002a_1")],
  by = "uid",
  all = FALSE
)

labs <- attr(tbl37$v1002b, "labels")

tbl37 <- tbl37 %>%
  mutate(
    v1001 = if_else(
      is.na(v1001), 2, v1001
    ),
    v1001 = case_when(
      is.na(v1002a) & is.na(v1002b) & is.na(v1002c) ~ 2,
      TRUE ~ v1001
    ),
    across(
      c(v1001a:v1015),
      ~ if_else(v1001 == 2, NA_real_, as.numeric(.x))
    ),
    v1002b = case_when(
      is.na(v1002b) & !is.na(v1002c) ~ v1002c,
      is.na(v1002b) & is.na(v1002c) ~ v1002a,
      TRUE ~ v1002b
    ),
    v1004 = if_else(
      v1004 > 100 | is.na(v1004) | v1004 == 0,
      100, 
      v1004
    ),
    v1004 = if_else(
      v1001 == 2, NA_real_, v1004
    ),
    v1006 = if_else(
      v1001 == 2, NA_real_, v1006
    ),
    v1005 = case_when(
      v1002c_1 %in% c("MASU TARKARI, MASU TARKARI BECHNE, LASUN LYERA BOKRA XODAYERA ORDER ANUSAR SUPPLY GARNE") ~ 400000,
      v1002c_1 %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 540000000,
      v1002c_1 %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 16200000,
      v1002c_1 %in% c("KIRANA SAMAN BIKRI") ~ 1500000,
      v1002c_1 %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 1545000,
      v1002c_1 %in% c("AAFNO HIACE CHALAUNE KARMACHARI SAHIT, DHAAN KUTNE, TEL PELNE") ~ 6400000,
      v1002c_1 %in% c("KIRANA SAMAN BECHNE, EGG CRATE BECHNE") ~ 600000,
      v1002c_1 %in% c("SUN PASAL, SHINGAR KA SAMAN BECHNE") ~ 2000000,
      v1002c_1 %in% c("KHAJA GHAR, PHOTO STUDIO") ~ 1000000,
      v1002c_1 %in% c("TARKARI BECHNE, NASTA KHAJA") ~ 900000,
      v1002c_1 %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 500000,
      v1002c_1 %in% c("KIRANA PASAL, BRAMMAN, PANDIT, PADNE") ~ 450000,
      v1002c_1 %in% c("KAPADA SILAUNE RA MARMAT SAMBHAR, COSMETICS JUTTA CHAPPAL") ~ 350000,
      v1002c_1 %in% c("MANCHHE OSAR PASAR GARNE, KIRANA PASAL") ~ 360000,
      v1002c_1 %in% c("COSMETICS SAMAN BECHNE RA PARLOUR KO KAAM, MANCHHE OSAR PASAR GARNE") ~ 360000,
      v1002c_1 %in% c("MOBILE BANAUNE NAYA MOBILE BECHNE ELECTRIC SAMAN BECHNE, DHAN GAHU KUTANI PISANI") ~ 210000,
      v1002c_1 %in% c("DHAN KUTAN PISANI, KIRANA PASAL") ~ 156000,
      v1002c_1 %in% c("KIRANA SAMAN BIKRI") ~ 3600000,
      v1002c_1 %in% c("PUJA KO SAMAN BECHNE, CAR CHALAUN SIKAUNE") ~ 1200000,
      id == 12145 ~ 5400000,
      TRUE ~ v1005
    ),
    v1006 = case_when(
      (is.na(v1007) | v1007 == 0) ~ 2, 
      TRUE ~ 1
    ),
    v1007 = case_when(
      v1002c_1 %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 860000,
      v1002c_1 %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 16200000,
      TRUE ~ v1007
    ),
    v1008 = case_when(
      v1002c_1 %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 265000,
      v1002c_1 %in% c("AAFNO HIACE CHALAUNE KARMACHARI SAHIT, DHAAN KUTNE, TEL PELNE") ~ 2400000,
      v1002c_1 %in% c("PUJA KO SAMAN BECHNE, CAR CHALAUN SIKAUNE") ~ 200000,
      v1002c_1 %in% c("KIRANA SAMAN BECHNE, EGG CRATE BECHNE") ~ 840000,
      v1002c_1 %in% c("TARKARI BECHNE, NASTA KHAJA") ~ 60000,
      v1002c_1 %in% c("MANCHHE OSAR PASAR GARNE, KIRANA PASAL") ~ 60000,
      v1002c_1 %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 22000,
      v1002c_1 %in% c("COSMETICS SAMAN BECHNE RA PARLOUR KO KAAM, MANCHHE OSAR PASAR GARNE") ~ 28000,
      v1002c_1 %in% c("SUN PASAL, SHINGAR KA SAMAN BECHNE") ~ 3000,
      v1002c_1 %in% c("KIRANA PASAL, BRAMMAN, PANDIT, PADNE") ~ 2500,
      v1002c_1 %in% c("KAPADA SILAUNE RA MARMAT SAMBHAR, COSMETICS JUTTA CHAPPAL") ~ 13200,
      TRUE ~ v1008
    ), 
    v1009a = case_when(
      v1002c_1 %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 2800000,
      v1002c_1 %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 500000,
      v1002c_1 %in% c("SUN PASAL, SHINGAR KA SAMAN BECHNE") ~ 350000,
      v1002c_1 %in% c("MASU TARKARI, MASU TARKARI BECHNE, LASUN LYERA BOKRA XODAYERA ORDER ANUSAR SUPPLY GARNE") ~ 200000,
      v1002c_1 %in% c("KIRANA SAMAN BECHNE, EGG CRATE BECHNE") ~ 360000,
      v1002c_1 %in% c("KAPADA SILAUNE RA MARMAT SAMBHAR, COSMETICS JUTTA CHAPPAL") ~ 150000,
      v1002c_1 %in% c("MOBILE BANAUNE NAYA MOBILE BECHNE ELECTRIC SAMAN BECHNE, DHAN GAHU KUTANI PISANI") ~ 58000,
      v1002c_1 %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 30000,
      v1002c_1 %in% c("KIRANA SAMAN BIKRI") ~ 342000,
      v1002c_1 %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 1490400,
      v1002c_1 %in% c("KIRANA SAMAN BIKRI") ~ 1476000
    ),
    v1009b = case_when(
      v1002c_1 %in% c("KIRANA SAMAN BIKRI") ~ 1200000,
      v1002c_1 %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 500000,
      v1002c_1 %in% c("KHAJA GHAR, PHOTO STUDIO") ~ 250000,
      v1002c_1 %in% c("TARKARI BECHNE, NASTA KHAJA") ~ 350000,
      v1002c_1 %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 25000,
      v1002c_1 %in% c("KUKHURAKO DANA, CHHALLA, KUKHURA SAGA SAMBANDHIT SAAMANHARU") ~ 115000,
      v1002c_1 %in% c("PAPER SUPPLY") ~ 1000000, 
      TRUE ~ v1009b
    ),
    v1010 = case_when(
      v1002c_1 %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 40000,
      v1002c_1 %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 460000, 
      TRUE ~ v1010
    ),
    v1011 = case_when(
      v1002a_1 %in% c("MASU PASAL, LASUN LYERA BOKRA XODAYERA ORDER ANUSAR SUPPLY GARNE") ~ 388000,
      v1002a_1 %in% c("AAFNO HIACE CHALAUNE, AAFNO MIL CHALAUNE") ~ 3000000,
      v1002a_1 %in% c("KIRANA STORE, EGG CRATE FACTORY") ~ 800000,
      v1002a_1 %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 8520000,
      v1002a_1 %in% c("SUN CHADI KO GHANA BECHNE, COSMETICS PASAL") ~ 3290000,
      v1002a_1 %in% c("FRESS HOUSE, KIRANA PASAL") ~ 1030000,
      v1002a_1 %in% c("TARKARI BECHNE, KHAJA NASTA") ~ 382000,
      v1002a_1 %in% c("KIRAN PASAL") ~ 3976000,
      v1002a_1 %in% c("1") ~ 3976000,
      v1002a_1 %in% c("HOTEL, PHOTO STUDIO") ~ 1864000,
      v1002a_1 %in% c("PASAL, PANDIT") ~ 1780000,
      v1002a_1 %in% c("AUTO CHALAUNE, KIRANA PASAL") ~ 676000,
      v1002a_1 %in% c("BEAUTY PARLOUR N COSMETICS, AUTO CHALAUNE") ~ 620000,
      v1002a_1 %in% c("PUJA PASAL, CAR DRIVING CENTER") ~ 640000,
      v1002a_1 %in% c("TRUCK DRIVER, KIRANA STORE") ~ 270000,
      v1002a_1 %in% c("TAILOR, COSMETICS PLUS JUTTA CHAPPAL") ~ 344000,
      v1002a_1 %in% c("TRACTOR THRESAR KHET JODNE DHAN GAHU JHARNE, KIRANA PASAL KHADHYANA SAMAN WHOLESALE") ~ 587887,
      v1002a_1 %in% c("MOBILE PASAL, ELECTRIC SAMAN BECHNE, MEEL CHALAUNE KUTANI PISANI KHADHYANA SAMAN") ~ 700000,
      v1002a_1 %in% c("MEEL CHALAUNE KUTANI PISANI, KIRANA PASAL") ~ 960000,
      v1002a_1 %in% c("FRESH HOUSE") ~ 731000,
      TRUE ~ v1011
    )
  )

tbl37 <- tbl37 %>%
  mutate(
    v1002b = haven::labelled(v1002b, labs),
    across(
      c(v1005:v1015),
      ~ na_if(.x, 0)
    )
  ) %>%
  select(-v1002a_1, -v1002c_1)

tbl37 <- tbl37 %>%
  group_by(v1002b) %>%
  mutate(
    v1005 = case_when(
      id == 8746 ~ 1800000,
      id == 5830 ~ 1500000,
      TRUE ~ v1005
    ),
    v1007 = case_when(
      v1002b == 3 & !is.na(v1007) & v1007 > 600000 ~ round(mean(v1007[v1007 <= 600000], na.rm = TRUE)), 
      v1002b == 15 & !is.na(v1007) & v1007 > 600000 ~ round(mean(v1007[v1007 <= 600000], na.rm = TRUE)),
      v1002b == 16 & !is.na(v1007) & v1007 > 1000000 ~ round(mean(v1007[v1007 <= 1000000], na.rm = TRUE)),
      v1002b == 6 & is.na(v1007) ~ v1005/10,
      v1002b == 14 & is.na(v1007) ~ v1005/10,
      id == 12125 ~ 1620000,
      TRUE ~ v1007
    ), 
    v1009a = case_when(
      v1002b == 1 & !is.na(v1009a) & v1009a > 280000 ~ round(mean(v1009a[v1009a <= 280000], na.rm = TRUE)), 
      v1002b == 3 & !is.na(v1009a) & v1009a > 800000 ~ round(mean(v1009a[v1009a <= 800000], na.rm = TRUE)), 
      v1002b == 7 & !is.na(v1009a) & v1009a > 420000 ~ round(mean(v1009a[v1009a <= 420000], na.rm = TRUE)),
      v1002b == 9 & !is.na(v1009a) & v1009a > 1500000 ~ round(mean(v1009a[v1009a <= 1500000], na.rm = TRUE)),
      v1002b == 21 & !is.na(v1009a) & v1009a > 50000 ~ round(mean(v1009a[v1009a <= 50000], na.rm = TRUE)),
      id == 12125 ~ 90400,
      TRUE ~ v1009a
    ),
    v1009b = case_when(
      v1002b == 1 & !is.na(v1009b) & v1009b > 28000 ~ round(mean(v1009b[v1009b <= 28000], na.rm = TRUE)), 
      v1002b == 3 & !is.na(v1009b) & v1009b > 80000 ~ round(mean(v1009b[v1009b <= 80000], na.rm = TRUE)), 
      v1002b == 7 & !is.na(v1009b) & v1009b > 42000 ~ round(mean(v1009b[v1009b <= 42000], na.rm = TRUE)),
      v1002b == 9 & !is.na(v1009b) & v1009b > 150000 ~ round(mean(v1009b[v1009b <= 150000], na.rm = TRUE)),
      v1002b == 21 & !is.na(v1009b) & v1009b > 5000 ~ round(mean(v1009b[v1009b <= 5000], na.rm = TRUE)),
      TRUE ~ v1009b
    )
  )

tbl37 <- tbl37 %>%
  mutate(
    v1006 = case_when(
      !is.na(v1002b) & !is.na(v1007) ~ 1, 
      TRUE ~ 2
    ), 
    v1011 =
      coalesce(v1005, 0) -
      coalesce(v1007, 0) -
      coalesce(v1008, 0) -
      coalesce(v1009a, 0) +
      coalesce(v1009b, 0) -
      coalesce(v1010, 0) , 

    across(
      v1011,
      ~ na_if(.x, 0)
    )  
  )

rm(s10)

############################################ SECTION 11.1 (TABLE 38) #############################################

hh_head <- tbl02 %>%
  filter(v107 == 1) %>%
  select(id, personid, v101) %>%
  rename(personid_1a = personid)

tbl38 <- tbl38 %>%
  mutate(
    v1106 = if_else(v1106 < 3000, NA_real_, v1106),
    v1106 = if_else(
      v1106 == 0, NA_real_, v1106
    )
  ) %>%
  filter(!is.na(v1106)) %>%
  mutate(
    v1101 = 1,
    v1105 = case_when(
      is.na(v1105) ~ sample(1:11, n(), replace = TRUE),
      TRUE ~ v1105
    )
  ) %>%
  group_by(v1105) %>%
  mutate(
    v1107b = case_when(
      is.na(v1107b) ~ round(mean(v1107b, na.rm = TRUE)), 
      v1107b > 100 ~ round(mean(v1107b, na.rm = TRUE)),
      TRUE ~ v1107b
    ), 
    v1107a = (v1107b / 100) * v1106,
    v1109 = case_when(
      is.na(v1110) | v1110 == 0 ~ 3, 
      TRUE ~ v1109 
    ),
    v1110 = case_when(
      v1110 > v1106 ~ v1110 / 10, 
      TRUE ~ v1110
    )
  ) %>%
  ungroup()

tbl38 <- tbl38 %>%
  left_join(hh_head, by = "id") %>%
  mutate(
    personid = case_when(
      is.na(personid) ~ personid_1a, 
      TRUE ~ personid
    ), 
    v1103 = case_when(
      is.na(v1103) ~ v101,
      TRUE ~ v1103
    )
  ) %>%
  select(-personid_1a, -v101)

############################################ SECTION 11.2 (TABLE 39) #############################################

tbl39 <- tbl39 %>%
  mutate(
    v1111 = 1,
    v1116 = case_when(
      v1116 < 3000 ~ NA_real_,
      TRUE ~ v1116
    )
  ) %>%
  filter(!is.na(v1116)) %>%
  filter(!is.na(v1115)) %>%
  group_by(v1115) %>%
  mutate(
    v1117b = case_when(
      v1117b > 50 ~ round(mean(v1117b, na.rm = TRUE)),
      TRUE ~ v1117b
    )
  ) %>%
  ungroup() %>%
  mutate(
    v1117a = (v1117b / 100) * v1116, 
    v1119 = case_when(
      is.na(v1120) | v1120 == 0 ~ 3, 
      TRUE ~ v1119
    ),
    v1120 = case_when(
      v1120 > v1116 ~ v1120 / 10,
      TRUE ~ v1120
    )
  )

tbl39 <- tbl39 %>%
  left_join(hh_head, by = "id") %>%
  mutate(
    personid = case_when(
      is.na(personid) ~ personid_1a, 
      TRUE ~ personid
    ), 
    v1113 = case_when(
      is.na(v1113) ~ v101,
      TRUE ~ v1113
    )
  ) %>%
  select(-personid_1a, -v101)

############################################ SECTION 11.3 (TABLE 40) #############################################

tbl40 <- tbl40 %>%
  mutate(
    across(
      v1121:v1132,
      ~ na_if(.x, 0)
    ),
    v1122 = case_when(
      v1122 < 10 ~ NA_real_, 
      TRUE ~ v1122
    ),
    v1121 = case_when(
      is.na(v1122) ~ 2, 
      TRUE ~ 1
    ),
    v1128 = case_when(
      v1128 < 10 ~ NA_real_,
      TRUE ~ v1128
    ), 
    v1127 = case_when(
      is.na(v1127) ~ 2, 
      TRUE ~ 1
    ),
    across(
      c(v1122:v1124),
      ~ if_else(v1121 == 2, NA, .x)
    ),
    across(
      c(v1128:v1130),
      ~ if_else(v1127 == 2, NA, .x)
    )
  )

############################################ SECTION 12.1 (TABLE 41) #############################################

tbl41 <- tbl41 %>%
  mutate(
    v1203 = as.numeric(v1203), 
    v1204 = as.numeric(v1204)
  ) %>%
  filter(
    !is.na(v1204)  &
    !is.na(v1205)  & 
    !is.na(v1206) 
  ) %>%
  filter(!is.na(personid))

remittance_qualified <- tbl02 %>%
  filter(v109 %in% c(3, 4))

tbl41 <- tbl41 %>%
  filter(personid %in% remittance_qualified$personid)

remittance_missing <- read.xlsx("misc/remittance_missing.xlsx")

remittance_missing <- merge(
  remittance_missing, 
  tbl02[, c("personid", "v109", "v101")],
  by = "personid"
)

remittance_missing <- remittance_missing %>%
  mutate(
    v1204 = case_when(
      v1203 <= 10 ~ v1203, 
      v1203 <= 18 & v1203 > 10 ~ sample(1:6, n(), replace = TRUE), 
      v1203 > 18 ~ sample(1:10, n(), replace = TRUE),
      TRUE ~ v1204
    ),
    v1205 = case_when(
      v1203 <= 15 ~ 1, 
      v1203 > 15 & v1203 <= 18 ~ sample(1:2, n(), replace = TRUE),
      v1203 > 50 & v109 == 3 ~ 1,
      v1203 > 18 ~ sample(1:4, n(), replace = TRUE),
      TRUE ~ v1205
    ),
    v1207 = case_when(
      v1203 <= 15 ~ 5,
      v1203 >= 16 & v1203 <= 18 ~ 4, 
      v1203 > 18 & v1205 == 2 ~ 4,
      v1203 > 18 & v1205 %in% c(3, 4) ~ 1,
      v1203 > 18 & v1205 == 5 ~ 2, 
      v1203 > 50 & v109 == 3 ~ 3, 
      v1203 %in% c(20:30) & v109 == 4 ~ sample(c(1, 4), n(), replace = TRUE),
      TRUE ~ 1
    ),
    v1208 = 2  
  ) 

districts <- toupper(unique(haven::as_factor(tbl02$district)))

countries <- c("QATAR", "UAE", "UK", "SPAIN", "GERMANY", "SAUDI", "JORDAN", "KUWAIT", "BAHRAIN", "AUSTRALIA", "US", "CROATIA", "HUNGARY")

remittance_missing <- remittance_missing %>%
  mutate(v1206 = as.character(v1206)) %>%
  mutate(
    v1206 = if_else(
      v109 == 3,
      sample(districts, n(), replace = TRUE),
      as.character(v1206)
    ),
    v1206 = if_else(
      v109 == 4,
      sample(countries, n(), replace = TRUE),
      as.character(v1206)
    )
  ) %>%
  rename(v1202 = v101) %>%
  select(-v109, -v102) 

for (i in setdiff(1:ncol(remittance_missing), c(11, 13, 16, 25))) { 
  remittance_missing[[i]] <- as.numeric(gsub("[^0-9]", "", remittance_missing[[i]]))
}

tbl41 <- tbl41 %>%
  rows_append(remittance_missing)

tbl41 <- tbl41 %>%
  mutate(
    v1210 = case_when(
      v1210 == 0 & v1209 > 50 ~ v1209,
      TRUE ~ v1210
    ), 
    v1209 = case_when(
      v1209 > 50 ~ 1, 
      TRUE ~ v1209
    ), 
    v1209 = case_when(
      v1209 == 0 & v1210 > 0 ~ 1,
      TRUE ~ v1209
    )
  )

tbl41 <- tbl41 %>%
  filter(personid %in% remittance_qualified$personid)

sum(remittance_qualified$personid %in% tbl41$personid)

rm(remittance_missing, remittance_qualified, hh_head)

############################################ SECTION 12.2 (TABLE 42) #############################################

tbl42 <- tbl42 %>%
  mutate(
    v1213b = if_else(
      v1213b == 0, 
      NA, 
      v1213b
    ),
    v1214b = if_else(
      v1214b == 0, 
      NA, 
      v1214b
    ),
    v1213a = case_when(
      v1213b > 0 ~ 1, 
      TRUE ~ 2
    ), 
    v1214a = case_when(
      v1214b > 0 ~ 1, 
      TRUE ~ 2
    )
  )

############################################ SECTION 13.1 (TABLE 43) #############################################

for (i in setdiff(1:ncol(tbl43), c(10, 23))) {
  tbl43[[i]] <- as.numeric(gsub("[^0-9]", "", tbl43[[i]]))
}

tbl43 <- tbl43 %>%
  mutate(
    v1304b = case_when(
      v1304a == v1304b ~ NA,
      TRUE ~ v1304b
    ),
    v1304c = case_when(
      v1304a == v1304c ~ NA,
      TRUE ~ v1304c
    ), 
    v1304d = case_when(
      v1304a == v1304d ~ NA,
      TRUE ~ v1304c
    ),
    v1303 = case_when(
      !is.na(v1304a) &  is.na(v1304b) &  is.na(v1304c) &  is.na(v1304d) ~ 1,
      !is.na(v1304a) & !is.na(v1304b) &  is.na(v1304c) &  is.na(v1304d) ~ 2,
      !is.na(v1304a) & !is.na(v1304b) & !is.na(v1304c) &  is.na(v1304d) ~ 3,
      !is.na(v1304a) & !is.na(v1304b) & !is.na(v1304c) & !is.na(v1304d) ~ 4,
      TRUE ~ NA_real_
    ),
    v1302 = case_when(
      is.na(v1303) & !is.na(v1305) ~ 2,
      TRUE ~ v1302
    ),
    across(
      c(v1303, v1304a, v1304b, v1304c, v1304d, v1305, v1306, v1307), 
      ~ na_if(.x, v1302 == 2)
    ),
    max_allowed = if_else(
      v1301 == 1,
      48000 * v1303,
      NA_real_
    ),
    p20_childgrant = quantile(
      v1305[v1301 == 6],
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_aamasurakshya = quantile(
      v1305[v1301 == 7], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_other = quantile(
      v1305[v1301 == 13],
      probs = 0.20, 
      na.rm = TRUE
    )
  ) %>%
  group_by(v1301, v1303) %>%
  mutate(
    v1305 = case_when(
      id == 13618 & v1305 == 240002 ~ 24000,
      v1301 == 1 & v1305 > max_allowed ~
        round(mean(v1305[v1305 <= max_allowed], na.rm = TRUE), -2),
      v1301 == 1 & v1305 < 4000 ~ 4000,
      v1301 == 1 & is.na(v1305) ~
        round(mean(v1305[v1305 <= max_allowed], na.rm = TRUE), -2),
      v1301 == 2 & !is.na(v1303) & v1305 < 2660 ~ 2660, 
      v1301 == 3 & !is.na(v1303) & v1305 < 4000 ~ 3990,
      v1301 == 6 & !is.na(v1303) & v1305 < 500 ~ p20_childgrant,
      v1301 == 7 & !is.na(v1303) & v1305 < 500 ~ p20_aamasurakshya,
      v1301 == 13 & !is.na(v1303) & v1305 < 500 ~ p20_other,
      TRUE ~ v1305
    )
  ) %>%
  ungroup() %>%
  select(-max_allowed, -p20_childgrant, -p20_aamasurakshya, -p20_other)

############################################ SECTION 13.2 (TABLE 44) #############################################

tbl44 <- tbl44 %>%
  mutate(
    v1309 = case_when(
      is.na(v1310) ~ 2, 
      TRUE ~ 1
    )
  )

############################################ SECTION 13.3 (TABLE 45) #############################################

for (i in setdiff(1:ncol(tbl45), c(10, 16))) { 
  tbl45[[i]] <- as.numeric(gsub("[^0-9]", "", tbl45[[i]]))
}

tbl45 <- tbl45 %>%
  mutate(
    v1311b = case_when(
      is.na(v1312) | v1312 == 0 ~ 2, 
      TRUE ~ 1
    ), 
    across(
      v1312,
      ~ na_if(.x, 0)
    ),
    v1312 = case_when(
      v1312 == 0 ~ NA_real_,
      v1312 == 36000034 ~ 360000,
      v1312 == 1440002 ~ 144000,
      v1312 == 1080000 ~ 80000,
      v1312 == 7e+06 ~ 700000,
      v1312 == 8e+06 ~ 800000,
      v1312 == 3e+06 ~ 300000,
      v1312 == 5000099 ~ 50000,
      TRUE ~ v1312
    )
  )

tbl45 <- tbl45 %>%
  mutate(
    p20_savings = quantile(
      v1312[v1311a == 1], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_fixed_deposit = quantile(
      v1312[v1311a == 2], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_stocks = quantile(
      v1312[v1311a == 3], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_cit = quantile(
      v1312[v1311a == 4], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_pension = quantile(
      v1312[v1311a == 5], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_insurance = quantile(
      v1312[v1311a == 9], 
      probs = 0.20,
      na.rm = TRUE
    ),
    p20_rent = quantile(
      v1312[v1311a == 11], 
      probs = 0.20,
      na.rm = TRUE
    ),
    v1312 = case_when(
      v1311a == 1 & v1311b == 1 & v1312 < 200 ~ p20_savings,
      v1311a == 2 & v1311b == 1 & v1312 < 3000 ~ p20_fixed_deposit,
      v1311a == 3 & v1311b == 1 & v1312 < 200 ~ p20_stocks,
      v1311a == 4 & v1311b == 1 & v1312 < 1500 ~ p20_cit,
      v1311a == 5 & v1311b == 1 & v1312 < 8000 ~ p20_pension,
      v1311a == 9 & v1311b == 1 & v1312 < 500 ~ p20_insurance,
      v1311a == 11 & v1311b == 1 & v1312 < 2000 ~ p20_rent,
      v1311a == 5 & v1311b == 1 & v1312 > 624000 ~ 360000,
      v1311a == 11 & v1311b == 1 & v1312 > 80000 ~ v1312 / 100,
      TRUE ~ v1312
    )
  ) %>%
  select(-p20_savings, -p20_fixed_deposit, -p20_stocks, -p20_cit, -p20_pension, -p20_insurance, -p20_rent)

rm(s6c1_add, s6c2_add, s6c3_missing, s6c3_qualified, s6c4_missing, ssf_respondent_id, s6b3_update, s6b4_updates)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

df_names <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]

for (nm in df_names) {
  df <- get(nm)
  df <- haven::zap_widths(df)
  
  write_dta(
    df,
    file.path("data", paste0(nm, ".dta"))
  )
}

rm(df)

####################################################################################################################

s0_res <- compare_df(section0, tbl01, group_col = "id")

identical(section0, tbl01)
s0_res$change_summary
s0_res$change_count



dir.create("data", showWarnings = FALSE, recursive = TRUE)

df_names <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]

for (nm in df_names) {
  df <- get(nm)
  df <- haven::zap_widths(df)
  
  write_dta(
    df,
    file.path("data", paste0(nm, ".dta"))
  )
}

rm(df)