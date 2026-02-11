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

in_dir <- "stata_data"

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

sections <- lapply(files, read_dta)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)

rm(sections)

gc()

#SECTION0

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

section0 <- section0 %>%
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
  )

#SECTION1A

section1a <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),

    v105_num  = grepl("[0-9]", v105a),
    v105_tmp  = if_else(v105_num, v105a, v105),
    v105a_tmp = if_else(v105_num, v105,  v105a),

    v106_num  = grepl("[0-9]", v106a),
    v106_tmp  = if_else(v106_num, v106a, v106),
    v106a_tmp = if_else(v106_num, v106,  v106a),

    v107_num  = grepl("[0-9]", v107a),
    v107_tmp  = if_else(v107_num, v107a, v107),
    v107a_tmp = if_else(v107_num, v107,  v107a)
  ) %>%
  mutate(
    v105  = v105_tmp,
    v105a = v105a_tmp,
    v106  = v106_tmp,
    v106a = v106a_tmp,
    v107  = v107_tmp,
    v107a = v107a_tmp
  ) %>%
  select(-ends_with("_num"), -ends_with("_tmp")) %>%
  mutate(
    v104a = as.numeric(gsub("[^0-9]", "", v104a)),
    v103 = case_when(
      v102 == "PRASANSHA BISTA" ~ 2, 
      TRUE ~ v103
    ),
    v103 = if_else(v103 == 96, 3, v103),
    v104a = if_else(is.na(v104a), 0, v104a),
    v105 = case_when(
      grepl("KUMHAL", v105, ignore.case = TRUE) ~ "3",
      grepl("NEWAR|SHRESTHA|THARU|SIMANTRAKIT|SIMANTAKRIT", v105, ignore.case = TRUE) ~ "2",
      v105a %in% c(
        " SIMANTRAKRIT", " ATI SIMANTAKRIT", " ATI SIMANTKRIT", " ATISIMANTKRIT",
        " ATISEMANTKRIT", " ATI SEMANTKRIT", " ATI SEMANTRAKRIT", " SIMANTAKRIT",
        " SIMANTRAKIT", " SIMANTRAKRIT", " ATI SIMANTRAKRIT",
        "CHEPANG", "CEPANG", " NEWAR", " THARU"
      ) ~ "2",
      v105a %in% c(" KUMHAR") ~ "3",
      v105a %in% c(" SANYASI", " JOGI") ~ "1",
      hhid %in% c("3101-19", "3101-20") ~ "2",
      hhid %in% c("5104-16") ~ "3",
      personid %in% c(5952789) ~ "1", 
      ID %in% c(14569) ~ "2", 
      ID %in% c(14526) ~ "1",
      TRUE ~ v105
    ),
    v106 = case_when(
      v106a %in% c(" SACHHAI") ~ "5",
      v106a %in% c(" NEWAR") ~ "1",
      personid %in% c("5952247", "PUJA RANABHAT") ~ "1", 
      ID %in% c("14468") ~ "2",
      ID %in% c("11084", "11085") ~ "1",
      v106a %in% c(" YUMA") ~ "6",
      v102 %in% c("IRFAN AALAM") ~ "3",
      TRUE ~ v106
    ),
    v107  = as.numeric(gsub("[^0-9]", "", v107)),
    v107 = case_when(
      v107a %in% c(
        "VAI KO CHORI", "DAIKO XORI", " SAUTHELO XORA", " PALEKO XORI"
      ) ~ 3, 
      v107a %in% c(
        " PANATINI", "PALATI", " PANATI"
      ) ~ 4,
      v107a %in% c(
        "BAINI PARNE", " BHADAINI", "BHADA", "BHADAINI", "SADU DIDI", " DIDI", 
        " PHUPU", " DIDI", " BHADAI"
      ) ~ 6, 
      v107a %in% c(
        " BAHINI KO CHHORI", " BAHINI KO XORA", "BAINIKO XORA", "VANJA", "BAHENIKO CHORA", "BHANJA",
        "BHANJI", "SALI KO CHHORA", " BHANJA", " BHANJI", " BHANJA"
      ) ~ 7,
      v107a %in% c(
        "JAWAI", "NATINI JWAI", "NATINI BUHARI", " NATINI BUHARI", " SAUTELEY BUHARI"
      ) ~ 8,
      v107a %in% c(
        "DEWAR", "DEWARANI", " DEURANI", " DEWAR", " NANDA", " JETHAJU", " JETHANI", " NANDA"
      ) ~ 9, 
      v107a %in% c(
        " FUPU SASU", " BUDHI SASU", " SASURA"
      ) ~ 10, 
      v107a %in% c(
        "XORI MANNU VAYERA RAAKHNU VAKO"
      ) ~ 11,
      v107a %in% c(
        " HAJUR AAMA", " GRANDMOTHER", " HAJURAAMA", "हजुरआमा", " HAJUR BABA", " HAJUR BABA", " HAJUR AAMA",
        " HAJURAMA", "HAJURAMA"
      ) ~ 12, 
      TRUE ~ v107
    )
  ) %>%
  mutate(
    ID   = as.numeric(ID),
    psu  = as.numeric(psu),
    ward = as.numeric(ward),
    hhld = as.numeric(hhld),
    v101 = as.numeric(v101),
    v103 = as.numeric(v103),
    v105 = as.numeric(v105),
    v106 = as.numeric(v106),
    v107 = as.numeric(v107),
    v108 = as.numeric(v108),
    v109 = as.numeric(v109),
    v110 = as.numeric(v110)
  ) %>%
  mutate(
    v108 = case_when(
    is.na(v108) & v109 %in% c(3, 4) ~ 0,
    is.na(v108) & v109 == 1 ~ 12,
    TRUE ~ v108)
  ) %>%
  filter(
    !personid %in% c(5952899, 13355, 13861, 15077)
  )

section1a <- section1a %>%
  group_by(hhid, v102) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    v107 = case_when(
      v102 == "MITRA KUMARI DHAMALA" & hhid == "3211-6" ~ 2,
      v102 == "KAMALA DEVI SARU" & hhid == "4206-3" ~ 2,
      v102 == "RISHI KUMAR MAHATO" & hhid == "2205-15" ~ 3,
      v102 == "PARBATI TIMILSINA" & hhid == "5111-7" ~ 2,
      v102 == "GANGA DEI SHRESTHA" & hhid == "5111-9" ~ 2, 
      v102 == "SAURAV BHANDARI" & hhid == "5209-15" ~ 3,
      v102 == "AABHASH DHAMI" & hhid == "7104-15" ~ 3,
      v102 == "RADHA KC" & hhid == "5211-16" ~ 2, 
      v102 == "KARNA BDR BUDHA MAGAR" & hhid == "6103-21" ~ 2, 
      v102 == "MANISHA TAMANG" & hhid == "3444-3" ~ 3,
      v107 == 96 ~ 11,
      TRUE ~ v107
    )
  )

invalid_hhids <- section1a %>%
  filter(v107 == 1, v109 %in% c(3, 4)) %>%
  distinct(hhid)

new_heads <- section1a %>%
  semi_join(invalid_hhids, by = "hhid") %>%
  filter(v109 %in% c(1, 2)) %>%
  group_by(hhid) %>%
  slice_max(v104a, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hhid, uniq_id)

section1a <- section1a %>%
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

rm(invalid_hhids, new_heads)

#SECTION1B

for (i in setdiff(1:ncol(section1b), c(2, 7, 8, 21, 22, 23))) {
  section1b[[i]] <- as.numeric(section1b[[i]])
}

section1b <- section1b %>%
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
        "SCHOOL BATA GARAYAKO CHHA" 
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
    v114 = case_when(
      is.na(v114) & v115 %in% c(2, 3) ~ 1,
      TRUE ~ v114
    ), 
    v114 = if_else(
      is.na(v114) & is.na(v115) & is.na(v116),
      3L,
      v114
    ),
    v114 = if_else(
      v114 == 1 & is.na(v115) & is.na(v116), 
      3L,
      v114
    ),
    v115 = if_else(
      v114 == 3, 
      NA_real_, 
      v115
    ),
    v115 = if_else(
      v114 == 1 & is.na(v115) & !is.na(v116), 
      1L,
      v114
    ),
    v116 = if_else(
      v114 == 3, 
      NA_real_, 
      v116
    ),
    v118 = if_else(!is.na(v119), 1L, v118),
    v120 = if_else(!is.na(v121), 1L, v120), 
  )

#SECTION2A1 

section2a1 <- section2a1 %>%
  mutate(
    v203_num = grepl("[0-9]", v203a),   
    
    v203_new  = if_else(v203_num, v203a, v203),
    v203a_new = if_else(v203_num, v203,  v203a),
    
    v203  = v203_new,
    v203a = v203a_new
  ) %>%
  select(-v203_num, -v203_new, -v203a_new) %>%
  mutate(
    v204_num = grepl("[0-9]", v204a),   
    
    v204_new  = if_else(v204_num, v204a, v204),
    v204a_new = if_else(v204_num, v204,  v204a),
    
    v204  = v204_new,
    v204a = v204a_new
  ) %>%
  select(-v204_num, -v204_new, -v204a_new) %>%
  mutate(
    v205_num = grepl("[0-9]", v205a),   
    
    v205_new  = if_else(v205_num, v205a, v205),
    v205a_new = if_else(v205_num, v205,  v205a),
    
    v205  = v205_new,
    v205a = v205a_new
  ) %>%
  select(-v205_num, -v205_new, -v205a_new) %>%
  mutate(
    v206_num = grepl("[0-9]", v206a),   
    
    v206_new  = if_else(v206_num, v206a, v206),
    v206a_new = if_else(v206_num, v206,  v206a),
    
    v206  = v206_new,
    v206a = v206a_new
  ) %>%
  select(-v206_num, -v206_new, -v206a_new) 


for (i in setdiff(1:ncol(section2a1), c(2, 7, 8, 13, 15, 17, 19, 20))) {
  section2a1[[i]] <- as.numeric(gsub("[^0-9]", "", section2a1[[i]]))
}

section2a1 <- section2a1 %>%
  group_by(psu) %>%
  mutate(
    mean_v202_psu = round(mean(v202[v202 <= 15 & v202 != 0], na.rm = TRUE)),
    v202 = ifelse(is.na(v202) | v202 == 0, mean_v202_psu, v202)
  ) %>%
  select(-mean_v202_psu) %>%  
  ungroup()

section2a1 <- section2a1 %>%
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
    )  )

#SECTION2A2

section2a2 <- section2a2 %>%
  rename(
    v213 = v213a, 
    v213a = v213b
  ) %>%
  mutate(
    v213_num = grepl("[0-9]", v213a),   
    
    v213_new  = if_else(v213_num, v213a, v213),
    v213a_new = if_else(v213_num, v213,  v213a),
    
    v213  = v213_new,
    v213a = v213a_new
  ) %>%
  select(-v213_num, -v213_new, -v213a_new) 
  

for (i in setdiff(1:ncol(section2a2), c(2, 7, 8, 16))) {
  section2a2[[i]] <- as.numeric(gsub("[^0-9]", "", section2a2[[i]]))
}

section2a2 <- section2a2 %>%
  mutate(
    v208 = case_when(
      is.na(v209) & is.na(v210) ~ 2,
      !is.na(v209) & !is.na(v210) ~ 1,
      TRUE ~ v208
    ),

    v211 = if_else(!is.na(v212), 1, v211),

    v213 = ifelse(v211 == 1 & !is.na(v212), NA_real_, v213),

    v213 = case_when(
      v213a %in% c(
        " AAFANTA LE DINU BHAKO", " QUARTER IN COMPANY", " QUARTER PROVIDED BY COMPANY",
        " SAWSYASASTHAKO LE DIYAKO ROOM", " KAM GARE BAPAT KO ACCOMODATIONS HOTEL LE UPLABDH",
        " COMPANY'S ROOM", " COMPANY PROVIDED", " OFFICE RESIDENCE", " QUARTER MA"
      ) ~ 2,

      v213a %in% c(
        " GHAR KO ROOM MATRA VADA MA LEYAKO", " ROOM EUTA LEKO", " EUTA ROOM LEYEKO",
        " FLAT VADA MA LEYEKO", " 96", " SAJHEDARI MA BASEKO",
        " GHAR KO 2 TA ROOM VADA MA LEYEKO", " TWO ROOM RENT MA LEYEKO",
        " 1UTA ROOM VADA MA LEYEKO", " EUTA ROOM LEKO", " FLAT LEYEKO",
        " JAGGA LEEJ MA LIYERA AFAILE TEMPORARY TAHARA HALERA BASEKO RA ANNUALLY 32000 RUPAYA BUJHAUNE GAREKO",
        " AFNO GHAR BANAI RAKHEKO BHAYERA KAILA PAISA TIRNEY KAILA NATIRNEY",
        " LAND KO RENT TIREKO RA AAFULIE TESMA GHAR BANAYEKO",
        " EUTA ROOM RENT MA", " FLAT MA LEYEKO", " 2 TA ROOM LEYEKO",
        " GHAR MA BASEKO BAAPAT KAKA LAI KHETI PAATI KO 50 DINU PARNEY",
        " BANDAKI LIYEKO 450000 3YRS CHODA SABAI PAISSA RETURN HUNCHA",
        " RENT MA BASEKO", " 1 ROOM RENT MA LEYEKO",
        " HOSTEL", " 1 ROOM MA BASEKO", " 2 ROOM RENT MA",
        " HOSTEL MA BASEKO", " FLAT RENT MA LEYAKO"
      ) ~ 1,

      v208 == 2 & is.na(v213a) ~ 2,
      v213 == 96 ~ 1,
      TRUE ~ v213
    ),

    v213a = ifelse(v211 == 1 & !is.na(v212), NA_character_, v213a),
    v214  = ifelse(v211 == 1 & !is.na(v212), NA_real_, v214),
    v215  = ifelse(v211 == 1 & !is.na(v212), NA_real_, v215),
    v213 = if_else(v213 > 3, 2, v213),
    v214 = if_else(v214 > 100000, v214 / 100, v214),
    v215 = if_else(v215 > 100000, v215 / 100, v215)
  )


#SECTION2A3 

section2a3 <- section2a3 %>%
  mutate(
    v216_num = grepl("[0-9]", v216a),   
    
    v216_new  = if_else(v216_num, v216a, v216),
    v216a_new = if_else(v216_num, v216,  v216a),
    
    v216  = v216_new,
    v216a = v216a_new
  ) %>%
  select(-v216_num, -v216_new, -v216a_new) %>%
  mutate(
    v218_num = grepl("[0-9]", v218a),   
    
    v218_new  = if_else(v218_num, v218a, v218),
    v218a_new = if_else(v218_num, v218,  v218a),
    
    v218  = v218_new,
    v218a = v218a_new
  ) %>%
  select(-v218_num, -v218_new, -v218a_new)

for (i in setdiff(1:ncol(section2a3), c(2, 7, 8, 11, 14, 24, 33))) {
  section2a3[[i]] <- as.numeric(gsub("[^0-9]", "", section2a3[[i]]))
}

section2a3 <- section2a3 %>%
  mutate(
    v216 = case_when(
      v216a %in% c(
        "BORING KO PANI", "DEEP BORING", "DIP BORNING",
        " UNDERGROUND WATER", " BOARDING BATA"
      ) ~ 3,

      v216a %in% c(
        "COMMON TAP", "GHAR XEU KO MANXE  KO DHARO USE GAREKO",
        "SARBAJANIK", "SARBAJANIK DHARA",
        "SARBJANIK", "SARBJANIK DHARA",
        "XIMAKINKO GHAR BATA PAISA DEYERA",
        "XIMEKI KO GHAR MA MAGERA KHANU HUNCHA",
        " DHUGEDHARA"
      ) ~ 2,
      v216a %in% c(
        "HOSTEL MA PAY GAREKO", "v216a"
      ) ~ 1,
      v216a %in% c(
        "JAAR", "JAAR AND TANKER",
        "JAR", "JAR  KO PANI", "JAR KO",
        "JAR KO PANI", "JARKO", "JARKO KO",
        "JARKO PANI",
        "जार", "जारको पानी",
        " JAAR", " JAAR KO PANI",
        " JAR", " JAR KO PANI"
      ) ~ 8,
      v216a %in% c(
        "TANKER", "TYANKAR KINEKO", "TYANKER KINNE",
        " TANKER BATA LERA AAUNE"
      ) ~ 9,
      TRUE ~ v216
    ), 
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

#SECTION2B

section2b <- section2b %>% 
  mutate(
    v226 = case_when(
      v227 != "" ~ "1",
      TRUE ~ v226
    ),
    v230 = case_when(
      v230 %in% c(
        "10% AAFAILE ARU OFFICE LE",
        "10% AAFNO RA OFFICE LE 22%",
        "10% AFULEY BAKI OFFICE BATA",
        "10% AFULEY BAKI OFFICE LEY",
        "10%AFNO RA 20% OFFICE KO",
        "10/20",
        "1700 SELF",
        "1750",
        "20% AAFULE BAKI OFFICELE",
        "22% SANSHA 10% AAFAI",
        "50% AAFU LYA 50%SARKAR LAY",
        "50% SELF & 50% GOVERNMENT",
        "AADHA AAFAI LE AADHA SARKAR LE",
        "AADHA ROJGAR DATALE",
        "AADHI AFAI LE TIREKO ADHI SARKAR LE",
        "COMPANY RA AFU",
        "HALF OFFICE HALF SELF",
        "OFFICE AND SELF",
        "OFFICE KO SALARY BATE",
        "OFFICE LE AAFNU SALARIE BATA",
        "OFFICE LE AAFNU SALARIE BATA GAREKO",
        "OFFICE RA AFU",
        "OFFICE RACAFU",
        "PALIKALE 50%AAFULE 50%"
      ) ~ "6",

      v230 %in% c(
        "11 % PAID BY RESPONDENT & 20% PAID BY OFFICE",
        "11 PERCENT AAFNAI TIRAYKO BAKI OFFICE LA TIRAYKO.",
        "11 PERCENT SELF PAYMENT REMAINING PAID BY OFFICE.",
        "11% AAFNAI 20% OFFICELE",
        "11% AAFNAI BAKI OFFICELE",
        "11% AAFNAI BAKI SCHOOL LE",
        "11% AAPHULE",
        "11% AFAI BAKI OFFICE",
        "11% AFAI BAKI OFFICE LEY",
        "11% AFAILE TIREKO ARU HOSPITAL LEY",
        "11% AFAILEY BAKI OFFICE BATA PAYMENT",
        "11% AFNAI RA 20% OFFICE LE TIRIDIYEKO",
        "11% AFULE ARU HOSPITAL LE",
        "11% APHULE",
        "11% APHULE TIREKO",
        "11% PAID BY STAFF AND OTHERS BY SAHAKARI",
        "11% RESPONDENT & 20% ORGANIZATION",
        "11% SELF",
        "11% SELF 20% SASTHA LE",
        "11%AAFU LAY 22 AAFU LYA",
        "11%AFAI BAKI OFFICE",
        "11%AFU 20%OFFICE",
        "11%AFU LE RA 20 %OFFICE LE",
        "11%AFU RA 20% COMPANY",
        "11%APHULE 20% OFFICE",
        "11%RESPONDENT & 20%ORGANIZATION",
        "20% OFFICE LE 11% APHAILE TIREKO",
        "20% SCHOOL 11% AAFU",
        "20%OFFICE 11%AAFU",
        "20%OFFICE RA 11% AFNU",
        "20%SCHOOL 11% AAFAI",
        "20%SCHOOL 11% AAFULA",
        "20%SCHOOL11% AAFULA",
        "AFU LE 11%RA COMPANI LE 20%",
        "AFU LE11%RA OFFICE LE 20%",
        "AFULE 11%20OFFICE",
        "APHAILE 11% SCHOOL LE 20%",
        "OFFICE LE 20%",
        "OFFICE LE 20% AAFULE 11%",
        "OFFICE LE 20%11%AFNU",
        "OFFICE LE 20%AFU LE 11%",
        "SASTHALE 20%",
        "SELF-11%",
        "SSF",
        "WE PAID 11 PERCENT AND THE OFFICE COVERED THE REST.."
      ) ~ "5",

      v230 %in% c(
        "COMPANY", "COMPANY LE", "COMPANY'", "EMPLOYER",
        "EMPLOYER PAID", "OFFICE", "OFFICE BATAW",
        "OFFICE LE", "OFFICE LEY", "OFFICER",
        "ORGANIZATION", "RAJGAR DATA",
        "ROJGAAR DATA", "ROJGAR DATA",
        "SCHOOL", "SCHOOL LE",
        "SCHOOL LE GARIDEYAKO"
      ) ~ "7",

      v230 %in% c(
        "GOVERNMENT",
        "NAPAL SARAKAR",
        "NEPAL GOVERNMENT",
        "NEPAL SARAKAR",
        "NEPAL SARKAR",
        "SARKAR"
      ) ~ "8",

      v230 %in% c(
        "HOSPITAL",
        "HOSPITAL 89%AAFULE11%",
        "HOSPITAL LE"
      ) ~ "9",

      v230 %in% c(
        "B GROUP SASTHYA",
        "BIZU",
        "CHEPANG SANGH",
        "RAKAM TIRNA PAREKO CHHOINA",
        "SWASTHYA LE",
        "THAHA XINA"
      ) ~ "2",

      v230 %in% c(
        "KAKA SASURA",
        "PAILA AFAI LEY TIRNU BHAKO",
        "UNCLE HARULE"
      ) ~ "1",

      v230a %in% c(
        " 10 % AFULE 22% OFFICE LE",
        " 10% AAFU LA 22%COLLAGE LA",
        " 10% AFNAI 10% OFFICE BATA",
        " 10% APHAILE 22% OFFICE LE",
        " 10% PERSONAL AND 10% OFFICE",
        " 10% RA OFFICE LE 22%",
        " 10% SELF AND 10% FROM OFFICE",
        " 20% SELF 80% OFFICE",
        " 2000 OFFICE LA 6000 AAAFU LA",
        " 22% OFFICE LA 10% AAAFU LA",
        " 50% GOVERNMENT.50% AFAI LE TIRE KO",
        " 50% SELF 50% GOVERNMENT"
      ) ~ "6",

      v230a %in% c(
        " 11% AFAILE 20% OFFICE LE",
        " 11% AFAILE TIRNE BAKI OFFICE LE",
        " 11% AFNO RA 20% OFFICE KO CONTRIBUTION",
        " 11% PAID AND OTHER PAID BY HOSPITAL",
        " 11% SELF AND REMAINING IS BY HOSPITAL",
        "20% OFFICE",
        "AAFULE11%"
      ) ~ "5",

      v230a %in% c(
        " OFFICE",
        " OFFICE LE",
        " OFFICE LE TIREKO",
        " ORGANIZATION",
        " ROJGAR DATA"
      ) ~ "7",

      v230a %in% c(
        " NEPAL SARAKAR",
        " NEPALI SARKAR"
      ) ~ "8",

      v230a %in% c(
        " THAHA XINA",
        " SASTHALE"
      ) ~ "2",

      TRUE ~ v230
    )
  ) %>%
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
  select(-v227h_1, everything(), v227h_1) %>%
  mutate(
    v230 = case_when(
      v230a %in% c(
        " HALF GOVERNMENT HALF 1700 AFULE ( AS AN FCHV)",
        " AAKHA SARKAR LE AADHA AAFAILE", " COMPANY LE",      
        " 50 PERCENT GOVERNMENT RA 50 PERCENT AAFAILE TIRNE",  
        " 50%GOVERNMENT LE .50% AFAI LE"
      ) ~ "3",
      TRUE ~ v230
    ),
    v232k = trimws(v232k),
    v232k = case_when(
    v232k %in% c("BHANEKO BELA PAISA NABHAYARA", "BIDESH KO BUDA AAYERA GARNE VANERA TETTIKAI VAKO XA.",
                "GARNAW MANN XAH TARA YEARLY NAI BHUJAUNW CHAI SAKINAW YEI VAYERA", "PAISA KO ABHAWA VAYER. PRIVETA HOSPITAL HARUMA LAGU NAVAYEKO LE",
                "PAISA NABHAYERA", "PAISA NABHAYERA TETI BELA BIMA GARNEY NAM TIPAUNEY BELA .",
                "PAISA VAYENAW PAXI", "WAHA LAI BIMA KO PAISA TIRNU SAKNU HUNNA VANERA VANNU VAAKO CHA",
                "YAKMUSTA PAISA NABHAYARA") ~ "1",

    v232k %in% c("/", "AABASYAK ABHAYARA", "AAFULE SAMAYA NANIKALEKOLE", "AAILE SAMMA JARURI NA SOCHERA",
                "ABA SSF GARNE BHANEKO CHHA OFFICE LE", "ABAW COMPANY LE GARIDINU HUNXA", "ABAW GARNI PROCESSING MA HUDAI XAH COMPANY BATAW",
                "ABAW PROCESSING HUDAI XAH", "ABAW PROCESSING MA RAHEKO XUH", "AGE 65BHAYO MA KARARKO KARMACHARI BHAYEKO LE",
                "ARMY KO BATA HUNX VANERA NAGAREKO", "ARMY KO COVER HUNE BHAYERA", "AWASYEK PARDA SAMJHINCHA NABHAYE BIRSHINCHA ANI YESARI GARNA BHYAYEKO CHAINA",
                "AYURVEDIC HOSPITAL MA BISWAS GARNU HUNEY VAAKO VAYERA BIMA NAGARNU VAAKO", "BANAUNA NAVYER NA BANAKO",
                "BIDESH THIYE 1 BARSA AGADI NAYA AAKO KURO THA VAYENA", "BIMA BATA K K SUBIDHA PAUCHA THA N BHAYAKO LE KAHA GAYAR GARNE HO TYO NE THA N BHAYAKO LE",
                "BIMA GARAUNU PARCHA VANERA THANAVAYEKO", "BIMA GARDA KATI PAISA LAGXA THAHA NAVAYEKO", "BIMA LIYE PANI KAAM NALAAGNEY VAYERA",
                "BIMA MA ABADHA HUNEY SAMAYA MA AFU GHAR MULI GHAR BATA TADHA RAHEKO TEI BHARA TES BARELY JANAKARI NAPAYEKO RA GHAR KO ARU SADASHYA BATA PANI JANAKARI NAPAKO,ANI TYO BELA MA TETI WASTA NAGAREKO.",
                "BINOD KUMAR GHARTI MAGAR HAD TAKEN OUT LIFE INSURANCE.NOW,AFTER SUFFERING A STROKE,HE HAS RECEIVED A TOTAL SUM OF 1 MILLION FROM THE LIFE INSURANCE.IT HAS BEEN ABOUT A MONTH SINCE HE RECEIVED THE AMOUNT.HE IS CURRENTLY USING THE SAME AMOUNT TO PAY FOR MEDICAL TREATMENT .APART FROM THAT,HE INFORMED THAT HIS EMPLOYER HAD ALSO SENT HIM SOME FUNDS FOR HIS TREATMENT.",
                "BIRAMI HUDA GARNU PARCHA BHANNE LAGCHA ANI PHERI BIRSIENCHA", "CARD BANEKO VAKHAR 2 DIN VAYO CHAALU HUNA 3 MONTHS LAGXA YO BARSA 2081 SAAL VARI CHAI VAYEKO THIYENA. (1 BARSA AGI KINA NAHUNU VAYEKO VANERA SODDA KHERI YO ANSWER)",
                "COMPANY LE SAHABHAGI NABANAYARA", "DOCUMENT PURA NAVAYAR", "DON'T KNOW ABOUT SSF", "EKLAII HUNX SAB KATA KATA BASXAN ANI TAII VAERA",
                "EKLAII VAERA NAGARAKO", "ENROLLMENT IS IN PROCESS", "FURSAD NABHAYERA", "GARCHU BHANDABHANDAI DHILA BHAYEKO",
                "GARNA MANN LAGEKO TARA PHURSAD NAVAYEKO KARAN LE", "GARNE BHANE FAMILY TOGETHER NA BHAYERA MILENA", "GARNE BHANNE SOCHA BHAYAKO BUT NAGAREKO",
                "GARNE PROCESS MA XU", "GHAR KO HAJUR AAMA MATRA JESTA NAGARIK BAT BIMA BHAYAKO", "GHAR PARIWAR KO SADASYA SANGA KURAKANI GARDA,SAMAYA ABHAB LEY GARNA FURSAD TATHA MESO NAHUNEY BHARA",
                "GHAR PARIWAR LE PACHI GARUMA BHANER N GAREKO", "GHARMA SIRMAN LE BIMA GARNA NAMANER", "HAL SALAI DHARMA PARIWARTAN GARNU BHAYEKO KARAN LEY GARDA TETI WASTA NABAHYEKO",
                "HAME LAI SSF KO KEHE KURA HARU TAHA N BHAYAR PANI HO", "HAMI LAI KEI THA XAINAW YESKO BAREMA KASAI LEY BHANEKO",
                "HE DOES NOT THINK IT'S OF ANY BENEFIT STILL IS OPEN TO IT IN FUTURE.", "HUSBAND AAUNU BHAYESI GARNE",
                "HUSBAND LE GARNA NAMANEKO", "HUSBAND POLICE HO HUSBAND KO BATA NAI HUNXA SEWA SUBIDA", "ICCHHYA NABHAYERA",
                "ICHYAA CHHA JANA K K MILIRAHEKO XAINA", "IN PROCESS ON ENROLLMENT. RECENTLY JOINED", "INTEREST NABHARA,JANAKARI NABHARA",
                "INTERSTATE NA VAYER PAISA CHAHIYEKO BELA MA NIKALNA MILDAI", "JESHTHA NAGRIK KO CARD BAT SAHULIYAT LIRAHEKO HUDA",
                "K K KURA MA SAHAYOGI HUNCHA R KASLE R KAHA GAYAR GARNE THA N BHAYAR", "KAHA KASARI GARNE TESKO FAIDA THA CHHAINA",
                "KAILE KASKO NAGRITA NAVETINE KAILE KASKO TEIVAERA NAGAREKO", "KAMM MA BESTA VAYERA",
                "LAAGU HUNA 3MAHINA LAAGNE XITO NAVAYERA NAGAREKO TURUNTA BIRAMI HUDA PAIYENA.", "LACK OF INFORMATION ABOUT HEALTH INSURANCE AND SOCIAL SECURITY FUNDS.",
                "MA AFU DHERAII JASO INDIA BASTHEE INIHARULEY GARENAXAN", "MA INDIAN ARMY BHAYAKOLE TEHI BATA SWASTHYA BIMA BHAYAKO HUDA AAWASYAK PARENA",
                "MAN NABHAYAR", "MANN NAVAYERA", "MERO SRIMA NAI MANDARIN BIMA GARNA LAI", "NAGARIKATA NABHAYEKO KARAN BUWA AAMA PATTA NALAGEKO",
                "NEPAL ARMY KO BIMA", "NEPAL ARMY MA HUNUBHAYEKO LE HEALTH SUBIDHA PRAPTA BHAIRAKO LE BIMA NAGAREKO TARA ABA GARNE BICHAR GAREKO.ARMY KO HEALTH SUBIDHA LINA TA-DA BHAYEKO LE.,",
                "PACHI GARUMA LA BHANNE SOCHERA", "PAHILA 6 BARSHE BIMA NISHULKA PAYEKO ABA YESHKO PAISHA NIRNUPARNE NAPARNE THANAI NAVAYERA",
                "PAHILA BIMA THIYO PACHHI PARIWARBATA CHHUTIAEPACHHI CHHUTAI AAFNAI PARIWARKO GARNE BHANEKO TARA GARNA MILDAINA BHANERA NAGAREKO",
                "PAILA PRIVATE LIFE INSURANCE GARNU BHAKO THIYO ANI UHA KO PARIWAR MA CHAI SCAM BHAKO BHAYERA UHA KO BISWASH GHAR VAYEKO KARAN LEY NAGARUNU BHAKO .",
                "PENDING", "PROCESS GARDAI XU", "PROCESS MA XU", "PROCESSING HUDAI XAH", "PROCESSING MA CHA. JUST ENROLLED,",
                "PROCESSING MA RAHEKO", "PROCESSING MA XUH", "RAMRO SANGA YESKO BAREMA KNOWLEDGE XAINW", "SAMAYE NAMILERA",
                "SHE HAD HEARD OF IT BUT DID NOT KNOW HOW SHE SAYS \"MALAI MESO BHAYENA\"", "SHREEMAN BIDESH BAT AAYAPAXI MATRA GARAUNU VNNERA",
                "SHREEMAN INDIAN ARMY BHAYAKOLE TEHI BATA SABAI GHAR PARIWARKO SWASTHYA BIMA BHAYAKO HUDA KHASAI AABASEK NAPARERA",
                "SUNEKO MATRAI XAH YES KO BAREMA KEI GYAN HARU TETI DHERAI NABHAYERA", "THA NAVAYERA", "THAHA CHHAINA",
                "THE ENROLLMENT IS IN PROCESS FOR KAMALA GHIMIRE,", "THEY ARE GOING TO ENROLL NOW,BUT TILL THIS POINT THEY NEVER GOT TO.",
                "WAITING FOR THE VISA SO,DIDN'T ENROLL IN INSURANCE", "YASMA ENROLLMENT HUNDA UPACHAR K K PAUCHA ARU K K HUNACHA",
                "YASKO BAREMA THA NAI CHHAINA", "YO BARSHA AABADHA HUNE NIRNAY GAREKA CHHAU", "YO BARSHA VARI MA BIMA GARNEY ANUMAN GARNU VAAKO CHA") ~ "2",

    v232k %in% c("- ARUKO GUNASO SUNERA PANI NAGAREKO.", "AAPATKALIN ABASTHAMA UPACHAR NAPAUNE HUDA,AUSADHIKO UPALABDHATA KO ABASTHA KHASAI RAMRO NADEKHERA,",
                "AAUSADHI HARU PANI SABAI PAUDAINA BHANER", "AAUSADHI SABAI NAPARNE VAYERA,DIN VARI SAMAYA LAGNE VARA,REFERL LE JHYAU",
                "AFULE USE GARNE MEDICINE KAHILE NAPAUNE,REFER KO SYSTEM JHANJHATILO,TIME DHERAI LAGNE", "ARU LE BHANEKO SUNER JHANJATILO BHAYAR",
                "ARUKO GUNASO SUNERA", "ARUKO KURA SUNERA", "BADHI SAMAYA KURNU PARNE BHAYARA",
                "BEEMA GAREKO MANXE LAI RAMRARI NAHERNE RA LINE BASNA PARNE AAUSADHI RAMRO NAPAUNE VAYEKO LE.", "BEEMA LE GARDA DHILAI HUNXA VANERA.",
                "BIMA GARAUNE HARU LE BHANEKO AAUSADHI PAUDAINA RE ANI HOSPITAL MA NE LINE BASNUN PARX RE DIN BHARI TEHI BHAYAR",
                "BIMA MA DHERAI JHANJATILO CHHA VANEKO SUNER NABANAYAKO", "DHERAI LINE BASNU PAERNE VAYERA.",
                "EMERGENCY CONDITION MA KAAM NALAGNE BHANERA BIMA GARNA CHAHINA", "ENGAGE HUNE SATHIHARU LE BHANEKO TETI RAMRO CHHAINA RE PENSON UPACHAR BHANER",
                "HOSPITAL GAYA PANI RAMRO GARDAINA BHANER N GAREKO", "JHANJITALO VAYERA LAIN BASNU PAERNE VAYERA",
                "LACK OF TRANSPARENCY IN THE PAYMENT PROCESS ANDQ UNAVAILABILITY OF THE HEALTH SERVICES WHEN THE INDIVIDUAL LEAVES THE SSF,",
                "LINE BASNA KO JHYAU LE", "PALO NAPAUNEE BHAYAR JHANJATILO VAYAR", "REFERRAL TIME LE DELAY HUNE BHAYERA ANI TIME MA SERVICE NA PAUNE BHAYERA WHEN IN EMERGENCY BUT THIS YEAR THEY ARE PLANNING TO",
                "SARKARI MA RAMRO SEWASUBIDHA NA VAYEKO BIMA GARAUNE LAI LAMO LINE LAGNU PARNE", "SEWA SUBIDHA DHILO PAYAKO SUNERA",
                "SHE DOES NOT THINK BIMA IS BENEFICIAL FOR PEOPLE LIKE HER WHO LIVE IN MANANG", "SWASTHYA BINA JHANJHATILO HUNE BHANNE SUNEKO LE",
                "UPACHAR RAMRO N HUNE BHAYAKO R LINE BASNA JHANJHAT") ~ "3",

    v232k %in% c("KHI SAMAYA PAHILA SAMMA BARAMBAR BIRAMI BHAINE SAMASYAA NABHAYARA.", "TESTO DIRGHA ROGE KEHE N BHAYAR") ~ "4",

    v232k %in% c("- PAHILA GARNE VANDA BEEMA BAAREY RAMRO JAANKARI PAIYENA.", "AAFU LE KHANE AUSHADHI BIMA AT UPLABDH NAHUNE VAYER. SAMAY MA SEWA LIN NAPUNE VAKO,",
                "BIMA GAREKO MANCHHELAI HOSPITAL MA KHASAI RAMRO BEBAHAR NAHUNE BHAYARA.", "BIMA GAREPANI AAFNO KHARCHA LAGNI BHAAEKOLE",
                "DHERAI LINE BASNU PARNE,EMERGENCY PARDA BIMA KO BIRAMI LAI TATKAL UPACHAR NAGARNE RA BIMA KO PATIENT LAI HOSPITAL KO KARMACHARI HARULE RAMRO BYABAHAR NAGARNE HUNALE.",
                "GARNAW MANN XAH HOSPITAL DHERAI TADA XAG", "HOSPITAL JADA HEALTH INSURANCE SEWA LINA JANE BIRAMI HARUKO LINE RA DABAI NAPAYAKO GUNASO SUNERA.",
                "HOSPITAL MA LINE DHERAI BHAYAR NAGAREKO,", "KEHI BARSHA AGHI BIMA GAREKO HOSPITAL MA UPACHAR GARNA JADA SWASTHYA KARMI HARULE RAMRO BEBAHAR NAGAREKALE RENO NAGAREKO",
                "LINE BASNU PARNE SUNERA,", "LINE DHERAI BASNU PARNE SAMAYA NABHAYAR NA GAREKO", "LINE DHERAI BASNU PARNE,AAUSADHI NA PAINE SUNERA",
                "LINE DHERAI BASNU PARX VANERA NA GAREKO", "LINE HUNXA VANNE JHANJHAT LE,SABAI AAUSADHI NAVAYERA", "MAHANGO AUSADHI HARU NADINEY ANI SASTO MATRAI DINEY.",
                "SABAI UPACHAR GARNA BAHIRA PATHAUNI,MAHANGO AAUSADHI NADINE SASTO MATRA DINE VAYEKO LE,TIME LAGNE VAYERA",
                "SWASTHYA UPACHAR JHANJHATILO HUNE BHANNE SUNEKO LE R BIMA HUNDA NE AAUSADHI BAHIRA KINNU PARNE BHAYAKO LE",
                "UPACHAR RAMRO GARDAINA RE BHANNE SUNER AAUSADHI SABAI DENDAINA BHANEKO LE", "UPACHAR RAMRO HUNDAINA BHANER LINE BASNUN PARNE BHANEKO AAUSADHI N HUNE",
                "UPACHAR RAMRO HUNDAINA LINE BASNUN PARNE RE 3/4 HOURS", "UPACHAR SAHAG CHHAINA AAUSADHI N PAUNE") ~ "5",

    v232k %in% c("4 JANA KO BEEMA NAGARNU KO KARAN YESTO XA - BEEMA GARNE VANDA VANDAI BEEMA GARAUNE MANXE NAAKO PAXI AAUDA YESTAI SAMAYA NAMILERA NAGAREKO GARNA MAN NAVAYERA HOINA TARA 2 JANA SENIOR CITIZEN KO CHAI PAALIKA LE NISULKA GARDERA GAREKO.",
                "AB GARNE HO YASPALI", "AB INSURANCE MA AABADDHA HUNE PLAN GAREKA CHHAU", "ABA GARNE BICHAR CHA", "ABA GARNE HO",
                "APPLY GAREKO CHAU ABA KO 2/3MAHINAMA START HUNCHA HOLA", "ARKO MONTH DEKHI ABADDHA MITI SURU HUNEY",
                "BIMA GARAUNE MANXE LAI BOLAUDA PANI NA AAYAR", "BIMA GARNA AAUDA KHET MA GAYAKO LE VET N BHAYAR N GAREKO",
                "BIMA GARNE KO HO KASARI GARNE KE THAHA NABHAYAR .BIMA KO FAIDA NI THAHA THIYAN", "BIMA GARWNU VKO TARA CARD AAKO XAINA,SADHI PARIWAR BHANERA DARTHA GAREKO",
                "BIMA KAHA GARNE THAHA NAVYER", "BIMA KARTA BIMA GARNA NAAAIDIYAKO", "BIMA KASALE BANAUXA THAHA NAI THIYEN.",
                "BIMA KASLE GARAUNE THA N BHAYAR", "BIMA RENEW GARNA DARTA SAHAYOGI GHARMA NAAUNE", "BIWAHA DARTA HAR KO THIYO RA BACHHAHARU JANM DARTA NA VAYEKO LE GARDA",
                "BOLAUDA GARNA AYENA AND HE ALSO HAS HEARD MANY COMPLAINS ON HEALTH INSURANCE WHOCH LED HIM TO DECIDE IT ISNT MUCH OF USE.",
                "DARTA SAHAYOGI KO ABHAB BHAYERA RENEW GARNA NAPAYEKO", "DARTA SAHAYOGI LE BELAIMA KHABAR NAGARETA", "DON'T KNOW ABOUT THE RENEWAL PROCESS",
                "GARNE PROCESS MA XU GARXU", "GARNI PROCESS MA XUH", "GARNI TARA PAXI", "GARNI VANDHAI RAHEKO", "GARXUH GARXUH VANDHAI RAYO",
                "GARXUH VANA VANDHAI RAHEKO", "GARXUH VANDHA VANDHAI RAHEKO", "GARXUH VANDHAI RAHEKO", "GARXUH VANDHAI RAHEKO TARA YETA BIMA KO KEI HOSPITAL XAINAW",
                "HEALTH INSURANCE GARAUNE BHANERA PAISA LIYERA TYO MANCHHE BHAGEKO HUNALE NI DAR LAGYO.", "JHANJATILO BHAYERA", "JHANJHATILO BHAYARA",
                "JHANJHATILO CHA BHANERA", "MERO TIME NAMILEKO BELA DARTA SAHAYOGI KO TIME NAMILERA ANI DARATA SAHYOGI KO TIME MILEKO BELA MERO NAMILERA",
                "NEPAL KO NAGARIKTA CHHAINE", "RAMRO UPACHAR HUNDAINA BHANNE SUNEKO R BIMA GARNA KOI N AAYAKO YAHA",
                "RENEW GARNAU NEY BAREY JANKARI NABHAKO", "SHE WENT TO GET SERVICES BUT FOR SOME REASON SHE COULDN'T UTILIZE IT THAT YEAR AFTER THAT SHE CLAIMED NO ONE CAME FOR RENEWAL.",
                "THEY MISSED ENROLLMENT OFFICER DUE TO BEING AWAY IN KHET", "TIME NA BHIYAR MATRA NAGAREKO", "TIME NA BHIYAUNE BHAYAR BIMA MA DHERAI LINE BASNU PAREN BHAYAR",
                "TIME NAI NAVAYERA", "WADA MA GAYER DHERAI PATAK BIMA MA AABADDHA HUN KHOJEKO TAR WADA BAT SAHAYOG NA PAYEKO.",
                "WAITING GARDA GARO VAYERA", "YAHAKO BATO TA YATAYAT KO RAMRO PAHUCHA GARI VAYOKO.REFER MA JHANTHAT VAYER") ~ "6",

    v232k %in% c("PAHILE VARNA VAYAKO TARA AHILE RENEW 2 BARSA VAYO NAGAREKO", "PAILA CHAI", "PAILA GARERA XODEKO",
                "RENEW NAGAREKO", "RENEW NAGAREKO") ~ "8",

    v232k %in% c("JAGIR THAI NAVAYEKO KARAN", "JANKARI NAVAYERA VRKHAR OFFICE JOIN VAYEKO LE 1 YRS MATRA VAYEKO XA JOINE VAYEKO",
                "KAM GAREKO THAU MA KURA HUDAI XA GARDINU HUNXA HOLA", "KAM MA AAYAR PACHI YAHA N BHAYAKO LE", "KHOI KEHE KURA THA CHHAINA SSF KO",
                "M LAI YO SSF BHANEKO K HO TAHA PANI CHHAINA", "OFFICE BATA SSF GAREDEYA HAME GARNA CAHANCHUM R SSF KO BAREMA JANKARI NE CHHAINA",
                "OFFICE JOINE VAYEKO 1YR MATRA VAYEKO VARNA PROCESS MA RAHEKO", "OFFICE KO KARMACHARI HARULE NAGERKO",
                "OFFICE LE GARNE PROCESS MA XA 1YRS MATRA VAYO OFFICE JOIN GAREKO.", "OFFICE LE GARNU VANEKO XAH200", "OFFICE LE PROCESS GARDAI XA",
                "OFFICE LE PROCESS GARDAI XA TARA VAYEKO XINA", "OFFICE LE PROCESS GERDAI XA VAYEKO XINA.", "OFFICE LE PROCESS GRADHAI XAH",
                "OFFICE MA LAGU NAI BHAYAKO CHHAINA", "ON THE PROCESS", "PAHELA SSF MA AABADHA BHAYAKO BUT SEWA SUBIDHA MAN NAPARERA CHODEKO.",
                "PAHILE KO JOB KO BATA FREE HEALTH CHECKUP HUNE BHAYERA BIMA MA AABADDA NABHAYEKO", "PROJECT HARU SHORT TERM VAERA",
                "RECENTLY STARTED WORKING AND ENROLLMENT IN SSF IS IN PROCESS,", "SALARY THORAI XA SSF MA DARTA HUDA JHAN THORAI HUNXAH ANI NAGAREKO",
                "SAMAJIK SURAKSHA KUSA VAYEKO LE HAMILAI AAWASYAKTA NAVAYE KO", "SSF BHAYAR. AABASEK NATHANERA", "SSF FAIDA HARU THA N BHAYAR",
                "SSF HUNCHA BHANNE TAHA N BHAYAR", "SSF KO BAREMA CLEAR KURA THA NA BHAYAKO LE", "SSF KO BAREMA KEHE THA NAI CHHAINA R OFFICE LE NE KEHE BHANEKO CHHAINA TEHI BHAYAR",
                "SSF KO BAREMA KHASAI BUJHEKO CHHAINA", "SSF KO BAREMA SUNEKO T HO TARA AJHA DETAILS THA NAI CHHAINA HAMRO LAGI K K HUNCHA BHANER",
                "SSF KO BAREMA THA CHHAINA", "SSF KO BAREMA THA NAVAYERA", "SSF KO BAREMA THA NE N BHAYAR HO", "SSF KO BAREMA THA XAINAW",
                "SSF KO FAIDA HARU KO KURA NAI THA BHAYAN", "SSF KO FAIDA HARU THA N BHAYAR PANI HO", "SSF KO FAIDA HARU THA NBHAYAKO LE",
                "SSF KO KEHE KURA HARU TETI MAN N PARER JASTI UPACHAR KO KURA HARU", "RETIREMENT KO KURA LE",
                "SSF MA PAHILA DEKHI HUNE SATHI HARU LE YASKO KEHE FAIDA CHHAINA BHANEKO LE N BASEKO", "THAHA NA PAYEKO SSF BANAUNA PARXA RA YASKO FAIDA BARE MA THAHA NA VAYEKO",
                "THE EMPLOYEE ASKED THE COMPANY TO GIVE THE SOCIAL SECURITY FUND (SSF) SCHEME.", " BUT THE COMPANY HAS NOT REPLIED TO THE REQUEST",
                "VRKHAR OFFICE JOINE GAREKO OFFICE LE GARNE PROCESS MA XA", "YO BAREMA THA NAI CHHAINA R OFFICE NE KEHE BHANEKO CHHAINA",
                "YO OFFICE MA LAGU BHAYAKO CHHAINA AFU LE KASARI KAHA GARNE THA N BHAYAR", "YO SSF KO BAREMA KEHE THA CHHAINA",
                "YO SSF KO BAREMA KEHE THA NAI CHHAINA") ~ "9",

    v232k %in% c("BECAUSE THEY DO DIRECTLY TO PRIVATE HOSPITALS OF KATHMANDU.", "INDIA BATA AUSADHI LERAYAKHANE", "NO NEED TO YET") ~ "10",

    TRUE ~ v232k
    ),
    v233j = trimws(v233j), 
    v233j = case_when(
    v233j %in% c("AFNO UPACHAR R PACHI KO LAGI BACHAT HUNCHA BHANER", 
                 "COMPANY LE GARDEKO") ~ "1",

    v233j %in% c("HEALTH INSURANCE SHOULD BE DONE FOR EMERGENCY", 
                 "KUNAI DURGHATANA BHAYO BHANE SAHAYOG HUNCHHA.", 
                 "६० वर्षपछि पेन्सन आउँछ भनेर") ~ "2",

    v233j %in% c("60YRS PAXI RAMRO HUNXA VANERA", "BIMA MA AABADDHA VAYEKO AAJAI THAHA PAYE", 
                 "BIMA MA AABADHA CHHU", "COMPANY INITIATION", "OLD AGE VAYARA PAXI GARNU PARXA VANARA", 
                 "PACHHIKO LAGI PAISA BACHAT HUNE DEKHRA", "PACHHINKO PANSON KO LAGI", 
                 "PACHI BHABISYA MA RAMRO HUNCHA BHANER", "PACHI LAI RAMRO HUNCHA BHANER", 
                 "PAXI VBISAYA KO LAGI SECURED HUNXA VANERA", "HOSPITAL MA LAMO SAMAYA LAGNE BHAYAKO LE", 
                 "HAME JASTO BUDA BUDI LAI SAJILA UPACHAR PAINCHA BHANER") ~ "3",

    v233j %in% c("AUSADHIKO UPALABDHATA HUNCHHA BHANERA HO TARA PAIDAINA", "BACHAT HUNCHHA BHANER", 
                 "GARNE BHANDA BHANDAI GAREKO CHAINA", "GHAR MA BUDHA BUDHI AMA BUWA HUNU BHAYAKO LE UPACHAR KO LAGI GAREKO", 
                 "JACH PANI GARINE BHAYERA", "KHARCHA DHERAI N HOS BHANNU KO LAGI", 
                 "OFFICE LE BHANER YASMA SSF MA GAYAKO", "SINCE,THE COMPANY STARTED", 
                 "SSF GARDEKO BHAYERA", "SSF MA AABADH VAYEKO LE", "SSF VAYEKO LE", 
                 "SWASTHYA UPACHAR MA SUBIDH HUNX BHANER", "TIME MILAUNA NASAKRA USE NAGAREKO", 
                 "UPACHAR KHARCHA DHERAI HUNDAINA", "UPACHAR KHARCHA KUM LAGACHA BHANER", 
                 "UPACHAR SAJILO HUNX BHANER", 
                 "बाबुको लागि स्कुलले गरिदिएको र परिवारको लागि भन्ने साथै पैसा निकाल्न गाह्रो भएकोले नगरेको") ~ "4",

    v233j %in% c("HE BELIEVES INSURANCE SHOULD BE DONE BY EVERYONE,WHETHER THEY END UP USING IT OR NOT,AS IT IS FOR THE ONES WHO MIGHT NEED IT AND ANYONE CAN BE IN THAT POSITION.", 
                 "PALIKA LE BANAUNU PARXA VANER BANAKO", "SWOYAMSEBIKA SABAILAI GARNA PARXA VANERA", 
                 "बिमा कर्ता ले गरिदिएको यसको बारेमा केही पनि थाहा नभएको") ~ "5",

    v233j %in% c("AAFU LAI KAI SAHAJ HUNXA VANERA", "AAFULE BACHAT RAKHEKO BHAYAKO LE 1400 LE FAMILY KO NAI BIMA HUNCHHA BHANERA", 
                 "GHAR MA SANO SANO BACHHA HARU BHAYAKO LE KHARCHA DHERAI HUNE BHAYAKO LE", "N/A", "NO", 
                 "UPACHAR GARNA POISA N HUNE LAI SAHAJ HUNCHA BHANER", 
                 "UPACHAR GARNA SAJILO N NESULKA HOLA BHANER") ~ "6",

    v233j %in% c("SUBIDA RAMRO XA VANERA") ~ "9",

    v233j %in% c("ALSO BECAUSE PALIKA DID IT FOR THEM FOR FREE", "BABU RED CARD WALA DISABLED CHILD", 
                 "FCHV BHAYAR FREE BHAYAKO LE", "FCHV KO FAMILY LAI RA JESTA NAGARIK LAI FREE MA INSURANCE GARAUNE HUNALE", 
                 "FCHV LAI ANIBARYA HEALTH INSURANCE SEWA PRADAN GAREKO CHHA", 
                 " HALF GOVERNMENT HALF 1700 AFULE ( AS AN FCHV)",
                 "GHAR MA AAPANGATA BHAYAKO PATIWAR BHAYAR R UPACHAR GARNA KO LAGI", 
                 "GHARMULIKO CHHORI FCHV HUNUHUNTHYO ANI BUBA RA AMA LAI PANI JOIN GARERA BIMA GARAKO", 
                 "JESTA NAGARIK KO BIMA SARKAR LE GARAIDIYEKOLE", "JESTHA NAGARIK FREE", 
                 "JESTHA NAGARIK VAYAKO LE BANEKO,FAMILY KO ARU KASAIKO PANI BIMA XAINA", 
                 "NAGARPALIKA LE JESTA NAGARIK KO NISULKA HEALTH INSURANCE GAREKOLE", 
                 "PALIKA LE FREE MA GARDIYA KO LE", "SARKAR BATA BIMA GARIDINUBHAKO LE", 
                 "SARKAR BATA FCHV LAI ANIBARYA HEALTH INSURANCE SEWA DINE BHAYERA", 
                 "SARKAR LE GARDEKO LE", "SARKAR LE GARDINE BHAYERA PAUNE SUBIDHA LI RAHEKO", 
                 "SARKAR LE GARIDINE BHAYERA", "SARKARI LE GARIDIYERA", "SAVING HUNE BHAYARA", 
                 "SCHOOL LE GARIDEYAKO", "VULNERABLE POPULATION AS IT WAS NEAR DUMPING SITE.", 
                 "कम्पनी आफैंले गरिदिएको") ~ "11",

    TRUE ~ v233j
    )
  )

for (i in setdiff(1:ncol(section2b), c(2, 7, 8, 14:21, 23, 35, 45, 82))) {
  section2b[[i]] <- as.numeric(gsub("[^0-9]", "", section2b[[i]]))
}

section2b <- section2b %>%
  mutate( 
    v228 = if_else(
      is.na(v229), 
      2, 
      1
    )
  )

#SECTION2C

section2c <- section2c %>%
  mutate(
    v260_num = grepl("[0-9]", v260a),

    v260_new  = if_else(v260_num, v260a, v260),
    v260a_new = if_else(v260_num, v260,  v260a),

    v260  = v260_new,
    v260a = v260a_new
  ) %>%
  select(-v260_num, -v260_new, -v260a_new) %>%
  mutate(
    v261_num = suppressWarnings(as.numeric(str_extract(v261, "\\d+"))),

    v261_txt = str_trim(
      str_remove_all(v261, "\\d+|,")
    ),

    v261  = v261_num,
    v261a = if_else(v261_txt != "", v261_txt, as.character(v261a))
  ) %>%
  select(-v261_num, -v261_txt)


for (i in setdiff(1:ncol(section2c), c(2, 8, 7, 13, 17, 19))) {
  section2c[[i]] <- as.numeric(gsub("[^0-9]", "", section2c[[i]]))
}

section2c <- section2c %>%
  mutate(
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
        "UMERA PUGER BITNU BHAYEKO", " OLD AGE"
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

#SECTION3A

for (i in setdiff(1:ncol(section3a), c(2, 8, 7))) {
  section3a[[i]] <- as.numeric(gsub("[^0-9]", "", section3a[[i]]))
}

section3a <- section3a %>%
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

section3a <- section3a %>%
  mutate(
    across(v303:v305, ~ na_if(.x, 0))
  ) %>%
   mutate(
    v305 = case_when(
      v301 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) & v304 == v305 ~ NA_real_,
      TRUE ~ v305
    )
  )

section3a  <- section3a %>%
  group_by(psu, v301) %>%
  mutate(
  v303 = case_when(
    v301 == 1 & v302 == 1 & v303 > 1500 ~ mean(v303[v303 <= 1500], na.rm = TRUE),
    TRUE ~ v303
  ), 
  v304 = case_when(
    v301 == 1 & v302 == 1 & v304 > 1500 ~ mean(v304[v304 <= 1500], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v304 = case_when(
    v301 == 2 & v302 == 1 & v304 > 400 ~ mean(v304[v304 <= 400], na.rm = TRUE),
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 2 & v302 == 1 & v303 > 400 ~ mean(v303[v303 <= 400], na.rm = TRUE), 
    TRUE ~ v303
  ), 
  v304 = case_when(
    v301 == 3 & v302 == 1 & v304 > 1000 ~ mean(v304[v304 <= 1000], na.rm = TRUE),
    TRUE ~ v304
  ), 
  v303 = case_when(
    v301 == 3 & v302 == 1 & v303 > 500 ~ mean(v303[v303 <= 500], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 4 & v302 == 1 & v304 > 500 ~ mean(v304[v304 <= 500], na.rm = TRUE),
    TRUE ~ v304
  ), 
  v303 = case_when(
    v301 == 4 & v302 == 1 & v303 > 500 ~ mean(v303[v303 <= 500], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 5 & v302 == 1 & v304 > 500 ~ mean(v304[v304 <= 500], na.rm = TRUE),
    TRUE ~ v304
  ), 
  v303 = case_when(
    v301 == 5 & v302 == 1 & v303 > 500 ~ mean(v303[v303 <= 500], na.rm = TRUE),
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 6 & v302 == 1 & v304 > 300 ~ v304/10,
    TRUE ~ v304
  ), 
  v303 = case_when(
    v301 == 6 & v302 == 1 & v303 > 300 ~ v303/10,
    TRUE ~ v303
  ), 
  v304 = case_when(
    v301 == 7 & v302 == 1 & v304 > 500 ~ mean(v304[v304 <= 500], na.rm = TRUE), 
    TRUE ~ v304
  ), 
  v303 = case_when(
    v301 == 7 & v302 == 1 & v303 > 500 ~ mean(v303[v303 <= 500], na.rm = TRUE), 
    TRUE ~ v303
  ), 
  v304 = case_when(
    v301 == 8 & v302 == 1 & v304 > 600 ~ mean(v304[v304 <= 600], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 8 & v302 == 1 & v303 > 300 ~ mean(v303[v303 <= 300], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 9 & v302 == 1 & v304 > 500 ~ mean(v304[v304 <= 500], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 9 & v302 == 1 & v303 > 500 ~ mean(v303[v303 <= 500], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 10 & v302 == 1 & v304 > 300 ~ mean(v304[v304 <= 300], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 10 & v302 == 1 & v303 > 100 ~ mean(v303[v303 <= 100], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 11 & v302 == 1 & v304 > 100 ~ mean(v304[v304 <= 100], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 11 & v302 == 1 & v303 > 50 ~ mean(v303[v303 <= 50], na.rm = TRUE), 
    TRUE ~ v303
  ), 
  v304 = case_when(
    v301 == 12 & v302 == 1 & v304 > 250 ~ mean(v304[v304 <= 250], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 12 & v302 == 1 & v303 > 50 ~ mean(v303[v303 <= 50], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 13 & v302 == 1 & v304 > 2000 ~ mean(v304[v304 <= 2000], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 13 & v302 == 1 & v303 > 100 ~ mean(v303[v303 <= 100], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 14 & v302 == 1 & v304 > 300 ~ mean(v304[v304 <= 300], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 14 & v302 == 1 & v303 > 20 ~ mean(v303[v303 <= 20], na.rm = TRUE), 
    TRUE ~ v303
  ),
  v304 = case_when(
    v301 == 15 & v302 == 1 & v304 > 350 ~ mean(v304[v304 <= 350], na.rm = TRUE), 
    TRUE ~ v304
  ),
  v303 = case_when(
    v301 == 15 & v302 == 1 & v303 > 50 ~ mean(v303[v303 <= 50], na.rm = TRUE), 
    TRUE ~ v303
  )
  ) %>%
  ungroup() %>%
  group_by(v301) %>%
  mutate(
  v304 = case_when(
    v301 == 1 & v302 == 1 & is.na(v303) & is.na(v304) & is.na(v305) ~ mean(v304[v304 <= 1500], na.rm = TRUE),
    TRUE ~ v304
  ), 
  v303 = case_when(
    v301 == 6 & v302 == 1 & v303 > 300 ~ mean(v303[v303 <= 300], na.rm = TRUE),
    TRUE ~ v303
  ), 
  v304 = case_when(
    v301 == 6 & v302 == 1 & v304 > 300 ~ mean(v304[v304 <= 300], na.rm = TRUE),
    TRUE ~ v304
  )
  ) %>%
  ungroup()
  
section3a <- section3a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    v304 = case_when(
      hhid %in% c("3341-3","3421-4","3602-1","3622-2","6309-1", "3502-3", "7308-3") & v301 == 1 ~ 500,
      hhid %in% c("3341-3","3421-4","3602-1","3622-2","6309-1", "3502-3", "7308-3") & v301 == 2 ~ 200,
      TRUE ~ v304
    )
  )

section3a <- section3a %>%
  mutate(
    v302 = case_when(
      (is.na(v303) | is.nan(v303)) & (is.na(v304) | is.nan(v304)) & (is.na(v305) | is.nan(v305)) ~ 2, 
      TRUE ~ 1
    )
  )

#SECTION3B

for (i in setdiff(1:ncol(section3b), c(2, 8, 7))) {
  section3b[[i]] <- as.numeric(gsub("[^0-9]", "", section3b[[i]]))
}

section3b <- section3b %>%
  mutate(
    v307 = case_when(
      (is.na(v308) | v308 == 0) &
      (is.na(v309) | v309 == 0) ~ 2,
      TRUE ~ 1
    ),
    across(v308:v309, ~ na_if(.x, 0))
  )


section3b <- section3b %>%
  group_by(psu, v306) %>%
  mutate(
    v308 = case_when(
      v306 == 4 & v307 == 1 & v308 > 150 ~ round(mean(v308[v308 <= 150], na.rm = TRUE)),
      TRUE ~ v308
    ), 
    v309 = case_when(
      v306 == 4 & v307 == 1 & v309 > 150 ~ round(mean(v309[v309 <= 150], na.rm = TRUE)),
      TRUE ~ v309
    ),
    v308 = case_when(
      v306 == 6 & v307 == 1 & v308 > 300 ~ round(mean(v308[v308 <= 300], na.rm = TRUE)),
      TRUE ~ v308
    ), 
    v309 = case_when(
      v306 == 6 & v307 == 1 & v309 > 300 ~ round(mean(v309[v309 <= 300], na.rm = TRUE)),
      TRUE ~ v309
    ),
    v308 = case_when(
      v306 == 7 & v307 == 1 & v308 > 2000 ~ round(mean(v308[v308 <= 2000], na.rm = TRUE)),
      TRUE ~ v308
    ), 
    v309 = case_when(
      v306 == 7 & v307 == 1 & v309 > 2000 ~ round(mean(v309[v309 <= 2000], na.rm = TRUE)),
      TRUE ~ v309
    ),
    v308 = case_when(
      v306 == 8 & v307 == 1 & v308 > 1000 ~ round(mean(v308[v308 <= 2000], na.rm = TRUE)),
      TRUE ~ v308
    ), 
    v309 = case_when(
      v306 == 8 & v307 == 1 & v309 > 500 ~ round(mean(v309[v309 <= 2000], na.rm = TRUE)),
      TRUE ~ v309
    )     
  ) %>%
  ungroup()

#SECTION4A

for (i in setdiff(1:ncol(section4a), c(2, 8, 7))) {
  section4a[[i]] <- as.numeric(gsub("[^0-9]", "", section4a[[i]]))
}

section4a <- section4a %>%
  mutate(
    v402 = case_when(
      (is.na(v403a) | v403a == 0) &
      (is.na(v403b) | v403b == 0) ~ 2,
      TRUE ~ 1
    ), 
    v403a = case_when(
      v403a == 30000083 ~ 3000,
      ID == 9092 & v401 == 3 ~ 6000, 
      TRUE ~ v403a
    ),
    v403b = case_when(
      ID == 4581 ~ 1200,
      ID == 3058 ~ 10000, 
      TRUE ~ v403b
    ), 
    across(v403a:v403b, ~ na_if(.x, 0))
  )

section4a <- section4a %>%
  group_by(psu, v401) %>%
  mutate(
    v403a = case_when(
      v401 == 1 & v402 == 1 & v403a > 100000 ~ round(mean(v403a[v403a <= 100000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 1 & v402 == 1 & v403b > 20000 ~ round(mean(v403b[v403b <= 20000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 2 & v402 == 1 & v403a > 25000 ~ round(mean(v403a[v403a <= 25000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 2 & v402 == 1 & v403b > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 3 & v402 == 1 & v403a > 1000000 ~ round(mean(v403a[v403a <= 1000000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 3 & v402 == 1 & v403b > 1000000 ~ round(mean(v403b[v403b <= 1000000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 4 & v402 == 1 & v403a > 126000 ~ round(mean(v403a[v403a <= 126000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 4 & v402 == 1 & v403b > 11000 ~ round(mean(v403b[v403b <= 11000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      ID == 12142 & v401 == 5 ~ 600000,
      TRUE ~ v403a
    ), 
    v403a = case_when(
      ID == 12142 & v401 == 6 ~ 150000,
      ID == 2863 & v401 == 6 ~ 150000,
      TRUE ~ v403a
    ),
    v403a = case_when(
      v401 == 8 & v402 == 1 & v403a > 100000 ~ round(mean(v403a[v403a <= 100000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 8 & v402 == 1 & v403b > 10000 ~ round(mean(v403b[v403b <= 10000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 9 & v402 == 1 & v403a > 75000 ~ round(mean(v403a[v403a <= 75000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 9 & v402 == 1 & v403b > 12000 ~ round(mean(v403b[v403b <= 10000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 10 & v402 == 1 & v403a > 25000 ~ round(mean(v403a[v403a <= 25000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 10 & v402 == 1 & v403b > 2000 ~ round(mean(v403b[v403b <= 2000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      ID == 6874 & v401 == 12 & v402 == 1 ~ 270000, 
      TRUE ~ v403a
    ), 
    v403a = case_when(
      v401 == 13 & v402 == 1 & v403a > 40000 ~ round(mean(v403a[v403a <= 40000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 13 & v402 == 1 & v403b > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE)),
      TRUE ~ v403b
    ),
    v403a = case_when(
      v401 == 14 & v402 == 1 & v403a > 25000 ~ round(mean(v403a[v403a <= 25000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 14 & v402 == 1 & v403b > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE)),
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
      v401 == 18 & v402 == 1 & v403a > 70000 ~ round(mean(v403a[v403a <= 70000], na.rm = TRUE)), 
      TRUE ~ v403a
    ),
    v403b = case_when(
      v401 == 18 & v402 == 1 & v403b > 10000 ~ round(mean(v403b[v403b <= 10000], na.rm = TRUE)), 
      TRUE ~ v403b
    ), 
    v403a = case_when(
      v401 == 19 & v402 == 1 & v403a > 35000 ~ round(mean(v403a[v403a <= 35000], na.rm = TRUE)), 
      TRUE ~ v403a
    ),
    v403b = case_when(
      v401 == 19 & v402 == 1 & v403b > 25000 ~ round(mean(v403b[v403b <= 25000], na.rm = TRUE)), 
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
      v401 == 22 & v402 == 1 & v403a > 50000 ~ round(mean(v403a[v403a <= 50000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 22 & v402 == 1 & v403a > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE))
    ), 
    v403a = case_when(
      v401 == 22 & v402 == 1 & v403a > 50000 ~ round(mean(v403a[v403a <= 50000], na.rm = TRUE)),
      TRUE ~ v403a
    ), 
    v403b = case_when(
      v401 == 22 & v402 == 1 & v403a > 5000 ~ round(mean(v403b[v403b <= 5000], na.rm = TRUE)), 
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
  )

section4a <- section4a %>%
  mutate(
    v403b = case_when(
      v401 == 21 & v402 == 1 & v403a == v403b ~ NA_real_,
      TRUE ~ v403b
    )
  )

#SECTION4B

for (i in setdiff(1:ncol(section4b), c(2, 7, 8))) {
  section4b[[i]] <- as.numeric(gsub("[^0-9]", "", section4b[[i]]))
}

section4b <- section4b %>%
  mutate(
    v404 = case_when(
      v404 == 2 & v407a %in% c(40000, 2120000, 4e+05) ~ 1,
      TRUE ~ v404
    )
  )

section4b <- section4b %>%
  group_by(psu, v405) %>%
  mutate(
    across(
      c(v407a, v407b),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE))

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

#SECTION 4C 

for (i in setdiff(1:ncol(section4c), c(2, 7, 8))) {
  section4c[[i]] <- as.numeric(gsub("[^0-9]", "", section4c[[i]]))
}

section4c <- section4c %>%
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
    commodity_mean = round(mean(v412a[v412a > 0 & v412a <= 29], na.rm = TRUE)),
    v412a = if_else(
      (v412a > 29 | (v412a == 0 & v412b > 0)) & !is.nan(commodity_mean),
      commodity_mean,
      v412a
    )
  ) %>%
  ungroup() %>%
  select(-commodity_mean)

section4c <- section4c %>%
  group_by(psu, v408) %>%
  mutate(
    across(
      c(v410, v411a, v411b, v411a, v411b, v413),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE))

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

#SECTION4D

for (i in setdiff(1:ncol(section4d), c(2, 7, 8))) {
  section4d[[i]] <- as.numeric(gsub("[^0-9]", "", section4d[[i]]))
}

section4d <- section4d %>%
  group_by(psu, v414) %>%
  mutate(
    across(
      c(v416a, v416b),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE))

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

section4d <- section4d %>%
  mutate(
    v415 = if_else(
      (is.na(v416a) & is.na(v416b)),
      2, 
      1
    )
  )

#SECTION5 

for (i in setdiff(1:ncol(section5), c(2, 7, 8))) {
  section5[[i]] <- as.numeric(gsub("[^0-9]", "", section5[[i]]))
}

section5 <- section5 %>%
  mutate(
    v501 = case_when(
      (is.na(v502e) | v502e == 0) ~ 1,
      (v501 == 96 & v502e > 0) ~ 2,
      TRUE ~ v501
    ), 
    v503 = case_when(
      v504 > 0 ~ 1, 
      TRUE ~ 2
    )
  )

section5 <- section5 %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v502a, v502b, v502c, v502d, v502e, v502f, v502g),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE))

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

#SECTION6

section1a <- section1a %>%
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
    ),
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

section6a <- section6a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

section6a <- merge(
  section6a,
  section1a[, c("uniq_id", "age_group")],
  by = "uniq_id" 
)

section1a <- section1a %>%
  select(-age_group)

section6a <- section6a %>%
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

#SECTION6B1

section6b1 <- section6b1 %>%
  mutate(
    v604a = trimws(v604a),
    v604 = case_when(
    
    v604a %in% c("MUTUROG", "COLESTEROL", "CHLOSTROAL", "CHOLEDTEROL", 
                 "CHORESTEROL", "COLDSTORE", "COLESTER", "COLESTROME", 
                 "COLSTORE", "COLSTRORE", "कोलेस्ट्रोल", "CHOLESTEROL") ~ "1",
    
    v604a %in% c("BP", "PRESSURE", "PRESSURE RA MANASIK ROG", 
                 "PRESSURE/SUGAR/THYROID", "LOW BLOOD PRESSURE", "96") ~ "2",
    
    v604 == "96" & v604a %in% c("") ~ "2",
    
    v604a %in% c("DIABETES", "DIABETIC", "SUGAR", "SUGAR BLOOD PRESSURE") ~ "3",
    
    v604a %in% c("BATH", "GHUNDAKO HADDI KHIYEKO.", "LUNGS KO BATHH VANNI", "LUPUS", "LUPUS") ~ "5",
    
    v604a %in% c("KIDNEY STONES KO UPRESAN GAREKO", "PATHTHARI", "PIABKO SAMASYAA", 
                 "PISAB BANDA HUNE GAREKO", "STONE", "STONE IN URINE PIPE", "PATTHARI") ~ "6",
    
    v604a %in% c("JNDISH", "HEPATITIS", "JAUNDICE", "LIVER KO SAMSYA") ~ "7",
    
    v604a %in% c("BONE MARROW TRANSPLANT", "BRAIN TUMOR", "CANCER", "TONGUE CANCER") ~ "8",
    
    v604a %in% c("MIRGI", "SEIZURE", "SIJAR") ~ "9",
    
    v604a %in% c("GANL TB", "TUBOC") ~ "10",
    
    v604a %in% c("THOYRED", "THYROID") ~ "12",
    
    v604a %in% c("ULCER", "AANDRA KO OPERATION GAREKO PIPE BAT STOOL GARNE GAREKO 2081_01-12 DEKHI", 
                 "ALSAR", "ANDRA MA GHAU", "APPENDIX", "BABASHIL", "GALLBLADDER STONES", 
                 "GATRIC", "PAYALS", "PET DUKHNA", "PILES", "PIT KO THAILIMA PATHALI") ~ "13",
    
    v604a %in% c("BACK PA", "THERAPY", "DHARD PET DUKHNA", "LEGAMENT KO SURGERY VKO", 
                 "SARIR MANOJ HATT JODA DUKHNA") ~ "15",
    
    v604a %in% c("NEURO DISEASE", "SNAYU", "परलासिस", "CEREBRAL PAIN", 
                 "MASTISK PAKSHYAGHAT(CP)", "MIGRAIN", "MIGRAINE", "MIGRANE", 
                 "NEURO", "PARALYSIS", "PARALYZED", "TAU KO DUKHNA SAMYASYA", 
                 "TAUKO DUKHNE PURANO ROG", "TAUKO KO", "TAUKO KO DUKAI", 
                 "LEFT HAND NACHALNEY") ~ "16",
    
    v604a %in% c("DEPRESSION", "ANJEITY", "ANXIETY", "HALLUCINATIONS") ~ "18",
    
    v604a %in% c("URIC ACID", "URIC ACID RA PROSTHETICS", "URIK ASID", "URIQE ACID", "URIC  ACID") ~ "20",
    
    v604a %in% c("POSTATE", "POSTED", "POSTERT", "PROSTATE", "PROSTED", "PROSTHETIC", 
                 "PROSTRATE", "PROTESTED KO SAMASYA", "URINE INFECTION") ~ "21",
    
    v604a %in% c("EAR PROBLEM", "ENT BIRAMI", "GHATI DUKHNAY", "PINASH") ~ "22",
    
    v604a %in% c("ACNE ISSUES", "ALLERGY", "CHHALA ROG DAJ", "DAJ", "SKIN ALLERGIES", 
                 "SKIN ALLERGY", "SKIN ELERGY", "SKIN PROBLEM", "SKIN ROG", "XALA SAMBANDHI") ~ "23",
    
    v604a %in% c("AAKHAKO SAMASYA", "EYE", "EYE INFECTION", "EYE ISSUES", "EYE PROBLEM", 
                 "EYE PROBLEMS", "JALABINDU", "JALBINDU", "JALBINDU VAYEKO", 
                 "JALBINDU VAYEKO REGULAR MEDICINE LAGAUNE PARXA") ~ "24",
    
    v604a %in% c("ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA MAA CHOT PAREKO", 
                 "DISLOCATED BACKBONE", "RIGHT HAND DISABLE DUE TO INJURY", "LEGAMENT KO  SURGERY VKO") ~ "26",
    
    v604a %in% c("LUNGS PROBLEM") ~ "27",
    
    v604a %in% c("BLOOD BAKLO VAYEKO", "BLOOD PATALO GARAUNAY", "NASA KO DABAI", 
                 "NASA SAMBANDHI", "OVER WEIGHT", "SICKLE CELL ANEMIA", 
                 "SICKLECELL ANIMIYA", "SPLEEN PROBLEM", "ANEMIA", "VARICOSE VEINS") ~ "28",
    
    v604a %in% c("AUTISTIC", "DISABLE", "PURNA APANGA") ~ "30",

    v604 %in% c("CHLORESTROL", "CHLOSTROAL", "CHOLESTEROL", "COLDSTORE", 
                "COLESTER", "COLSTRORE", "COLESTEROL") ~ "1",
    
    v604 %in% c("MUTUROG BLOOD PRESSURE", "PRESSURE LOW", "SUGAR BLOOD PRESSURE") ~ "2",
    
    v604 %in% c("DIABETES", "SUGAR") ~ "3",
    
    v604 %in% c("DAM", "DAM KO ROGI") ~ "4",
    
    v604 %in% c("KNEE PAIN", "LEG SWELLING") ~ "5",
    
    v604 %in% c("KIDNEY MA PATHALI", "KIDNEY STONES", "PATTHARI") ~ "6",
    
    v604 %in% c("ALCOLOHISM", "HE HAD TO BE HOSPITALIZED THIS YEAR DUE TO EXCESSIVE ALCOHOL CONSUMPTION") ~ "7",
    
    v604 %in% c("BREASTMA GATHO BHAKO", "TUMOR PETMA ( LIPOMA)") ~ "8",
    
    v604 %in% c("TB ROG") ~ "10",
    
    v604 %in% c("GASTRIC", "PILES", "ULCER", "ULCERS") ~ "13",
    
    v604 %in% c("BRAIN PROBLEM", "BRAIN PROBLEM - SCARS", "MIGRAINE", "MIGRAINE SAMBANDI", 
                "PARALICSES", "PARALYSIS", "PARTIAL PARALYSIS") ~ "16",
    
    v604 %in% c("ANZITY") ~ "18",
    
    v604 %in% c("URIC ACID") ~ "20",
    
    v604 %in% c("HYDROCELE", "PISAB KO SAMASYA PROSTATE", "PISAB ROKKINE SAMASYA", 
                "PISAB THAILI KO PROBLEM", "PISABKO KHARABI", "PROSTATE", 
                "PROSTATE PROBLEM", "PROSTED", "PROSTRATE", "URINE INFECTION") ~ "21",
    
    v604 %in% c("BODY ALLERGY", "CHALA SAMBANDHI", "KHUTTA MA DAG TAI CHILAUNA", 
                "SKIN ELERGY", "SKIN PROBLEM", "छालाको समस्या छाला रोग") ~ "23",
    
    v604 %in% c("AAKHA SAMBANDHI SAMASYA", "AAKHAKO - MOTIBIDNU SAMASYA", 
                "AKHA SAMBANDI", "MOTIBINDU", "RETINA PROBLEM JALBINDU") ~ "24",
    
    v604 %in% c("CHEST PROBLEM", "CHHATI SAMBANDHI SAMASYA", "PHOKSO KO PROBLEM", 
                "PLEURAL EFFUSION") ~ "27",
    
    v604 %in% c("POLYCYTHEMIA VERA", "RAGAT KO KAMI", "SICKLE CELL") ~ "28",
    
    v604 %in% c("AAPANGA", "DIFFERENTLY ABLE", "DISABILITY", "DOWN SYNDROME", 
                "INTELLECTUAL DISABILITY", "PURNA APANGA BHAYAKO KO") ~ "30",
    
    v604 %in% c("BUDO VAYARA KAMJORI VAYO KARAN") ~ "31",
    
    TRUE ~ as.character(v604)
  )
)

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
  ) %>%
  mutate(
    v610n = if_else(
      !is.na(v610n_1), 
      1, 
      v610n
    )
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
  v611m = ifelse(!is.na(v611m_1), 1, v611m_1)
  ) %>%
  select(-v611) %>%
  rename(
  v611a = v611_new,
  v611 = v611_1
  ) 
  

for (i in setdiff(1:ncol(section6b1), c(2, 7, 8, 14, 21, 22, 23, 38))) {
  section6b1[[i]] <- as.numeric(gsub("[^0-9]", "", section6b1[[i]]))
}

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

section6b1 <- merge(
  section6b1, 
  section1a[, c("uniq_id", "v104a")],
  by = "uniq_id"
)

section6b1 <- section6b1 %>%
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
      v606 > 2000 ~ 2082 - v606, 
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

section6b1 <- section6b1 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604),
    v606 = if_else(
      v606 > v104a , 1, v606
    )
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disease_id, -v104a)

section6b1 <- section6b1 %>%
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

section6b1 <- section6b1 %>%
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
      personid == 23262 & v604 == 2 ~ 21,
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
      TRUE ~ v604
    )
  ) %>%
  filter(
      personid != 15077
  )

section6b1 <- section6b1 %>%
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

section6b1 <- section6b1 %>%
  select(-`v603 == if_else(!is.na(v604), 1, 2)`, -uniq_id)

section6b1_added_rows <- read.xlsx("health section arrangement/input_outpatients.xlsx")

section6b1_added_rows <- section6b1_added_rows %>%
  select(-uniq_id, -disease_id) 

section6b1 <- rbind(
  section6b1, section6b1_added_rows
)

section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

#SECTION6B2

section6b2 <- section6b2 %>%
  mutate(
    v604a = trimws(v604a),
    v604 = case_when(
    
    v604a %in% c("MUTUROG", "COLESTEROL", "CHLOSTROAL", "CHOLEDTEROL", 
                 "CHORESTEROL", "COLDSTORE", "COLESTER", "COLESTROME", 
                 "COLSTORE", "COLSTRORE", "कोलेस्ट्रोल", "CHOLESTEROL") ~ "1",
    
    v604a %in% c("BP", "PRESSURE", "PRESSURE RA MANASIK ROG", 
                 "PRESSURE/SUGAR/THYROID", "LOW BLOOD PRESSURE", "96") ~ "2",
    
    v604 == "96" & v604a %in% c("") ~ "2",
    
    v604a %in% c("DIABETES", "DIABETIC", "SUGAR", "SUGAR BLOOD PRESSURE") ~ "3",
    
    v604a %in% c("BATH", "GHUNDAKO HADDI KHIYEKO.", "LUNGS KO BATHH VANNI", "LUPUS", "LUPUS") ~ "5",
    
    v604a %in% c("KIDNEY STONES KO UPRESAN GAREKO", "PATHTHARI", "PIABKO SAMASYAA", 
                 "PISAB BANDA HUNE GAREKO", "STONE", "STONE IN URINE PIPE", "PATTHARI") ~ "6",
    
    v604a %in% c("JNDISH", "HEPATITIS", "JAUNDICE", "LIVER KO SAMSYA") ~ "7",
    
    v604a %in% c("BONE MARROW TRANSPLANT", "BRAIN TUMOR", "CANCER", "TONGUE CANCER") ~ "8",
    
    v604a %in% c("MIRGI", "SEIZURE", "SIJAR") ~ "9",
    
    v604a %in% c("GANL TB", "TUBOC") ~ "10",
    
    v604a %in% c("THOYRED", "THYROID") ~ "12",
    
    v604a %in% c("ULCER", "AANDRA KO OPERATION GAREKO PIPE BAT STOOL GARNE GAREKO 2081_01-12 DEKHI", 
                 "ALSAR", "ANDRA MA GHAU", "APPENDIX", "BABASHIL", "GALLBLADDER STONES", 
                 "GATRIC", "PAYALS", "PET DUKHNA", "PILES", "PIT KO THAILIMA PATHALI") ~ "13",
    
    v604a %in% c("BACK PA", "THERAPY", "DHARD PET DUKHNA", "LEGAMENT KO SURGERY VKO", 
                 "SARIR MANOJ HATT JODA DUKHNA") ~ "15",
    
    v604a %in% c("NEURO DISEASE", "SNAYU", "परलासिस", "CEREBRAL PAIN", 
                 "MASTISK PAKSHYAGHAT(CP)", "MIGRAIN", "MIGRAINE", "MIGRANE", 
                 "NEURO", "PARALYSIS", "PARALYZED", "TAU KO DUKHNA SAMYASYA", 
                 "TAUKO DUKHNE PURANO ROG", "TAUKO KO", "TAUKO KO DUKAI", 
                 "LEFT HAND NACHALNEY") ~ "16",
    
    v604a %in% c("DEPRESSION", "ANJEITY", "ANXIETY", "HALLUCINATIONS") ~ "18",
    
    v604a %in% c("URIC ACID", "URIC ACID RA PROSTHETICS", "URIK ASID", "URIQE ACID", "URIC  ACID") ~ "20",
    
    v604a %in% c("POSTATE", "POSTED", "POSTERT", "PROSTATE", "PROSTED", "PROSTHETIC", 
                 "PROSTRATE", "PROTESTED KO SAMASYA", "URINE INFECTION") ~ "21",
    
    v604a %in% c("EAR PROBLEM", "ENT BIRAMI", "GHATI DUKHNAY", "PINASH") ~ "22",
    
    v604a %in% c("ACNE ISSUES", "ALLERGY", "CHHALA ROG DAJ", "DAJ", "SKIN ALLERGIES", 
                 "SKIN ALLERGY", "SKIN ELERGY", "SKIN PROBLEM", "SKIN ROG", "XALA SAMBANDHI") ~ "23",
    
    v604a %in% c("AAKHAKO SAMASYA", "EYE", "EYE INFECTION", "EYE ISSUES", "EYE PROBLEM", 
                 "EYE PROBLEMS", "JALABINDU", "JALBINDU", "JALBINDU VAYEKO", 
                 "JALBINDU VAYEKO REGULAR MEDICINE LAGAUNE PARXA") ~ "24",
    
    v604a %in% c("ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA MAA CHOT PAREKO", 
                 "DISLOCATED BACKBONE", "RIGHT HAND DISABLE DUE TO INJURY", "LEGAMENT KO  SURGERY VKO") ~ "26",
    
    v604a %in% c("LUNGS PROBLEM") ~ "27",
    
    v604a %in% c("BLOOD BAKLO VAYEKO", "BLOOD PATALO GARAUNAY", "NASA KO DABAI", 
                 "NASA SAMBANDHI", "OVER WEIGHT", "SICKLE CELL ANEMIA", 
                 "SICKLECELL ANIMIYA", "SPLEEN PROBLEM", "VARICOSE VEINS") ~ "28",
    
    v604a %in% c("AUTISTIC", "DISABLE", "PURNA APANGA") ~ "30",

    v604 %in% c("CHLORESTROL", "CHLOSTROAL", "CHOLESTEROL", "COLDSTORE", 
                "COLESTER", "COLSTRORE", "COLESTEROL") ~ "1",
    
    v604 %in% c("MUTUROG BLOOD PRESSURE", "PRESSURE LOW", "SUGAR BLOOD PRESSURE") ~ "2",
    
    v604 %in% c("DIABETES", "SUGAR") ~ "3",
    
    v604 %in% c("DAM", "DAM KO ROGI") ~ "4",
    
    v604 %in% c("KNEE PAIN", "LEG SWELLING") ~ "5",
    
    v604 %in% c("KIDNEY MA PATHALI", "KIDNEY STONES", "PATTHARI") ~ "6",
    
    v604 %in% c("ALCOLOHISM", "HE HAD TO BE HOSPITALIZED THIS YEAR DUE TO EXCESSIVE ALCOHOL CONSUMPTION") ~ "7",
    
    v604 %in% c("BREASTMA GATHO BHAKO", "TUMOR PETMA ( LIPOMA)") ~ "8",
    
    v604 %in% c("TB ROG") ~ "10",
    
    v604 %in% c("GASTRIC", "PILES", "ULCER", "ULCERS") ~ "13",
    
    v604 %in% c("BRAIN PROBLEM", "BRAIN PROBLEM - SCARS", "MIGRAINE", "MIGRAINE SAMBANDI", 
                "PARALICSES", "PARALYSIS", "PARTIAL PARALYSIS") ~ "16",
    
    v604 %in% c("ANZITY") ~ "18",
    
    v604 %in% c("URIC ACID") ~ "20",
    
    v604 %in% c("HYDROCELE", "PISAB KO SAMASYA PROSTATE", "PISAB ROKKINE SAMASYA", 
                "PISAB THAILI KO PROBLEM", "PISABKO KHARABI", "PROSTATE", 
                "PROSTATE PROBLEM", "PROSTED", "PROSTRATE", "URINE INFECTION") ~ "21",
    
    v604 %in% c("BODY ALLERGY", "CHALA SAMBANDHI", "KHUTTA MA DAG TAI CHILAUNA", 
                "SKIN ELERGY", "SKIN PROBLEM", "छालाको समस्या छाला रोग") ~ "23",
    
    v604 %in% c("AAKHA SAMBANDHI SAMASYA", "AAKHAKO - MOTIBIDNU SAMASYA", 
                "AKHA SAMBANDI", "MOTIBINDU", "RETINA PROBLEM JALBINDU") ~ "24",
    
    v604 %in% c("CHEST PROBLEM", "CHHATI SAMBANDHI SAMASYA", "PHOKSO KO PROBLEM", 
                "PLEURAL EFFUSION") ~ "27",
    
    v604 %in% c("POLYCYTHEMIA VERA", "RAGAT KO KAMI", "SICKLE CELL") ~ "28",
    
    v604 %in% c("AAPANGA", "DIFFERENTLY ABLE", "DISABILITY", "DOWN SYNDROME", 
                "INTELLECTUAL DISABILITY", "PURNA APANGA BHAYAKO KO") ~ "30",
    
    v604 %in% c("BUDO VAYARA KAMJORI VAYO KARAN") ~ "31",
    
    TRUE ~ as.character(v604)
  )
)

#SECTION6B3

section6b3 <- section6b3 %>%
  rename(
    v604 = v614
  ) %>%
  mutate(
    v604_num = as.numeric(str_extract(v604, "\\d+")),

    v604_txt = str_trim(
      str_remove_all(v604, "\\d+|,")
    ),

    v604  = v604_num,
    v604a = if_else(v604_txt != "", v604_txt, NA_character_)
  ) %>%
  select(-v604_num, -v604_txt, -v613, -v613b) 

section6b3 <- section6b3 %>%
  mutate(
    v604a = trimws(v604a),
    
    v604 = case_when(
      is.na(v604a) & v604 == 96 ~ 2,
      v604a %in% c("MUTUROG", "COLESTEROL", "CHLOSTROAL", "CHOLEDTEROL", 
                   "CHORESTEROL", "COLDSTORE", "COLESTER", "COLESTROME", 
                   "COLSTORE", "COLSTRORE", "कोलेस्ट्रोल", "CHOLESTEROL", 
                   "CHOLOSTREL", "CHLOSTROL", "CHOLESTEROLPROSTATE", "COLSTROL", 
                   "COLDSTORAL", "COL", "COL STORE", "CHOLESTEROL PROSTATE", 
                   "CHLORESTROL", "CHOLESTROL", "WEIGHT LOSS MEDICATION") ~ 1,
      
      v604a %in% c("BP", "PRESSURE", "PRESSURE RA MANASIK ROG", "BLOOD PRESSURE$ MOTUROG",
                   "PRESSURE/SUGAR/THYROID", "LOW BLOOD PRESSURE", "96", 
                   "MUTUROG BLOOD PRESSURE", "PRESSURE LOW", "BP LOW") ~ 2,
      
      (v604 == 96 | v604 == "96") & (v604a == "" | is.na(v604a)) ~ 2,
      
      v604a %in% c("DIABETES", "DIABETIC", "SUGAR", "SUGAR BLOOD PRESSURE") ~ 3,
      
      v604a %in% c("DAM", "DAM KO ROGI") ~ 4,
      
      v604a %in% c("BATH", "GHUNDAKO HADDI KHIYEKO.", "LUNGS KO BATHH VANNI", 
                   "LUPUS", "KNEE PAIN", "LEG SWELLING", "GHUNDAKO HADDI KHIYEKO") ~ 5,
      
      v604a %in% c("KIDNEY STONES KO UPRESAN GAREKO", "PATHTHARI", "PIABKO SAMASYAA", 
                   "PISAB BANDA HUNE GAREKO", "STONE", "STONE IN URINE PIPE", "PATHARI",
                   "PATTHARI", "PACHTHARI", "KIDNEY MA PATHALI", "KIDNEY STONES", "PISAB BANDA HUNE",
                   "PISABKO SAMASYA", "PISAB ROKKINE SAMASYAA", "KIDNEY STONES CAUSE OPESAN") ~ 6,
      
      v604a %in% c("JNDISH", "HEPATITIS", "JAUNDICE", "LIVER KO SAMSYA", "EXCESSIVE ALCOHOL CONSUMPTION",
                   "ALCOLOHISM", "HE HAD TO BE HOSPITALIZED THIS YEAR DUE TO EXCESSIVE ALCOHOL CONSUMPTION",
                   "LIBAR KO SAMSYA") ~ 7,
      
      v604a %in% c("BONE MARROW TRANSPLANT", "BRAIN TUMOR", "CANCER", "BREAST MA GATHO AAKO",
                   "TONGUE CANCER", "BREASTMA GATHO BHAKO", "TUMOR PETMA ( LIPOMA)",
                   "PET MA TUMOR ( LIPOMA)") ~ 8,
      
      v604a %in% c("MIRGI", "SEIZURE", "SIJAR") ~ 9,
      
      v604a %in% c("GANL TB", "TUBOC", "TB ROG") ~ 10,
      
      v604a %in% c("THOYRED", "THYROID") ~ 12,
      
      v604a %in% c("ULCER", "ULCERS", "ALSAR", "ANDRA MA GHAU", "APPENDIX", "DIGESTIVE",
                   "BABASHIL", "GALLBLADDER STONES", "GATRIC", "GASTRIC", "ANDRA MA SAMASYA",
                   "PAYALS", "PET DUKHNA", "PILES", "PIT KO THAILIMA PATHALI", 
                   "AANDRA KO OPERATION GAREKO PIPE BAT STOOL GARNE GAREKO 2081_01-12 DEKHI",
                   "PILES KO LAI SHE SOMETIMES USES OINTMENT BUT MOSTLY TAKES AYURVEDIC MEDICINE") ~ 13,
      
      v604a %in% c("BACK PA", "THERAPY", "DHARD PET DUKHNA", 
                   "SARIR MANOJ HATT JODA DUKHNA") ~ 15,
      
      v604a %in% c("NEURO DISEASE", "SNAYU", "परलासिस", "CEREBRAL PAIN", 
                   "MASTISK PAKSHYAGHAT(CP)", "MIGRAIN", "MIGRAINE", "MIGRANE", 
                   "NEURO", "PARALYSIS", "PARALYZED", "PARALICSES", 
                   "TAU KO DUKHNA SAMYASYA", "TAUKO DUKHNE PURANO ROG", 
                   "TAUKO KO", "TAUKO KO DUKAI", "LEFT HAND NACHALNEY", 
                   "BRAIN PROBLEM", "BRAIN PROBLEM - SCARS", "MIGRAINE SAMBANDI", 
                   "PARTIAL PARALYSIS", "TAU KO SAMYASYA", "PARALYCIS",
                   "PARTIAL PARALYSIS LEFT HAND NACHALNEY", "CP") ~ 16,
      
      v604a %in% c("DEPRESSION", "ANJEITY", "ANXIETY", "HALLUCINATIONS", "ANZITY") ~ 18,
      
      v604a %in% c("URIC ACID", "URIC ACID RA PROSTHETICS", "URIK ASID", 
                   "URIQE ACID", "URIC  ACID", "URIK ACID") ~ 20,
      
      v604a %in% c("POSTATE", "POSTED", "POSTERT", "PROSTATE", "PROSTED", 
                   "PROSTHETIC", "PROSTRATE", "PROTESTED KO SAMASYA", "URINE INFECTION",
                   "PROTEST", "POSTERD", "HYDROCELE", "PISAB KO SAMASYA PROSTATE", 
                   "PISAB ROKKINE SAMASYA", "PISAB THAILI KO PROBLEM", "PISABKO KHARABI",
                   "PROSTATE PROBLEM", "PISAB THAILI") ~ 21,
      
      v604a %in% c("EAR PROBLEM", "ENT BIRAMI", "GHATI DUKHNAY", "PINASH", 
                   "NOSE KO ALLERGY VAYEKO.", "RUGHA NOSE KO ALLERGY", "GHATIKO SAMASYA") ~ 22,
      
      v604a %in% c("ACNE ISSUES", "ALLERGY", "CHHALA ROG DAJ", "DAJ", 
                   "SKIN ALLERGIES", "SKIN ALLERGY", "SKIN ELERGY", "SKIN PROBLEM", 
                   "SKIN ROG", "XALA SAMBANDHI", "CHHALAKO ROG", "CHALA ROG", 
                   "SKIN", "SKIN CONDITION", "छालाको समस्या छाला रोग", "ALARJI",
                   "CHALA SAMBANDHI", "KHUTTA MA DAG TAI CHILAUNA", "BODY ALLERGY",
                   "CHHALA SAMBANDHI", "ACNE PRONE SKIN") ~ 23,
      
      v604a %in% c("AAKHAKO SAMASYA", "EYE", "EYE INFECTION", "EYE ISSUES", "EYE MOTIBINDU", 
                   "EYE PROBLEM", "EYE PROBLEMS", "JALABINDU", "JALBINDU", 
                   "JALBINDU VAYEKO", "JALBINDU VAYEKO REGULAR MEDICINE LAGAUNE PARXA",
                   "AAKHA SAMBANDHI SAMASYA", "AAKHAKO - MOTIBIDNU SAMASYA", 
                   "AKHA SAMBANDI", "MOTIBINDU", "RETINA PROBLEM JALBINDU",
                   "AAKHAKO SAMASYA - MOTIBIDNU", "EYE PROBLEM/INFECTION", "AAKHA KO SAMASYA") ~ 24,
      
      v604a %in% c("ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA MAA CHOT PAREKO", 
                   "DISLOCATED BACKBONE", "RIGHT HAND DISABLE DUE TO INJURY", 
                   "LEGAMENT KO  SURGERY VKO", "LEGAMENT KO SURGERY VKO", 
                   "LEGAMENT KO PROBLEM", "ACCIDENT", "BACKBONE DISLOCATED", 
                   "ADMITTED WITH BROKEN LEG.SHE WAS TAKEN TO TULSIPUR INDIA FOR TREATMENTWHICH COSTS APPROX.NPR..", 
                   "HAND INJURY", "LIGAMENT KO SMSYA", "ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA NACHALNE") ~ 26,
      
      v604a %in% c("LUNGS PROBLEM", "PHOKSO KO PROBLEM", "CHEST PROBLEM", 
                   "CHHATI SAMBANDHI SAMASYA", "PLEURAL EFFUSION", "PHOKSO") ~ 27,
      
      v604a %in% c("BLOOD BAKLO VAYEKO", "BLOOD PATALO GARAUNAY", "NASA KO DABAI", 
                   "NASA SAMBANDHI", "OVER WEIGHT", "SICKLE CELL ANEMIA", 
                   "SICKLECELL ANIMIYA", "SPLEEN PROBLEM", "VARICOSE VEINS", 
                   "RAGAT KO KAMI", "POLYCYTHEMIA VERA", "SICKLE CELL", "NASA",
                   "NASA KO", "BLOOD BAKLOVAYEKO") ~ 28,
      
      v604a %in% c("SCRUBE TIFUS") ~ 29,
      
      v604a %in% c("AUTISTIC", "DISABLE", "PURNA APANGA", "DIFFERENTLY ABLE", 
                   "AAPANGA", "DOWN SYNDROME", "INTELLECTUAL DISABILITY", 
                   "PURNA APANGA BHAYAKO KO", "COMPLETE DISABILITY", "JANMA JAT APANGA") ~ 30,
      
      v604a %in% c("BUDO VAYARA KAMJORI VAYO KARAN", "SARIRA KAMJORI") ~ 31,

      TRUE ~ as.numeric(v604)
    ) 
  ) %>%
  filter(personid != 60249)

for (i in setdiff(1:ncol(section6b3), c(2, 7, 8, 29))) {
  section6b3[[i]] <- as.numeric(gsub("[^0-9]", "", section6b3[[i]]))
}

section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b3 <- section6b3 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup()

section6b3 <- section6b3 %>%
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
      TRUE ~ v604
    ),
    v101 = case_when(
      personid == 12487 ~ 2,
      TRUE ~ v101
    ),
    personid = case_when(
      uniq_id == c("2207-8-1") ~ 12488,
      TRUE ~ personid
    )
  )

section6b3 <- section6b3 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b3 <- section6b3 %>%
  filter(
    !disease_id %in% c(
      "1101-20-2-NA", "1105-15-6-13", "1111-13-1-2",
      "1208-18-NA-16", "1208-18-NA-7", "2207-8-3-12",
      "1418-3-1-3", "2109-3-2-15", "2207-8-5-12", 
      "2209-10-1-2", "3101-6-5-NA", "3103-10-1-1", 
      "3103-14-2-1", "3103-14-2-3", "5321-2-1-15",
      "5321-2-2-1"
    ),
    personid != 15077
  )


section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

missing_outpatients <- anti_join(
  section6b3, 
  section6b1, 
  by = "disease_id"
)

rm(missing_outpatients)

#SECTION6B4

section6b4 <- section6b4 %>%
  rename(
    v604a = v603
  ) %>%
  mutate(
    v604_num = suppressWarnings(as.numeric(str_extract(v604, "\\d+"))),

    v604_txt = str_trim(
      str_remove_all(v604, "\\d+|,")
    ),

    v604  = v604_num,
    v604a = if_else(v604_txt != "", v604_txt, NA_character_)
  ) %>%
  select(-v604_num, -v604_txt)   

section6b4 <- section6b4 %>%
  mutate(
    v604a = trimws(v604a),
    
    v604 = case_when(
      is.na(v604a) & v604 == 96 ~ 2,
      v604a %in% c("MUTUROG", "COLESTEROL", "CHLOSTROAL", "CHOLEDTEROL", 
                   "CHORESTEROL", "COLDSTORE", "COLESTER", "COLESTROME", 
                   "COLSTORE", "COLSTRORE", "कोलेस्ट्रोल", "CHOLESTEROL", 
                   "CHOLOSTREL", "CHLOSTROL", "CHOLESTEROLPROSTATE", "COLSTROL", 
                   "COLDSTORAL", "COL", "COL STORE", "CHOLESTEROL PROSTATE", 
                   "CHLORESTROL", "CHOLESTROL", "WEIGHT LOSS MEDICATION") ~ 1,
      
      v604a %in% c("BP", "PRESSURE", "PRESSURE RA MANASIK ROG", "BLOOD PRESSURE$ MOTUROG",
                   "PRESSURE/SUGAR/THYROID", "LOW BLOOD PRESSURE", "96", 
                   "MUTUROG BLOOD PRESSURE", "PRESSURE LOW", "BP LOW") ~ 2,
      
      (v604 == 96 | v604 == "96") & (v604a == "" | is.na(v604a)) ~ 2,
      
      v604a %in% c("DIABETES", "DIABETIC", "SUGAR", "SUGAR BLOOD PRESSURE") ~ 3,
      
      v604a %in% c("DAM", "DAM KO ROGI") ~ 4,
      
      v604a %in% c("BATH", "GHUNDAKO HADDI KHIYEKO.", "LUNGS KO BATHH VANNI", 
                   "LUPUS", "KNEE PAIN", "LEG SWELLING", "GHUNDAKO HADDI KHIYEKO") ~ 5,
      
      v604a %in% c("KIDNEY STONES KO UPRESAN GAREKO", "PATHTHARI", "PIABKO SAMASYAA", 
                   "PISAB BANDA HUNE GAREKO", "STONE", "STONE IN URINE PIPE", "PATHARI",
                   "PATTHARI", "PACHTHARI", "KIDNEY MA PATHALI", "KIDNEY STONES", "PISAB BANDA HUNE",
                   "PISABKO SAMASYA", "PISAB ROKKINE SAMASYAA", "KIDNEY STONES CAUSE OPESAN", "KIDNEY") ~ 6,
      
      v604a %in% c("JNDISH", "HEPATITIS", "JAUNDICE", "LIVER KO SAMSYA", "EXCESSIVE ALCOHOL CONSUMPTION",
                   "ALCOLOHISM", "HE HAD TO BE HOSPITALIZED THIS YEAR DUE TO EXCESSIVE ALCOHOL CONSUMPTION",
                   "LIBAR KO SAMSYA", "LIVER PROBLEM") ~ 7,
      
      v604a %in% c("BONE MARROW TRANSPLANT", "BRAIN TUMOR", "CANCER", "BREAST MA GATHO AAKO",
                   "TONGUE CANCER", "BREASTMA GATHO BHAKO", "TUMOR PETMA ( LIPOMA)",
                   "PET MA TUMOR ( LIPOMA)",
                  "BREAST TUMER.DURING THE TREATMENT OF BREAST TUMER APPROXIMATELY  LAKH RUPEES WERE SPENT ON GOING TO INDIA FOR TREATMENT.") ~ 8,
      
      v604a %in% c("MIRGI", "SEIZURE", "SIJAR") ~ 9,
      
      v604a %in% c("GANL TB", "TUBOC", "TB ROG") ~ 10,
      
      v604a %in% c("THOYRED", "THYROID") ~ 12,
      
      v604a %in% c("ULCER", "ULCERS", "ALSAR", "ANDRA MA GHAU", "APPENDIX", "DIGESTIVE",
                   "BABASHIL", "GALLBLADDER STONES", "GATRIC", "GASTRIC", "ANDRA MA SAMASYA",
                   "PAYALS", "PET DUKHNA", "PILES", "PIT KO THAILIMA PATHALI", 
                   "AANDRA KO OPERATION GAREKO PIPE BAT STOOL GARNE GAREKO 2081_01-12 DEKHI",
                   "PILES KO LAI SHE SOMETIMES USES OINTMENT BUT MOSTLY TAKES AYURVEDIC MEDICINE") ~ 13,
      
      v604a %in% c("BACK PA", "THERAPY", "DHARD PET DUKHNA", 
                   "SARIR MANOJ HATT JODA DUKHNA") ~ 15,
      
      v604a %in% c("NEURO DISEASE", "SNAYU", "परलासिस", "CEREBRAL PAIN", 
                   "MASTISK PAKSHYAGHAT(CP)", "MIGRAIN", "MIGRAINE", "MIGRANE", 
                   "NEURO", "PARALYSIS", "PARALYZED", "PARALICSES", 
                   "TAU KO DUKHNA SAMYASYA", "TAUKO DUKHNE PURANO ROG", 
                   "TAUKO KO", "TAUKO KO DUKAI", "LEFT HAND NACHALNEY", 
                   "BRAIN PROBLEM", "BRAIN PROBLEM - SCARS", "MIGRAINE SAMBANDI", 
                   "PARTIAL PARALYSIS", "TAU KO SAMYASYA", "PARALYCIS",
                   "PARTIAL PARALYSIS LEFT HAND NACHALNEY", "CP") ~ 16,
      
      v604a %in% c("DEPRESSION", "ANJEITY", "ANXIETY", "HALLUCINATIONS", "ANZITY") ~ 18,
      
      v604a %in% c("URIC ACID", "URIC ACID RA PROSTHETICS", "URIK ASID", 
                   "URIQE ACID", "URIC  ACID", "URIK ACID") ~ 20,
      
      v604a %in% c("POSTATE", "POSTED", "POSTERT", "PROSTATE", "PROSTED", 
                   "PROSTHETIC", "PROSTRATE", "PROTESTED KO SAMASYA", "URINE INFECTION",
                   "PROTEST", "POSTERD", "HYDROCELE", "PISAB KO SAMASYA PROSTATE", 
                   "PISAB ROKKINE SAMASYA", "PISAB THAILI KO PROBLEM", "PISABKO KHARABI",
                   "PROSTATE PROBLEM", "PISAB THAILI") ~ 21,
      
      v604a %in% c("EAR PROBLEM", "ENT BIRAMI", "GHATI DUKHNAY", "PINASH", 
                   "NOSE KO ALLERGY VAYEKO.", "RUGHA NOSE KO ALLERGY", "GHATIKO SAMASYA") ~ 22,
      
      v604a %in% c("ACNE ISSUES", "ALLERGY", "CHHALA ROG DAJ", "DAJ", 
                   "SKIN ALLERGIES", "SKIN ALLERGY", "SKIN ELERGY", "SKIN PROBLEM", 
                   "SKIN ROG", "XALA SAMBANDHI", "CHHALAKO ROG", "CHALA ROG", 
                   "SKIN", "SKIN CONDITION", "छालाको समस्या छाला रोग", "ALARJI",
                   "CHALA SAMBANDHI", "KHUTTA MA DAG TAI CHILAUNA", "BODY ALLERGY",
                   "CHHALA SAMBANDHI", "ACNE PRONE SKIN") ~ 23,
      
      v604a %in% c("AAKHAKO SAMASYA", "EYE", "EYE INFECTION", "EYE ISSUES", "EYE MOTIBINDU", 
                   "EYE PROBLEM", "EYE PROBLEMS", "JALABINDU", "JALBINDU", 
                   "JALBINDU VAYEKO", "JALBINDU VAYEKO REGULAR MEDICINE LAGAUNE PARXA",
                   "AAKHA SAMBANDHI SAMASYA", "AAKHAKO - MOTIBIDNU SAMASYA", 
                   "AKHA SAMBANDI", "MOTIBINDU", "RETINA PROBLEM JALBINDU", "EYE DISEASE.",
                   "AAKHAKO SAMASYA - MOTIBIDNU", "EYE PROBLEM/INFECTION", "AAKHA KO SAMASYA") ~ 24,
      
      v604a %in% c("ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA MAA CHOT PAREKO", 
                   "DISLOCATED BACKBONE", "RIGHT HAND DISABLE DUE TO INJURY", 
                   "LEGAMENT KO  SURGERY VKO", "LEGAMENT KO SURGERY VKO", 
                   "LEGAMENT KO PROBLEM", "ACCIDENT", "BACKBONE DISLOCATED", 
                   "ADMITTED WITH A BROKEN LEG.SHE WAS TAKEN TO TULSIPUR INDIA FOR TREATMENTWHICH COSTS APPROX.NPR..", 
                   "HAND INJURY", "LIGAMENT KO SMSYA", "ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA NACHALNE",
                   "LADERA EMERGENCY MA HELICOPTER MA KATHMANDU LAGEKO") ~ 26,
      
      v604a %in% c("LUNGS PROBLEM", "PHOKSO KO PROBLEM", "CHEST PROBLEM", 
                   "CHHATI SAMBANDHI SAMASYA", "PLEURAL EFFUSION", "PHOKSO") ~ 27,
      
      v604a %in% c("BLOOD BAKLO VAYEKO", "BLOOD PATALO GARAUNAY", "NASA KO DABAI", 
                   "NASA SAMBANDHI", "OVER WEIGHT", "SICKLE CELL ANEMIA", 
                   "SICKLECELL ANIMIYA", "SPLEEN PROBLEM", "VARICOSE VEINS", 
                   "RAGAT KO KAMI", "POLYCYTHEMIA VERA", "SICKLE CELL", "NASA",
                   "NASA KO", "BLOOD BAKLOVAYEKO") ~ 28,
      
      v604a %in% c("SCRUBE TIFUS") ~ 29,
      
      v604a %in% c("AUTISTIC", "DISABLE", "PURNA APANGA", "DIFFERENTLY ABLE", 
                   "AAPANGA", "DOWN SYNDROME", "INTELLECTUAL DISABILITY", 
                   "PURNA APANGA BHAYAKO KO", "COMPLETE DISABILITY", "JANMA JAT APANGA") ~ 30,
      
      v604a %in% c("BUDO VAYARA KAMJORI VAYO KARAN", "SARIRA KAMJORI") ~ 31,

      personid == 20655 & is.na(v604) ~ 16,
      personid == 35802 & is.na(v604) ~ 13, 
      personid == 54033 & is.na(v604) ~ 3,
      personid == 54116 & is.na(v604) ~ 6,

      TRUE ~ as.numeric(v604)
    ) 
  ) 

section6b4 <- section6b4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(v101))

section6b4 <- section6b4 %>%
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
      TRUE ~ v604
    )
  ) %>%
  filter(
    !disease_id %in% c(
      "3207-4-2-1", "3441-1-2-16", "1109-20-2-2", "4209-1-1-13"
    )
  )

section6b4 <- section6b4 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

missing_inpatients <- anti_join(
  section6b4, 
  section6b1, 
  by = "disease_id"
)

for (i in setdiff(1:ncol(section6b4), c(2, 7, 8, 11))) {
  section6b4[[i]] <- as.numeric(gsub("[^0-9]", "", section6b4[[i]]))
}

rm(missing_inpatients, section6b1_added_rows)

#SECTION6C1

section6c1 <- section6c1 %>%
  mutate(
    v630a = trimws(v630a),
    v630 = case_when(
    v630 %in% c("CHISO COLD", "CHISO RUGHA", "COLD ALLERGY", "KHOKI", "KHOKI LAGEKO", 
                "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO", "खोकी", "रुघाखोकी") ~ "6",

    v630 == "JONDISH LIVAR PHET MA PANI VKO" ~ "9",

    v630 %in% c("URINE INFECTION", "URINE PROBLEM") ~ "10",

    v630 == "DATA KO SAMASYA" ~ "11",

    v630 %in% c("ALLERGIES", "ALLERGY", "CHALA SAMBANDHI ROGH", "GHAU KHATIRA", 
                "JUI KHATIRA", "KHUTTA MA ALLERGY AAKO", "PANI FOKA BATA PANI NILAKELL") ~ "14",

    v630 %in% c("BANCHARO BATA KHUTA KATIYO", "HAAT VACHIYEKO UPACHAR", 
                "HAND FRACTURE", "LIGAMENT TEARING") ~ "15",

    v630 %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630 %in% c("BACK PAIN", "BACKBONE MA PROBLEM VAYERA", "BONE PAIN", "DHAD DUKHEKO", 
                "DHAD DUKHNE", "DHAD DUKHNE SAMASYA", "DHAD KO", "DHADKO DUKHAI", 
                "DHARD DUKHNA SAMYASYA", "GODA DUKHEKO", "GUDHA DUKHEKO", 
                "HAAD JORNI KO SAMASYA", "HAAT KHUTTA KO JORNI DUKHEKO", 
                "HAAT KO HADDI KO PROBLEM", "HADIKHIYER", "HADIKO SAMASA", 
                "HADJORNI", "HADJORNI DUKHNE HATKO", "HARDJORNI DUKHNE", "HAT DUKHERA", 
                "HAT KHUTTA DUKHEKO", "HAT KHUTTA DUKHNA", "HAT KHUTTA DUKHNE", 
                "HATH KO HADI DUKHEKO", "HATT KHUTTA DUKHNA", "JOINT PAIN", "JYU DHUKHEKO", 
                "JYU DUKHNE", "KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                "MULTIPLE JOINT PAIN", "PAIN IN LEG", "खुट्टा को कुर्कुच्चा दुख्ने", 
                "BACKPAINEYE DRYNESS") ~ "19",

    v630 %in% c("ANAEMIA", "INCREASE IN CHOLESTEROL LEVEL", "RAGATMA KHARABI") ~ "20",

    v630 %in% c("BONE MARROW TRANSPLANT", "CANCER KO LAXAN", "LUMP IN BREAST") ~ "21",

    v630 %in% c("ANDRA MA GAU", "APPENDICITIS OPERATION", "APPENDIX", "BHOMIT BHAYEKO", 
                "CONSTIPATION", "GASTIK", "GASTRIC", "GASTRIC KO PROBLEM", "GASTRITIS", 
                "GYASTRIK", "HAAT DUKHNE PAYALS", "LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", 
                "PET DUKHER", "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", "PET KO CHECK", 
                "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", "PETA DUKHEKO", 
                "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE", "ULCER", 
                "ULCER KO OPERATION", "VOMITING", "YAPENDIKS KO OPTION") ~ "22",

    v630 == "OTH TALU FATEKO" ~ "23",

    v630 == "DAMM" ~ "24",

    v630 %in% c("DELIVERY", "DELIVERY CHECKUP", "PERGINENC", "PREGENCY", 
                "PREGNANCY CHECK UP", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", 
                "PREGNANT", "SUTKERI", "SUTKERI BHAYEKO") ~ "25",

    v630 == "JANMA JATA APANGA" ~ "26",

    v630 %in% c("EAR PROBLEM", "ENT (DAT (TEETH)KO CHECK GARAUNA GAYEKO TARA SSF MA DA NAPARNE VAYERA NAK", 
                "GHATI KO SAMASYA", "GHATI MA GIRKHA", "GHATIMA SAMASYA", "JIBRO KO SAMASYA", 
                "MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", "NAK RA MUKHA BATA BLOOD AKO", 
                "NAKKO SAMSHYA", "TANSIL", "TONSIL", "TONSILLITIS", "TONSILS", "ट्वान्सिल") ~ "27",

    v630 %in% c("AKHAMA CHO", "EYE PROBLEM", "EYE CHECK GARNA GAYEKO") ~ "28",

    v630 == "DAAD" ~ "29",

    v630 %in% c("BODYSCHE", "JIU DUKHEKO", "KAMJORI", "KAMJORI BHAYEKO", "KAMJORI VAYEKO") ~ "30",

    v630 %in% c("CHECK UP PREGNANT NA BHAYERA", "GAINO PATHEGHAR SAMASYA", "GYAENO PROBLEM", 
                "GYANO KO PROBLEM", "IRREGULAR MENSURATION", "MINS VAYAKO BELA PET DUKHEKO", 
                "PATHAGHAR SAMANDI SAMASYA IS", "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", 
                "PERIOD ANIYAMIT HUNE", "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", 
                "SHIST SURGERY", "UTERUS INFECTION", "UTERUS PROBLAM") ~ "31",

    v630 %in% c("KAMJORI BP LOW", "MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630 %in% c("FOLLOWUP OF HERNIA OPERATIO", "HARNIYA", "HARNIYA KO OPERATION", "HERNIA") ~ "33",

    v630 == "SCROP TRIFECTA" ~ "35",

    v630 %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI", "KIDNEY INFECTION SYMPTOM OF CANCER") ~ "36",

    v630 %in% c("GAL BLADORS ROBLAM", "LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                "PATTHARIKO AUSADHI", "STONE", 
                "UHA KO URIC ACID ATHAWA LIVER KO SAMASYA LEY HAST DUKHEKO VANERA DOCTOR LEY VANNU BHAYO") ~ "37",

    v630 %in% c("CHATI DUKHEKO", "CHATTI DUKHA SAMASYA", "CHEST INFECTION", 
                "FOKSO MA PANI DEKHIYEKO", "TUBERCULOSIS") ~ "38",

    v630 %in% c("BHULNE SAMASYA", "HEAD ISSUES", "MANASIK SAMASYA") ~ "40",

    v630 %in% c("DIZZINESS", "HEADACE", "HEADACHE", "JHUTTA JHAMJHAMAUNE", "MIGRAINE", 
                "MIGREN HEADACHE", "NASA", "NASA DABE KO", "NASA SAMBANDHI", 
                "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", "TAUKO DUKHANE", 
                "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", 
                "THAUKO DUKHANE", "TUKO DUKHAYA") ~ "41",

    v630 %in% c("BATHA ROGA", "URIC ACID", "URIK ASID") ~ "42",

    v630 == "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO" ~ "43",

    v630a == "RAGATMA KHARABI" ~ "20",

    v630a %in% c("KHOKI", "KHOKI LAGEKO", "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO") ~ "6",

    v630a %in% c("KHUTTA MA ALLERGY AAKO", "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO", 
                 "PANI FOKA BATA PANI NILAKELL") ~ "14",

    v630a == "LIGAMENT TEARING" ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630a %in% c("KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                 "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                 "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                 "MULTIPLE JOINT PAIN", "PAIN IN LEG") ~ "19",

    v630a == "LUMP IN BREAST" ~ "21",

    v630a %in% c("LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", "PET DUKHER", 
                 "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                 "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", 
                 "PET KO CHECK", "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", 
                 "PETA DUKHEKO", "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a %in% c("PERGINENC", "PREGENCY", "PREGNANCY CHECK UP", 
                 "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", "PREGNANT", 
                 "SUTKERI", "SUTKERI BHAYEKO") ~ "25",

    v630a %in% c("MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", 
                 "NAK RA MUKHA BATA BLOOD AKO", "NAKKO SAMSHYA", "TANSIL", 
                 "TONSIL", "TONSILLITIS", "TONSILS") ~ "27",

    v630a %in% c("KAMJORI", "KAMJORI BHAYEKO", "KAMJORI BP LOW", "KAMJORI VAYEKO") ~ "30",

    v630a %in% c("MINS VAYAKO BELA PET DUKHEKO", "PATHAGHAR SAMANDI SAMASYA IS", "SHIST KO SURGERY",
                 "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", "PERIOD ANIYAMIT HUNE", 
                 "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", "SHIST SURGERY") ~ "31",

    v630a %in% c("MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI") ~ "36",

    v630a %in% c("LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                 "PATTHARIKO AUSADHI", "STONE") ~ "37",

    v630a == "TUBERCULOSIS" ~ "38",

    v630a == "MANASIK SAMASYA" ~ "40",

    v630a %in% c("MIGRAINE", "MIGREN HEADACHE", "NASA", "NASA DABE KO", 
                 "NASA SAMBANDHI", "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", 
                 "TAUKO DUKHANE", "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", 
                 "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", "THAUKO DUKHANE", "TUKO DUKHAYA") ~ "41",

    v630a %in% c("CHISO COLD", "CHISO RUGHA", "COLD ALLERGY", "KHOKI", "KHOKI LAGEKO", 
                "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO", "खोकी", "रुघाखोकी") ~ "6",

    v630a == "JONDISH LIVAR PHET MA PANI VKO" ~ "9",

    v630a %in% c("URINE INFECTION", "URINE PROBLEM") ~ "10",

    v630a == "DATA KO SAMASYA" ~ "11",

    v630a %in% c("ALLERGIES", "ALLERGY", "CHALA SAMBANDHI ROGH", "GHAU KHATIRA", 
                "JUI KHATIRA", "KHUTTA MA ALLERGY AAKO", "PANI FOKA BATA PANI NILAKELL") ~ "14",

    v630a %in% c("BANCHARO BATA KHUTA KATIYO", "HAAT VACHIYEKO UPACHAR", 
                "HAND FRACTURE", "LIGAMENT TEARING") ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630a %in% c("BACK PAIN", "BACKBONE MA PROBLEM VAYERA", "BONE PAIN", "DHAD DUKHEKO", 
                "DHAD DUKHNE", "DHAD DUKHNE SAMASYA", "DHAD KO", "DHADKO DUKHAI", 
                "DHARD DUKHNA SAMYASYA", "GODA DUKHEKO", "GUDHA DUKHEKO", 
                "HAAD JORNI KO SAMASYA", "HAAT KHUTTA KO JORNI DUKHEKO", 
                "HAAT KO HADDI KO PROBLEM", "HADIKHIYER", "HADIKO SAMASA", 
                "HADJORNI", "HADJORNI DUKHNE HATKO", "HARDJORNI DUKHNE", "HAT DUKHERA", 
                "HAT KHUTTA DUKHEKO", "HAT KHUTTA DUKHNA", "HAT KHUTTA DUKHNE", 
                "HATH KO HADI DUKHEKO", "HATT KHUTTA DUKHNA", "JOINT PAIN", "JYU DHUKHEKO", 
                "JYU DUKHNE", "KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                "MULTIPLE JOINT PAIN", "PAIN IN LEG", "खुट्टा को कुर्कुच्चा दुख्ने") ~ "19",

    v630a %in% c("ANAEMIA", "INCREASE IN CHOLESTEROL LEVEL", "RAGATMA KHARABI") ~ "20",

    v630a %in% c("BONE MARROW TRANSPLANT", "CANCER KO LAXAN", "LUMP IN BREAST") ~ "21",

    v630a %in% c("ANDRA MA GAU", "APPENDICITIS OPERATION", "APPENDIX", "BHOMIT BHAYEKO", 
                "CONSTIPATION", "GASTIK", "GASTRIC", "GASTRIC KO PROBLEM", "GASTRITIS", 
                "GYASTRIK", "HAAT DUKHNE PAYALS", "LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", 
                "PET DUKHER", "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", "PET KO CHECK", 
                "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", "PETA DUKHEKO", 
                "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE", "ULCER", 
                "ULCER KO OPERATION", "VOMITING", "YAPENDIKS KO OPTION") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a == "DAMM" ~ "24",

    v630a %in% c("DELIVERY", "DELIVERY CHECKUP", "PERGINENC", "PREGENCY", 
                "PREGNANCY CHECK UP", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", 
                "PREGNANT", "SUTKERI", "SUTKERI BHAYEKO", "ANC CHECKUP IN PRIVATE HOSPITAL") ~ "25",

    v630a == "JANMA JATA APANGA" ~ "26",

    v630a %in% c("EAR PROBLEM", "ENT (DAT (TEETH)KO CHECK GARAUNA GAYEKO TARA SSF MA DA NAPARNE VAYERA NAK", 
                "GHATI KO SAMASYA", "GHATI MA GIRKHA", "GHATIMA SAMASYA", "JIBRO KO SAMASYA", 
                "MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", "NAK RA MUKHA BATA BLOOD AKO", 
                "NAKKO SAMSHYA", "TANSIL", "TONSIL", "TONSILLITIS", "TONSILS", "ट्वान्सिल") ~ "27",

    v630a %in% c("AKHAMA CHO", "EYE PROBLEM") ~ "28",

    v630a == "DAAD" ~ "29",

    v630a %in% c("BODYSCHE", "JIU DUKHEKO", "KAMJORI", "KAMJORI BHAYEKO", "KAMJORI VAYEKO") ~ "30",

    v630a %in% c("CHECK UP PREGNANT NA BHAYERA", "GAINO PATHEGHAR SAMASYA", "GYAENO PROBLEM", 
                "GYANO KO PROBLEM", "IRREGULAR MENSURATION", "MINS VAYAKO BELA PET DUKHEKO", 
                "PATHAGHAR SAMANDI SAMASYA IS", "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", 
                "PERIOD ANIYAMIT HUNE", "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", 
                "SHIST SURGERY", "UTERUS INFECTION", "UTERUS PROBLAM") ~ "31",

    v630a %in% c("KAMJORI BP LOW", "MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a %in% c("FOLLOWUP OF HERNIA OPERATIO", "HARNIYA", "HARNIYA KO OPERATION", "HERNIA") ~ "33",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI") ~ "36",

    v630a %in% c("GAL BLADORS ROBLAM", "LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                "PATTHARIKO AUSADHI", "STONE", 
                "UHA KO URIC ACID ATHAWA LIVER KO SAMASYA LEY HAST DUKHEKO VANERA DOCTOR LEY VANNU BHAYO") ~ "37",

    v630a %in% c("CHATI DUKHEKO", "CHATTI DUKHA SAMASYA", "CHEST INFECTION", 
                "FOKSO MA PANI DEKHIYEKO", "TUBERCULOSIS") ~ "38",

    v630a %in% c("BHULNE SAMASYA", "HEAD ISSUES", "MANASIK SAMASYA") ~ "40",

    v630a %in% c("DIZZINESS", "HEADACE", "HEADACHE", "JHUTTA JHAMJHAMAUNE", "MIGRAINE", 
                "MIGREN HEADACHE", "NASA", "NASA DABE KO", "NASA SAMBANDHI", 
                "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", "TAUKO DUKHANE", 
                "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", 
                "THAUKO DUKHANE", "TUKO DUKHAYA") ~ "41",

    v630a %in% c("BATHA ROGA", "URIC ACID", "URIK ASID") ~ "42",

    v630a == "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO" ~ "43",

    v630a == "FOOD POISON" ~ "22",

    v630a %in% c("CHEST & STOMACH PROBLEM", "TIFID", "TYPHOID") ~ "2",

    v630a == "FOKSO KO PROBLEMP" ~ "38",

    v630a %in% c("KEHI VAKO CHHAIN CHHAIN") ~ "6",

    v630a %in% c("BLOOD AND URINE INFECTION" , "YOUN ROD PANI BAGNE") ~ "10",

    v630a %in% c("BLOOD INFECTION") ~ "20",

    v630a == "EYE CHECK GARDA" ~ "28",

    v630a == "BRUSELA" ~ "17",

    v630a %in% c("BACK PAIN", "BACK PAIN KO SAMASYA BHAKO THIYO", "BACKPAIN", 
                 "DHAD DUKHE", "DHAD DUKHNE", "DHAD DUKHNE KHUTTA DUKHNE", 
                 "DHADA DUKHEKO", "DISCOGENIC LBD(LOWER BACK PAIN)", "GHUDA DUKHANE", 
                 "HADJORANI DUKHEKO", "HATH DUKHEKO", "HATH KHUTTA DUKHAI", 
                 "KAMAR GHUDA DUKHEKOLE", "KURKUCHA DUKHNE POLNE", 
                 "BODY ACHE", "BODY PAIN", "हात खुट्टा कम्मर दुखेको") ~ "19",

    v630a %in% c("ABDOMEN PAIN", "APPENDIX", "GALLSTONE", "GASTIC", "GASTIK", "STOMACH  INFECTION",
                 "GASTRIC INFECTION", "GASTRITIS", "HEART BURN", "DISHA GOTA PAREKO",
                 "INTESTINE OPERATION SUDDENLY AS THERE WAS GROWTH IN HIS INTESTINE", 
                 "KABJIYAT", "KAMMAR DUKHEKO", "KOKHA DUKHEKO", "PAYALSH", "PAYELS", 
                 "PET DUKHANE", "PET DUKHERA", "PET DUKHERA VOMIT BHAKO", "PET KO SAMASAYA", 
                 "PETKO OPERATION GAREKO", "PILES", "STOMACH", "STOMACH INFECTION", 
                 "STOMACH ACHE", "STOMACH PAIN", "एपेन्डिसाइड", "ABDOMINAL PAIN",
                 "THEY DON'T KNOW ABOUT THE ACTUAL DISEASE AS PER THE DOCTOR THEY ALSO DON'T KNOW THE ACTUAL DISEASE . GASTRIC") ~ "22",

    v630a %in% c("PREGNANCY CHECK UP", "PREGNANT", 
                 "UHA KO BREAST FEEDING GARNA KO LAGI AWASHEK MATRA MA DUDH NAPAKO HUNALEY BIGAT EK HAFTA DEKHI AAUSADHI SEWAN GARDAI HUNUNXA") ~ "25",

    v630a %in% c("GHATI KO SAMASYA", "NAAK MA MASU PALAKO", "PINASH", "TONSIL") ~ "27",

    v630a == "OVERALL" ~ "30",

    v630a %in% c("MAHINA BARI NIHAMIT NAVAYERA", "PATHAK GHAR SAMANDI SAMASYA", 
                 "PATHEGHAR KO OPERATION", "PATHEGHAR KO SAMASYA", "PATHEGHAR SAMBANDI SAMASYA THIYO") ~ "31",

    v630a == "HEART PROBLEM" ~ "32",

    v630a %in% c("HARNIYA KO OPERATION GAREKO", "HARNIYA KO OPERATION  GAREKO") ~ "33",

    v630a == "HIV AIDS" ~ "34",

    v630a %in% c("KIDANEY MA PATHARIYA", "KIDNEY INFECTION", "KIDNEY STONE", "KIDANEY  MA PATHARIYA",
                 "KIDNI JACHA RA UPACHAR", "PISABMA KHARABI", "STONE OPERATION") ~ "36",

    v630a %in% c("MILD LIVER DISEASE", "PATHARI", "PATHARI KO OPERATION", "PATTHARIYA", 
                 "PITA THAILIMA PATHARI KO", "PITKO THAILI MA PATHALI", "PITTATHAILI KO OPERATION") ~ "37",

    v630a == "PROSTATE" ~ "39",

    v630a %in% c("BEHOSH VAYEKO EKKASHI", "DHARD KO NASA CHAPIYA KO", "MIGRAINE", 
                 "PARALYSIS", "RINGADA CHALEKO", "RINGATA", "TAUKO DUKHAI", 
                 "TAUKO DUKHEKO", "YAUTA LEG NACHALEKO") ~ "41",

    v630a == "WORM" ~ "44",

    TRUE ~ as.character(v630)
  )
  )

for (i in setdiff(1:ncol(section6c1), c(2, 7, 8, 14, 15, 16))) {
  section6c1[[i]] <- as.numeric(gsub("[^0-9]", "", section6c1[[i]]))
}

cols_after_v629 <- names(section6c1)[(match("v629", names(section6c1)) + 1):ncol(section6c1)]

section6c1 <- section6c1 %>%
  mutate(
    across(
      all_of(cols_after_v629),
      ~ ifelse(v629 == 2 & !is.na(v630), NA, .)
    ),
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    !disease_id %in% c("3109-9-8-NA")
  )

section6c1 <- section6c1 %>%
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
      v630 == 96 ~ 19,
      TRUE ~ v630
    )
  ) 

#SECTION6C2

section6c2 <- section6c2 %>%
  mutate(
    v630_num = suppressWarnings(as.numeric(str_extract(v630, "\\d+"))),

    v630_txt = str_trim(
      str_remove_all(v630, "\\d+|,")
    ),

    v630  = v630_num,
    v630a = if_else(v630_txt != "", v630_txt, NA_character_)
  ) %>%
  select(-v630_num, -v630_txt)  

section6c2 <- section6c2 %>%
  mutate(
  v630 = case_when(
    v630a %in% c("CHISO COLD", "CHISO RUGHA", "COLD ALLERGY", "KHOKI", "KHOKI LAGEKO", "RUGAKHOKI",
                "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO", "खोकी", "रुघाखोकी", "RUGHA KHOKI",
                "ROUGHA KHOLA", "COLD", "COLD") ~ "6",

    v630a == "JONDISH LIVAR PHET MA PANI VKO" ~ "9",

    v630a %in% c("URINE INFECTION", "URINE PROBLEM") ~ "10",

    v630a %in% c("DATA KO SAMASYA", "DAAT KO SAMASYA", "DAAT KO SAMASYA") ~ "11",

    v630a %in% c("ALLERGIES", "ALLERGY", "CHALA SAMBANDHI ROGH", "GHAU KHATIRA", 
                "JUI KHATIRA", "KHUTTA MA ALLERGY AAKO", "PANI FOKA BATA PANI NILAKELL") ~ "14",

    v630a %in% c("BANCHARO BATA KHUTA KATIYO", "HAAT VACHIYEKO UPACHAR", 
                "HAND FRACTURE", "LIGAMENT TEARING", "LEGAMENT SMSYA") ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE", "SNAKE BITE") ~ "18",

    v630a %in% c("BACK PAIN", "BACKBONE MA PROBLEM VAYERA", "BONE PAIN", "DHAD DUKHEKO", 
                "DHAD DUKHNE", "DHAD DUKHNE SAMASYA", "DHAD KO", "DHADKO DUKHAI", 
                "DHARD DUKHNA SAMYASYA", "GODA DUKHEKO", "GUDHA DUKHEKO", "GHUDA DUKHEKO",
                "HAAD JORNI KO SAMASYA", "HAAT KHUTTA KO JORNI DUKHEKO", "DHARD DUKHNA",
                "HAAT KO HADDI KO PROBLEM", "HADIKHIYER", "HADIKO SAMASA", 
                "HADJORNI", "HADJORNI DUKHNE HATKO", "HARDJORNI DUKHNE", "HAT DUKHERA", 
                "HAT KHUTTA DUKHEKO", "HAT KHUTTA DUKHNA", "HAT KHUTTA DUKHNE", 
                "HATH KO HADI DUKHEKO", "HATT KHUTTA DUKHNA", "JOINT PAIN", "JYU DHUKHEKO", 
                "JYU DUKHNE", "KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                "MULTIPLE JOINT PAIN", "PAIN IN LEG", "खुट्टा को कुर्कुच्चा दुख्ने", "HADDI",
                "KARANG DUKHEKO", "JIU HATH DUKHEKO", "DHARD DUKHNA", "HAT DUKHNE SAMASAYA",
                "DHARD DUKHNA") ~ "19",

    v630a %in% c("ANAEMIA", "INCREASE IN CHOLESTEROL LEVEL", "RAGATMA KHARABI", "KAMJORIBP LOW", 
                 "INCREASES IN CHOLESTEROL LEVEL", "ANEMIA") ~ "20",

    v630a %in% c("BONE MARROW TRANSPLANT", "CANCER KO LAXAN", "LUMP IN BREAST") ~ "21",

    v630a %in% c("ANDRA MA GAU", "APPENDICITIS OPERATION", "APPENDIX", "BHOMIT BHAYEKO", 
                "CONSTIPATION", "GASTIK", "GASTRIC", "GASTRIC KO PROBLEM", "GASTRITIS", 
                "GYASTRIK", "HAAT DUKHNE PAYALS", "LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", 
                "PET DUKHER", "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", "PET KO CHECK", 
                "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", "PETA DUKHEKO", 
                "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE", "ULCER", "PET DUKNE",
                "ULCER KO OPERATION", "VOMITING", "YAPENDIKS KO OPTION", "PET DUKHYAKO",
                "PETDUKHERA", "DOCTOR DOESN'T KNOW ABOUT THEIR CONDITION THEY SAY HE HAS GASTRITIS.",
                "PET DUKHERA FOOD POISON BHAKO", "PET DHUKERA", "GASTRIC PROBLEM", "APPENDIX KO OPTION",
                "PET KO SAMASYAA VAAKO BU K HO VANERA THA NAVAAKO HOSPITAL MA.", "ANDRA MA GHAU",
                "PETKO SAMASYAA") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a == "DAMM" ~ "24",

    v630a %in% c("DELIVERY", "DELIVERY CHECKUP", "PERGINENC", "PREGENCY", "DELIVERY CONDITION",
                "PREGNANCY CHECK UP", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", 
                "PREGNANT", "SUTKERI", "SUTKERI BHAYEKO", "ANC CHECKUP VISIT IN PRIVATE HOSPITAL") ~ "25",

    v630a == "JANMA JATA APANGA" ~ "26",

    v630a %in% c("EAR PROBLEM", "ENT", "ENT (DAT (TEETH)KO CHECK GARAUNA GAYEKO TARA SSF MA DA NAPARNE VAYERA NAK", 
                "GHATI KO SAMASYA", "GHATI MA GIRKHA", "GHATIMA SAMASYA", "JIBRO KO SAMASYA", "ENT",
                "MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", "NAK RA MUKHA BATA BLOOD AKO", 
                "NAKKO SAMSHYA", "TANSIL", "TONSIL", "TONSILLITIS", "TONSILS", "ट्वान्सिल", "NOSE TONSIL") ~ "27",

    v630a %in% c("AKHAMA CHO", "EYE PROBLEM") ~ "28",

    v630a == "DAAD" ~ "29",

    v630a %in% c("BODYSCHE", "JIU DUKHEKO", "KAMJORI", "KAMJORI BHAYEKO", "KAMJORI VAYEKO",
                 "NORMAL", "OVERALL WHOLE BODY CHECK", "BODEYSCHE", "DUKHAI") ~ "30",

    v630a %in% c("CHECK UP PREGNANT NA BHAYERA", "GAINO PATHEGHAR SAMASYA", "GYAENO PROBLEM", 
                "GYANO KO PROBLEM", "IRREGULAR MENSURATION", "MINS VAYAKO BELA PET DUKHEKO", 
                "PATHAGHAR SAMANDI SAMASYA IS", "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", 
                "PERIOD ANIYAMIT HUNE", "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", 
                "SHIST SURGERY", "UTERUS INFECTION", "UTERUS PROBLAM", "GAINO PATHEGHAR",
                "PERIOD ANIYAMIT", "GYENO O KO PROBLEM") ~ "31",

    v630a %in% c("KAMJORI BP LOW", "MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a %in% c("FOLLOWUP OF HERNIA OPERATIO", "FOLLOWUP OF HERNIA OPERATION", "HARNIYA", "HARNIYA KO OPERATION", "HERNIA",
                 "FOLLOWUP OF HERNIA OPERATION") ~ "33",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI",
                 "KIDNEY MA PATHARIYA") ~ "36",

    v630a %in% c("GAL BLADORS ROBLAM", "LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                "PATTHARIKO AUSADHI", "STONE", "GAL BLADOR PROBLAM", "LIVER KO SAMASYA",
                "UHA KO URIC ACID ATHAWA LIVER KO SAMASYA LEY HAST DUKHEKO VANERA DOCTOR LEY VANNU BHAYO",
                "PITKO THAILI MA PATHALI VAYEKO MA OPRESAN GAREKO") ~ "37",

    v630a %in% c("CHATI DUKHEKO", "CHATTI DUKHA SAMASYA", "CHEST INFECTION", 
                "FOKSO MA PANI DEKHIYEKO", "TUBERCULOSIS") ~ "38",

    v630a %in% c("BHULNE SAMASYA", "HEAD ISSUES", "MANASIK SAMASYA") ~ "40",

    v630a %in% c("DIZZINESS", "HEADACE", "HEADACHE", "JHUTTA JHAMJHAMAUNE", "MIGRAINE", 
                "MIGREN HEADACHE", "NASA", "NASA DABE KO", "NASA SAMBANDHI", 
                "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", "TAUKO DUKHANE", 
                "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", 
                "THAUKO DUKHANE", "TUKO DUKHAYA", "TAU KO DUKHNA", "HEAD INJURIES") ~ "41",

    v630a %in% c("BATHA ROGA", "URIC ACID", "URIK ASID", "URIKASID") ~ "42",

    v630a == "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO" ~ "43",

    v630a == "RAGATMA KHARABI" ~ "20",

    v630a %in% c("KHOKI", "KHOKI LAGEKO", "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO") ~ "6",

    v630a %in% c("KHUTTA MA ALLERGY AAKO", "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO", 
                 "PANI FOKA BATA PANI NILAKELL", "SARIR MA PANIKO PHOKA AYEKO") ~ "14",

    v630a == "LIGAMENT TEARING" ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630a %in% c("KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                 "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                 "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                 "MULTIPLE JOINT PAIN", "PAIN IN LEG") ~ "19",

    v630a == "LUMP IN BREAST" ~ "21",

    v630a %in% c("LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", "PET DUKHER", "PAYELSH",
                 "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                 "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", 
                 "PET KO CHECK", "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", 
                 "PETA DUKHEKO", "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE",
                 "BHOMIT BHAYERA NAROKIYEKO", "PETDUKHEKO") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a %in% c("PERGINENC", "PREGENCY", "PREGNANCY CHECK UP", 
                 "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", "PREGNANT", 
                 "SUTKERI", "SUTKERI BHAYEKO") ~ "25",

    v630a %in% c("MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", 
                 "NAK RA MUKHA BATA BLOOD AKO", "NAKKO SAMSHYA", "TANSIL", 
                 "TONSIL", "TONSILLITIS", "TONSILS") ~ "27",

    v630a %in% c("KAMJORI", "KAMJORI BHAYEKO", "KAMJORI BP LOW", "KAMJORI VAYEKO", "JIUDUKHE KO",
                 "JIU DUKHNE") ~ "30",

    v630a %in% c("MINS VAYAKO BELA PET DUKHEKO", "PATHAGHAR SAMANDI SAMASYA IS", 
                 "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", "PERIOD ANIYAMIT HUNE", 
                 "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", "SHIST SURGERY",
                 "EK DAMAI GARO VAYO MAHINA BARI NIHAMIT NAVAYERA", "PATHAGHAR SAMANDI",
                 "PATHAGHAR SAMANDI SAMASYA", "LOW ABDOMINAL PAIN WHITE VAGINAL DISCHARGE BURNING MICTURITION") ~ "31",

    v630a %in% c("MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI") ~ "36",

    v630a %in% c("LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                 "PATTHARIKO AUSADHI", "STONE") ~ "37",

    v630a == "TUBERCULOSIS" ~ "38",

    v630a == "MANASIK SAMASYA" ~ "40",

    v630a %in% c("MIGRAINE", "MIGREN HEADACHE", "NASA", "NASA DABE KO", 
                 "NASA SAMBANDHI", "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", 
                 "TAUKO DUKHANE", "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", 
                 "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", "THAUKO DUKHANE", "TUKO DUKHAYA") ~ "41",

    v630a %in% c("CHISO COLD", "CHISO RUGHA", "COLD ALLERGY", "KHOKI", "KHOKI LAGEKO", 
                "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO", "खोकी", "रुघाखोकी") ~ "6",

    v630a %in% c("JONDISH LIVAR PHET MA PANI VKO", "JANDIS") ~ "9",

    v630a %in% c("URINE INFECTION", "URINE PROBLEM") ~ "10",

    v630a == "DATA KO SAMASYA" ~ "11",

    v630a %in% c("ALLERGIES", "ALLERGY", "CHALA SAMBANDHI ROGH", "GHAU KHATIRA", "PANI FOKA BATA PANI NIKALEKO",
                "JUI KHATIRA", "KHUTTA MA ALLERGY AAKO", "PANI FOKA BATA PANI NILAKELL", "KHATERA",
                "KHUTTA MA ELERGY BHAYEKO") ~ "14",

    v630a %in% c("BANCHARO BATA KHUTA KATIYO", "HAAT VACHIYEKO UPACHAR", 
                "HAND FRACTURE", "LIGAMENT TEARING", "TAUKO MA GHAU") ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630a %in% c("BACK PAIN", "BACKBONE MA PROBLEM VAYERA", "BONE PAIN", "DHAD DUKHEKO", 
                "DHAD DUKHNE", "DHAD DUKHNE SAMASYA", "DHAD KO", "DHADKO DUKHAI", 
                "DHARD DUKHNA SAMYASYA", "GODA DUKHEKO", "GUDHA DUKHEKO", "DHADA DUKHE KO",
                "HAAD JORNI KO SAMASYA", "HAAT KHUTTA KO JORNI DUKHEKO", "HADIKHIYERA",
                "HAAT KO HADDI KO PROBLEM", "HADIKHIYER", "HADIKO SAMASA", 
                "HADJORNI", "HADJORNI DUKHNE HATKO", "HARDJORNI DUKHNE", "HAT DUKHERA", 
                "HAT KHUTTA DUKHEKO", "HAT KHUTTA DUKHNA", "HAT KHUTTA DUKHNE", 
                "HATH KO HADI DUKHEKO", "HATT KHUTTA DUKHNA", "JOINT PAIN", "JYU DHUKHEKO", 
                "JYU DUKHNE", "KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                "MULTIPLE JOINT PAIN", "PAIN IN LEG", "खुट्टा को कुर्कुच्चा दुख्ने", "KAMAR GHUDA DUKHEKO") ~ "19",

    v630a %in% c("ANAEMIA", "INCREASE IN CHOLESTEROL LEVEL", "RAGATMA KHARABI", "NASAKO SAMASYA",
                 "RAGATKO KHARABI") ~ "20",

    v630a %in% c("BONE MARROW TRANSPLANT", "CANCER KO LAXAN", "LUMP IN BREAST",
                 "SYMPTOMS OF CANCER KIDNEY INFECTION", "CANCER") ~ "21",

    v630a %in% c("ANDRA MA GAU", "APPENDICITIS OPERATION", "APPENDIX", "BHOMIT BHAYEKO", 
                "CONSTIPATION", "GASTIK", "GASTRIC", "GASTRIC KO PROBLEM", "GASTRITIS", 
                "GYASTRIK", "HAAT DUKHNE PAYALS", "LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", 
                "PET DUKHER", "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", "PET KO CHECK", 
                "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", "PETA DUKHEKO", 
                "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE", "ULCER", 
                "ULCER KO OPERATION", "VOMITING", "YAPENDIKS KO OPTION", "EPIGASTRIC PAIN",
                "INTESTINE OPERATION", "APPENDICITIS") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a == "DAMM" ~ "24",

    v630a %in% c("DELIVERY", "DELIVERY CHECKUP", "PERGINENC", "PREGENCY", 
                "PREGNANCY CHECK UP", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", 
                "PREGNANT", "SUTKERI", "SUTKERI BHAYEKO", "PERGINENC TEST") ~ "25",

    v630a == "JANMA JATA APANGA" ~ "26",

    v630 %in% c("EAR PROBLEM", "ENT (DAT (TEETH)KO CHECK GARAUNA GAYEKO TARA SSF MA DA NAPARNE VAYERA NAK", 
                "GHATI KO SAMASYA", "GHATI MA GIRKHA", "GHATIMA SAMASYA", "JIBRO KO SAMASYA", 
                "MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", "NAK RA MUKHA BATA BLOOD AKO", 
                "NAKKO SAMSHYA", "TANSIL", "TONSIL", "TONSILLITIS", "TONSILS", "ट्वान्सिल", "NOSE BLEEDING") ~ "27",

    v630a %in% c("AKHAMA CHO", "EYE PROBLEM", "EYE CHECK GARNA GAYEKO") ~ "28",

    v630a == "DAAD" ~ "29",

    v630a %in% c("BODYSCHE", "JIU DUKHEKO", "NORMAL", "KAMJORI", "KAMJORI BHAYEKO", "KAMJORI VAYEKO") ~ "30",

    v630a %in% c("CHECK UP PREGNANT NA BHAYERA", "GAINO PATHEGHAR SAMASYA", "GYAENO PROBLEM", 
                "GYANO KO PROBLEM", "IRREGULAR MENSURATION", "MINS VAYAKO BELA PET DUKHEKO", 
                "PATHAGHAR SAMANDI SAMASYA IS", "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", 
                "PERIOD ANIYAMIT HUNE", "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", 
                "SHIST SURGERY", "UTERUS INFECTION", "UTERUS PROBLAM", "PREGNANT NA BHAYERA CHECK UP") ~ "31",

    v630a %in% c("KAMJORI BP LOW", "MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a %in% c("FOLLOWUP OF HERNIA OPERATIO", "HARNIYA", "HARNIYA KO OPERATION", "HERNIA") ~ "33",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI", "PAYHARI") ~ "36",

    v630a %in% c("GAL BLADORS ROBLAM", "LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                "PATTHARIKO AUSADHI", "STONE", "PITA THAILIKO PATHARI", "JONDISH LIVAR PHET MA PANI",
                "UHA KO URIC ACID ATHAWA LIVER KO SAMASYA LEY HAST DUKHEKO VANERA DOCTOR LEY VANNU BHAYO") ~ "37",

    v630a %in% c("CHATI DUKHEKO", "CHATTI DUKHA SAMASYA", "CHEST INFECTION", 
                "FOKSO MA PANI DEKHIYEKO", "TUBERCULOSIS", "FOKSO KO PROBLEM") ~ "38",

    v630a %in% c("BHULNE SAMASYA", "HEAD ISSUES", "MANASIK SAMASYA") ~ "40",

    v630a %in% c("DIZZINESS", "HEADACE", "HEADACHE", "JHUTTA JHAMJHAMAUNE", "MIGRAINE", 
                "MIGREN HEADACHE", "NASA", "NASA DABE KO", "NASA SAMBANDHI", 
                "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", "TAUKO DUKHANE", 
                "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", 
                "THAUKO DUKHANE", "TUKO DUKHAYA", "KHUTTA JHAMJHAMAUNE") ~ "41",

    v630a %in% c("BATHA ROGA", "URIC ACID", "URIK ASID") ~ "42",

    v630a %in% c("KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO", "OPERATION KHUTTA KOMKHIL") ~ "43",

    v630a == "FOOD POISON" ~ "22",

    v630a %in% c("CHEST & STOMACH PROBLEM", "TIFID", "TYPHOID", "THYPHOID", "CHEST & STOMACH PAIN") ~ "2",

    v630a == "FOKSO KO PROBLEMP" ~ "38",

    v630a %in% c( "ANC CHECKUP IN PRIVATE HOSPITAL", "KEHI VAKO CHHAIN CHHAIN") ~ "6",

    v630a %in% c("BLOOD AND URINE INFECTION" , "YOUN ROD PANI BAGNE") ~ "10",

    v630a %in% c("BLOOD INFECTION") ~ "20",

    v630a == "EYE CHECK GARDA" ~ "28",

    v630a == "BRUSELA" ~ "17",

    v630a %in% c("BACK PAIN", "BACK PAIN KO SAMASYA BHAKO THIYO", "BACKPAIN", 
                 "DHAD DUKHE", "DHAD DUKHNE", "DHAD DUKHNE KHUTTA DUKHNE", 
                 "DHADA DUKHEKO", "DISCOGENIC LBD(LOWER BACK PAIN)", "GHUDA DUKHANE", 
                 "HADJORANI DUKHEKO", "HATH DUKHEKO", "HATH KHUTTA DUKHAI", 
                 "KAMAR GHUDA DUKHEKOLE", "KURKUCHA DUKHNE POLNE", 
                 "BODY ACHE", "BODY PAIN", "हात खुट्टा कम्मर दुखेको") ~ "19",

    v630a %in% c("ABDOMEN PAIN", "APPENDIX", "GALLSTONE", "GASTIC", "GASTIK", "STOMACH  INFECTION",
                 "GASTRIC INFECTION", "GASTRITIS", "HEART BURN", "DISHA GOTA PAREKO",
                 "INTESTINE OPERATION SUDDENLY AS THERE WAS GROWTH IN HIS INTESTINE", 
                 "KABJIYAT", "KAMMAR DUKHEKO", "KOKHA DUKHEKO", "PAYALSH", "PAYELS", 
                 "PET DUKHANE", "PET DUKHERA", "PET DUKHERA VOMIT BHAKO", "PET KO SAMASAYA", 
                 "PETKO OPERATION GAREKO", "PILES", "STOMACH", "STOMACH INFECTION", 
                 "STOMACH ACHE", "STOMACH PAIN", "एपेन्डिसाइड", "ABDOMINAL PAIN",
                 "THEY DON'T KNOW ABOUT THE ACTUAL DISEASE AS PER THE DOCTOR THEY ALSO DON'T KNOW THE ACTUAL DISEASE . GASTRIC") ~ "22",

    v630a %in% c("PREGNANCY CHECK UP", "PREGNANT", 
                 "UHA KO BREAST FEEDING GARNA KO LAGI AWASHEK MATRA MA DUDH NAPAKO HUNALEY BIGAT EK HAFTA DEKHI AAUSADHI SEWAN GARDAI HUNUNXA") ~ "25",

    v630a %in% c("GHATI KO SAMASYA", "NAAK MA MASU PALAKO", "PINASH", "TONSIL", "NOSE BLEEDING") ~ "27",

    v630a == "OVERALL" ~ "30",

    v630a %in% c("MAHINA BARI NIHAMIT NAVAYERA", "PATHAK GHAR SAMANDI SAMASYA", 
                 "PATHEGHAR KO OPERATION", "PATHEGHAR KO SAMASYA", "PATHEGHAR SAMBANDI SAMASYA THIYO") ~ "31",

    v630a == "HEART PROBLEM" ~ "32",

    v630a %in% c("HARNIYA KO OPERATION GAREKO", "HARNIYA KO OPERATION  GAREKO") ~ "33",

    v630a == "HIV AIDS" ~ "34",

    v630a %in% c("KIDANEY MA PATHARIYA", "KIDNEY INFECTION", "KIDNEY STONE", "KIDANEY  MA PATHARIYA",
                 "KIDNI JACHA RA UPACHAR", "PISABMA KHARABI", "STONE OPERATION") ~ "36",

    v630a %in% c("MILD LIVER DISEASE", "PATHARI", "PATHARI KO OPERATION", "PATTHARIYA", 
                 "PITA THAILIMA PATHARI KO", "PITKO THAILI MA PATHALI", "PITTATHAILI KO OPERATION") ~ "37",

    v630a == "PROSTATE" ~ "39",

    v630a %in% c("BEHOSH VAYEKO EKKASHI", "DHARD KO NASA CHAPIYA KO", "MIGRAINE", 
                 "PARALYSIS", "RINGADA CHALEKO", "RINGATA", "TAUKO DUKHAI", 
                 "TAUKO DUKHEKO", "YAUTA LEG NACHALEKO", "RINGADA CHALNE") ~ "41",

    v630a == "WORM" ~ "44",

    TRUE ~ as.character(v630)
  )
)

#SECTION6C4

section6c4 <- section6c4 %>%
  mutate(
    v630_num = str_extract(v630, "\\d+"),
    
    v630_txt = str_trim(str_remove_all(v630, "\\d+|,")),
    
    v630 = if_else(!is.na(v630_num), v630_num, NA_character_),
    
    v630a = if_else(v630_txt != "", v630_txt, NA_character_)
  ) %>%
  select(-v630_num, -v630_txt) 

section6c4 <- section6c4 %>%
  filter(v630 != "") %>%
  mutate(
  v630 = case_when(
    v630a %in% c("CHISO COLD", "CHISO RUGHA", "COLD ALLERGY", "KHOKI", "KHOKI LAGEKO", "RUGAKHOKI",
                "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO", "खोकी", "रुघाखोकी", "RUGHA KHOKI",
                "ROUGHA KHOLA", "COLD", "COLD") ~ "6",

    v630a == "JONDISH LIVAR PHET MA PANI VKO" ~ "9",

    v630a %in% c("URINE INFECTION", "URINE PROBLEM") ~ "10",

    v630a %in% c("DATA KO SAMASYA", "DAAT KO SAMASYA", "DAAT KO SAMASYA") ~ "11",

    v630a %in% c("ALLERGIES", "ALLERGY", "CHALA SAMBANDHI ROGH", "GHAU KHATIRA", 
                "JUI KHATIRA", "KHUTTA MA ALLERGY AAKO", "PANI FOKA BATA PANI NILAKELL") ~ "14",

    v630a %in% c("BANCHARO BATA KHUTA KATIYO", "HAAT VACHIYEKO UPACHAR", 
                "HAND FRACTURE", "LIGAMENT TEARING", "LEGAMENT SMSYA") ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE", "SNAKE BITE") ~ "18",

    v630a %in% c("BACK PAIN", "BACKBONE MA PROBLEM VAYERA", "BONE PAIN", "DHAD DUKHEKO", 
                "DHAD DUKHNE", "DHAD DUKHNE SAMASYA", "DHAD KO", "DHADKO DUKHAI", 
                "DHARD DUKHNA SAMYASYA", "GODA DUKHEKO", "GUDHA DUKHEKO", "GHUDA DUKHEKO",
                "HAAD JORNI KO SAMASYA", "HAAT KHUTTA KO JORNI DUKHEKO", "DHARD DUKHNA",
                "HAAT KO HADDI KO PROBLEM", "HADIKHIYER", "HADIKO SAMASA", 
                "HADJORNI", "HADJORNI DUKHNE HATKO", "HARDJORNI DUKHNE", "HAT DUKHERA", 
                "HAT KHUTTA DUKHEKO", "HAT KHUTTA DUKHNA", "HAT KHUTTA DUKHNE", 
                "HATH KO HADI DUKHEKO", "HATT KHUTTA DUKHNA", "JOINT PAIN", "JYU DHUKHEKO", 
                "JYU DUKHNE", "KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                "MULTIPLE JOINT PAIN", "PAIN IN LEG", "खुट्टा को कुर्कुच्चा दुख्ने", "HADDI",
                "KARANG DUKHEKO", "JIU HATH DUKHEKO", "DHARD DUKHNA", "HAT DUKHNE SAMASAYA",
                "DHARD DUKHNA") ~ "19",

    v630a %in% c("ANAEMIA", "INCREASE IN CHOLESTEROL LEVEL", "RAGATMA KHARABI", "KAMJORIBP LOW", 
                 "INCREASES IN CHOLESTEROL LEVEL", "ANEMIA") ~ "20",

    v630a %in% c("BONE MARROW TRANSPLANT", "CANCER KO LAXAN", "LUMP IN BREAST") ~ "21",

    v630a %in% c("ANDRA MA GAU", "APPENDICITIS OPERATION", "APPENDIX", "BHOMIT BHAYEKO", 
                "CONSTIPATION", "GASTIK", "GASTRIC", "GASTRIC KO PROBLEM", "GASTRITIS", 
                "GYASTRIK", "HAAT DUKHNE PAYALS", "LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", 
                "PET DUKHER", "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", "PET KO CHECK", 
                "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", "PETA DUKHEKO", 
                "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE", "ULCER", "PET DUKNE",
                "ULCER KO OPERATION", "VOMITING", "YAPENDIKS KO OPTION", "PET DUKHYAKO",
                "PETDUKHERA", "DOCTOR DOESN'T KNOW ABOUT THEIR CONDITION THEY SAY HE HAS GASTRITIS.",
                "PET DUKHERA FOOD POISON BHAKO", "PET DHUKERA", "GASTRIC PROBLEM", "APPENDIX KO OPTION",
                "PET KO SAMASYAA VAAKO BU K HO VANERA THA NAVAAKO HOSPITAL MA.", "ANDRA MA GHAU",
                "PETKO SAMASYAA") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a == "DAMM" ~ "24",

    v630a %in% c("DELIVERY", "DELIVERY CHECKUP", "PERGINENC", "PREGENCY", "DELIVERY CONDITION",
                "PREGNANCY CHECK UP", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", 
                "PREGNANT", "SUTKERI", "SUTKERI BHAYEKO", "ANC CHECKUP VISIT IN PRIVATE HOSPITAL") ~ "25",

    v630a == "JANMA JATA APANGA" ~ "26",

    v630a %in% c("EAR PROBLEM", "ENT", "ENT (DAT (TEETH)KO CHECK GARAUNA GAYEKO TARA SSF MA DA NAPARNE VAYERA NAK", 
                "GHATI KO SAMASYA", "GHATI MA GIRKHA", "GHATIMA SAMASYA", "JIBRO KO SAMASYA", "ENT",
                "MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", "NAK RA MUKHA BATA BLOOD AKO", 
                "NAKKO SAMSHYA", "TANSIL", "TONSIL", "TONSILLITIS", "TONSILS", "ट्वान्सिल", "NOSE TONSIL") ~ "27",

    v630a %in% c("AKHAMA CHO", "EYE PROBLEM") ~ "28",

    v630a == "DAAD" ~ "29",

    v630a %in% c("BODYSCHE", "JIU DUKHEKO", "KAMJORI", "KAMJORI BHAYEKO", "KAMJORI VAYEKO",
                 "NORMAL", "OVERALL WHOLE BODY CHECK", "BODEYSCHE", "DUKHAI") ~ "30",

    v630a %in% c("CHECK UP PREGNANT NA BHAYERA", "GAINO PATHEGHAR SAMASYA", "GYAENO PROBLEM", 
                "GYANO KO PROBLEM", "IRREGULAR MENSURATION", "MINS VAYAKO BELA PET DUKHEKO", 
                "PATHAGHAR SAMANDI SAMASYA IS", "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", 
                "PERIOD ANIYAMIT HUNE", "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", 
                "SHIST SURGERY", "UTERUS INFECTION", "UTERUS PROBLAM", "GAINO PATHEGHAR",
                "PERIOD ANIYAMIT", "GYENO O KO PROBLEM") ~ "31",

    v630a %in% c("KAMJORI BP LOW", "MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a %in% c("FOLLOWUP OF HERNIA OPERATIO", "FOLLOWUP OF HERNIA OPERATION", "HARNIYA", "HARNIYA KO OPERATION", "HERNIA",
                 "FOLLOWUP OF HERNIA OPERATION") ~ "33",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI",
                 "KIDNEY MA PATHARIYA") ~ "36",

    v630a %in% c("GAL BLADORS ROBLAM", "LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                "PATTHARIKO AUSADHI", "STONE", "GAL BLADOR PROBLAM", "LIVER KO SAMASYA",
                "UHA KO URIC ACID ATHAWA LIVER KO SAMASYA LEY HAST DUKHEKO VANERA DOCTOR LEY VANNU BHAYO",
                "PITKO THAILI MA PATHALI VAYEKO MA OPRESAN GAREKO") ~ "37",

    v630a %in% c("CHATI DUKHEKO", "CHATTI DUKHA SAMASYA", "CHEST INFECTION", 
                "FOKSO MA PANI DEKHIYEKO", "TUBERCULOSIS") ~ "38",

    v630a %in% c("BHULNE SAMASYA", "HEAD ISSUES", "MANASIK SAMASYA") ~ "40",

    v630a %in% c("DIZZINESS", "HEADACE", "HEADACHE", "JHUTTA JHAMJHAMAUNE", "MIGRAINE", 
                "MIGREN HEADACHE", "NASA", "NASA DABE KO", "NASA SAMBANDHI", 
                "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", "TAUKO DUKHANE", 
                "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", 
                "THAUKO DUKHANE", "TUKO DUKHAYA", "TAU KO DUKHNA", "HEAD INJURIES") ~ "41",

    v630a %in% c("BATHA ROGA", "URIC ACID", "URIK ASID", "URIKASID") ~ "42",

    v630a == "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO" ~ "43",

    v630a == "RAGATMA KHARABI" ~ "20",

    v630a %in% c("KHOKI", "KHOKI LAGEKO", "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO") ~ "6",

    v630a %in% c("KHUTTA MA ALLERGY AAKO", "KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO", 
                 "PANI FOKA BATA PANI NILAKELL", "SARIR MA PANIKO PHOKA AYEKO") ~ "14",

    v630a == "LIGAMENT TEARING" ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630a %in% c("KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                 "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                 "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                 "MULTIPLE JOINT PAIN", "PAIN IN LEG") ~ "19",

    v630a == "LUMP IN BREAST" ~ "21",

    v630a %in% c("LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", "PET DUKHER", "PAYELSH",
                 "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                 "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", 
                 "PET KO CHECK", "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", 
                 "PETA DUKHEKO", "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE",
                 "BHOMIT BHAYERA NAROKIYEKO", "PETDUKHEKO") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a %in% c("PERGINENC", "PREGENCY", "PREGNANCY CHECK UP", 
                 "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", "PREGNANT", 
                 "SUTKERI", "SUTKERI BHAYEKO") ~ "25",

    v630a %in% c("MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", 
                 "NAK RA MUKHA BATA BLOOD AKO", "NAKKO SAMSHYA", "TANSIL", 
                 "TONSIL", "TONSILLITIS", "TONSILS") ~ "27",

    v630a %in% c("KAMJORI", "KAMJORI BHAYEKO", "KAMJORI BP LOW", "KAMJORI VAYEKO", "JIUDUKHE KO",
                 "JIU DUKHNE") ~ "30",

    v630a %in% c("MINS VAYAKO BELA PET DUKHEKO", "PATHAGHAR SAMANDI SAMASYA IS", 
                 "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", "PERIOD ANIYAMIT HUNE", 
                 "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", "SHIST SURGERY",
                 "EK DAMAI GARO VAYO MAHINA BARI NIHAMIT NAVAYERA", "PATHAGHAR SAMANDI",
                 "PATHAGHAR SAMANDI SAMASYA", "LOW ABDOMINAL PAIN WHITE VAGINAL DISCHARGE BURNING MICTURITION") ~ "31",

    v630a %in% c("MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI") ~ "36",

    v630a %in% c("LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                 "PATTHARIKO AUSADHI", "STONE") ~ "37",

    v630a == "TUBERCULOSIS" ~ "38",

    v630a == "MANASIK SAMASYA" ~ "40",

    v630a %in% c("MIGRAINE", "MIGREN HEADACHE", "NASA", "NASA DABE KO", 
                 "NASA SAMBANDHI", "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", 
                 "TAUKO DUKHANE", "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", 
                 "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", "THAUKO DUKHANE", "TUKO DUKHAYA") ~ "41",

    v630a %in% c("CHISO COLD", "CHISO RUGHA", "COLD ALLERGY", "KHOKI", "KHOKI LAGEKO", 
                "ROUGH KHOKI", "RUGA", "RUGHA", "RUGHA JORO", "खोकी", "रुघाखोकी") ~ "6",

    v630a %in% c("JONDISH LIVAR PHET MA PANI VKO", "JANDIS") ~ "9",

    v630a %in% c("URINE INFECTION", "URINE PROBLEM") ~ "10",

    v630a == "DATA KO SAMASYA" ~ "11",

    v630a %in% c("ALLERGIES", "ALLERGY", "CHALA SAMBANDHI ROGH", "GHAU KHATIRA", "PANI FOKA BATA PANI NIKALEKO",
                "JUI KHATIRA", "KHUTTA MA ALLERGY AAKO", "PANI FOKA BATA PANI NILAKELL", "KHATERA",
                "KHUTTA MA ELERGY BHAYEKO") ~ "14",

    v630a %in% c("BANCHARO BATA KHUTA KATIYO", "HAAT VACHIYEKO UPACHAR", 
                "HAND FRACTURE", "LIGAMENT TEARING", "TAUKO MA GHAU") ~ "15",

    v630a %in% c("KUKURLE TOKEKO", "SNACK BITE") ~ "18",

    v630a %in% c("BACK PAIN", "BACKBONE MA PROBLEM VAYERA", "BONE PAIN", "DHAD DUKHEKO", 
                "DHAD DUKHNE", "DHAD DUKHNE SAMASYA", "DHAD KO", "DHADKO DUKHAI", 
                "DHARD DUKHNA SAMYASYA", "GODA DUKHEKO", "GUDHA DUKHEKO", "DHADA DUKHE KO",
                "HAAD JORNI KO SAMASYA", "HAAT KHUTTA KO JORNI DUKHEKO", "HADIKHIYERA",
                "HAAT KO HADDI KO PROBLEM", "HADIKHIYER", "HADIKO SAMASA", 
                "HADJORNI", "HADJORNI DUKHNE HATKO", "HARDJORNI DUKHNE", "HAT DUKHERA", 
                "HAT KHUTTA DUKHEKO", "HAT KHUTTA DUKHNA", "HAT KHUTTA DUKHNE", 
                "HATH KO HADI DUKHEKO", "HATT KHUTTA DUKHNA", "JOINT PAIN", "JYU DHUKHEKO", 
                "JYU DUKHNE", "KAMAR DUKHNA", "KHUTA DUKHERA SUNEKO", "KHUTTA DUKHEKO", 
                "KHUTTA DUKHNE RA SWELLINGA", "KHUTTA HARU DUKHNEY BHAYERA SUNNEKO", 
                "KHUTTA SWELLING AND HADDI DUKHNE", "KNEE PAIN", "MINOR LEG PAIN", 
                "MULTIPLE JOINT PAIN", "PAIN IN LEG", "खुट्टा को कुर्कुच्चा दुख्ने", "KAMAR GHUDA DUKHEKO") ~ "19",

    v630a %in% c("ANAEMIA", "INCREASE IN CHOLESTEROL LEVEL", "RAGATMA KHARABI", "NASAKO SAMASYA",
                 "RAGATKO KHARABI") ~ "20",

    v630a %in% c("BONE MARROW TRANSPLANT", "CANCER KO LAXAN", "LUMP IN BREAST",
                 "SYMPTOMS OF CANCER KIDNEY INFECTION", "CANCER") ~ "21",

    v630a %in% c("ANDRA MA GAU", "APPENDICITIS OPERATION", "APPENDIX", "BHOMIT BHAYEKO", 
                "CONSTIPATION", "GASTIK", "GASTRIC", "GASTRIC KO PROBLEM", "GASTRITIS", 
                "GYASTRIK", "HAAT DUKHNE PAYALS", "LOW ABDOMINAL PAIN", "PAILS", "PET DUKHEKO", 
                "PET DUKHER", "PET DUKHERA", "PET DUKHERW", "PET DUKHERW GAKO", "PET DUKHNA", 
                "PET DUKHNA KAMBAR DUKHNA", "PET DUKHNE", "PET KAMAR DUKHNA", "PET KO CHECK", 
                "PET KO OPERATION", "PET SAMBANDHI", "PETA DUKHE KO", "PETA DUKHEKO", 
                "PETKO SAMASA", "PETKO SAMSYA", "PILES", "STOMACH ACHE", "ULCER", 
                "ULCER KO OPERATION", "VOMITING", "YAPENDIKS KO OPTION", "EPIGASTRIC PAIN",
                "INTESTINE OPERATION", "APPENDICITIS") ~ "22",

    v630a == "OTH TALU FATEKO" ~ "23",

    v630a == "DAMM" ~ "24",

    v630a %in% c("DELIVERY", "DELIVERY CHECKUP", "PERGINENC", "PREGENCY", 
                "PREGNANCY CHECK UP", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW", 
                "PREGNANT", "SUTKERI", "SUTKERI BHAYEKO", "PERGINENC TEST") ~ "25",

    v630a == "JANMA JATA APANGA" ~ "26",

    v630 %in% c("EAR PROBLEM", "ENT (DAT (TEETH)KO CHECK GARAUNA GAYEKO TARA SSF MA DA NAPARNE VAYERA NAK", 
                "GHATI KO SAMASYA", "GHATI MA GIRKHA", "GHATIMA SAMASYA", "JIBRO KO SAMASYA", 
                "MUKHMA GHAU", "NAK KO PINASH", "NAK KO SAMASYA", "NAK RA MUKHA BATA BLOOD AKO", 
                "NAKKO SAMSHYA", "TANSIL", "TONSIL", "TONSILLITIS", "TONSILS", "ट्वान्सिल", "NOSE BLEEDING") ~ "27",

    v630a %in% c("AKHAMA CHO", "EYE PROBLEM", "EYE CHECK GARNA GAYEKO") ~ "28",

    v630a == "DAAD" ~ "29",

    v630a %in% c("BODYSCHE", "JIU DUKHEKO", "NORMAL", "KAMJORI", "KAMJORI BHAYEKO", "KAMJORI VAYEKO") ~ "30",

    v630a %in% c("CHECK UP PREGNANT NA BHAYERA", "GAINO PATHEGHAR SAMASYA", "GYAENO PROBLEM", 
                "GYANO KO PROBLEM", "IRREGULAR MENSURATION", "MINS VAYAKO BELA PET DUKHEKO", 
                "PATHAGHAR SAMANDI SAMASYA IS", "PATHEGHAR KO SAMASYA", "PATHEGHAR KO SAMSYA", 
                "PERIOD ANIYAMIT HUNE", "PERIOD PAIN", "PERIOD PAIN BHAYEKO", "PERIOD PAN", 
                "SHIST SURGERY", "UTERUS INFECTION", "UTERUS PROBLAM", "PREGNANT NA BHAYERA CHECK UP") ~ "31",

    v630a %in% c("KAMJORI BP LOW", "MUTU HALLANE", "MUTUROG", "PRESSURE LOW VYERW") ~ "32",

    v630a %in% c("FOLLOWUP OF HERNIA OPERATIO", "HARNIYA", "HARNIYA KO OPERATION", "HERNIA") ~ "33",

    v630a == "SCROP TRIFECTA" ~ "35",

    v630a %in% c("KIDNEY STONES", "PISAB ROKIYAKO", "PISAB THAILIKO PATHARI", "PAYHARI") ~ "36",

    v630a %in% c("GAL BLADORS ROBLAM", "LIVER KO SAMAYA", "LIVER PROBLEM", "PATHARIKO", 
                "PATTHARIKO AUSADHI", "STONE", "PITA THAILIKO PATHARI", "JONDISH LIVAR PHET MA PANI",
                "UHA KO URIC ACID ATHAWA LIVER KO SAMASYA LEY HAST DUKHEKO VANERA DOCTOR LEY VANNU BHAYO") ~ "37",

    v630a %in% c("CHATI DUKHEKO", "CHATTI DUKHA SAMASYA", "CHEST INFECTION", 
                "FOKSO MA PANI DEKHIYEKO", "TUBERCULOSIS", "FOKSO KO PROBLEM") ~ "38",

    v630a %in% c("BHULNE SAMASYA", "HEAD ISSUES", "MANASIK SAMASYA") ~ "40",

    v630a %in% c("DIZZINESS", "HEADACE", "HEADACHE", "JHUTTA JHAMJHAMAUNE", "MIGRAINE", 
                "MIGREN HEADACHE", "NASA", "NASA DABE KO", "NASA SAMBANDHI", 
                "NEURO KO PROBLEM", "TAU KO DUKHNA BOMIT HUNA", "TAUKO DUKHANE", 
                "TAUKO DUKHEKO", "TAUKO DUKHEKO VYERW", "TAUKO DUKHNE", "TAUKO MA GHAU AAKO", 
                "THAUKO DUKHANE", "TUKO DUKHAYA", "KHUTTA JHAMJHAMAUNE") ~ "41",

    v630a %in% c("BATHA ROGA", "URIC ACID", "URIK ASID") ~ "42",

    v630a %in% c("KHUTTA MA KHIL AAYAR KTM GAYAR OPERATION GARE KO", "OPERATION KHUTTA KOMKHIL") ~ "43",

    v630a == "FOOD POISON" ~ "22",

    v630a %in% c("CHEST & STOMACH PROBLEM", "TIFID", "TYPHOID", "THYPHOID", "CHEST & STOMACH PAIN") ~ "2",

    v630a == "FOKSO KO PROBLEMP" ~ "38",

    v630a %in% c( "ANC CHECKUP IN PRIVATE HOSPITAL", "KEHI VAKO CHHAIN CHHAIN") ~ "6",

    v630a %in% c("BLOOD AND URINE INFECTION" , "YOUN ROD PANI BAGNE") ~ "10",

    v630a %in% c("BLOOD INFECTION") ~ "20",

    v630a == "EYE CHECK GARDA" ~ "28",

    v630a == "BRUSELA" ~ "17",

    v630a %in% c("BACK PAIN", "BACK PAIN KO SAMASYA BHAKO THIYO", "BACKPAIN", 
                 "DHAD DUKHE", "DHAD DUKHNE", "DHAD DUKHNE KHUTTA DUKHNE", 
                 "DHADA DUKHEKO", "DISCOGENIC LBD(LOWER BACK PAIN)", "GHUDA DUKHANE", 
                 "HADJORANI DUKHEKO", "HATH DUKHEKO", "HATH KHUTTA DUKHAI", 
                 "KAMAR GHUDA DUKHEKOLE", "KURKUCHA DUKHNE POLNE", 
                 "BODY ACHE", "BODY PAIN", "हात खुट्टा कम्मर दुखेको") ~ "19",

    v630a %in% c("ABDOMEN PAIN", "APPENDIX", "GALLSTONE", "GASTIC", "GASTIK", "STOMACH  INFECTION",
                 "GASTRIC INFECTION", "GASTRITIS", "HEART BURN", "DISHA GOTA PAREKO",
                 "INTESTINE OPERATION SUDDENLY AS THERE WAS GROWTH IN HIS INTESTINE", 
                 "KABJIYAT", "KAMMAR DUKHEKO", "KOKHA DUKHEKO", "PAYALSH", "PAYELS", 
                 "PET DUKHANE", "PET DUKHERA", "PET DUKHERA VOMIT BHAKO", "PET KO SAMASAYA", 
                 "PETKO OPERATION GAREKO", "PILES", "STOMACH", "STOMACH INFECTION", 
                 "STOMACH ACHE", "STOMACH PAIN", "एपेन्डिसाइड", "ABDOMINAL PAIN",
                 "THEY DON'T KNOW ABOUT THE ACTUAL DISEASE AS PER THE DOCTOR THEY ALSO DON'T KNOW THE ACTUAL DISEASE . GASTRIC") ~ "22",

    v630a %in% c("PREGNANCY CHECK UP", "PREGNANT", 
                 "UHA KO BREAST FEEDING GARNA KO LAGI AWASHEK MATRA MA DUDH NAPAKO HUNALEY BIGAT EK HAFTA DEKHI AAUSADHI SEWAN GARDAI HUNUNXA") ~ "25",

    v630a %in% c("GHATI KO SAMASYA", "NAAK MA MASU PALAKO", "PINASH", "TONSIL", "NOSE BLEEDING") ~ "27",

    v630a == "OVERALL" ~ "30",

    v630a %in% c("MAHINA BARI NIHAMIT NAVAYERA", "PATHAK GHAR SAMANDI SAMASYA", 
                 "PATHEGHAR KO OPERATION", "PATHEGHAR KO SAMASYA", "PATHEGHAR SAMBANDI SAMASYA THIYO") ~ "31",

    v630a == "HEART PROBLEM" ~ "32",

    v630a %in% c("HARNIYA KO OPERATION GAREKO", "HARNIYA KO OPERATION  GAREKO") ~ "33",

    v630a == "HIV AIDS" ~ "34",

    v630a %in% c("KIDANEY MA PATHARIYA", "KIDNEY INFECTION", "KIDNEY STONE", "KIDANEY  MA PATHARIYA",
                 "KIDNI JACHA RA UPACHAR", "PISABMA KHARABI", "STONE OPERATION") ~ "36",

    v630a %in% c("MILD LIVER DISEASE", "PATHARI", "PATHARI KO OPERATION", "PATTHARIYA", 
                 "PITA THAILIMA PATHARI KO", "PITKO THAILI MA PATHALI", "PITTATHAILI KO OPERATION") ~ "37",

    v630a == "PROSTATE" ~ "39",

    v630a %in% c("BEHOSH VAYEKO EKKASHI", "DHARD KO NASA CHAPIYA KO", "MIGRAINE", 
                 "PARALYSIS", "RINGADA CHALEKO", "RINGATA", "TAUKO DUKHAI", 
                 "TAUKO DUKHEKO", "YAUTA LEG NACHALEKO", "RINGADA CHALNE") ~ "41",

    v630a == "WORM" ~ "44",

    TRUE ~ as.character(v630)
  )
)

section6c4 <- section6c4 %>%
  mutate(
    v630 = case_when(
      personid == 777 ~ "8",
      personid == 3421 ~ "22",
      personid == 8150 ~ "19",
      personid == 8427 ~ "22",
      personid == 9616 ~ "41",
      personid == 16352 ~ "2",
      personid == 17818 ~ "1", 
      personid == 18176 ~ "15",
      personid == 19046 ~ "37",
      personid == 19684 ~ "22", 
      personid == 20888 ~ "6",
      personid == 24132 ~ "32",
      personid == 24286 ~ "10", 
      personid == 25246 ~ "41", 
      personid == 25322 ~ "25", 
      personid == 25356 ~ "24", 
      personid == 27238 ~ "28",
      personid == 27383 ~ "30",
      personid == 27484 ~ "10",
      personid == 27815 ~ "37",
      personid == 33458 ~ "9",
      personid == 38132 ~ "38",
      personid == 38711 ~ "32",
      personid == 47193 ~ "19",
      personid == 48072 ~ "22", 
      personid == 53953 ~ "19",
      personid == 55502 ~ "31",
      personid == 55737 ~ "22",
      personid == 59527 ~ "41",
      personid == 59529 ~ "22", 
      personid == 15320 ~ "36",
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

section6c1 <- section6c1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  )

section6c4 <- section6c4 %>%
  mutate(
    v630 = as.numeric(v630),
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  )

missing_acute <- anti_join(
  section6c4, 
  section6c1, 
  by = "disease_id"
)

section6c4 <- section6c4 %>%
  mutate(
    v658_num = suppressWarnings(as.numeric(str_extract(v658, "\\d+"))),

    v658_txt = str_trim(
      str_remove_all(v658, "\\d+|,")
    ),

    v658  = v658_num,
    v658a = if_else(v658_txt != "", v658_txt, NA_character_)
  ) %>%
  select(-v658_num, -v658_txt) %>%
  mutate(
    v652_num = suppressWarnings(as.numeric(str_extract(v652, "\\d+"))),

    v652_txt = str_trim(
      str_remove_all(v652, "\\d+|,")
    ),

    v652  = v652_num,
    v652a = if_else(v652_txt != "", v652_txt, NA_character_)
  ) %>%
  select(-v652_num, -v652_txt) 

for (i in setdiff(1:ncol(section6c4), c(2, 7, 8, 29, 34:36))) {
  section6c4[[i]] <- as.numeric(gsub("[^0-9]", "", section6c4[[i]]))
}

rm(missing_acute)

#UPDATING THE COST FOR ACUTE ILLNESS.

s0 <- read_dta("stata_data1/section0.dta")
s1a <- read_dta("stata_data1/section1a.dta")

acute_costs1 <- read.xlsx("/home/sobaakun/NHIPsurvey/health section arrangement/acute_costs- 22 Jan- reviewed bks.xlsx")

acute_costs1 <- merge(
  acute_costs1, 
  s0[, c("hhid", "ID")],
  by = "hhid"
)

s1a <- s1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

acute_costs1 <- merge(
  acute_costs1, 
  s1a[, c("uniq_id", "personid")],
  by = "uniq_id"
)

acute_costs2 <- read.xlsx("health section arrangement/acute_costs_remaining(dt).xlsx")

acute_costs <- bind_rows(
  acute_costs1,
  acute_costs2
) %>%
  arrange(disease_id) %>%
  group_by(disease_id) %>%
  slice_tail(n = 1) %>%  
  ungroup()

write.xlsx(acute_costs, "acute_costs.xlsx")

#UPDATING COST FOR CHRONIC INPATIENT.

chronic_inpatient1 <- read.xlsx("health section arrangement/Chronic_inpatient_costs_HB include age and sex- updated BKS Jan 17.xlsx")

chronic_inpatient1 <- merge(
  chronic_inpatient1, 
  s1a[, c("uniq_id", "personid")],
  by = "uniq_id"
)

chronic_inpatient2 <- read.xlsx("health section arrangement/chronic_inpatient_remaining (dt).xlsx")

chronic_inpatient2 <- chronic_inpatient2 %>%
  mutate(
    other_chronic_condition = as.character(other_chronic_condition)
  )

chronic_inpatient <- bind_rows(
  chronic_inpatient1, 
  chronic_inpatient2
) %>%
  arrange(disease_id) %>%
  group_by(disease_id) %>%
  slice_tail(n = 1) %>%
  ungroup()

write.xlsx(chronic_inpatient, "chronic_inpatient.xlsx")

#UPDATING COST FOR CHRONIC OUTPATIENT.

chronic_outpatient1 <- read.xlsx("health section arrangement/chronic_outpatient_costs - cost adjusted incl emergency bks 27 Jan.xlsx")

chronic_outpatient1 <- merge(
  chronic_outpatient1, 
  s1a[, c("uniq_id", "personid", "hhid")],
  by = "uniq_id"
)

chronic_outpatient2 <- read.xlsx("health section arrangement/chronic_outpatient_remaining (dt).xlsx")

chronic_outpatient <- bind_rows(
  chronic_outpatient1, 
  chronic_outpatient2
) %>%
  arrange(disease_id) %>%
  group_by(disease_id) %>%
  slice_tail(n = 1) %>%
  ungroup()

write.xlsx(chronic_outpatient, "chronic_outpatient.xlsx")

#TRANSLATING THE COST DATAFRAMES INTO THE MAIN DATAFRAMES

#TRANSLATION FOR SECTION6C4

acute_costs <- acute_costs %>%
  rename(
    v651a = emergency_costs, 
    v651b = opd_charges, 
    v651c = laboratory_costs, 
    v651d = imaging_costs, 
    v651e = medicine_costs, 
    v651f = medical_supplies_costs, 
    v651g = transportation_costs, 
    v651h = accomodation_costs, 
    v651i = care_giver_costs, 
    v651j = other_costs, 
    v651k = total_costs
  ) %>%
  select(personid, v651a:v651k)

section6c4 <- section6c4 %>%
  group_by(personid) %>%
  mutate(obs_id = row_number()) %>%
  ungroup()

acute_costs <- acute_costs %>%
  group_by(personid) %>%
  mutate(obs_id = row_number()) %>%
  ungroup()

section6c4 <- section6c4 %>%
  left_join(
    acute_costs %>% select(personid, obs_id, v651a:v651k),
    by = c("personid", "obs_id"),
    suffix = c("", "_tmp")
  ) %>%
  mutate(across(
    v651a:v651k,
    ~ coalesce(get(paste0(cur_column(), "_tmp")), .)
  )) %>%
  select(-ends_with("_tmp"), -obs_id)

#TRANSLATING FOR SECTION6B4

chronic_inpatient <- chronic_inpatient %>%
  rename(
    v618a = Emergency, 
    v618b = `Bed.Charges`, 
    v618c = Laboratory,
    v618d = Imaging, 
    v618e = Medicines, 
    v618f = `Medical.Supplies/.Devices`,
    v618g = `Trans.portation`, 
    v618h = `Food.&.Accommo.dation`,
    v618i = `Care.Giver.Cost`,
    v618j = Other.Costs,
    v618k = Total.cost
  ) %>%
  select(personid, v618a:v618k)

section6b4 <- section6b4 %>%
  group_by(personid) %>%
  mutate(obs_id = row_number()) %>%
  ungroup()

chronic_inpatient <- chronic_inpatient %>%
  group_by(personid) %>%
  mutate(obs_id = row_number()) %>%
  ungroup()

section6b4 <- section6b4 %>%
  left_join(
    chronic_inpatient %>% select(personid, obs_id, v618a:v618k),
    by = c("personid", "obs_id"),
    suffix = c("", "_tmp")
  ) %>%
  mutate(across(
    v618a:v618k,
    ~ coalesce(get(paste0(cur_column(), "_tmp")), .)
  )) %>%
  select(-ends_with("_tmp"), -obs_id)

#TRANSLATING FOR SECTION6B3

chronic_outpatient <- chronic_outpatient %>%
  rename(
    v614a = emergency_costs, 
    v614b = opd_charges, 
    v614c = laboratory_costs,
    v614d = imaging_costs, 
    v614e = medicine_costs, 
    v614f = medical_supplies_costs,
    v614g = transportation_costs, 
    v614h = accomodation_costs,
    v614i = care_giver_costs,
    v614j = other_costs,
    v614k = total_costs
  ) %>%
  select(personid, v614a:v614k)

section6b3 <- section6b3 %>%
  group_by(personid) %>%
  mutate(obs_id = row_number()) %>%
  ungroup()

chronic_outpatient <- chronic_outpatient %>%
  group_by(personid) %>%
  mutate(obs_id = row_number()) %>%
  ungroup()

section6b3 <- section6b3 %>%
  left_join(
    chronic_outpatient %>% select(personid, obs_id, v614a:v614k),
    by = c("personid", "obs_id"),
    suffix = c("", "_tmp")
  ) %>%
  mutate(across(
    v614a:v614k,
    ~ coalesce(get(paste0(cur_column(), "_tmp")), .)
  )) %>%
  select(-ends_with("_tmp"), -obs_id)

rm(
  acute_costs, acute_costs1, acute_costs2, chronic_inpatient, chronic_inpatient1, chronic_inpatient2,
  chronic_outpatient, chronic_outpatient1, chronic_outpatient2, s0, s1a
)

#SECOND TRANSLATION FOR SECTION 6.2.3 

chronic_outpatient <- read.xlsx("health section arrangement/CHRONIC-opd-COST-EDITED.xlsx")

chronic_outpatient <- chronic_outpatient %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  rename(
    v614a = emergency_costs, 
    v614b = opd_charges, 
    v614c = laboratory_costs, 
    v614d = imaging_costs, 
    v614e = medicine_costs, 
    v614f = medical_supplies_costs, 
    v614g = transportation_costs, 
    v614h = accomodation_costs, 
    v614i = care_giver_costs, 
    v614j = other_costs, 
    v614k = total_costs
  )

section6b3 <- section6b3 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup()

section6b3 <- section6b3 %>%
  rows_update(
    chronic_outpatient %>% select(disease_id, v614a:v614k), 
    by = "disease_id", 
    unmatched = "ignore"
  )

section6b3 <- section6b3 %>%
  mutate(
    across(v614a:v614k, ~ na_if(.x, 0))
  )

rm(chronic_outpatient)

#SECOND TRANSLATION FOR SECTION 6.2.4

chronic_inpatient <- read.xlsx("health section arrangement/chronic_inpatient_costs 4 Feb rev sent.xlsx")

chronic_inpatient <- chronic_inpatient %>%
  rename(
    v618a = emergency_costs, 
    v618b = bed_charges, 
    v618c = laboratory_costs, 
    v618d = imaging_costs, 
    v618e = medicine_costs, 
    v618f = medical_supplies_costs, 
    v618g = transportation_costs, 
    v618h = accomodation_costs, 
    v618i = care_giver_costs, 
    v618j = other_costs, 
    v618k = total_costs
  ) %>%
  select(disease_id, v618a:v618k)

section6b4 <- section6b4 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b4 <- section6b4 %>%
  rows_update(
    chronic_inpatient %>% select(disease_id, v618a:v618k),
    by = "disease_id"
  )

section6b4 <- section6b4 %>%
  mutate(
    across(v618a:v618k, ~ na_if(.x, 0))
  )

rm(chronic_inpatient)

#SECOND TRANSLATION FOR SECTION 6.3.4

acute_costs <- read.xlsx("health section arrangement/acute_costs 4 Feb.xlsx")

acute_costs <- acute_costs %>%
  rename(
    v651a = `Emergency.v651a`, 
    v651b = `OPD/IPD.v651b`, 
    v651c = Lab.v651c,
    v651d = Imagingv651d, 
    v651e = Med.v651e,
    v651f = Supply.v651f, 
    v651g = Transp.v651g, 
    v651h = Accomod.v651h, 
    v651i = Care.Giverv651i,
    v651j = Other.v651j,
    v651k = Total.Cost.v651k, 
  )

section6c4 <- section6c4 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  ) 

section6c4 <- section6c4 %>%
  rows_update(
    acute_costs %>% select(disease_id, v651a:v651k),
    by = "disease_id"
  )

section6c4 <- section6c4 %>%
  mutate(
    across(v651a:v651k, ~ na_if(.x, 0)),
    v630 = if_else(
    v630 == 96, 
    19, 
    v630
    )
  ) 


rm(acute_costs)

#SECTION7

section7 <- section7 %>%
  mutate(
    ward = as.numeric(gsub("[^0-9]", "", ward)),
    v702 = as.numeric(gsub("[^0-9]", "", v702)), 
    v703 = as.numeric(gsub("[^0-9]", "", v703)),
    v721 = as.numeric(gsub("[^0-9]", "", v721))
  )


section7 <- section7 %>%
  mutate(
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
    ),
    v710b = trimws(v710b),
    v710b = case_when(
      v710b %in% c("96, CONTRACT BASIC", "96, CONTRACT BASIS MA KAAM GARNE", 
                   "96, KARAR MA", "96, PAYMENT ON VISA APPROVAL", 
                   "CONTRACT, 96") ~ "1",
      
      v710b %in% c("96, AAFAI GHARMA TUTION PADHAUNE", 
                   "HOME TUITION PADHAUNE, 2") ~ "2",
      
      v710b %in% c(", 96, SWAROJAGAR PRIVATE LAWYER", "3, DAINIK J", 
                   "96, , AFNAI PASAL VAKO", "96, HAIR CUT AFNAI SALON", 
                   "96, KHELAUNA RA KIRING HARU BANAYARA ONLINE BUSINESS GARNE KAMDAR.", 
                   "SWAROJGAR, 3") ~ "3",
      
      v710b %in% c("4, AAFANAI KIRANA PASAL", "4, GHAR MA FURNISHING AGARNAY", 
                   "96, AGENT", "96, AUTO DRIVE", "96, BORA BOKNE KAM PALDAR", 
                   "96, DAILY", "96, DAILY WAGES", "96, DAINIK JYALADARI KAM", 
                   "96, GADI CHALAUNE", "96, JYALA DARI KAM", "96, JYALA MAJDURI", 
                   "96, JYALADARI", "96, JYALADARI KAM", "96, KAPADA SILAUNI CONTRACT", 
                   "96, MAJDUR", "96, MISTRI KO KAM", "96, PER DAY PAY", 
                   "96, PUROHIT KO KAM", "96, RECEIVES TIPS PER PASSENGER", 
                   "AGENT, 96", "BALUWA RAKHNE, 96", "BALUWA UTHAUNE, 96", 
                   "CHHANA CHHAKO ,RUKH KATEKO, 96", "COMMISSION ANUSAR PAISA DINCHHA, 96", 
                   "DAINIK JYALADARI", "GHATTA CHALAUDA JATI DIYO TETI LIGNE, 96", 
                   "JYALA MA KHETIPATI GARNE, 96", "JYALADARI KAM, 4", "MAJDURI, 96", 
                   "MISTRI, 96", "PUJAPATH GARAUNE, 96", "SELF, 96", 
                   "THEKKA KO KAM, 96", "THEKKA, 96", "ठेक्का, 96") ~ "4",
      
      v710b %in% c("5, GHARKO BEBASAYAMA  SAHAYOG", "96, ARU KO KHET MA", 
                   "AAFNAI KAAM, 96", "FCHV, 96", "SOCIAL SERVICE, 96", 
                   "SOCIAL SERVICES, 96", "VOLUNTEERS, 96") ~ "5",
      
      v710b == "96" ~ "4",
      
      TRUE ~ v710b
    ),
    v714a = trimws(v714a),
    v714a = case_when(
      v714a %in% c("96, BEBSAIK KERA KHETI GARNE", "96, POULTRY FARM", 
                   "AAFNAI KHETI GAREKO, 96", "POULTRY FIRM") ~ "1",

      v714a %in% c("GITTI BALUWA KUTANE BECHANE, 96") ~ "2",

      v714a %in% c(", 96, AACHAR BANAUNE", "96, DAIPAR COMPANY, ", "96, FURNITURE WORK", 
                   "96, GARMENT FACTORY MA KAM GARNE", "96, GLASS MAKER", 
                   "96, HANDICRAFT EXPORT", "96, STOREKEEPER", "FURNITURE KO KAMM, 96", 
                   "FURNITURE MA KAM GARNEY, 96", "FURNITURE MAKING", "96, DAIPAR COMPANY,",
                   "IRONS AAUJAR BUILDING RA REPAIR GARNE, 96", "KAPADA SILAUNA, 96", 
                   "KAPADA SILAUNE KAM, 96", "KAPADA SILAUNE, 96", "LUGA SILAUNE, 96", 
                   "PANT GARNEE, 6", "PLY MANUFACTURING, 96", "PLYWOOD RA SISAKO MA KAM GARNE, 96", 
                   "SILAI BUNAI, 96", "TAILORING KO KAM, 96", "TAILORING, 96", 
                   "96, KHUKURI, BANCHARO, KODALO, HASIYA, KODALI, BAMPHAK, CHIMTA, ODAN, CHULESI YI SABAI BANAUNE RA UDHYAUNE") ~ "3",

      v714a %in% c("96, TAR KO JALI BANAUNE KAM", "HOUSE WIRING, 96") ~ "4",

      v714a %in% c("OUTSOURCING OFFICE VAYEKO LE KHATAYEKO THAU MA GAYERA SARSAFAI KO KAAM GARNE, 96") ~ "5",

      v714a %in% c("6, GHAR BANAUNE KAM", "6, LABOUR", "96", "96, DAKARMI KO KAM", 
                   "96, GHAR BANAUNE", "96, SADAK NIRMAN", "96, TILER", 
                   "ALUMINUM KO DOORS FITTING GARNE, 96", "ARKAKO GHAR BANAUNE KAM, 96", 
                   "FURNISHING ACTIVITIES, 96", "FURNISHING KO KAMM, 1", "GHAR BANAUNE KAM, 96", 
                   "GHAR, WALL NIRMAN KO KAM, 96", "GRIL WARDING GARNE KAM, 96", 
                   "KATH KO KAM THEKKA LIYE RA GARNE, 96", "LEBAR, 4", "LIBER KO KAM, 96", 
                   "MISTRI KO KAMM, 96", "NIRMAN, 96", "ROAD SIDE CONSTRUCTION MA HELPER KO KAM, 96", 
                   "THEKDAR, 6", "THEKKA KO KAM, 96", "96, SUN CHANDI KO GAR GAHANA BANAUNE KAAM",
                   "96, SIKARME FURNITURE LAGAYAT KATH KO KAM GARNE") ~ "6",

      v714a %in% c("96, ARU KO PASAL MA KAM GARNEY", "96, AUSADI PASAL (MEDICAL)", 
                   "96, BATO MA SAMAN BIKRI GARNAY", "96, COSMETICS PASAL MAA SAMAN BECHNE", 
                   "96, GROCERY SHOP", "96, IMPORT AND EXPORT", "96, KIRANA PASAL", 
                   "96, MASU PASAL", "96, SHOE SHOP", "AAFNU MASU PASAL, 96", 
                   "AAFNU WATCH PASAL MA KAM GAREKO, 96", "BIJULI PASAL, 96", 
                   "BISHABAZAR COMPANY PVT LTD, 96", "BOOK COPY PENCIL, 96", 
                   "FRUITS SELLING IN DOKO, 96", "KIRANA PASAL, 96", "KIRANA PSAL, 96", 
                   "PHARMACY, 96", "PHARMECY, 96", "RUDRAKSHYA, 96", "THELA CHALAUNI, 96", 
                   "WATCH PASAL MA HELP GAREKO, 96",
                   "96, INDIA BATA KHADHYANA KO SAMAN HARU LYAIDINE BYAPARI HARULAI") ~ "7",

      v714a %in% c("96, CARGO KO SAMAN PATHAUNE", "96, SCHOOL BUS DRIVER", 
                   "AUTO CHALAUNE KAM, 96", "BASEKO KHALASI GARNE, 96", 
                   "DRIVER, 8", "DRIVING, 96", "DUNGACHALAUNE, 96", 
                   "GADI CHALAUNE KAM, 96", "GADI KO TICKET KATNE, 8", "TIYAKTAR, 8") ~ "8",

      v714a %in% c("BANK MA KHANA KHAJA BANAUNE KARYALAYE SAHAYOGI, 96", "CATRING, 96", 
                   "CHIYA KHAJA PASAL, 96", "TOURIST GUIDE KO KAM, 96") ~ "9",

      v714a %in% c("ONLINE, 96") ~ "10",
      v714a %in% c("96, COMPUTER APRETOR", "NET JADAN GARNE, 96", 
                   "SAUDI MA TECHNICAL SUPPORT KO KAM, 96") ~ "11",

      v714a %in% c("96, SAHAKARI", "BACHAT GARNEY, 96", "BACHAT UTHAUNE, 96", 
                   "BIMA DARTA SAHAYOGI, 96", "INSURANCE COMPANY, 96",
                   "BHARAT BATA PATHAYAKO PAISA IME MARPHAT EXCHANGE GARNE KAM") ~ "12",

      v714a %in% c("96, GHAR GAGGA KO KAM") ~ "13",

      v714a %in% c(", 96, PANDIT - PUJA GARNU HUNEY", "96, , JAGGA KO LEKHA RA NIBEDAN LEKHNE", 
                   "96, CONSULTANCY", "96, LEKHAPDI GARNE", "96, LOGESTIC MANEGER", 
                   "96, PUJARI", "96, PUROHIT KO KAM", "RESEARCH, 96") ~ "14",

      v714a %in% c(", 96, SECURITY KO KAM", "96, HELPER KO KAM", "96, KARYALAYA SAHAYOGI", 
                   "96, MANPOWER AGENT", "96, OUT SOURCING COMPANY", 
                   "96, SECURITY GUARD KO LAGI", "96, SECURITY GURD KO KAM", 
                   "96, TRAVEL AGENCY", "ACTS AS A MIDDLEMAN FOR SENDING PEOPLE FOR FOREIGN EMPLOYMENT., 96", 
                   "HO, 15", "MANPOWER MA BIDESH PATHAUNE MANXE, 96", "OUT SOURCING COMPANY, 96", 
                   "OUT SOURCING, 96", "OUTSOURCING COMPANI, 96", 
                   "OUTSOURCING OFFICE VAYEKO LE KHATAYAKO THAU MA GAYERA SARSAFAI KO KAAM GARNE, 96", 
                   "OUTSOURCING OFFICE VAYEKO LE STAFF HIRED GARI RELATED OFFICE MA MAIN POWER SUPPLY GARNI, 96", 
                   "STAFF OUTSOURCING COMPANY, 96") ~ "15",

      v714a %in% c("96, NEPAL POLICE", "GUARD, 96", "KARYALAY SAHAYOGI, 4", 
                   "KARYALAYA SAHAYOGI PESHA, 96", "KARYALAYA SAHAYOGI, 96", 
                   "KARYALAYE SAHAYOGI, 96", "SARKARIKARYALAYA  MA FUL TATHA DUWO LAGAUNE, 96", 
                   "SECURITY GARD KO KAM, 96", "SECURITY, 3", "SECURITY, 96") ~ "16",

      v714a %in% c("96, ELEMENTARY OCCUPATION", "SCHOOL AAYA KO KAM, 96", "SCHOOL MA SAHAYOGI KO KAM, 96") ~ "17",

      v714a %in% c("96, CHILD CARE GARNE", "96, FCHV", "96, GUMBHA", 
                   "96, HOSPITAL SARSAFAI GARNE", "96, KSHETRAPATI HOSPITAL", 
                   "CARE GIVER, 96", "HOSPITAL, 14", "ORGANIZATION, 18", 
                   "SARASAFAI GARNE WARD MA SAHAYOGI KAM GARNE, 96") ~ "18",

      v714a %in% c("96, CHAUTARA", "SWIMMING POOL, 96") ~ "19",
      
      v714a %in% c("9, TRADE UNION", "96, , BIGREKO PHONE MARMAT GARNE", 
                   "96, BEAUTY PARLOUR (20)", "96, GARMENT KO KAPADA SILAUNI", 
                   "96, GHAR GHAR MA BIGRIYE KO MECHINARY SAMAN BANAUNE KAK", 
                   "96, KAPABA SILAUNE TAILORING", "96, KAPADA SILAUNE", 
                   "96, KAPADA SILAUNE KAM", "96, KAPADA SILSUNE", "96, KAPAL KATANE", 
                   "96, MACHINE REPAIR KO KAM", "96, SAFARI SERVICE", "96, TAILORING", 
                   "96, TRADE UNION", "96, TRADE UNION KO OFFICEMAX KAAM GARNE", 
                   "BEAUTICIAN, 96", "CAR,BIKE WASHING, 96", "COBLER, 96", 
                   "GAUMA JANACHETANA SAMBANDHI TALIM DINE, 96", "HA, 96", 
                   "HAIR CUT SALON, 96", "HOUSE KEEPING, 96", "JAJAMAN KO KAM GARNE, 96", 
                   "LABOR SAMBANDHI KAM GARNE, 96", "PARLOUR N COSMETICS, 96", 
                   "PUJAPATH GARAUNE, 96", "PUJARI, 96", "PUROHIT, 96", 
                   "SERVICES, 96", "SARSAPHAI GARNE OFFICE KHOLNE BANDA GARNE KAM, 96", 
                   "SWEEPER, 96", "TRADE UNION, 96", "TRADE UNIONS, 96") ~ "20",

      v714a %in% c("ARUBKO GHAR KO PERSONAL GADI DRIVING GARIDINE, 96", 
                   "BIDESHI KO GHAR MA KHANA BANAUNE U GHAR SARSAFAI GARNE KAM, 96", 
                   "GHAR KO KAAM, 96", "GHAR MA KHANA BANAUNE KAAM, 96", 
                   "GHARAYASI KAM, 96", "GHARELU KAM, 96", "WORK ON OTHERS HOUSE, 96", 
                   "WORKING ON OTHERS HOUSE, 96") ~ "21",
      
      v714a %in% c("HELPER, 22", "ORGANIZATION KO, 22") ~ "22",

      TRUE ~ v714a
    ), 
    v716 = trimws(v716),
    v716 = case_when(
      v716 %in% c(
        "AGENT", "BHAIRAHAWA AIRPORT", "BIDHUT PRADHIKARAN", "COKE KO DEALER", 
        "COOPERATIVE", "COUNSULTENCY", "DAIPAR COMPANY", "DUDH BECHNE", 
        "ELECTRIC MARMAT SEWA", "ELECTRIC OFFICE", "ENTERTAINMENT( KATHMANDU FUN PARK)", 
        "EXPOT IMPORT", "FCHV", "FIELD MA GAYERA PAISA UTHAUNE KAMHARU", 
        "FURNITURE, 2", "GHAR BAUNE DAKARMI KO KAM", "GOVERNMENT HOSPITAL KARAR MA", 
        "GOVERNMENT SCHOOL", "HARDWARE PASAL", "HARDWARE PASAL MA SAMAN BECHNE", 
        "HEALTH INSURANCE BOARD", "HEALTH POST", "LEKHAPADI", "MAHILA BIKAS", 
        "MILL", "MULTIPLE ORGANIZATION", "PAISA JAMMA GARNE LOAN SAMBANDHI KAROBAR GARNE", 
        "PLASTI UTAPADAN GARNE HO", "REMITANCE", "RETAIL SHOP", "RETAILER SHOP", 
        "SAAAFANO", "SAGTHITH SECTOR, 1", "SAHAKARI", "SAVING AND CREDIT CO-OPERATIVE", 
        "SCHOOL", "SCOOL", "SEWA", "SWASTHYA BINA KARYAKRAM", "TEACHING", 
        "TRADING", "YAMAHA COMPANY DISTRIBUTOR"
      ) ~ "1",

      v716 %in% c(
        "AAFNAI", "AAFNAI AUTO", "AAFNAI GHAR KO", "AFNAI", 
        "AFNAI GHAR KO KRISI RA PASUPALAN KO KAM", 
        "AFNAI GHAR KO KRISI TATHA PASUPALAN KO KAM GARNU VAYEKO.", 
        "AFNO", "AFNO KHET", "AGRICULTURE", "BYAKTIGAT", "GHAR KAI KRISI", 
        "GHAR MAI KAPDA SILAUNE", "GHARSYASI", "GHARYASII", "GOAT FARMING", 
        "KAPDA SILAUNE", "KHADHANNA KO THOK BECHNE", "KHAJA PASAL", 
        "KHETIPATI KO KAM", "KHETIPATI RA PASUPALAN", "KIRANA PASAL", 
        "KIRANA SHOP", "KIRANA STORE", "KIRSHI", "KRISI", "KRISI KO KAM", 
        "NIGI", "NIGI BEBASAYA", "NIGI KRISHI", "NIGI KRISHI BEBASAYA", 
        "NIJI", "NIJI APARTMENT", "NIJI BIDHYALAYA", "NIJI KIRANA STORE", 
        "NIJI KRISHI", "NIJI KRISHI CHEETRA", "OK", "OOOOO", "PARLOUR", 
        "PARLOUR SAMBHANDI", "PASAL KO KAM AAFANO", "PRIVATE DRIVING", 
        "PRIVATE SCHOOL", "PRIVATE WON BUSINESS", "SELF", "SINGLE", 
        "आफ्नै खेतीपाती", "आफ्नै खेतीपाती र पशुपालन", 
        "कृषिमा ज्याला गरेको कोदो गोड्ने", "खेतीपातीमा काम गरेको"
      ) ~ "2",

      TRUE ~ v716
  )
)

for (i in setdiff(1:ncol(section7), c(2, 7, 8, 19, 25, 34))) {
  section7[[i]] <- as.numeric(gsub("[^0-9]", "", section7[[i]]))
}

#SECTION8 

for (i in setdiff(1:ncol(section8), c(2, 7, 8, 13, 16))) { 
  section8[[i]] <- as.numeric(gsub("[^0-9]", "", section8[[i]]))
}

section8 <- section8 %>%
  mutate(
    v802 = case_when(
      v802 == 2 & v803 != "" ~ 1,
      v803 == "" & is.na(v803c) ~ 2,
      personid %in% c(14210, 51558) ~ 2,
      TRUE ~ v802
    ),
    v803c = case_when(
      v803 %in% c(
        "HOTEL ADMIN HR", "1"
      ) ~ 1, 
      v803 %in% c(
        "BOARDING SCHOOL", "IT SAMBANDHI", "TEACHER", "BACHALAI PADAUNE", "0, TEACHER"
      ) ~ 2,
      v803 %in% c(
        "IT SUPPORT", "LEKHANDASI", "SCHOOL MA PADHAUNE", "LEKHA ADHIKRITH", "PRASASANIK SEWA MA SAHAYOG, PRASASANIK SEWA"
      ) ~ 3,
      v803 %in% c(
        "4", "BANK MA TELLER", "HALKARA COUNTER MA BASNE", "TOP QUALITY POULTRY FEED MA ACCOUNTING KO KAM GARNU VAYO",
        "GAGA AGENT", "WARD OFFICE MA", "NGO MA KAAM GARNE", "SARKARI OFFICE MA KAAM GARNEY", "RECEPTIONIST",
        "ADMIN", "COOPERATIVE EMPLOYEE", "FF, PROCUREMENT OFFICER", "MARKETING IN FINANCE, ALUMINUM RELATED WORK JHYAL, DHOKA BANAUNE KAAM"
      ) ~ 4,
      v803 %in% c(
        "MANPOWER AGENT", "PUROHIT", "PASAL", "BEAUTICIAN", "MRKETING", "COLLECTION KO KAM", "GG", 
        "HOUSE KEEPING", "HOTEL MA SAFE", "AAFNAI JOB LINK"
      ) ~ 5,
      v803 %in% c(
        "THEKKPATTA", "DHUP BANAUNE", "PARLOUR MA KAM GARNE", "BIDI BANAUNE", "BIDI BANAUNE KAM",
        "NIRMAN SAMBANDHI", "GHAR BANAUNE MISTREE", "ALUMINUM KO KAM", "KHAPADA SILAUNE", "ELECTRICIAN",
        "WELDING AND MAINTENANCE. MILL MECHANICAL", "AUTO MECHANIC", "WIRING KO KAMM", "GHAR KO GARO LAGAUNE",
        "DHUP BATTI BANAUNE", "AC MECHANIC", "MISTRI"
      ) ~ 7,
      v803 %in% c(
        "THREAD MAKING, MACHINE OPERATOR", "AAFNAI AUTO CHALAUNE", "JCV", "BUS DRIVER", "PENTAR KO KAM",
        "AMBULANCE DRIVER", "TRIPPER DRIVER", "DRIVER"
      ) ~ 8,
      v803 %in% c(
        "JYALA MA KHETIPATI SAMBANDHI KAAM GARNE", "ARU KO KHET MA DHAN ROPNE", "RGH", "LABOUR,MISTRI",
        "DHAN ROPNE KAM,", "JYALADRI", "HOUSE KEEPING", "ETA BHATAMA LABOUR KAM", "DHAN ROPNE", "DHAN GODNE,ROPNE",
        "DHAN ROPNE, GODNE", "KHETI KISANU", "JYAMI GHAR BANAUNE", "GARI RAHEKO", "KEHTI PATI", "GARIRHEKO",
        "GARI RAHEKO", "GARIRHEKO", "JYALA MAJHDOORI", "DHAN GODNE, ROPNE", "LABOR", "JHYAL DHOKA BANAUNE SAHAYOG",
        "PANTIN", "PANCHKANYA PROFILE MA ALUMINUM SAHAYOGI KAM", "DAURA BOKNE/KATNE KAM, KRISHI KAMMA DAILY JYALADARI KAM GARNE.",
        "BHARI BOKNE KAM HARU, JYALADARI KAM", "GARO LAUNE DHUNGA MATO BOKNE SABAI KAAM, JYALA MA KHETIPATI SAMBANDHI SABAI KAAM",
        "BHAWAN NIRMAAN SAMBANDHI KAAM HARU DHUNGA MATO KO, KHANNE, GODMEL AADI BAARIKO SABAI KAAM GARNE",
        "GHARMA COLOUR LAGAUNEE KAM GARNEE, KHETIPATI MA JYALA KO KAM", "KHETIPATI MA JYALA, BATO BANAUNE KAM",
        "SADAKKO KULO SAFA GARNE KAM, DAURA KATNE, BARI KHANNE,JASTO KRISHI KAMMA", "FURNITUREKO SAMANHARU  BANAUNE, BHAWAN /BATO AADI BANAUNE",
        "MADHAV POUDEL JI WAS IN FOREIGN EMPLOYMENT AND HAS RETURNED TO NEPAL TWO MONTHS AGO.AND EVEN NOW HE IS PLANNING WHICH COUNTRY TO GO IN ORDER TO CONTINUE HIS FOREIGN  EMPLOYMENT AND HE ALSO INFORMED THAT HE USED TO SEND AN AVERAGE OF 25 THOUSAND RUPEES PER MONTH WHILE HE WAS IN FOREIGN EMPLOYMENT."
      ) ~ 9, 
      TRUE ~ v803c        
    )
  ) 

section8 <- section8 %>%
  group_by(v803c) %>%
  mutate(
    v804 = case_when(
      is.na(v804) & v808 > 0 ~ 2,
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


section8 <- section8 %>%
  mutate(
    v802 = case_when(
      is.na(v805) & !is.na(v808a) & v808a > 0 ~ 2,
      TRUE ~ v802
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
    )
  )

section8 <- section8 %>%
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
    )
  ) %>%
  ungroup()

section8 <- section8 %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v806, v807, v808a, v808b, v808c, v808d, v808e, v809, v810a, v810b),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- round(mean(.x, na.rm = TRUE))

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()  

#SECTION9A1

section9a <- section9a %>%
  filter(
    !(v902b == "" & v901 == "" & v902a == "" & v905 == "")
  ) %>%
  select(-v907a) %>%
  rename(
    v907a = X24
  ) %>%
  mutate(
    v901 = case_when(
      v901 == "" ~ "1",
      v901 == "2" ~ "1", 
      TRUE ~ v901
    ),
    v907a = trimws(v907a), 
    v907a = case_when(
     v907a %in% c("0 NESULKA GAREKO", "2 KATHA DIYEKO", 
                 "AFNAI DAJU BHAI LEY GARI RAKHNH BHAKO HUNUNXA TESTO PAISA LEKO XAINA", 
                 "AFNAI MAITI KO GAREKO SO NO ANY PAYMENT", 
                 "AILE SAMMA PAKO XAINA TARA ABA BATA PAUNE.", 
                 "BHARKHER KHETI LAGAYAKO DEYAKO CHHAINA", 
                 "CHHORA HARU LAI GARI KHANU DINU BHAKO", 
                 "DIYENA", "JETHAJU LEY GARI KHA VANERA DEKO PAISA TIRNU PARDAINA", 
                 "KEI LINE DINE NAGAREKO", "LINE DINE NAGAREKO", 
                 "NA", "NISULKA GARI KHANA DIYAKO.", "NO", 
                 "SKIP HUNU PARNE", "0", "96", 
                 "AAFAI KHETI GAREKO", "AAFAI VAIKO LIYEKO LE KEI DEKO XAINA", 
                 "AAFNO AAFANTA KO VAYERA KEHI DIYEKO XAINA.", "ADHIYA NADIYEKO", 
                 "AFNAI CHHORA LE KAMAI GARNE GAREKO", 
                 "AFNO VAI LAI KHATI GARNE DEYAKO R TESBAPAT POISA AANA KEHE N LENE OPTION NOT APPLICABLE BHAYAKO LE KUT THAKKA WA BHADA MA DEYAKO MA TIK LAGAYAKO", 
                 "AGRICULTURE PRODUCTION WAS DONE ON OFFICE LAND AND SHE DIDN'T PAY ANY AMOUNT & THINGS.SHE ALSO DON'T KNOW HOW MUCH IT COST WHILE SELLING LAND", 
                 "ARUKO JAGGA MA GAREKO HO GHAR SIDE KO PAISA ANI KEI DINU PARDAINA", 
                 "BINA PAISA YETIKAI GARI KHAU VANERA DIYEKO BAARI BAJHAI NAHOS VANERA", 
                 "FREE", "GHR KO BUWA LE DINE", "K HI PANI DIDAINAN YETIKAI DIYEKO", 
                 "KAINADINE BANDHAKI LEKO KHETHO", "KEHI DUNU PARDAINA", 
                 "KEHI LINE DINE NAGAREKO.", "KEI LINE DINE NAGAREKO TETTIKAI KAMAYERA KHANALAI DIYEKO", 
                 "KEI PANI LINE GAREKO XAINA GHAR MA SASU SASURA LE GARNE GAREKO", 
                 "LINE DINE NAGAREKO TETTIKAI KAMAYERA KHANALAI DIYEKO", 
                 "LINE DINE NAGAREKO TETTIKAI KAMAYERA KHANALAI LIYEKO", 
                 "LINE DINE NAGAREKO TETTIKAI KAMAYERA KHANE", 
                 "OO", "SELF USE", "SITTAI MA PRAYOG GAREKO", 
                 "TETTIKAI KAMAYERA KHANALAI DIYEKO", "TETTIKAI KAMAYERA KHANE", 
                 "YO SAL BHARKHAR LAGAKO", "केही दिनु नपर्ने", 
                 "निःशुल्क दिएको आफ्नै भाइले गरेर खाने", 
                 "UHA KO NAM MA JAGGA XORA XUTIYEKO HUNA LEY KHET BARI UTA GARNU HUNXA RA TES BAPAT KEI KHANEY ANNA DAL HARU DINEY"
                 ) ~ "0",
    v907a == "100" ~ "100",
    v907a == "150" ~ "150",
    v907a == "1 MURI TORI" ~ "300",
    v907a == "500" ~ "500",
    v907a == "KHADHAYAN BALI 15 KG" ~ "600",
    v907a == "KHADHYAN BALI 20 KG" ~ "800",
    v907a %in% c("1", "1000") ~ "1000",
    v907a %in% c("30KG", "1200") ~ "1200",
    v907a == "8 PAATHI DHAAN" ~ "1280",
    v907a == "1300" ~ "1300",
    v907a == "1400" ~ "1400",
    v907a %in% c("MILLET (RS1500)", "1500", "MAKAI 18KG") ~ "1500",
    v907a == "1600" ~ "1600",
    v907a == "1800" ~ "1800",
    v907a %in% c("GADAUDI 2000", "SAG PAT TARKARI UBJAU MATRA DINE 2000", "2000") ~ "2000",
    v907a %in% c("60 KG", "2400") ~ "2400",
    v907a == "2500" ~ "2500",
    v907a %in% c("MILLET (10 PATHI)3000", "30", "3000") ~ "3000",
    v907a == "3500" ~ "3500",
    v907a %in% c("1 MAN MAKAI 20 KG BHATMAS", "3600") ~ "3600",
    v907a == "CHAMAL 50 KG DAL 15 KG" ~ "3800",
    v907a == "3900 CASH RECEIVED" ~ "3900",
    v907a %in% c("1 QUENTAL DHAN", "100 KILO DHAN GAU", "1 KUNTAL GAHU", 
                 "1 QUENTAL GAHU", "40", "4000", "SAAG") ~ "4000",
    v907a == "4500" ~ "4500",
    v907a == "4800" ~ "4800",
    v907a %in% c("5000 MOHIKHETKO LAGI DIYAKO", "GAU", "5", "50", "5000") ~ "5000",
    v907a == "5400" ~ "5400",
    v907a == "5500" ~ "5500",
    v907a %in% c("20KG AALU", "DHAN 1 KUNTAL GHEHU 50KG", "GAHU150KG", 
                 "DHAN 150KG", "6000") ~ "6000",
    v907a == "6150" ~ "6150",
    v907a == "6350" ~ "6350",
    v907a %in% c("2 MURI", "4 MAN DHAN DINE GARXAN") ~ "6400",
    v907a == "6500" ~ "6500",
    v907a %in% c("RS.7000", "7000") ~ "7000",
    v907a == "7500" ~ "7500",
    v907a %in% c("200 KG KHADHAN", "3 MURI DHAN", "DHAN 200KG", 
                 "DHANN 8000", "KHADHAN BALI 200KG", "8000", 
                 'HE SAID THAT " I DON\'T GO THERE AND I DON\'T KNOW HOW MUCH IT YIELDS; WHATEVER THEY GIVE THAT\'S IT AND THE THINGS I GET WAS 2.5 QUINTAL DHAN',
                 "UHA KO NAM MA JAGGA  XORA XUTIYEKO HUNA LEY KHET BARI UTA GARNU HUNXA RA TES BAPAT KEI KHANEY ANNA DAL HARU DINEY",
                 "2 KUNTAL DHAN PAKO THIYE", "2 QUINTLE", "DHAN5 MURI") ~ "8000",
    v907a %in% c("DHAM 3 MURI", "2 QUENTAL DHAN", "2 QUENTEL DHAN", 
                 "2 QUENTAL 25 KG DHAN MATRA DIYAKO KHARCHA K HI DINA NAPARNE", 
                 "9000") ~ "9000",
    v907a %in% c("MAKAI 4MURI", "DHAN 4 MAN GEHU 2MAN", "DHAN 4  MAN GEHU 2MAN") ~ "9600",
    v907a %in% c("OVERALL 1.5 QUINTAL VEGETABLE", "MAKAI", "10000") ~ "10000",
    v907a == "DHAN 2 MASURI 20 KG" ~ "10400",
    v907a == "3 QUENTAL DHAN" ~ "10884",
    v907a == "11000" ~ "11000",
    v907a == "11700" ~ "11700",
    v907a %in% c("12000(DHAN)", "12000DHAN", "8 MAN DHAN", "12000", "3 QUINTAL DHAN") ~ "12000",
    v907a == "12500" ~ "12500",
    v907a == "12600" ~ "12600",
    v907a %in% c("DHAN4 MURI", "12800") ~ "12800",
    v907a == "13000" ~ "13000",
    v907a == "13333" ~ "13333",
    v907a == "13500" ~ "13500",
    v907a %in% c("14000 KO DHAN", "14000") ~ "14000",
    v907a == "14400" ~ "14400",
    v907a %in% c("15000 KO DHAN", "15000(KODO)(MILLET)", "DHAN,GAHU", 
                 "RS.15000 PAID FOR LAND LEASE", "15000", "DHAN 40 GEHU 20  MAN  MASULI 2MAN") ~ "15000",
    v907a == "1 QUINTAL GAHU 10KG TORI 2 QUINTAL DHAN" ~ "15500",
    v907a == "15600" ~ "15600",
    v907a %in% c("4 QUINTLE", "DHAN 4QUENTAL", "DHAN 5 MURI", "5 MURI DHAN", "10 MAN", "16000") ~ "16000",
    v907a == "17000" ~ "17000",
    v907a == "17500" ~ "17500",
    v907a == "18200" ~ "18200",
    v907a == "18600" ~ "18600",
    v907a %in% c("6 MURI", "6 MURI DHAN", "19000") ~ "19000",
    v907a == "19200" ~ "19200",
    v907a %in% c("18 MAN DHAN", "20000(DHAN)", "5 QUENTAL DHAN", "5 QUENTEL", 
                 "500KG KHADHYAN", "6 QUENTEL", "DHAN", "DHAN 5 QUENTAL", 
                 "DHAN 5 QUENTEL", "DHAN 500KG", "DHAN 5QU", 
                 "5 KUNTAL DHAN", "5 QUINTAL DHAN", "20000", "2") ~ "20000",
    v907a == "DHAN 10MAN GAHU3MAN" ~ "20800",
    v907a %in% c("21000 DHAN", "21000") ~ "21000",
    v907a == "21600" ~ "21600",
    v907a %in% c("22000 TIRAYKO", "7 MURI DHAN PAYAKO", "7 MURI DHAN", "7  MURI DHAN PAYAKO",
                 "DHAN 6 QUENTAL", "DHAN 6 QUENTEL", "22000 DAM KO ANNA BALI", 
                 "22000") ~ "22000",
    v907a == "22150" ~ "22150",
    v907a == "7MURI" ~ "22400",
    v907a == "22500" ~ "22500",
    v907a %in% c("20 MAN DHAN DINU PAR XA", "23000", "20  MAN DHAN", "20  MAN DHAN DIYEKO") ~ "23000",
    v907a == "23100" ~ "23100",
    v907a %in% c("15 MAN", "6 KUNTAL DHAN", "7.5 MURI", "7.5MURI DHAN", 
                 "15 MAN DIYAKO", "6 QUINTLE", "DHAN 10 MN GEHU 5 MN", "24000") ~ "24000",
    v907a == "24500" ~ "24500",
    v907a %in% c("20 MAN DHAN", "20 MAN DHAN DIYEKO", "25000(DHAN)", 
                 "RICE", "25000") ~ "25000",
    v907a %in% c("8 MURI", "8 MURI DHAN", "25600") ~ "25600",
    v907a == "26000" ~ "26000",
    v907a == "26250" ~ "26250",
    v907a == "27000" ~ "27000",
    v907a == "DHAN 10 MAN GEHU 5 MAN DAL 30 KG" ~ "27600",
    v907a == "15 MURI DHAN 27750" ~ "27750",
    v907a %in% c("12 MURI", "7 QUINTEL GAHU PAYEKO", "28000") ~ "28000",
    v907a %in% c("9 MURI", "28800") ~ "28800",
    v907a == "29500" ~ "29500",
    v907a %in% c("30000 DHAN", "AALU", "AALU ", "DHAN ", "DHAN 8", "30000", 
                 "30000 YO GOVERNMENT KO JAGGA HO TEI NI ARULAU THEKKA MAA DINU BHAKO CHA") ~ "30000",
    v907a == "31500" ~ "31500",
    v907a == "31800" ~ "31800",
    v907a %in% c("10 MURI", "20MAN DHAN", "8 KUNTAL", "8QU", "DHAN 20", "32000") ~ "32000",
    v907a == "33000" ~ "33000",
    v907a == "34000" ~ "34000",
    v907a == "34400" ~ "34400",
    v907a == "34900" ~ "34900",
    v907a %in% c("10 QUINTAL DHAN", "35000 (DHAN)", "35000") ~ "35000",
    v907a %in% c("9 QUINTLE", "9 QUINTLE", "9  QUINTLE", "36000") ~ "36000",
    v907a == "36450" ~ "36450",
    v907a == "37000" ~ "37000",
    v907a %in% c("12MURI DHAN", "38000") ~ "38000",
    v907a %in% c("DHAN 12 MURI", "38400") ~ "38400",
    v907a == "38500" ~ "38500",
    v907a %in% c("39000 DHAN KO", "39000") ~ "39000",
    v907a == "39200" ~ "39200",
    v907a %in% c("100000 DHAN", "DHAN 10 KUNTAL GHEHU 4 KUNTAL", "DHAN 10 QUINTAL", 
                 "DANN 10QUENTEL", "25 MAN", "10QU", "DHAN10 KUNTAL", 
                 "GAHU 10 QUENTEL", "40000", 
                 "5 BARSA KO LAGI 2 LAKH LIYARA BANDHAKI RAKHEKO RA TYO KHET KO UBJANI. SABAI UNIHARU LE NAI KHANE GARERA DIYAKO JAHILE 2LAKH TIRINX TYO JAGGA FIRTA HUNE GARI") ~ "40000",
    v907a %in% c("41600", "DHAN 13 MURI PAYAKO") ~ "41600",
    v907a %in% c("12 QUENTAL DHAN KHET GARNE LE NAI SABAI KHARCH BEHORX", "42000") ~ "42000",
    v907a == "42300" ~ "42300",
    v907a %in% c("1.5 QUINTLE MUSTARD RECEIVED.THE LAND WAS GIVEN TO OTHERS IN THE CHAPTER", "42900") ~ "42900",
    v907a == "43900" ~ "43900",
    v907a == "44000" ~ "44000",
    v907a %in% c("45000(DHAN)", "45000") ~ "45000",
    v907a == "45600" ~ "45600",
    v907a == "46000" ~ "46000",
    v907a == "47250" ~ "47250",
    v907a %in% c("17 QUENTEL DHAN", "48000(DHAN)", "DHAN 12 QUENTAL ", "DHAN 15MURI", 
                 "DHAN12", "12 QUINTLE", "DHAN 15 MURI", "48000", "DHAN 12 QUENTAL") ~ "48000",
    v907a %in% c("20 MURI DHAN", "50(MAN DHAN RA MAIZE)(RS 50000)", "DHAN DAAL(RS50000 NEAR KO)", 
                 "DHAN GAHU DAAL (50000)", "25 BORA", "12.5 DHAN QUINTAL", "50000", "20 MURI") ~ "50000",
    v907a %in% c("13 QUENTEL", "13QUENTAL DHAN") ~ "52000",
    v907a %in% c("DHAN 55000", "CHAMAL") ~ "55000",
    v907a == "56000" ~ "56000",
    v907a == "58800" ~ "58800",
    v907a %in% c("40 MAN DHAN", "40MAN DHAN", "10 KUNTAL DHAN GAHU 5 KUNTAL GHEHU", 
                 "15 DHAN 3MURI DAL", "60000") ~ "60000",
    v907a == "61000" ~ "61000",
    v907a == "61400" ~ "61400",
    v907a == "61500" ~ "61500",
    v907a == "62500(DHAN)" ~ "62500",
    v907a %in% c("20MURI", "45 MAN", "DHAN25 MAN GEHU 15 MAN", "64000", "DHAN20MURI") ~ "64000",
    v907a == "65000" ~ "65000",
    v907a == "DHAN 12GAHU5(QUENTEL)" ~ "68000",
    v907a %in% c("DHAN 25MURI", "2 QUINTAL TORI", "70000") ~ "70000",
    v907a == "71100" ~ "71100",
    v907a %in% c("72000 (DHAN)", "72000") ~ "72000",
    v907a %in% c("DHAN 40 GEHU 5 MAN 2 MASURI MAN", "DHAN 8 GHEHU 4 MAN") ~ "73600",
    v907a == "75000" ~ "75000",
    v907a == "77000 DHAN" ~ "77000",
    v907a == "79500" ~ "79500",
    v907a %in% c("60 MAN DHAN DIYEKO", "DHAN 20 QU", "80000") ~ "80000",
    v907a == "83600" ~ "83600",
    v907a == "84000" ~ "84000",
    v907a == "53 MAN DHAN" ~ "84800",
    v907a == "85000" ~ "85000",
    v907a == "18DHAN 1.5 MURI DAL" ~ "86400",
    v907a %in% c("80 MAN DHAN DIYEKO", "90000") ~ "90000",
    v907a == "95000" ~ "95000",
    v907a %in% c("30 MURI DHAN DIYEKO", "DHAN 40 GEHU 20", "96000") ~ "96000",
    v907a %in% c("DHAN GAHU 45 QUENTAL", "100000") ~ "100000",
    v907a == "100500" ~ "100500",
    v907a == "104000" ~ "104000",
    v907a %in% c("105000 DHAN KO", "105000") ~ "105000",
    v907a == "DHAN 40 GEHU 20 MAN MASULI 2MAN" ~ "105600",
    v907a == "27QU" ~ "108000",
    v907a %in% c("DHAN 35 GEHU 21MAN", "110000") ~ "110000",
    v907a == "DHAN 40MAN GEHU 30MAN" ~ "112000",
    v907a == "114000" ~ "114000",
    v907a == "115000" ~ "115000",
    v907a == "120000" ~ "120000",
    v907a %in% c("4500 KG", "125000") ~ "125000",
    v907a == "130000" ~ "130000",
    v907a == "140000" ~ "140000",
    v907a == "144000" ~ "144000",
    v907a == "145000" ~ "145000",
    v907a == "148225" ~ "148225",
    v907a == "150000" ~ "150000",
    v907a == "40 DHAN MAN GEHU 20 MAN TORI 5 MAN" ~ "166000",
    v907a == "167500" ~ "167500",
    v907a == "170000" ~ "170000",
    v907a == "175000" ~ "175000",
    v907a == "180000" ~ "180000",
    v907a == "190000" ~ "190000",
    v907a == "192000" ~ "192000",
    v907a %in% c("200000 PAISA DINU BHAKO CHA TYO RETURN NAGARNE SAMMA KHETI GARI KHANA PAUNU HUNCHA", 
                 "200000  PAISA DINU BHAKO CHA TYO RETURN NAGARNE SAMMA KHETI GARI KHANA PAUNU HUNCHA", 
                 "200000 ( 5YEARS KO LAGI LIYEKO RA PAILAI TIREKO )", "50 QUINTLE", 
                 "5 BARSA KO LAGI 2 LAKH LIYARA BANDHAKI RAKHEKO RA TYO KHET KO UBJANI. SABAI UNIHARU LE NAI KHANE GARERA DIYAKO  JAHILE 2LAKH TIRINX TYO JAGGA FIRTA HUNE GARI",
                 "200000") ~ "200000",
    v907a == "215000" ~ "215000",
    v907a == "216000" ~ "216000",
    v907a == "220000" ~ "220000",
    v907a == "225000" ~ "225000",
    v907a == "240000" ~ "240000",
    v907a == "250000" ~ "250000",
    v907a == "260000" ~ "260000",
    v907a == "275000" ~ "275000",
    v907a == "300000" ~ "300000",
    v907a == "315000" ~ "315000",
    v907a == "350000" ~ "350000",
    v907a == "400000" ~ "400000",
    v907a == "480000" ~ "480000",
    v907a == "500000" ~ "500000",
    v907a == "800000" ~ "800000",
    v907a == "2000000" ~ "2000000",
    v907a == "5000000" ~ "5000000",
    v907a == "105000  DHAN KO " ~ "105000",

    TRUE ~ (trimws(v907a)) 

    )
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
  )

for (i in setdiff(1:ncol(section9a), c(2, 7, 8, 13, 20))) { 
  section9a[[i]] <- as.numeric(gsub("[^0-9]", "", section9a[[i]]))
}

#SECTION9B

for (i in setdiff(1:ncol(section9b), c(2, 7, 8))) { 
  section9b[[i]] <- as.numeric(gsub("[^0-9]", "", section9b[[i]]))
}

section9b <- section9b %>%
  mutate(
    v908 = case_when(
      (v909a > 0 | v909b > 0 | v909c > 0) ~ 1, 
      TRUE ~ 2
    ), 
    v911 = case_when(
      (v912a > 0 | v912b > 0 | v912c > 0) ~ 1, 
      TRUE ~ 2
    )
  )


#SECTION9C

for (i in setdiff(1:ncol(section9c), c(2, 7, 8, 11))) { 
  section9c[[i]] <- as.numeric(gsub("[^0-9]", "", section9c[[i]]))
}

section9c <- section9c %>%
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
    )
  )

section9c <- section9c %>%
  filter(!is.na(v915))

#SECTION9D

for (i in setdiff(1:ncol(section9d), c(2, 7, 8))) { 
  section9d[[i]] <- as.numeric(gsub("[^0-9]", "", section9d[[i]]))
}

section9d <- section9d %>%
  mutate(
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
    )
  ) %>%
  select(-v919a)

#SECTION9E

for (i in setdiff(1:ncol(section9e), c(2, 7, 8))) { 
  section9e[[i]] <- as.numeric(gsub("[^0-9]", "", section9e[[i]]))
}

section9e <- section9e %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    v934 = if_else(!is.na(v935), 1L, 2L)
  ) 

#SECTION9F1

for (i in setdiff(1:ncol(section9f1), c(2, 7, 8))) { 
  section9f1[[i]] <- as.numeric(gsub("[^0-9]", "", section9f1[[i]]))
}

section9f1 <- section9f1 %>%
  mutate(
    v941 = if_else(
      is.na(v941), 
      0,
      v941
    )
  )

#SECTION9F2

for (i in setdiff(1:ncol(section9f2), c(2, 7, 8))) { 
  section9f2[[i]] <- as.numeric(gsub("[^0-9]", "", section9f2[[i]]))
}

section9f2 <- section9f2 %>%
  mutate(
    v943 = if_else(
      is.na(v943), 
      0,
      v943
    )
  )

#SECTION10

section10 <- section10 %>%
  mutate(
    v1002b = case_when(
    v1002b %in% c("AGRICULTURAL BUSINESS, 96", "KUKHURA PALAN, 96") ~ "1",

    v1002b %in% c("(TRAILER) LUGA SILAYUNE KAM, 96", ", 96, KUTANI, PISANI MILL", 
                  "7, 3", "8, 3", "96, DHAATU SAMBANDHI SABAI KAAM GARNE", 
                  "96, KAPADA SILAUNE TAILORING", "96, TAILOR", "AARAN, 1", 
                  "FURNISHING, 96", "FURNITURE KARKHANA SIKARMIKO KAM., 6", 
                  "GRIL PASAL, 96", "KAPADA SILAUNE, 96", "KAPADA SILSUNE, 96", 
                  "KUTANPISAN, 96", "MASALA PIDHANE MIL, 96", "MATO VADA HARU BANAUNE, 19", 
                  "MIL, 96", "TAILORING BUSINESS, 96", "TELARING, 96") ~ "3",

    v1002b %in% c("6, 7", "7, 6") ~ "6",

    v1002b %in% c("7", "7, KIRANA PASAL", "96, 7", "96, MEDICINE PASAL", 
                  "96, PHARMACY", "CHICKEN, 96", "JAAD RAKSI, 96", 
                  "KIRAN PASAL, 96", "KIRANA PASAL, 96", "KIRANA PSAL, 96", 
                  "PHARMACY, 96", "RUDRAKSHYA SEASONAL BUSINESS, 96", 
                  "SAIKAKO BASAL, 96", "SEEING CLOTHES, 96", "SELLING OF GOODS, 96", 
                  "STATIONARY SAMAN, 96") ~ "7",

    v1002b %in% c("7, 8", "8, 7", "DHUWANI SEWA, 96", "DRIVING, 96", 
                  "SAFARI - EV CHALAUNU HUNEY, 96", "SAFARI DRIVING, 96") ~ "8",

    v1002b %in% c("7, 9", "9, 10", "96", "96, AAFNAI CHIYA KHAJA PASAL", 
                  "96, BHOJ BIHE PARTY HARUMA KHANA BANAUNE KHANA KHANE BADHA HARU BHADA MA LAGAUNE", 
                  "96, HOTEL", "CHATPAT PASAL MA CHATPAT SELL GARNE, 96", 
                  "CHIYA PASAL, 96", "HOTEL BEBASAYA, 9") ~ "9",

    v1002b %in% c("96, CONSULTING FIRM", "LEKHAPDI, 96", "VET CLINIC GAI BASTU KO CLINIC, 96") ~ "14",

    v1002b == "17, BORADING SCHOOL  CHALAUNE" ~ "16",

    v1002b == "7, 18" ~ "18",

    v1002b == "GAMING ZONE, 96" ~ "19",

    v1002b %in% c("96, BEAUTY PARLOUR", "96, KAPAL KATANE", "96, MECHINARY SAMAKO SERVICE CENTER", 
                  "96, PARLOUR", "BEAUTY PARLER, 96", "BEAUTY PARLOR, 96", 
                  "CHINNA HERAUNEY KAM HAAT HERIDINEY KAM GARIDINU HUNXA, 96", 
                  "HAIR CUT SALON, 96", "HAIRCUT SOLON, 96", "KAPAL KATNE, 96", 
                  "WATCH REPAIR AND WATCH CENTER, 96") ~ "20",

    TRUE ~ v1002b
  )
)

for (i in setdiff(seq_len(ncol(section10)), c(2, 7, 8, 13, 15))) {
    section10[[i]] <- as.numeric(gsub("[^0-9]", "", section10[[i]]))
}

section10 <- section10 %>%
  mutate(
    v1004 = if_else(
      v1004 > 100 | is.na(v1004) | v1004 == 0,
      100, 
      v1004
    ),
    v1005 = case_when(
      v1002c %in% c("MASU TARKARI, MASU TARKARI BECHNE, LASUN LYERA BOKRA XODAYERA ORDER ANUSAR SUPPLY GARNE") ~ 400000,
      v1002c %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 540000000,
      v1002c %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 16200000,
      v1002c %in% c("KIRANA SAMAN BIKRI") ~ 1500000,
      v1002c %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 1545000,
      v1002c %in% c("AAFNO HIACE CHALAUNE KARMACHARI SAHIT, DHAAN KUTNE, TEL PELNE") ~ 6400000,
      v1002c %in% c("KIRANA SAMAN BECHNE, EGG CRATE BECHNE") ~ 600000,
      v1002c %in% c("SUN PASAL, SHINGAR KA SAMAN BECHNE") ~ 2000000,
      v1002c %in% c("KHAJA GHAR, PHOTO STUDIO") ~ 1000000,
      v1002c %in% c("TARKARI BECHNE, NASTA KHAJA") ~ 900000,
      v1002c %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 500000,
      v1002c %in% c("KIRANA PASAL, BRAMMAN, PANDIT, PADNE") ~ 450000,
      v1002c %in% c("KAPADA SILAUNE RA MARMAT SAMBHAR, COSMETICS JUTTA CHAPPAL") ~ 350000,
      v1002c %in% c("MANCHHE OSAR PASAR GARNE, KIRANA PASAL") ~ 360000,
      v1002c %in% c("COSMETICS SAMAN BECHNE RA PARLOUR KO KAAM, MANCHHE OSAR PASAR GARNE") ~ 360000,
      v1002c %in% c("MOBILE BANAUNE NAYA MOBILE BECHNE ELECTRIC SAMAN BECHNE, DHAN GAHU KUTANI PISANI") ~ 210000,
      v1002c %in% c("DHAN KUTAN PISANI, KIRANA PASAL") ~ 156000,
      v1002c %in% c("KIRANA SAMAN BIKRI") ~ 3600000,
      v1002c %in% c("PUJA KO SAMAN BECHNE, CAR CHALAUN SIKAUNE") ~ 1200000,
      TRUE ~ v1005
    ),
    v1006 = case_when(
      (is.na(v1007) | v1007 == 0) ~ 2, 
      TRUE ~ 1
    ),
    v1007 = case_when(
      v1002c %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 860000,
      v1002c %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 16200000,
      TRUE ~ v1007
    ),
    v1008 = case_when(
      v1002c %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 265000,
      v1002c %in% c("AAFNO HIACE CHALAUNE KARMACHARI SAHIT, DHAAN KUTNE, TEL PELNE") ~ 2400000,
      v1002c %in% c("PUJA KO SAMAN BECHNE, CAR CHALAUN SIKAUNE") ~ 200000,
      v1002c %in% c("KIRANA SAMAN BECHNE, EGG CRATE BECHNE") ~ 840000,
      v1002c %in% c("TARKARI BECHNE, NASTA KHAJA") ~ 60000,
      v1002c %in% c("MANCHHE OSAR PASAR GARNE, KIRANA PASAL") ~ 60000,
      v1002c %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 22000,
      v1002c %in% c("COSMETICS SAMAN BECHNE RA PARLOUR KO KAAM, MANCHHE OSAR PASAR GARNE") ~ 28000,
      v1002c %in% c("SUN PASAL, SHINGAR KA SAMAN BECHNE") ~ 3000,
      v1002c %in% c("KIRANA PASAL, BRAMMAN, PANDIT, PADNE") ~ 2500,
      v1002c %in% c("KAPADA SILAUNE RA MARMAT SAMBHAR, COSMETICS JUTTA CHAPPAL") ~ 13200,
      TRUE ~ v1008
    ), 
    v1009a = case_when(
      v1002c %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 2800000,
      v1002c %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 500000,
      v1002c %in% c("SUN PASAL, SHINGAR KA SAMAN BECHNE") ~ 350000,
      v1002c %in% c("MASU TARKARI, MASU TARKARI BECHNE, LASUN LYERA BOKRA XODAYERA ORDER ANUSAR SUPPLY GARNE") ~ 200000,
      v1002c %in% c("KIRANA SAMAN BECHNE, EGG CRATE BECHNE") ~ 360000,
      v1002c %in% c("KAPADA SILAUNE RA MARMAT SAMBHAR, COSMETICS JUTTA CHAPPAL") ~ 150000,
      v1002c %in% c("MOBILE BANAUNE NAYA MOBILE BECHNE ELECTRIC SAMAN BECHNE, DHAN GAHU KUTANI PISANI") ~ 58000,
      v1002c %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 30000,
      v1002c %in% c("KIRANA SAMAN BIKRI") ~ 342000,
      v1002c %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 1490400,
      v1002c %in% c("KIRANA SAMAN BIKRI") ~ 1476000
    ),
    v1009b = case_when(
      v1002c %in% c("KIRANA SAMAN BIKRI") ~ 1200000,
      v1002c %in% c("KHET JOTNE DHAN GAHU JHARNE, KIRANA KHADHYANA SAMAN WHOLESALE PETROL , MEDICINE SABAI KO") ~ 500000,
      v1002c %in% c("KHAJA GHAR, PHOTO STUDIO") ~ 250000,
      v1002c %in% c("TARKARI BECHNE, NASTA KHAJA") ~ 350000,
      v1002c %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 25000,
      v1002c %in% c("KUKHURAKO DANA, CHHALLA, KUKHURA SAGA SAMBANDHIT SAAMANHARU") ~ 115000,
      v1002c %in% c("PAPER SUPPLY") ~ 1000000, 
      TRUE ~ v1009b
    ),
    v1010 = case_when(
      v1002c %in% c("MASU KATERA BECHNE, KIRANA PASAL") ~ 40000,
      v1002c %in% c("GITTI BALUWA LOAD, KIRANA SAMAN BECHNE") ~ 460000, 
      TRUE ~ v1010
    ),
    v1011 = case_when(
      v1002a %in% c("MASU PASAL, LASUN LYERA BOKRA XODAYERA ORDER ANUSAR SUPPLY GARNE") ~ 388000,
      v1002a %in% c("AAFNO HIACE CHALAUNE, AAFNO MIL CHALAUNE") ~ 3000000,
      v1002a %in% c("KIRANA STORE, EGG CRATE FACTORY") ~ 800000,
      v1002a %in% c("MEDICINE PASAL, THEKKA PATTA GARNE  GHAR, NALA ,ROAD, BADH  BANAUNE") ~ 8520000,
      v1002a %in% c("SUN CHADI KO GHANA BECHNE, COSMETICS PASAL") ~ 3290000,
      v1002a %in% c("FRESS HOUSE, KIRANA PASAL") ~ 1030000,
      v1002a %in% c("TARKARI BECHNE, KHAJA NASTA") ~ 382000,
      v1002a %in% c("KIRAN PASAL") ~ 3976000,
      v1002a %in% c("1") ~ 3976000,
      v1002a %in% c("HOTEL, PHOTO STUDIO") ~ 1864000,
      v1002a %in% c("PASAL, PANDIT") ~ 1780000,
      v1002a %in% c("AUTO CHALAUNE, KIRANA PASAL") ~ 676000,
      v1002a %in% c("BEAUTY PARLOUR N COSMETICS, AUTO CHALAUNE") ~ 620000,
      v1002a %in% c("PUJA PASAL, CAR DRIVING CENTER") ~ 640000,
      v1002a %in% c("TRUCK DRIVER, KIRANA STORE") ~ 270000,
      v1002a %in% c("TAILOR, COSMETICS PLUS JUTTA CHAPPAL") ~ 344000,
      v1002a %in% c("TRACTOR THRESAR KHET JODNE DHAN GAHU JHARNE, KIRANA PASAL KHADHYANA SAMAN WHOLESALE") ~ 587887,
      v1002a %in% c("MOBILE PASAL, ELECTRIC SAMAN BECHNE, MEEL CHALAUNE KUTANI PISANI KHADHYANA SAMAN") ~ 700000,
      v1002a %in% c("MEEL CHALAUNE KUTANI PISANI, KIRANA PASAL") ~ 960000,
      v1002a %in% c("FRESH HOUSE") ~ 731000,
      TRUE ~ v1011
    )
  )

#SECTION11A

section11a <- section11a %>%
  mutate(v1105 = case_when(
    v1105 %in% c("2, PRABHU BANK", "MEGHA BANK, 2", "NEPAL BANK, 1") ~ "1",

    v1105 %in% c("KRISHI BANK, 2", "KRISHI BANK, 96", 
                  "KRISHI BIKASH BANK, 2", "KRISHI BIKASH BANK, 96") ~ "2",

    v1105 %in% c("GARIBI NIBARAN, 4", "SANO KISAN, 96", "UNIQUE NEPAL SAMUHA, 4") ~ "4",

    v1105 == "96, THULA SAHAKARMI SAMUHA" ~ "5",

    v1105 %in% c("SANCHAYA KOSH, 96", "SANJAY KOSH, 96") ~ "6",

    v1105 %in% c("96", "96, MAHILA SAMUHA", "96, SAMUHA BATA", "96, SAMUHABATA", 
                  "96, SAVING GROUP", "96, SSF", "96, YUBA CLUB", "AAMA SAMUHA, 96", 
                  "AKASMIT KOSH, 96", "AMA SAMUHA BATA, 96", "AMA SAMUHA, 96", 
                  "BACHAT SAMUHA BATA, 96", "BACHAT SAMUHA, 96", "BIMA COMPANY BATA, 96", 
                  "DALIT SAMUHA, 96", "GAU KO SAMUHA, 96", "GAUGHAR KO SAMUHA BATA, 96", 
                  "GAUGHARKO SAMUHA, 96", "GHARGAU KO SAMUHA BATA, 96", "GHAUGHAR SAMUHABAT, 96", 
                  "GHAUGHARKO SAMUHA, 96", "KRISHI SAMUHA, 96", "KRISHI SAMUHAKO, 96", 
                  "LIFE INSURANCE, 96", "MAHILA SAMUHA, 96", "MET LIFE INSURANCE, 96", 
                  "MOTHERS GROUP MA, 96", "NAGARPALIKA, 96", "NATIONAL LIFE INSURANCE, 96", 
                  "NEPAL LIFE INSURANCE, 96", "PANI KO SAMHUA, 96", "PRIME LIFE INSIRENCE, 96", 
                  "SAMUHA BATA LIYEKO, 96", "SAMUHA BATA, 96", "SAMUHA, 96", 
                  "SAMUHABATA, 96", "SAMUHBAT, 96", "SAVING GROUP, 96", 
                  "SSF BATA LEYAKO, 96", "SSF BATA, 96", "SSF, 96", 
                  "TOL BIKASH SAMITI, 96", "TOL BIKASH, 96", "TOLBIKASH SAMITI, 96", 
                  "WOMAN GROUP, 96", "WOMEN GROUP, 11", "WOMEN'S GROUP, 96", "YOUBA CLUB, 96") ~ "8",

    v1105 %in% c("12", "96, व्यक्तिगत समूहबाट", "BIMA BATW, 11", "CHHIMEKI BATA, 96") ~ "11",

    TRUE ~ v1105
  ))

for (i in setdiff(1:ncol(section11a), c(2, 7, 8, 11, 14, 19, 22))) { 
  section11a[[i]] <- as.numeric(gsub("[^0-9]", "", section11a[[i]]))
}

section11a <- section11a %>%
  mutate(
    v1106 = case_when(
      grepl("^[0-9]+$", v1102) ~ as.numeric(v1102),
      TRUE ~ v1106
    )
  )

#SECTION11B

for (i in setdiff(1:ncol(section11b), c(2, 7, 8, 11, 14, 19))) { 
  section11b[[i]] <- as.numeric(gsub("[^0-9]", "", section11b[[i]]))
}

#SECTION11C

for (i in setdiff(1:ncol(section11c), c(2, 7, 8))) { 
  section11c[[i]] <- as.numeric(gsub("[^0-9]", "", section11c[[i]])) 
}

section11c <- section11c %>%
  mutate(
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
      TRUE ~ 1128
    ), 
    v1127 = case_when(
      is.na(v1127) ~ 2, 
      TRUE ~ 1
    )
  )

#SECTION12A

section12a <- section12a %>%
  filter(
    v1204 != "" &
    v1205 != "" & 
    v1206 != ""
  ) %>%
  mutate(
    v1205 = case_when(
      v1205 %in% c(
        "UTA PANI BASOBAS VAYEKO LE, 96", "UTA PAHAD MA AFNU GHR MA BASEKO, 96",
        "KATHMANDU MA NI GHAR CHA SO TETAI BASNA AND KAAM GARNA, 96", 
        "GHARMA BASNEE, 96", "GHAR MA BASNA GAYEKO, 96", "GHAR CHHADEKO HOINNA GAAU KO GHAR MA BASNE, 96",
        "GHAR BASEKO, 96", "GAUKO AAFNAI GHAR MA BASEKO, 96", "GAU TIRAI BASNE, 96", 
        "DEPENDENT VISA MA GAYEKO, 96", "ARKO TIR PANI BASOBAS VAYEKO LE, 96", 
        "AAMA AAFNAI GHARMA HUNUHUNCHHA, 96", "AAFAI GHAR MA BASNU HUNCHHA, 96",
        "96, आफ्नै घरमा बसेको", "96, UTA GHAR MAI BASNU HUNCHHA", "96, UHA TETAI BASNU HUNEY GAU KO GHAR ( GURBAKOT)", 
        "96, UHA KO GHR MAI TEHI HO", "96, UHA GHR MAI HUNUHUNCHA", "96, UHA AFNAI GHR MA HUNUHUNCHA",
        "96, SRIMAN SANGAI GAYA KO", "96, SHRIMAN SANG", "96, SHERMAN SANG", "96, SEPARATED FROM HUSBAND BUT NOT DIVORCE",
        "96, POKHARA MA AAFNAI GHAR MA BASNE", "96, PAHAD KO GHR MA BASEKO", "96, NEW BIRN BABY 6 MONTH", 
        "96, MAMA GHAR MA BASEKO", "96, INDIA MA PANI GHAR CHHA UTAI  BASNU HUNX", "96, GHR NAI TEHI HO",
        "96, GHAR MAI BASCHAN USKO BUDA CHAI BUTWAL HO BASNE", "96, GHAR KURNA", 
        "96, GHAR CHHADEKO HOINNA GAAU KO GHAR MA BASNE", "96, GHAR BYABHAR NAMILERA ALAG BASEKO", 
        "96, GHAR BASEKO MAKAWANPUR", "96, GAUMA PANI GHAR VAYEKO LE", "96, GAUKO KO", 
        "96, GAUKO GHAR MA BASNE", "96, GAUKO GHAR MA BASEKO", "96, GAUKO AAFNAI GHAR MA BASNE GAREKO", 
        "96, GAUKO AAFNAI GHAR MA BASEKO GHAR CHHADEKO HOINA", "96, GAU KO GHAR MA BASNU HUNCHHA",
        "96, GAU KO GHAR MA BASNE GAREKO N", "96, GAU KO GHAR MA BASNE GAREKO", "96, GAU KO GHAR MA BASNE", 
        "96, GAU KO GHAR MA BASEKO", "96, GAAU KO GHAR MA BASNE KAHILE YETA KAHILE UTA",
        "96, GAAU KO GHAR MA BASNE AAFNO KHETRI PATI GARNE", "96, GAAU KO GHAR MA BASNE", 
        "96, FAMILY SABAI RAMECHHP MA BASNE", "96, AAMA HERNA BASEKO",
        "96, BIMALA ARYAL IS CURRENTLY LIVING WITH HER FAMILY MEMBERS IN MAITI.SHE IS CURRENTLY IN LABOR..LT SEEMS THAT ABOUT 60-70 THOUSAND RUPEES WAS SPENT IN THE HOSPITAL DURING THE DELIVERY.",
        "96, BECAUSE HIS MOTHER AND FATHER LIVE THERE", "96, BACHHA AAMA SANG", 
        "96, BACHH SAPATARI MA NAI JANME KO RA U JANME DEKHI TEHI BASE KO", 
        "96, BABA AAMA SANG BASNE", "96, AFNAI GHAR MA BASEKO", "96, AAFNAI GHAR MA BASEKO", 
        "96, AAFNAI GHAR KO GHAR", "96, AAFAI GHAR MA BASNU HUNCHHA", "96", "2 TIR BASOBAS BHAYAKO LE, 96",
        "2 THAU MA BASOBAS VAYEKO LE, 96", "1, MAMU SANGA GAYEKO"
      ) ~ "1", 
      v1205 %in% c(
        "PADHAI KO LAGI, 96", "96, TO EDUCATE HER SON", "96, SUDHAR KENDRA RAKHYAKO", 
        "96, SHIV PRASAD CHAUDARY IS CURRENTLY IN THE JAIL AS A PRISONER AND IS SERVING A PRISON SENTENCE.",
        "96, LAMA PADHNA KTM BASNU VAYEKO", "96, JAIL", "96, FOR HER DAUGHTER'S EDUCATION,SHE LIVES IN KTM.", 
        "96, ENTRANCE",  "96, BACHHA PADHAUNA", "96, BACHCHA PADHAUNA", "2, PADHNA"
      ) ~ "2", 
      v1205 %in% c(
        "RAJNITI, 96", "KAM SIKNA, 96", "96, INTERNSHIP", "96, GHARMA JHAGADA GARERA BHAGEKO", "96, GALAT SANGATMA PARERA"
      ) ~ "3",
      v1205 %in% c(
        "THE RESPONDENT LEFT THE HOUSE FOR JOB BUT OTHER FAMILY LIVES IN HOME., 96",
        "SASASTRA PRAHARI, 96","JOB, 96", "JOB MA, 96", "JAGIR, 96", "INDIAN ARMY, 96",
        "GOVERNMENT JOB, 96", "BAIDESIK, 96", "BAIDESIK ROJGARI, 96", "BAIDESIK ROGARI, 96", 
        "APF, 96", "96, UTTARDATA KTM MA KAM KO SILSILA MA BASNE DALJIT JEE GAAU MA AAFNAI GHAR MA BASNE JANMA DEKHI MAI BASIRAHEKO GHAR CHHADEKO HOINA",
        "96, THE RESPONDENT LEFT THE HOUSE FOR JOB BUT HIS WIFE IS IN SINDULI", "96, THE RESPONDENT LEFT HOME FOR JOB BUT MOTHER IS IN HOME.", 
        "96, ROJGAR KO LAGI", "96, RETURN BACK TO JOB", "96, KHETI PATI GARNA LEKH TIRA KO GHAR MA", 
        "96, KAM KO LAGI TRAVEL GUIDE", "96, KAM GARNE0", "96, KAM GARNE INDIA", "96, JHAPA MA KHETIPATI GARNA JANUVAYAKO",
        "96, JHAPA MA JOB GARNE", "96, JAAGIR GARNA", "96, BAIDESIK ROGARI MA JANE TAYARI", 
        "96, BAIDESIK ROGARI", "96, ARMY", "96, DOCTOR", "96, AAFNAI KRISHI TATHA PASHUPALAN KO LAGI", 
        "96, AAFNAI KRISHI TATHA PASHUPALAN", "3, PATHAU CHALAUNE"
      ) ~ "4", 
      v1205 %in% c(
        "96, SANO KIRANA PASAL TATHA KAWAD KO KAGAJ CARTOON HARU JAMMA GAERA BECHNU HUNCHA",
        "96, SANO KIRANA PASAL TATHA KAWAD KO KAGAJ CARTOON HARU JAMMA GAERA BECHNU HUNCHA"
      ) ~ "5",
      v1205 %in% c(
        "96, ACCORDING TO KUNTI SHARMA,MADHUSUDAN GAIRE HAS BEEN RECEIVING CONTINUED. TREATMENT IN A HOSPITAL IN INDIA FOR THE PAST TWO YEARS DUE TO A SEVERE MENTAL ILLNESS.. SIMILARLY 1.5_2 LAKH RUPEES HAVE BEEN SPENT ANNUALLY ON HIS TREATMENT.",
        "96, BIRAMI BHAYERA DHARAN MA HOSPITAL NAJIK RAKHEKO"
      ) ~ "6",
      v1205 %in% c(
        "3, TRAVEL"
      ) ~ "7",
      TRUE ~ v1205
    ), 
    v1207 = case_when(
      is.na(v1207) & v1205 %in% c("1") ~ 3,
      is.na(v1207) & v1205 %in% c("2") ~ 4, 
      TRUE ~ v1207
    )
  )
  

for (i in setdiff(1:ncol(section12a), c(2, 7, 8, 16))) { 
  section12a[[i]] <- as.numeric(gsub("[^0-9]", "", section12a[[i]]))
}

section12a <- section12a %>%
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

#SECTION12B

for (i in setdiff(1:ncol(section12b), c(2, 7, 8))) { 
  section12b[[i]] <- as.numeric(gsub("[^0-9]", "", section12b[[i]]))
}

section12b <- section12b %>%
  mutate(
    v1213a = case_when(
      v1213b > 0 ~ 1, 
      TRUE ~ 2
    ), 
    v1214a = case_when(
      v1214b > 0 ~ 1, 
      TRUE ~ 2
    )
  )

#SECTION13A

for (i in setdiff(1:ncol(section13a), c(2, 7, 8))) { 
  section13a[[i]] <- as.numeric(gsub("[^0-9]", "", section13a[[i]]))
}

section13a <- section13a %>%
  mutate(
    v1303 = case_when(
      v1303 == 0 & is.na(v1304a) ~ NA_real_,
      v1303 == 0 & !is.na(v1304a) ~ 1, 
      TRUE ~ v1303
    )
  )

#SECTION13B

section13b <- section13b %>%
  mutate(
    v1309 = case_when(
      is.na(v1310) ~ 2, 
      TRUE ~ 1
    )
  )

#SECTION13C

for (i in setdiff(1:ncol(section13c), c(2, 7, 8))) { 
  section13c[[i]] <- as.numeric(gsub("[^0-9]", "", section13c[[i]]))
}

section13c <- section13c %>%
  mutate(
    v1311b = case_when(
      is.na(v1312) | v1312 == 0 ~ 2, 
      TRUE ~ 1
    ), 
    v1312 = case_when(
      v1312 == 0 ~ NA_real_,
      TRUE ~ v1312
    )
  )

#WEALTH INDEX 

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
    urban_rural = case_when(
      palika_type %in% c(1, 2, 3) ~ 1, #urban
      palika_type %in% c(4) ~ 2 #rural
    )
  )

wealth_index <- section2a1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  ) %>%
  select(hhid, v202, v203, v204, v205, v206)

wealth_index <- wealth_index %>%
  mutate(
    mud_bonded_foundation = case_when(
      v203 == 1 ~ 1, 
      TRUE ~ 0
    ),
    cement_bonded_foundation = case_when(
      v203 == 2 ~ 1, 
      TRUE ~ 0
    ),
    concrete_pillar_foundation = case_when(
      v203 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    wooden_pillar_foundation = case_when(
      v203 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    sheets_foundation = case_when(
      v203 == 5 ~ 1, 
      TRUE ~ 0
    ),
    mud_bonded_wall = case_when(
      v204 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    cement_bonded_wall = case_when(
      v204 == 2 ~ 1, 
      TRUE ~ 0
    ), 
    wooden_wall = case_when(
      v204 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    bamboo_wall = case_when(
      v204 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    unbaked_brick_wall = case_when(
      v204 == 5 ~ 1, 
      TRUE ~ 0
    ), 
    sheets_wall = case_when(
      v204 == 6 ~ 1, 
      TRUE ~ 0
    ), 
    sheets_roof = case_when(
      v205 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    rcc_roof = case_when(
      v205 == 2 ~ 1, 
      TRUE ~ 0
    ), 
    tile_roof = case_when(
      v205 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    stone_roof = case_when(
      v205 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    wood_roof = case_when(
      v205 == 5 ~ 1, 
      TRUE ~ 0
    ), 
    straw_roof = case_when(
      v205 == 6 ~ 1, 
      TRUE ~ 0
    ), 
    mud_floor = case_when(
      v206 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    cement_floor = case_when(
      v206 == 2 ~ 1, 
      TRUE ~ 0
    ), 
    tile_floor = case_when(
      v206 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    plank_floor = case_when(
      v206 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    parquet_floor = case_when(
      v206 == 5 ~ 1, 
      TRUE ~ 0
    )
  ) %>%
  select(-v203, -v204, -v205, -v206)

section2a2 <- section2a2 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

wealth_index <- merge(
  wealth_index, 
  section2a2[, c("hhid", "v208", "v213")],
  by = "hhid"
)

wealth_index <- wealth_index %>%
  mutate(
    dwelling_ownership = case_when(
      v208 == 1 ~ 1, 
      TRUE ~ 0
    ),
    owner_occupancy = case_when(
      v213 == 1 ~ 1, 
      TRUE ~ 0
    )
  ) %>%
  select(-v208, -v213)

section2a3 <- section2a3 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

wealth_index <- merge(
  wealth_index, 
  section2a3[, c("hhid", "v216", "v223", "v225", "v218", "v220", "v222a", "v222b", "v222c")], 
  by = "hhid"
)

wealth_index <- wealth_index %>%
  mutate(
    piped_water_private = case_when(
      v216 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    piped_water_shared = case_when(
      v216 == 2 ~ 1, 
      TRUE ~ 0,
    ),
    handpump = case_when(
      v216 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    covered_well = case_when(
      v216 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    uncovered_well = case_when(
      v216 == 5 ~ 1, 
      TRUE ~ 0
    ), 
    spout_water = case_when(
      v216 == 6 ~ 1, 
      TRUE ~ 0
    ), 
    river = case_when(
      v216 == 7 ~ 1, 
      TRUE ~ 0
    ), 
    jar = case_when(
      v216 == 8 ~ 1, 
      TRUE ~ 0
    ), 
    tanker = case_when(
      v216 == 9 ~ 1, 
      TRUE ~ 0
    ), 
    municipality = case_when(
      v223 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    private_collector = case_when(
      v223 == 2 ~ 1, 
      TRUE ~ 0
    ), 
    dumping = case_when(
      v223 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    burned = case_when(
      v223 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    fertilizer = case_when(
      v223 == 5 ~ 1, 
      TRUE ~ 0
    ), 
    public_sewage = case_when(
      v225 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    septic_tank = case_when(
      v225 == 2 ~ 1, 
      TRUE ~ 0 
    ), 
    ordinary_toilet = case_when(
      v225 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    public_toilet = case_when(
      v225 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    no_toilet = case_when(
      v225 == 5 ~ 1, 
      TRUE ~ 0
    ), 
    firewood = case_when(
      v218 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    lp_gas = case_when(
      v218 == 2 ~ 1, 
      TRUE ~ 0
    ), 
    biogas_cooking = case_when(
      v218 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    kerosene_cooking = case_when(
      v218 == 4 ~ 1, 
      TRUE ~ 0
    ), 
    dung_cake = case_when(
      v218 == 5 ~ 1, 
      TRUE ~ 0
    ), 
    electricity_cooking = case_when(
      v218 == 6 ~ 1, 
      TRUE ~ 0
    ),
    electricity_light = case_when(
      v220 == 1 ~ 1, 
      TRUE ~ 0
    ), 
    solar = case_when(
      v220 == 2 ~ 1, 
      TRUE ~ 0
    ), 
    kerosene_lighting = case_when(
      v220 == 3 ~ 1, 
      TRUE ~ 0
    ), 
    biogas_lighting = case_when(
      v220 == 4 ~ 1, 
      TRUE ~ 0
    ),
    internet = case_when(
      v222c == 1 ~ 1, 
      TRUE ~ 0
    )
  ) %>%
  select(-v216, -v223, -v225, -v218, -v220, -v222a, -v222b, -v222c)

assets <- section4c %>%
  mutate(hhid = paste0(psu, "-", hhld)) %>%
  filter(v408 %in% c(1:27)) %>%
  mutate(
    asset = recode(
      v408,
      `1` = "radio", 
      `2` = "camera",
      `3` = "bicycle",
      `4` = "rickshaw",
      `5` = "motorcycle",
      `6` = "tractor", 
      `7` = "car", 
      `8` = "bus",
      `9` = "refrigerator", 
      `10` = "microwave", 
      `11` = "geyser", 
      `12` = "washing_machine", 
      `13` = "fan",
      `14` = "heater", 
      `15` = "television", 
      `16` = "air_conditioner", 
      `17` = "vacuum_cleaner", 
      `18` = "inverter", 
      `19` = "solar_panel", 
      `20` = "solar_heater", 
      `21` = "electric_iron", 
      `22` = "telephone", 
      `23` = "sewing_machine", 
      `24` = "computer", 
      `25` = "wrist_watch", 
      `26` = "furniture", 
      `27` = "lpg_stove"
    )
  ) %>%
  select(hhid, asset, v409) %>%
  pivot_wider(
    names_from = asset,
    values_from = v409
  )

wealth_index <- wealth_index %>%
  left_join(assets, by = "hhid")

land_ownership <- section9a %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  ) %>%
  group_by(hhid) %>%
  slice(1) %>%
  ungroup()

wealth_index <- merge(
  wealth_index, 
  land_ownership[, c("hhid", "v901")],
  by = "hhid", 
  all = TRUE
)

livestock_ownership <- section9e %>%
  filter(!is.na(v933)) %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

wealth_index <- merge(
  wealth_index, 
  livestock_ownership[, c("hhid", "v934")],
  by = "hhid", 
  all = TRUE
)

section0 <- section0 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )  

wealth_index <- merge(
  wealth_index, 
  section0[, c("hhid", "hhld_member_t")], 
  by = "hhid"
)

wealth_index <- wealth_index %>%
  rename(
    land_ownership = v901, 
    livestock_ownership = v934
  ) %>%
  mutate(
    hhld_member_t = as.numeric(hhld_member_t),
    hhld_member_t = if_else(is.na(hhld_member_t), 11, hhld_member_t),
    rooms_per_capita = v202 / hhld_member_t, 
    rooms_per_capita = scale(rooms_per_capita),
    radio = if_else(
      radio == 2, 0, 1
    ),
    camera = if_else(
      camera == 2, 0, 1
    ),
    bicycle = if_else(
      bicycle == 2, 0, 1
    ),
    rickshaw = if_else(
      rickshaw == 2, 0, 1
    ),
    motorcycle = if_else(
      motorcycle == 2, 0, 1
    ),
    tractor = if_else(
      tractor == 2, 0, 1
    ),
    car = if_else(
      car == 2, 0, 1
    ),
    bus = if_else(
      bus == 2, 0, 1
    ),
    refrigerator = if_else(
      refrigerator == 2, 0, 1
    ),
    microwave = if_else(
      microwave == 2, 0, 1
    ),
    geyser = if_else(
      geyser == 2, 0, 1
    ),
    washing_machine = if_else(
      washing_machine == 2, 0, 1
    ),
    fan = if_else(
      fan == 2, 0, 1
    ),
    heater = if_else(
      heater == 2, 0, 1
    ),
    television = if_else(
      television == 2, 0, 1
    ),
    air_conditioner = if_else(
      air_conditioner == 2, 0, 1
    ),
    vacuum_cleaner = if_else(
      vacuum_cleaner == 2, 0, 1
    ),
    inverter = if_else(
      inverter == 2, 0, 1
    ),
    solar_panel = if_else(
      solar_panel == 2, 0, 1
    ),
    solar_heater = if_else(
      solar_heater == 2, 0, 1
    ),
    electric_iron = if_else(
      electric_iron == 2, 0, 1
    ),
    telephone = if_else(
      telephone == 2, 0, 1
    ),
    sewing_machine = if_else(
      sewing_machine == 2, 0, 1
    ), 
    computer = if_else(
      computer == 2, 0, 1
    ),
    wrist_watch = if_else(
      wrist_watch == 2, 0, 1
    ),
    furniture = if_else(
      furniture == 2, 0, 1
    ),
    lpg_stove = if_else(
      lpg_stove == 2, 0, 1
    ),
    land_ownership = case_when(
      land_ownership == 2 | is.na(land_ownership) ~ 0,
      TRUE ~ 1
    ),
    livestock_ownership = case_when(
      livestock_ownership == 2 | is.na(livestock_ownership) ~ 0,
      TRUE ~ 1
    )
  ) %>%
  select(-v202, -hhld_member_t)

wealth_index <- wealth_index %>%
  left_join(
    section0 %>% select(hhid, urban_rural),
    by = "hhid"
  )

wealth_urban <- wealth_index %>%
  filter(urban_rural == 1)

wealth_rural <- wealth_index %>%
  filter(urban_rural == 2)

pca_common <- rbind(
  wealth_urban,
  wealth_rural
)

pca_common <- pca_common %>%
  select(-hhid, -urban_rural)

pca_input_urban <- wealth_urban %>%
  select(-hhid, -urban_rural) 

pca_input_rural <- wealth_rural %>%
  select(-hhid, -urban_rural) 

zero_var_common <- sapply(pca_common, function(x) sd(x, na.rm = TRUE) == 0)
zero_var_urban <- sapply(pca_input_urban, function(x) sd(x, na.rm = TRUE) == 0)
zero_var_rural <- sapply(pca_input_rural, function(x) sd(x, na.rm = TRUE) == 0)

pca_common <- pca_common[, !zero_var_common]
pca_input_urban <- pca_input_urban[, !zero_var_urban]
pca_input_rural <- pca_input_rural[, !zero_var_rural]

pca_common <- prcomp(pca_common, scale. = TRUE, center = TRUE)
pca_urban <- prcomp(pca_input_urban, scale. = TRUE, center = TRUE)
pca_rural <- prcomp(pca_input_rural, scale. = TRUE, center = TRUE)

summary(pca_urban)
summary(pca_rural)
summary(pca_common)

wealth_index$score_common <- pca_common$x[, 1]

wealth_urban$score_urban <- pca_urban$x[, 1]
wealth_rural$score_rural <- pca_rural$x[, 1]

wealth_urban <- wealth_urban %>%
  left_join(wealth_index %>% select(hhid, score_common), by = "hhid")

wealth_rural <- wealth_rural %>%
  left_join(wealth_index %>% select(hhid, score_common), by = "hhid")

model_urb <- lm(score_common ~ score_urban, data = wealth_urban)
model_rur <- lm(score_common ~ score_rural, data = wealth_rural)

urb_alpha <- coef(model_urb)[1] 
urb_beta  <- coef(model_urb)[2] 

rur_alpha <- coef(model_rur)[1] 
rur_beta  <- coef(model_rur)[2] 

wealth_urban <- wealth_urban %>%
  mutate(final_wealth_score = urb_alpha + (urb_beta * score_urban))

wealth_rural <- wealth_rural %>%
  mutate(final_wealth_score = rur_alpha + (rur_beta * score_rural))

final_wealth_dataset <- bind_rows(
  wealth_urban %>% select(hhid, urban_rural, final_wealth_score),
  wealth_rural %>% select(hhid, urban_rural, final_wealth_score)
)

final_wealth_dataset <- final_wealth_dataset %>%
  mutate(
    quintile = ntile(final_wealth_score, 5)
  )

head(final_wealth_dataset)

final_wealth_dataset <- final_wealth_dataset %>%
  mutate(
    wealth_quintile = ntile(final_wealth_score, 5),
    wealth_quintile = factor(
      wealth_quintile,
      levels = 1:5,
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    )
  )

wealth_rural <- wealth_rural %>%
  mutate(
    wealth_score = pca_rural$x[, 1],
    wealth_quintile = ntile(wealth_score, 5),
    wealth_quintile = factor(
      wealth_quintile,
      levels = 1:5,
      labels = c("Rural Poorest", "Rural Poorer", "Rural Middle", "Rural Richer", "Rural Richest")
    )
  )

wealth_index <- bind_rows(
  wealth_urban,
  wealth_rural
)

ggplot(wealth_index, aes(x = wealth_score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "Distribution of Household Wealth Index Scores",
    x = "Wealth Score",
    y = "Density"
  ) +
  theme_minimal()

ggplot(wealth_index, aes(x = wealth_quintile, y = wealth_score, fill = wealth_quintile)) +
  geom_boxplot() +
  labs(
    title = "Wealth Scores by Quintile",
    x = "Wealth Quintile",
    y = "Wealth Score"
  ) +
  theme_minimal() +
  guides(fill = "none")

section0 <- merge(
  section0,
  wealth_index[,c ("hhid", "wealth_score")],
  by = "hhid"
)

section2a1 <- section2a1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

section2a1 <- merge(
  section2a1,
  wealth_index[,c ("hhid", "wealth_score")],
  by = "hhid"
)

section2a2 <- merge(
  section2a2,
  wealth_index[,c ("hhid", "wealth_score")],
  by = "hhid"
)

section2a3 <- merge(
  section2a3,
  wealth_index[,c ("hhid", "wealth_score")],
  by = "hhid"
)

section2b <- merge(
  section2b,
  wealth_index[,c ("hhid", "wealth_score")],
  by = "hhid"
)

rm(
  wealth_index, wealth_rural, wealth_urban, assets, 
  land_ownership, livestock_ownership, pca_input_rural,
  pca_input_urban
)

dir.create("clean_data2", showWarnings = FALSE, recursive = TRUE)

df_names <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]

for (nm in df_names) {
  df <- get(nm)
  df <- haven::zap_widths(df)
  
  write_dta(
    df,
    file.path("clean_data2", paste0(nm, ".dta"))
  )
}

food_grain <- section3a %>%
  filter(v301 == 1)

food_grain <- food_grain %>%
  mutate(
    across(v303:v305, ~ na_if(.x, 0))
  )

food_grain <- food_grain %>%
  mutate(
    total_grains = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    )
  )

q_bounds <- quantile(food_grain$total_grains, probs = c(0.05, 0.95), na.rm = TRUE)

food_grain_adjusted <- food_grain %>%
  filter(
    total_grains > 250,
    total_grains < 1650
  )

lentils <- section3a %>%
  filter(v301 == 2)

lentils <- lentils %>%
  mutate(
    total_lentils = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    )
  )
