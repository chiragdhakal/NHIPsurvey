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

#SECTION1A

section1a <- section1a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),

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
      TRUE ~ v106
    ),
    v107  = as.numeric(gsub("[^0-9]", "", v107))
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
  filter(personid != 5952899)

section1a <- section1a %>%
  group_by(hhid, v102) %>%
  slice(1) %>%
  ungroup()

#SECTION1B

for (i in setdiff(1:ncol(section1b), c(2, 7, 8, 21, 22, 23))) {
  section1b[[i]] <- as.numeric(section1b[[i]])
}

section1b <- section1b %>%
  mutate(
    v111 = if_else(
      if_any(v112a:v112h, ~ !is.na(.)),
      1L,
      v111
    ),
    v111 = if_else(
      if_any(v112a:v112h, ~ is.na(.)),
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

mean_v202 <- mean(section2a1$v202[section2a1$v202 <= 15], na.rm = TRUE) 

section2a1 <- section2a1 %>%
  mutate(
    v201 = if_else(
      is.na(v201), 
      1, 
      v201
    ), 
    v202 = if_else(
      v202 > 15,
      mean_v202, 
      v202
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
        " BAMBOO", "JASTA KO", "JASTAPATA", "JASTA", "JASTA"
      ) ~ 4, 
      v203a %in% c(
        "BLOCK", "FALAM CEMENT", "FALAM RA CEMENT", "IRON"
      ) ~ 3, 
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
        "ALBESTER", " ALBESTER"
       ) ~ 1,
       v205a %in% c(
        "SIMETKO TALI"
       ) ~ 2, 
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
    )
  )

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
    v211 = if_else(
      !is.na(v212), 
      1, 
      v211
    ), 
    v213a = if_else(v211 == 1 & !is.na(v212), NA_real_, v213a),
    v213a = case_when(
      v213b %in% c(
      " AAFANTA LE DINU BHAKO", " QUARTER IN COMPANY", " QUARTER PROVIDED BY COMPANY",
      " SAWSYASASTHAKO LE DIYAKO ROOM", " KAM GARE BAPAT KO ACCOMODATIONS HOTEL LE UPLABDH", 
      " COMPANY'S ROOM", " COMPANY PROVIDED", " OFFICE RESIDENCE", " QUARTER MA"
      ) ~ 2,
      v213b %in% c(
      " GHAR KO ROOM MATRA VADA MA LEYAKO", " ROOM EUTA LEKO", " EUTA ROOM LEYEKO", 
      " FLAT VADA MA LEYEKO", " 96", " SAJHEDARI MA BASEKO", " GHAR KO 2 TA ROOM VADA MA LEYEKO",
      " TWO ROOM RENT MA LEYEKO", " 1UTA ROOM VADA MA LEYEKO", " EUTA ROOM LEKO", " FLAT LEYEKO",
      " JAGGA LEEJ MA LIYERA AFAILE TEMPORARY TAHARA HALERA BASEKO RA ANNUALLY 32000 RUPAYA BUJHAUNE GAREKO", 
      " AFNO GHAR BANAI RAKHEKO BHAYERA KAILA PAISA TIRNEY KAILA NATIRNEY", " LAND KO RENT TIREKO RA AAFULIE TESMA GHAR BANAYEKO",
      " EUTA ROOM RENT MA", " FLAT MA LEYEKO", " 2 TA ROOM LEYEKO", " GHAR MA BASEKO BAAPAT KAKA LAI KHETI PAATI KO 50 DINU PARNEY",
      " BANDAKI LIYEKO 450000 3YRS CHODA SABAI PAISSA RETURN HUNCHA", " RENT MA BASEKO", " 1 ROOM RENT MA LEYEKO",
      " HOSTEL", " RENT MA BASEKO", " 1 ROOM MA BASEKO", " 2 ROOM RENT MA", " HOSTEL MA BASEKO", " FLAT RENT MA LEYAKO"
      ) ~ 1,
      v208 == 2 & is.na(v213a) ~ 2,
      v213a == 96 ~ 1,
      TRUE ~ v213a
    ),
    v213b = if_else(v211 == 1 & !is.na(v212), NA_character_, v213b),
    v214 = if_else(v211 == 1 & !is.na(v212), NA_real_, v214),
    v215 = if_else(v211 == 1 & !is.na(v212), NA_real_, v215),
    v214 = if_else(v214 > 100000, v214/100, v214),
    v215 = if_else(v215 > 100000, v215/100, v215),
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
      is.na(v223), 
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
  select(-v227h_1, everything(), v227h_1)

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
    )
  )

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
      ID == 9092 ~ 6000000, 
      TRUE ~ v403a
    ),
    v403b = case_when(
      ID == 4581 ~ 1200,
      ID == 3058 ~ 10000, 
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

#SECTION4D

for (i in setdiff(1:ncol(section4d), c(2, 7, 8))) {
  section4d[[i]] <- as.numeric(gsub("[^0-9]", "", section4d[[i]]))
}

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
      mean(v601a, na.rm = TRUE), 
      v601a
    ), 
    v601b = if_else(
      is.na(v601b), 
      mean(v601b, na.rm = TRUE), 
      v601b
    ), 
    
  )

  


  
  
