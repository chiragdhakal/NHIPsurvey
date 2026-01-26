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

mean_v202 <- round(mean(section2a1$v202[section2a1$v202 <= 15], na.rm = TRUE))

section2a1 <- section2a1 %>%
  mutate(
    v201 = if_else(
      is.na(v201), 
      1, 
      v201
    ), 
    v202 = if_else(
      v202 > 15 | is.na(v202),
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
    ), 
    v260 = case_when(
      v260 == 96 & v260a == "" ~ 9,
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
    )
  ) %>%
  ungroup()

section6b1 <- section6b1 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  ) %>%
  group_by(disease_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disease_id, -v104a)

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
  ) 

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
  left_join(
    section6b1 %>% select(disease_id, v604),
    by = "disease_id",
    suffix = c("", "_from_b1")
  ) 


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
    v907a_raw = v907a, 
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
                   "UHA KO NAM MA JAGGA  XORA XUTIYEKO HUNA LEY KHET BARI UTA GARNU HUNXA RA TES BAPAT KEI KHANEY ANNA DAL HARU DINEY"
                   ) ~ 0,

      v907a %in% c("100") ~ 100,
      v907a %in% c("150") ~ 150,
      v907a %in% c("1 MURI TORI") ~ 300,
      v907a %in% c("500") ~ 500,
      v907a %in% c("KHADHAYAN BALI 15 KG") ~ 600,
      v907a %in% c("KHADHYAN BALI 20 KG") ~ 800,
      v907a %in% c("1", "1000") ~ 1000,
      v907a %in% c("30KG", "1200") ~ 1200,
      v907a %in% c("8 PAATHI DHAAN") ~ 1280,
      v907a %in% c("1300") ~ 1300,
      v907a %in% c("1400") ~ 1400,
      v907a %in% c("MILLET (RS1500)", "1500") ~ 1500,
      v907a %in% c("1600") ~ 1600,
      v907a %in% c("1800") ~ 1800,
      v907a %in% c("GADAUDI 2000", "SAG PAT TARKARI UBJAU MATRA DINE 2000", "2000") ~ 2000,
      v907a %in% c("60 KG", "2400") ~ 2400,
      v907a %in% c("2500") ~ 2500,
      v907a %in% c("MILLET (10 PATHI)3000", "30", "3000") ~ 3000,
      v907a %in% c("3500") ~ 3500,
      v907a %in% c("1 MAN MAKAI 20 KG BHATMAS", "3600") ~ 3600,
      v907a %in% c("CHAMAL 50 KG DAL 15 KG") ~ 3800,
      v907a %in% c("3900 CASH RECEIVED") ~ 3900,
      v907a %in% c("1 QUENTAL DHAN", "100 KILO DHAN GAU", "1 KUNTAL GAHU", 
                   "1 QUENTAL GAHU", "40", "4000", "SAAG") ~ 4000,
      v907a %in% c("4500") ~ 4500,
      v907a %in% c("4800") ~ 4800,
      v907a %in% c("5000 MOHIKHETKO LAGI DIYAKO", "GAU", "5", "50", "5000") ~ 5000,
      v907a %in% c("5400") ~ 5400,
      v907a %in% c("5500") ~ 5500,
      v907a %in% c("20KG AALU", "DHAN 1 KUNTAL GHEHU 50KG", "GAHU150KG", 
                   "DHAN 150KG", "6000") ~ 6000,
      v907a %in% c("6150") ~ 6150,
      v907a %in% c("6350") ~ 6350,
      v907a %in% c("2 MURI", "4 MAN DHAN DINE GARXAN") ~ 6400,
      v907a %in% c("6500") ~ 6500,
      v907a %in% c("RS.7000", "7000") ~ 7000,
      v907a %in% c("7500") ~ 7500,
      v907a %in% c("200 KG KHADHAN", "3 MURI DHAN", "DHAN 200KG", 
                   "DHANN 8000", "KHADHAN BALI 200KG", "8000", 
                   "2 KUNTAL DHAN PAKO THIYE", "2 QUINTLE") ~ 8000,
      v907a %in% c("DHAM 3 MURI", "2 QUENTAL DHAN", "2 QUENTEL DHAN", 
                   "2 QUENTAL 25 KG DHAN MATRA DIYAKO KHARCHA K HI DINA NAPARNE", 
                   "9000") ~ 9000,
      v907a %in% c("MAKAI 4MURI", "DHAN 4  MAN GEHU 2MAN") ~ 9600,

      v907a %in% c("OVERALL 1.5 QUINTAL VEGETABLE", "MAKAI", "10000") ~ 10000,
      v907a %in% c("DHAN 2 MASURI 20 KG") ~ 10400,
      v907a %in% c("3 QUENTAL DHAN") ~ 10884,
      v907a %in% c("11000") ~ 11000,
      v907a %in% c("11700") ~ 11700,
      v907a %in% c("12000(DHAN)", "12000DHAN", "8 MAN DHAN", "12000", "3 QUINTAL DHAN") ~ 12000,
      v907a %in% c("12500") ~ 12500,
      v907a %in% c("12600") ~ 12600,
      v907a %in% c("DHAN4 MURI", "12800") ~ 12800,
      v907a %in% c("13000") ~ 13000,
      v907a %in% c("13333") ~ 13333,
      v907a %in% c("13500") ~ 13500,
      v907a %in% c("14000 KO DHAN", "14000") ~ 14000,
      v907a %in% c("14400") ~ 14400,
      v907a %in% c("15000 KO DHAN", "15000(KODO)(MILLET)", "DHAN,GAHU", 
                   "RS.15000 PAID FOR LAND LEASE", "15000") ~ 15000,
      v907a %in% c("1 QUINTAL GAHU 10KG TORI 2 QUINTAL DHAN") ~ 15500,
      v907a %in% c("15600") ~ 15600,
      v907a %in% c("4 QUINTLE", "DHAN 4QUENTAL", "DHAN 5 MURI", "5 MURI DHAN", "10 MAN", "16000") ~ 16000,
      v907a %in% c("17000") ~ 17000,
      v907a %in% c("17500") ~ 17500,
      v907a %in% c("18200") ~ 18200,
      v907a %in% c("18600") ~ 18600,
      v907a %in% c("6 MURI", "6 MURI DHAN", "19000") ~ 19000,
      v907a %in% c("19200") ~ 19200,
      v907a %in% c("18 MAN DHAN", "20000(DHAN)", "5 QUENTAL DHAN", "5 QUENTEL", 
                   "500KG KHADHYAN", "6 QUENTEL", "DHAN", "DHAN 5 QUENTAL", 
                   "DHAN 5 QUENTEL", "DHAN 500KG", "DHAN 5QU", 
                   "5 KUNTAL DHAN", "5 QUINTAL DHAN", "20000", "2") ~ 20000,
      v907a %in% c("DHAN 10MAN GAHU3MAN") ~ 20800,
      v907a %in% c("21000 DHAN", "21000") ~ 21000,
      v907a %in% c("21600") ~ 21600,
      v907a %in% c("22000 TIRAYKO", "7  MURI DHAN PAYAKO", "7 MURI DHAN", 
                   "DHAN 6 QUENTAL", "DHAN 6 QUENTEL", "22000 DAM KO ANNA BALI", 
                   "22000") ~ 22000,
      v907a %in% c("22150") ~ 22150,
      v907a %in% c("7MURI") ~ 22400,
      v907a %in% c("22500") ~ 22500,
      v907a %in% c("20 MAN DHAN DINU PAR XA", "23000") ~ 23000,
      v907a %in% c("23100") ~ 23100,
      v907a %in% c("15 MAN", "6 KUNTAL DHAN", "7.5 MURI", "7.5MURI DHAN", 
                   "15 MAN DIYAKO", "6 QUINTLE", "DHAN 10 MN GEHU 5 MN", "24000") ~ 24000,
      v907a %in% c("24500") ~ 24500,
      v907a %in% c("20  MAN DHAN", "20  MAN DHAN DIYEKO", "25000(DHAN)", 
                   "RICE", "25000") ~ 25000,
      v907a %in% c("8 MURI", "8 MURI DHAN", "25600") ~ 25600,
      v907a %in% c("26000") ~ 26000,
      v907a %in% c("26250") ~ 26250,
      v907a %in% c("27000") ~ 27000,
      v907a %in% c("DHAN 10 MAN GEHU 5 MAN DAL 30 KG") ~ 27600,
      v907a %in% c("15 MURI DHAN 27750") ~ 27750,
      v907a %in% c("12 MURI", "7 QUINTEL GAHU PAYEKO", "28000") ~ 28000,
      v907a %in% c("9 MURI", "28800") ~ 28800,
      v907a %in% c("29500") ~ 29500,
      v907a %in% c("30000 DHAN", "AALU", "AALU ", "DHAN ", "DHAN 8", "30000", 
                   "30000 YO GOVERNMENT KO JAGGA HO TEI NI ARULAU THEKKA MAA DINU BHAKO CHA") ~ 30000,
      v907a %in% c("31500") ~ 31500,
      v907a %in% c("31800") ~ 31800,
      v907a %in% c("10 MURI", "20MAN DHAN", "8 KUNTAL", "8QU", "DHAN 20", "32000") ~ 32000,
      v907a %in% c("33000") ~ 33000,
      v907a %in% c("34000") ~ 34000,
      v907a %in% c("34400") ~ 34400,
      v907a %in% c("34900") ~ 34900,
      v907a %in% c("10 QUINTAL DHAN", "35000 (DHAN)", "35000") ~ 35000,
      v907a %in% c("9  QUINTLE", "9 QUINTLE", "36000") ~ 36000,
      v907a %in% c("36450") ~ 36450,
      v907a %in% c("37000") ~ 37000,
      v907a %in% c("12MURI DHAN", "38000") ~ 38000,
      v907a %in% c("DHAN 12 MURI", "38400") ~ 38400,
      v907a %in% c("38500") ~ 38500,
      v907a %in% c("39000 DHAN KO", "39000") ~ 39000,
      v907a %in% c("39200") ~ 39200,
      v907a %in% c("100000 DHAN", "DHAN 10 KUNTAL GHEHU 4 KUNTAL", "DHAN 10 QUINTAL", 
                   "DANN 10QUENTEL", "25 MAN", "10QU", "DHAN10 KUNTAL", 
                   "GAHU 10 QUENTEL", "40000", 
                   "5 BARSA KO LAGI 2 LAKH LIYARA BANDHAKI RAKHEKO RA TYO KHET KO UBJANI. SABAI UNIHARU LE NAI KHANE GARERA DIYAKO  JAHILE 2LAKH TIRINX TYO JAGGA FIRTA HUNE GARI") ~ 40000,
      v907a %in% c("41600", "DHAN 13 MURI PAYAKO") ~ 41600,
      v907a %in% c("12 QUENTAL DHAN KHET GARNE LE NAI SABAI KHARCH BEHORX", "42000") ~ 42000,
      v907a %in% c("42300") ~ 42300,
      v907a %in% c("1.5 QUINTLE MUSTARD RECEIVED.THE LAND WAS GIVEN TO OTHERS IN THE CHAPTER", "42900") ~ 42900,
      v907a %in% c("43900") ~ 43900,
      v907a %in% c("44000") ~ 44000,
      v907a %in% c("45000(DHAN)", "45000") ~ 45000,
      v907a %in% c("45600") ~ 45600,
      v907a %in% c("46000") ~ 46000,
      v907a %in% c("47250") ~ 47250,
      v907a %in% c("17 QUENTEL DHAN", "48000(DHAN)", "DHAN 12 QUENTAL ", "DHAN 15MURI", 
                   "DHAN12", "12 QUINTLE", "DHAN 15 MURI", "48000") ~ 48000,
      v907a %in% c("20 MURI DHAN", "50(MAN DHAN RA MAIZE)(RS 50000)", "DHAN DAAL(RS50000 NEAR KO)", 
                   "DHAN GAHU DAAL (50000)", "25 BORA", "12.5 DHAN QUINTAL", "50000") ~ 50000,

      v907a %in% c("13 QUENTEL", "13QUENTAL DHAN") ~ 52000,
      v907a %in% c("DHAN 55000", "CHAMAL") ~ 55000,
      v907a %in% c("56000") ~ 56000,
      v907a %in% c("58800") ~ 58800,
      v907a %in% c("40 MAN DHAN", "40MAN DHAN", "10 KUNTAL DHAN GAHU 5 KUNTAL GHEHU", 
                   "15 DHAN 3MURI DAL", "60000") ~ 60000,
      v907a %in% c("61000") ~ 61000,
      v907a %in% c("61400") ~ 61400,
      v907a %in% c("61500") ~ 61500,
      v907a %in% c("62500(DHAN)") ~ 62500,
      v907a %in% c("20MURI", "45 MAN", "DHAN25 MAN GEHU 15 MAN", "64000", "DHAN20MURI") ~ 64000,
      v907a %in% c("65000") ~ 65000,
      v907a %in% c("DHAN 12GAHU5(QUENTEL)") ~ 68000,
      v907a %in% c("DHAN 25MURI", "2 QUINTAL TORI", "70000") ~ 70000,
      v907a %in% c("71100") ~ 71100,
      v907a %in% c("72000 (DHAN)", "72000") ~ 72000,
      v907a %in% c("DHAN 40 GEHU 5 MAN 2 MASURI MAN", "DHAN 8 GHEHU 4 MAN") ~ 73600,
      v907a %in% c("75000") ~ 75000,
      v907a %in% c("77000 DHAN") ~ 77000,
      v907a %in% c("79500") ~ 79500,
      v907a %in% c("60 MAN DHAN DIYEKO", "DHAN 20 QU", "80000") ~ 80000,
      v907a %in% c("83600") ~ 83600,
      v907a %in% c("84000") ~ 84000,
      v907a %in% c("53 MAN DHAN") ~ 84800,
      v907a %in% c("85000") ~ 85000,
      v907a %in% c("18DHAN 1.5 MURI DAL") ~ 86400,
      v907a %in% c("80 MAN DHAN DIYEKO", "90000") ~ 90000,
      v907a %in% c("95000") ~ 95000,
      v907a %in% c("30 MURI DHAN DIYEKO", "DHAN 40 GEHU 20", "96000") ~ 96000,
      v907a %in% c("DHAN GAHU 45 QUENTAL", "100000") ~ 100000,
      v907a %in% c("100500") ~ 100500,
      v907a %in% c("104000") ~ 104000,
      v907a %in% c("105000  DHAN KO", "105000") ~ 105000,
      v907a %in% c("DHAN 40 GEHU 20  MAN  MASULI 2MAN") ~ 105600,
      v907a %in% c("27QU") ~ 108000,
      v907a %in% c("DHAN 35 GEHU 21MAN", "110000") ~ 110000,
      v907a %in% c("DHAN 40MAN GEHU 30MAN") ~ 112000,
      v907a %in% c("114000") ~ 114000,
      v907a %in% c("115000") ~ 115000,
      v907a %in% c("120000") ~ 120000,
      v907a %in% c("4500 KG", "125000") ~ 125000,
      v907a %in% c("130000") ~ 130000,
      v907a %in% c("140000") ~ 140000,
      v907a %in% c("144000") ~ 144000,
      v907a %in% c("145000") ~ 145000,
      v907a %in% c("148225") ~ 148225,
      v907a %in% c("150000") ~ 150000,
      v907a %in% c("40 DHAN MAN GEHU 20 MAN TORI 5 MAN") ~ 166000,
      v907a %in% c("167500") ~ 167500,
      v907a %in% c("170000") ~ 170000,
      v907a %in% c("175000") ~ 175000,
      v907a %in% c("180000") ~ 180000,
      v907a %in% c("190000") ~ 190000,
      v907a %in% c("192000") ~ 192000,
      v907a %in% c("200000  PAISA DINU BHAKO CHA TYO RETURN NAGARNE SAMMA KHETI GARI KHANA PAUNU HUNCHA", 
                   "200000 ( 5YEARS KO LAGI LIYEKO RA PAILAI TIREKO )", "50 QUINTLE", 
                   "200000") ~ 200000,
      v907a %in% c("215000") ~ 215000,
      v907a %in% c("216000") ~ 216000,
      v907a %in% c("220000") ~ 220000,
      v907a %in% c("225000") ~ 225000,
      v907a %in% c("240000") ~ 240000,
      v907a %in% c("250000") ~ 250000,
      v907a %in% c("260000") ~ 260000,
      v907a %in% c("275000") ~ 275000,
      v907a %in% c("300000") ~ 300000,
      v907a %in% c("315000") ~ 315000,
      v907a %in% c("350000") ~ 350000,
      v907a %in% c("400000") ~ 400000,
      v907a %in% c("480000") ~ 480000,
      v907a %in% c("500000") ~ 500000,
      v907a %in% c("800000") ~ 800000,
      v907a %in% c("2000000") ~ 2000000,
      v907a %in% c("5000000") ~ 5000000,

      TRUE ~ as.numeric(v907a)
    )
  ) %>%
  select(-v907a_raw) %>% 
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

wealth_index <- section2a1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  ) %>%
  select(hhid, v202, v203, v204, v205, v206)

section2a2 <- section2a2 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

wealth_index <- merge(
  wealth_index, 
  section2a2[, c("hhid", "v208", "v213")],
  by = "hhid"
)

section2a3 <- section2a3 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

wealth_index <- merge(
  wealth_index, 
  section2a3[, c("hhid", "v216", "v223", "v225", "v218", "v220", "v222a", "v222b", "v222c")], 
  by = "hhid"
)

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
  mutate(
    hhld_member_t = as.numeric(hhld_member_t),
    hhld_member_t = if_else(is.na(hhld_member_t), 11, hhld_member_t),
    rooms_per_capita = v202 / hhld_member_t, 
  ) %>%
  rename(
    dwelling_ownership = v208,
    foundation_improved = v203, 
    wall_improved = v204, 
    roof_improved = v205, 
    floor_improved = v206, 
    owner_occupancy = v213, 
    improved_water = v216, 
    improved_toilet = v225, 
    improved_waste = v223, 
    clean_cooking_fuel = v218, 
    electricity_access = v220, 
    phone_access = v222a,
    internet_access = v222c, 
    tv_access = v222b,
    land_ownership = v901, 
    livestock_ownership = v934     
  ) %>%
  mutate(
    dwelling_ownership = if_else(
      dwelling_ownership == 2, 0, 1
    ),
    foundation_improved = case_when(
      foundation_improved %in% c(2, 3) ~ 1,
      TRUE ~ 0
    ),
    wall_improved = case_when(
      wall_improved %in% c(2, 6) ~ 1, 
      TRUE ~ 0
    ),
    roof_improved = case_when(
      roof_improved %in% c(1, 2, 3, 4) ~ 1,
      TRUE ~ 0
    ),
    floor_improved = case_when(
      floor_improved %in% c(2, 3, 5) ~ 1, 
      TRUE ~ 0
    ),
    improved_water = case_when(
      improved_water %in% c(1, 2, 3, 4, 8, 9) ~ 1,
      TRUE ~ 0
    ),
    improved_toilet = case_when(
      improved_toilet %in% c(1, 2) ~ 1, 
      TRUE ~ 0
    ),
    improved_waste = case_when(
      improved_waste %in% c(1, 2) ~ 1, 
      TRUE ~ 0
    ),
    phone_access = if_else(
      phone_access == 2, 0, 1
    ),
    tv_access = if_else(
      tv_access == 2, 0, 1
    ),
    internet_access = if_else(
      internet_access == 2, 0, 1
    ),
    clean_cooking_fuel = case_when(
      clean_cooking_fuel %in% c(2, 3, 6) ~ 1, 
      TRUE ~ 0
    ),
    electricity_access = case_when(
      electricity_access %in% c(1, 2) ~ 1, 
      TRUE ~ 0
    ),
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
    ),
  ) %>%
  select(-v202, -hhld_member_t, -owner_occupancy)

pca_input <- wealth_index %>%
  select(-hhid)

pca_input <- pca_input %>%
  select(where(~ var(.) != 0))

pca_result <- prcomp(pca_input, scale. = TRUE, center = TRUE)

summary(pca_result)

wealth_index <- wealth_index %>%
  mutate(
    wealth_score = pca_result$x[, 1]
  )

wealth_index <- wealth_index %>%
  mutate(
    wealth_quintile = factor(
      wealth_quintile,
      levels = 1:5,
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    )
  )
