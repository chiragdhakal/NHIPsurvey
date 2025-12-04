if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(openxlsx)
library(writexl)

section0 <- read.xlsx("dataset/cover page.xlsx")
section1a <- read.xlsx("dataset/section 1.xlsx")
section1b <- read.xlsx("dataset/Part 1_1 Household Roster-1.xlsx")
section2a1 <- read.xlsx("dataset/Household Characteristics.xlsx")
section2a2 <- read.xlsx("dataset/Section 2_1 Housing Expenses.xlsx")
section2a3 <- read.xlsx("dataset/Utilities and Amenities.xlsx")
section2b <- read.xlsx("dataset/Section 2_2_ National health insurence.xlsx")
section2c <- read.xlsx("dataset/PART 2_3_ Mortality (Death) Information.xlsx")
section3a <- read.xlsx("dataset/Section 3_ Consumption of Food.xlsx")
section3b <- read.xlsx("dataset/Part 3_1_ Food away from home.xlsx")
section4a <- read.xlsx("dataset/section 4.xlsx")
section4b <- read.xlsx("dataset/Part 4_2_ Expenditure Abroad.xlsx")
section4c <- read.xlsx("dataset/Part 4_3_ Inventory of Durable Goods.xlsx")
section4d <- read.xlsx("dataset/Part 4_4_ Own Account Consumption of Goods.xlsx")
section5 <- read.xlsx("dataset/Section 5_ Expense in Education.xlsx")
section6a <- read.xlsx("dataset/section 6.xlsx")
section6b1 <- read.xlsx("dataset/Part 6_2_1_ Chronic Illness and Health Seeking Behaviour.xlsx")
section6b2 <- read.xlsx("dataset/Part 6_2_2_ Chronic Illness and Expenditure Tracking.xlsx")
section6b3 <- read.xlsx("dataset/Part 6_2_3_ Chronic Illness and Expenditure Tracking – Outpatient (Regular Checkups).xlsx")
section6b4 <- read.xlsx("dataset/Part 6_2_4_ Chronic Illness and Expenditure Tracking – Inpatient.xlsx")
section6b5 <- read.xlsx("dataset/section 6_2_5.xlsx")
section6c1 <- read.xlsx("dataset/Part 6_3_1_ Acute Illness and health seeking behaviour.xlsx")
section6c2 <- read.xlsx("dataset/Part 6.3.2_ Acute illness and health screening.xlsx")
section6c3 <- read.xlsx("dataset/Part 6_3_3_ Acute Illness health seeking and expenditure tracking.xlsx")
section6c4 <- read.xlsx("dataset/Part 6_3_4_ Acute Illness health seeking and expenditure tracking.xlsx")
section6d <- read.xlsx("dataset/PART 6_4_ Household Health Care Seeking.xlsx")
section7 <- read.xlsx("dataset/Swction 7_ Labor and Employment.xlsx")
section8 <- read.xlsx("dataset/Section 8_ Wage Jobs.xlsx")
section9a <- read.xlsx("dataset/section 9.xlsx")
section9b <- read.xlsx("dataset/Part 9_2_ Landholding  Increase Decrease.xlsx")
section9c <- read.xlsx("dataset/Part 9_3_ Production and Uses.xlsx")
section9d <- read.xlsx("dataset/Part 9_4_ Expenditure.xlsx")
section9e <- read.xlsx("dataset/Part 9_5_ Livestock.xlsx")
section9f1 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure.xlsx")
section9f2 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure (1).xlsx")
section10 <- read.xlsx("dataset/Income from Non - Agricultural Enterprises.xlsx")
section11a <- read.xlsx("dataset/section 11.xlsx")
section11b <- read.xlsx("dataset/Part 11_2_ Lending and Outstanding Loans.xlsx")
section11c <- read.xlsx("dataset/Part 11_3_ Other Assets.xlsx")
section12a <- read.xlsx("dataset/Remittance and transfer.xlsx")
section12b <- read.xlsx("dataset/Part 12_2. Other Remittances.xlsx")
section13a <- read.xlsx("dataset/section 13.xlsx")
section13b <- read.xlsx("dataset/Part 13_2_ Social Assistance.xlsx")
section13c <- read.xlsx("dataset/Part 13_3_ Other Income.xlsx")

#COUNTING NUMBER OF HOUSEHOLDS IN EACH PSU

psu_counts <- section0 %>%
  group_by(psu) %>%
  summarise(n_hhlds = n()) %>%
  ungroup()

NHIP_insured <- c(1101:1112, 2101:2110, 3101:3115, 4101:4109, 5101:5111, 6101:6108, 7101:7109)
NHIP_non_insured <- c(1201:1212, 2201:2210, 3201:3215, 4201:4209, 5201:5211, 6201:6208, 7201:7209)
SSF_insured <- c(1301:1318, 2301:2315, 3301:3390, 4301:4322, 5301:5323, 6301:6316, 7301:7311)
SSF_non_insured <- c(1401:1418, 2401:2415, 3401:3490, 4401:4422, 5401:5423, 6401:6416, 7401:7411)

total_interviewed <- psu_counts %>%
  mutate(
    category = case_when(
      psu %in% NHIP_insured    ~ "NHIP_insured",
      psu %in% NHIP_non_insured ~ "NHIP_non_insured",
      psu %in% SSF_insured     ~ "SSF_insured",
      psu %in% SSF_non_insured ~ "SSF_non_insured",
      TRUE                     ~ "Other"
    )
  ) %>%
  group_by(category) %>%
  summarise(total_hhlds = sum(n_hhlds))


#VALIDATING PALIKA AND DISTRICT FOR NHIP AND NON NHIP

nhip_check_geoinputs <- section0 %>%
  filter(
    #KOSHI
    (psu %in% c(1101, 1201) & !(province == 1 & district == 102 & palika == 10207)) |
    (psu %in% c(1102, 1202) & !(province == 1 & district == 103 & palika == 10304)) |
    (psu %in% c(1103, 1203) & !(province == 1 & district == 106 & palika == 10602)) |
    (psu %in% c(1104, 1204) & !(province == 1 & district == 108 & palika == 10804)) |
    (psu %in% c(1105, 1205) & !(province == 1 & district == 108 & palika == 10805)) |
    (psu %in% c(1106, 1206) & !(province == 1 & district == 110 & palika == 11005)) |
    (psu %in% c(1107, 1207) & !(province == 1 & district == 111 & palika == 11102)) |
    (psu %in% c(1108, 1208) & !(province == 1 & district == 112 & palika == 11211)) |
    (psu %in% c(1109, 1209) & !(province == 1 & district == 112 & palika == 11214)) |
    (psu %in% c(1110, 1210) & !(province == 1 & district == 113 & palika == 11303)) |
    (psu %in% c(1111, 1211) & !(province == 1 & district == 113 & palika == 11306)) |
    (psu %in% c(1112, 1212) & !(province == 1 & district == 113 & palika == 11309)) |
    #MADHESH
    (psu %in% c(2101, 2201) & !(province == 2 & district == 201 & palika == 20110)) |
    (psu %in% c(2102, 2202) & !(province == 2 & district == 201 & palika == 20111)) |
    (psu %in% c(2103, 2203) & !(province == 2 & district == 202 & palika == 20206)) |
    (psu %in% c(2104, 2204) & !(province == 2 & district == 202 & palika == 20214)) |
    (psu %in% c(2105, 2205) & !(province == 2 & district == 203 & palika == 20302)) |
    (psu %in% c(2106, 2206) & !(province == 2 & district == 205 & palika == 20514)) |
    (psu %in% c(2107, 2207) & !(province == 2 & district == 206 & palika == 20613)) |
    (psu %in% c(2108, 2208) & !(province == 2 & district == 206 & palika == 20614)) |
    (psu %in% c(2109, 2209) & !(province == 2 & district == 207 & palika == 20703)) |
    (psu %in% c(2110, 2210) & !(province == 2 & district == 208 & palika == 20807)) |
    #BAGMATI
    (psu %in% c(3101, 3201) & !(province == 3 & district == 301 & palika == 30102)) |
    (psu %in% c(3102, 3202) & !(province == 3 & district == 302 & palika == 30207)) |
    (psu %in% c(3103, 3203) & !(province == 3 & district == 302 & palika == 30212)) |
    (psu %in% c(3104, 3204) & !(province == 3 & district == 305 & palika == 30512)) |
    (psu %in% c(3105, 3205) & !(province == 3 & district == 306 & palika == 30601)) |
    (psu %in% c(3106, 3206) & !(province == 3 & district == 306 & palika == 30604)) |
    (psu %in% c(3107, 3207) & !(province == 3 & district == 306 & palika == 30608)) |
    (psu %in% c(3108, 3208) & !(province == 3 & district == 308 & palika == 30802)) |
    (psu %in% c(3109, 3209) & !(province == 3 & district == 308 & palika == 30803)) |
    (psu %in% c(3110, 3210) & !(province == 3 & district == 309 & palika == 30909)) |
    (psu %in% c(3111, 3211) & !(province == 3 & district == 311 & palika == 31101)) |
    (psu %in% c(3112, 3212) & !(province == 3 & district == 312 & palika == 31206)) |
    (psu %in% c(3113, 3213) & !(province == 3 & district == 313 & palika == 31301)) |
    (psu %in% c(3114, 3214) & !(province == 3 & district == 313 & palika == 31303)) |
    (psu %in% c(3115, 3215) & !(province == 3 & district == 313 & palika == 31304)) |
    #GANDAKI
    (psu %in% c(4101, 4201) & !(province == 4 & district == 402 & palika == 40204)) |
    (psu %in% c(4102, 4202) & !(province == 4 & district == 405 & palika == 40504)) |
    (psu %in% c(4103, 4203) & !(province == 4 & district == 406 & palika == 40602)) |
    (psu %in% c(4104, 4204) & !(province == 4 & district == 406 & palika == 40605)) |
    (psu %in% c(4105, 4205) & !(province == 4 & district == 407 & palika == 40704)) |
    (psu %in% c(4106, 4206) & !(province == 4 & district == 408 & palika == 40803)) |
    (psu %in% c(4107, 4207) & !(province == 4 & district == 408 & palika == 40806)) |
    (psu %in% c(4108, 4208) & !(province == 4 & district == 410 & palika == 41003)) |
    (psu %in% c(4109, 4209) & !(province == 4 & district == 410 & palika == 41007)) |
    #LUMBINI
    (psu %in% c(5101, 5201) & !(province == 5 & district == 504 & palika == 50403)) |
    (psu %in% c(5102, 5202) & !(province == 5 & district == 506 & palika == 50601)) |
    (psu %in% c(5103, 5203) & !(province == 5 & district == 506 & palika == 50609)) |
    (psu %in% c(5104, 5204) & !(province == 5 & district == 507 & palika == 50703)) |
    (psu %in% c(5105, 5205) & !(province == 5 & district == 509 & palika == 50901)) |
    (psu %in% c(5106, 5206) & !(province == 5 & district == 509 & palika == 50903)) |
    (psu %in% c(5107, 5207) & !(province == 5 & district == 510 & palika == 51007)) |
    (psu %in% c(5108, 5208) & !(province == 5 & district == 510 & palika == 51008)) |
    (psu %in% c(5109, 5209) & !(province == 5 & district == 511 & palika == 51105)) |
    (psu %in% c(5110, 5210) & !(province == 5 & district == 511 & palika == 51106)) |
    (psu %in% c(5111, 5211) & !(province == 5 & district == 511 & palika == 51108)) |
    #KARNALI
    (psu %in% c(6101, 6201) & !(province == 6 & district == 603 & palika == 60303)) |
    (psu %in% c(6102, 6202) & !(province == 6 & district == 604 & palika == 60404)) |
    (psu %in% c(6103, 6203) & !(province == 6 & district == 606 & palika == 60602)) |
    (psu %in% c(6104, 6204) & !(province == 6 & district == 606 & palika == 60608)) |
    (psu %in% c(6105, 6205) & !(province == 6 & district == 607 & palika == 60703)) |
    (psu %in% c(6106, 6206) & !(province == 6 & district == 608 & palika == 60806)) |
    (psu %in% c(6107, 6207) & !(province == 6 & district == 609 & palika == 60902)) |
    (psu %in% c(6108, 6208) & !(province == 6 & district == 610 & palika == 61004)) |
    #FAR-WEST
    (psu %in% c(7101, 7201) & !(province == 7 & district == 701 & palika == 70103)) |
    (psu %in% c(7102, 7202) & !(province == 7 & district == 702 & palika == 70210)) |
    (psu %in% c(7103, 7203) & !(province == 7 & district == 704 & palika == 70403)) |
    (psu %in% c(7104, 7204) & !(province == 7 & district == 704 & palika == 70410)) |
    (psu %in% c(7105, 7205) & !(province == 7 & district == 706 & palika == 70601)) |
    (psu %in% c(7106, 7206) & !(province == 7 & district == 708 & palika == 70811)) |
    (psu %in% c(7107, 7207) & !(province == 7 & district == 708 & palika == 70813)) |
    (psu %in% c(7108, 7208) & !(province == 7 & district == 709 & palika == 70901)) |
    (psu %in% c(7109, 7209) & !(province == 7 & district == 709 & palika == 70909))
  )


#REPLACE PALIKA AND DISTRICT NAME TO NSO CODE
ssf_sample <- read.xlsx("/home/sobaakun/Downloads/SSF_Employer_Roster_Sample.xlsx", sheet = "SSF_Info")
district_codes <- c(
  "Taplejung"=101, "Sankhuwasabha"=102, "Solukhumbu"=103,
  "Okhaldhunga"=104, "Khotang"=105, "Bhojpur"=106,
  "Dhankuta"=107, "Terhathum"=108, "Panchthar"=109,
  "Ilam"=110, "Jhapa"=111, "Morang"=112, "Sunsari"=113,
  "Udayapur"=114, "Saptari"=201, "Siraha"=202, "Dhanusha"=203,
  "Mahottari"=204, "Sarlahi"=205, "Rautahat"=206, "Bara"=207,
  "Parsa"=208, "Dolakha"=301, "Sindhupalchok"=302, "Rasuwa"=303,
  "Dhading"=304, "Nuwakot"=305, "Kathmandu"=306, "Bhaktapur"=307,
  "Lalitpur"=308, "Kavrepalanchok"=309, "Ramechhap"=310, "Sindhuli"=311,
  "Makawanpur"=312, "Chitwan"=313, "Gorkha"=401, "Manang"=402,
  "Mustang"=403, "Myagdi"=404, "Kaski"=405, "Lamjung"=406,
  "Tanahu"=407, "Nawalparasi East"=408, "Syangja"=409, "Parbat"=410,
  "Baglung"=411, "Rukum East"=501, "Rolpa"=502, "Pyuthan"=503,
  "Gulmi"=504, "Arghakhanchi"=505, "Palpa"=506, "Nawalparasi West"=507,
  "Rupandehi"=508, "Kapilvastu"=509, "Dang"=510, "Banke"=511,
  "Bardiya"=512, "Dolpa"=601, "Mugu"=602, "Humla"=603, "Jumla"=604,
  "Kalikot"=605, "Dailekh"=606, "Jajarkot"=607, "Rukum West"=608,
  "Salyan"=609, "Surkhet"=610, "Bajura"=701, "Bajhang"=702,
  "Darchula"=703, "Baitadi"=704, "Dadeldhura"=705, "Doti"=706,
  "Achham"=707, "Kailali"=708, "Kanchanpur"=709
)

ssf_sample$District <- district_codes[as.character(ssf_sample$District)]

write.csv(ssf_sample, "ssf_sample.csv")

ssf_sample <- ssf_sample %>%
  select(District, Name.of.Local.Level, llcode, Province, Ward, Employer.Name)

ssf_check_geopoints <- section0 %>%
  filter(
    #KOSHI
    psu == 1301 & !(province == 1 & district == 102 & palika == 10206 & ward == 9) |
    psu == 1302 & !(province == 1 & district == 110 & palika == 11003 & ward == 9) |
    psu == 1303 & !(province == 1 & district == 112 & palika == 11203 & ward == 10) |
    psu == 1306 & !(province == 1 & district == 111 & palika == 11101 & ward == 10) |
    psu == 1307 & !(province == 1 & district == 111 & palika == 11112 & ward == 5) |
    psu == 1308 & !(province == 1 & district == 113 & palika == 11306 & ward == 8) |
    psu == 1309 & !(province == 1 & district == 113 & palika == 11306 & ward == 9) |
    psu == 1310 & !(province == 1 & district == 110 & palika == 11003 & ward == 9) |
    psu == 1314 & !(province == 1 & district == 113 & palika == 11307 & ward == 8) |
    psu == 1315 & !(province == 1 & district == 112 & palika == 11213 & ward == 2) |
    psu == 1316 & !(province == 1 & district == 111 & palika == 11101 & ward == 6) |
    psu == 1317 & !(province == 1 & district == 113 & palika == 11307 & ward == 8) |
    psu == 1318 & !(province == 1 & district == 112 & palika == 11214 & ward == 7) |
    #MADHESH
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107 & ward == 6) |
    psu == 2302 & !(province == 2 & district == 205 & palika == 20502 & ward == 6) |
    psu == 2303 & !(province == 2 & district == 204 & palika == 20415 & ward == 2) |
    psu == 2304 & !(province == 2 & district == 205 & palika == 20501 & ward == 8) |
    psu == 2305 & !(province == 2 & district == 208 & palika == 20807 & ward == 32) |
    psu == 2306 & !(province == 2 & district == 203 & palika == 20315 & ward == 10) |
    psu == 2307 & !(province == 2 & district == 207 & palika == 20703 & ward == 4) |
    psu == 2308 & !(province == 2 & district == 203 & palika == 20315 & ward == 4) |
    psu == 2309 & !(province == 2 & district == 207 & palika == 20703 & ward == 2) |
    psu == 2310 & !(province == 2 & district == 206 & palika == 20612 & ward == 3) |
    psu == 2311 & !(province == 2 & district == 208 & palika == 20807 & ward == 10) |
    psu == 2312 & !(province == 2 & district == 208 & palika == 20807 & ward == 22) |
    psu == 2313 & !(province == 2 & district == 207 & palika == 20703 & ward == 1) |
    psu == 2314 & !(province == 2 & district == 208 & palika == 20807 & ward == 6) |
    psu == 2315 & !(province == 2 & district == 207 & palika == 20704 & ward == 4) |
    #BAGMATI
    psu == 3301 & !(province == 3 & district == 302 & palika == 30205 & ward == 9) |
    psu == 3302 & !(province == 3 & district == 309 & palika == 30909 & ward == 6) |
    psu == 3303 & !(province == 3 & district == 312 & palika == 31206 & ward == 2) |
    psu == 3304 & !(province == 3 & district == 309 & palika == 30909 & ward == 7) |
    psu == 3305 & !(province == 3 & district == 309 & palika == 30905 & ward == 8) |
    psu == 3306 & !(province == 3 & district == 313 & palika == 31304 & ward == 1) |
    psu == 3307 & !(province == 3 & district == 313 & palika == 31304 & ward == 3) |
    psu == 3308 & !(province == 3 & district == 309 & palika == 30906 & ward == 4) |
    psu == 3309 & !(province == 3 & district == 309 & palika == 30904 & ward == 6) |
    psu == 3310 & !(province == 3 & district == 312 & palika == 31206 & ward == 8) |
    psu == 3311 & !(province == 3 & district == 313 & palika == 31304 & ward == 16) |
    psu == 3312 & !(province == 3 & district == 313 & palika == 31304 & ward == 16) |
    psu == 3313 & !(province == 3 & district == 309 & palika == 30904 & ward == 13) |
    psu == 3314 & !(province == 3 & district == 312 & palika == 31206 & ward == 8) |
    psu == 3315 & !(province == 3 & district == 313 & palika == 31304 & ward == 8) |
    psu == 3316 & !(province == 3 & district == 306 & palika == 30610 & ward == 3) |
    psu == 3317 & !(province == 3 & district == 306 & palika == 30610 & ward == 14) |
    psu == 3318 & !(province == 3 & district == 307 & palika == 30703 & ward == 4) |
    psu == 3319 & !(province == 3 & district == 308 & palika == 30802 & ward == 5) |
    psu == 3320 & !(province == 3 & district == 306 & palika == 30608 & ward == 29) |
    psu == 3321 & !(province == 3 & district == 306 & palika == 30608 & ward == 31) |
    psu == 3322 & !(province == 3 & district == 306 & palika == 30608 & ward == 11) |  
    psu == 3323 & !(province == 3 & district == 306 & palika == 30608 & ward == 12) |
    psu == 3324 & !(province == 3 & district == 306 & palika == 30608 & ward == 13) |
    psu == 3325 & !(province == 3 & district == 306 & palika == 30608 & ward == 16) |
    psu == 3326 & !(province == 3 & district == 306 & palika == 30604 & ward == 8) |
    psu == 3327 & !(province == 3 & district == 306 & palika == 30608 & ward == 1) |
    psu == 3328 & !(province == 3 & district == 306 & palika == 30608 & ward == 6) |
    psu == 3329 & !(province == 3 & district == 306 & palika == 30608 & ward == 6) |
    psu == 3330 & !(province == 3 & district == 306 & palika == 30608 & ward == 10) |
    psu == 3331 & !(province == 3 & district == 306 & palika == 30602 & ward == 7) |
    psu == 3332 & !(province == 3 & district == 306 & palika == 30602 & ward == 9) |
    psu == 3333 & !(province == 3 & district == 306 & palika == 30604 & ward == 7) |
    psu == 3334 & !(province == 3 & district == 306 & palika == 30604 & ward == 4) |
    psu == 3335 & !(province == 3 & district == 306 & palika == 30604 & ward == 12) |
    psu == 3336 & !(province == 3 & district == 306 & palika == 30607 & ward == 3) |
    psu == 3337 & !(province == 3 & district == 306 & palika == 30608 & ward == 10) |
    psu == 3338 & !(province == 3 & district == 306 & palika == 30608 & ward == 29) |
    psu == 3339 & !(province == 3 & district == 306 & palika == 30608 & ward == 30) |
    psu == 3340 & !(province == 3 & district == 306 & palika == 30608 & ward == 32) |
    psu == 3341 & !(province == 3 & district == 307 & palika == 30704 & ward == 4) |
    psu == 3342 & !(province == 3 & district == 308 & palika == 30802 & ward == 5) |
    psu == 3343 & !(province == 3 & district == 308 & palika == 30802 & ward == 10) |
    psu == 3344 & !(province == 3 & district == 308 & palika == 30802 & ward == 14) |
    psu == 3345 & !(province == 3 & district == 306 & palika == 30604 & ward == 11) |
    psu == 3346 & !(province == 3 & district == 306 & palika == 30602 & ward == 3) |
    psu == 3347 & !(province == 3 & district == 306 & palika == 30605 & ward == 10) |
    psu == 3348 & !(province == 3 & district == 306 & palika == 30603 & ward == 9) |
    psu == 3349 & !(province == 3 & district == 306 & palika == 30608 & ward == 3) |
    psu == 3350 & !(province == 3 & district == 306 & palika == 30608 & ward == 5) |
    psu == 3351 & !(province == 3 & district == 306 & palika == 30608 & ward == 9) |
    psu == 3352 & !(province == 3 & district == 306 & palika == 30608 & ward == 10) |
    psu == 3353 & !(province == 3 & district == 306 & palika == 30608 & ward == 29) |
    psu == 3354 & !(province == 3 & district == 306 & palika == 30608 & ward == 30) |
    psu == 3355 & !(province == 3 & district == 308 & palika == 30802 & ward == 5) |
    psu == 3356 & !(province == 3 & district == 308 & palika == 30802 & ward == 5) |
    psu == 3357 & !(province == 3 & district == 308 & palika == 30802 & ward == 8) |
    psu == 3358 & !(province == 3 & district == 308 & palika == 30802 & ward == 10) |
    psu == 3359 & !(province == 3 & district == 308 & palika == 30802 & ward == 13) |
    psu == 3360 & !(province == 3 & district == 308 & palika == 30802 & ward == 14) |
    psu == 3361 & !(province == 3 & district == 308 & palika == 30802 & ward == 22) |
    psu == 3362 & !(province == 3 & district == 308 & palika == 30802 & ward == 10) |
    psu == 3363 & !(province == 3 & district == 308 & palika == 30802 & ward == 1) |
    psu == 3364 & !(province == 3 & district == 307 & palika == 30703 & ward == 3) |
    psu == 3365 & !(province == 3 & district == 307 & palika == 30703 & ward == 3) |
    psu == 3366 & !(province == 3 & district == 306 & palika == 30608 & ward == 29) |
    psu == 3367 & !(province == 3 & district == 306 & palika == 30608 & ward == 28) |
    psu == 3368 & !(province == 3 & district == 306 & palika == 30608 & ward == 22) |
    psu == 3369 & !(province == 3 & district == 306 & palika == 30608 & ward == 11) |
    psu == 3370 & !(province == 3 & district == 306 & palika == 30604 & ward == 8) |
    psu == 3371 & !(province == 3 & district == 306 & palika == 30608 & ward == 1) |
    psu == 3372 & !(province == 3 & district == 306 & palika == 30608 & ward == 7) |
    psu == 3373 & !(province == 3 & district == 306 & palika == 30608 & ward == 32) |
    psu == 3374 & !(province == 3 & district == 306 & palika == 30608 & ward == 11) |
    psu == 3375 & !(province == 3 & district == 306 & palika == 30608 & ward == 29) |
    psu == 3376 & !(province == 3 & district == 308 & palika == 30802 & ward == 16) |
    psu == 3377 & !(province == 3 & district == 307 & palika == 30703 & ward == 2) |
    psu == 3378 & !(province == 3 & district == 307 & palika == 30701 & ward == 4) |
    psu == 3379 & !(province == 3 & district == 308 & palika == 30802 & ward == 1) |
    psu == 3380 & !(province == 3 & district == 308 & palika == 30802 & ward == 2) |
    psu == 3381 & !(province == 3 & district == 308 & palika == 30802 & ward == 2) |
    psu == 3382 & !(province == 3 & district == 308 & palika == 30802 & ward == 3) |
    psu == 3383 & !(province == 3 & district == 308 & palika == 30802 & ward == 15)|  
    #GANDAKI
    psu == 4301 & !(province == 4 & district == 405 & palika == 40504 & ward == 2) |
    psu == 4302 & !(province == 4 & district == 405 & palika == 40504 & ward == 9) |
    psu == 4303 & !(province == 4 & district == 405 & palika == 40504 & ward == 9) |
    psu == 4304 & !(province == 4 & district == 405 & palika == 40504 & ward == 10) |
    psu == 4305 & !(province == 4 & district == 405 & palika == 40504 & ward == 15) |
    psu == 4306 & !(province == 4 & district == 409 & palika == 40903 & ward == 4)|
    psu == 4307 & !(province == 4 & district == 408 & palika == 40803 & ward == 2) |
    psu == 4308 & !(province == 4 & district == 408 & palika == 40808 & ward == 1) |
    psu == 4309 & !(province == 4 & district == 408 & palika == 40808 & ward == 2) |
    psu == 4310 & !(province == 4 & district == 408 & palika == 40801 & ward == 16) |
    psu == 4311 & !(province == 4 & district == 405 & palika == 40504 & ward == 6) |
    psu == 4312 & !(province == 4 & district == 405 & palika == 40504 & ward == 8) |
    psu == 4313 & !(province == 4 & district == 405 & palika == 40504 & ward == 9) |
    psu == 4314 & !(province == 4 & district == 407 & palika == 40710 & ward == 2) |
    psu == 4315 & !(province == 4 & district == 407 & palika == 40702 & ward == 10) |
    psu == 4316 & !(province == 4 & district == 408 & palika == 40801 & ward == 2) |
    psu == 4317 & !(province == 4 & district == 408 & palika == 40806 & ward == 17) |
    psu == 4318 & !(province == 4 & district == 405 & palika == 40504 & ward == 8) |
    psu == 4319 & !(province == 4 & district == 405 & palika == 40504 & ward == 9) |
    psu == 4320 & !(province == 4 & district == 405 & palika == 40504 & ward == 10) |
    psu == 4321 & !(province == 4 & district == 408 & palika == 40805 & ward == 2) |
    psu == 4322 & !(province == 4 & district == 408 & palika == 40801 & ward == 4) |
    #LUMBINI
    psu == 5301 & !(province == 5 & district == 503 & palika == 50305 & ward == 2) |
    psu == 5302 & !(province == 5 & district == 510 & palika == 51002 & ward == 15) |
    psu == 5303 & !(province == 5 & district == 509 & palika == 50901 & ward == 2) |
    psu == 5304 & !(province == 5 & district == 511 & palika == 51103 & ward == 2) |
    psu == 5305 & !(province == 5 & district == 511 & palika == 51104 & ward == 2) |
    psu == 5306 & !(province == 5 & district == 511 & palika == 51106 & ward == 2) |
    psu == 5307 & !(province == 5 & district == 511 & palika == 51102 & ward == 11) |
    psu == 5308 & !(province == 5 & district == 512 & palika == 51208 & ward == 8) |
    #psu == 5309 & !(province == 5 & district == 503 & palika == 50305 & ward == 5) |
    psu == 5310 & !(province == 5 & district == 508 & palika == 50802 & ward == 6) |
    psu == 5311 & !(province == 5 & district == 508 & palika == 50808 & ward == 1) |
    #psu == 5312 & !(province == 5 & district == 508 & palika == 50808 & ward == 7) |
    psu == 5313 & !(province == 5 & district == 508 & palika == 50802 & ward == 6) |
    psu == 5314 & !(province == 5 & district == 508 & palika == 50808 & ward == 5) |
    psu == 5315 & !(province == 5 & district == 509 & palika == 50903 & ward == 1) |
    psu == 5316 & !(province == 5 & district == 511 & palika == 51102 & ward == 14) |
    psu == 5317 & !(province == 5 & district == 511 & palika == 51106 & ward == 18) |
    psu == 5318 & !(province == 5 & district == 508 & palika == 50802 & ward == 9) |
    psu == 5319 & !(province == 5 & district == 508 & palika == 50811 & ward == 1) |
    psu == 5320 & !(province == 5 & district == 511 & palika == 51106 & ward == 12) |
    psu == 5321 & !(province == 5 & district == 508 & palika == 50802 & ward == 10) |
    psu == 5322 & !(province == 5 & district == 507 & palika == 50702 & ward == 5) |
    #KARNALI
    psu == 6301 & !(province == 6 & district == 603 & palika == 60303 & ward == 6) |
    psu == 6302 & !(province == 6 & district == 608 & palika == 60804 & ward == 1) |
    psu == 6303 & !(province == 6 & district == 608 & palika == 60804 & ward == 1) |
    psu == 6304 & !(province == 6 & district == 610 & palika == 61006 & ward == 4) |
    psu == 6305 & !(province == 6 & district == 610 & palika == 61006 & ward == 4) |
    psu == 6306 & !(province == 6 & district == 610 & palika == 61006 & ward == 10) |
    psu == 6307 & !(province == 6 & district == 602 & palika == 60202 & ward == 2) |
    psu == 6308 & !(province == 6 & district == 606 & palika == 60608 & ward == 1) |
    psu == 6309 & !(province == 6 & district == 610 & palika == 61006 & ward == 8) |
    psu == 6310 & !(province == 6 & district == 610 & palika == 61006 & ward == 1) |
    psu == 6311 & !(province == 6 & district == 602 & palika == 60201 & ward == 6) |
    psu == 6312 & !(province == 6 & district == 603 & palika == 60303 & ward == 5) |
    psu == 6313 & !(province == 6 & district == 604 & palika == 60404 & ward == 6) |
    psu == 6314 & !(province == 6 & district == 610 & palika == 61006 & ward == 6) |
    psu == 6315 & !(province == 6 & district == 610 & palika == 61006 & ward == 4) |
    psu == 6316 & !(province == 6 & district == 610 & palika == 61006 & ward == 4) |
    #FARWESTERN
    psu == 7301 & !(province == 7 & district == 708 & palika == 70803 & ward == 2) |
    psu == 7302 & !(province == 7 & district == 708 & palika == 70805 & ward == 2) |
    psu == 7303 & !(province == 7 & district == 709 & palika == 70909 & ward == 4) |
    psu == 7304 & !(province == 7 & district == 709 & palika == 70904 & ward == 18) |
    psu == 7305 & !(province == 7 & district == 705 & palika == 70502 & ward == 5) |
    psu == 7306 & !(province == 7 & district == 708 & palika == 70803 & ward == 1) |
    psu == 7307 & !(province == 7 & district == 709 & palika == 70904 & ward == 6) |
    psu == 7308 & !(province == 7 & district == 706 & palika == 70605 & ward == 5) |
    psu == 7309 & !(province == 7 & district == 708 & palika == 70807 & ward == 4) |
    psu == 7310 & !(province == 7 & district == 708 & palika == 70813 & ward == 4) |
    psu == 7311 & !(province == 7 & district == 709 & palika == 70901 & ward == 4) 
  )


#MAKING HOUSEHOLD ID UNIQUE 

section0 <- section0 %>%
  group_by(psu) %>%
  mutate(hhld = row_number()) %>%
  ungroup()

#MAKING PSU AND HHID UNIQUE 

psu_counts <- section0 %>%
  group_by(psu) %>%
  summarise(n_hhlds = n()) %>%
  ungroup()

section0 <- section0 %>%
  mutate(
    uniq_hhid = paste0(psu, "-", hhld), 
  ) 

any(duplicated(section0$uniq_hhid))
section0[duplicated(section0$uniq_id), "uniq_hhid"]

#MAKING HOUSEHOLD ID UNIQUE 

section0 <- section0 %>%
  group_by(psu) %>%
  mutate(hhld = row_number()) %>%
  ungroup()

#CHECKING EDUCATION DATA CONSISTENCY

section1a_edu <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    uniq_id1 = paste0(ID, "-", v101),
    v109 = as.integer(v109) 
  ) %>%
  filter(v109 == 1)
any(duplicated(section1a_edu$uniq_id))
any(duplicated(section1a_edu$personid))
section1a_edu[duplicated(section1a_edu$personid), "personid"]
section1a_edu[duplicated(section1a_edu$personid), "personid"]

  
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
  filter(verified == "Y")
any(duplicated(section5$personid))
section5[duplicated(section5$personid), "personid"]

section5 <- section5 %>%
  filter(personid %in% c(13734, 13735, 13737, 13738, 14801, 15258))


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

#REMITTANCE CONSISTENCY CHECK
section1a_remit <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    v109 = as.integer(v109) 
  ) %>%
  filter(v109 == 4)
any(duplicated(section1a_remit$uniq_id))
section1a_remit[duplicated(section1a_remit$uniq_id1), "uniq_id"]

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


#HEALTH EXPENDITURE AND OUT OF POCKET EXPENDITURE RATIO 
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

#CHECKING MISMATCHES WHILE SELECTING HOUSEHOLDS 

section0_mismatch <- merge.data.frame(section0, section2b,
by.x = "ID",
by.y = "ID",
all = FALSE)

section0_mismatch <- section0_mismatch %>%
  select(ID, psu.x, enrollment, province, district, palika.x, ward.x, hhld.x, v228, v229)

section0_mismatch <- section0_mismatch %>% 
  mutate(
    enrollment = as.integer(enrollment),
    v228 = as.integer(v228),
    v229 = as.integer(v229)
  ) %>%
  filter(
    (enrollment %in% c(1, 3) & v228 == 2) |
    (enrollment %in% c(2, 4) & v228 == 1 & v229 %in% c(1, 2)) 
  )

section0_mismatch <- section0_mismatch %>%
  filter(is.na(v228)) %>%
  mutate(enrollment = as.integer(enrollment)) %>%
  filter(enrollment == 1)
  
#CHECKING FOR NUMBER OF ACUTE AND CHRONIC ILLNESS IN THE SAMPLE

section6b1 <- read.xlsx("dataset/Part 6_2_1_ Chronic Illness and Health Seeking Behaviour.xlsx")
section6c1 <- read.xlsx("dataset/Part 6_3_1_ Acute Illness and health seeking behaviour.xlsx")

sum(is.na(section6b1$v603))
sum(is.na(section6c1$v629))

chronic_na <- section6b1 %>%
  filter(is.na(v603)) 

write.csv(chronic_na, "v603_na.csv")

acute_na <- section6c1 %>%
  filter(is.na(v629))

write.csv(acute_na, "v629_na.csv")


section6b1 <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  filter(!is.na(v603))

section6c1 <- section6c1 %>%
  mutate(v629 = as.integer(v629)) %>%
  filter(!is.na(v629))

sum(section6b1$v603 == 1, na.rm = TRUE)
sum(section6b1$v603 == 2, na.rm = TRUE)
sum(section6c1$v629 == 1, na.rm = TRUE)
sum(section6c1$v629 == 2, na.rm = TRUE)

#HOUSEHOLDS WITH ACUTE ILLNESS
acute <- section6b1 %>%
  group_by(ID) %>%
  filter(all(v603 == 1)) %>%
  distinct(ID)


#HOUSEHOLDS WITH CHRONIC ILLNESS
chronic <- section6c1 %>%
  group_by(ID) %>%
  filter(all(v629 == 1)) %>%
  distinct(ID)

#NO ACUTE ILLNESS HOUSEHOLDS
no_acute <- section6b1 %>%
  group_by(ID) %>%
  filter(all(v603 == 2)) %>%
  distinct(ID)

#NO CHRONIC ILLNESS HOUSEHOLDS
no_chronic <- section6c1 %>%
  group_by(ID) %>%
  filter(all(v629 == 2)) %>%
  distinct(ID)

#HOUSEHOLDS WITH NO CHRONIC AND NO ACUTE ILLNESSES
common_id <- intersect(acute$ID, chronic$ID)

common_id

#CHECKING FOR DUPLICATES IN ACUTE DISEASE LIST
acute_duplicates <- section6b1 %>%
  filter(duplicated(paste(personid, v604)) | duplicated(paste(personid, v604), fromLast = TRUE)) %>%
  select(ID, psu, palika, ward, hhld, version, verified, interviewer_id, v101, v603, personid, v604) 

write.csv(acute_duplicates, "acute_duplicates.csv")


#CHECKING FOR DUPLICATES IN 
chronic_duplicates <- section6c1 %>%
  filter(duplicated(paste(personid, v630)) | duplicated(paste(personid, v630), fromLast = TRUE)) %>%
  select(ID, psu, palika, ward, hhld, version, verified, interviewer_id, v101, v629, personid, v630)

write.csv(chronic_duplicates, "chronic_duplicates.csv")

#ENUMERATOR WISE NUMBERS OF NO HEALTH RECORDS 

chronic_households <- section6b1 %>% 
  mutate(v603 = as.integer(v603)) %>%
  group_by(ID) %>%
  summarise(
    hh_has_sick = any(v603 == 1),
    .groups = "drop"
  )

chronic_households <- merge.data.frame(chronic_households, section0, by.x = "ID", by.y = "ID")

chronic_households <- chronic_households %>%
  select(ID, hh_has_sick, Name.of.enumerator) %>%
  group_by(hh_has_sick, Name.of.enumerator) %>%
  summarise(
    n_households = n(),
    .groups = "drop"
  )

write.csv(chronic_households, "chronic_counts.csv")

acute_households <- section6c1 %>% 
  mutate(v629 = as.integer(v629)) %>%
  group_by(ID) %>%
  summarise(
    hh_has_sick = any(v629 == 1),
    .groups = "drop"
  )

acute_households <- merge.data.frame(acute_households, section0, by.x = "ID", by.y = "ID")

acute_households <- acute_households %>%
  select(ID, hh_has_sick, Name.of.enumerator) %>%
  group_by(hh_has_sick, Name.of.enumerator) %>%
  summarise(
    n_households = n(),
    .groups = "drop"
  )

write.csv(acute_households, "acute_counts.csv")

#CHECKING FOR COMMAS

section0_commas <- section0 %>%
  select(-village, -name, -respondent) %>%
  filter(if_any(everything(), ~grepl(",", .)))

write.csv(section0_commas, "commas check/section0_commas.csv", row.names = FALSE)

section1a_commas <- section1a %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section1a <- section1a %>%
  mutate(across(where(~ any(grepl(",", .))), ~ sub(",.*", "", .)))

write.csv(section1a_commas, "commas check/section1a_commas.csv", row.names = FALSE)

section1b_commas <- section1b %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section1b_commas, "commas check/section1b_commas.csv", row.names = FALSE)

section2a1_commas <- section2a1 %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section2a1_commas, "commas check/section2a1_commas.csv", row.names = FALSE)

section2a2_commas <- section2a2 %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section2a2_commas, "commas check/section2a2_commas.csv", row.names = FALSE)

section2a3_commas <- section2a3 %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section2a3_commas, "commas check/section2a3_commas.csv", row.names = FALSE)

section2b_commas <- section2b %>%
  select(ID, 50:74) %>%
  filter(if_any(everything(), ~ grepl(",", .))) 

write.csv(section2b_commas, "commas check/section2b_commas.csv", row.names = FALSE)

section3a_commas <- section3a %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section3a_commas, "commas check/section3a_commas.csv", row.names = FALSE)

section3b_commas <- section3b %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section3b_commas, "commas check/section3b_commas.csv", row.names = FALSE)

section4a_commas <- section4a %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section4a_commas, "commas check/section4a_commas.csv", row.names = FALSE)

section4b_commas <- section4b %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section4b_commas, "commas check/section4b_commas.csv", row.names = FALSE)

section4c_commas <- section4c %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section5_commas <- section5 %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section5 <- section5 %>%
  mutate(across(where(~ any(grepl(",", .))), ~ sub(",.*", "", .)))

write.csv(section5_commas, "commas check/section5_commas.csv", row.names = FALSE)

section6a_commas <- section6a %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section6a_commas, "commas check/section6a_commas.csv", row.names = FALSE)

section6b3_commas <- section6b3 %>%
  select(-v614) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section6b4_commas <- section6b4 %>%
  select(-v604) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section6b5_commas <- section6b5 %>%
  select(-v624, -v626) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section6b5_commas, "commas check/section6b5_commas.csv", row.names = FALSE)

section6c4_commas <- section6c4 %>%
  select(-v630, -v652, -v658) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section6c4_commas, "commas check/section6c4_commas.csv", row.names = FALSE)

section6d_commas <- section6d %>%
  select(-v668h, -v665) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section6d_commas, "commas check/section6d_commas.csv", row.names = FALSE)

section7_commas <- section7 %>%
  select(-v709a, -v710b, -v714a, -v716) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section7 <- section7 %>%
  mutate(across(
    .cols = -c(v709a, v710b, v714a, v716),  
    .fns = ~ ifelse(grepl(",", .), sub(",.*", "", .), .)
  ))


write.csv(section7_commas, "commas check/section7_commas.csv", row.names = FALSE)

section8_commas <- section8 %>%
  select(-v803, -v803b ) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section8 <- section8 %>%
  mutate(across(
    .cols = -c(v803, v803b),  
    .fns = ~ ifelse(grepl(",", .), sub(",.*", "", .), .)
  ))

write.csv(section8_commas, "commas check/section8_commas.csv", row.names = FALSE)

section9a_commas <- section9a %>%
  select(-v902b) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section9a <- section9a %>%
  mutate(across(
    .cols = -c(v902b), 
    .fns = ~ ifelse(grepl(",", .), sub(",.*", "", .), .)
  ))


write.csv(section9a_commas, "commas check/section9a_commas.csv", row.names = FALSE)

section9b_commas <- section9b %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section9b_commas, "commas check/section9b_commas.csv", row.names = FALSE)

section9c_commas <- section9c %>%
  select(-v914b_1) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section9d_commas <- section9d %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section9d_commas, "commas check/section9d_commas.csv", row.names = FALSE)

section9e_commas <- section9e %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section9f1_commas <- section9f1 %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section9f2_commas <- section9f2 %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section10_commas <- section10 %>%
  select(ID, 15:28, -v1002c) %>%
  filter(if_any(everything(), ~ grepl(",", .))) 

write.csv(section10_commas, "commas check/section10_commas.csv", row.names = FALSE)

section11a_commas <- section11a %>%
  select(-v1102, -v1105, -v1108a) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section11b_commas <- section11b %>%
  select(-v1112) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section11c_commas <- section11c %>%
  filter(if_any(everything(), ~ grepl(",", .)))

write.csv(section11c_commas, "commas check/section11c_commas.csv", row.names = FALSE)

section12a_commas <- section12a %>%
  select(-v1205, -v1206) %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section12b_commas <- section12b %>%
  filter(if_any(everything(), ~  grepl(",", .)))

write.csv(section12b_commas, "commas check/section12b_commas.csv", row.names = FALSE)

section13a_commas <- section13a %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section13b_commas <- section13b %>%
  filter(if_any(everything(), ~ grepl(",", .)))

section13c_commas <- section13c %>%
  filter(if_any(everything(), ~ grepl(",", .)))



