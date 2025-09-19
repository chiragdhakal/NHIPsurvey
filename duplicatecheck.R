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
section9f1 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure (1).xlsx")
section9f2 <- read.xlsx("dataset/Part 9_6_ Livestock Income and Expenditure.xlsx")
section10 <- read.xlsx("dataset/Income from Non - Agricultural Enterprises.xlsx")
section11a <- read.xlsx("dataset/section 11.xlsx")
section11b <- read.xlsx("dataset/Part 11_2_ Lending and Outstanding Loans.xlsx")
section11c <- read.xlsx("dataset/Part 11_3_ Other Assets.xlsx")
section12a <- read.xlsx("dataset/Remittance and transfer.xlsx")
section12b <- read.xlsx("dataset/Part 12_2. Other Remittances.xlsx")
section13a <- read.xlsx("dataset/section 13.xlsx")
section13b <- read.xlsx("dataset/Part 13_2_ Social Assistance.xlsx")
section13c <- read.xlsx("dataset/Part 13_3_ Other Income.xlsx")

#CHECKING FOR DUPLICATES IN SECTION1A

section1a_duplicates <- section1a %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section1a_duplicates, "duplicates check/section1a_duplicates.csv")


#CHECKING FOR DUPLICATES IN SECTION1B

section1b_duplicates <- section1b %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section1b_duplicates, "duplicates check/section1b_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION5

section5_duplicates <- section5 %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section5_duplicates, "duplicates check/section5_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6a

section6a_duplicates <- section6a %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6a_duplicates, "duplicates check/section6a_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6b1

section6b1_duplicates <- section6b1 %>%
  group_by(personid, v604, v604a) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6b1_duplicates, "duplicates check/section6b1_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6b2

section6b2_duplicates <- section6b2 %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6b2_duplicates, "duplicates check/section6b2_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6b3

section6b3_duplicates <- section6b3 %>%
  group_by(personid, v614) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6b3_duplicates, "duplicates check/section6b3_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6b4

section6b4_duplicates <- section6b4 %>%
  group_by(personid, v604) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6b3_duplicates, "duplicates check/section6b3_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6b5

section6b5_duplicates <- section6b5 %>%
  group_by(personid, v624) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6b5_duplicates, "duplicates check/section6b5_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6c1

section6c1_duplicates <- section6c1 %>%
  group_by(personid, v630) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6c1_duplicates, "duplicates check/section6c1_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6c2

section6c2_duplicates <- section6c2 %>%
  group_by(personid, v630) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6c2_duplicates, "duplicates check/section6c2_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6c3

section6c3_duplicates <- section6c3 %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6c3_duplicates, "duplicates check/section6c3_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6c4

section6c4_duplicates <- section6c4 %>%
  group_by(personid, v630) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section6c4_duplicates, "duplicates check/section6c4_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION6c5 

#section6c5_duplicates <- section6c5 %>%
  #group_by(personid) %>%
  #filter(n() > 1) %>%
  #ungroup()

#write.csv(section6c5_duplicates, "duplicates check/section6c5_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION7

section7_duplicates <- section7 %>%
  group_by(personid) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section7_duplicates, "duplicates check/section7_duplicates.csv") 

#CHECKING FOR DUPLICATES IN SECTION8

section8_duplicates <- section8 %>%
  group_by(personid, v803) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section8_duplicates, "duplicates check/section8_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION9a 

section9a_duplicates <- section9a %>%
  group_by(ID, v902a, v902b) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section9a_duplicates, "duplicates check/section9a_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION9c

section9c_duplicates <- section9c %>%
  group_by(ID, v914b) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section9c_duplicates, "duplicates check/section9c_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION9e

section9e_duplicates <- section9e %>%
  group_by(ID, v934a) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section9e_duplicates, "duplicates check/section9e_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION9f1

section9f1_duplicates <- section9f1 %>%
  group_by(ID, v940) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section9f1_duplicates, "duplicates check/section9f1_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION9f2

section9f2_duplicates <- section9f2 %>%
  group_by(ID, v942) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section9f2_duplicates, "duplicates check/section9f2_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION10

section10_duplicates <- section10 %>%
  group_by(ID, v1002, v1002a) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section10_duplicates, "duplicates check/section10_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION11a

section11a_duplicates <- section11a %>%
  group_by(ID, personid, v1102, v1104a, v1106) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section11a_duplicates, "duplicates check/section11a_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION11b

section11b_duplicates <- section11b %>%
  group_by(ID, personid, v1112, v1114a, v1116) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section11b_duplicates, "duplicates check/section11b_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION12a 

section12a_duplicates <- section12a %>%
  group_by(ID, personid, v1202) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section12a_duplicates, "duplicates check/section12a_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION13a 

section13a_duplicates <- section13a %>%
  group_by(ID, v1301) %>%
  filter(n() > 1) %>%
  ungroup() 

write.csv(section13a_duplicates, "duplicates check/section13a_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION13b 

section13b_duplicates <- section13b %>% 
  group_by(ID, v1308) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section13b_duplicates, "duplicates check/section13b_duplicates.csv")

#CHECKING FOR DUPLICATES IN SECTION13c

section13c_duplicates <- section13c %>%
  group_by(ID, v1311a, v1311b) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(section13c_duplicates, "duplicates check/section13c_duplicates.csv")


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
    (psu %in% c(1101, 1201) & !(province == 1 & district == 102 & palika == 10207 & ward == 7)) |
    (psu %in% c(1102, 1202) & !(province == 1 & district == 103 & palika == 10304 & ward == 7)) |
    (psu %in% c(1103, 1203) & !(province == 1 & district == 106 & palika == 10602 & ward == 7)) |
    (psu %in% c(1104, 1204) & !(province == 1 & district == 108 & palika == 10804 & ward == 7)) |
    (psu %in% c(1105, 1205) & !(province == 1 & district == 108 & palika == 10805 & ward == 7)) |
    (psu %in% c(1106, 1206) & !(province == 1 & district == 110 & palika == 11005 & ward == 7)) |
    (psu %in% c(1107, 1207) & !(province == 1 & district == 111 & palika == 11102 & ward == 7)) |
    (psu %in% c(1108, 1208) & !(province == 1 & district == 112 & palika == 11211 & ward == 7)) |
    (psu %in% c(1109, 1209) & !(province == 1 & district == 112 & palika == 11214 & ward == 7)) |
    (psu %in% c(1110, 1210) & !(province == 1 & district == 113 & palika == 11303 & ward == 7)) |
    (psu %in% c(1111, 1211) & !(province == 1 & district == 113 & palika == 11306 & ward == 7)) |
    (psu %in% c(1112, 1212) & !(province == 1 & district == 113 & palika == 11309 & ward == 7)) |
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

write.csv(nhip_check_geoinputs, "nhipincorrect_geoinputs.csv")

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
    psu == 1301 & !(province == 1 & district == 102 & palika == 10206) |
    psu == 1302 & !(province == 1 & district == 110 & palika == 11003) |
    psu == 1303 & !(province == 1 & district == 112 & palika == 11203) |
    psu == 1306 & !(province == 1 & district == 111 & palika == 11101) |
    psu == 1307 & !(province == 1 & district == 111 & palika == 11112) |
    psu == 1308 & !(province == 1 & district == 113 & palika == 11306) |
    psu == 1309 & !(province == 1 & district == 113 & palika == 11306) |
    psu == 1310 & !(province == 1 & district == 110 & palika == 11003) |
    psu == 1314 & !(province == 1 & district == 113 & palika == 11307) |
    psu == 1315 & !(province == 1 & district == 112 & palika == 11213) |
    psu == 1316 & !(province == 1 & district == 111 & palika == 11101) |
    psu == 1317 & !(province == 1 & district == 113 & palika == 11307) |
    psu == 1318 & !(province == 1 & district == 112 & palika == 11214) |
    #MADHESH
    psu == 2301 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2302 & !(province == 2 & district == 205 & palika == 20502) |
    psu == 2303 & !(province == 2 & district == 204 & palika == 20415) |
    psu == 2304 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2305 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2306 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2307 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2308 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2309 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2310 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2311 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2312 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2313 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2314 & !(province == 2 & district == 201 & palika == 20107) |
    psu == 2315 & !(province == 2 & district == 201 & palika == 20107) |
    #BAGMATI
    psu == 3301 & !(province == 3 & district == 302 & palika == 30205) |
    psu == 3302 & !(province == 3 & district == 309 & palika == 30909) |
    psu == 3303 & !(province == 3 & district == 312 & palika == 31206) |
    psu == 3304 & !(province == 3 & district == 309 & palika == 30909) |
    psu == 3305 & !(province == 3 & district == 309 & palika == 30905) |
    psu == 3306 & !(province == 3 & district == 313 & palika == 31304) |
    psu == 3307 & !(province == 3 & district == 313 & palika == 31304) |
    psu == 3308 & !(province == 3 & district == 309 & palika == 30906) |
    psu == 3309 & !(province == 3 & district == 309 & palika == 30904) |
    psu == 3310 & !(province == 3 & district == 312 & palika == 31206) |
    psu == 3311 & !(province == 3 & district == 313 & palika == 31304) |
    psu == 3312 & !(province == 3 & district == 313 & palika == 31304) |
    psu == 3313 & !(province == 3 & district == 309 & palika == 30904) |
    psu == 3314 & !(province == 3 & district == 312 & palika == 31206) |
    psu == 3315 & !(province == 3 & district == 313 & palika == 31304) |
    psu == 3316 & !(province == 3 & district == 306 & palika == 30610) |
    psu == 3317 & !(province == 3 & district == 306 & palika == 30610) |
    psu == 3318 & !(province == 3 & district == 307 & palika == 30703) |
    psu == 3319 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3320 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3321 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3322 & !(province == 3 & district == 306 & palika == 30608) |  
    psu == 3323 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3324 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3325 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3326 & !(province == 3 & district == 306 & palika == 30604) |
    psu == 3327 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3328 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3329 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3330 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3331 & !(province == 3 & district == 306 & palika == 30602) |
    psu == 3332 & !(province == 3 & district == 306 & palika == 30602) |
    psu == 3333 & !(province == 3 & district == 306 & palika == 30604) |
    psu == 3334 & !(province == 3 & district == 306 & palika == 30604) |
    psu == 3335 & !(province == 3 & district == 306 & palika == 30604) |
    psu == 3336 & !(province == 3 & district == 306 & palika == 30607) |
    psu == 3337 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3338 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3339 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3340 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3341 & !(province == 3 & district == 307 & palika == 30704) |
    psu == 3342 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3343 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3344 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3345 & !(province == 3 & district == 306 & palika == 30604) |
    psu == 3346 & !(province == 3 & district == 306 & palika == 30602) |
    psu == 3347 & !(province == 3 & district == 306 & palika == 30605) |
    psu == 3348 & !(province == 3 & district == 306 & palika == 30603) |
    psu == 3349 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3350 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3351 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3352 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3353 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3354 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3355 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3356 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3357 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3358 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3359 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3360 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3361 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3362 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3363 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3364 & !(province == 3 & district == 307 & palika == 30703) |
    psu == 3365 & !(province == 3 & district == 307 & palika == 30703) |
    psu == 3366 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3367 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3368 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3369 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3370 & !(province == 3 & district == 306 & palika == 30604) |
    psu == 3371 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3372 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3373 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3374 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3375 & !(province == 3 & district == 306 & palika == 30608) |
    psu == 3376 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3377 & !(province == 3 & district == 307 & palika == 30703) |
    psu == 3378 & !(province == 3 & district == 307 & palika == 30701) |
    psu == 3379 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3380 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3381 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3382 & !(province == 3 & district == 308 & palika == 30802) |
    psu == 3383 & !(province == 3 & district == 308 & palika == 30802) |  
    #GANDAKI
    psu == 4301 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4302 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4303 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4304 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4305 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4306 & !(province == 4 & district == 409 & palika == 40903) |
    psu == 4307 & !(province == 4 & district == 408 & palika == 40803) |
    psu == 4308 & !(province == 4 & district == 408 & palika == 40808) |
    psu == 4309 & !(province == 4 & district == 408 & palika == 40808) |
    psu == 4310 & !(province == 4 & district == 408 & palika == 40801) |
    psu == 4311 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4312 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4313 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4314 & !(province == 4 & district == 407 & palika == 40710) |
    psu == 4315 & !(province == 4 & district == 407 & palika == 40702) |
    psu == 4316 & !(province == 4 & district == 408 & palika == 40801) |
    psu == 4317 & !(province == 4 & district == 408 & palika == 40806) |
    psu == 4318 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4319 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4320 & !(province == 4 & district == 405 & palika == 40504) |
    psu == 4321 & !(province == 4 & district == 408 & palika == 40805) |
    psu == 4322 & !(province == 4 & district == 408 & palika == 40801) |
    #LUMBINI
    psu == 5301 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5302 & !(province == 5 & district == 510 & palika == 51002) |
    psu == 5303 & !(province == 5 & district == 509 & palika == 50901) |
    psu == 5304 & !(province == 5 & district == 511 & palika == 51103) |
    psu == 5305 & !(province == 5 & district == 511 & palika == 51104) |
    psu == 5306 & !(province == 5 & district == 511 & palika == 51106) |
    psu == 5307 & !(province == 5 & district == 511 & palika == 51102) |
    psu == 5308 & !(province == 5 & district == 512 & palika == 51208) |
    #psu == 5309 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5310 & !(province == 5 & district == 508 & palika == 50802) |
    psu == 5311 & !(province == 5 & district == 508 & palika == 50808) |
    #psu == 5312 & !(province == 5 & district == 508 & palika == 50808) |
    psu == 5313 & !(province == 5 & district == 508 & palika == 50802) |
    psu == 5314 & !(province == 5 & district == 508 & palika == 50808) |
    psu == 5315 & !(province == 5 & district == 509 & palika == 50903) |
    psu == 5316 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5317 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5318 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5319 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5320 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5321 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5322 & !(province == 5 & district == 503 & palika == 50305) |
    psu == 5323 & !(province == 5 & district == 503 & palika == 50305) 
  )


#COMPLETENESS CHECKS

#CHECKING FOR NAs IN AGE

section1a_age <- section1a %>%
  mutate(v104a = as.integer(v104a)) %>%
  filter(is.na(v104a))

write.csv(section1a_age, "completeness checks/ageNA.csv")

#CHECKING WHETHER THERE ARE ALL HOUSEHOLD RESIDENTS IN SECTION1B

section1a_v109 <- section1a %>%
  select(personid, v109)
  
section1a[duplicated(section1a$personid), "personid"]

section1a_hhresident <- section1a %>%
  filter(
    trimws(v109) %in% c(1, 2)  
  ) 

section1b[duplicated(section1b$personid), "personid"]

section1b_hhstatus <- merge.data.frame(section1b, section1a_v109, by.x = "personid", by.y = "personid", all = FALSE)

section1b_miscategorized <- section1b_hhstatus %>%
  filter(trimws(v109) %in% c(3, 4))

write.csv(section1b_miscategorized, "completeness checks/section1b_miscategorized.csv")

#CHECKING FOR EDUCATION CONSISTENCY

section1b_education <- section1b %>%
  filter(
    trimws(v115) == 3
  )

section1b_education[duplicated(section1b_education$personid), "personid"]

section5[duplicated(section5$personid), "personid"]

section5_missing <- section1b_education %>%
  anti_join(section5, by = "personid")

write.csv(section5_missing, "completeness checks/education_qualified_missing.csv")

#CHECKING FOR AGE <= 5 IN SECTION 6.1

age_mishap_6.1 <- section1a %>%
  inner_join(section6a, by = "personid") %>%
  mutate(v104a = as.integer(v104a)) %>%
  select(ID = ID.x, palika = palika.x, ward = ward.x,
         hhld = hhld.x, version = version.x, verified = verified.x,
         v101 = v101.x, v104a, v601) %>%
  filter(is.na(v104a) | v104a < 5) 

write.csv(age_mishap_6.1, "completeness checks/age_mishap_6.1.csv")

#CHECKING FOR ALL PEOPLE IN SECTION 6.2.1

section1a_chronic_qualified <- section1a %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(
    v109 %in% c(1, 2)
  )

section6b1_missing <-  section1a_chronic_qualified %>%
  anti_join(section6b1, by = "personid")

write.csv(section6b1_missing, "section6b1_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.2.2

section6b1_chronic_checks <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  filter(v603 == "1"
  )

section6b1_chronic_checks[duplicated(section6b1_chronic_checks$personid), "personid"]

section6b2_qualified_missing <- section6b1_chronic_checks %>%
  anti_join(section6b2, by = "personid")

write.csv(section6b2_qualified_missing, "section6b2_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.2.3

section6b1_chronic_inpatients <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  filter(
    v603 == 1, 
    v605a > 0
  )

section6b3_qualified_missing <- section6b1_chronic_inpatients %>%
  anti_join(section6b3, by = "personid")

write.csv(section6b3_qualified_missing, "section6b3_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.2.4

section6b1_inpatients <- section6b1 %>%
  mutate(v605b = as.integer(v605b)) %>%
  filter(trimws(v605b) > 0)

section6b4_missing <- section6b1_inpatients %>%
  anti_join(section6b4, by = "personid")

write.csv(section6b4_missing, "completeness checks/section6b4_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.2.5 

section1a_olderthan5 <- section1a %>%
  mutate(
    v104a = as.integer(v104a),
    v109 = as.integer(v109) 
  ) %>%
  filter(v104a > 5) %>%
  filter(v109 %in% c(1,2))

section6b1_chronic <- section6b1 %>%
  mutate(v603 = as.integer(v603)) %>%
  group_by(ID) %>%
  filter(any(v603 == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(personid, v603)

section6b5_qualify <- merge.data.frame(section1a_olderthan5, section6b1_chronic, by.x = "personid", by.y = "personid")

section6b5_missing <- section6b5_qualify %>%
  anti_join(section6b5, by = "personid")

write.csv(section6b5_missing, "section6b5_missing.csv")

#CHECKING FOR ALL PEOPLE IN SECTION 6.3.1

section1a_acute_qualified <- section1a %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(
    v109 %in% c(1, 2)
  )

section6c1_missing <-  section1a_acute_qualified %>%
  anti_join(section6c1, by = "personid")

write.csv(section6c1_missing, "section6c1_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.3.2

section6c1_acute_checks <- section6c1 %>%
  mutate(v629 = as.integer(v629)) %>%
  filter(v629 == "1"
  )

section6c1_acute_checks[duplicated(section6c1_acute_checks$personid), "personid"]

section6c2_qualified_missing <- section6c1_acute_checks %>%
  anti_join(section6c2, by = "personid")

write.csv(section6c2_missing, "section6c2_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.3.3

section6c3_qualified_missing <- section6c1_acute_checks %>%
  anti_join(section6c3, by = "personid")

write.csv(section6c3_missing, "section6c3_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.3.4

section6c4_qualified_missing <- section6c1_acute_checks %>%
  anti_join(section6c4, by = "personid")

write.csv(section6c4_missing, "section6c4_missing.csv")

#CHECKING FOR ALL PEOPLE QUALIFYING FOR SECTION 6.3.5 

section6c5_qualified_missing <- section6c1_acute_checks %>%
  anti_join(section6c5, by = "personid")

write.csv(section6c5_missing, "section6c5_missing.csv")

#CHECKING FOR ALL PEOPLE IN SECTION 7 

section7_qualified <- section1a %>%
  mutate(
    v104a = as.integer(v104a),
    v109 = as.integer(v109)
  ) %>%
  filter(
    v104a > 10, 
    v109 %in% c(1, 2)
  )

section7_missing <- section7_qualified %>%
  anti_join(section7, by = "personid")

write.csv(section7_missing, "section7_missing.csv")

#CHECKING FOR ALL PEOPLE IN SECTION 8 

section8_qualified <- section1a %>%
  mutate(
    v104a = as.integer(v104a),
    v109 = as.integer(v109)
  ) %>%
  filter(
    v104a > 10, 
    v109 %in% c(1, 2)
  )

section8_missing <- section8_qualified %>%
  anti_join(section8, by = "personid")

write.csv(section8_missing, "section8_missing.csv")

#CHECKING FOR ALL PEOPLE IN SECTION11A 

section11a_qualified <- section1a %>%
  mutate(
    v104a = as.integer(v104a),
    v109 = as.integer(v109)
  ) %>%
  filter(
    v104a > 10, 
    v109 %in% c(1, 2)
  )

section11a_missing <- section11a_qualified %>%
  anti_join(section11a, by = "personid")

write.csv(section11a_missing, "section11a_missing.csv")

#CHECKING FOR ALL PEOPLE IN SECTION12A

section12a_qualified <- section1a %>%
  mutate(
    v104a = as.integer(v104a),
    v109 = as.integer(v109)
  ) %>%
  filter(
    v104a > 10, 
    v109 %in% c(3, 4)
  )

section12a_missing <- section12a_qualified %>%
  anti_join(section12a, by = "personid")

write.csv(section12a_missing, "section12a_missing.csv")

#CHECKING FOR HOUSEHOLDS WITH NO HEALTH EXPENDITURE

sec4_healthex <- section4a %>%
  filter(v401 == 22)

health_expenditure <- section2b %>%
  inner_join(section6d, by = "ID") %>%
  inner_join(sec4_healthex, by = "ID")

health_expenditure <- health_expenditure %>%
  select(ID, psu = psu.x, palika = palika.x, ward = ward.x,
         hhld = hhld.x, version = version.x, verified = verified.x,
         v249, v662, v403a, v403b)

no_healthexpense <- merge.data.frame(health_expenditure, section0, by.x = "ID", by.y = "ID")

no_healthexpense <- no_healthexpense %>%
  filter(v249 == 0) %>%
  select(ID, psu = psu.x, palika = palika.x, ward = ward.x, hhld = hhld.x, version = version.x, verified = verified.x, v249, respondent, Name.of.enumerator)

no_expense_enumerator <- no_healthexpense %>%
  group_by(Name.of.enumerator) %>%
  summarise(
    n_hhlds = n()
  )

health_expenditure <- health_expenditure %>%
  mutate(
    v249 = as.integer(v249), 
    v662 = as.integer(v662)
  )

#Section12 discrepency 

outside_household <- section1a %>%
  mutate(v109 = as.integer(v109)) %>%
  filter(v109 %in% c(3, 4)) %>%
  select(personid, v109)

outside_household <- merge.data.frame(section12a, outside_household, by.x = "personid", by.y = "personid", all = FALSE)

outside_household <- outside_household %>%
  select(personid, ID, psu, palika, ward, hhld, version, verified, v101, v802, v109)

write.csv(outside_household, "section12adiscrepency.csv")

#UNIQUE ID 

section0 <- section0 %>%
  group_by(psu) %>%
  arrange(psu, hhld, ID) %>%      
  mutate(
    hhld_serial = row_number(),     
    uniq_hhid = paste0(psu, "-", hhld_serial)
  ) %>%
  ungroup()

any(duplicated(section0$uniq_hhid))

#NORMAL DISTRIBUTION CONSISTENCY OF TWO HEALTH EXPENSE VARIABLES

mean_v249 <- mean(health_expenditure$v249, na.rm = TRUE)
sd_v249   <- sd(health_expenditure$v249, na.rm = TRUE)

mean_v662 <- mean(health_expenditure$v662, na.rm = TRUE)
sd_v662   <- sd(health_expenditure$v662, na.rm = TRUE)

xrange <- seq(
  min(mean_v249 - 4*sd_v249, mean_v662 - 4*sd_v662),
  max(mean_v249 + 4*sd_v249, mean_v662 + 4*sd_v662),
  length.out = 1000
)

dens_v249 <- dnorm(xrange, mean = mean_v249, sd = sd_v249)
dens_v662 <- dnorm(xrange, mean = mean_v662, sd = sd_v662)

plot(xrange, dens_v249, type = "l", col = "red", lwd = 2,
     xlab = "Value", ylab = "Density",
     main = "Normal Curves for v249 and v662")
lines(xrange, dens_v662, col = "blue", lwd = 2)

legend("topright", legend = c("v249", "v662"), col = c("red", "blue"), lwd = 2)

#INCOME AND EXPENDITURE CONSISTENCY CHECK


