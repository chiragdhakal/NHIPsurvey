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
library(purrr)

in_dir <- "stata_data1"

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

sections <- lapply(files, read_dta)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)

#MAKING NEW CATEGORIES FOR CHRONIC DISEASES

section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b3 <- section6b3 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b4 <- section6b4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b3 <- section6b3 %>%
  mutate(
    v604a = trimws(v604a),
    v604 = case_when(
      # Heart Diseases
      v604a %in% c(
        "कोलेस्ट्रोल", "COLSTORE", "CHOLESTEROL", "COLSTRORE", "CHOLOSTREL", "CHLOSTROAL",
        "COLSTROL", "CHLOSTROL", "COLESTROME", "COLESTER", "COLDSTORAL",
        "COLESTEROL", "CHOLESTEROLPROSTATE", "CHOLESTROL", "CHOLEDTEROL",
        "COLDSTORE", "COL", "CHORESTEROL", "COL STORE", "CHOLESTEROL PROSTATE", "CHOLESTEROL", "CHLORESTROL"
      ) ~ 1,

      # Uric Acid
      v604a %in% c(
        "URIC ACID", "URIK ACID", "URIK ASID", "URIC  ACID",
        "URIC ACID RA PROSTHETICS", "URIQE ACID"
      ) ~ 20,

      # Diabetes / Sugar
      v604a %in% c("SUGAR", "SUGAR BLOOD PRESSURE", "DIABETIC") ~ 3,

      # Chronic Gastrointestinal Diseases
      v604a %in% c(
        "PILES", "PAYALS", "ALSAR", "ULCER", "ULCERS",
        "PILES KO LAI SHE SOMETIMES USES OINTMENT BUT MOSTLY TAKES AYURVEDIC MEDICINE",
        "GASTRIC", "GATRIC", "APPENDIX"
      ) ~ 13,

      # Male Reproductive Diseases
      v604a %in% c(
        "PROSTATE", "PROSTED", "PROSTRATE", "PROTEST", "POSTATE", "PROTESTED KO SAMASYA", "POSTERT",
        "PROSTATE PROBLEM", "POSTED", "POSTERD", "PROSTHETIC", "PROSTRATE", "PROSTATE", "POSTATE",
        "PIABKO SAMASYAA", "PISAB BANDA HUNE GAREKO", "PISAB KO SAMASYA PROSTATE", "PISAB ROKKINE SAMASYA",
        "PISAB THAILI KO PROBLEM", "PISABKO KHARABI", "STONE IN URINE PIPE"
      ) ~ 21,

      # Blood Pressure
      v604a %in% c("PRESSURE", "PRESSURE LOW", "BP LOW", "LOW BLOOD PRESSURE") ~ 2,

      # Neurological condition
      v604a %in% c("MIGRAINE", "MIGRANE", "MIGRAIN", "MIGRAINE SAMBANDI") ~ 16,

      # Joint / Knee pain
      v604a %in% c("KNEE PAIN", "BATH") ~ 5,

      # Cancer
      v604a %in% c("CANCER", "TONGUE CANCER") ~ 8,

      # Skin diseases
      v604a %in% c(
        "CHHALAKO ROG", "CHALA ROG", "SKIN PROBLEM", "SKIN ALLERGY", "BODY ALLERGY",
        "SKIN ALLERGIES", "SKIN ELERGY", "SKIN", "SKIN CONDITION", "XALA SAMBANDHI",
        "छालाको समस्या छाला रोग", "ALLERGY", "ACNE ISSUES", "CHHALA ROG DAJ", "SKIN ROG",
        "CHALA SAMBANDHI", "KHUTTA MA DAG TAI CHILAUNA"
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
    ) ~ 16,

    #TRAUMA/INJURY 
    v604a %in% c(
      "LEGAMENT KO PROBLEM", "ACCIDENT", "BACKBONE DISLOCATED", "ADMITTED WITH BROKEN LEG.SHE WAS TAKEN TO TULSIPUR INDIA FOR TREATMENTWHICH COSTS APPROX.NPR..",
      "HAND INJURY", "DISLOCATED BACKBONE", "ACCIDENT BHAYERA PARALYSIS JASTO TAUKO HAT KHUTTA MAA CHOT PAREKO",
      "RIGHT HAND DISABLE DUE TO INJURY"
    ) ~ 26,


    #DISABILITY 
    v604a %in% c(
      "DIFFERENTLY ABLE", "PURNA APANGA", "AAPANGA", "DISABLE", "DOWN SYNDROME"
    ) ~ 30 ,

    #LUNG DISEASES 
    v604a  %in% c(
      "LUNGS KO BATHH VANNI", "LUNGS PROBLEM", "PHOKSO KO PROBLEM"
    ) ~ 27,

    #INFECTITOUS DISEASE 
    v604a %in% c(
    "SCRUBE TIFUS"
    ) ~ 29,

    #BLOOD AND BLOOD VESSEL DISEASES 
    v604a %in% c(
    "BLOOD BAKLO VAYEKO", "BLOOD PATALO GARAUNAY", "VARICOSE VEINS", "NASA KO DABAI", "NASA SAMBANDHI", "RAGAT KO KAMI"
    ) ~ 28,
    
    #LIVER DISEASES 
    v604a %in% c(
      "ALCOLOHISM", "LIVER KO SAMSYA", "HE HAD TO BE HOSPITALIZED THIS YEAR DUE TO EXCESSIVE ALCOHOL CONSUMPTION"
    ) ~ 7,

    v604a == "" & v604 == 96 ~ 2,

      TRUE ~ v604
    )
  )


section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101))


section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b3 <- section6b3 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_outpatient_costs <- section6b3 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

components <- c(
  "v614a", "v614b", "v614c", "v614d", "v614e",
  "v614f", "v614g", "v614h", "v614i", "v614j"
)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    total_sum = rowSums(across(all_of(components)), na.rm = TRUE),
    max_component = do.call(
      pmax,
      c(across(all_of(components)), na.rm = TRUE)
    ),
    max_share = max_component / total_sum,
    flag_single_bucket = if_else(
      total_sum > 0 & max_share > 0.9,
      1L, 0L
    )
  )

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    n_positive_components =
      rowSums(across(all_of(components), ~ . > 0), na.rm = TRUE),

    flag_total_only =
      if_else(
        v614k > 0 & n_positive_components == 0,
        1L, 0L
      )
  )

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  filter(flag_single_bucket == 1 | flag_total_only == 1)

missing_diseases <- anti_join(
  chronic_outpatient_costs, 
  section6b1, 
  by = "disease_id"
) %>%
  select(disease_id, uniq_id, v604, personid) %>%
  distinct()

missing_diseases

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  section6b1[, c("disease_id", "v605a", "v605b", "v606", "v607", "v611")],
  by = "disease_id"
)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  select(
    -uid, -psu, -palika, -ward, -hhld, -version, -verified,
    -interviewer_id, -personid, -v101, 
    -v615, -v616, -v617
  )

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  select(
    ID, disease_id, uniq_id, v103, v604, v604a, v605a, v605b, v104a, v606, v607, v611, everything()
  )

chronic_checkups <- chronic_checkups %>%
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

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  rename(chronic_condition = v604) %>%
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
    )
  )

chronic_inpatient_costs <- section6b4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b4 <- section6b4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

section6b1_inpatients <- section6b1 %>%
  filter(v605b > 0, na.rm = TRUE)


missing_diseases <- anti_join(
  chronic_inpatient_costs, 
  section6b1, 
  by = "disease_id"
) %>%
  select(disease_id, uniq_id, v604, personid) %>%
  distinct()

chronic_inpatient_costs <- merge(
  chronic_inpatient_costs, 
  section6b1[, c("disease_id", "v605a", "v605b", "v611")],
  by = "disease_id"
)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  select(
    -uid, -psu, -palika, -ward, -hhld, -version, -verified,
    -interviewer_id, -personid, -v101, -v619, -v620, -v621
  ) 

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  select(
    ID, disease_id, uniq_id, v103, v604, v605a, v605b, v611, everything()
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  rename(chronic_condition = v604) %>%
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
    )
  )
  
#MAKING NEW CATEGORY FOR ACUTE DISEASES 

section6c1 <- section6c1 %>%
  mutate(
    v630a = trimws(v630a),
    v630 = case_when(

      v630a %in% c(
        "रुघाखोकी", "खोकी", "RUGHA KHOKI", "RUGAKHOKI", "ROUGHA KHOLA",
        "KHOKI", "COLD ALLERGY", "COLD", "CHISO RUGHA"
      ) ~ 6,

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

      v630a %in% c("TONSILS", "TONSILLITIS", "TONSIL") ~ 19,

      v630a %in% c(
        "PREGNANT", "PREGNANCY KO BELA SUGAR LEVEL HIGH VYERW",
        "PREGNANCY CHECK UP", "SUTKERI",
        "PREGNANT NA BHAYERA CHECK UP",
        "PERIOD PAIN", "PERIOD ANIYAMIT",
        "PATHEGHAR KO SAMASYA", "PATHEGHAR KO OPERATION"
      ) ~ 20,

      v630a %in% c(
        "THAUKO DUKHANE", "TAUKO MA GHAU", "TAUKO DUKHNE",
        "TAUKO DUKHEKO VYERW", "TAUKO DUKHEKO",
        "TAU KO DUKHNA",
        "HEADACHE", "HEADACE", "HEAD INJURIES"
      ) ~ 21,

      v630a %in% c(
        "PISABMA KHARABI", "PISAB THAILIKO PATHARI", "PISAB ROKIYAKO"
      ) ~ 10,

      v630a %in% c("ENT", "EAR PROBLEM") ~ 13,

      TRUE ~ v630
    )
  )

section6c1 <- section6c1 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld)
  )

acute_illness <- section6c1 %>%
  filter(v629 == 1) %>%
  filter(v630 == 96)

acute_illness <- acute_illness %>%
  mutate(
    v631a_d = as.Date(v631a, format = "%Y/%m/%d"),
    v631b_d = as.Date(v631b, format = "%Y/%m/%d"),
    disease_duration = as.integer(v631b_d - v631a_d)
  ) %>%
  select(-ID, -palika, -uid, -psu, -palika, -v631a, -v631b, -ward, -hhld, -version, -verified, -interviewer_id, -personid, -v101, -v629) %>%
  select(uniq_id, disease_id, hhid, v630, v630a, disease_duration, v636)
  

acute_illness <- acute_illness %>%
  mutate(
    v630 = case_when(
      v630 == 1  ~ "Diarrhoea",
      v630 == 2  ~ "Typhoid",
      v630 == 3  ~ "Dengue",
      v630 == 4  ~ "Malaria",
      v630 == 5  ~ "Acute Respiratory Infection",
      v630 == 6  ~ "Cold/Flu/Fever",
      v630 == 7  ~ "Pneumonia",
      v630 == 8  ~ "Measles",
      v630 == 9  ~ "Jaundice",
      v630 == 10 ~ "UTI",
      v630 == 11 ~ "Dental Problem",
      v630 == 12 ~ "Acute Eye Infection",
      v630 == 13 ~ "Acute Ear Infection",
      v630 == 14 ~ "Skin Disease",
      v630 == 15 ~ "Injury",
      v630 == 16 ~ "Accident",
      v630 == 17 ~ "Other Fever",
      v630 == 96 ~ "Other",
      TRUE ~ NA_character_
    ), 
    v636 = case_when(
      v636 == 1  ~ "Health Post",
      v636 == 2  ~ "Primary Health Centre",
      v636 == 3  ~ "Government Hospital",
      v636 == 4  ~ "Government Outreach Clinic",
      v636 == 5  ~ "Government Ayurveda Centre",
      v636 == 6  ~ "Pharmacy/Drug Seller",
      v636 == 7  ~ "Private Clinic",
      v636 == 8  ~ "Private/Community Hospital",
      v636 == 9  ~ "Private Ayurveda Centre",
      v636 == 10 ~ "Health Worker’s Home",
      v636 == 11 ~ "Alternative/Traditional Healer",
      v636 == 12 ~ "Abroad (India/Other)",
      v636 == 13 ~ "Other (Specify)",
      v636 == 14 ~ "Others",
      TRUE       ~ NA_character_
    )
  ) %>%
  rename(
    health_facility = v636
  )

write.xlsx(acute_illness, "acute_illness.xlsx")

section6c4 <- section6c4 %>%
  mutate(
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    hhid = paste0(psu, "-", hhld)
  )

acute_costs <- section6c4 %>%
  select(hhid, uniq_id, disease_id, personid, v630, v630a, v651a, v651b, v651c, v651d, v651d, v651e, v651f, v651g, v651h, v651i, v651j, v651k) 

missing_diseases <- anti_join(
  acute_costs, 
  section6c1, 
  by = "disease_id"
) %>%
  select(disease_id, uniq_id, v630, personid) %>%
  distinct()

acute_costs <- merge(
  acute_costs, 
  section6c1[, c("disease_id", "v636")], 
  by = "disease_id"
)

acute_costs <- acute_costs %>%
  mutate(
    v630 = case_when(
      v630 == 1  ~ "Diarrhoea",
      v630 == 2  ~ "Typhoid",
      v630 == 3  ~ "Dengue",
      v630 == 4  ~ "Malaria",
      v630 == 5  ~ "Acute Respiratory Infection",
      v630 == 6  ~ "Cold/Flu/Fever",
      v630 == 7  ~ "Pneumonia",
      v630 == 8  ~ "Measles",
      v630 == 9  ~ "Jaundice",
      v630 == 10 ~ "UTI",
      v630 == 11 ~ "Dental Problem",
      v630 == 12 ~ "Acute Eye Infection",
      v630 == 13 ~ "Acute Ear Infection",
      v630 == 14 ~ "Skin Disease",
      v630 == 15 ~ "Injury",
      v630 == 16 ~ "Accident",
      v630 == 17 ~ "Other Fever",
      v630 == 96 ~ "Other",
      v630 == 18 ~ "Abdominal Pain/Gastritis", 
      v630 == 19 ~ "Tonsils", 
      v630 == 20 ~ "Pregnancy/Gynaecological", 
      v630 == 21 ~ "Headache/Head Injury",
      TRUE ~ NA_character_
    ), 
    v636 = case_when(
      v636 == 1  ~ "Health Post",
      v636 == 2  ~ "Primary Health Centre",
      v636 == 3  ~ "Government Hospital",
      v636 == 4  ~ "Government Outreach Clinic",
      v636 == 5  ~ "Government Ayurveda Centre",
      v636 == 6  ~ "Pharmacy/Drug Seller",
      v636 == 7  ~ "Private Clinic",
      v636 == 8  ~ "Private/Community Hospital",
      v636 == 9  ~ "Private Ayurveda Centre",
      v636 == 10 ~ "Health Worker’s Home",
      v636 == 11 ~ "Alternative/Traditional Healer",
      v636 == 12 ~ "Abroad (India/Other)",
      v636 == 13 ~ "Other (Specify)",
      v636 == 14 ~ "Others",
      TRUE       ~ NA_character_
    )
  ) 



chronic_inpatient_costs <- read.xlsx("/home/sobaakun/NHIPsurvey/chronic_inpatient_costs.xlsx")

facility_map <- c(
  "1"  = "Health Post", 
  "2"  = "Primary Health Centre", 
  "3"  = "Government Hospital", 
  "4"  = "Government Outreach Clinic", 
  "5"  = "Government Ayurveda Centre",
  "6"  = "Pharmacy/Drug Seller", 
  "7"  = "Private Clinic", 
  "8"  = "Private/Community Hospital", 
  "9"  = "Private Ayurveda Centre", 
  "10" = "Health Worker's Home", 
  "11" = "Alternative/Traditional Healer", 
  "12" = "Abroad (India/Other)", 
  "96" = "Others"
)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  mutate(
    health_facility_label = map_chr(
      str_split(health_facility, ",\\s*"),
      ~ paste(recode(.x, !!!facility_map), collapse = ", ")
    )
  ) %>%
  select(-health_facility)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  rename(
    emergency_costs          = v618a,
    bed_charges              = v618b, 
    laboratory_costs         = v618c, 
    imaging_costs            = v618d, 
    medicine_costs           = v618e, 
    medical_supplies_costs   = v618f, 
    transportation_costs     = v618g, 
    accomodation_costs       = v618h,
    care_giver_costs         = v618i, 
    other_costs              = v618j, 
    total_costs              = v618k
  )

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  left_join(
    section1a %>% select(uniq_id, v103, v104a),
    by = "uniq_id"
  ) %>%
  mutate(
    v103 = case_when(
      v103 == 1 ~ "Male",
      v103 == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    age = v104a
  ) %>%
  select(-v104a)

section6b1 <- section6b1 %>%
  mutate(
    uniq_id    = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  left_join(
    section6b1 %>% select(disease_id, v606, v607),
    by = "disease_id"
  ) %>%
  mutate(
    disease_onset_years = if_else(is.na(v606), 0, v606),
    currently_seeking_healthcare = case_when(
      v607 == 1 ~ "Yes",
      v607 == 2 ~ "No",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-v606, -v607)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  select(
    ID,
    disease_id,
    uniq_id,
    v103,
    age,
    chronic_condition,
    other_chronic_condition,
    disease_onset_years,
    checkup_count,
    hospitalized_count,
    health_facility_label,
    currently_seeking_healthcare,
    emergency_costs,
    bed_charges,
    laboratory_costs,
    imaging_costs,
    medicine_costs,
    medical_supplies_costs,
    transportation_costs,
    accomodation_costs,
    care_giver_costs,
    other_costs,
    total_costs
  )

write.xlsx(chronic_inpatient_costs, "chronic_inpatient_costs.xlsx")

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  section1a[, c("uniq_id", "v103", "v104a")], 
  by = "uniq_id"
)


chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    health_facility_label = map_chr(
      str_split(v611, ",\\s*"),
      ~ paste(recode(.x, !!!facility_map), collapse = ", ")
    )
  ) %>%
  select(-v611)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  rename(
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

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  left_join(
    section1a %>% select(uniq_id, v103, v104a),
    by = "uniq_id"
  ) %>%
  mutate(
    v103 = case_when(
      v103 == 1 ~ "Male",
      v103 == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    age = v104a
  ) %>%
  select(-v104a)

section6b1 <- section6b1 %>%
  mutate(
    uniq_id    = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  left_join(
    section6b1 %>% select(disease_id, v606, v607),
    by = "disease_id"
  ) %>%
  mutate(
    disease_onset_years = if_else(is.na(v606), 0, v606),
    currently_seeking_healthcare = case_when(
      v607 == 1 ~ "Yes",
      v607 == 2 ~ "No",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-v606, -v607)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  select(
    ID,
    disease_id,
    uniq_id,
    gender,
    v104a,
    chronic_condition,
    v604a,
    v606,
    v605a,
    v605b,
    health_facility_label,
    v607,
    emergency_costs,
    opd_charges,
    laboratory_costs,
    imaging_costs,
    medicine_costs,
    medical_supplies_costs,
    transportation_costs,
    accomodation_costs,
    care_giver_costs,
    other_costs,
    total_costs
  ) %>%
mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female"
    ), 
    v607 = case_when(
      v607 == 1 ~ "Yes", 
      v607 == 2 ~ "No"
    )
  )


chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female"
    )
  )

outpatient_na_costs <- section6b3 %>%
  filter(
    if_all(v614a:v614k, ~ is.na(.))
  )

outpatient_no_costs <- section6b3 %>%
  filter(
    if_all(v614a:v614k, ~ is.na(.) | . == 0)
  )


#CODE FOR REMAINING WORK ON HEALTH SECTION COST BREAKDOWN

chronic_outpatient_costs <- read.xlsx("health section arrangement/chronic_outpatient_costs - cost adjusted incl emergency bks 27 Jan.xlsx")

chronic_outpatient_remaining <- chronic_outpatient_costs %>%
  filter(
  (is.na(emergency_costs) | emergency_costs == 0) & 
  (is.na(opd_charges) | opd_charges == 0) &
  (is.na(laboratory_costs) | laboratory_costs == 0) &
  (is.na(imaging_costs) | imaging_costs == 0) & 
  (is.na(medicine_costs) | medicine_costs == 0) & 
  (is.na(medical_supplies_costs) | medical_supplies_costs == 0) &
  (is.na(transportation_costs) | transportation_costs == 0) &
  (is.na(accomodation_costs) | accomodation_costs == 0) &
  (is.na(care_giver_costs) | care_giver_costs == 0) &
  (is.na(other_costs) | other_costs == 0) &
  (is.na(total_costs) | total_costs == 0)
  )

section0 <- section0 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

chronic_outpatient_remaining <- merge(
  chronic_outpatient_remaining,
  section0[, c("ID", "hhid")],
  by = "ID"
)

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    hhid = paste0(psu, "-", hhld)
  )

chronic_outpatient_remaining <- merge(
  chronic_outpatient_remaining,
  section1a[, c("uniq_id", "personid", "district")],
  by = "uniq_id"
)

chronic_outpatient_remaining <- merge(
  chronic_outpatient_remaining,
  expenditure_hhld[, c("hhid", "reported_oop", "copay_amount")],
  by = "hhid"
)

chronic_outpatient_remaining <- chronic_outpatient_remaining %>%
  select(ID, hhid, personid, uniq_id, disease_id, district, everything())

write.xlsx(chronic_outpatient_remaining, "health section arrangement/chronic_outpatient_remaining.xlsx")

acute_costs <- read.xlsx("health section arrangement/acute_costs- 22 Jan- reviewed bks.xlsx")

acute_costs_remaining <- acute_costs %>%
  filter(
  (is.na(emergency_costs) | emergency_costs == 0) & 
  (is.na(opd_charges) | opd_charges == 0) &
  (is.na(laboratory_costs) | laboratory_costs == 0) &
  (is.na(imaging_costs) | imaging_costs == 0) & 
  (is.na(medicine_costs) | medicine_costs == 0) & 
  (is.na(medical_supplies_costs) | medical_supplies_costs == 0) &
  (is.na(transportation_costs) | transportation_costs == 0) &
  (is.na(accomodation_costs) | accomodation_costs == 0) &
  (is.na(care_giver_costs) | care_giver_costs == 0) &
  (is.na(other_costs) | other_costs == 0) &
  (is.na(total_costs) | total_costs == 0)
  )

acute_costs_remaining <- merge(
  acute_costs_remaining,
  expenditure_hhld[, c("hhid", "reported_oop", "copay_amount")],
  by = "hhid"
)

acute_costs_remaining <- merge(
  acute_costs_remaining, 
  section1a[, c("uniq_id", "ID", "personid", "district")],
  by = "uniq_id"
)

acute_costs_remaining <- acute_costs_remaining %>%
  select(ID, hhid, personid, uniq_id, disease_id, district, everything())

write.xlsx(acute_costs_remaining, "health section arrangement/acute_costs_remaining.xlsx")


chronic_inpatient_costs <- read.xlsx("health section arrangement/Chronic_inpatient_costs_HB include age and sex- updated BKS Jan 17.xlsx")

chronic_inpatient_remaining <- chronic_inpatient_costs %>%
  filter(
  (is.na(Emergency) | Emergency == 0) & 
  (is.na(`Bed.Charges`) | `Bed.Charges` == 0) &
  (is.na(Laboratory) | Laboratory == 0) &
  (is.na(Imaging) | Imaging == 0) & 
  (is.na(Medicines) | Medicines == 0) & 
  (is.na(`Medical.Supplies/.Devices`) | `Medical.Supplies/.Devices` == 0) &
  (is.na(`Trans.portation`) | `Trans.portation` == 0) &
  (is.na(`Food.&.Accommo.dation`) | `Food.&.Accommo.dation` == 0) &
  (is.na(`Care.Giver.Cost`) | `Care.Giver.Cost` == 0) &
  (is.na(`Other.Costs`) | `Other.Costs` == 0) &
  (is.na(`Total.cost`) | `Total.cost` == 0)
  )

chronic_inpatient_remaining <- merge(
  chronic_inpatient_remaining,
  expenditure_hhld[, c("hhid", "reported_oop", "copay_amount")],
  by = "hhid"
)

chronic_inpatient_remaining <- merge(
  chronic_inpatient_remaining, 
  section1a[, c("uniq_id", "personid", "district")], 
  by = "uniq_id"
) 

chronic_inpatient_remaining <- chronic_inpatient_remaining %>%
  select(ID, hhid, personid, uniq_id, disease_id, district, everything())

write.xlsx(chronic_inpatient_remaining, "health section arrangement/chronic_inpatient_remaining.xlsx")

acute_others <- read.xlsx("health section arrangement/Acute_illness- categorization_HB.xlsx")

acute_others_empty <- acute_others %>%
  filter(is.na(v630a))

acute_others_empty <- merge(
  acute_others_empty,
  expenditure_hhld[, c("hhid", "reported_oop", "copay_amount")],
  by = "hhid"
)

acute_others_empty <- merge(
  acute_others_empty, 
  section1a[, c("uniq_id", "ID", "personid")]
)

acute_others_empty <- acute_others_empty %>%
  select(ID, hhid, personid, uniq_id, disease_id, everything())

write.xlsx(acute_others_empty, "health section arrangement/acute_others_empty.xlsx")

chronic_checkup_counts <- read.xlsx("health section arrangement/chronic_checkup_count.xlsx")

chronic_checkup_counts <- merge(
  chronic_checkup_counts, 
  section1a[, c("uniq_id", "hhid", "ID", "personid", "district")],
  by = "uniq_id"
)

chronic_checkup_counts <- merge(
  chronic_checkup_counts, 
  expenditure_hhld[, c("hhid", "reported_oop", "copay_amount")],
  by = "hhid"
)

chronic_checkup_counts <- chronic_checkup_counts %>%
  select(ID, hhid, personid, uniq_id, disease_id, , district, everything())

write.xlsx(chronic_checkup_counts, "health section arrangement/chronic_checkup_counts.xlsx")

chronic_outpatient_remaining <- read.xlsx("health section arrangement/chronic_outpatient_remaining.xlsx")

chronic_inpatient_remaining <- read.xlsx("health section arrangement/chronic_inpatient_remaining.xlsx")

acute_costs_remaining <- read.xlsx("health section arrangement/acute_costs_remaining.xlsx")

section0 <- section0 %>%
  mutate(
    hhid = paste0(psu, "-", hhld)
  )

acute_costs_remaining <- merge(
  acute_costs_remaining, 
  section0[, c("hhid", "district")], 
  by = "hhid"
)

acute_costs_remaining <- acute_costs_remaining %>%
  mutate(
    district = case_when(
      district == 101 ~ "Taplejung",
    district == 102 ~ "Sankhuwasabha",
    district == 103 ~ "Solukhumbu",
    district == 104 ~ "Okhaldhunga",
    district == 105 ~ "Khotang",
    district == 106 ~ "Bhojpur",
    district == 107 ~ "Dhankuta",
    district == 108 ~ "Terhathum",
    district == 109 ~ "Panchthar",
    district == 110 ~ "Ilam",
    district == 111 ~ "Jhapa",
    district == 112 ~ "Morang",
    district == 113 ~ "Sunsari",
    district == 114 ~ "Udayapur",
    
    # Province 2: Madhesh
    district == 201 ~ "Saptari",
    district == 202 ~ "Siraha",
    district == 203 ~ "Dhanusha",
    district == 204 ~ "Mahottari",
    district == 205 ~ "Sarlahi",
    district == 206 ~ "Rautahat",
    district == 207 ~ "Bara",
    district == 208 ~ "Parsa",
    
    # Province 3: Bagmati
    district == 301 ~ "Dolakha",
    district == 302 ~ "Sindhupalchok",
    district == 303 ~ "Rasuwa",
    district == 304 ~ "Dhading",
    district == 305 ~ "Nuwakot",
    district == 306 ~ "Kathmandu",
    district == 307 ~ "Bhaktapur",
    district == 308 ~ "Lalitpur",
    district == 309 ~ "Kavrepalanchok",
    district == 310 ~ "Ramechhap",
    district == 311 ~ "Sindhuli",
    district == 312 ~ "Makwanpur",
    district == 313 ~ "Chitwan",
    
    # Province 4: Gandaki
    district == 401 ~ "Gorkha",
    district == 402 ~ "Manang",
    district == 403 ~ "Mustang",
    district == 404 ~ "Myagdi",
    district == 405 ~ "Kaski",
    district == 406 ~ "Lamjung",
    district == 407 ~ "Tanahu",
    district == 408 ~ "Nawalparasi (East)",
    district == 409 ~ "Syangja",
    district == 410 ~ "Parbat",
    district == 411 ~ "Baglung",
    
    # Province 5: Lumbini
    district == 501 ~ "Rukum (East)",
    district == 502 ~ "Rolpa",
    district == 503 ~ "Pyuthan",
    district == 504 ~ "Gulmi",
    district == 505 ~ "Arghakhanchi",
    district == 506 ~ "Palpa",
    district == 507 ~ "Nawalparasi (West)",
    district == 508 ~ "Rupandehi",
    district == 509 ~ "Kapilbastu",
    district == 510 ~ "Dang",
    district == 511 ~ "Banke",
    district == 512 ~ "Bardiya",
    
    # Province 6: Karnali
    district == 601 ~ "Dolpa",
    district == 602 ~ "Mugu",
    district == 603 ~ "Humla",
    district == 604 ~ "Jumla",
    district == 605 ~ "Kalikot",
    district == 606 ~ "Dailekh",
    district == 607 ~ "Jajarkot",
    district == 608 ~ "Rukum (West)",
    district == 609 ~ "Salyan",
    district == 610 ~ "Surkhet",
    
    # Province 7: Sudurpashchim
    district == 701 ~ "Bajura",
    district == 102 ~ "Bajhang", # Note: Sometimes written as 702
    district == 702 ~ "Bajhang",
    district == 703 ~ "Darchula",
    district == 704 ~ "Baitadi",
    district == 705 ~ "Dadeldhura",
    district == 706 ~ "Doti",
    district == 707 ~ "Achham",
    district == 708 ~ "Kailali",
    district == 709 ~ "Kanchanpur",
    
    TRUE ~ "Unknown"
    )
  )

chronic_outpatient_costs <- read_dta("clean_data/section6b3.dta")
chronic_inpatient_costs <- read_dta("clean_data/section6b4.dta")
acute_costs <- read_dta("clean_data/section6c4.dta")
s6b1 <- read_dta("clean_data/section6b1.dta")
s6c1 <- read_dta("clean_data/section6c1.dta")

acute_costs <- acute_costs %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  )

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

s6b1 <- s6b1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

s6c1 <- s6c1 %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  )

acute_costs <- acute_costs %>%
  select(ID, uid, hhid, personid, uniq_id, disease_id, v630, v651a, v651b, v651c, v651d, v651e, v651f, v651g, v651h, v651i, v651j, v651k)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  select(ID, uid, hhid, personid, uniq_id, disease_id, v604, v618a, v618b, v618c, v618d, v618e, v618f, v618g, v618h, v618i, v618j, v618k)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  select(ID, uid, hhid, personid, uniq_id, disease_id, v604, v614a, v614b, v614c, v614d, v614e, v614f, v614g, v614h, v614i, v614j, v614k)

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  s6b1[, c("disease_id", "v605a", "v605b", "v606", "v607", "v611")],
  by = "disease_id"
)

chronic_inpatient_costs <- merge(
  chronic_inpatient_costs, 
  s6b1[, c("disease_id", "v605a", "v605b", "v606", "v607", "v611")],
  by = "disease_id"
)

acute_costs <- merge(
  acute_costs, 
  s6c1[, c("disease_id", "v631a", "v631b", "v636")],
  by = "disease_id"
)

facility_map <- c(
  "1"  = "Health Post", 
  "2"  = "Primary Health Centre", 
  "3"  = "Government Hospital", 
  "4"  = "Government Outreach Clinic", 
  "5"  = "Government Ayurveda Centre",
  "6"  = "Pharmacy/Drug Seller", 
  "7"  = "Private Clinic", 
  "8"  = "Private/Community Hospital", 
  "9"  = "Private Ayurveda Centre", 
  "10" = "Health Worker's Home", 
  "11" = "Alternative/Traditional Healer", 
  "12" = "Abroad (India/Other)", 
  "96" = "Others"
)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  mutate(
    health_facility_label = map_chr(
      str_split(v611, ",\\s*"),
      ~ paste(recode(.x, !!!facility_map), collapse = ", ")
    )
  ) %>%
  select(-v611)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    health_facility_label = map_chr(
      str_split(v611, ",\\s*"),
      ~ paste(recode(.x, !!!facility_map), collapse = ", ")
    )
  ) %>%
  select(-v611)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  rename(
    emergency_costs          = v618a,
    bed_charges              = v618b, 
    laboratory_costs         = v618c, 
    imaging_costs            = v618d, 
    medicine_costs           = v618e, 
    medical_supplies_costs   = v618f, 
    transportation_costs     = v618g, 
    accomodation_costs       = v618h,
    care_giver_costs         = v618i, 
    other_costs              = v618j, 
    total_costs              = v618k
  )

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  rename(
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

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  rename(chronic_condition = v604) %>%
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
      TRUE ~ NA_character_
    )
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  rename(chronic_condition = v604) %>%
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
      TRUE ~ NA_character_
    )
  )


acute_costs <- acute_costs %>%
  mutate(
    v631a_d = as.Date(v631a, format = "%Y/%m/%d"),
    v631b_d = as.Date(v631b, format = "%Y/%m/%d"),
    disease_duration = as.integer(v631b_d - v631a_d)
  ) %>%
  select(-v631a_d, -v631b_d, -v631a, -v631b)

acute_costs <- acute_costs %>%
  mutate(
    v630 = case_when(
      v630 == 1  ~ "Diarrhoea",
      v630 == 2  ~ "Typhoid",
      v630 == 3  ~ "Dengue",
      v630 == 4  ~ "Malaria",
      v630 == 5  ~ "Acute Respiratory Infection",
      v630 == 6  ~ "Cold/Flu/Fever",
      v630 == 7  ~ "Pneumonia",
      v630 == 8  ~ "Measles",
      v630 == 9  ~ "Jaundice",
      v630 == 10 ~ "UTI",
      v630 == 11 ~ "Dental Problem",
      v630 == 12 ~ "Acute Eye Infection",
      v630 == 13 ~ "Acute Ear Infection",
      v630 == 14 ~ "Skin Disease",
      v630 == 15 ~ "Injury",
      v630 == 16 ~ "Accident",
      v630 == 17 ~ "Other Fever",
      v630 == 18 ~ "Animal Bite", 
      v630 == 19 ~ "Arthritis", 
      v630 == 20 ~ "Blood Diseases", 
      v630 == 21 ~ "Cancer",
      v630 == 22 ~ "Gastrointestinal Diseases",
      v630 == 23 ~ "Congenital Anomaly",
      v630 == 24 ~ "COPD",
      v630 == 25 ~ "Pregnancy/Postpartum", 
      v630 == 26 ~ "Disability", 
      v630 == 27 ~ "ENT", 
      v630 == 28 ~ "Eye Problems",
      v630 == 29 ~ "Fungal Infections",
      v630 == 30 ~ "Geriatric Problem", 
      v630 == 31 ~ "Gynecological Problem", 
      v630 == 32 ~ "Heart Disease", 
      v630 == 33 ~ "Hernia", 
      v630 == 34 ~ "HIV",
      v630 == 35 ~ "Infectious Disease", 
      v630 == 36 ~ "Kidney Disease", 
      v630 == 37 ~ "Liver Disease",
      v630 == 38 ~ "Lungs Disease", 
      v630 == 39 ~ "Male Reproductive Diseases", 
      v630 == 40 ~ "Mental Illness", 
      v630 == 41 ~ "Neurological Conditions",
      v630 == 42 ~ "Uric Acid", 
      v630 == 43 ~ "Warts", 
      v630 == 44 ~ "Worms",
      TRUE ~ NA_character_
    ), 
    v636 = case_when(
      v636 == 1  ~ "Health Post",
      v636 == 2  ~ "Primary Health Centre",
      v636 == 3  ~ "Government Hospital",
      v636 == 4  ~ "Government Outreach Clinic",
      v636 == 5  ~ "Government Ayurveda Centre",
      v636 == 6  ~ "Pharmacy/Drug Seller",
      v636 == 7  ~ "Private Clinic",
      v636 == 8  ~ "Private/Community Hospital",
      v636 == 9  ~ "Private Ayurveda Centre",
      v636 == 10 ~ "Health Worker’s Home",
      v636 == 11 ~ "Alternative/Traditional Healer",
      v636 == 12 ~ "Abroad (India/Other)",
      v636 == 13 ~ "Other (Specify)",
      v636 == 14 ~ "Others",
      TRUE       ~ NA_character_
    )
  ) 

s1a <- read_dta("clean_data/section1a.dta")

chronic_inpatient_costs <- merge(
  chronic_inpatient_costs, 
  s1a[, c("uniq_id", "v103", "v104a")],
  by = "uniq_id"
)

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  s1a[, c("uniq_id", "v103", "v104a")],
  by = "uniq_id"
)

acute_costs <- merge(
  acute_costs, 
  s1a[, c("uniq_id", "v103", "v104a")],
  by = "uniq_id"
)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  mutate(
    gender = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female", 
      TRUE ~ NA_character_
    ),
    receiving_treatment = case_when(
      v607 == 1 ~ "Yes", 
      v607 == 2 ~ "No", 
      TRUE ~ NA_character_
    )
  ) %>%
  rename(
    age = v104a, 
    checkup_counts = v605a, 
    hospitalized_counts = v605b, 
    disease_onset_years = v606
  ) %>%
  select(-v607, -v103)


chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    gender = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female", 
      TRUE ~ NA_character_
    ),
    receiving_treatment = case_when(
      v607 == 1 ~ "Yes", 
      v607 == 2 ~ "No", 
      TRUE ~ NA_character_
    )
  ) %>%
  rename(
    age = v104a, 
    checkup_counts = v605a, 
    hospitalized_counts = v605b, 
    disease_onset_years = v606
  ) %>%
  select(-v607, -v103)

acute_costs <- acute_costs %>%
  mutate(
    gender = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female", 
      TRUE ~ NA_character_
    )
  ) %>%
  rename(
    age = v104a
  ) %>%
  select(-v103)

write.xlsx(chronic_inpatient_costs, "chronic_inpatient_costs.xlsx")
write.xlsx(chronic_outpatient_costs, "chronic_outpatient_costs.xlsx")
write.xlsx(acute_costs, "acute_costs.xlsx")

