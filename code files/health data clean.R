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

in_dir <- "stata_data"

files <- list.files(in_dir, pattern = "\\.dta$", full.names = TRUE)

sections <- lapply(files, read_dta)

names(sections) <- tools::file_path_sans_ext(basename(files))

list2env(sections, .GlobalEnv)

rm(sections)

gc()

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
)
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

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  section1a[, c("uniq_id", "v103", "v104a")],
  by = "uniq_id"
)

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  select(
    id, disease_id, uniq_id, v103, v604, v605a, v605b, v104a, v606, v607, v611, everything()
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

#######################################################################################################

acute_costs <- section6c4 %>%
  filter(!is.na(v630)) %>%
  mutate(
    across(v651a:v651k, ~ na_if(.x, 0)),
    hhid = paste0(psu, "-", hhld), 
    uniq_id = paste0(psu, "-", hhld, "-", v101), 
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  )
  

section6c1 <- section6c1 %>%
  mutate(
    v631a_d = as.Date(v631a, format = "%Y/%m/%d"),
    v631b_d = as.Date(v631b, format = "%Y/%m/%d"),
    disease_duration = as.integer(v631b_d - v631a_d), 
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v630)
  ) %>%
  select(-v631a_d, -v631b_d)

acute_costs <- acute_costs %>%
  select(id, uid, hhid, personid, uniq_id, disease_id, v630, v651a, v651b, v651c, v651d, v651e, v651f, v651g, v651h, v651i, v651j, v651k)

acute_costs <- merge(
  acute_costs, 
  section6c1[, c("disease_id", "disease_duration", "v636", "district", "palika")],
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

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

acute_costs <- merge(
  acute_costs,
  section1a[, c("uniq_id", "v103", "v104a")], 
  by = "uniq_id"
)

acute_costs <- acute_costs %>%
  mutate(
    v103 = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female"    )
  )

write.xlsx(acute_costs, "acute_costs.xlsx")  

acute_costs <- read.xlsx("acute_costs.xlsx")

chronic_inpatient_costs <- chronic_inpatient_costs %>%
   mutate(
    district_name = case_when(
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
      district == 201 ~ "Saptari",
      district == 202 ~ "Siraha",
      district == 203 ~ "Dhanusha",
      district == 204 ~ "Mahottari",
      district == 205 ~ "Sarlahi",
      district == 206 ~ "Rautahat",
      district == 207 ~ "Bara",
      district == 208 ~ "Parsa",
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
      district == 312 ~ "Makawanpur",
      district == 313 ~ "Chitwan",
      district == 401 ~ "Gorkha",
      district == 402 ~ "Manang",
      district == 403 ~ "Mustang",
      district == 404 ~ "Myagdi",
      district == 405 ~ "Kaski",
      district == 406 ~ "Lamjung",
      district == 407 ~ "Tanahu",
      district == 408 ~ "Nawalparasi East",
      district == 409 ~ "Syangja",
      district == 410 ~ "Parbat",
      district == 411 ~ "Baglung",
      district == 501 ~ "Rukum East",
      district == 502 ~ "Rolpa",
      district == 503 ~ "Pyuthan",
      district == 504 ~ "Gulmi",
      district == 505 ~ "Arghakhanchi",
      district == 506 ~ "Palpa",
      district == 507 ~ "Nawalparasi West",
      district == 508 ~ "Rupandehi",
      district == 509 ~ "Kapilvastu",
      district == 510 ~ "Dang",
      district == 511 ~ "Banke",
      district == 512 ~ "Bardiya",
      district == 601 ~ "Dolpa",
      district == 602 ~ "Mugu",
      district == 603 ~ "Humla",
      district == 604 ~ "Jumla",
      district == 605 ~ "Kalikot",
      district == 606 ~ "Dailekh",
      district == 607 ~ "Jajarkot",
      district == 608 ~ "Rukum West",
      district == 609 ~ "Salyan",
      district == 610 ~ "Surkhet",
      district == 701 ~ "Bajura",
      district == 702 ~ "Bajhang",
      district == 703 ~ "Darchula",
      district == 704 ~ "Baitadi",
      district == 705 ~ "Dadeldhura",
      district == 706 ~ "Doti",
      district == 707 ~ "Achham",
      district == 708 ~ "Kailali",
      district == 709 ~ "Kanchanpur",
      TRUE ~ NA_character_   # fallback for any unmatched code
    )
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
    doof_accomodation_costs       = v614h,
    care_giver_costs         = v614i, 
    other_costs              = v614j, 
    total_costs              = v614k
  )

  palika_code <- c(
    10101,10102,10103,10104,10105,10106,10107,10108,10109,
    10201,10202,10203,10204,10205,10206,10207,10208,10209,10210,
    10301,10302,10303,10304,10305,10306,10307,10308,
    10401,10402,10403,10404,10405,10406,10407,10408,
    10501,10502,10503,10504,10505,10506,10507,10508,10509,10510,
    10601,10602,10603,10604,10605,10606,10607,10608,10609,
    10701,10702,10703,10704,10705,10706,10707,
    10801,10802,10803,10804,10805,10806,
    10901,10902,10903,10904,10905,10906,10907,10908,
    11001,11002,11003,11004,11005,11006,11007,11008,
    11009,11010,
    11101,11102,11103,11104,11105,11106,11107,11108,11109,11110,11111,11112,11113,11114,11115,
    11201,11202,11203,11204,11205,11206,11207,11208,11209,11210,11211,11212,11213,11214,11215,11216,11217,
    11301,11302,11303,11304,11305,11306,11307,11308,11309,11310,11311,11312,
    11401,11402,11403,11404,11405,11406,11407,11408,
    20101,20102,20103,20104,20105,20106,20107,20108,20109,20110,20111,20112,20113,20114,20115,20116,20117,20118,
    20201,20202,20203,20204,20205,20206,20207,20208,20209,20210,20211,
    20301,20302,20303,20304,20305,20306,20307,20308,20309,20310,20311,20312,20313,20314,20315,20316,20317,20318,
    20401,20402,20403,20404,20405,20406,20407,20408,20409,20410,20411,20412,20413,20414,20415,
    20501,20502,20503,20504,20505,20506,20507,20508,20509,20510,20511,20512,20513,20514,20515,20516,20517,20518,20519,20520,
    20601,20602,20603,20604,20605,20606,20607,20608,20609,20610,20611,20612,20613,20614,20615,20616,20617,20618,
    20701,20702,20703,20704,20705,20706,20707,20708,20709,20710,20711,20712,20713,20714,20715,20716,20801,20802,20803,20804,20805,20806,20807,20808,20809,20810,20811,20812,20813,20814,
    30101,30102,30103,30104,30105,30106,30107,30108,30109,
    30201,30202,30203,30204,30205,30206,30207,30208,30209,30210,30211,30212,
    30301,30302,30303,30304,30305,
    30401,30402,30403,30404,30405,30406,30407,30408,30409,30410,30411,30412,30413,
    30501,30502,30503,30504,30505,30506,30507,30508,30509,30510,30511,30512,
    30601,30602,30603,30604,30605,30606,30607,30608,30609,30610,30611,
    30701,30702,30703,30704,
    30801,30802,30803,30804,30805,30806,
    30901,30902,30903,30904,30905,30906,30907,30908,30909,30910,30911,
    31001,31002,31003,31004,31005,31006,31007,31008,
    31101,31102,31103,31104,31105,31106,31107,31108,31109,
    31201,31202,31203,31204,31205,31206,31207,31208,31209,31210,
    31301,31302,31303,31304,31305,31306,31307,
    40101,40102,40103,40104,40105,40106,40107,40108,40109,40110,40111,
    40201,40202,40203,40204,
    40301,40302,40303,40304,40305,
    40401,40402,40403,40404,40405,40406,
    40501,40502,40503,40504,40505,
    40601,40602,40603,40604,40605,40606,40607,40608,
    40701,40702,40703,40704,40705,40706,40707,40708,40709,40710,
    40801,40802,40803,40804,40805,40806,40807,40808,
    40901,40902,40903,40904,40905,40906,40907,40908,40909,40910,40911,
    41001,41002,41003,41004,41005,41006,41007,
    41101,41102,41103,41104,41105,41106,41107,41108,41109,41110,
    50101,50102,50103,50201,50202,50203,50204,50205,50206,50207,50208,50209,50210,
    50301,50302,50303,50304,50305,50306,50307,50308,50309,
    50401,50402,50403,50404,50405,50406,50407,50408,50409,50410,50411,50412,50413,
    50501,50502,50503,50504,50505,50506,50601,50602,50603,50604,50605,50606,50607,50608,50609,50610,
    50701,50702,50703,50704,50705,50706,50707,
    50801,50802,50803,50804,50805,50806,50807,50808,50809,50810,50811,50812,50813,50814,50815,50816,
    50901,50902,50903,50904,50905,50906,50907,50908,50909,50910,
    51001,51002,51003,51004,51005,51006,51007,51008,51009,51010,
    51101,51102,51103,51104,51105,51106,51107,51108,
    51201,51202,51203,51204,51205,51206,51207,51208,
    60101,60102,60103,60104,60105,60106,60107,60108,
    60201,60202,60203,60204,
    60301,60302,60303,60304,60305,60306,60307,
    60401,60402,60403,60404,60405,60406,60407,
    60501,60502,60503,60504,60505,60506,60507,60508,60509,
    60601,60602,60603,60604,60605,60606,60607,60608,60609,60610,60611,
    60701,60702,60703,60704,60705,60706,60707,
    60801,60802,60803,60804,60805,60806,
    60901,60902,60903,60904,60905,60906,60907,60908,60909,60910,
    61001,61002,61003,61004,61005,61006,61007,61008,61009,
    70101,70102,70103,70104,70105,70106,70107,70108,70109,
    70201,70202,70203,70204,70205,70206,70207,70208,70209,70210,70211,70212,
    70301,70302,70303,70304,70305,70306,70307,70308,70309,
    70401,70402,70403,70404,70405,70406,70407,70408,70409,70410,
    70501,70502,70503,70504,70505,70506,70507,
    70601,70602,70603,70604,70605,70606,70607,70608,70609,
    70701,70702,70703,70704,70705,70706,70707,70708,70709,70710,
    70801,70802,70803,70804,70805,70806,70807,70808,70809,70810,70811,70812,70901,70902,70903,70904,70905,70906,70907,70908,70909
  )
  palika_name <- c(
    "Faktanglung","Mikhwakhola","Meringden","Maiwakhola","Athrai Tribeni","Fungling","Pathibhara Yangwarak","Sirijunga","Sidingwa",
    "Bhotkhola","Makalu","Silichong","Chichila","Sabhapokhari","Khandbari","Panchkhapan","Chainpur","Madi","Dharmadevi","Khumbu Pasanglhamu",
    "Mahakulung","Sotang","Mapya Dudhakoshi","Thulung Dudhakoshi","Necha Salyan","Solu Dudhakunda","Likhu Pike","Chisankhu Gadhi",
    "Siddhicharan","Molung","Khiji Demwa","Likhu","Champadevi","Sunkoshi","Manebhanjyang","Kepilasgadhi",
    "Aiselukharka","Rawa Besi","Halesi Tuwachung","Diktel Rupakot Majhuwagadhi","Sakela","Diprung Chuichumba","Khotehang","Jante Dhunga","Baraha Pokhari",
    "Sadananda","Salpa Silichho","Temke Maiyum","Bhojpur","Arun","Pauwa Dungma","Ramprasad Rai","Hatuwagadhi","Aamchowk","Mahalaxmi","Pakhribas",
    "Chhathar Jorpati","Dhankuta","Sahidbhumi","Sangurigadhi","Chaubise","Aatharai",
    "Fedap","Menchhayayem","Myanglung","Laligurans","Chhathar","Yangwarak","Hilihang","Falelung","Fidim","Falgunanda","Kummayek","Tumwewa","Miklajung",
    "Maijogmai","Sandakpur","Illam","Deumai","Fakfokthum","Mangsebung","Chulachuli","Mai","Suryodaya","Rong","Mechinagar","Buddhashanti","Arjundhara",
    "Kankai","Shivasatakshi","Kamal","Damak","Gauradaha","Gaurigunj","Jhapa","Barhadashi","Birtamod","Haldibari","Bhadrapur","Kachankawal",
    "Miklajung","Letang","Kerabari","Sundarharaincha","Belbari","Kanepokhari","Pathari Sanischare","Urlabari","Ratuwamai","Sunawarsi","Rangeli","Gramthan",
    "Budhiganga","Biratnagar","Katahari","Dhanpalthan","Jahada","Dharan","Barah Kshetra","Koshi","Bhokraha Narasingha","Ramdhuni","Itahari","Duhabi",
    "Gadhi","Inaruwa","Harinagar","Dewangunj","Barju","Belaka","Chaudandigadhi","Triyuga","Rautamai","Limchungbung","Tapli","Katari","Udayapurgadhi","Saptakoshi",
    "Kanchanrup","Agnisair Krishnasabaran","Rupani","Sambhunath","Khadak","Surunga","Balan-Bihul","Bodebarsain","Dakneshwori","Rajgadh","Bishnupur","Rajbiraj",
    "Mahadeva","Tirahut","Hanumannagar Kankalini","Tilathi Koiladi","Chhinnamasta","Lahan",
    "Dhangadhimai","Golbazar","Mirchaiya","Karjanha","Kalyanpur","Naraha","Bishnupur","Arnama","Sukhipur","Laxmipur Patari","Sakhuwanankarkatti","Bhagawanpur",
    "Nawarajpur","Bariyarpatti","Aurahi","Siraha","Ganeshman Charnath","Dhanusadham","Mithila","Bateshwor","Chhireshwornath","Laxminiya","Mithila Bihari","Hansapur",
    "Sabaila","Sahidnagar","Kamala","Janak Nandini","Bideha","Aurahi","Janakpurdham","Dhanauji","Nagarain","Mukhiyapatti Musaharmiya","Bardibas","Gausala",
    "Sonama","Aurahi","Bhanggaha","Loharpatti","Balawa","Ram Gopalpur","Samsi","Manara Shisawa","Ekdara","Mahottari","Pipara","Matihani","Jaleshwor","Lalbandi",
    "Hariwan","Bagmati","Barahathawa","Haripur","Ishworpur","Haripurwa","Parsa","Bramhapuri","Chandranagar","Kabilasi","Chakraghatta","Basbariya","Dhankaul","Ramnagar",
    "Balara","Godaita","Bishnu","Kaudena","Malangawa","Chandrapur","Gujara","Fatuwa Bijayapur","Katahariya","Brindaban","Gadhimai","Madhav Narayan","Garuda","Dewahi Gonahi",
    "Maulapur","Baudhimai","Paroha","Rajpur","Yamunamai","Durga Bhagawati","Rajdevi","Gaur","Ishanath","Nijgadh","Kolhawi","Jitpur Simara","Parwanipur","Prasauni",
    "Bishrampur","Pheta","Kalaiya","Karaiyamai","Baragadhi","Adarsha Kotwal","Simroungadh","Pacharauta","Mahagadhimai","Devtal","Subarna","Thori",
    "Jirabhawani","Jagarnathpur","Paterwa Sugauli","Sakhuwa Prasauni","Parsagadhi","Birgunj","Bahudarmai","Pokhariya","Kalikamai","Dhobini","Chhipaharmai","Pakaha Mainapur",
    "Bindabasini","Gaurishankar","Bigu","Kalinchowk","Baiteshwor","Jiri","Tamakoshi","Melung","Shailung","Bhimeshwor","Bhotekoshi","Jugal","Panchpokhari Thanpal","Helambu",
    "Melamchi","Indrawati","Chautara Sangachowkgadhi","Balephi","Barabise","Tripurasundari","Lisankhu Pakhar","Sunkoshi","Gosainkunda","Aamachhodingmo","Uttargaya","Kalika",
    "Naukunda","Rubi Valley","Khaniyabash","Ganga Jamuna","Tripurasundari","Netrawati Dabjong","Nilkantha","Jwalamukhi","Siddhalek","Benighat Rorang","Gajuri","Galchhi",
    "Thakre","Dhunibeshi","Dupcheshwor","Tadi","Suryagadhi","Bidur","Kispang","Myagang","Tarakeshwor","Belkotgadhi","Likhu","Panchakanya","Shivapuri","Kakani",
    "Shankharapur","Kageshwori Manohara","Gokarneshwor","Budhanilkantha","Tokha","Tarakeshwor","Nagarjun","Kathmandu","Kirtipur","Chandragiri","Dakshinkali","Changunarayan",
    "Bhaktapur","Madhyapur Thimi","Suryabinayak","Mahalaxmi","Lalitpur","Godawari","Konjyosom","Mahankal","Bagmati","Chauri Deurali","Bhumlu","Mandan Deupur","Banepa",
    "Dhulikhel","Panchkhal","Temal","Namobuddha","Panauti","Bethanchowk","Roshi","Mahabharat","Khanikhola","Umakunda","Gokulganga","Likhu Tamakoshi","Ramechhap","Manthali",
    "Khandadevi","Doramba","Sunapati","Dudhouli","Phikkal","Tinpatan","Golanjor","Kamalamai","Sunkoshi","Ghyanglekh","Marin","Hariharpurgadhi","Indrasarowar","Thaha",
    "Kailash","Raksirang","Manahari","Hetauda","Bhimphedi","Makwanpurgadhi","Bakaiya","Bagmati","Rapti","Kalika","Ichchha Kamana","Bharatpur","Ratnanagar","Khairahani",
    "Madi","Chumanubri","Ajirkot","Barpak Sulikot","Dharche","Aarughat","Bhimsen Thapa","Siranchowk","Palungtar","Gorkha","Sahid Lakhan","Gandaki","Narpa Bhumi",
    "Manang Dishyang","Chame","Nashon","Lo-Ghekar Damodarkunda","Gharpajhong","Baragung Muktikshetra","Lomanthang","Thasang","Annapurna","Raghuganga","Dhawalagiri",
    "Malika","Mangala","Beni","Madi","Machhapuchchhre","Annapurna","Pokhara","Rupa","Dordi","Marsyangdi","Kwaholasothar","Madhya Nepal","Beshi Shahar","Sundarbazar",
    "Rainas","Dudhapokhari","Bhanu","Byas","Myagde","Shuklagandaki","Bhimad","Ghiring","Rhishing","Devghat","Bandipur","Aanbukhaireni","Gaindakot","Bulingtar",
    "Baudikali","Hupsekot","Devchuli","Kawasoti","Madhyabindu","Binayi Triveni","Putalibazar","Phedikhola","Aandhikhola","Arjunchaupari","Bhirkot","Biruwa",
    "Harinas","Chapakot","Waling","Galyang","Kaligandaki","Modi","Jaljala","Kushma","Phalebas","Mahashila","Bihadi","paiyu","Baglung","Kathekhola","Tarakhola",
    "Tamankhola","Dhorpatan","Nisikhola","Badigad","Galkot","Bareng","Jaimuni","Putha Uttarganga","Sisne","Bhoome","Sunchhahari","Thawang","Paribartan","Gangadev",
    "Madi","Tribeni","Rolpa","Runtigadhi","Sunil Smriti","Lungri","Gaumukhi","Naubahini","Jhimruk","Pyuthan","Sworgadwari","Mandavi","Mallarani","Aairawati",
    "Sarumarani","Kaligandaki","Satyawoti","Chandrakot","Musikot","Isma","Malika","Madane","Dhurkot","Resunga","Gulmi Durbar","Chhatrakot","Rurukshetra","Chhatradev",
    "Malarani","Bhumikasthan","Sandhikharka","Panini","Shitganga","Rampur","Purbakhola","Rambha","Baganaskali","Tansen","Ribdikot","Rainadevi Chhahara","Tinau","Mathagadhi",
    "Nisdi","Bardaghat","Sunwal","Ramgram","Palhinandan","Sarawal","Pratappur","Susta","Devdaha","Butwal","Sainamaina","Kanchan","Gaidahawa","Shuddhodhan","Siyari",
    "Tilottama","Omsatiya","Rohindi","Sidharthanagar","Mayadevi","Lumbini Sanskritik","Kotahimai","Sammarimai","Marchawari","Banganga","Buddhabhumi","Shivaraj","Bijayanagar",
    "Krishnanagar","Maharajgang","Kapilvastu","Yesodhara","Mayadevi","Suddhodhan","Bangalachuli","Ghorahi","Tulsipur","Shantinagar","Babai","Dangisharan","Lamahi","Rapti",
    "Gadhawa","Rajpur","Rapti Sonari","Kohalpur","Baijanath","Khajura","Janaki","Nepalgunj","Duduwa","Narainapur","Bansgadhi","Barbardiya","Thakurbaba","Geruwa","Rajapur",
    "Madhuwan","Gulariya","Badhaiyatal","Dolpo Buddha","Shey Phoksundo","Jagadulla","Mudkechula","Tripurasundari","Thuli Bheri","Kaike","Chharka Tangsong","Mugum Karmarog",
    "Chhayanath Rara","Soru","Khatyad","Chankheli","Kharpunath","Simkot","Namkha","Sarkegad","Adanchuli","Tanjakot","Patarasi","Kanka Sundari","Sinja","Chandannath",
    "Guthichaur","Tatopani","Tila","Hima","Pachaljharana","Raskot","Sanni Tribeni","Naraharinath","Khandachakra","Tilagupha","Mahawai","Shuva Kalika","Naumule","Mahabu",
    "Bhairabi","Thantikandh","Aathbis","Chamunda Bindrasaini","Dullu","Narayan","Bhagawatimai","Dungeshwor","Gurans","Barekot","Kuse","Junichande","Chhedagad","Shivalaya",
    "Bheri","Nalagad","Aathabisakot","Sanibheri","Banphikot","Musikot","Tribeni","Chaurjahari","Darma","Kumakha","Banagad Kupinde","Siddha Kumakha","Bagachaur","Chhatreswori",
    "Sharada","Kalimati","Tribeni","Kapurkot","Simta","Chingad","Lekabeshi","Gurbhakot","Bheriganga","Birendranagar","Barahatal","Panchapuri","Chaukune","Himali","Gaumul","Budhinanda",
    "Swamikartik Khapar","Jagannath","Badimalika","Khaptad Chhededaha","Budhiganga","Tribeni","Saipal","Bungal","Surma","Talkot","Masta","Jayaprithivi","Chhabispathibhera",
    "Durgathali","Kedarsyun","Bitthadchir","Thalara","Khaptad Chhanna","Byas","Duhun","Mahakali","Naugad","Apihimal","Marma","Shailyashikhar","Malikarjun","Lekam","Dilasaini",
    "Dogadakedar","Purchaudi","Surnaya","Dashrathchand","Pancheshwor","Shivanath","Melauli","Patan","Sigas","Nawadurga","Amargadhi","Ajayameru","Bhageshwor","Parashuram","Aalital",
    "Ganyapdhura","Purbichauki","Sayal","Aadarsha","Shikhar","Dipayal Silgadhi","K.I.Singh","Bogatan Phudsil","Badikedar","Jorayal","Panchadewal Binayak","Ramaroshan","Mellekh",
    "Sanphebagar","Chaurpati","Mangalsen","Bannigadhi Jayagadh","Kamalbazar","Dhakari","Turmakhand","Mohanyal","Chure","Godawari","Gauriganga","Ghodaghodi","Bardagoriya","Lamki Chuha",
    "Janaki","Joshipur","Tikapur","Bhajani","Kailari","Dhangadhi","Krishnapur","Shuklaphata","Bedkot","Bhimdatta","Dodhara Chandani","Laljhadi","Punarbas","Belauri","Beldandi"
  )

section1a <- section1a %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101)
  )

section6b1 <- section6b1 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  section6b1[, c("disease_id", "v605a", "v605b", "v606", "v607", "v611", "district")],
  by = "disease_id"
)

chronic_outpatient_costs <- merge(
  chronic_outpatient_costs, 
  section1a[, c("uniq_id", "v103", "v104a")],
  by = "uniq_id"
)



chronic_outpatient_costs <- chronic_outpatient_costs %>%
  select(
    id, disease_id, uniq_id, v103, v604, v605a, v605b, v104a, v606, v607, v611, everything()
  )



chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    v604 = case_when(
      v604 == 1  ~ "Heart Diseases",
      v604 == 2  ~ "Hypertension",
      v604 == 3  ~ "Diabetes",
      v604 == 4  ~ "Asthma/COPD",
      v604 == 5  ~ "Rheumatism/ Arthritis",
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
      v604 == 29 ~ "Infectious Disease",
      v604 == 30 ~ "Disability",
      v604 == 31 ~ "Geriatric Problems",
      v604 == 96 ~ "Other",
      TRUE       ~ NA_character_  
    ),
    v103 = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female", 
      TRUE ~ NA_character_
    )
  )

chronic_outpatient_costs <- read.xlsx("chronic_outpatient_costs.xlsx")

chronic_outpatient_costs <- chronic_outpatient_costs %>%
  mutate(
    v607 = case_when(
      v607 == 1 ~ "Yes",
      v607 == 2 ~ "No",
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


chronic_inpatient_costs <- section6b4 %>%
  mutate(
    uniq_id = paste0(psu, "-", hhld, "-", v101),
    disease_id = paste0(psu, "-", hhld, "-", v101, "-", v604)
  )

chronic_inpatient_costs <- merge(
  chronic_inpatient_costs, 
  section6b1[, c("disease_id", "v605a", "v605b", "v606", "v607", "v611", "district")],
  by = "disease_id"
)

chronic_inpatient_costs <- merge(
  chronic_inpatient_costs, 
  section1a[, c("uniq_id", "v103", "v104a")],
  by = "uniq_id"
)

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  mutate(
    v604 = case_when(
      v604 == 1  ~ "Heart Diseases",
      v604 == 2  ~ "Hypertension",
      v604 == 3  ~ "Diabetes",
      v604 == 4  ~ "Asthma/COPD",
      v604 == 5  ~ "Rheumatism/ Arthritis",
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
      v604 == 29 ~ "Infectious Disease",
      v604 == 30 ~ "Disability",
      v604 == 31 ~ "Geriatric Problems",
      v604 == 96 ~ "Other",
      TRUE       ~ NA_character_  
    ),
    v103 = case_when(
      v103 == 1 ~ "Male", 
      v103 == 2 ~ "Female", 
      TRUE ~ NA_character_
    )
  )

chronic_inpatient_costs <- chronic_inpatient_costs %>%
  mutate(
    v607 = case_when(
      v607 == 1 ~ "Yes",
      v607 == 2 ~ "No",
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

write.xlsx(chronic_inpatient_costs, "chronic_inpatient_costs.xlsx")
