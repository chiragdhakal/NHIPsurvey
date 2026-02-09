rm(list = ls())

section3a <- read_dta("stata_data/section3a.dta")

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

###############################################################################################

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
    ), 
    total_grains = if_else(
      total_grains == 0, NA_real_, total_grains
    )
  )

summary(food_grain)

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
    ), 
    total_lentils = if_else(
      total_lentils == 0, NA_real_, total_lentils
    )
  ) 

summary(lentils)

meat <- section3a %>%
  filter(v301 == 3)

meat <- meat %>%
  mutate(
    total_meat = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ), 
    total_meat = if_else(
      total_meat == 0, NA_real_, total_meat
    )
  )

summary(meat)

eggs_milk <- section3a %>%
  filter(v301 == 4) %>%
  mutate(
    total_eggs_milk = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_eggs_milk = if_else(
      total_eggs_milk == 0, NA_real_, total_eggs_milk
    )
  )

summary(eggs_milk)

ghee <- section3a %>%
  filter(v301 == 5) %>%
  mutate(
    total_ghee = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_ghee = if_else(
      total_ghee == 0, NA_real_, total_ghee
    )
  )

summary(ghee, na.rm = TRUE)

oil <- section3a %>%
  filter(v301 == 6) %>%
  mutate(
    total_oil = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_oil = if_else(
      total_oil == 0, NA_real_, total_oil
    )
  )

summary(oil, na.rm = TRUE)

fruits <- section3a %>%
  filter(v301 == 7) %>%
  mutate(
    total_fruits = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_fruits = if_else(
      total_fruits == 0, NA_real_, total_fruits
    )
  )

summary(fruits, na.rm = TRUE)

vegetable <- section3a %>%
  filter(v301 == 8) %>%
  mutate(
    total_vegetable = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_vegetable = if_else(
      total_vegetable == 0, NA_real_, total_vegetable
    )
  )

summary(vegetable, na.rm = TRUE)

sweet <- section3a %>%
  filter(v301 == 9) %>%
  mutate(
    total_sweet = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_sweet = if_else(
      total_sweet == 0, NA_real_, total_sweet
    )
  )

summary(sweet, na.rm = TRUE)

spices <- section3a %>%
  filter(v301 == 10) %>%
  mutate(
    total_spices = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_spices = if_else(
      total_spices == 0, NA_real_, total_spices
    )
  )

summary(spices, na.rm = TRUE)

tea <- section3a %>%
  filter(v301 == 11) %>%
  mutate(
    total_tea = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_tea = if_else(
      total_tea == 0, NA_real_, total_tea
    )
  )

summary(tea, na.rm = TRUE)

non_alcoholic <- section3a %>%
  filter(v301 == 12) %>%
  mutate(
    total_non_alcoholic = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_non_alcoholic = if_else(
      total_non_alcoholic == 0, NA_real_, total_non_alcoholic
    )
  )

summary(non_alcoholic, na.rm = TRUE)

alcoholic <- section3a %>%
  filter(v301 == 13) %>%
  mutate(
    total_alcoholic = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_alcoholic = if_else(
      total_alcoholic == 0, NA_real_, total_alcoholic
    )
  )

summary(alcoholic, na.rm = TRUE)

tobacco <- section3a %>%
  filter(v301 == 14) %>%
  mutate(
    total_tobacco = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_tobacco = if_else(
      total_tobacco == 0, NA_real_, total_tobacco
    )
  )

summary(tobacco, na.rm = TRUE)

prepared_food <- section3a %>%
  filter(v301 == 14) %>%
  mutate(
    total_prepared_food = rowSums(
      cbind(v303, v304, v305), 
      na.rm = TRUE
    ),
    total_prepared_food = if_else(
      total_prepared_food == 0, NA_real_, total_prepared_food
    )
  )

summary(prepared_food, na.rm = TRUE)

household_food_consumption <- section3a %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    total_consumption = if_else(
      is.na(v303) & is.na(v304) & is.na(v305),
      NA_real_,
      rowSums(cbind(v303, v304, v305), na.rm = TRUE)
    )
  ) 

household_food_consumption <- household_food_consumption %>%
  group_by(hhid) %>%
  summarise(
    household_consumption = sum(total_consumption, na.rm = TRUE),
    .groups = "drop"
  )

#################################################################################

#Section 3b

section3b <- read_dta("stata_data/section3b.dta")

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

###########################################################################################

tea_coffee <- section3b %>%
  filter(v306 == 1) %>%
  mutate(
  total_tea_coffee = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_tea_coffee = if_else(
      total_tea_coffee == 0, NA_real_, total_tea_coffee
    )
  )

breakfast <- section3b %>%
  filter(v306 == 2) %>%
  mutate(
  total_breakfast = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_breakfast = if_else(
      total_breakfast == 0, NA_real_, total_breakfast
    )
  )

summary(breakfast)

lunch <- section3b %>%
  filter(v306 == 3) %>%
  mutate(
  total_lunch = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_lunch = if_else(
      total_lunch == 0, NA_real_, total_lunch
    )
  )

summary(lunch)

snack <- section3b %>%
  filter(v306 == 4) %>%
  mutate(
  total_snack = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_snack = if_else(
      total_snack == 0, NA_real_, total_snack
    )
  )

summary(snack)

dinner <- section3b %>%
  filter(v306 == 5) %>%
  mutate(
  total_dinner = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_dinner = if_else(
      total_dinner == 0, NA_real_, total_dinner
    )
  )

summary(dinner)

coke <- section3b %>%
  filter(v306 == 6) %>%
  mutate(
  total_coke = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_coke = if_else(
      total_coke == 0, NA_real_, total_coke
    )
  )

summary(coke)

alcohol <- section3b %>%
  filter(v306 == 7) %>%
  mutate(
  total_alcohol = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_alcohol = if_else(
      total_alcohol == 0, NA_real_, total_alcohol
    )
  )

summary(alcohol)

other_food <- section3b %>%
  filter(v306 == 8) %>%
  mutate(
  total_other_food = rowSums(cbind(v308, v309), na.rm = TRUE),
  total_other_food = if_else(
      total_other_food == 0, NA_real_, total_other_food
    )
  )

summary(other_food)

##############################################################################################

#SECTION4a

section4a <- read_dta("stata_data/section4a.dta")

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


######################################################################################

clothing <- section4a %>%
  filter(v401 == 1) 

summary(clothing)

shoes <- section4a %>%
  filter(v401 == 2)

summary(shoes)

house <- section4a %>%
  filter(v401 == 3) 

summary(house)

fuel <- section4a %>%
  filter(v401 == 4)

summary(fuel)

furniture <- section4a %>%
  filter(v401 == 5)

summary(furniture)

tile <- section4a %>%
  filter(v401 == 6)

summary(tile)

appliances <- section4a %>%
  filter(v401 == 7)

summary(appliances)

kitchen_appliances <- section4a %>%
  filter(v401 == 8)

summary(kitchen_appliances)

garden_appliances <- section4a %>%
  filter(v401 == 9)

summary(garden_appliances)

house_cleaning <- section4a %>%
  filter(v401 == 10) 

summary(house_cleaning)

vehicle <- section4a %>%
  filter(v401 == 11)

summary(vehicle)

vehicle_repair <- section4a %>%
  filter(v401 == 12)

summary(vehicle_repair)

public_transport <- section4a %>%
  filter(v401 == 13)

summary(public_transport)

communication <- section4a %>%
  filter(v401 == 14)

summary(communication)

photo <- section4a %>%
  filter(v401 == 15)

summary(photo)

music <- section4a %>%
  filter(v401 == 16)

summary(music)

sports <- section4a %>%
  filter(v401 == 17)

summary(sports)

amusement <- section4a %>%
  filter(v401 == 18)

summary(amusement)

books <- section4a %>%
  filter(v401 == 19) 

summary(books)

domestic_holiday <- section4a %>%
  filter(v401 == 20)

summary(domestic_holiday)

education <- section4a %>%
  filter(v401 == 21)

summary(education)

health <- section4a %>%
  filter(v401 == 22) 

summary(health)

hostel <- section4a %>%
  filter(v401 == 23)

summary(hostel)

personal_use <- section4a %>%
  filter(v401 == 24)

summary(personal_use)

social_security <- section4a %>%
  filter(v401 == 25)

summary(social_security)

insurance <- section4a %>%
  filter(v401 == 26) 

summary(insurance)

banking <- section4a %>%
  filter(v401 == 27)

summary(banking)

admin_costs <- section4a %>%
  filter(v401 == 28) 

summary(admin_costs)

festivals <- section4a %>%
  filter(v401 == 29)

summary(festivals)

other_nonfood <- section4a %>%
  filter(v401 == 30)

summary(other_nonfood)

#############################################################################################

rm(list = ls())

section4b <- read_dta("stata_data/section4b.dta")

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

###########################################################################

section4c <- read_dta("stata_data/section4c.dta")

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

###########################################################################################

section4d <- read_dta("stata_data/section4d.dta")

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

##############################################################################################

section5 <- read_dta("stata_data/section5.dta")

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

##############################################################################################

section8 <- read_dta("stata_data/section8.dta")

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

###############################################################################################

section9a <- read_dta("stata_data/section9a.dta")

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

section9a <- section9a %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v906, v907a, v907b),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()


#SECTION9B

section9b <- read_dta("stata_data/section9b.dta")

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

section9b <- section9b %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v906, v907a, v907b),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

section9b <- section9b %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v910, v913),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

#SECTION9C

section9c <- read_dta("stata_data/section9c.dta")

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

section9c <- section9c %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v918c, v918d),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()


#SECTION9D

section9d <- read_dta("stata_data/section9d.dta")

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

section9d <- section9d %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v920, v921, v923, v924, v926, v927, v928, v929, v930, v931, v932a, v932b, v932c),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

#SECTION9E

section9e <- read_dta("stata_data/section9e.dta")

for (i in setdiff(1:ncol(section9e), c(2, 7, 8))) { 
  section9e[[i]] <- as.numeric(gsub("[^0-9]", "", section9e[[i]]))
}

section9e <- section9e %>%
  mutate(
    hhid = paste0(psu, "-", hhld),
    v934 = if_else(!is.na(v935), 1L, 2L)
  ) 

section9e <- section9e %>%
  group_by(psu) %>%
  mutate(
    across(
      c(v936b, v937b, v938b, v939b ),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

#SECTION9F1

section9f1 <- read_dta("stata_data/section9f1.dta")

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

section9f1 <- section9f1 %>%
  group_by(psu, v940) %>%
  mutate(
    across(
      c(v941),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()

##############################################################################################

section9f2 <- read_dta("stata_data/section9f2.dta")

for (i in setdiff(1:ncol(section9f2), c(2, 7, 8))) { 
  section9f2[[i]] <- as.numeric(gsub("[^0-9]", "", section9f2[[i]]))
}

section9f2 <- section9f2 %>%
  mutate(
    v943 = if_else(
      is.na(v943), 
      0,
      v943
    ),
    v943 = as.numeric(v943)
  )

section9f2 <- section9f2 %>%
  group_by(psu) %>%
  mutate(
    v943 = {
      p5  <- quantile(v943, 0.05, na.rm = TRUE)
      p95 <- quantile(v943, 0.95, na.rm = TRUE)
      mu  <- mean(v943, na.rm = TRUE)

      if_else(v943 < p5 | v943 > p95, mu, v943)
    }
  ) %>%
  ungroup()

section9f2 <- section9f2 %>%
  mutate(
    across(
      v943,
      ~ na_if(.x, 0)
    )
  )

################################################################################################

section10 <- read_dta("stata_data/section10.dta")

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

section10 <- section10 %>%
  group_by(v1002b) %>%
  mutate(
    across(
      c(v1005, v1007, v1009a, v1009b, v1010, v1011, v1012, v1013, v1014, v1015),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) %>%
  ungroup()


#############################################################################################

section12a <- read_dta("stata_data/section12a.dta")

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

section12a <- section12a %>%
  mutate(
    across(
      c(v1210, v1211, v1212),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) 

#######################################################################################

section13c  <- read_dta("stata_data/section13c.dta")

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

section13c <- section13c %>%
  mutate(
    across(
      c(v1312),
      ~ {
        p5   <- quantile(.x, 0.05, na.rm = TRUE)
        p95  <- quantile(.x, 0.95, na.rm = TRUE)
        mu   <- mean(.x, na.rm = TRUE)

        if_else(.x < p5 | .x > p95, mu, .x)
      }
    )
  ) 
