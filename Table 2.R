library(dplyr)
library(fwildclusterboot)

exp1_clean <- exp1 %>%
  rename(
    consent            = q1,
    county_confirm     = q2,
    facts_known_clinton= q3_a,
    facts_known_trump  = q3_b,
    donation_public    = q4_a,
    donation_private   = q4_b,
    sob_culture        = q5_a,
    sob_immigration    = q5_b,
    gender             = q6,
    year_of_birth      = q7,
    ethnicity          = q8,
    education          = q9,
    income             = q10,
    household_size     = q11,
    marital_status     = q12
  ) %>%
  mutate(
    # Convert donation_public and donation_private to characters before comparison
    donate = case_when(
      donation_public == "Yes, please donate $1 to FAIR on my behalf." ~ TRUE,
      donation_private == "Yes, please donate $1 to FAIR on my behalf." ~ TRUE,
      TRUE ~ FALSE),
    
    # The rest of your code remains unchanged
    female = ifelse(!is.na(gender) & gender == "Female", 1, 0),
    age = ifelse(!is.na(year_of_birth), 2018 - year_of_birth, NA),
    white = ifelse(!is.na(ethnicity) & grepl("White or European American", ethnicity), 1, 0),
    years_edu = case_when(
      education == "Less than high school degree" ~ 10,
      education == "High school graduate (high school diploma or equivalent including GED)" ~ 12,
      education == "Some college but no degree" ~ 13,
      education == "Associate degree in college (2-year)" ~ 14,
      education == "Bachelor's degree in college (4-year)" ~ 16,
      education == "Master's degree" ~ 18,
      education %in% c("Professional degree (JD, MD)", "Doctoral degree") ~ 21,
      TRUE ~ NA_real_
    ),
    income_000s = case_when(
      income == "Less than $10,000" ~ 10,
      income == "$10,000 to $19,999" ~ 15,
      income == "$20,000 to $29,999" ~ 25,
      income == "$30,000 to $39,999" ~ 35,
      income == "$40,000 to $49,999" ~ 45,
      income == "$50,000 to $59,999" ~ 55,
      income == "$60,000 to $69,999" ~ 65,
      income == "$70,000 to $79,999" ~ 75,
      income == "$80,000 to $89,999" ~ 85,
      income == "$90,000 to $99,999" ~ 95,
      income == "$100,000 to $149,999" ~ 125,
      income == "$150,000 or more" ~ 150,
      TRUE ~ NA_real_
    ),
    married = ifelse(!is.na(marital_status) & marital_status == "Married", 1, 0),
    missing_demographics = ifelse(is.na(gender), 1, 0),
    clinton_private = ifelse(t_trump_clinton == "Clinton Won" & t_public_private == "Private", 1, 0),
    clinton_public  = ifelse(t_trump_clinton == "Clinton Won" & t_public_private == "Public", 1, 0),
    trump_private   = ifelse(t_trump_clinton == "Trump Won" & t_public_private == "Private", 1, 0),
    trump_public    = ifelse(t_trump_clinton == "Trump Won" & t_public_private == "Public", 1, 0),
    trump  = ifelse(t_trump_clinton == "Trump Won", 1, 0),
    public = ifelse(t_public_private == "Public", 1, 0),
    treatment = paste(t_trump_clinton, t_public_private)
  )



# Run regressions
# Model 1: without controls
model1_exp2 <- lm(donate ~ public + trump + trump_public, data = exp1_clean)
summary(model1_exp2)

# Model 2: with controls
model2_exp2 <- lm(donate ~ public + trump + trump_public + female + age + married + years_edu + income_000s + white, data = exp1_clean)
summary(model2_exp2)

run_wild_bootstrap <- function(model, term) {
  # Run wild bootstrap with Webb method
  boot_result <- boottest(
    model, 
    param = term, 
    B = 9999, 
    type = "Webb"
  )
  
  # Return and print results
  cat("Wild Bootstrap Results for Term:", term, "\n")
  print(summary(boot_result))
  
  return(boot_result)
}

terms_to_test <- c("public", "trump", "trump_public")

bootstrap_results1_1B <- lapply(terms_to_test, function(term) {
  run_wild_bootstrap(model1_exp2, term)
})

bootstrap_results2_1B <- lapply(terms_to_test, function(term) {
  run_wild_bootstrap(model2_exp2, term)
})
