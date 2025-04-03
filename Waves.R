library(dplyr)

# First, let's rename the variables and add after=0 to wave1 datasets
app_exp1b_wave1_pilot <- app_exp1b_wave1_pilot %>%
  rename(
    state = q1,
    gender = q2,
    year_of_birth = q3,
    marital_status = q4,
    ethnicity = q5,
    education = q6,
    income = q7,
    prob_trump_win_info = q8,
    sob = q9,
    donation_anti = q10_a,
    donation_pro = q10_b
  ) %>%
  mutate(
    donate = case_when(
      !is.na(donation_anti) & donation_anti == "Yes" ~ 1,
      !is.na(donation_anti) & donation_anti == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    after = 0,  # These data were collected before the election
    public = ifelse(t_public_private == "Public", 1, 0),
    information = ifelse(t_info_control == "Information", 1, 0)
  ) %>%
  filter(!is.na(donation_anti))

app_exp1b_wave1 <- app_exp1b_wave1 %>%
  rename(
    state = q1,
    gender = q2,
    year_of_birth = q3,
    marital_status = q4,
    ethnicity = q5,
    education = q6,
    income = q7,
    prob_trump_win_info = q8,
    sob = q9,
    donation_anti = q10_a,
    donation_pro = q10_b,
    prob_trump_win_control = q11
  ) %>%
  mutate(
    donate = case_when(
      !is.na(donation_anti) & donation_anti == "Yes" ~ 1,
      !is.na(donation_anti) & donation_anti == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    after = 0,  # These data were collected before the election
    public = ifelse(t_public_private == "Public", 1, 0),
    information = ifelse(t_info_control == "Information", 1, 0)
  ) %>%
  filter(!is.na(donation_anti))

# First, collect all matchid values from the first two datasets
wave1_matchids <- c(
  app_exp1b_wave1_pilot$matchid,
  app_exp1b_wave1$matchid
)

# Now process the Wave 2 dataset, removing duplicates
app_exp1b_wave2 <- app_exp1b_wave2 %>%
  rename(
    state = q1,
    gender = q2,
    year_of_birth = q3,
    marital_status = q4,
    ethnicity = q5,
    education = q6,
    income = q7,
    sob = q8,
    donation_anti = q9_a,
    donation_pro = q9_b
  ) %>%
  # Filter out people who appeared in the previous datasets
  filter(!(matchid %in% wave1_matchids)) %>%
  mutate(
    donate = case_when(
      !is.na(donation_anti) & donation_anti == "Yes" ~ 1,
      !is.na(donation_anti) & donation_anti == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    after = 1,  # This data was collected after the election
    public = ifelse(t_public_private == "Public", 1, 0),
    information = 0
  ) %>%
  filter(!is.na(donation_anti))

control_vars <- c("state", "gender", "year_of_birth", "marital_status", "ethnicity", "education", "income")

# Now combine all three datasets, keeping the variables we need plus controls
combined_data_all <- bind_rows(
  app_exp1b_wave1_pilot %>% select(public, information, after, donate, matchid, all_of(control_vars)),
  app_exp1b_wave1 %>% select(public, information, after, donate, matchid, all_of(control_vars)),
  app_exp1b_wave2 %>% select(public, information, after, donate, matchid, all_of(control_vars))
)


# Calculate donation rates by treatment group, including after-election data
donation_means_all <- combined_data_all %>%
  # Create a new grouping variable that includes after-election status
  mutate(
    treatment_group = case_when(
      after == 1 & public == 1 ~ "After Election Public",
      after == 1 & public == 0 ~ "After Election Private",
      after == 0 & public == 1 & information == 1 ~ "Information Public",
      after == 0 & public == 0 & information == 1 ~ "Information Private",
      after == 0 & public == 1 & information == 0 ~ "Control Public",
      after == 0 & public == 0 & information == 0 ~ "Control Private"
    )
  ) %>%
  group_by(treatment_group) %>%
  summarize(
    meandonate = mean(donate, na.rm = TRUE) * 100,  # Convert to percentage
    sddonate = sd(donate, na.rm = TRUE) * 100,
    n = sum(!is.na(donate)),
    se = sddonate / sqrt(n),
    lower_ci = meandonate - 1.96 * se,
    upper_ci = meandonate + 1.96 * se
  )

print(donation_means_all)

library(stargazer)

# Filter data for Wave 1 (where after=0)
combined_data_wave1 <- combined_data_all %>% filter(after == 0)

# Model 1: Basic model for Wave 1
model1 <- lm(donate ~ public + information + public*information, data = combined_data_wave1)

# Model 2: Adding controls for Wave 1
control_formula <- paste(control_vars, collapse = " + ")
model2_formula <- paste("donate ~ public + information + public*information +", control_formula)
model2 <- lm(as.formula(model2_formula), data = combined_data_wave1)
summary(model2)

# Model 3: Basic model for all waves
model3 <- lm(donate ~ public + information + public*information + after + public*after, 
             data = combined_data_all)

# Model 4: Adding controls for all waves
model4_formula <- paste("donate ~ public + information + public*information + after + public*after +", 
                        control_formula)
model4 <- lm(as.formula(model4_formula), data = combined_data_all)

# Create stargazer output
stargazer(model1, model2, model3, model4, 
          type = "latex",
          title = "Regression Results",
          column.labels = c("Wave 1", "Wave 1 w/Controls", "All Waves", "All Waves w/Controls"),
          dep.var.labels = "Donation Amount",
          covariate.labels = c("Public", "Information", "After", 
                               "Public x Information", "Public x After",
                               "Gender", "Year of Birth", "Marital Status", 
                               "Ethnicity", "Education", "Income"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"),
          notes = "Note: *p<0.05; **p<0.01; ***p<0.001")

write.csv(combined_data_wave1, "combined_wave1_data.csv", row.names = FALSE)
write.csv(combined_data_all, "combined_all_waves_data.csv", row.names = FALSE)

extract_coeff_pvals <- function(model) {
  # Extract coefficients and p-values
  coef_summary <- summary(model)$coefficients
  
  # Define the variables of interest
  vars <- c("(Intercept)", "public", "information", "public:information")
  
  # Subset only the relevant coefficients and p-values
  results <- coef_summary[rownames(coef_summary) %in% vars, c(1, 4)]
  
  # Rename columns for clarity
  colnames(results) <- c("Estimate", "p-value")
  
  return(results)
}

extract_coeff_pvals2 <- function(model) {
  # Extract coefficients and p-values
  coef_summary <- summary(model)$coefficients
  
  # Define the variables of interest
  vars <- c("(Intercept)", "public", "information", "public:information", "after", "public:after")
  
  # Subset only the relevant coefficients and p-values
  results <- coef_summary[rownames(coef_summary) %in% vars, c(1, 4)]
  
  # Rename columns for clarity
  colnames(results) <- c("Estimate", "p-value")
  
  return(results)
}

extract_coeff_pvals(model1)

extract_coeff_pvals(model2)

extract_coeff_pvals2(model3)

extract_coeff_pvals2(model4)
