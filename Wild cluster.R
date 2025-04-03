library(dplyr)
library(fwildclusterboot)

# Read the CSV files
combined_wave1_data <- read.csv("combined_wave1_data.csv")
combined_all_waves_data <- read.csv("combined_all_waves_data.csv")

# Define Model 1
model1 <- lm(donate ~ public + information + public*information, data = combined_wave1_data)

# Function to run wild bootstrap for a specific term
run_wild_bootstrap <- function(model, term) {
  # Run wild bootstrap with Webb method
  boot_result <- boottest(
    model, 
    param = term, 
    B = 9999, 
    clustid = c("state"), 
    type = "Webb"
  )
  
  # Return and print results
  cat("Wild Bootstrap Results for Term:", term, "\n")
  print(summary(boot_result))
  
  return(boot_result)
}

# Terms to test
terms_to_test <- c("public", "information", "public:information")

# Run wild bootstrap for each term
bootstrap_results1 <- lapply(terms_to_test, function(term) {
  run_wild_bootstrap(model1, term)
})

control_vars <- c("state", "gender", "year_of_birth", "marital_status", "ethnicity", "education", "income")

# Create the formula for Model 2
control_formula <- paste(control_vars, collapse = " + ")
model2_formula <- paste("donate ~ public + information + public*information +", control_formula)

model2 <- lm(as.formula(model2_formula), data = combined_wave1_data)

# Run wild bootstrap for each term
bootstrap_results_model2 <- lapply(terms_to_test, function(term) {
  run_wild_bootstrap(model2, term)
})

model3 <- lm(donate ~ public + information + public*information + after + public*after, 
             data = combined_all_waves_data)

terms_to_test_model3 <- c("public", "information", "public:information", "after", "public:after")

bootstrap_results_model3 <- lapply(terms_to_test_model3, function(term) {
  run_wild_bootstrap(model3, term)
})

model4_formula <- paste("donate ~ public + information + public*information + after + public*after +", 
                        control_formula)
model4 <- lm(as.formula(model4_formula), data = combined_all_waves_data)

terms_to_test_model4 <- c("public", "information", "public:information", "after", "public:after")

# Run wild bootstrap for each term in Model 4
bootstrap_results_model4 <- lapply(terms_to_test_model4, function(term) {
  run_wild_bootstrap(model4, term)
})
