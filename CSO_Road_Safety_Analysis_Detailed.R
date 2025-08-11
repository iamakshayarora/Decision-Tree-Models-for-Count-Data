
library(tidyr) # For pivot_wider if needed, and unnest

# --- 2. Data Preparation ---

# A. Base monthly data with target variable
monthly_data <- RoadFatalities %>%
  mutate(
    Year = as.numeric(substr(Month, 1, 4)),
    MonthName = factor(gsub("^[0-9]{4} ", "", Month), levels = month.name)
  ) %>%
  dplyr::select(Year, MonthName, VALUE_Fatality = VALUE)%>%
  filter(Year != 2025)

# B. Monthly predictors from Traffic_Collisions_Casualties_bymonth
monthly_collision_stats <- Traffic_Collisions_Casualties_bymonth %>%
  mutate(
    MonthName = factor(Month.of.Year, levels = month.name)
  ) %>%
  filter(Statistic.Label %in% c("Fatal Collisions", "Injury Collisions", "Injured Casualties", "Killed Casualties"),
         MonthName != "All months" ) %>%
  dplyr::select(Year, MonthName, Statistic.Label, VALUE) %>%
  pivot_wider(names_from = Statistic.Label, values_from = VALUE) %>%
  rename(
    Fatal_Col_Month = `Fatal Collisions`,
    Injury_Col_Month = `Injury Collisions`,
    Injured_Cas_Month = `Injured Casualties`,
    Killed_Cas_Month = `Killed Casualties`
  )

# Join monthly collision stats
monthly_data <- monthly_data %>%
  left_join(monthly_collision_stats, by = c("Year", "MonthName"))

# C. Annual Predictors

# C.1 From Number_of_Casualties
annual_casualty_stats <- Number_of_Casualties %>%
  filter(!Statistic.Label %in% c("All Killed and Injured Casualties"), Age.Group == "All ages", !Road.User.Type %in% c("All road users", "All Car users")) %>%
  group_by(Year, Road.User.Type) %>%
  summarise(TotalCasualties = sum(VALUE), .groups = 'drop') %>%
  group_by(Year) %>%
  mutate(
    AllUserCasualtiesInYear = sum(TotalCasualties)
  ) %>%
  ungroup() %>%
  #filter(Road.User.Type %in% c("Motor cyclists", "Pedestrians")) %>%
  mutate(
    PropCasualties = TotalCasualties / AllUserCasualtiesInYear
  ) %>%
  dplyr::select(Year, Road.User.Type, PropCasualties) %>%
  pivot_wider(names_from = Road.User.Type, values_from = PropCasualties) %>%
  rename(
    Motorcyclist_Casualties_Annual_Prop = `Motor cyclists`,
    Pedestrian_Casualties_Annual_Prop = Pedestrians,
    Car_drivers_Casualties_Annual_Prop = `Car drivers`,
    Car_passengers_Casualties_Annual_Prop = `Car passengers`,
    Pedal_cyclists_Casualties_Annual_Prop = `Pedal cyclists`,
    Other_road_users_Casualties_Annual_Prop = `Other road users`
  ) %>%
  dplyr::select(Year, Motorcyclist_Casualties_Annual_Prop, Pedestrian_Casualties_Annual_Prop, Car_drivers_Casualties_Annual_Prop, Car_passengers_Casualties_Annual_Prop, Pedal_cyclists_Casualties_Annual_Prop, Other_road_users_Casualties_Annual_Prop)


# C.2 From Vehicles_in_Collisions (Proportion of HGVs in Fatal Collisions)
annual_vehicle_stats <- Vehicles_in_Collisions %>%
  filter(!Statistic.Label == "All Fatal and Injury Collisions", !Country.of.Residence == "All countries") %>% # Assuming 'All countries' is the total
  group_by(Year, Type.of.Vehicle) %>%
  summarise(NumVehicles = sum(VALUE), .groups = 'drop') %>%
  group_by(Year) %>%
  mutate(TotalVehiclesInFatalColl = sum(NumVehicles)) %>%
  ungroup() %>%
  filter(Type.of.Vehicle == "Goods vehicles") %>%
  mutate(HGV_Involved_Fatal_Annual_Prop = NumVehicles / TotalVehiclesInFatalColl) %>%
  dplyr::select(Year, HGV_Involved_Fatal_Annual_Prop)


# C.3 From Traffic_Collisions_Casualties_byweek (Weekend Fatalities Proportion)
annual_week_stats <- Traffic_Collisions_Casualties_byweek %>%
  filter(Statistic.Label == "Fatal Collisions") %>%
  group_by(Year) %>%
  mutate(TotalFatalitiesInYear = sum(VALUE[Day.of.Week == "All days"])) %>% # Get total from "All days" row
  ungroup() %>%
  filter(Day.of.Week %in% c("Saturday", "Sunday")) %>%
  group_by(Year, TotalFatalitiesInYear) %>%
  summarise(WeekendFatalities = sum(VALUE), .groups = 'drop') %>%
  mutate(Weekend_Fatalities_Annual_Prop = WeekendFatalities / TotalFatalitiesInYear) %>%
  dplyr::select(Year, Weekend_Fatalities_Annual_Prop)

# C.5 From Drivers_in_Collisions
# Define young age groups, e.g., "17 - 20 years", "21 - 24 years"
# This requires exact matching with `Age.Group` levels in your data.
# Let's assume "17-24" broadly. For simplicity, using 'All ages' for male prop.
young_age_groups <- c("17 - 20 years", "21 - 24 years") # Adjust based on actual levels in Drivers_in_Collisions$Age.Group

annual_driver_stats <- Drivers_in_Collisions %>%
  filter(grepl("Fatal Collisions", Statistic.Label) | !Statistic.Label == "All Drivers of Cars Involved in Fatal and Injury Collisions") %>% # Adjust Statistic.Label based on data
  # For Young Drivers
  # This part is complex as it depends on the exact structure and whether 'All ages' includes breakdown
  # Placeholder for Young_Drivers_Fatal_Coll_Annual_Prop - requires more detailed data inspection
  # For Male Drivers
  group_by(Year, Sex) %>%
  filter(Age.Group == "All ages") %>% # Assuming 'All ages' sums up drivers correctly
  summarise(NumDrivers = sum(VALUE, na.rm=TRUE), .groups = 'drop') %>%
  group_by(Year) %>%
  mutate(TotalDriversInFatalColl = sum(NumDrivers)) %>%
  ungroup() %>%
  filter(Sex == "Male") %>%
  mutate(Male_Drivers_Fatal_Coll_Annual_Prop = NumDrivers / TotalDriversInFatalColl) %>%
  dplyr::select(Year, Male_Drivers_Fatal_Coll_Annual_Prop)


# Join all annual stats to monthly_data
monthly_data <- monthly_data %>%
  left_join(annual_casualty_stats, by = "Year") %>%
  left_join(annual_vehicle_stats, by = "Year") %>%
  left_join(annual_week_stats, by = "Year") %>%
  left_join(annual_hour_stats, by = "Year") %>%
  left_join(annual_driver_stats, by = "Year")


# D. Final Target Variable Categorization
monthly_data <- monthly_data %>%
  mutate(
    Fatality.Category = factor(
      case_when(
        VALUE_Fatality >= 1 & VALUE_Fatality <= 19 ~ "Low",
        VALUE_Fatality >= 20 ~ "High",
        TRUE ~ NA_character_
      ),
      levels = c("Low", "High")
    )
  ) %>%
  filter(!is.na(Fatality.Category)) # Remove rows if target is NA

# Handle NAs in predictors (e.g., by imputation or removing rows)
# For simplicity in this example, we'll use complete cases.
# In a real scenario, more sophisticated NA handling is advised.
colSums(is.na(monthly_data)) # Check NAs
monthly_data_complete <- monthly_data[complete.cases(monthly_data), ]
if(nrow(monthly_data_complete) < nrow(monthly_data)){
  print(paste("Removed", nrow(monthly_data) - nrow(monthly_data_complete), "rows due to NAs in predictors."))
}
if(nrow(monthly_data_complete) == 0){
  stop("No complete cases after joining and NA removal. Check data merging steps and NA sources.")
}


print("Prepared data with expanded features (head):")
print(head(monthly_data_complete))
print(summary(monthly_data_complete))
table(monthly_data_complete$Fatality.Category)


# --- 3. Data Splitting ---
set.seed(123)
trainIndex <- createDataPartition(monthly_data_complete$Fatality.Category, p = .7, list = FALSE, times = 1)
training_data <- monthly_data_complete[trainIndex, ]
testing_data <- monthly_data_complete[-trainIndex, ]

if(nrow(training_data) == 0 || nrow(testing_data) == 0){
  stop("Training or testing data has 0 rows. Check data splitting or prior NA handling.")
}

print(paste("Training data observations:", nrow(training_data)))
print(paste("Testing data observations:", nrow(testing_data)))


# --- 4. Helper function to categorize regression predictions ---
categorize_predictions <- function(predictions) {
  factor(
    case_when(
      predictions >= 1 & predictions <= 19 ~ "Low",
      predictions >= 20 ~ "High",
      predictions < 1 ~ "Low", # Default for predictions < 1
      TRUE ~ NA_character_ # Should ideally not happen if predictions are numeric
    ),
    levels = c("Low", "High")
  )
}

# Define the formula
# Excluding Year from numeric predictors if it's also used as a factor or inherently captured by annual stats
predictor_vars <- c("MonthName", "Serious_Injury_Coll_Month", "Minor_Injury_Coll_Month",
                    "Motorcyclist_Casualties_Annual_Prop", "Pedestrian_Casualties_Annual_Prop",
                    "HGV_Involved_Fatal_Annual_Prop", "Weekend_Fatalities_Annual_Prop",
                    "Night_Time_Fatalities_Annual_Prop", "Male_Drivers_Fatal_Coll_Annual_Prop",
                    "Year") # Year included as a numeric trend/control

# Check if all predictor_vars are in training_data
missing_cols <- setdiff(predictor_vars, names(training_data))
if(length(missing_cols) > 0) {
  stop(paste("Following predictor variables are missing from training_data:", paste(missing_cols, collapse=", ")))
}

# Ensure no NA values in predictor columns used for modeling within training/testing sets again
# This should already be handled by `monthly_data_complete` but as a safeguard for formula construction.
training_data <- training_data[complete.cases(training_data[, c("VALUE_Fatality", predictor_vars)]), ]
testing_data <- testing_data[complete.cases(testing_data[, c("VALUE_Fatality", predictor_vars)]), ]


model_formula_str <- paste("VALUE_Fatality ~", paste(predictor_vars, collapse = " + "))
model_formula <- as.formula(model_formula_str)

model_formula_class_str <- paste("Fatality.Category ~", paste(predictor_vars, collapse = " + "))
model_formula_class <- as.formula(model_formula_class_str)


# --- 5. Poisson Regression Model ---
poisson_model <- glm(model_formula, family = poisson, data = training_data)
# ... (rest of the evaluation code for Poisson, similar to previous script) ...
# Predictions and accuracy for Poisson model
poisson_pred_counts_train <- predict(poisson_model, newdata = training_data, type = "response")
poisson_pred_category_train <- categorize_predictions(poisson_pred_counts_train)
poisson_accuracy_train <- if(length(unique(training_data$Fatality.Category)) < 2 || length(unique(poisson_pred_category_train)) < 2) NA else confusionMatrix(poisson_pred_category_train, training_data$Fatality.Category)$overall["Accuracy"]

poisson_pred_counts_test <- predict(poisson_model, newdata = testing_data, type = "response")
poisson_pred_category_test <- categorize_predictions(poisson_pred_counts_test)
poisson_accuracy_test <- if(length(unique(testing_data$Fatality.Category)) < 2 || length(unique(poisson_pred_category_test)) < 2) NA else confusionMatrix(poisson_pred_category_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("Poisson Model AIC:", AIC(poisson_model)))
print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test, 4)))


# --- 6. Negative Binomial Regression Model ---
negbin_model <- NULL
negbin_accuracy_train <- NA
negbin_accuracy_test <- NA
tryCatch({
  negbin_model <- glm.nb(model_formula, data = training_data)
  # ... (rest of the evaluation code for Negative Binomial) ...
  negbin_pred_counts_train <- predict(negbin_model, newdata = training_data, type = "response")
  negbin_pred_category_train <- categorize_predictions(negbin_pred_counts_train)
  negbin_accuracy_train <- if(length(unique(training_data$Fatality.Category)) < 2 || length(unique(negbin_pred_category_train)) < 2) NA else confusionMatrix(negbin_pred_category_train, training_data$Fatality.Category)$overall["Accuracy"]
  
  negbin_pred_counts_test <- predict(negbin_model, newdata = testing_data, type = "response")
  negbin_pred_category_test <- categorize_predictions(negbin_pred_counts_test)
  negbin_accuracy_test <- if(length(unique(testing_data$Fatality.Category)) < 2 || length(unique(negbin_pred_category_test)) < 2) NA else confusionMatrix(negbin_pred_category_test, testing_data$Fatality.Category)$overall["Accuracy"]
  
  print(paste("Negative Binomial Model AIC:", AIC(negbin_model)))
  print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train, 4)))
  print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test, 4)))
}, error = function(e) {
  print(paste("Error in Negative Binomial Model:", e$message))
})


# --- 7. Decision Tree Model (CART) ---
# For tree models, ensure 'Year' is factor if desired for partitioning, or leave numeric if the algorithm handles it.
# rpart handles numeric predictors by finding optimal splits.
training_data_tree <- training_data # Can create a copy and factorize Year if needed: %>% mutate(Year = factor(Year))
testing_data_tree <- testing_data   # %>% mutate(Year = factor(Year))

cart_model <- rpart(model_formula_class, data = training_data_tree, method = "class")
# ... (rest of the evaluation code for CART) ...
cart_pred_train <- predict(cart_model, newdata = training_data_tree, type = "class")
cart_accuracy_train <- if(length(unique(training_data_tree$Fatality.Category)) < 2 || length(unique(cart_pred_train)) < 2) NA else confusionMatrix(cart_pred_train, training_data_tree$Fatality.Category)$overall["Accuracy"]

cart_pred_test <- predict(cart_model, newdata = testing_data_tree, type = "class")
cart_accuracy_test <- if(length(unique(testing_data_tree$Fatality.Category)) < 2 || length(unique(cart_pred_test)) < 2) NA else confusionMatrix(cart_pred_test, testing_data_tree$Fatality.Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test, 4)))
# rpart.plot(cart_model) # To visualize

# --- 8. Decision Tree Model (C5.0) ---
# C5.0 handles numeric predictors well.
c50_model <- C5.0(model_formula_class, data = training_data) # Using training_data with numeric Year
# ... (rest of the evaluation code for C5.0) ...
c50_pred_train <- predict(c50_model, newdata = training_data, type = "class")
c50_accuracy_train <- if(length(unique(training_data$Fatality.Category)) < 2 || length(unique(c50_pred_train)) < 2) NA else confusionMatrix(c50_pred_train, training_data$Fatality.Category)$overall["Accuracy"]

c50_pred_test <- predict(c50_model, newdata = testing_data, type = "class")
c50_accuracy_test <- if(length(unique(testing_data$Fatality.Category)) < 2 || length(unique(c50_pred_test)) < 2) NA else confusionMatrix(c50_pred_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test, 4)))


# --- 9. Decision Tree Model (CHAID) ---
chaid_accuracy_train <- NA
chaid_accuracy_test <- NA
if ("CHAID" %in% rownames(installed.packages())) {
  library(CHAID)
  tryCatch({
    # CHAID usually prefers factors. Create a version of data where numeric predictors for CHAID are binned or factored if necessary.
    # For this example, using current training_data_tree (which has MonthName as factor, Year as numeric/factor based on above)
    # Ensure all predictors in the formula for CHAID are factors or ordered factors.
    # For simplicity, let's assume current predictors are usable or convert numeric ones to factors.
    training_data_chaid <- training_data_tree %>%
      mutate(across(where(is.numeric) & !all_of(c("VALUE_Fatality")), factor)) # Factorize numeric predictors for CHAID
    
    testing_data_chaid <- testing_data_tree %>%
      mutate(across(where(is.numeric) & !all_of(c("VALUE_Fatality")), factor))
    
    # Check levels consistency
    for(col_name in predictor_vars) {
      if(is.factor(training_data_chaid[[col_name]])) {
        levels_train <- levels(training_data_chaid[[col_name]])
        levels_test <- levels(testing_data_chaid[[col_name]])
        if(!all(levels_test %in% levels_train)) {
          # Handle new levels in test data not seen in training for factors, common issue
          # Simplest: Convert test levels to be a subset of train levels, or map to a common category / NA
          # For robust solution, this needs careful handling (e.g. `forcats::fct_other`)
          # print(paste("Warning: New factor levels in test set for", col_name))
          # For now, we proceed, CHAID might handle it or error.
        }
        # Ensure test set factor levels are consistent with training set if CHAID is strict
        testing_data_chaid[[col_name]] <- factor(testing_data_chaid[[col_name]], levels = levels_train)
        
      }
    }
    
    
    chaid_model <- chaid(model_formula_class, data = training_data_chaid, 
                         control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7))
    
    chaid_pred_train <- predict(chaid_model, newdata = training_data_chaid, type = "response")
    chaid_accuracy_train <- if(length(unique(training_data_chaid$Fatality.Category)) < 2 || length(unique(chaid_pred_train)) < 2) NA else confusionMatrix(chaid_pred_train, training_data_chaid$Fatality.Category)$overall["Accuracy"]
    
    chaid_pred_test <- predict(chaid_model, newdata = testing_data_chaid, type = "response")
    chaid_accuracy_test <- if(length(unique(testing_data_chaid$Fatality.Category)) < 2 || length(unique(chaid_pred_test)) < 2) NA else confusionMatrix(chaid_pred_test, testing_data_chaid$Fatality.Category)$overall["Accuracy"]
    
    print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train, 4)))
    print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test, 4)))
  }, error = function(e) {
    print(paste("Error in CHAID Model:", e$message))
  })
} else {
  print("CHAID package not installed. Skipping CHAID model.")
}

# --- 10. Results Summary Table ---
results_summary <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train * 100, 2),
    ifelse(is.null(negbin_model) || is.na(negbin_accuracy_train), NA, round(negbin_accuracy_train * 100, 2)),
    round(cart_accuracy_train * 100, 2),
    round(c50_accuracy_train * 100, 2),
    round(chaid_accuracy_train * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test * 100, 2),
    ifelse(is.null(negbin_model) || is.na(negbin_accuracy_test), NA, round(negbin_accuracy_test * 100, 2)),
    round(cart_accuracy_test * 100, 2),
    round(c50_accuracy_test * 100, 2),
    round(chaid_accuracy_test * 100, 2)
  )
)

print("--- Model Comparison Summary (Expanded Features) ---")
print(results_summary)