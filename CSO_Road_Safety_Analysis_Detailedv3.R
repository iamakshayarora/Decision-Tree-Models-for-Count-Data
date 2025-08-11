head(Casualties_updated)
names(Casualties_updated)
unique(Casualties_updated$Statistic.Label)
unique(Casualties_updated$Year)
unique(Casualties_updated$Age.Group)
unique(Casualties_updated$Sex)
unique(Casualties_updated$Road.User.Type)
summary(Casualties_updated)


head(Driver_collisions_updated)
names(Driver_collisions_updated)
unique(Driver_collisions_updated$Statistic.Label)
unique(Driver_collisions_updated$Year)
unique(Driver_collisions_updated$Age.Group)
unique(Driver_collisions_updated$Sex)
summary(Driver_collisions_updated)


head(Car_User_collisions_updated)
names(Car_User_collisions_updated)
unique(Car_User_collisions_updated$Statistic.Label)
unique(Car_User_collisions_updated$Year)
unique(Car_User_collisions_updated$Seat.Belt.Usage)
summary(Car_User_collisions_updated)


head(Vehicle_collisions_updated)
names(Vehicle_collisions_updated)
unique(Vehicle_collisions_updated$Statistic.Label)
unique(Vehicle_collisions_updated$Year)
unique(Vehicle_collisions_updated$Country.of.Residence)
unique(Vehicle_collisions_updated$Type.of.Vehicle)
summary(Vehicle_collisions_updated)









##################################################################################






Driver_collisions_updated_processed <- Driver_collisions_updated %>%
  mutate(VALUE = ifelse(is.na(VALUE), 0, VALUE)) %>%
  rename(Count = VALUE) %>%
  mutate(
    Occupant_Type = "Driver",
    Outcome = case_when(
      Statistic.Label == "Drivers of Cars Killed in Collisions" ~ "Killed",
      Statistic.Label == "Drivers of Cars Injured in Collisions" ~ "Injured",
      Statistic.Label == "Drivers of Cars Uninjured in Collisions" ~ "Uninjured",
      TRUE ~ NA_character_ 
    )
  ) %>%
  dplyr::select(Year, Age.Group, Sex, Occupant_Type, Outcome, Count)


print(head(Driver_collisions_updated_processed))
print(summary(Driver_collisions_updated_processed$Count))
print(unique(Driver_collisions_updated_processed$Outcome))


Casualties_passengers_processed <- Casualties_updated %>%
  filter(Road.User.Type == "Car passengers") %>%
  rename(Count = VALUE) %>%
  mutate(
    Occupant_Type = "Car Passenger",
    Outcome = case_when(
      Statistic.Label == "Killed Casualties" ~ "Killed",
      Statistic.Label == "Injured Casualties" ~ "Injured",
      TRUE ~ NA_character_ 
    )
  ) %>%
  dplyr::select(Year, Age.Group, Sex, Occupant_Type, Outcome, Count)



print(head(Casualties_passengers_processed))
print(summary(Casualties_passengers_processed$Count))
print(unique(Casualties_passengers_processed$Outcome))


combined_data <- bind_rows(Driver_collisions_updated_processed, Casualties_passengers_processed)


print(head(combined_data))
print(tail(combined_data))
print(paste("Number of rows in combined_data:", nrow(combined_data)))


combined_data <- combined_data %>%
  mutate(
    Age.Group = as.factor(Age.Group),
    Sex = as.factor(Sex),
    Occupant_Type = as.factor(Occupant_Type),
    Outcome = as.factor(Outcome),
    Year = as.factor(Year) 
  )


str(combined_data)
summary(combined_data)
print(levels(combined_data$Outcome)) 



combined_data <- combined_data %>%
  mutate(
    Fatality.Category = factor(
      case_when(
        Outcome == "Killed" & Count >= 0 & Count <= 4 ~ "Low",
        Outcome == "Killed" & Count >= 5 & Count <= 14 ~ "Medium",
        Outcome == "Killed" & Count >= 15 ~ "High",

        Outcome == "Injured" & Count >= 0 & Count <= 49 ~ "Low",
        Outcome == "Injured" & Count >= 50 & Count <= 299 ~ "Medium",
        Outcome == "Injured" & Count >= 300 ~ "High",

        Outcome == "Uninjured" & Count >= 0 & Count <= 49 ~ "Low",
        Outcome == "Uninjured" & Count >= 50 & Count <= 299 ~ "Medium",
        Outcome == "Uninjured" & Count >= 300 ~ "High",
        
        TRUE ~ NA_character_ 
      ),
      levels = c("Low", "Medium", "High")
    )
  ) %>%
  filter(!is.na(Fatality.Category)) 



set.seed(123)
trainIndex2 <- createDataPartition(combined_data$Fatality.Category, p = .7, list = FALSE, times = 1)
training_data2 <- combined_data[trainIndex2, ]
testing_data2 <- combined_data[-trainIndex2, ]

print(paste("Training data observations:", nrow(training_data2)))
print(paste("Testing data observations:", nrow(testing_data2)))

print(table(training_data2$Fatality.Category))

print(table(testing_data2$Fatality.Category))





categorize_predictions2 <- function(predictions, outcomes_vector) {
  if (length(predictions) != length(outcomes_vector)) {
    stop("Length of predictions and outcomes_vector must be the same.")
  }
  
  categories <- character(length(predictions)) 
  
  for (i in 1:length(predictions)) {
    pred_val <- predictions[i]
    outcome_val <- as.character(outcomes_vector[i]) 
    
    if (is.na(pred_val) || is.na(outcome_val)) {
      categories[i] <- NA_character_
      next 
    }
    
    if (outcome_val == "Killed") {
      if (pred_val >= 0 && pred_val <= 4) categories[i] <- "Low"
      else if (pred_val >= 5 && pred_val <= 14) categories[i] <- "Medium"
      else if (pred_val >= 15) categories[i] <- "High"
      else categories[i] <- "Low" 
    } else if (outcome_val == "Injured") {
      if (pred_val >= 0 && pred_val <= 49) categories[i] <- "Low"
      else if (pred_val >= 50 && pred_val <= 299) categories[i] <- "Medium"
      else if (pred_val >= 300) categories[i] <- "High"
      else categories[i] <- "Low"
    } else if (outcome_val == "Uninjured") {
      if (pred_val >= 0 && pred_val <= 49) categories[i] <- "Low"
      else if (pred_val >= 50 && pred_val <= 299) categories[i] <- "Medium"
      else if (pred_val >= 350) categories[i] <- "High"
      else categories[i] <- "Low"
    } else {
      categories[i] <- NA_character_ 
    }
  }
  
  return(factor(categories, levels = c("Low", "Medium", "High")))
}


poisson_model2 <- glm(Count ~ Year + Age.Group + Sex + Occupant_Type + Outcome,
                                 family = poisson(link = "log"),
                                 data = training_data2)

print(summary(poisson_model2))
print(paste("Poisson Model AIC:", AIC(poisson_model2)))
print(paste("Poisson Model Deviance:", deviance(poisson_model2)))

poisson_pred_counts_train2 <- predict(poisson_model2, newdata = training_data2, type = "response")
print(poisson_pred_counts_train2)

poisson_pred_category_train2 <- categorize_predictions2(poisson_pred_counts_train2, training_data2$Outcome)

all_levels <- c("Low", "Medium", "High")
poisson_pred_category_train2 <- factor(poisson_pred_category_train2, levels = all_levels)
training_data_fatality_category <- factor(training_data2$Fatality.Category, levels = all_levels)
valid_train_indices <- !is.na(poisson_pred_category_train2) & !is.na(training_data_fatality_category)

cm_train <- confusionMatrix(poisson_pred_category_train2[valid_train_indices], 
                            training_data_fatality_category[valid_train_indices])
print("Training Confusion Matrix:")
print(cm_train)
poisson_accuracy_train2 <- cm_train$overall["Accuracy"]


single_new_observation <- data.frame(
  Year = 2024,
  Age.Group = factor("15 - 17 years"),
  Sex = factor("Female"),            
  Occupant_Type = factor("Car Passenger"), 
  Outcome = factor("Injured"))

poisson_pred_test <- predict(poisson_model2, newdata = single_new_observation, type = "response")
print(poisson_pred_test)


poisson_pred_counts_test <- predict(poisson_model2, newdata = testing_data2, type = "response")
poisson_pred_category_test <- categorize_predictions2(poisson_pred_counts_test, testing_data2$Outcome)

poisson_pred_category_test <- factor(poisson_pred_category_test, levels = all_levels)
testing_data_fatality_category <- factor(testing_data2$Fatality.Category, levels = all_levels)

valid_test_indices <- !is.na(poisson_pred_category_test) & !is.na(testing_data_fatality_category)

cm_test <- confusionMatrix(poisson_pred_category_test[valid_test_indices], 
                           testing_data_fatality_category[valid_test_indices])
print("Testing Confusion Matrix:")
print(cm_test)
poisson_accuracy_test2 <- cm_test$overall["Accuracy"]


print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train2, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test2, 4)))











#Negative Binomial Regression 
negbin_model2 <- glm.nb(Count ~ Year + Age.Group + Sex + Occupant_Type + Outcome, data = training_data2)
print(summary(negbin_model2))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model2)))
print(paste("Negative Binomial Model Deviance:", deviance(negbin_model2)))

negbin_pred_counts_train2 <- predict(negbin_model2, newdata = training_data2, type = "response")
negbin_pred_category_train2 <- categorize_predictions2(negbin_pred_counts_train2, training_data2$Outcome)
negbin_pred_category_train2 <- factor(negbin_pred_category_train2, levels = all_levels) 

valid_negbin_train_indices <- !is.na(negbin_pred_category_train2) & !is.na(training_data_fatality_category)
cm_negbin_train <- confusionMatrix(negbin_pred_category_train2[valid_negbin_train_indices], training_data_fatality_category[valid_negbin_train_indices])
negbin_accuracy_train2 <- cm_negbin_train$overall["Accuracy"]

negbin_pred_counts_test <- predict(negbin_model2, newdata = testing_data2, type = "response")
negbin_pred_category_test <- categorize_predictions2(negbin_pred_counts_test, testing_data2$Outcome)
negbin_pred_category_test <- factor(negbin_pred_category_test, levels = all_levels) 

valid_negbin_test_indices <- !is.na(negbin_pred_category_test) & !is.na(testing_data_fatality_category)
cm_negbin_test <- confusionMatrix(negbin_pred_category_test[valid_negbin_test_indices], testing_data_fatality_category[valid_negbin_test_indices])
negbin_accuracy_test2 <- cm_negbin_test$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train2, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test2, 4)))


#CART
cart_model2 <- rpart(Fatality.Category ~ Year + Age.Group + Sex + Occupant_Type + Outcome, data = training_data2, method = "class")
png("RSA_CART_decision_tree.png", width = 1500, height = 800, res = 150)
rpart.plot(cart_model2, extra = 100, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 0.7, cex=0.8) 
dev.off()
print(summary(cart_model2))

cart_pred_train2 <- predict(cart_model2, newdata = training_data2, type = "class")
cart_pred_train2 <- factor(cart_pred_train2, levels = all_levels) 
valid_cart_train_indices <- !is.na(cart_pred_train2) & !is.na(training_data_fatality_category)
cm_cart_train <- confusionMatrix(cart_pred_train2[valid_cart_train_indices], training_data_fatality_category[valid_cart_train_indices])
cart_accuracy_train2 <- cm_cart_train$overall["Accuracy"]

cart_pred_test2 <- predict(cart_model2, newdata = testing_data2, type = "class")
cart_pred_test2 <- factor(cart_pred_test2, levels = all_levels) 
valid_cart_test_indices <- !is.na(cart_pred_test2) & !is.na(testing_data_fatality_category)
cm_cart_test <- confusionMatrix(cart_pred_test2[valid_cart_test_indices], testing_data_fatality_category[valid_cart_test_indices])
cart_accuracy_test2 <- cm_cart_test$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train2, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test2, 4)))

#C5.0
print("Fitting C5.0 Decision Tree Model...")
c50_model2 <- C5.0(Fatality.Category ~ Year + Age.Group + Sex + Occupant_Type + Outcome, data = training_data2)
print(summary(c50_model2)) 

c50_pred_train2 <- predict(c50_model2, newdata = training_data2, type = "class")
c50_pred_train2 <- factor(c50_pred_train2, levels = all_levels)
valid_c50_train_indices <- !is.na(c50_pred_train2) & !is.na(training_data_fatality_category)
cm_c50_train <- confusionMatrix(c50_pred_train2[valid_c50_train_indices], training_data_fatality_category[valid_c50_train_indices])
c50_accuracy_train2 <- cm_c50_train$overall["Accuracy"]

c50_pred_test2 <- predict(c50_model2, newdata = testing_data2, type = "class")
c50_pred_test2 <- factor(c50_pred_test2, levels = all_levels)
valid_c50_test_indices <- !is.na(c50_pred_test2) & !is.na(testing_data_fatality_category)
cm_c50_test <- confusionMatrix(c50_pred_test2[valid_c50_test_indices], testing_data_fatality_category[valid_c50_test_indices])
c50_accuracy_test2 <- cm_c50_test$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train2, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test2, 4)))


#CHAID
print("Fitting CHAID Decision Tree Model...")
chaid_model2 <- chaid(Fatality.Category ~ Year + Age.Group + Sex + Occupant_Type + Outcome, data = training_data2,
                     control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7))
print(chaid_model2)

chaid_pred_train2 <- predict(chaid_model2, newdata = training_data2, type = "response")
chaid_pred_train2 <- factor(chaid_pred_train2, levels = all_levels)
valid_chaid_train_indices <- !is.na(chaid_pred_train2) & !is.na(training_data_fatality_category)
cm_chaid_train <- confusionMatrix(chaid_pred_train2[valid_chaid_train_indices], training_data_fatality_category[valid_chaid_train_indices])
chaid_accuracy_train2 <- cm_chaid_train$overall["Accuracy"]

chaid_pred_test2 <- predict(chaid_model2, newdata = testing_data2, type = "response")
chaid_pred_test2 <- factor(chaid_pred_test2, levels = all_levels)
valid_chaid_test_indices <- !is.na(chaid_pred_test2) & !is.na(testing_data_fatality_category)
cm_chaid_test <- confusionMatrix(chaid_pred_test2[valid_chaid_test_indices], testing_data_fatality_category[valid_chaid_test_indices])
chaid_accuracy_test2 <- cm_chaid_test$overall["Accuracy"]

print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train2, 4)))
print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test2, 4)))


results_summary2 <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train2 * 100, 2),
    round(negbin_accuracy_train2 * 100, 2),
    round(cart_accuracy_train2 * 100, 2),
    round(c50_accuracy_train2 * 100, 2),
    round(chaid_accuracy_train2 * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test2 * 100, 2),
    round(negbin_accuracy_test2 * 100, 2),
    round(cart_accuracy_test2 * 100, 2),
    round(c50_accuracy_test2 * 100, 2),
    round(chaid_accuracy_test2 * 100, 2)
  )
)

print("--- Model Comparison Summary ---")
print(results_summary2)