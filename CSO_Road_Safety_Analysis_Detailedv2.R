Casualties_updated <- Number_of_Casualties %>%
  filter(
    Statistic.Label != "All Killed and Injured Casualties",
    Age.Group != "All ages",
    Road.User.Type != "All road users",
    Road.User.Type != "All Car users"
  )

Driver_collisions_updated <- Drivers_in_Collisions %>%
  filter(
    Statistic.Label != "All Drivers of Cars Involved in Fatal and Injury Collisions",
    Age.Group != "All ages"
  )

Car_User_collisions_updated <- Car_Users_in_Collisions %>%
  filter(
    Statistic.Label != "All Users of Cars Involved in Fatal and Injury Collisions",
    Seat.Belt.Usage != "All car drivers",
    Seat.Belt.Usage != "All front seat passengers",
  )

Vehicle_collisions_updated <- Vehicles_in_Collisions %>%
  filter(
    Statistic.Label != "All Fatal and Injury Collisions",
    Country.of.Residence != "All countries"
  )


head(Car_User_collisions_updated)
unique(Car_User_collisions_updated$Seat.Belt.Usage)
unique(Car_User_collisions_updated$Statistic.Label)


Car_User_collisions_updatedv2 <- Car_User_collisions_updated %>%
  mutate(
    Statistic.Label = case_when(
      Statistic.Label == "Users of Cars Killed in Collisions" ~ "Fatal Collisions",
      Statistic.Label == "Users of Cars Injured in Collisions" ~ "Injury Collisions",
      Statistic.Label == "Users of Cars Uninjured in Collisions" ~ "Uninjured Collisions",
      TRUE ~ Statistic.Label 
    )
  ) %>%
  pivot_wider(
    names_from = Seat.Belt.Usage, 
    values_from = VALUE          
  ) %>%
  rename_with(~gsub(" ", "_", .x), .cols = contains("seat belt")) %>%
  rename_with(~gsub(":", "", .x), .cols = contains("seat belt")) %>%
  rename_with(~gsub("-", "_", .x), .cols = contains("seat belt"))



head(Driver_collisions_updated)
unique(Driver_collisions_updated$Statistic.Label)
unique(Driver_collisions_updated$Age.Group)
unique(Driver_collisions_updated$Sex)


Driver_collisions_updatedv2 <- Driver_collisions_updated %>%
  mutate(
    Combined_Age_Group = case_when(
      Age.Group %in% c("0 - 5 years", "6 - 9 years", "10 - 14 years", "15 - 17 years") ~ "Underage Drivers",
      Age.Group %in% c("18 - 20 years", "21 - 24 years", "25 - 34 years", "35 - 44 years", "45 - 54 years") ~ "Adult Drivers",
      Age.Group %in% c("55 - 64 years", "65 years and over") ~ "Old Drivers",
      Age.Group == "Age unknown" ~ "Age Unknown", 
      TRUE ~ NA_character_ 
    )
  ) %>%

  group_by(Year, Statistic.Label, Combined_Age_Group, Sex) %>%
  summarise(
    Total_Value = sum(VALUE, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  pivot_wider(
    id_cols = c(Year, Statistic.Label), 
    names_from = c(Combined_Age_Group, Sex), 
    values_from = Total_Value,             
    names_glue = "{Combined_Age_Group}_{Sex}", 
    values_fill = 0                        
  ) %>%
  rename_with(~ paste0(., "_drivers"), .cols = ends_with("Male") | ends_with("Female")) %>%
  rename_with(~ gsub("_Male_drivers", "_male_drivers", .x), .cols = contains("_Male_drivers")) %>%
  rename_with(~ gsub("_Female_drivers", "_female_drivers", .x), .cols = contains("_Female_drivers")) %>%
  rename_with(~ gsub(" ", "_", .x), .cols = -c(Year, Statistic.Label)) %>%
  rename_with(~ gsub("-", "_", .x), .cols = -c(Year, Statistic.Label)) 


Driver_collisions_updatedv2 <- Driver_collisions_updatedv2 %>%
  mutate(
    Statistic.Label = case_when(
      Statistic.Label == "Drivers of Cars Killed in Collisions" ~ "Fatal Collisions",
      Statistic.Label == "Drivers of Cars Injured in Collisions" ~ "Injury Collisions",
      Statistic.Label == "Drivers of Cars Uninjured in Collisions" ~ "Uninjured Collisions",
      TRUE ~ Statistic.Label 
    )
  )


head(Driver_collisions_updatedv2)




head(Casualties_updated)
unique(Casualties_updated$Statistic.Label)
unique(Casualties_updated$Age.Group)
unique(Casualties_updated$Sex)
unique(Casualties_updated$Road.User.Type)



Casualties_updatedv2 <- Casualties_updated %>%

  mutate(
    Combined_Age_Group = case_when(
      Age.Group %in% c("0 - 5 years", "6 - 9 years", "10 - 14 years", "15 - 17 years") ~ "Underage",
      Age.Group %in% c("18 - 20 years", "21 - 24 years", "25 - 34 years", "35 - 44 years", "45 - 54 years") ~ "Adult",
      Age.Group %in% c("55 - 64 years", "65 years and over") ~ "Old",
      Age.Group == "Age unknown" ~ "Age Unknown",
      TRUE ~ NA_character_ 
    )
  ) %>%
  mutate(
    Sex_Label = case_when(
      Sex == "Male" ~ "Male Casualty",
      Sex == "Female" ~ "Female Casualty",
      TRUE ~ Sex 
    )
  ) %>%
  group_by(Year, Statistic.Label, Combined_Age_Group, Sex_Label, Road.User.Type) %>%
  summarise(
    Total_Value = sum(VALUE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    id_cols = c(Year, Statistic.Label),
    names_from = c(Combined_Age_Group, Sex_Label, Road.User.Type), 
    values_from = Total_Value,                              
    names_glue = "{Combined_Age_Group}_{Sex_Label}_{Road.User.Type}", 
    values_fill = 0                                          
  ) %>%
  rename_with(~ gsub(" ", "_", .x), .cols = -c(Year, Statistic.Label)) %>%
  rename_with(~ gsub("-", "_", .x), .cols = -c(Year, Statistic.Label))


head(Casualties_updatedv2)

Casualties_updatedv2 <- Casualties_updatedv2 %>%
  mutate(
    Statistic.Label = case_when(
      Statistic.Label == "Injured Casualties" ~ "Injury Collisions",
      Statistic.Label == "Killed Casualties" ~ "Fatal Collisions",
      TRUE ~ Statistic.Label 
    )
  )


head(Vehicle_collisions_updated)
unique(Vehicle_collisions_updated$Statistic.Label)
unique(Vehicle_collisions_updated$Country.of.Residence)
unique(Vehicle_collisions_updated$Type.of.Vehicle)



Vehicle_collisions_updatedv2 <- Vehicle_collisions_updated %>%
  group_by(Statistic.Label, Year) %>%
  summarise(
    Total_Value = sum(VALUE, na.rm = TRUE),
    .groups = 'drop' 
  )

head(Vehicle_collisions_updatedv2)

Car_User_collisions_updatedv2 <- Car_User_collisions_updatedv2 %>%
  dplyr::select(-UNIT)



head(Casualties_updatedv2)
head(Driver_collisions_updatedv2)
head(Car_User_collisions_updatedv2)
head(Vehicle_collisions_updatedv2)


final_joined_data <- Vehicle_collisions_updatedv2 %>%
  left_join(Driver_collisions_updatedv2, by = c("Year", "Statistic.Label")) %>%
  left_join(Car_User_collisions_updatedv2, by = c("Year", "Statistic.Label")) %>%
  left_join(Casualties_updatedv2, by = c("Year", "Statistic.Label"))


head(final_joined_data)


names(final_joined_data)


final_joined_data <- final_joined_data %>%
  mutate(
    Fatality.Category = factor(
      case_when(
        Total_Value >= 1 & Total_Value <= 149 & Statistic.Label=="Fatal Collisions" ~ "Low Fatality",
        Total_Value >= 150 & Total_Value <= 249 & Statistic.Label=="Fatal Collisions" ~ "Medium Fatality",
        Total_Value >= 250 & Statistic.Label=="Fatal Collisions" ~ "High Fatality",
        Total_Value >= 1 & Total_Value <= 5499 & Statistic.Label=="Injury Collisions" ~ "Low Injury",
        Total_Value >= 5500 & Total_Value <= 7499 & Statistic.Label=="Injury Collisions" ~ "Medium Injury",
        Total_Value >= 7500 & Statistic.Label=="Injury Collisions" ~ "High Injury",
        TRUE ~ NA_character_ 
      ),
      levels = c("Low Fatality","Medium Fatality","High Fatality","Low Injury","Medium Injury","High Injury")
    )
  )

final_joined_data <- final_joined_data %>% mutate(Year = factor(Year))



set.seed(123)
trainIndex2 <- createDataPartition(final_joined_data$Fatality.Category, p = .7, list = FALSE, times = 1)
training_data2 <- final_joined_data[trainIndex2, ]
testing_data2 <- final_joined_data[-trainIndex2, ]
training_data2 <- training_data2 %>%
  mutate(across(everything(), ~replace_na(., 0)))

print(paste("Training data observations:", nrow(training_data2)))
print(paste("Testing data observations:", nrow(testing_data2)))

categorize_predictions2 <- function(predictions, statistic_label) {
  factor(
    case_when(
      predictions >= 1 & predictions <= 149 & statistic_label == "Fatal Collisions" ~ "Low Fatality",
      predictions >= 150 & predictions <= 249 & statistic_label == "Fatal Collisions" ~ "Medium Fatality",
      predictions >= 250 & statistic_label == "Fatal Collisions" ~ "High Fatality",
      predictions >= 1 & predictions <= 5499 & statistic_label == "Injury Collisions" ~ "Low Injury",
      predictions >= 5500 & predictions <= 7499 & statistic_label == "Injury Collisions" ~ "Medium Injury",
      predictions >= 7500 & statistic_label == "Injury Collisions" ~ "High Injury",
      TRUE ~ NA_character_ 
    ),
    levels = c("Low Fatality", "Medium Fatality", "High Fatality", "Low Injury", "Medium Injury", "High Injury")
  )
}

names(final_joined_data)


poisson_model2 <- glm(
  Total_Value ~
    Statistic.Label + Year + 
    Adult_Drivers_female_drivers + Adult_Drivers_male_drivers + Age_Unknown_female_drivers + Age_Unknown_male_drivers + Old_Drivers_female_drivers + Old_Drivers_male_drivers + Underage_Drivers_female_drivers + Underage_Drivers_male_drivers + 
    `Car_drivers:_seat_belt_in_use` + 
    `Car_drivers:_seat_belt_not_in_use` + `Car_drivers:_seat_belt_usage_unknown` + `Car_drivers:_seat_belt_usage_not_stated` + `Front_seat_passengers:_seat_belt_in_use` + `Front_seat_passengers:_seat_belt_not_in_use` + `Front_seat_passengers:_seat_belt_usage_unknown` + `Front_seat_passengers:_seat_belt_usage_not_stated` + 
    Adult_Female_Casualty_Car_drivers + Adult_Female_Casualty_Car_passengers + Adult_Female_Casualty_Motor_cyclists + Adult_Female_Casualty_Other_road_users + Adult_Female_Casualty_Pedal_cyclists + Adult_Female_Casualty_Pedestrians + 
    Adult_Male_Casualty_Car_drivers + Adult_Male_Casualty_Car_passengers + Adult_Male_Casualty_Motor_cyclists + Adult_Male_Casualty_Other_road_users + Adult_Male_Casualty_Pedal_cyclists + Adult_Male_Casualty_Pedestrians + 
    Age_Unknown_Female_Casualty_Car_drivers + Age_Unknown_Female_Casualty_Car_passengers + Age_Unknown_Female_Casualty_Motor_cyclists + Age_Unknown_Female_Casualty_Other_road_users + Age_Unknown_Female_Casualty_Pedal_cyclists + Age_Unknown_Female_Casualty_Pedestrians + 
    Age_Unknown_Male_Casualty_Car_drivers + Age_Unknown_Male_Casualty_Car_passengers + Age_Unknown_Male_Casualty_Motor_cyclists + Age_Unknown_Male_Casualty_Other_road_users + Age_Unknown_Male_Casualty_Pedal_cyclists + Age_Unknown_Male_Casualty_Pedestrians + 
    Old_Female_Casualty_Car_drivers + Old_Female_Casualty_Car_passengers + Old_Female_Casualty_Motor_cyclists + Old_Female_Casualty_Other_road_users + Old_Female_Casualty_Pedal_cyclists + Old_Female_Casualty_Pedestrians + 
    Old_Male_Casualty_Car_drivers + Old_Male_Casualty_Car_passengers + Old_Male_Casualty_Motor_cyclists + Old_Male_Casualty_Other_road_users + Old_Male_Casualty_Pedal_cyclists + Old_Male_Casualty_Pedestrians + 
    Underage_Female_Casualty_Car_drivers + Underage_Female_Casualty_Car_passengers + Underage_Female_Casualty_Motor_cyclists + Underage_Female_Casualty_Other_road_users + Underage_Female_Casualty_Pedal_cyclists + Underage_Female_Casualty_Pedestrians +
    Underage_Male_Casualty_Car_drivers + Underage_Male_Casualty_Car_passengers + Underage_Male_Casualty_Motor_cyclists + Underage_Male_Casualty_Other_road_users + Underage_Male_Casualty_Pedal_cyclists + Underage_Male_Casualty_Pedestrians,
  family = poisson,
  data = training_data2
)
print(summary(poisson_model2))
print(paste("Poisson Model AIC:", AIC(poisson_model2)))
print(paste("Poisson Model Deviance:", deviance(poisson_model2)))


poisson_pred_counts_train2 <- predict(poisson_model2, newdata = training_data2, type = "response")
poisson_pred_category_train2 <- categorize_predictions2(poisson_pred_counts_train2,statistic_label = training_data2$Statistic.Label)
poisson_accuracy_train2 <- confusionMatrix(poisson_pred_category_train2, training_data2$Fatality.Category)$overall["Accuracy"]

poisson_pred_counts_test2 <- predict(poisson_model2, newdata = testing_data2, type = "response")
poisson_pred_category_test2 <- categorize_predictions2(poisson_pred_counts_test2,statistic_label = testing_data2$Statistic.Label)
poisson_accuracy_test2 <- confusionMatrix(poisson_pred_category_test2, testing_data2$Fatality.Category)$overall["Accuracy"]

print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train2, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test2, 4)))


negbin_model <- glm.nb(VALUE ~ MonthName + Year, data = training_data)
print(summary(negbin_model))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model)))
print(paste("Negative Binomial Model Deviance:", deviance(negbin_model)))


negbin_pred_counts_train <- predict(negbin_model, newdata = training_data, type = "response")
negbin_pred_category_train <- categorize_predictions2(negbin_pred_counts_train)
negbin_accuracy_train <- confusionMatrix(negbin_pred_category_train, training_data$Fatality.Category)$overall["Accuracy"]

negbin_pred_counts_test <- predict(negbin_model, newdata = testing_data, type = "response")
negbin_pred_category_test <- categorize_predictions2(negbin_pred_counts_test)
negbin_accuracy_test <- confusionMatrix(negbin_pred_category_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test, 4)))




cart_model <- rpart(Fatality.Category ~ MonthName + Year, data = training_data, method = "class")
rpart.plot(cart_model, extra = 101, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 1.2, cex=0.5)
print(summary(cart_model))


cart_pred_train <- predict(cart_model, newdata = training_data, type = "class")
cart_accuracy_train <- confusionMatrix(cart_pred_train, training_data$Fatality.Category)$overall["Accuracy"]

cart_pred_test <- predict(cart_model, newdata = testing_data, type = "class")
cart_accuracy_test <- confusionMatrix(cart_pred_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test, 4)))


c50_model <- C5.0(Fatality.Category ~ MonthName + Year, data = training_data) 


c50_pred_train <- predict(c50_model, newdata = training_data, type = "class")
c50_accuracy_train <- confusionMatrix(c50_pred_train, training_data$Fatality.Category)$overall["Accuracy"]

c50_pred_test <- predict(c50_model, newdata = testing_data, type = "class")
c50_accuracy_test <- confusionMatrix(c50_pred_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test, 4)))



chaid_model <- chaid(Fatality.Category ~ MonthName + Year, data = training_data, 
                     control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7)) 
print(chaid_model)


chaid_pred_train <- predict(chaid_model, newdata = training_data, type = "response")
chaid_accuracy_train <- confusionMatrix(chaid_pred_train, training_data$Fatality.Category)$overall["Accuracy"]

chaid_pred_test <- predict(chaid_model, newdata = testing_data, type = "response")
chaid_accuracy_test <- confusionMatrix(chaid_pred_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train, 4)))
print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test, 4)))



results_summary <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train * 100, 2),
    ifelse(is.null(negbin_model), NA, round(negbin_accuracy_train * 100, 2)),
    round(cart_accuracy_train * 100, 2),
    round(c50_accuracy_train * 100, 2),
    round(chaid_accuracy_train * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test * 100, 2),
    ifelse(is.null(negbin_model), NA, round(negbin_accuracy_test * 100, 2)),
    round(cart_accuracy_test * 100, 2),
    round(c50_accuracy_test * 100, 2),
    round(chaid_accuracy_test * 100, 2)
  )
)

print("--- Model Comparison Summary ---")
print(results_summary)