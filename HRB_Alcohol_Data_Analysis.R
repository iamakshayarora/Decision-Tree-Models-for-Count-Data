Alcohol_Data <- read.csv("../Datasets/HRB National Drugs Library/Modified CSVs/Alcohol_Year_County_Gender.csv")


head(Alcohol_Data)
summary(Alcohol_Data)

Alcohol_Data %>%
  group_by(Year) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Alcohol_Data %>%
  group_by(County) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  print(n = Inf)

Alcohol_Data %>%
  group_by(Age.Range) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Alcohol_Data %>%
  group_by(Gender) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

unique(Alcohol_Data$Year)
unique(Alcohol_Data$County)
unique(Alcohol_Data$Age.Range)
unique(Alcohol_Data$Gender)



###########################################################


alcohol_merged_data <- left_join(Alcohol_Data, aggregated_population_v2, by = c("Year", "County", "Age.Range", "Gender"))

head(alcohol_merged_data,n=15)

alcohol_merged_data_final <- na.omit(alcohol_merged_data)

head(alcohol_merged_data_final)

nrow(alcohol_merged_data_final)

str(alcohol_merged_data_final)

unique(alcohol_merged_data_final$Year)
unique(alcohol_merged_data_final$County)
unique(alcohol_merged_data_final$Age.Range)
unique(alcohol_merged_data_final$Gender)

summary(alcohol_merged_data_final)



############################################################



alcohol_merged_data_final <- alcohol_merged_data_final %>%
  mutate(
    County = as.factor(County),
    Age.Range = as.factor(Age.Range),
    Gender = as.factor(Gender),
    Year = as.factor(Year)
  )


alcohol_merged_data_final <- alcohol_merged_data_final %>%
  mutate(
    Treatment_Category = factor(
      case_when(
        Count >= 0 & Count <= 20 ~ "Low",
        Count >= 21 & Count <= 100 ~ "Medium",
        Count > 100 ~ "High",
        TRUE ~ NA_character_
      ),
      levels = c("Low", "Medium", "High")
    )
  ) %>%
  filter(!is.na(Treatment_Category))

print(levels(alcohol_merged_data_final$Treatment_Category))


set.seed(123)

trainIndex <- createDataPartition(alcohol_merged_data_final$Treatment_Category, p = .7, list = FALSE, times = 1)
training_data <- alcohol_merged_data_final[trainIndex, ]
testing_data <- alcohol_merged_data_final[-trainIndex, ]

print(paste("Training data observations:", nrow(training_data)))
print(paste("Testing data observations:", nrow(testing_data)))

print("Training Data Distribution:")
print(table(training_data$Treatment_Category))
print("Testing Data Distribution:")
print(table(testing_data$Treatment_Category))


categorize_predictions <- function(predictions) {
  categories <- character(length(predictions))
  for (i in 1:length(predictions)) {
    pred_val <- predictions[i]
    if (is.na(pred_val)) {
      categories[i] <- NA_character_
      next
    }
    if (pred_val >= 0 && pred_val <= 20) {
      categories[i] <- "Low"
    } else if (pred_val > 20 && pred_val <= 100) {
      categories[i] <- "Medium"
    } else if (pred_val > 100) {
      categories[i] <- "High"
    } else {
      categories[i] <- "Low"
    }
  }
  return(factor(categories, levels = c("Low", "Medium", "High")))
}





# Converting Numerical Predictors into a Factor

training_data$Total_Population_Binned <- cut(training_data$Total_Population,
                                             breaks = quantile(training_data$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                             include.lowest = TRUE, 
                                             labels = c("Q1", "Q2", "Q3", "Q4"))

training_data$Very_Bad_General_Health_Binned <- cut(training_data$Very_Bad_General_Health,
                                                    breaks = quantile(training_data$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                    include.lowest = TRUE,
                                                    labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

training_data$Bad_General_Health_Binned <- cut(training_data$Bad_General_Health,
                                               breaks = quantile(training_data$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

training_data$Fair_General_Health_Binned <- cut(training_data$Fair_General_Health,
                                                breaks = quantile(training_data$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

training_data$Good_General_Health_Binned <- cut(training_data$Good_General_Health,
                                                breaks = quantile(training_data$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

training_data$Very_Good_General_Health_Binned <- cut(training_data$Very_Good_General_Health,
                                                     breaks = quantile(training_data$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

training_data$General_Health_Not_Stated_Binned <- cut(training_data$General_Health_Not_Stated,
                                                      breaks = quantile(training_data$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                      include.lowest = TRUE,
                                                      labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))

testing_data$Total_Population_Binned <- cut(testing_data$Total_Population,
                                            breaks = quantile(training_data$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4"))

testing_data$Very_Bad_General_Health_Binned <- cut(testing_data$Very_Bad_General_Health,
                                                   breaks = quantile(training_data$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                   include.lowest = TRUE,
                                                   labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

testing_data$Bad_General_Health_Binned <- cut(testing_data$Bad_General_Health,
                                              breaks = quantile(training_data$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                              include.lowest = TRUE,
                                              labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

testing_data$Fair_General_Health_Binned <- cut(testing_data$Fair_General_Health,
                                               breaks = quantile(training_data$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

testing_data$Good_General_Health_Binned <- cut(testing_data$Good_General_Health,
                                               breaks = quantile(training_data$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

testing_data$Very_Good_General_Health_Binned <- cut(testing_data$Very_Good_General_Health,
                                                    breaks = quantile(training_data$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                    include.lowest = TRUE,
                                                    labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

testing_data$General_Health_Not_Stated_Binned <- cut(testing_data$General_Health_Not_Stated,
                                                     breaks = quantile(training_data$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))



#Poisson model
poisson_model <- glm(Count ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     family = poisson(link = "log"),
                     data = training_data)

print(summary(poisson_model))
print(paste("Poisson Model AIC:", AIC(poisson_model)))

poisson_pred_counts_train <- predict(poisson_model, newdata = training_data, type = "response")
poisson_pred_category_train <- categorize_predictions(poisson_pred_counts_train)
poisson_accuracy_train <- confusionMatrix(poisson_pred_category_train, training_data$Treatment_Category)$overall["Accuracy"]

poisson_pred_counts_test <- predict(poisson_model, newdata = testing_data, type = "response")
poisson_pred_category_test <- categorize_predictions(poisson_pred_counts_test)
poisson_accuracy_test <- confusionMatrix(poisson_pred_category_test, testing_data$Treatment_Category)$overall["Accuracy"]

print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test, 4)))





#Negative Binomial model
negbin_model <- glm.nb(Count ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                         Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                         Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                       data = training_data)

print(summary(negbin_model))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model)))

negbin_pred_counts_train <- predict(negbin_model, newdata = training_data, type = "response")
negbin_pred_category_train <- categorize_predictions(negbin_pred_counts_train)
negbin_accuracy_train <- confusionMatrix(negbin_pred_category_train, training_data$Treatment_Category)$overall["Accuracy"]

negbin_pred_counts_test <- predict(negbin_model, newdata = testing_data, type = "response")
negbin_pred_category_test <- categorize_predictions(negbin_pred_counts_test)
negbin_accuracy_test <- confusionMatrix(negbin_pred_category_test, testing_data$Treatment_Category)$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test, 4)))




#CART model
cart_model <- rpart(Treatment_Category ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                      Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                      Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                    data = training_data, method = "class")
png("HRB_Alcohol_CART_decision_tree.png", width = 1500, height = 800, res = 150)
rpart.plot(cart_model, extra = 100, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 0.7, cex=0.7)
dev.off()

summary(cart_model)

cart_pred_train <- predict(cart_model, newdata = training_data, type = "class")
cart_accuracy_train <- confusionMatrix(cart_pred_train, training_data$Treatment_Category)$overall["Accuracy"]

cart_pred_test <- predict(cart_model, newdata = testing_data, type = "class")
cart_accuracy_test <- confusionMatrix(cart_pred_test, testing_data$Treatment_Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test, 4)))




#C5.0 model
c50_model <- C5.0(Treatment_Category ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                    Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                    Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                  data = training_data)

print(summary(c50_model))


c50_pred_train <- predict(c50_model, newdata = training_data, type = "class")
c50_accuracy_train <- confusionMatrix(c50_pred_train, training_data$Treatment_Category)$overall["Accuracy"]

c50_pred_test <- predict(c50_model, newdata = testing_data, type = "class")
c50_accuracy_test <- confusionMatrix(c50_pred_test, testing_data$Treatment_Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test, 4)))


#CHAID model
chaid_model <- chaid(Treatment_Category ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     data = training_data,
                     control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7))

print(chaid_model)

chaid_pred_train <- predict(chaid_model, newdata = training_data, type = "response")
chaid_accuracy_train <- confusionMatrix(chaid_pred_train, training_data$Treatment_Category)$overall["Accuracy"]

chaid_pred_test <- predict(chaid_model, newdata = testing_data, type = "response")
chaid_accuracy_test <- confusionMatrix(chaid_pred_test, testing_data$Treatment_Category)$overall["Accuracy"]

print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train, 4)))
print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test, 4)))





results_summary <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train * 100, 2),
    round(negbin_accuracy_train * 100, 2),
    round(cart_accuracy_train * 100, 2),
    round(c50_accuracy_train * 100, 2),
    round(chaid_accuracy_train * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test * 100, 2),
    round(negbin_accuracy_test * 100, 2),
    round(cart_accuracy_test * 100, 2),
    round(c50_accuracy_test * 100, 2),
    round(chaid_accuracy_test * 100, 2)
  )
)

print("--- Alcohol Model Comparison Summary ---")
print(results_summary)

