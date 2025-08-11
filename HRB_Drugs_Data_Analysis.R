Drugs_Data <- read.csv("../Datasets/HRB National Drugs Library/Modified CSVs/Drugs_Year_County_Gender.csv")


head(Drugs_Data)
summary(Drugs_Data)

Drugs_Data %>%
  group_by(Year) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Drugs_Data %>%
  group_by(County) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  print(n = Inf)

Drugs_Data %>%
  group_by(Age.Range) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Drugs_Data %>%
  group_by(Gender) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

unique(Drugs_Data$Year)
unique(Drugs_Data$County)
unique(Drugs_Data$Age.Range)
unique(Drugs_Data$Gender)



###########################################################


Drugs_merged_data <- left_join(Drugs_Data, aggregated_population_v2, by = c("Year", "County", "Age.Range", "Gender"))

head(Drugs_merged_data,n=15)

Drugs_merged_data_final <- na.omit(Drugs_merged_data)

head(Drugs_merged_data_final)

nrow(Drugs_merged_data_final)

str(Drugs_merged_data_final)

unique(Drugs_merged_data_final$Year)
unique(Drugs_merged_data_final$County)
unique(Drugs_merged_data_final$Age.Range)
unique(Drugs_merged_data_final$Gender)

summary(Drugs_merged_data_final)



############################################################



Drugs_merged_data_final <- Drugs_merged_data_final %>%
  mutate(
    County = as.factor(County),
    Age.Range = as.factor(Age.Range),
    Gender = as.factor(Gender),
    Year = as.factor(Year)
  )


Drugs_merged_data_final <- Drugs_merged_data_final %>%
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

print(levels(Drugs_merged_data_final$Treatment_Category))


set.seed(123)

trainIndex3 <- createDataPartition(Drugs_merged_data_final$Treatment_Category, p = .7, list = FALSE, times = 1)
training_data3 <- Drugs_merged_data_final[trainIndex3, ]
testing_data3 <- Drugs_merged_data_final[-trainIndex3, ]

print(paste("Training data observations:", nrow(training_data3)))
print(paste("Testing data observations:", nrow(testing_data3)))

print("Training Data Distribution:")
print(table(training_data3$Treatment_Category))
print("Testing Data Distribution:")
print(table(testing_data3$Treatment_Category))


categorize_predictions3 <- function(predictions) {
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

training_data3$Total_Population_Binned <- cut(training_data3$Total_Population,
                                              breaks = quantile(training_data3$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                              include.lowest = TRUE, 
                                              labels = c("Q1", "Q2", "Q3", "Q4")) 


training_data3$Very_Bad_General_Health_Binned <- cut(training_data3$Very_Bad_General_Health,
                                                     breaks = quantile(training_data3$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

training_data3$Bad_General_Health_Binned <- cut(training_data3$Bad_General_Health,
                                                breaks = quantile(training_data3$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

training_data3$Fair_General_Health_Binned <- cut(training_data3$Fair_General_Health,
                                                 breaks = quantile(training_data3$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                 include.lowest = TRUE,
                                                 labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

training_data3$Good_General_Health_Binned <- cut(training_data3$Good_General_Health,
                                                 breaks = quantile(training_data3$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                 include.lowest = TRUE,
                                                 labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

training_data3$Very_Good_General_Health_Binned <- cut(training_data3$Very_Good_General_Health,
                                                      breaks = quantile(training_data3$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                      include.lowest = TRUE,
                                                      labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

training_data3$General_Health_Not_Stated_Binned <- cut(training_data3$General_Health_Not_Stated,
                                                       breaks = quantile(training_data3$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                       include.lowest = TRUE,
                                                       labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))

testing_data3$Total_Population_Binned <- cut(testing_data3$Total_Population,
                                             breaks = quantile(training_data3$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                             include.lowest = TRUE,
                                             labels = c("Q1", "Q2", "Q3", "Q4"))

testing_data3$Very_Bad_General_Health_Binned <- cut(testing_data3$Very_Bad_General_Health,
                                                    breaks = quantile(training_data3$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                    include.lowest = TRUE,
                                                    labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

testing_data3$Bad_General_Health_Binned <- cut(testing_data3$Bad_General_Health,
                                               breaks = quantile(training_data3$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

testing_data3$Fair_General_Health_Binned <- cut(testing_data3$Fair_General_Health,
                                                breaks = quantile(training_data3$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

testing_data3$Good_General_Health_Binned <- cut(testing_data3$Good_General_Health,
                                                breaks = quantile(training_data3$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

testing_data3$Very_Good_General_Health_Binned <- cut(testing_data3$Very_Good_General_Health,
                                                     breaks = quantile(training_data3$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

testing_data3$General_Health_Not_Stated_Binned <- cut(testing_data3$General_Health_Not_Stated,
                                                      breaks = quantile(training_data3$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                      include.lowest = TRUE,
                                                      labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))





#Poisson model
poisson_model3 <- glm(Count ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                        Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                        Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     family = poisson(link = "log"),
                     data = training_data3)

print(summary(poisson_model3))
print(paste("Poisson Model AIC:", AIC(poisson_model3)))

poisson_pred_counts_train3 <- predict(poisson_model3, newdata = training_data3, type = "response")
poisson_pred_category_train3 <- categorize_predictions3(poisson_pred_counts_train3)
poisson_accuracy_train3 <- confusionMatrix(poisson_pred_category_train3, training_data3$Treatment_Category)$overall["Accuracy"]

poisson_pred_counts_test3 <- predict(poisson_model3, newdata = testing_data3, type = "response")
poisson_pred_category_test3 <- categorize_predictions3(poisson_pred_counts_test3)
poisson_accuracy_test3 <- confusionMatrix(poisson_pred_category_test3, testing_data3$Treatment_Category)$overall["Accuracy"]

print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train3, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test3, 4)))





#Negative Binomial model
negbin_model3 <- glm.nb(Count ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                          Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                          Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                       data = training_data3)

print(summary(negbin_model3))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model3)))

negbin_pred_counts_train3 <- predict(negbin_model3, newdata = training_data3, type = "response")
negbin_pred_category_train3 <- categorize_predictions3(negbin_pred_counts_train3)
negbin_accuracy_train3 <- confusionMatrix(negbin_pred_category_train3, training_data3$Treatment_Category)$overall["Accuracy"]

negbin_pred_counts_test3 <- predict(negbin_model3, newdata = testing_data3, type = "response")
negbin_pred_category_test3 <- categorize_predictions3(negbin_pred_counts_test3)
negbin_accuracy_test3 <- confusionMatrix(negbin_pred_category_test3, testing_data3$Treatment_Category)$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train3, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test3, 4)))




#CART model
cart_model3 <- rpart(Treatment_Category ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                    data = training_data3, method = "class")

png("HRB_Drugs_CART_decision_tree.png", width = 1500, height = 800, res = 150)
rpart.plot(cart_model3, extra = 100, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 0.8, cex=0.7)
dev.off()
summary(cart_model3)

cart_pred_train3 <- predict(cart_model3, newdata = training_data3, type = "class")
cart_accuracy_train3 <- confusionMatrix(cart_pred_train3, training_data3$Treatment_Category)$overall["Accuracy"]

cart_pred_test3 <- predict(cart_model3, newdata = testing_data3, type = "class")
cart_accuracy_test3 <- confusionMatrix(cart_pred_test3, testing_data3$Treatment_Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train3, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test3, 4)))




#C5.0 model
c50_model3 <- C5.0(Treatment_Category ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                     Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                     Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                  data = training_data3)

print(summary(c50_model3))

c50_pred_train3 <- predict(c50_model3, newdata = training_data3, type = "class")
c50_accuracy_train3 <- confusionMatrix(c50_pred_train3, training_data3$Treatment_Category)$overall["Accuracy"]

c50_pred_test3 <- predict(c50_model3, newdata = testing_data3, type = "class")
c50_accuracy_test3 <- confusionMatrix(c50_pred_test3, testing_data3$Treatment_Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train3, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test3, 4)))





#CHAID model
chaid_model3 <- chaid(Treatment_Category ~ Year + County + Age.Range + Gender + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     data = training_data3,
                     control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7))

print(chaid_model3)

chaid_pred_train3 <- predict(chaid_model3, newdata = training_data3, type = "response")
chaid_accuracy_train3 <- confusionMatrix(chaid_pred_train3, training_data3$Treatment_Category)$overall["Accuracy"]

chaid_pred_test3 <- predict(chaid_model3, newdata = testing_data3, type = "response")
chaid_accuracy_test3 <- confusionMatrix(chaid_pred_test3, testing_data3$Treatment_Category)$overall["Accuracy"]

print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train3, 4)))
print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test3, 4)))






results_summary3 <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train3 * 100, 2),
    round(negbin_accuracy_train3 * 100, 2),
    round(cart_accuracy_train3 * 100, 2),
    round(c50_accuracy_train3 * 100, 2),
    round(chaid_accuracy_train3 * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test3 * 100, 2),
    round(negbin_accuracy_test3 * 100, 2),
    round(cart_accuracy_test3 * 100, 2),
    round(c50_accuracy_test3 * 100, 2),
    round(chaid_accuracy_test3 * 100, 2)
  )
)

print("--- Drugs Model Comparison Summary ---")
print(results_summary3)

