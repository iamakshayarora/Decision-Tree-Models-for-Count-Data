Alcohol_Datav2 <- read.csv("../Datasets/HRB National Drugs Library/Modified CSVs/Alcohol_Year_County_Gender v2.csv")


head(Alcohol_Datav2)
summary(Alcohol_Datav2)

Alcohol_Datav2 %>%
  group_by(Year) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Alcohol_Datav2 %>%
  group_by(County) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  print(n = Inf)

Alcohol_Datav2 %>%
  group_by(Age.Range) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Alcohol_Datav2 %>%
  group_by(Gender) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

unique(Alcohol_Datav2$Year)
unique(Alcohol_Datav2$County)
unique(Alcohol_Datav2$Age.Range)
unique(Alcohol_Datav2$Gender)



###########################################################


alcohol_merged_datav2 <- left_join(Alcohol_Datav2, aggregated_population_v2, by = c("Year", "County", "Age.Range", "Gender"))

head(alcohol_merged_datav2,n=15)

alcohol_merged_datav2_final <- na.omit(alcohol_merged_datav2)

head(alcohol_merged_datav2_final)

nrow(alcohol_merged_datav2_final)

str(alcohol_merged_datav2_final)

unique(alcohol_merged_datav2_final$Year)
unique(alcohol_merged_datav2_final$County)
unique(alcohol_merged_datav2_final$Age.Range)
unique(alcohol_merged_datav2_final$Gender)

summary(alcohol_merged_datav2_final)



############################################################



alcohol_merged_datav2_final <- alcohol_merged_datav2_final %>%
  mutate(
    County = as.factor(County),
    Age.Range = as.factor(Age.Range),
    Gender = as.factor(Gender),
    Year = as.factor(Year),
    Missing = as.factor(Missing)
  )


alcohol_merged_datav2_final <- alcohol_merged_datav2_final %>%
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

print(levels(alcohol_merged_datav2_final$Treatment_Category))


set.seed(123)

trainIndex4 <- createDataPartition(alcohol_merged_datav2_final$Treatment_Category, p = .7, list = FALSE, times = 1)
training_data4 <- alcohol_merged_datav2_final[trainIndex4, ]
testing_data4 <- alcohol_merged_datav2_final[-trainIndex4, ]

print(paste("Training data observations:", nrow(training_data4)))
print(paste("Testing data observations:", nrow(testing_data4)))

print("Training Data Distribution:")
print(table(training_data4$Treatment_Category))
print("Testing Data Distribution:")
print(table(testing_data4$Treatment_Category))


categorize_predictions4 <- function(predictions) {
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

training_data4$Total_Population_Binned <- cut(training_data4$Total_Population,
                                             breaks = quantile(training_data4$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                             include.lowest = TRUE, 
                                             labels = c("Q1", "Q2", "Q3", "Q4"))

training_data4$Very_Bad_General_Health_Binned <- cut(training_data4$Very_Bad_General_Health,
                                                    breaks = quantile(training_data4$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                    include.lowest = TRUE,
                                                    labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

training_data4$Bad_General_Health_Binned <- cut(training_data4$Bad_General_Health,
                                               breaks = quantile(training_data4$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

training_data4$Fair_General_Health_Binned <- cut(training_data4$Fair_General_Health,
                                                breaks = quantile(training_data4$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

training_data4$Good_General_Health_Binned <- cut(training_data4$Good_General_Health,
                                                breaks = quantile(training_data4$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

training_data4$Very_Good_General_Health_Binned <- cut(training_data4$Very_Good_General_Health,
                                                     breaks = quantile(training_data4$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

training_data4$General_Health_Not_Stated_Binned <- cut(training_data4$General_Health_Not_Stated,
                                                      breaks = quantile(training_data4$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                      include.lowest = TRUE,
                                                      labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))

testing_data4$Total_Population_Binned <- cut(testing_data4$Total_Population,
                                            breaks = quantile(training_data4$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4"))

testing_data4$Very_Bad_General_Health_Binned <- cut(testing_data4$Very_Bad_General_Health,
                                                   breaks = quantile(training_data4$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                   include.lowest = TRUE,
                                                   labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

testing_data4$Bad_General_Health_Binned <- cut(testing_data4$Bad_General_Health,
                                              breaks = quantile(training_data4$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                              include.lowest = TRUE,
                                              labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

testing_data4$Fair_General_Health_Binned <- cut(testing_data4$Fair_General_Health,
                                               breaks = quantile(training_data4$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

testing_data4$Good_General_Health_Binned <- cut(testing_data4$Good_General_Health,
                                               breaks = quantile(training_data4$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

testing_data4$Very_Good_General_Health_Binned <- cut(testing_data4$Very_Good_General_Health,
                                                    breaks = quantile(training_data4$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                    include.lowest = TRUE,
                                                    labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

testing_data4$General_Health_Not_Stated_Binned <- cut(testing_data4$General_Health_Not_Stated,
                                                     breaks = quantile(training_data4$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))



#Poisson model
poisson_model4 <- glm(Count ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     family = poisson(link = "log"),
                     data = training_data4)

print(summary(poisson_model4))
print(paste("Poisson Model AIC:", AIC(poisson_model4)))

poisson_pred_counts_train4 <- predict(poisson_model4, newdata = training_data4, type = "response")
poisson_pred_category_train4 <- categorize_predictions4(poisson_pred_counts_train4)
poisson_accuracy_train4 <- confusionMatrix(poisson_pred_category_train4, training_data4$Treatment_Category)$overall["Accuracy"]

poisson_pred_counts_test4 <- predict(poisson_model4, newdata = testing_data4, type = "response")
poisson_pred_category_test4 <- categorize_predictions4(poisson_pred_counts_test4)
poisson_accuracy_test4 <- confusionMatrix(poisson_pred_category_test4, testing_data4$Treatment_Category)$overall["Accuracy"]

print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train4, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test4, 4)))





#Negative Binomial model
negbin_model4 <- glm.nb(Count ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                         Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                         Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                       data = training_data4)

print(summary(negbin_model4))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model4)))

negbin_pred_counts_train4 <- predict(negbin_model4, newdata = training_data4, type = "response")
negbin_pred_category_train4 <- categorize_predictions4(negbin_pred_counts_train4)
negbin_accuracy_train4 <- confusionMatrix(negbin_pred_category_train4, training_data4$Treatment_Category)$overall["Accuracy"]

negbin_pred_counts_test4 <- predict(negbin_model4, newdata = testing_data4, type = "response")
negbin_pred_category_test4 <- categorize_predictions4(negbin_pred_counts_test4)
negbin_accuracy_test4 <- confusionMatrix(negbin_pred_category_test4, testing_data4$Treatment_Category)$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train4, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test4, 4)))




#CART model
cart_model4 <- rpart(Treatment_Category ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                      Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                      Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                    data = training_data4, method = "class")

rpart.plot(cart_model4, extra = 101, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 1.2)

cart_pred_train4 <- predict(cart_model4, newdata = training_data4, type = "class")
cart_accuracy_train4 <- confusionMatrix(cart_pred_train4, training_data4$Treatment_Category)$overall["Accuracy"]

cart_pred_test4 <- predict(cart_model4, newdata = testing_data4, type = "class")
cart_accuracy_test4 <- confusionMatrix(cart_pred_test4, testing_data4$Treatment_Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train4, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test4, 4)))




#C5.0 model
c50_model4 <- C5.0(Treatment_Category ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                    Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                    Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                  data = training_data4)

print(summary(c50_model4))


c50_pred_train4 <- predict(c50_model4, newdata = training_data4, type = "class")
c50_accuracy_train4 <- confusionMatrix(c50_pred_train4, training_data4$Treatment_Category)$overall["Accuracy"]

c50_pred_test4 <- predict(c50_model4, newdata = testing_data4, type = "class")
c50_accuracy_test4 <- confusionMatrix(c50_pred_test4, testing_data4$Treatment_Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train4, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test4, 4)))


#CHAID model
chaid_model4 <- chaid(Treatment_Category ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     data = training_data4,
                     control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7))

print(chaid_model4)

chaid_pred_train4 <- predict(chaid_model4, newdata = training_data4, type = "response")
chaid_accuracy_train4 <- confusionMatrix(chaid_pred_train4, training_data4$Treatment_Category)$overall["Accuracy"]

chaid_pred_test4 <- predict(chaid_model4, newdata = testing_data4, type = "response")
chaid_accuracy_test4 <- confusionMatrix(chaid_pred_test4, testing_data4$Treatment_Category)$overall["Accuracy"]

print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train4, 4)))
print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test4, 4)))





results_summary4 <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train4 * 100, 2),
    round(negbin_accuracy_train4 * 100, 2),
    round(cart_accuracy_train4 * 100, 2),
    round(c50_accuracy_train4 * 100, 2),
    round(chaid_accuracy_train4 * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test4 * 100, 2),
    round(negbin_accuracy_test4 * 100, 2),
    round(cart_accuracy_test4 * 100, 2),
    round(c50_accuracy_test4 * 100, 2),
    round(chaid_accuracy_test4 * 100, 2)
  )
)

print("--- Alcohol Model v2 Comparison Summary ---")
print(results_summary4)

