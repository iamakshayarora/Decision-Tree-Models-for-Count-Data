Drugs_Datav2 <- read.csv("../Datasets/HRB National Drugs Library/Modified CSVs/Drugs_Year_County_Gender v2.csv")


head(Drugs_Datav2)
summary(Drugs_Datav2)

Drugs_Datav2 %>%
  group_by(Year) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Drugs_Datav2 %>%
  group_by(County) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  print(n = Inf)

Drugs_Datav2 %>%
  group_by(Age.Range) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

Drugs_Datav2 %>%
  group_by(Gender) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count))

unique(Drugs_Datav2$Year)
unique(Drugs_Datav2$County)
unique(Drugs_Datav2$Age.Range)
unique(Drugs_Datav2$Gender)



###########################################################


Drugs_merged_datav2 <- left_join(Drugs_Datav2, aggregated_population_v2, by = c("Year", "County", "Age.Range", "Gender"))

head(Drugs_merged_datav2,n=15)

Drugs_merged_datav2_final <- na.omit(Drugs_merged_datav2)

head(Drugs_merged_datav2_final)

nrow(Drugs_merged_datav2_final)

str(Drugs_merged_datav2_final)

unique(Drugs_merged_datav2_final$Year)
unique(Drugs_merged_datav2_final$County)
unique(Drugs_merged_datav2_final$Age.Range)
unique(Drugs_merged_datav2_final$Gender)

summary(Drugs_merged_datav2_final)



############################################################



Drugs_merged_datav2_final <- Drugs_merged_datav2_final %>%
  mutate(
    County = as.factor(County),
    Age.Range = as.factor(Age.Range),
    Gender = as.factor(Gender),
    Year = as.factor(Year),
    Missing  = as.factor(Missing)
  )


Drugs_merged_datav2_final <- Drugs_merged_datav2_final %>%
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

print(levels(Drugs_merged_datav2_final$Treatment_Category))


set.seed(123)

trainIndex5 <- createDataPartition(Drugs_merged_datav2_final$Treatment_Category, p = .7, list = FALSE, times = 1)
training_data5 <- Drugs_merged_datav2_final[trainIndex5, ]
testing_data5 <- Drugs_merged_datav2_final[-trainIndex5, ]

print(paste("Training data observations:", nrow(training_data5)))
print(paste("Testing data observations:", nrow(testing_data5)))

print("Training Data Distribution:")
print(table(training_data5$Treatment_Category))
print("Testing Data Distribution:")
print(table(testing_data5$Treatment_Category))


categorize_predictions5 <- function(predictions) {
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

training_data5$Total_Population_Binned <- cut(training_data5$Total_Population,
                                              breaks = quantile(training_data5$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                              include.lowest = TRUE, 
                                              labels = c("Q1", "Q2", "Q3", "Q4")) 


training_data5$Very_Bad_General_Health_Binned <- cut(training_data5$Very_Bad_General_Health,
                                                     breaks = quantile(training_data5$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

training_data5$Bad_General_Health_Binned <- cut(training_data5$Bad_General_Health,
                                                breaks = quantile(training_data5$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

training_data5$Fair_General_Health_Binned <- cut(training_data5$Fair_General_Health,
                                                 breaks = quantile(training_data5$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                 include.lowest = TRUE,
                                                 labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

training_data5$Good_General_Health_Binned <- cut(training_data5$Good_General_Health,
                                                 breaks = quantile(training_data5$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                 include.lowest = TRUE,
                                                 labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

training_data5$Very_Good_General_Health_Binned <- cut(training_data5$Very_Good_General_Health,
                                                      breaks = quantile(training_data5$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                      include.lowest = TRUE,
                                                      labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

training_data5$General_Health_Not_Stated_Binned <- cut(training_data5$General_Health_Not_Stated,
                                                       breaks = quantile(training_data5$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                       include.lowest = TRUE,
                                                       labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))

testing_data5$Total_Population_Binned <- cut(testing_data5$Total_Population,
                                             breaks = quantile(training_data5$Total_Population, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                             include.lowest = TRUE,
                                             labels = c("Q1", "Q2", "Q3", "Q4"))

testing_data5$Very_Bad_General_Health_Binned <- cut(testing_data5$Very_Bad_General_Health,
                                                    breaks = quantile(training_data5$Very_Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                    include.lowest = TRUE,
                                                    labels = c("VBGH_Q1", "VBGH_Q2", "VBGH_Q3", "VBGH_Q4"))

testing_data5$Bad_General_Health_Binned <- cut(testing_data5$Bad_General_Health,
                                               breaks = quantile(training_data5$Bad_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                               include.lowest = TRUE,
                                               labels = c("BGH_Q1", "BGH_Q2", "BGH_Q3", "BGH_Q4"))

testing_data5$Fair_General_Health_Binned <- cut(testing_data5$Fair_General_Health,
                                                breaks = quantile(training_data5$Fair_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("FGH_Q1", "FGH_Q2", "FGH_Q3", "FGH_Q4"))

testing_data5$Good_General_Health_Binned <- cut(testing_data5$Good_General_Health,
                                                breaks = quantile(training_data5$Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                include.lowest = TRUE,
                                                labels = c("GGH_Q1", "GGH_Q2", "GGH_Q3", "GGH_Q4"))

testing_data5$Very_Good_General_Health_Binned <- cut(testing_data5$Very_Good_General_Health,
                                                     breaks = quantile(training_data5$Very_Good_General_Health, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                     include.lowest = TRUE,
                                                     labels = c("VGGH_Q1", "VGGH_Q2", "VGGH_Q3", "VGGH_Q4"))

testing_data5$General_Health_Not_Stated_Binned <- cut(testing_data5$General_Health_Not_Stated,
                                                      breaks = quantile(training_data5$General_Health_Not_Stated, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                                      include.lowest = TRUE,
                                                      labels = c("GHNS_Q1", "GHNS_Q2", "GHNS_Q3", "GHNS_Q4"))





#Poisson model
poisson_model5 <- glm(Count ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                        Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                        Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                      family = poisson(link = "log"),
                      data = training_data5)

print(summary(poisson_model5))
print(paste("Poisson Model AIC:", AIC(poisson_model5)))

poisson_pred_counts_train5 <- predict(poisson_model5, newdata = training_data5, type = "response")
poisson_pred_category_train5 <- categorize_predictions5(poisson_pred_counts_train5)
poisson_accuracy_train5 <- confusionMatrix(poisson_pred_category_train5, training_data5$Treatment_Category)$overall["Accuracy"]

poisson_pred_counts_test5 <- predict(poisson_model5, newdata = testing_data5, type = "response")
poisson_pred_category_test5 <- categorize_predictions5(poisson_pred_counts_test5)
poisson_accuracy_test5 <- confusionMatrix(poisson_pred_category_test5, testing_data5$Treatment_Category)$overall["Accuracy"]

print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train5, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test5, 4)))





#Negative Binomial model
negbin_model5 <- glm.nb(Count ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                          Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                          Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                        data = training_data5)

print(summary(negbin_model5))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model5)))

negbin_pred_counts_train5 <- predict(negbin_model5, newdata = training_data5, type = "response")
negbin_pred_category_train5 <- categorize_predictions5(negbin_pred_counts_train5)
negbin_accuracy_train5 <- confusionMatrix(negbin_pred_category_train5, training_data5$Treatment_Category)$overall["Accuracy"]

negbin_pred_counts_test5 <- predict(negbin_model5, newdata = testing_data5, type = "response")
negbin_pred_category_test5 <- categorize_predictions5(negbin_pred_counts_test5)
negbin_accuracy_test5 <- confusionMatrix(negbin_pred_category_test5, testing_data5$Treatment_Category)$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train5, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test5, 4)))




#CART model
cart_model5 <- rpart(Treatment_Category ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                       Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                       Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                     data = training_data5, method = "class")

rpart.plot(cart_model5, extra = 101, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 1.2)

cart_pred_train5 <- predict(cart_model5, newdata = training_data5, type = "class")
cart_accuracy_train5 <- confusionMatrix(cart_pred_train5, training_data5$Treatment_Category)$overall["Accuracy"]

cart_pred_test5 <- predict(cart_model5, newdata = testing_data5, type = "class")
cart_accuracy_test5 <- confusionMatrix(cart_pred_test5, testing_data5$Treatment_Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train5, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test5, 4)))




#C5.0 model
c50_model5 <- C5.0(Treatment_Category ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                     Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                     Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                   data = training_data5)

print(summary(c50_model5))

c50_pred_train5 <- predict(c50_model5, newdata = training_data5, type = "class")
c50_accuracy_train5 <- confusionMatrix(c50_pred_train5, training_data5$Treatment_Category)$overall["Accuracy"]

c50_pred_test5 <- predict(c50_model5, newdata = testing_data5, type = "class")
c50_accuracy_test5 <- confusionMatrix(c50_pred_test5, testing_data5$Treatment_Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train5, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test5, 4)))





#CHAID model
chaid_model5 <- chaid(Treatment_Category ~ Year + County + Age.Range + Gender + Missing + Total_Population_Binned +
                        Very_Bad_General_Health_Binned + Bad_General_Health_Binned + Fair_General_Health_Binned +
                        Good_General_Health_Binned + Very_Good_General_Health_Binned + General_Health_Not_Stated_Binned,
                      data = training_data5,
                      control = chaid_control(alpha2 = 0.05, alpha4 = 0.05, minsplit = 20, minbucket = 7))

print(chaid_model5)

chaid_pred_train5 <- predict(chaid_model5, newdata = training_data5, type = "response")
chaid_accuracy_train5 <- confusionMatrix(chaid_pred_train5, training_data5$Treatment_Category)$overall["Accuracy"]

chaid_pred_test5 <- predict(chaid_model5, newdata = testing_data5, type = "response")
chaid_accuracy_test5 <- confusionMatrix(chaid_pred_test5, testing_data5$Treatment_Category)$overall["Accuracy"]

print(paste("CHAID Training Accuracy:", round(chaid_accuracy_train5, 4)))
print(paste("CHAID Testing Accuracy:", round(chaid_accuracy_test5, 4)))






results_summary5 <- data.frame(
  Model = c("Poisson Regression", "Negative Binomial Regression", "CART", "C5.0", "CHAID"),
  Training_Accuracy_Pct = c(
    round(poisson_accuracy_train5 * 100, 2),
    round(negbin_accuracy_train5 * 100, 2),
    round(cart_accuracy_train5 * 100, 2),
    round(c50_accuracy_train5 * 100, 2),
    round(chaid_accuracy_train5 * 100, 2)
  ),
  Testing_Accuracy_Pct = c(
    round(poisson_accuracy_test5 * 100, 2),
    round(negbin_accuracy_test5 * 100, 2),
    round(cart_accuracy_test5 * 100, 2),
    round(c50_accuracy_test5 * 100, 2),
    round(chaid_accuracy_test5 * 100, 2)
  )
)

print("--- Drugs Model v2 Comparison Summary ---")
print(results_summary5)

