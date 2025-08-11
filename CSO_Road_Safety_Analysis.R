# install.packages(c("dplyr", "lubridate", "MASS", "rpart", "rpart.plot", "caret", "CHAID", "C50"))
library(dplyr)
library(lubridate)
library(MASS) 
library(rpart) 
library(rpart.plot) 
library(caret) 

#install.packages("C50")
library(C50)
#install.packages("CHAID")
#install.packages("CHAID", repos="http://R-Forge.R-project.org") 
library(CHAID)

RoadFatalities_new <- RoadFatalities %>%
  mutate(
    YearMonthStr = Month,
    Year = as.numeric(substr(Month, 1, 4)),
    MonthName = factor(gsub("^[0-9]{4} ", "", Month), levels = month.name)
  ) %>%
  dplyr::select(Year, MonthName, VALUE) %>%
  filter(Year != 2025)


RoadFatalities_new <- RoadFatalities_new %>%
  mutate(
    Fatality.Category = factor(
      case_when(
        VALUE >= 1 & VALUE <= 9 ~ "Low",
        VALUE >= 10 & VALUE <= 29 ~ "Medium",
        VALUE >= 30 ~ "High",
        TRUE ~ NA_character_ 
      ),
      levels = c("Low","Medium","High")
    )
  )

RoadFatalities_new <- RoadFatalities_new %>% mutate(Year = factor(Year))

print(head(RoadFatalities_new))
print(summary(RoadFatalities_new))
table(RoadFatalities_new$Fatality.Category)


set.seed(123)
trainIndex <- createDataPartition(RoadFatalities_new$Fatality.Category, p = .7, list = FALSE, times = 1)
training_data <- RoadFatalities_new[trainIndex, ]
testing_data <- RoadFatalities_new[-trainIndex, ]

print(paste("Training data observations:", nrow(training_data)))
print(paste("Testing data observations:", nrow(testing_data)))

categorize_predictions <- function(predictions) {
  factor(
    case_when(
      predictions >= 1 & predictions <= 9 ~ "Low",
      predictions >= 10 & predictions <= 29 ~ "Medium",
      predictions >= 30 ~ "High",
      TRUE ~ "Low" 
    ),
    levels = c("Low","Medium","High")
  )
}

#Poisson Regression

poisson_model <- glm(VALUE ~ MonthName + Year, family = poisson, data = training_data)
print(summary(poisson_model))
print(paste("Poisson Model AIC:", AIC(poisson_model)))
print(paste("Poisson Model Deviance:", deviance(poisson_model)))


poisson_pred_counts_train <- predict(poisson_model, newdata = training_data, type = "response")
poisson_pred_category_train <- categorize_predictions(poisson_pred_counts_train)
poisson_accuracy_train <- confusionMatrix(poisson_pred_category_train, training_data$Fatality.Category)$overall["Accuracy"]

poisson_pred_counts_test <- predict(poisson_model, newdata = testing_data, type = "response")
poisson_pred_category_test <- categorize_predictions(poisson_pred_counts_test)
poisson_accuracy_test <- confusionMatrix(poisson_pred_category_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("Poisson Training Accuracy:", round(poisson_accuracy_train, 4)))
print(paste("Poisson Testing Accuracy:", round(poisson_accuracy_test, 4)))


#Negative Binomial Regression

negbin_model <- glm.nb(VALUE ~ MonthName + Year, data = training_data)
print(summary(negbin_model))
print(paste("Negative Binomial Model AIC:", AIC(negbin_model)))
print(paste("Negative Binomial Model Deviance:", deviance(negbin_model)))

negbin_pred_counts_train <- predict(negbin_model, newdata = training_data, type = "response")
negbin_pred_category_train <- categorize_predictions(negbin_pred_counts_train)
negbin_accuracy_train <- confusionMatrix(negbin_pred_category_train, training_data$Fatality.Category)$overall["Accuracy"]

negbin_pred_counts_test <- predict(negbin_model, newdata = testing_data, type = "response")
negbin_pred_category_test <- categorize_predictions(negbin_pred_counts_test)
negbin_accuracy_test <- confusionMatrix(negbin_pred_category_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("Negative Binomial Training Accuracy:", round(negbin_accuracy_train, 4)))
print(paste("Negative Binomial Testing Accuracy:", round(negbin_accuracy_test, 4)))


#CART

cart_model <- rpart(Fatality.Category ~ MonthName + Year, data = training_data, method = "class")
rpart.plot(cart_model, extra = 101, fallen.leaves = TRUE, main = "CART Decision Tree", tweak = 1.2, cex=0.5)
print(summary(cart_model))

cart_pred_train <- predict(cart_model, newdata = training_data, type = "class")
cart_accuracy_train <- confusionMatrix(cart_pred_train, training_data$Fatality.Category)$overall["Accuracy"]

cart_pred_test <- predict(cart_model, newdata = testing_data, type = "class")
cart_accuracy_test <- confusionMatrix(cart_pred_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("CART Training Accuracy:", round(cart_accuracy_train, 4)))
print(paste("CART Testing Accuracy:", round(cart_accuracy_test, 4)))

#C5.0

c50_model <- C5.0(Fatality.Category ~ MonthName + Year, data = training_data) 

c50_pred_train <- predict(c50_model, newdata = training_data, type = "class")
c50_accuracy_train <- confusionMatrix(c50_pred_train, training_data$Fatality.Category)$overall["Accuracy"]

c50_pred_test <- predict(c50_model, newdata = testing_data, type = "class")
c50_accuracy_test <- confusionMatrix(c50_pred_test, testing_data$Fatality.Category)$overall["Accuracy"]

print(paste("C5.0 Training Accuracy:", round(c50_accuracy_train, 4)))
print(paste("C5.0 Testing Accuracy:", round(c50_accuracy_test, 4)))


#CHAID

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

