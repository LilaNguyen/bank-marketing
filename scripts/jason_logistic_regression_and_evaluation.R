library(dplyr)
library(janitor)
library(ggplot2)
library(pROC)
library(pscl)

# ------------------------- Load cleaned data set -------------------------
source("scripts/01_data_cleaning_and_eda.R")
data <- bank_model


# ------------------------- Analyze cleaned data structure -------------------------
View(data)
glimpse(data)
table(data$y)


# ------------------------- Create binary target -------------------------
data$y_binary <- ifelse(data$y == "yes", 1, 0)
table(data$y_binary)


# ------------------------- Create train/test split -------------------------
n <- nrow(data)
train_index <- sample(1:n, size = 0.7 * n)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# ------------------------- Base logistic regression model -------------------------
model_base <- glm(y_binary ~ age + job + marital + education + default + housing + loan + contact + 
                    month + day_of_week + 
                    campaign + previous_contact + previous + poutcome,
                    data = train_data,
                    family = "binomial")
summary(model_base)
base_percent_devReduction <- (model_base$null.deviance - model_base$deviance) / model_base$null.deviance
base_percent_devReduction
# [NOTE]: Base model with no interaction improves deviance by ~16.94%


# ------------------------- Predict using base model for a comparison baseline ------------------------- 
# Predict probabilities for base model
test_data$pred_prob_base <- predict(model_base, newdata = test_data, type = "response")

# Create base ROC object
roc_base_obj <- roc(test_data$y_binary, test_data$pred_prob_base) # computes all possible thresholds and performance values

# Determine best balancing threshold between sensitivity and specificity
best <- coords(roc_base_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
best_threshold <- as.numeric(best["threshold"]) 

# Classify using best threshold
test_data$pred_class_base <- ifelse(test_data$pred_prob_base > best_threshold, 1, 0)

# Confusion matrix
base_cm <- table(Actual = test_data$y_binary, 
      Predicted = test_data$pred_class_base)

# Base model: Sensitivity & Specificity
TN <- base_cm["0", "0"] # true negative
FN <- base_cm["1", "0"] # false negative
TP <- base_cm["1", "1"] # true negative
FP <- base_cm["0", "1"] # false positive
base_sensitivity <- TP / (TP + FN)
base_specificity <- TN / (TN + FP)
base_sensitivity
base_specificity
# [NOTE]: Base model is decently accurate at predicting NO (specificity ~86.53%)
# but worse at predicting YES (sensitivity ~53.35%)


# ------------------------- Base ROC curve -------------------------
# Create new data frame for base ROC
roc_base_df <- data.frame(tpr = roc_base_obj$sensitivities,
                          fpr = 1 - roc_base_obj$specificities,
                          model = "Base Model")

# AUC value for base model
base_auc <- auc(roc_base_obj)

# Graph base ROC curve
base_roc <- ggplot(roc_base_df, aes(x = fpr, y = tpr)) +
                  geom_line(color = "pink", linewidth = 1) +
                  geom_abline(linetype = "dashed", color = "darkgrey") +
                  labs(title = paste0("Base ROC Curve (AUC = ", round(base_auc, 3), ")"),
                       x = "False Positive Rate",
                       y = "True Positive Rate") +
                  theme_light()
base_roc


# ------------------------- Interaction logistic regression model -------------------------
model_interact <- glm(y_binary ~ age + job + marital + education + default + housing + loan + contact + 
                        month + day_of_week + 
                        campaign + previous_contact + previous + poutcome +
                        campaign * poutcome + 
                        previous * poutcome +
                        education * job +
                        age * campaign +
                        default * loan +
                        campaign * loan,
                      data = train_data,
                      family = "binomial")

# Selecting meaningful factors using backward selection
model_selected <- step(model_interact, direction = "backward")

# Comparing percentage of deviance reduction between based and selected interaction model
summary(model_selected)
selected_percent_devReduction <- (model_selected$null.deviance - model_selected$deviance) / model_selected$deviance
selected_percent_devReduction
AIC(model_base, model_selected)
# [NOTE]: Interaction model improves deviance by ~20.27%, so about 3.33% improvement
# Interaction model reduced AIC by 4.0 compared to base model
# Also reduced df by 10 (47 -> 37), suggesting the selected interaction model is better fitting, less complex with fewer variables used


# ------------------------- Predict using selected interaction model -------------------------
# Predict probabilities for selected interaction model
test_data$pred_prob_selected <- predict(model_selected, newdata = test_data, type = "response")

# Create ROC object for selected interaction model
roc_selected_obj <- roc(test_data$y_binary, test_data$pred_prob_selected)

# Determine best balanced threshold for selected interaction model
best <- coords(roc_selected_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
balance_threshold <- as.numeric(best["threshold"])

# Classify using the balanced threshold
test_data$pred_class_selected <- ifelse(test_data$pred_prob_selected > balance_threshold, 1, 0)

# Confusion matrix
base_cm <- table(Actual = test_data$y_binary, 
                 Predicted = test_data$pred_class_selected)

# Selected model: Sensitivity & Specificity
TN <- base_cm["0", "0"] # true negative
FN <- base_cm["1", "0"] # false negative
TP <- base_cm["1", "1"] # true negative
FP <- base_cm["0", "1"] # false positive
selected_sensitivity <- TP / (TP + FN)
selected_specificity <- TN / (TN + FP)
selected_sensitivity
selected_specificity
# [NOTE]: Interaction model improved sensitivity by ~1.02% but reduced specificity by ~1.24%

# Combining the two ROC data frames into one
roc_selected_df <- data.frame(tpr = roc_selected_obj$sensitivities,
                              fpr = 1 - roc_selected_obj$specificities,
                              model = "Selected Interaction Model")
roc_combined <- rbind(roc_base_df, roc_selected_df) 
selected_auc <- auc(roc_selected_obj)

# Plot both ROC curves for comparison
compare_roc <- ggplot(roc_combined, aes(x = fpr, y = tpr, color = model, linewidth = model)) +
                      geom_line() +
                      geom_abline(linetype = "dashed", color = "darkgrey") +
                      scale_color_manual(
                        values = c("Base Model" = "pink", "Selected Interaction Model" = "darkblue")
                      ) +
                      scale_linewidth_manual(
                        values = c("Base Model" = 2, "Selected Interaction Model" = 0.6) 
                      ) +
                      labs(title = paste0("ROC Curve Comparison (Base AUC = ", round(base_auc, 5), " | Selected AUC = ", round(selected_auc, 5), ")"),
                           x = "False Positive Rate",
                           y = "True Positive Rate",
                           color = "Model Type Color",
                           linewidth = "Model Type Line Width"
                      ) +
                      theme_light()
compare_roc

# Finding summary table
final_model_comparison <- data.frame(Base_Deviance_Reduction = base_percent_devReduction, Selected_Deviance_Reduction = selected_percent_devReduction, 
                                     Base_Pseudo_R2 = round(pR2(model_base)["McFadden"], 5), Selected_Pseudo_R2 = round(pR2(model_selected)["McFadden"], 5), Base_Sensitivity = base_sensitivity,
                                     Selected_Sensitivity = selected_sensitivity, Base_Specificity = base_specificity, Selected_Specificity = selected_specificity)
View(final_model_comparison)



