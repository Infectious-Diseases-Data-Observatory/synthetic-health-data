source("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/LB_syn_wide/prepprocessing.R")


DMLB$is_real <- "T"
DMLB_syn$is_real <- "F"
DMLB_dist <- rbind(DMLB, DMLB_syn)


# Convert the target variable to a factor if it's not already
DMLB_dist$is_real <- as.factor(DMLB_dist$is_real)

trainIndex <- createDataPartition(DMLB_dist$is_real, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
DMLB_dist_train <- DMLB_dist[ trainIndex,]
DMLB_dist_test  <- DMLB_dist[-trainIndex,]


tune_grid <- expand.grid(
  nrounds = c(100, 150, 200),   # Number of boosting rounds
  max_depth = c(3, 5, 7),       # Maximum depth of trees
  eta = c(0.1, 0.2, 0.3),       # Learning rate
  gamma = 0,                    # Minimum loss reduction required to make a further partition
  colsample_bytree = 0.75,      # Subsample ratio of columns when constructing each tree
  min_child_weight = 1,         # Minimum sum of instance weight (hessian) needed in a child
  subsample = 0.8               # Subsample ratio of the training instance
)


cv_control <- trainControl(
  method = "cv",                # Cross-validation
  number = 5,                   # 5-fold cross-validation
  verboseIter = TRUE,           # Print training log
  classProbs = TRUE,            # Needed for AUC or ROC curves
  summaryFunction = twoClassSummary, # For binary classification
  allowParallel = TRUE          # Allow parallel processing
)

gbdt_mult <- train(
  is_real ~ .,                  # Model formula
  data = DMLB_dist_train,       # Training data
  method = "xgbTree",           # Model type
  trControl = cv_control,       # Cross-validation settings
  tuneGrid = tune_grid,         # Tuning grid
  metric = "ROC",               # Use ROC as the evaluation metric
  na.action = na.pass
)


# Extract the results
results_mult <- gbdt_mult$results[, c("nrounds", "max_depth", "eta", "ROC")]
print(results_mult)
# Predict the probabilities on the test set
pred_prob_mult <- predict(gbdt_mult, newdata = DMLB_dist_test, type = "prob")
prob_real_mult <- pred_prob_mult[, "T"]
# Compute the distinguishability score
dist_score_mult <- mean((prob_real_mult - 0.5)^2)
print(paste("Distinguishability Score:", dist_score_mult))



# - Retrain Specific Tree - #


tune_grid <- expand.grid(
  nrounds = c(200),    # Number of boosting rounds
  max_depth = c(7),       # Maximum depth of trees
  eta = c(0.3),      # Learning rate
  gamma = 0,                    # Minimum loss reduction required to make a further partition
  colsample_bytree = 0.75,      # Subsample ratio of columns when constructing each tree
  min_child_weight = 1,         # Minimum sum of instance weight (hessian) needed in a child
  subsample = 0.8               # Subsample ratio of the training instance
)

cv_control <- trainControl(
  method = "cv",                # Cross-validation
  number = 5,                   # 5-fold cross-validation
  verboseIter = TRUE,           # Print training log
  classProbs = TRUE,            # Needed for AUC or ROC curves
  summaryFunction = twoClassSummary, # For binary classification
  allowParallel = TRUE          # Allow parallel processing
)

gbdt_27 <- train(
  is_real ~ .,                  # Model formula
  data = DMLB_dist_train,       # Training data
  method = "xgbTree",           # Model type
  trControl = cv_control,       # Cross-validation settings
  tuneGrid = tune_grid,         # Tuning grid
  metric = "ROC",               # Use ROC as the evaluation metric
  na.action = na.pass
)


# - Evaluation - #


# Extract the results
results_27 <- data.frame(gbdt_27$results)
results_27 <- data.frame(results_27[, c("nrounds", "max_depth", "eta", "ROC")])
print(results_27)


# Predict the probabilities on the test set
pred_prob_27 <- predict(gbdt_27, newdata = DMLB_dist_test, type = "prob")
prob_real_27 <- pred_prob_27[, "T"]
# Compute the distinguishability score
dist_score_27 <- mean((prob_real_27 - 0.5)^2)
print(paste("Distinguishability Score:", dist_score_27))

