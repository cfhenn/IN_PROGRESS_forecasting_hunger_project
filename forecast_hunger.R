library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)
library(BAS)
library(neuralnet)

df <- read.csv("Downloads/df_all_merged.csv")

df$index <-  seq.int(nrow(df))

climate_vars      <- c("maxtemp", "mintemp", "avgtemp", "nightlight", "precipitation")
months_considered <- c("one_month_ago", "two_month_ago")

for(i in 1:length(months_considered)) { 
  for (j in 1:length(climate_vars)) {
    for (k in 1:i){
      df$index[k] <- i
    }
    df$new[(df$index > i)] <- df[,(names(df) == climate_vars[j])][df$index-i]                     
    colnames(df)[ncol(df)] <- paste0(climate_vars[j],"_", months_considered[i])  # Rename column name
  }
}

df <- df[complete.cases(df), ] #glmnet won't take  NAs



drop <- c("enumeration_area", "month", "index")
df = df[,!(names(df) %in% drop)]

set.seed(100) 

index = sample(1:nrow(df), 0.7*nrow(df)) 

train <- df[index,] # Create the training data 
test  <- df[-index,] # Create the test data

cols = c( 'nightlight', 'precipitation', 'maxtemp', 'mintemp', 'avgtemp', 
          'maxtemp_one_month_ago', 'mintemp_one_month_ago', 'avgtemp_one_month_ago', 'nightlight_one_month_ago', 'precipitation_one_month_ago',
          'maxtemp_two_month_ago', 'mintemp_two_month_ago', 'avgtemp_two_month_ago', 'nightlight_two_month_ago', 'precipitation_two_month_ago')

pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

#ORDINARY LEAST SQUARES REGRESSION
ols_model = lm(hunger ~ ., data = train)
summary(ols_model)

#Create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Predict and evaluate the model on training data
predictions = predict(ols_model, newdata = train)
eval_metrics(ols_model, train, predictions, target = 'hunger')

# Predict and evaluate the model on test data
predictions = predict(ols_model, newdata = test)
eval_metrics(ols_model, test, predictions, target = 'hunger')

#REGULARIZATION
cols_reg = c( 'hunger', 'nightlight', 'precipitation', 'maxtemp', 'mintemp', 'avgtemp', 
              'maxtemp_one_month_ago', 'mintemp_one_month_ago', 'avgtemp_one_month_ago', 'nightlight_one_month_ago', 'precipitation_one_month_ago',
              'maxtemp_two_month_ago', 'mintemp_two_month_ago', 'avgtemp_two_month_ago', 'nightlight_two_month_ago', 'precipitation_two_month_ago')

dummies <- dummyVars(hunger ~ ., data = df[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))

#RIDGE REGRESSION
x = as.matrix(train_dummies)
y_train = train$hunger

x_test = as.matrix(test_dummies)
y_test = test$hunger

lambdas <- 10^seq(2, -3, by = -.1)


ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)

cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)


#LASSO REGRESSION
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best lambda
lambda_best <- lasso_reg$lambda.min 

# Estimate model using best lambda
lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

# Prediction and evaluation on training data
predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)

#BAYESIAN REGRESSION
model_bas <- bas.lm(hunger ~ .,
                     data = df,
                     method = "MCMC",
                     prior = "ZS-null",
                     modelprior = uniform())

summary(model_bas)


#NEURAL NETWORK 
#currently probably not  enough data to make this an effective method
#but I may be able to add more data when more becomes public
#currently, it just classifies eas  into  those where more than 25% of the population is  hugnry
#& those where  less than  25% are hunger - analog predictions  to be added  laters

nn  <- neuralnet(hunger~., data=train, hidden=3, act.fct = "logistic", linear.output = FALSE)



# Binary classification
nn <- neuralnet(hunger > 0.25 ~ ., train, linear.output = FALSE)
pred <- predict(nn, test)
table(test$hunger > 0.25, pred[, 1] > 0.5)


