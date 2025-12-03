library(tidyverse)
library("tidymodels")
library(glmnet)
set.seed(12325)
temp <- read.csv("data/bpo-2023.csv")
bpo_data <- drop_na(temp[-c(1:6,8, 9, 11:14, 16:19, 21:23, 25:36,38,39)])

# split data

data_split <- initial_validation_split(bpo_data, prop = c(0.7, 0.15))

train_data <- training(data_split)
test_data <- testing(data_split)
validation_data <- validation(data_split)

# stepwise regression

intercept_only <- lm(TOTAL.GREENHOUSE.GAS.EMISSIONS..metric.tons.of.CO2e. ~ 1, data = train_data)
full_model_formula <- formula(lm(TOTAL.GREENHOUSE.GAS.EMISSIONS..metric.tons.of.CO2e. ~ ., train_data))

stepwise_model <- stats::step(intercept_only, scope = full_model_formula)

# ridge regression

Y <- train_data[,5]
X <- as.matrix(train_data[,-5])

ridge_model <- cv.glmnet(x= X, y= Y, alpha = 0)
best_lambda_ridge <- ridge_model$lambda.min
best_lambda_ridge

# lasso regression

lasso_model <- cv.glmnet(x= X,y =Y, alpha = 1)
best_lambda_lasso <- lasso_model$lambda.min
best_lambda_lasso

library(tidyverse)
library(glmnet)
data(mtcars)
Y <- (mtcars[, 1]) # mpg
X <- as.matrix(mtcars[, -1]) # drop mpg
ridge_res <- cv.glmnet(X, y = Y, alpha = 0) # alpha = 0 for ridge regression
ridge_res$lambda.min

