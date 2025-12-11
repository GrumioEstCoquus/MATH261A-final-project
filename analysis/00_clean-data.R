library(tidyverse)
library("tidymodels")
library(glmnet)
set.seed(1282025)
temp <- read.csv("data/bpo-2023.csv")
bpo_data_temp <- drop_na(temp[-c(1:6,8, 9, 11:14, 16:19, 21:23, 25:36,38:39)])
bpo_data <- bpo_data_temp |>
  rename('year_built' = 'YEAR.BUILT', 'floor_area' = 'REPORTED.GROSS.FLOOR.AREA..ft.2.',
         'energy_star' = 'ENERGY.STAR.SCORE', 'tot_electricity' = 'TOTAL.ELECTRICITY.USE..kWh.',
         'greenhouse_emissions' = 'TOTAL.GREENHOUSE.GAS.EMISSIONS..metric.tons.of.CO2e.',
         'water_use' = 'TOTAL.WATER.USE..kgal.')

# visualizes 
bpo_data_temp$TOTAL.WATER.USE..kgal.
plot(bpo_data$Ye)
plot(bpo_data$REPORTED.GROSS.FLOOR.AREA..ft.2., bpo_data$greenhouse_emissions)
plot(bpo_data$tot_electricity, bpo_data$greenhouse_emissions)
plot(bpo_data$ENERGY.STAR.SCORE, bpo_data$greenhouse_emissions)
plot(bpo_data$TOTAL.WATER.USE..kgal., bpo_data$greenhouse_emissions)
pairs(bpo_data)

mean(bpo_data$greenhouse_emissions)
sd(bpo_data$greenhouse_emissions)

# split data

data_split <- initial_validation_split(bpo_data, prop = c(0.7, 0.15))

train_data <- training(data_split)
test_data <- testing(data_split)
validation_data <- validation(data_split)

# stepwise regression
water_only <- lm(greenhouse_emissions ~ water_use, data = train_data)
summary(water_only)

intercept_only <- lm(greenhouse_emissions ~ 1, data = train_data)
full_model_formula <- formula(lm(greenhouse_emissions ~ ., train_data))

stepwise_model <- stats::step(intercept_only, scope = full_model_formula)

summary(stepwise_model)

# energy use only

energy_only <- lm(greenhouse_emissions ~ tot_electricity, data = train_data)

# area only 

area_model <- lm(greenhouse_emissions ~ floor_area, data = train_data)
summary(area_model)

# full model

full_model <- lm(greenhouse_emissions ~ ., train_data)

# ridge regression

Y <- as.matrix(train_data[,5])
X <- as.matrix(train_data[,-5])

ridge_model <- cv.glmnet(x= X, y = Y, alpha = 0)
best_lambda_ridge <- ridge_model$lambda.min
best_lambda_ridge

coef_ridge <- coef(ridge_model, s ="lambda.min")
coef_ridge
# lasso regression

lasso_model <- cv.glmnet(x= X,y =Y, alpha = 1)

best_lambda_lasso <- lasso_model$lambda.min
best_lambda_lasso
coef_lasso <- coef(lasso_model, s = "lambda.min")
coef_lasso[3]

# ROOT MEAN SQUARED ERROR #

# water only

predicted <- predict(water_only, newdata = validation_data)
rmse_water <- sqrt(mean((validation_data$greenhouse_emissions - predicted)^2))

# electricity only

predicted <- predict(energy_only, newdata = validation_data)
rmse_electricty <- sqrt(mean((validation_data$greenhouse_emissions - predicted)^2))

# area only

predicted <- predict(area_model, newdata = validation_data)
rmse_area <- sqrt(mean((validation_data$greenhouse_emissions - predicted)^2))

# stepwise 

predicted <- predict(stepwise_model, newdata = validation_data)
rmse_step <- sqrt(mean((validation_data$greenhouse_emissions - predicted)^2))

# full model

predicted <- predict(full_model, newdata = validation_data)
rmse_full <- sqrt(mean((validation_data$greenhouse_emissions - predicted)^2))

# ridge regression

predicted <- predict(ridge_model, s = best_lambda_ridge, newx = as.matrix(validation_data[,-5]))
rmse_ridge <- sqrt(mean((as.matrix(validation_data[,5]) - predicted)^2))

# Lasso regression

predicted <- predict(lasso_model, s = best_lambda_lasso, newx = as.matrix(validation_data[,-5]))
rmse_lasso <- sqrt(mean((as.matrix(validation_data[,5]) - predicted)^2))


# LASSO is the best model yay!!!!!

# test data set

predicted <- predict(lasso_model, s = best_lambda_lasso, newx = as.matrix(test_data[,-5]))
rmse_best_model <- sqrt(mean((as.matrix(test_data[,5]) - predicted)^2))


rmse_table <-data.frame(
  Model = c("a", "b", "c"),
  rmse = c(1,2,3)
)

knitr::kable(
  rmse_table
)
