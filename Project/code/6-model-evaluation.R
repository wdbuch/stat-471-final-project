#load libraries
library(glmnetUtils)
library(tidyverse)

#load test data
health_test = read_csv('data/clean/health_test.csv')

#mutate the test data
health_test = health_test %>%
  mutate(health_test, party = as.factor(party)) #convert party to factor

#load linear fit object
load('results/linear_fit.Rda')

#load ridge fit object
load('results/ridge_fit.Rda')

#load lasso fit object
load('results/lasso_fit.Rda')

#load random forest object
load('results/rf_fit_tuned.Rda')

#load boosting object
load('results/gbm_fit_tuned.Rda')

#evaluate OLS RMSE
ols_RMSE = sqrt(mean(linear_fit$residuals^2))

#evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = health_test, 
                            s = 'lambda.1se') %>%
  as.numeric()

ridge_RMSE = sqrt(mean((ridge_predictions -
                          health_test$per_capita_health_cost)^2))

#evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = health_test,
                            s = 'lambda.1se') %>%
  as.numeric()

lasso_RMSE = sqrt(mean((lasso_predictions -
                          health_test$per_capita_health_cost)^2))

#evaluate random forest RMSE
rf_predictions = predict(rf_fit_tuned,
                         newdata = health_test)

rf_RMSE = sqrt(mean((rf_predictions -
                       health_test$per_capita_health_cost)^2))

#evaluate boosting RMSE
gbm_predictions = predict(gbm_fit_tuned,
                          n.trees = optimal_num_trees,
                          newdata = health_test)

boost_RMSE = sqrt(mean((gbm_predictions -
                          health_test$per_capita_health_cost)^2))

#write table
tibble(Method = c('OLS', 'Ridge', 'Lasso', 'Random Forest', 'Boosting'),
       `Test RMSE` = c(ols_RMSE , ridge_RMSE, lasso_RMSE,
                       rf_RMSE, boost_RMSE)) %>%
  arrange(`Test RMSE`) %>%
  write_tsv('results/model-evaluation.tsv')