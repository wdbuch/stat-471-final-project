#load libraries
library(tidyverse)
library(glmnetUtils)
source('code/functions/plot_glmnet.R') 

#read in the training data
health_train = read_csv('data/clean/health_train.csv')

#run linear regression
linear_fit = lm(formula = per_capita_health_cost ~ . - fips,
                data = health_train)

#check residuals for normality
OLS_normality_check = health_train %>%
  ggplot(aes(x = linear_fit$residuals)) +
  geom_histogram(fill = 'lightblue') +
  labs(x = 'Linear Model Fit Residuals', y = 'Count') +
  theme_bw()

#save the residuals plot
ggsave(filename = 'results/OLS-normality-check.png', 
       plot = OLS_normality_check, 
       device = 'png', 
       width = 5, 
       height = 4)

#save the linear fit object
save(linear_fit, file = 'results/linear_fit.Rda')

#screenshot included in results
#summary(linear_fit)

#run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(per_capita_health_cost ~ . - fips,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = health_train)

#save the ridge fit object
save(ridge_fit, file = 'results/ridge_fit.Rda')

#create ridge trace plot
p = plot_glmnet(ridge_fit, health_train, features_to_plot = 6)
ggsave(filename = 'results/ridge-trace-plot.png', 
       plot = p, 
       device = 'png', 
       width = 6, 
       height = 4)

#run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(per_capita_health_cost ~ . - fips,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = health_train)

#save the lasso fit object
save(lasso_fit, file = 'results/lasso_fit.Rda')

#create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = 'in', 
    filename = 'results/lasso-cv-plot.png')
plot(lasso_fit)
dev.off()

#create lasso trace plot
p = plot_glmnet(lasso_fit, health_train, features_to_plot = 6)
ggsave(filename = 'results/lasso-trace-plot.png', 
       plot = p, 
       device = 'png', 
       width = 6, 
       height = 4)

#extract features selected by lasso and their coefficients
beta_hat_std2 = extract_std_coefs(lasso_fit, health_train)
beta_hat_std2 %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv('results/lasso-features-table.tsv')