#load libraries
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(tidyverse)
library(kableExtra)
library(cowplot) 
source('code/functions/plot_glmnet.R')

#read in the training data
health_train = read_csv('data/clean/health_train.csv')

#mutate the training data
health_train = health_train %>%
  mutate(health_train, party = as.factor(party)) #convert party to factor

#run random forest
time_100 = system.time(randomForest(per_capita_health_cost ~ .-fips,
                                    importance = FALSE,
                                    ntree = 100,
                                    data = health_train))['elapsed']

time_200 = system.time(randomForest(per_capita_health_cost ~ .-fips,
                                    importance = FALSE,
                                    ntree = 200,
                                    data = health_train))['elapsed']

time_300 = system.time(randomForest(per_capita_health_cost ~ .-fips,
                                    importance = FALSE,
                                    ntree = 300,
                                    data = health_train))['elapsed']

time_400 = system.time(randomForest(per_capita_health_cost ~ .-fips,
                                    importance = FALSE,
                                    ntree = 400,
                                    data = health_train))['elapsed']

time_500 = system.time(randomForest(per_capita_health_cost ~ .-fips,
                                    importance = FALSE,
                                    ntree = 500,
                                    data = health_train))['elapsed']

rf_times = tibble(ntree = c(100, 200, 300, 400, 500),
                  elapsed_time = c(time_100, time_200, time_300,
                                   time_400, time_500))

set.seed(1)
num_m_to_try = as.integer(20 / time_100)
num_m = as.integer(max(num_m_to_try, 5))
mvalues = as.integer(seq.int(from = 1, to = 42, length.out = num_m))
mse = numeric(length(mvalues))
ntree = 100
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(per_capita_health_cost ~ .-fips, mtry = m, data = health_train)
  mse[idx] = rf_fit$mse[ntree]
}

#out of bag error plot
m_and_mse = tibble(m = mvalues, mse = mse)
oob_error = m_and_mse %>%
  ggplot(aes(x = m, y = mse)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = mvalues) +
  labs(x = 'm', y = 'Out of Bag Error') +
  theme_bw()

#save the oob error plot
ggsave(filename = 'results/oob-error-plot.png', 
       plot = oob_error, 
       device = 'png', 
       width = 6, 
       height = 6)

#calculate best m
best_m = m_and_mse %>%
  arrange(mse) %>%
  head(1) %>%
  pull(m)

set.seed(5)
rf_fit_tuned = randomForest(per_capita_health_cost ~ . -fips, mtry =
                              best_m, ntree = 500, importance = TRUE,
                            data = health_train)

#save random forest model
save(rf_fit_tuned, file = 'results/rf_fit_tuned.Rda')

#best m random forest plot
mrf = tibble(mse = rf_fit_tuned$mse, trees = 1:500)
mrf = mrf %>%
  ggplot(aes(x = trees, y = mse)) +
  geom_line() +
  labs(x = 'Trees', y = 'CV Error', title = 'Random Forest, m = 36') +
  theme_bw()

#save the m random forest plot
ggsave(filename = 'results/m-random-forest-plot.png', 
       plot = mrf, 
       device = 'png', 
       width = 4, 
       height = 3)

#create the variable importance plot
png(width = 10, 
    height = 10,
    res = 300,
    units = 'in', 
    filename = 'results/variable-importance-plot.png')
varImpPlot(rf_fit_tuned, n.var = 10)
dev.off()

#Boosting
set.seed(1)
gbm_fit_1 = gbm(per_capita_health_cost ~ . -fips,
                distribution = 'gaussian',
                n.trees = 1000,
                interaction.depth = 1,
                shrinkage = 0.1,
                cv.folds = 5,
                data = health_train)

gbm_fit_2 = gbm(per_capita_health_cost ~ . -fips,
                distribution = 'gaussian',
                n.trees = 1000,
                interaction.depth = 2,
                shrinkage = 0.1,
                cv.folds = 5,
                data = health_train)

gbm_fit_3 = gbm(per_capita_health_cost ~ . -fips,
                distribution = 'gaussian',
                n.trees = 1000,
                interaction.depth = 3,
                shrinkage = 0.1,
                cv.folds = 5,
                data = health_train)

ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3)
)

#plot boosting CV errors
boost_cv_errors = cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, color = factor(depth))) +
  geom_hline(yintercept = min(gbm_fit_1$cv.error),
             linetype = 'dashed', color = 'red') +
  geom_hline(yintercept = min(gbm_fit_2$cv.error),
             linetype = 'dashed', color = 'green') +
  geom_hline(yintercept = min(gbm_fit_3$cv.error),
             linetype = 'dashed', color = 'blue') +
  geom_line() +
  scale_color_manual(labels = c('1', '2', '3'),
                     values = c('red', 'green', 'blue')) +
  labs(x = 'n trees', y = 'CV Error', color = 'Interaction Depth') +
  theme_bw()

#save the boosting cv errors plot
ggsave(filename = 'results/boosting-cv-errors-plot.png',
       plot = boost_cv_errors,
       device = 'png',
       width = 6,
       height = 5)

#calculate optimal number of trees
gbm_fit_tuned = gbm_fit_2
optimal_num_trees = gbm.perf(gbm_fit_2, plot.it = FALSE)

#save first ten rows of relative influence table for optimal boosting model
relative_influence_table = summary(gbm_fit_tuned, n.trees =
                                     optimal_num_trees, plotit = FALSE) %>%
  head(10) %>% 
  write_tsv('results/relative-influence-table.tsv')

#save the boosting model
save(gbm_fit_tuned, file = 'results/gbm_fit_tuned.Rda')