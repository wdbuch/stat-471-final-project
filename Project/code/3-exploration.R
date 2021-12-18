#load libraries
library(kableExtra)
library(tidyverse)
library(ggcorrplot)
library(viridis)

#read in the training data
health_train = read_csv('data/clean/health_train.csv')

#plot variable correlation
health_corr = health_train %>%
  select(-fips, -per_capita_health_cost, -party) #deselect

health_corr
correlation = cor_pmat(health_corr)

corr_plot = ggcorrplot(correlation, hc.order = TRUE, type = 'lower', 
                       outline.color = 'white',
                       ggtheme = ggplot2::theme_gray,
                       colors = c('#6D9EC1', 'white', '#E46726'))

#save the correlation plot
ggsave(filename = 'results/corr-plot.png',
       plot = corr_plot,
       device = 'png',
       width = 20,
       height = 20)

#calculate the median healthcare cost per capita
median_health_cost = median(health_train$per_capita_health_cost)

#plot a histogram of the per capita health expense distribution
hist = health_train %>%
  ggplot(aes(x = per_capita_health_cost)) +
  geom_histogram(fill = 'lightblue') +
  labs(x = 'Per Capita Healthcare Speding ($)', y = 'FIPS Count') +
  theme_bw() +
  geom_vline(xintercept = median_health_cost, color = 'black', 
             linetype = 'dashed') +
  annotate('text', x = median_health_cost + 2250, y = 70,
           label = paste('median = $10,428'))

#save the histogram
ggsave(filename = 'results/cost-per-capita-histogram.png',
       plot = hist,
       device = 'png',
       width = 5,
       height = 3)

#plot a boxplot of per capita health expense by party
box = health_train %>%
  select(per_capita_health_cost, party) %>%
  ggplot(aes(x = party, y = per_capita_health_cost)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = .6) +
  geom_jitter(size = .3, alpha = .3) +
  theme_bw() +
  labs(x = 'Political Party', y = 'Per Capita Healthcare Cost ($)') +
  theme(legend.position = 'none')

#save the boxplot
ggsave(filename = 'results/cost-per-capita-party-boxplot.png',
       plot = box,
       device = 'png',
       width = 5,
       height = 5)

#examine top five counties by per capita healthcare spending
health_train %>%
  select(fips, per_capita_health_cost) %>%
  arrange(desc(per_capita_health_cost)) %>%
  head(5) %>%
  write_csv('results/top-5-spenders-per-capita-data.csv')

#examine bottom five counties by per capita healthcare spending
health_train %>%
  select(fips, per_capita_health_cost) %>%
  arrange(per_capita_health_cost) %>%
  head(5) %>%
  write_csv('results/bottom-5-spenders-per-capita-data.csv')