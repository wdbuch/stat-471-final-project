#read in cleaned data
health_data = read_csv('data/clean/health_data.csv')

#split into train and test
set.seed(5) #set seed
n = nrow(health_data)
train_samples = sample(1:n, round(0.8*n))

health_train = health_data[train_samples,]
health_test = health_data[-train_samples,]

#save the train and test data
write_csv(health_train, 'data/clean/health_train.csv')
write_csv(health_test, 'data/clean/health_test.csv')