#read the dataset
ridge_train <- read.csv("ridge_train.csv")
ridge_test <- read.csv("ridge_test.csv")
#save the dataset
save(ridge_test, file = "../data/ridge_test.rda")
save(ridge_train, file = "../data/ridge_train.rda")
