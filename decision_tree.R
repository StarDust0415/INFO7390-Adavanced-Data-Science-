library(rpart)
library(rpart.plot)
My_route <- "/Users/jiaminghong/Documents/Advances Data Sci:Architecture/Midterm project/after.csv"
preprocessed_data <- read.csv(My_route, 1)
preprocessed_data [,1] <- NULL
preprocessed_data [,1] <- NULL

#sample_ind <- sample(307511, 30000)
#train_data <- joint_all[-sample_ind,]
#test_data <- joint_all[sample_ind,]

sample_ind <- sample(307511, 100000)
original_data <- preprocessed_data[sample_ind,]
sample2_ind <- sample(100000, 10000)
train_data <- original_data[-sample2_ind,]
test_data <- original_data[sample2_ind,]

tree_result <- rpart(TARGET~., data = train_data, method = "class", 
                     control=rpart.control(minsplit=2, minbucket=1, cp=-1, maxdepth = 5))
#rpart.plot(tree_result)
#minsplit=2, minbucket=1,, maxdepth = 15
predict_train <- predict(tree_result, train_data)
predict_train_final <- vector(length = nrow(predict_train))
for(i in 1:nrow(predict_train)){
  predict_train_final[i] <- ifelse(predict_train[i,1]>=0.5,0,1)
}
CT_train <- table(predict_train_final,train_data$TARGET)
CT_train

predict_test <- predict(tree_result, test_data)
predict_test_final <- vector(length = nrow(predict_test))
for(i in 1:nrow(predict_test)){
  predict_test_final[i] <- ifelse(predict_test[i,1]>=0.5,0,1)
}
CT_test <- table(predict_test_final,test_data$TARGET)
CT_test
#aa <- as.data.frame(predict(tree_result, train_data))
#sum(aa$`1`>0.5)
#rpart.plot(tree_result)
evaluate_by_mse <- function(model, test){
  mean((model - test$TARGET)^2)
}
mse <- evaluate_by_mse(predict_train_final, train_data)
