library(randomForest)
library(xgboost)
library(Metrics)

#IMPORT TABLE
trsf <- read.csv("/Users/yizheliu/Desktop/midterm project/after.csv")
trainData <- sample(nrow(trsf), 0.1*nrow(trsf), replace = FALSE)
data <- trsf[trainData,]
data <- data[,-1]
dim(data)

#SAMPLE DATA
set.seed(12345)
sample <- sample(nrow(data),size = 0.9*nrow(data))
train_data <- data[sample,]
test_data <- data[-sample,]

#RANDOM FOREST
rfMod <- randomForest(TARGET ~.,data = train_data, importance=TRUE,
                      mtry=66, ntree=500, proximity=TRUE)
rfMod
rf <- predict(rfMod, newdata = test_data)
rf
err_rf <- mean(as.numeric(rf > 0.5) != test_data$TARGET)
err_rf
rmse_rf <- rmse(as.matrix(test_data["TARGET"]),rf)
rmse_rf
mse <- rmse_rf^2
mse
importance(rfMod)
varImpPlot(rfMod)

#XGBOOST
dtrain <- xgb.DMatrix(data = as.matrix(train_data[!names(train_data) %in% c("TARGET")]), 
                      label = train_data$TARGET)
train_data.xgb <- xgboost(data=dtrain, 
                          max_depth=7,
                          eta = 0.2, 
                          nthread=4, 
                          nrounds=50, 
                          lambda=1, 
                          objective="reg:linear")
dtest <- as.matrix(test_data[!names(train_data) %in% c("TARGET")])
predic.xgb <- predict(train_data.xgb,dtest)
predic.xgb
err <- mean(as.numeric(predic.xgb > 0.5) != test_data$TARGET)
err
rmse_xgb <- rmse(as.matrix(test_data["TARGET"]),predic.xgb)
rmse_xgb
mse_xgb <- rmse_xgb^2
mse_xgb
importance <- xgb.importance(colnames(train_data[!names(train_data) %in% c("TARGET")]), model=train_data.xgb)
importance
importance1 <- importance[1:10,]
importance1
xgb.plot.importance(importance1, rel_to_first=TRUE, xlab="Relative Importance")
