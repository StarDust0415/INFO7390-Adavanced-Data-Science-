#library(ggplot2)
setwd("/Users/shixinying/Desktop/7390/Assignment/mid-term")
# READ TRAIN AND JOIN DATA
file <- "after.csv"
train <- read.csv(file,header=T,sep=",")
train <- train[,-1]
train <- train[,-1]

# SAMPLE 90% DATA TO TRAIN DATA & 10% DATA TO TEST
set.seed(12345)
sample_new <- sample(nrow(train),round(0.9*nrow(train)))
trainData <- train[sample_new,]
testData <- train[-sample_new,]

# APPLYING LINEAR REGRESSION
linearModel <- lm(trainData$TARGET ~ ., data = trainData)
summary(linearModel)

# MAKE THE PREDICT USING TEST DATA AND CALCULATE THE MSE
pred1 <- predict(linearModel, testData)
mse1 <- mean((pred1 - testData$TARGET)^2)
show(mse1)

# P-value tests whether R-squared is different from 0. 
# Usually we need a p-value lower than 0.05 to show a statistically significant relationship between X and Y. 
# It indicates the reliability of X to predict Y. 
# Performs an F-test on the model. This takes the parameters of our model and compares it to a model that has fewer parmeters. 
# In theory the model with more parameters should fit better. 
# If the model with more parameters (your model) doesn't perform better than the model with fewer parameters, 
# the F-test will have a high p-value. 
# If the model with more parameters is better than the model with fewer parameters, you will have a lower p-value.

# SELECT THE PARAMETERS WHICH SIGNIFICANCE LOWER THAN 0.05
p_val <- summary(linearModel)$coef
select_col <- rownames(data.frame(p_val[p_val[,4] <= 0.05, 4])[,0]) 

# DIVIDE THE NEW TRAIN DATA AND TEST DATA
trainData_selectCol <- trainData[,select_col]
testData_selectCol <- testData[,select_col]
dim(trainData_selectCol)  # 206 -> 53
dim(testData_selectCol) # 30751,53

# BUILD A NEW MODEL AND PREDICT
# CALCULATE THE MSE VALUE
linearModel2 <- lm(trainData$TARGET ~ ., trainData_selectCol)
pred <- predict(linearModel2, testData_selectCol)
MSE2 <- mean((pred - testData$TARGET)^2)
show(MSE2)

library(forecast)
accuracy(pred,testData$TARGET)

# p_val2 <- summary(linearModel2)$coef
# select_col2 <- rownames(data.frame(p_val2[p_val2[,4] <= 0.05, 4])[,0])
# select_col2 <- select_col2[-1]
# selectCol <- trainData.selectCol[,select_col2]
# testSelect <- trainData.selectCol[,select_col2]
# dim(selectCol)
# linearModel3 <- lm(trainData$TARGET ~., data=selectCol)
# pred3 <- predict(linearModel3, data = testSelect)
# MSE3 <- mean((pred3 - testData$TARGET)^2)
# show(MSE3)
#install.packages('pROC')
library(pROC)
ROC <- roc(testData$TARGET,pred,levels=c('0','1'), plot=TRUE, print.auc=TRUE)
#print.thres=TRUE
summary(linearModel2)
