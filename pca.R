require(pls)
setwd("/Users/silu.zhao/Documents/advanced data science/midterm/all")
file <- "after.csv"
train<-read.csv(file,header=T,sep=",")
dim(train)
set.seed(12345)
sample_new = sample(307511,round(0.9*307511))
trainData <- train[sample_new,]
trainData <- train[1:1000,]
pcr_model <- pcr(TARGET~., data = trainData, scale = FALSE, validation = "CV")
summary(pcr_model)
# Plot the root mean squared error
validationplot(pcr_model)
# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")
predplot(pcr_model)
test <- train[1000:2000,]
#head(test)
y_test <- train[1000:2000, 3]
pcr_pred <- predict(pcr_model, test, ncomp = 3)
mean((pcr_pred - y_test)^2)
