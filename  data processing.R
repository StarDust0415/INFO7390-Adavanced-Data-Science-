#install.packages("formattable")
library(formattable)
library(caret)
setwd("/Users/yizheliu/Desktop/midterm project")
# READ TRAIN AND JOIN DATA
file <- "application_train.csv"
train<-read.csv(file,header=T,sep=",")
dim(train)
join_file <-  "joinTable.csv"
join_data <-read.csv(join_file ,header=T,sep=",")
# TRANSFER DTA TO DATA FRAME
train <- as.data.frame(train)
join_data <- as.data.frame(join_data)
# MERGE DATA
train <- merge(x = train, y = join_data, by = "SK_ID_CURR", all.x = TRUE)
dim(train)
head(train)
summary(train)
train$DAYS_EMPLOYED[train$DAYS_EMPLOYED == 365243] <- NA
col <- colnames(train)
df_total = data.frame()
for(name in col) {
  nul_value <- sum(is.na(train[,name]))
  num <- nul_value/length(train[,name])
  if(num < 0.2) {
    df <- data.frame(c(name),c(percent(num)))
    df_total <- rbind(df_total,df)
  }
}
dim(df_total)
head(df_total)
df_total[order(df_total[,2]),]
keep_col <- df_total[,1]
keep_col
#Data <- subset( train, select = c(df_total[1,] ))
new_train <- train[, names(train) %in% keep_col, drop = F]
dim(new_train)
# one-hot-encoding categorical features
ohe_feats = c('NAME_TYPE_SUITE', 'NAME_INCOME_TYPE', 'NAME_EDUCATION_TYPE','NAME_FAMILY_STATUS','NAME_HOUSING_TYPE')
dmy <- dummyVars(" ~ .", data = new_train)
trsf <- data.frame(predict(dmy, newdata = new_train))
dim(trsf)
data.class(trsf)
options("scipen"=999)
head(trsf)
# colMeans(trsf,na.rm = TRUE)
# mean(trsf,na.rm = TRUE)

col_new <- colnames(trsf)
for(name in col_new) {
  trsf[is.na(trsf[,name]),name] <- mean(trsf[,name], na.rm=T)
  print(sum(is.na(trsf[,name])))
}
#algae[is.na(algae$Chla),"Chla"] <- mean(algae$Chla, na.rm=T)
print(trsf)
#write.csv(trsf, "after.csv")
dim(trsf)

