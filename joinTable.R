#import table 
#setwd("/Users/shixinying/Desktop/7390/Assignment/mid-term")
#join_file <-  "installments_payments.csv"
#join_data <-read.csv(join_file ,header=T,sep=",")
join_data = data.frame(read.csv('/Users/yizheliu/Desktop/midterm project/installments_payments.csv', header = 1))
head(join_data)

#DAYS subtraciton
join_data$df_days <- (join_data$DAYS_ENTRY_PAYMENT - join_data$DAYS_INSTALMENT)
#AMT
join_data$df_AMT <- (join_data$AMT_PAYMENT - join_data$AMT_INSTALMENT)
#delete useless cols
join_data <- join_data[,-c(1,3,4,5,6,7,8)]
head(join_data)
#library(dplyr)
#join_data <- mutate(join_data, df_installments = join_data[,c(5)] - lag(join_data[,c(4)]))
#head(join_data)

#check the frequnecy 
id_number <- as.data.frame(table(join_data$SK_ID_CURR))
id_number

#calculate the mean of every id
cal_mean <- aggregate(.~ join_data$SK_ID_CURR, data=join_data, mean)
cal_mean

done_join_data <- cal_mean[,-1]
done_join_data


