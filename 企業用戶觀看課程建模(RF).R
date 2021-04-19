setwd("D:/cw_intern/raw data")
library(data.table)
library(dplyr)
dat4 = fread("用戶看課分析(含產業別).csv")
dat5 <- dat4 %>% 
  select(用戶ID, 單堂課ID, 產業別)  
fwrite(dat5, "用戶看課建模.csv")

# =========== RF model =========== #
library(rsample)      # data splitting 
library(randomForest) # basic implementation

### 原始檔案太大無法執行(6.2G)
# set.seed(123)
# dat5_split = initial_split(data = dat5, prop = .7)
# dat5_train = training(dat5_split)
# dat5_test   = testing(dat5_split)
# 
# m1 <- randomForest(
#   formula = as.factor(產業別) ~ .,
#   data    = dat5_train
# );m1  

#=====================#
#10萬筆的表現比50萬好，可是50萬蠻花時間的
smalldat5 = dat5[1:500000,] 
set.seed(123)
smalldat5_split = initial_split(data = smalldat5, prop = .7)
smalldat5_train = training(smalldat5_split)
smalldat5_test   = testing(smalldat5_split)
table(pred_randomForest, smalldat5_test$產業別)
m1 <- randomForest(
  formula = as.factor(產業別) ~ .,
  data    = smalldat5_train
);m1 

pred_randomForest <- predict(m1, smalldat5_test)
head(pred_randomForest)
table(pred_randomForest, smalldat5_test$產業別)

#================



