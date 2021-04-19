setwd("D:/cw_intern/生產力優化專案/臉書廣告花費優化")

library(data.table)
library(dplyr)

# 已經把按讚留言分享併入，日期早的要在前面
df = fread("臉書貼文資料all.csv") %>% as.data.frame() 


without = c(1,2,5,6,8)
df = df[,-without]


paid = which(df$`Lifetime Post Paid Reach`!=0) 
paid_post = df[paid,]
# paid_post = paid_post[1:nrow(paid_post)-1,] # 如果把欄位解釋先拿掉，這行不執行
write.csv(paid_post, "paid_post.csv")


as.Date(df$已發佈[1],format = "%m/%d/%Y")
as.Date(paid_post$已發佈[1],format = "%m/%d/%Y")

diff = c()
for(i in 1:nrow(paid_post)){
  
  diff[i] = as.Date(paid_post$已發佈[i+1],format = "%m/%d/%Y")-
    as.Date(paid_post$已發佈[i],format = "%m/%d/%Y")
  
}
diff = diff[1:length(diff)-1] #扣掉最後一天沒辦法減的

first = as.Date(paid_post$已發佈[1],format = "%m/%d/%Y")- # 第一天到第一次廣告
  as.Date(df$已發佈[1],format = "%m/%d/%Y")

last = as.Date(df$已發佈[nrow(df)],format = "%m/%d/%Y") - 
  as.Date(paid_post$已發佈[nrow(paid_post)],format = "%m/%d/%Y")
  

diff = c(diff, last)


# 第一次下廣告前的日期，不用看
# print(as.Date(df$已發佈[1],format = "%m/%d/%Y") + 0:diff[1])

group = list()  # 分組的日期
for(i in 2:length(diff)){
  group[[i]] = as.Date(paid_post$已發佈[i],format = "%m/%d/%Y") + 0:diff[i-1]
  
}
group = group[-37]; group

group_date_length = diff+1


###===========================================================###



setwd("D:/cw_intern/生產力優化專案/臉書廣告花費優化")

all_Advertorial = read.csv("全部廣編企畫的貼文.csv")
paid_Advertorial = read.csv("有買廣告的廣編企劃貼文.csv")
df = all_Advertorial[,c(9,10,12,13,15)]
df2 = paid_Advertorial[,c(9,10,12,13,15)]
cor = cor(df)
cor2 = cor(df2)
##　total 跟 organic 的相關性超低，主要受受有/無廣告組的影響

summary(paid_Advertorial$Lifetime_Post_Total_Reach)
summary(paid_Advertorial$Lifetime_Post_Paid_Reach)
summary(paid_Advertorial$Lifetime_Post_organic_reach)

## 比較廣邊下廣告/不下廣告、非廣編下廣告/不下廣告的
## 總觸及整體中位數

