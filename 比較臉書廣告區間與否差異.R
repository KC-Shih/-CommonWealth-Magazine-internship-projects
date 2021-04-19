setwd("D:/cw_intern/生產力優化專案/臉書廣告花費優化")

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

df =  fread("比較廣告區間與否差異(已整理).csv") %>% as.data.frame() 
df = df[,c(1,5,8,9,11)]


ggplot(df, aes(y=`自然觸及率`))+
  geom_histogram(bins=50)+facet_wrap(~廣告區間,ncol=5)

boxplot(df$互動率~df$廣告區間, col=rgb(0.2,0.8,0.5,0.5) , border=T , main="",outline=FALSE)
boxplot(df$互動率~df$廣告區間, col=rgb(0.2,0.8,0.5,0.5) , border=T , main="",outline=FALSE)



ggplot(df, aes(y = `自然觸及率` ))+
  geom_bar()
hist(df$自然觸及率 , 
     breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="")

hist(log(df$自然觸及率[df$廣告區間==0]), main="非廣告區間",
     breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F,
     xlim = c(-8,0))

hist(log(df$自然觸及率[df$廣告區間==1]) , main="廣告區間",
     breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F,
     xlim = c(-8,0))


summary(df$自然觸及率[df$廣告區間==0])  # 非廣告區間
summary(df$自然觸及率[df$廣告區間==1])  # 廣告區間

# 平均值沒有顯著差異 t = 1.0916, df = 9835.1, p-value = 0.275
t.test(df$自然觸及率[df$廣告區間==0],df$自然觸及率[df$廣告區間==1])
t.test(df$互動率[df$廣告區間==0],df$互動率[df$廣告區間==1])


# wrong
#ggplot(df, aes(x = `類型`,y=`自然觸及率` ,fill = as.factor(`廣告區間`)))+
#  geom_bar(stat="identity", position=position_dodge())

library(lattice) 
histogram(x=~`自然觸及率`|as.factor(`廣告區間`),data = df)
histogram(x=~`互動率`|as.factor(`廣告區間`),data = df)



# MANOVA
y = cbind(df$`Lifetime Post organic reach`,df$自然觸及率,df$互動率)

mod1 = manova(y~as.factor(df$廣告區間), data=df)

library(HoRM)

# 全都顯著
summary(mod1, test="Wilks")
summary(mod1, test="Roy")
summary(mod1, test="Pillai")
summary(mod1, test="Hotelling-Lawley")




