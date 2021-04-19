setwd("D:/cw_intern/raw data")
library(dplyr)
library(data.table)

# load data
fo2019 = fread("已回收問卷_統計用檔案＿for創新.csv")
fo2019 = as.data.frame(fo2019)
fo2020 = fread("2020《天下XIMD》2000大企業數位轉型大調查_統計用檔案.csv")
fo2020 = as.data.frame(fo2020)

y19n20 = read.csv("y19n20.csv")
colnames(y19n20) = "23_企業統一編號"

y19y20 = read.csv("y19y20.csv")
colnames(y19y20) = "23_企業統一編號"

n19y20 =  read.csv("n19y20.csv")
colnames(n19y20) = "28_企業統一編號"

### select ###

# 2019年有填，2020年沒有填
y19n20s = inner_join(fo2019, y19n20, by = "23_企業統一編號") #[1] 409 111 (原424)
write.csv(y19n20R, "y19n20s.csv")

# 2019年有填，2020年也有填
y19y20s = inner_join(fo2019, y19y20, by = "23_企業統一編號") #[1] 468 111 (原477)
write.csv(y19y20R, "y19y20s.csv")

# 2019年沒有填，但是2020年有填
n19y20$`28_企業統一編號` = as.numeric(as.character(n19y20$`28_企業統一編號`))
fo2020$`28_企業統一編號` = as.numeric(fo2020$`28_企業統一編號`)
n19y20s = inner_join(fo2020, n19y20, by = "28_企業統一編號") #[1] 263 145 (原267)
write.csv(n19y20R, "n19y20s.csv")


###############
library(jtools)
library(ggplot2)
setwd("D:/cw_intern/csv_for_R")
Q3_1 = read.csv("Q3_1新商業模式test.csv")
colnames(Q3_1) = c("tax ID number", "Q3.1", "NO.15", "年分", "營收成長率")
table(Q3_1$NO.15)



# 不服從常態分布
# 因為有負值無法進行對數轉換
# fit一條線性模型並不合理 !!


shapiro.test(Q3_1$營收成長率) # 小於0.05不服從常態分布
kruskal.test(Q3_1$營收成長率,factor(as.character(Q3_1$NO.15))) # 大於0.05，表示組間無顯著差異
  

ggplot(Q3_1, aes(x = Q3_1$NO.15, y = Q3_1$營收成長率, fill = factor(Q3_1$年分), colour = as.character(Q3_1$NO.15)))+
  geom_boxplot(width=.5)+
  labs(x = "第15題選擇", 
       y = "營收成長率(%)", 
       title="第3題選擇[發展新商業模式]")+
  theme(text=element_text(size=10))+
  scale_colour_manual(name = "different selects")

  
##############################


Q3_2 = read.csv("Q3_2成本降低.csv")
Q3_2 = na.omit(Q3_2)
colnames(Q3_2) = c("company name","tax ID number", "Q3.2", "NO.15", 
                   "2019獲利率(%)","2018獲利率(%)", "獲利率差異(%)" )
table(Q3_2$NO.15)
shapiro.test(Q3_2$`獲利率差異(%)`) # 不服從常態分布
kruskal.test(Q3_2$`獲利率差異(%)`,factor(as.character(Q3_2$NO.15))) # 組間無顯著差異

ggplot(Q3_2, aes(x = factor(Q3_2$NO.15), y = Q3_2$`獲利率差異(%)`, colour = as.character(Q3_2$NO.15)))+
  geom_boxplot(width=.5)+
  labs(x = "第15題選擇", 
       y = "獲利率差異(%)", 
       title="第3題選擇[成本降低]",
       subtitle = "獲利率差異(%) = 2019獲利率(%)-2018獲利率(%)")+
  scale_colour_discrete(name = "different selects")+ 
  theme(text=element_text(size=10,  family="wqy-microhei"))#+
  #scale_y_continuous(breaks=seq(-20, 70, 5)) 

############################

Q3_3 = read.csv("Q3_3提高效率.csv")
Q3_3 = na.omit(Q3_3)
colnames(Q3_3) = c("company name","tax ID number", "Q3.3", "NO.15", 
                   "2019獲利率(%)","2018獲利率(%)", "獲利率差異(%)" )
table(Q3_3$NO.15)
shapiro.test(Q3_3$`獲利率差異(%)`) # 不服從常態分布
kruskal.test(Q3_3$`獲利率差異(%)`,factor(as.character(Q3_3$NO.15))) # 組間無顯著差異

ggplot(Q3_3, aes(x = factor(Q3_3$NO.15), y = Q3_3$`獲利率差異(%)`, colour = as.character(Q3_3$NO.15)))+
  geom_boxplot(width=.5)+
  labs(x = "第15題選擇", 
       y = "獲利率差異(%)", 
       title="第3題選擇[提高效率]",
       subtitle = "獲利率差異(%) = 2019獲利率(%)-2018獲利率(%)")+
  scale_colour_discrete(name = "different selects")+ 
  theme(text=element_text(size=10,  family="wqy-microhei"))#+
#scale_y_continuous(breaks=seq(-20, 70, 5)) 

  
############################

Q3_4 = read.csv("Q3_4開拓新市場與客戶.csv")
Q3_4 = na.omit(Q3_4)
colnames(Q3_4) = c("company name","tax ID number", "Q3.4", "NO.15", 
                   "營收成長率" )
table(Q3_4$NO.15)
shapiro.test(Q3_4$營收成長率) # 不服從常態分布
kruskal.test(Q3_4$營收成長率,factor(as.character(Q3_4$NO.15))) # 組間無顯著差異 # p-value =  0.38

ggplot(Q3_4, aes(x = Q3_4$NO.15, y = Q3_4$營收成長率, colour = as.character(Q3_4$NO.15)))+
  geom_boxplot(width=.5)+
  labs(x = "第15題選擇", 
       y = "獲利率差異(%)", 
       title="第3題選擇[開拓新市場與客戶]")+
  scale_colour_discrete(name = "different selects")+ 
  theme(text=element_text(size=10,  family="wqy-microhei"))#+
#scale_y_continuous(breaks=seq(-20, 70, 5)) 

###### 增加2018年資料 #####
Q3_4 = read.csv("Q3_4開拓新市場與客戶2018_2019.csv")
Q3_4 = na.omit(Q3_4)
colnames(Q3_4) = c("company name","tax ID number", "Q3.4", "NO.15", "年分",
                   "營收成長率" )

ggplot(Q3_4, aes(x = Q3_4$NO.15, y = Q3_4$營收成長率,fill = factor(Q3_4$年分), colour = as.character(Q3_4$NO.15)))+
  geom_boxplot(width=.5)+
  labs(x = "第15題選擇", 
       y = "營收成長率(%)", 
       title="第3題選擇[開拓新市場與客戶]")+
  scale_colour_discrete(name = "different selects")+ 
  theme(text=element_text(size=10))#+
#scale_y_continuous(breaks=seq(-20, 70, 5)) 

###############
Q3_5 = read.csv("Q3_5增加利潤.csv")
colnames(Q3_5) = c("company name","tax ID number", "Q3.5", "NO.15", 
                   "2019獲利率(%)","2018獲利率(%)", "獲利率差異(%)" )
Q3_5 = Q3_5[-29,] # 去除 大將開發 
table(Q3_5$NO.15)
shapiro.test(Q3_5$獲益率差異.) # 不服從常態分布
kruskal.test(Q3_5$獲益率差異.,factor(as.character(Q3_5$NO.15))) # 組間無顯著差異

ggplot(Q3_5, aes(x = factor(Q3_5$NO.15), y = Q3_5$獲益率差異., colour = as.character(Q3_5$NO.15)))+
  geom_boxplot(width=.5)+
  labs(x = "第15題選擇", 
       y = "獲利率差異(%)", 
       title="第3題選擇[增加利潤]",
       subtitle = "獲利率差異(%) = 2019獲利率(%)-2018獲利率(%)")+
  scale_colour_discrete(name = "different selects")+ 
  theme(text=element_text(size=10))#+
#scale_y_continuous(breaks=seq(-20, 70, 5)) 


####################################3
library(ggmosaic)
crossplot = read.csv("crossplot3_15.csv", header = T)
#crossplot = as.matrix(crossplot)
ggplot(data = crossplot) +
  geom_mosaic(aes(weight = 占比,  x = product(轉型目的),
                  fill = 轉型成效) + labs(x = "nn", title='aa'))

aaa = read.csv("aaa.csv", header = T)
aaa = as.data.frame(aaa)


df = array(
  c(5,8,13,44,0,0, 
    42,47,82,39,15,1, 
    43,29,58,31,15,0,
    7,7,9,8,0,0,
    2,1,2,3,0,0), 
  dim = c(6,5),
  dimnames=list( 轉型目的 = c("發展新商業模式","成本降低","提高效率"
                          ,"開拓新市場與客戶","增加利潤", "其他"),
                轉型成效 = c("目前未進行數位轉型", 
                              "正在進行數位轉型，但仍未見到成效",
                              "已有數位轉型成效，但未達成目標",
                              "達成數位轉型的成效目標",
                              "超越數位轉型的成效目標"))
)

