setwd("D:\\cw_intern\\csv_for_R")
#########################
# 1
Q1 = read.csv("Q1.csv", header = T)
colnames(Q1) = c("數位轉型","總體策略")

Q1 = subset(Q1, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q1)
QQ1 = prop.table(table(Q1),2)


#####################
# 4
Q4 = read.csv("Q4.csv", header = T)
colnames(Q4) = c("數位轉型","總體策略")

Q4 = subset(Q4, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q4)
QQ4 = prop.table(table(Q4),2)

##################
#5
Q5 = read.csv("Q5.csv", header = T)
colnames(Q5) = c("數位轉型","主管")

Q5 = subset(Q5, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q5)
QQ5=prop.table(table(Q5),2)

##################
# 7
Q7 = read.csv("Q7.csv",header = T)
colnames(Q7) = c("數位轉型","多少人有數位經營的經驗")
Q7 = subset(Q7, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q7)
QQ7 = prop.table(table(Q7),2)


##################
# 7
Q7 = read.csv("Q7.csv",header = T)
colnames(Q7) = c("數位轉型","數位經營的經驗")
Q7 = subset(Q7, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q7)
QQ7 = prop.table(table(Q7),2)


Q7_2 = subset(Q7, 數位轉型==2)

##################
# 8
Q8 = read.csv("Q8.csv",header = T)
colnames(Q8) = c("數位轉型","專業職能模型")
Q8 = subset(Q8, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q8)
QQ8 = prop.table(table(Q8),2)



Q8 %>% 
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)


##################
# 9.1 難以用Table呈現
Q9.1 =  array(
  c(66,36,61,67,52,41,27,47,36,2,13,
    67,31,53,52,42,34,16,41,22,1,1,
    15,8,12,8,8,5,3,4,4,0,0,
    1,0,3,4,2,1,0,0,1,1,0), 
  dim = c(4,11),
  dimnames=list( 數位轉型 = c("正在進行數位轉型，但仍未見到成效",
                          "已有數位轉型成效，但未達成目標",
                          "達成數位轉型的成效目標",
                          "超越數位轉型的成效目標" ),
                 提供的數位技能 = c("大數據分析",	"人工智慧",
                              "數位行銷",	"社群經營",
                              "電子商務",	"客戶關係數位化",
                              "IoT物聯網",	"工業4.0與智慧製造",
                              "雲端服務",	"其他",	"無"))
)

QQ9.1 = prop.table(Q9.1,2)

Q15 = c(rep(2,66+36+61+67+52+41+27+47+36+2+13), 
        rep(3,67+31+53+52+42+34+16+41+22+1+1),
        rep(4,15+8+12+8+8+5+3+4+4+0+0),
        rep(5,1+0+3+4+2+1+0+0+1+1+0))

Q9 = c(rep(1,66), rep(2,36),rep(3,61),rep(4,67),  #2
       rep(5,52),rep(6,41),rep(7,27),rep(8,47),
       rep(9,36),rep(10,2),rep(11,13),
       
       rep(1,67), rep(2,31),rep(3,53),rep(4,52),  #3
       rep(5,42),rep(6,34),rep(7,16),rep(8,41),
       rep(9,22),rep(10,1),rep(11,1),
       
       rep(1,15), rep(2,8),rep(3,12),rep(4,8),  #4
       rep(5,8),rep(6,5),rep(7,3),rep(8,4),
       rep(9,4),rep(10,0),rep(11,0),
       
       rep(1,1), rep(2,0),rep(3,3),rep(4,4),  #5
       rep(5,2),rep(6,1),rep(7,0),rep(8,0),
       rep(9,1),rep(10,1),rep(11,0)
       
)

Q15_9 = cbind(Q15,Q9)
write.csv(Q15_9, "Q15_9.csv")


#################
# 10
Q10 = read.csv("Q10.csv",header = T)
colnames(Q10) = c("數位轉型","人數")
Q10 = subset(Q10, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q10)
QQ10 = prop.table(table(Q10),2)

######################
# 12大數據

Q12大數據 = read.csv("Q12大數據.csv",header = T)
colnames(Q12大數據) = c("數位轉型","大數據分析")
Q12大數據 = subset(Q12大數據, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12大數據)
QQ12大數據 = prop.table(table(Q12大數據),2)

######################
# 12人工智慧

Q12人工智慧 = read.csv("Q12人工智慧.csv",header = T)
colnames(Q12人工智慧) = c("數位轉型","人工智慧")
Q12人工智慧 = subset(Q12人工智慧, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12人工智慧)
QQ12人工智慧 = prop.table(table(Q12人工智慧),2)

######################
# 12數位行銷

Q12數位行銷 = read.csv("Q12數位行銷.csv",header = T)
colnames(Q12數位行銷) = c("數位轉型","數位行銷")
Q12數位行銷 = subset(Q12數位行銷, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12數位行銷)
QQ12數位行銷 = prop.table(table(Q12數位行銷),2)


######################
# 12社群經營

Q12社群經營 = read.csv("Q12社群經營.csv",header = T)
colnames(Q12社群經營) = c("數位轉型","社群經營")
Q12社群經營 = subset(Q12社群經營, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12社群經營)
QQ12社群經營 = prop.table(table(Q12社群經營),2)

######################
# 12電子商務

Q12電子商務 = read.csv("Q12電子商務.csv",header = T)
colnames(Q12電子商務) = c("數位轉型","電子商務")
Q12電子商務 = subset(Q12電子商務, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12電子商務)
QQ12電子商務 = prop.table(table(Q12電子商務),2)

######################
# 12客戶關係數位化

Q12客戶關係數位化 = read.csv("Q12客戶關係數位化.csv",header = T)
colnames(Q12客戶關係數位化) = c("數位轉型","客戶關係數位化")
Q12客戶關係數位化 = subset(Q12客戶關係數位化, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12客戶關係數位化)
QQ12客戶關係數位化 = prop.table(table(Q12客戶關係數位化),2)

######################
# 12IoT物聯網 

Q12IoT物聯網  = read.csv("Q12IoT物聯網.csv",header = T)
colnames(Q12IoT物聯網 ) = c("數位轉型","IoT物聯網 ")
Q12IoT物聯網  = subset(Q12IoT物聯網 , 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12IoT物聯網 )
QQ12IoT物聯網  = prop.table(table(Q12IoT物聯網 ),2)


######################
# 12工業4.0與智慧製造

Q12工業4.0與智慧製造  = read.csv("Q12工業4.0與智慧製造.csv",header = T)
colnames(Q12工業4.0與智慧製造) = c("數位轉型","工業4.0與智慧製造")
Q12工業4.0與智慧製造 = subset(Q12工業4.0與智慧製造 , 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12工業4.0與智慧製造)
QQ12工業4.0與智慧製造 = prop.table(table(Q12工業4.0與智慧製造),2)

######################
# 12雲端服務

Q12雲端服務 = read.csv("Q12雲端服務.csv",header = T)
colnames(Q12雲端服務) = c("數位轉型","雲端服務")
Q12雲端服務 = subset(Q12雲端服務, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q12雲端服務)
QQ12雲端服務 = prop.table(table(Q12雲端服務),2)

#####################
# 20

Q20 = read.csv("Q20.csv", header = T)
colnames(Q20) = c("數位轉型","covid-19")

Q20 = subset(Q20, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q20)
QQ20 = prop.table(table(Q20),2)

####################
# 10為分母
Q10 = read.csv("Q10.csv",header = T)
colnames(Q10) = c("數位轉型","人數")
Q10 = subset(Q10, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q10)
QQ10為分母 = prop.table(table(Q10),1)
QQ10為分母 = t(QQ10為分母)
####################
# 22為分母
Q22 = read.csv("Q22.csv",header = T)
colnames(Q22) = c("數位轉型","covid-19vs營收")
Q22 = subset(Q22, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q22)
QQ22 = prop.table(table(Q22),1)
QQ22 = t(QQ22)


####################
# 16為分母
Q16 = read.csv("Q16.csv",header = T)
colnames(Q16) = c("數位轉型","發展出新的商業模式")
Q16 = subset(Q16, 數位轉型==2|數位轉型==3|數位轉型==4|數位轉型==5)
table(Q16)
QQ16 = prop.table(table(Q16),1)
QQ16 = t(QQ16)

#####################
# 匯出百分比檔案
### 成功了！！
library("xlsx")
write.xlsx(QQ1, file = "rJ.xlsx",sheetName = "Sheet1",append=F)

write.xlsx(QQ4, file = "rJ.xlsx",sheetName = "Q4", append = T)

write.xlsx(QQ16, file = "rJ.xlsx",sheetName = "Q16", append = T)


####   ==匯出百分比檔案==  ####
setwd("D:\\cw_intern\\csv_for_R\\新交叉分析百分比")

NO.15 = c("正在進行數位轉型，但仍未見到成效", "已有數位轉型成效，但未達成目標", 
          "達成數位轉型的成效目標", "超越數位轉型的成效目標")

QQ1 = cbind(NO.15, QQ1)
write.csv(QQ1, "Q1_15.csv")

QQ4 = cbind(NO.15, QQ4)
write.csv(QQ4, "Q4_15.csv")

QQ5 = cbind(NO.15, QQ5)
write.csv(QQ5, "Q5_15.csv")

QQ7 = cbind(NO.15, QQ7)
write.csv(QQ7, "Q7_15.csv")

QQ8 = cbind(NO.15, QQ8)
write.csv(QQ8, "Q8_15.csv")

QQ10 = cbind(NO.15, QQ10)
write.csv(QQ10, "Q10_15.csv")

QQ12大數據 = cbind(NO.15, QQ12大數據)
write.csv(QQ12大數據, "Q12大數據_15.csv")

QQ12人工智慧 = cbind(NO.15, QQ12人工智慧)
write.csv(QQ12人工智慧, "Q12人工智慧_15.csv")

QQ12數位行銷 = cbind(NO.15, QQ12數位行銷)
write.csv(QQ12數位行銷, "Q12數位行銷_15.csv")

QQ12社群經營 = cbind(NO.15, QQ12社群經營)
write.csv(QQ12社群經營, "Q12社群經營_15.csv")

QQ12電子商務 = cbind(NO.15, QQ12電子商務)
write.csv(QQ12電子商務, "Q12電子商務_15.csv")

QQ12客戶關係數位化 = cbind(NO.15, QQ12客戶關係數位化)
write.csv(QQ12客戶關係數位化, "Q12客戶關係數位化_15.csv")

QQ12IoT物聯網 = cbind(NO.15, QQ12IoT物聯網 )
write.csv(QQ12IoT物聯網 , "Q12IoT物聯網 _15.csv")

QQ12工業4.0與智慧製造 = cbind(NO.15, QQ12工業4.0與智慧製造)
write.csv(QQ12工業4.0與智慧製造, "Q12工業4.0與智慧製造_15.csv")

QQ12雲端服務 = cbind(NO.15, QQ12雲端服務)
write.csv(QQ12雲端服務, "Q12雲端服務_15.csv")

QQ20 = cbind(NO.15, QQ20)
write.csv(QQ20, "Q20_15.csv")


QQ10為分母 = rbind(NO.15, QQ10為分母)
write.csv(QQ10為分母, "Q15_10.csv")

QQ22 = rbind(NO.15, QQ22)
write.csv(QQ22, "Q15_22.csv")

QQ16 = rbind(NO.15, QQ16)
write.csv(QQ16, "Q15_16.csv")


