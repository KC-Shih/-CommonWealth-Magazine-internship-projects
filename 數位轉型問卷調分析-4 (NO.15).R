library(dplyr)                                       # for data manipulation
library(ggplot2)                                     # for visualization
library(scales)   
library(forcats)


# automatically determining breaks/labels 
# Create Pie chart
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(plotdata, aes(x = "", y = prop, fill = race)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  labs(title = "Marriage Participants by Race")







#########################
#########################
per = c(0.12,0.26,0.60,0.93)
perr = paste0(per*100,"%")
Q7 = c("超過50％"	,"25～50％","低於25％","完全沒有")  
label = c("a","a","b","b")
Q = cbind(Q7, per,label)
Q = as.data.frame(Q)


ggplot(Q, aes(fct_reorder(Q7,as.numeric(per)), per), fill=factor(label)) +
  ylim(0,1)+
  geom_bar(stat='identity')+#,fill="saddlebrown") +
  geom_text(aes(label=label), color="snow1", vjust=2,size=6 ) +
  geom_text(aes(label= perr), color="black", vjust=-0.5,size=5 ) +
  labs(x = "貴公司的核心經營團隊裡，有多少比例的人具備數位經營（營運）的經驗？"
       , y = "百分比"
       ,title= "貴公司正在進行的數位轉型對於營運績效是否有顯著影響？
               ANS：正在進行數位轉型，但仍未見到成效")+
  guides(fill=FALSE)

##########

per = c(0.25,0.28,0.42,0.63,0.52 )
perr = paste0(per*100,"%")
Q12大 = c("目前無發展此項目的需求","正在評估、計畫中",
         "選定特定範圍做試驗性推動","多數部門均推動一段時間", "已成為常規營運流程")  
label = c("a","a","a,b","b","a,b")
Q = cbind(Q12大, per,label)
Q = as.data.frame(Q)

ggplot(Q, aes(fct_reorder(Q12大,as.numeric(per)), per), fill=factor(label)) +
  geom_bar(stat='identity')+#,fill="saddlebrown") +
  geom_text(aes(label=label), color="snow1", vjust=2,size=6 ) +
  geom_text(aes(label= perr), color="black", vjust=-0.5,size=5 ) +
  
  labs(x = "貴公司在數位轉型(大數據分析)的進行程度？ "
       , y = "占整體比例"
       ,title= "貴公司正在進行的數位轉型對於營運績效是否有顯著影響？
       ANS：已有數位轉型成效，但未達成目標")+
  guides(fill=FALSE)

