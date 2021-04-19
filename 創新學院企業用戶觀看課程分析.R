setwd("D:/cw_intern/raw data")
library(data.table)
library(dplyr)


dat = fread("data202001-202009.csv") %>% as.data.frame() #1574819 8
company = unique(dat$公司名稱) %>% as.data.frame() #117 家企業
colnames(company)= c("公司名稱")
dat2 = read.csv("industry_category_list.csv") # 209   2
addcompany = matrix(c("玉山銀行(玉山商業銀行)", "維鯨企業有限公司"
                      , "金融業","製造業"),2,2,byrow = F) %>% as.data.frame()
colnames(addcompany) =c("名稱","產業別")
dat2 =  rbind(dat2, addcompany)
aa = inner_join(company, dat2, by =c("公司名稱" = "名稱"))#115
anti_join(company, dat2, by =c("公司名稱" = "名稱")) #2
# 兩家公司沒有資料 (已加入)
#1 玉山銀行(玉山商業銀行)
#2 維鯨企業有限公司

dat3 = inner_join(dat, dat2, by =c("公司名稱" = "名稱"))# 合併公司產業別後資料
length(unique(dat3$單堂課名稱)) #1536
length(unique(dat3$課組名稱))   # 694

dat4 <- dat3 %>% mutate(new單堂課名稱 = paste0( dat3$單堂課ID,dat3$單堂課名稱))
#fwrite(dat4, "用戶看課分析(含產業別).csv")
#
# (1)按照產業別看單堂課次數

#######看單堂課前10名各產業佔幾組：課組在各產業前幾名重複性較高
####old
# dat3 %>%  
#   group_by(產業別,單堂課名稱) %>% 
#   summarise(單堂課看課次數 = n()) %>%
#   arrange(desc(單堂課看課次數))

dat4 %>%  
  group_by(產業別, 課組名稱, new單堂課名稱) %>% 
  summarise(單堂課看課次數 = n()) %>%
  arrange(desc(單堂課看課次數))
#########

# dat3 %>%  
#   group_by(產業別,單堂課名稱) %>% 
#   summarise(單堂課看課次數 = n()) %>%
#   arrange(desc(單堂課看課次數)) %>% 
#   arrange(desc(產業別)) 

# dat4 %>%  
#   group_by(產業別,new單堂課名稱) %>% 
#   summarise(單堂課看課次數 = n()) %>%
#   arrange(desc(單堂課看課次數)) %>% 
#   arrange(desc(產業別)) 

# (2)按照產業別看課組次數

## 課組前10名各產業佔幾組
dat3 %>% 
  group_by(課組名稱, 產業別) %>% 
  summarise(課組看課次數 = n()) %>% 
  arrange(desc(課組看課次數))

課組 <- dat3 %>% 
  group_by(產業別,課組名稱) %>% 
  summarise(課組看課次數 = n()) %>% 
  arrange(desc(課組看課次數)) %>% 
  arrange(desc(產業別)) 

# (3) 各公司總看課數(不分單堂課或是課組)(較無意義)
dat3 %>%  # aa
  group_by(產業別,公司名稱) %>% 
  summarise(公司看課次數 = n()) %>% 
  arrange(desc(公司看課次數, 產業別))

# (4) 各公司單堂課排名(較不具參考意義)
out <- dat4 %>%    #  97,112 x 3
  group_by(用戶ID,  公司名稱,單堂課ID, new單堂課名稱, 課組名稱) %>% 
  summarise(單堂課_公司看課次數 = n()) %>%
  arrange(desc(單堂課_公司看課次數))
write.csv(out, "前台公司單堂課看課數.csv")
## (4) 各公司課組排名 (從產業別觀察較具意義)
dat3 %>%  #  44,694 x 3
  group_by(課組名稱, 產業別, 公司名稱) %>% 
  summarise(課組_公司看課次數 = n()) %>%
  arrange(desc(課組_公司看課次數))

##############
school <-dat3 %>% 
  filter(公司名稱=="東吳大學"|
               公司名稱=="東吳大學圖書館"|
               公司名稱=="圖書館_廢棄帳號"|
               公司名稱=="政大EMBA"|
               公司名稱=="正修科技大學"|
               公司名稱=="中原大學") %>% 
  group_by(課組名稱,公司名稱) %>% 
  summarise(課組看課次數 = n()) %>% 
  arrange(desc(課組看課次數))

school_單堂課 <- dat4 %>% 
  filter(公司名稱=="東吳大學"|
               公司名稱=="東吳大學圖書館"|
               公司名稱=="圖書館_廢棄帳號"|
               公司名稱=="政大EMBA"|
               公司名稱=="正修科技大學"|
               公司名稱=="中原大學") %>% 
  group_by(課組名稱,公司名稱,new單堂課名稱) %>% 
  summarise(單堂課看課次數 = n()) %>% 
  arrange(desc(單堂課看課次數))


summary(dat3)
dat3[grepl(dat3$公司名稱, pattern = "廢棄"),] # 廢棄帳號仍有一定看課數
s = aa[grepl(aa$公司名稱, pattern = "廢棄帳號"),] # 廢棄帳號仍有一定看課數
summary(s)

## ========  串接資料 =======##

setwd("D:/cw_intern/raw data")
library(data.table)
library(dplyr)

dat6 = read.csv("串接_課程_產業別.csv")

dat7 <- dat6 %>%  ##單堂課
  select(客戶產業別, 主頻道,次頻道, 課組名稱, 上線日期,單堂課名稱,
              介接次數) %>% 
  group_by(客戶產業別, 課組名稱, 單堂課名稱,
                主頻道,次頻道, 上線日期, 介接次數) %>% 
  arrange(desc(介接次數))

aa = dat6 %>%  
  select(客戶產業別, 課組名稱, 單堂課名稱,
              主頻道,次頻道, 上線日期,介接次數) %>% 
  group_by(課組名稱) %>% 
  summarise(介接次數= sum(as.numeric(介接次數)))%>% 
  arrange(desc(介接次數))


