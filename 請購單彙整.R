## =================================================== ##
## 日期：2020/12/23                                    ##
## 維護者：施愷哲                                      ##
## 讀入資料格式：excel                                 ##
## 所有檔案放在同一個資料夾(第10、16行更改檔案位置)    ##
## 彙整後資料名稱：請購單全年清單.xlsx                 ##
## =================================================== ##


library(readxl)
library(xlsx)
all_xls = list.files(path="D:/cw_intern/raw data/請款資料彙整",
                     pattern="\\.xls$", recursive = TRUE)

full_file = list()
needed_cell = list()
for(i in 1:length(all_xls)){
  setwd("D:/cw_intern/raw data/請款資料彙整")
  full_file[[i]] = read_excel(all_xls[i], col_names = F)
  needed_cell[[i]] = cbind(strsplit(as.character(full_file[[i]][2,4]), "：")[[1]][2], 
                           strsplit(as.character(full_file[[i]][2,5]), "：")[[1]][2],
                           as.character(full_file[[i]][29,5]),
                           as.character(full_file[[i]][29,8]))
  print(round(i/length(all_xls),3))
}

do.call(rbind, lapply(needed_cell, as.data.frame)) -> final_file
colnames(final_file) = c("請購日","受款人","請購用途","總金額")
write.xlsx(final_file, "請購單全年清單.xlsx")


