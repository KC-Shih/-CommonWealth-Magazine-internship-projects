setwd("D:/cw_intern/csv_for_R")
##################
# 員工人數
Q15_員工 = c(rep(2,31+54+33+31+47), 
           rep(3,15+29+25+31+41),
           rep(4,1+9+2+4+8),
           rep(5,2+1+1+1+1))

員工人數2019 = c(rep(1,31), rep(2,54),rep(3,33),rep(4,31),  #2
             rep(5,47),
             
             rep(1,15), rep(2,29),rep(3,25),rep(4,31),  #3
             rep(5,41),
             
             rep(1,1), rep(2,9),rep(3,2),rep(4,4),  #4
             rep(5,8),
             
             rep(1,2), rep(2,1),rep(3,1),rep(4,1),  #5
             rep(5,1)
             
)

Q15_員工人數 = cbind(Q15_員工,員工人數2019)
write.csv(Q15_員工人數, "Q15_員工人數2019.csv")


########
# 公司性質
Q15_公司 = c(rep(2,182+14), 
        rep(3,124+17),
        rep(4,21+3),
        rep(5,6+0))

公司性質 = c(rep(1,182), rep(2,14),
      
       rep(1,124), rep(2,17),
       
       rep(1,21), rep(2,3),
       
       rep(1,6), rep(2,0)
)
Q15_公司性質 = cbind(Q15_公司,公司性質)
write.csv(Q15_公司性質, "Q15_公司性質.csv")

##########
# 公司行業別


Q15_行業 = c(rep(2,46+19+55+33+24+19), 
        rep(3,31+11+37+30+16+16),
        rep(4,1+1+5+6+5+6),
        rep(5,1+0+3+0+2+0))

公司行業別 = c(rep(1,46), rep(2,19),rep(3,55),rep(4,33),  #2
       rep(5,24),rep(6,19),
       
       rep(1,31), rep(2,11),rep(3,37),rep(4,30),  #3
       rep(5,16),rep(6,16),
       
       rep(1,1), rep(2,1),rep(3,5),rep(4,6),  #4
       rep(5,5),rep(6,6),
       
       rep(1,1), rep(2,0),rep(3,3),rep(4,0),  #5
       rep(5,2),rep(6,0)
       )

Q15_公司行業別 = cbind(Q15_行業,公司行業別)
write.csv(Q15_公司行業別, "Q15_公司行業別.csv")


#####################

library(pheatmap)
pheatmap(QQ9.1[,1:10], display_numbers = T,
         colorRampPalette(c('white','red'))(40), 
         cluster_rows = F, cluster_cols = F, 
         fontsize_number = 15)
