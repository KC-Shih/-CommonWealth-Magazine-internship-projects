######
# 1*7  第10頁
dat1_7 = matrix(c(15,	49,	192,	11,
               5,	39,	345,	114),nrow = 2, ncol = 4, byrow = T)
chisq.test(dat1_7)

# 1*10 第11頁

dat1_10 = matrix(c(186,	73,	6,	4,	1,
                   421,	60,	11,	2,	1),nrow = 2, ncol = 5, byrow = T)          
chisq.test(dat1_10)
fisher.test(dat1_10)

#1*15  第12頁
dat1_15 = matrix(c(21,	130,	97,	18,	4,
                   385,	66,	44,	6,	2),nrow = 2, ncol = 5, byrow = T)          
chisq.test(dat1_15)
fisher.test(dat1_15,simulate.p.value=TRUE)

#1*20  第13頁 
dat1_20 = matrix(c(46,	114,	69,	28,	11,
                   17,	113,	309, 39, 17),nrow = 2, ncol = 5, byrow = T)          
chisq.test(dat1_20)

#1*23教育訓練 第14頁 
dat1_23教 = matrix(c(98,	133,	33,
                    58,	197,	209),nrow = 2, ncol = 3, byrow = T)          
chisq.test(dat1_23教)


#4*16  P20

dat4_16 = matrix(c(7,	13,	13,	10,	7,	20,	0,	14,	4,
                   6,	11,	7,	19,	23,	35,	2,	24,	4),nrow = 2, ncol = 9, byrow = T)          
chisq.test(dat4_16)
fisher.test(dat4_16,simulate.p.value=TRUE)

#4*22  P21

dat4_22 = matrix(c(1,	0,0,	4,	0,	1,	0,	1,	0,
                   1,	5,	4,	2,	4,	9,	1,	4,	1,
                   1,	3,	2,	3,	3,	4,	0,	2,	2,
                   9,	10,	12,	12,	17,	27,	1,	19,	4,
                   5,	9,	3,	10,	10,	16,	0,	13,	1),nrow = 5, ncol = 9, byrow = T)          
chisq.test(dat4_22)
fisher.test(dat4_22,simulate.p.value=TRUE)

# P.27  2020年數位轉型的投資金額較2019年的變化幅度？

datP27 = matrix(c(46,	6,	12,	1,	15,	9,	5,	10,
                   53,	6,	19,	3,	13,	12,	7,	5,
                   57,	6,	15,	2,	17,	11,	10,	8,
                   60,	7,	16,	7,	18,	10,	7,	9,
                   1,	0,	0,	1,	0,	0,	0,	0,
                   1,	1,	0,	0,	0,	1,	0,	1,
                   1,	0,	0,	0,	0,	1,	0,	0),nrow = 7, ncol = 8, byrow = T)          
chisq.test(datP27)
fisher.test(datP27,simulate.p.value=TRUE)

####加總下面持平跟減少
datP27sum = matrix(c(46,	6,	12,	1,	15,	9,	5,	10,
                  53,	6,	19,	3,	13,	12,	7,	5,
                  57,	6,	15,	2,	17,	11,	10,	8,
                  63,	8,	16,	8,	18,	12,	7,	10),nrow = 4, ncol = 8, byrow = T)          
chisq.test(datP27sum)
fisher.test(datP27sum,simulate.p.value=TRUE)


## 7*15  P30

dat7_15 = matrix(c(3,	2,	11,	3,	1,
                   26,	16,	36,	6,	3,
                   263,	164,	93,	15,	1,
                   111,	13,	1,	0,	0),nrow = 4, ncol = 5, byrow = T)          
chisq.test(dat7_15)
fisher.test(dat7_15,simulate.p.value=TRUE)


## 8*15 p38

dat8_15 = matrix(c(87,	99,	106,	20,	4,
                   316,	94,	35,	4,	2),nrow = 2, ncol = 5, byrow = T)          
chisq.test(dat8_15)
fisher.test(dat8_15,simulate.p.value=TRUE)

## 8*16 p39

dat8_16 = matrix(c(112,116,
                   28,105),nrow = 2, ncol = 2, byrow = T)          
chisq.test(dat8_16)
fisher.test(dat8_16)#,simulate.p.value=TRUE)

## 15*10 P79

dat15_10 = matrix(c(353,	40,	6,	0,	0,
                    158,	32,	2	,2,	0,
                    82,	49,	5,	2,	2,
                    11,	7,	4,	2,	0,
                    2,	4,	0,	0,	0),nrow = 5, ncol = 5, byrow = T)          
chisq.test(dat15_10)
fisher.test(dat15_10,simulate.p.value=TRUE)

### 8*9.1 P34
dat8_9.1 = matrix(c(128,	74,	113,	103,	108,	64,	36,	66,	60,	2	,11,
                    74,	26,	75,	107,	83,	59,	26,	72,	49,	2,	172),nrow = 2, ncol = 11, byrow = T)          
chisq.test(dat8_9.1)
fisher.test(dat8_9.1,simulate.p.value=TRUE)

### 8*9.2 P35
dat8_9.2 = matrix(c(104,	59,	87,	61,	64,	33,	34,	72,	49,	8,	67,
                    48,	25,	39,	40,	38,	26,	18,	62,	27,	2,	273),nrow = 2, ncol = 11, byrow = T)          
chisq.test(dat8_9.2)
fisher.test(dat8_9.2,simulate.p.value=TRUE)

### 8*9.3 P36
dat8_9.3 = matrix(c(173,	115,	107,	49,	77,	62,	58,	98,	64,	3,	6,
                    170,	71,	100,	51,	88,	98,	51,	151,	60,	1,	92),nrow = 2, ncol = 11, byrow = T)          
chisq.test(dat8_9.3)
fisher.test(dat8_9.3,simulate.p.value=TRUE)

### 8*13 P37
dat8_13 = matrix(c(182,	94,	107,	57,	67,	85,	63,	96,	64	,1	,6,
                   217,	96,	107,	55,	88,	105,	52,	156,	68,	0,	53),nrow = 2, ncol = 11, byrow = T)          
chisq.test(dat8_13)
fisher.test(dat8_13,simulate.p.value=TRUE)



### 22*8 P122

dat22_8 = matrix(c(15,	6,
                   45,	50,
                   33,	49,
                   142,	230,
                   80,	112),nrow = 5, ncol = 2, byrow = T)          
chisq.test(dat22_8)
#fisher.test(dat22_8,simulate.p.value=TRUE)  no use


#####################################################
##############20201007白皮書最後補分析###############
#####################################################

dat1_20 = matrix(c(46,	114,	69,	28,	11,
                   17,	113,	309,	39,	17),nrow = 2, ncol = 5, byrow = T)          
chisq.test(dat1_20)

dat7_22 = matrix(c(4,	7,	1	,4	,3,
                   1,	16,	10,	37,	22,
                   16,	58,	51,	274,	135,
                   0,	14,	20,	57,	33), nrow = 4, ncol = 5, byrow = T)

chisq.test(dat7_22) #not suitable
fisher.test(dat7_22,simulate.p.value=TRUE)  #  p-value = 0.0004998

dat6_22 = matrix(c(2,	14, 0,	22,	14,
                   1,	9,	4,	26,	19,
                   2,	3,	8,  36,	13,
                   3,	7,	8,	25,	22,
                   0,	0,	0,	1,	0,
                   0,	0,	0,	1,	0,
                   0,	0,	0,	0,	1), nrow = 7, ncol = 5, byrow = T)

chisq.test(dat6_22) #not suitable
fisher.test(dat6_22,simulate.p.value=TRUE)  #  p-value = 0.02499

dat6_22r = matrix(c(2,	14, 0,	22,	14,
                   1,	9,	4,	26,	19,
                   2,	3,	8,  36,	13,
                   3,	7,	8,	25,	22),  nrow = 4, ncol = 5, byrow = T)

chisq.test(dat6_22r)
fisher.test(dat6_22r ,simulate.p.value=TRUE)   #  p-value = 0.01249 


dat10_22 = matrix(c(7,	67,	69,	308,	151,
                    9,	20,	10,	57,	35,
                    3,	2,	3,	3,	5,
                    0,	3,	0,	2,	1,
                    0,  1,  0,	1,	0), nrow = 5, ncol = 5, byrow = T)

chisq.test(dat10_22) #not suitable
fisher.test(dat10_22,simulate.p.value=TRUE)  #  p-value = 0.0004998

dat11_22 = matrix(c(4,	8,   3, 	7,	6,
                    10,	39,	22,	135,	73,
                    6,	34,	34,	152,	67,
                    1,	12,	22,	75,	43), nrow = 4, ncol = 5, byrow = T)

chisq.test(dat11_22) #not suitable
fisher.test(dat11_22,simulate.p.value=TRUE)  #  p-value = 0.0004998
## 會顯著是因為"以大數據分析為主，經驗為輔"這項的比例跟其他差太多，若去除則不顯住

dat15_22 = matrix(c(11,	38,	50,	209,	95,
                    3,	20,	17,	94,	60,
                    5,	28,	10,	64,	33,
                    1,	8,	3,	6,	5,
                    1,	1,	1,	3,	0), nrow = 5, ncol = 5, byrow = T)

chisq.test(dat15_22) #not suitable
fisher.test(dat15_22,simulate.p.value=TRUE)  #  p-value = 0.002499

dat22_8 = matrix(c(15,	6,
                   45,	50,
                   33,	49,
                   142,	230,
                   80,	112), nrow = 5, ncol = 2, byrow = T)

chisq.test(dat22_8) #弱顯著



dat5_16 = matrix(c(13,	22,
                   52,	62,
                   7,	10,
                   5,	12,
                   2,	3,
                   6,	8,
                   2,	0,
                   8,	16), nrow = 8, ncol = 2, byrow = T)

chisq.test(dat5_16) #not suitable
fisher.test(dat5_16,simulate.p.value=TRUE)  #  p-value = 0.6322 

dat16_10 = matrix(c(72,	53,	8,	5,	2,
                    178,	39,	2,	1,	0), nrow = 2, ncol = 5, byrow = T)

chisq.test(dat16_10) #not suitable
fisher.test(dat16_10,simulate.p.value=TRUE)  #  p-value = 0.0004998

dat10_16 = matrix(c(72,	178,
                    53,	39,
                    8,	2,
                    5,	1,
                    2,	0), nrow = 5, ncol = 2, byrow = T)

chisq.test(dat10_16) #not suitable
fisher.test(dat10_16,simulate.p.value=TRUE)  #  p-value = 0.0004998

rownames(dat10_16) = c("10 %以下","11~30%","31~50%","51~70%","70 %以上")
colnames(dat10_16) = c("是", "否")

library(rcompanion)
a10_16t = pairwiseNominalIndependence(dat10_16,fisher = TRUE,gtest  = FALSE,
                            chisq  = FALSE, digits = 3)

cldList(comparison = a10_16t$Comparison,
        p.value    = a10_16t$p.adj.Fisher,
        threshold  = 0.05)                            



#############
dat8_16 = matrix(c(112,	116,
                   28,	105), nrow = 2, ncol = 2, byrow = T)

chisq.test(dat8_16) #p-value = 2.366e-07 顯著

####################
# 用15題新資料的1014
dat5_15 = matrix(c(1,	19,	3,	0,
                   53,	52,	7,	3,
                   8,	6, 2,	0,
                   12,	5,	1,	0,
                   2,	1,	1,	1,
                   9,	5,	0,	0,
                   0,	2,	0,	0,
                   17,	4,	3,	0), nrow = 8, ncol = 4, byrow = T)

chisq.test(dat5_15) #not suitable
fisher.test(dat5_15,simulate.p.value=TRUE)  #  p-value = 0.0004998
## 有顯著差異

dat15_10 = matrix(c(158,	32,	2,	2,	0,
                    82,	49,	5,	2,	2,
                    11,	7,	4,	2,	0,
                    2,	4,	0,	0,	0), nrow = 4, ncol = 5, byrow = T)

chisq.test(dat15_10) #not suitable
fisher.test(dat15_10,simulate.p.value=TRUE)  #  p-value = 0.0004998
## 有顯著差異


dat15_16 = matrix(c(40,	154,
                    81,	58,
                    15,	7,
                    3,	3), nrow = 4, ncol = 2, byrow = T)

chisq.test(dat15_16) #not suitable
fisher.test(dat15_16,simulate.p.value=TRUE)  #  p-value = 0.0004998
## 有顯著差異

## 4_15沒有顯著差異                      
dat4_15 = matrix(c(5,	5,	3,	0,
                   11,	7,	4,	1,
                   8,	11,	0,	1,
                   18,	10,	1,	0,
                   13,	15,	2,	1,
                   34,	17,	2,	1,
                   1,	1,	0,	0,
                   19,	17,	3,	0,
                   2,	4, 2,	0), nrow = 9, ncol = 4, byrow = T)                            
rownames(dat4_15) = c('行政部門',
                      '業務部門',
                      '行銷部門',
                      '研發／產品開發部門',
                      '製造部門',
                      'IT部門',
                      '品管部門',
                      '成立專責小組或新部門',
                      '其他')                            
colnames(dat4_15) = c("正在進行數位轉型，但仍未見到成效",	"已有數位轉型成效，但未達成目標",
                      "達成數位轉型的成效目標",	"超越數位轉型的成效目標")

chisq.test(dat4_15) #not suitable
fisher.test(dat4_15,simulate.p.value=TRUE)  #  p-value = 0.2034
# a4_15t = pairwiseNominalIndependence(dat4_15,fisher = TRUE,gtest  = FALSE,
#                             chisq  = FALSE, digits = 3)
# 
# cldList(comparison = a4_15t$Comparison,
#         p.value    = a4_15t$p.adj.Fisher,
#         threshold  = 0.05)
dat10_15 = matrix(c(158,	82,	11,	2,
                    32,	49,	7,	4,
                    2,	5,	4,	0,
                    2,	2,	2,	0,
                    0,	2,	0,	0), nrow = 5, ncol = 4, byrow = T)

chisq.test(dat10_15) #not suitable
fisher.test(dat10_15,simulate.p.value=TRUE)  #  p-value = 0.0004998

rownames(dat10_15) = c("10%以下","11~30%","31~50%","51~70%","70%以上")
colnames(dat10_15) = c("正在進行數位轉型，但仍未見到成效",	"已有數位轉型成效，但未達成目標",
                      "達成數位轉型的成效目標",	"超越數位轉型的成效目標")
a10_15t = pairwiseNominalIndependence(dat10_15,fisher = TRUE,gtest  = FALSE,
                  chisq  = FALSE, digits = 3)
cldList(comparison = a10_15t$Comparison,
        p.value    = a10_15t$p.adj.Fisher,
        threshold  = 0.05)

##

dat8_15 = matrix(c(99,	106,	20,	4,
                    94,	35,	4,	2), nrow = 2, ncol = 4, byrow = T)

chisq.test(dat8_15) #not suitable
fisher.test(dat8_15,simulate.p.value=TRUE)  #  p-value = 0.0004998

colnames(dat8_15) = c("正在進行數位轉型，但仍未見到成效",	"已有數位轉型成效，但未達成目標",
                       "達成數位轉型的成效目標",	"超越數位轉型的成效目標")
rownames(dat8_15) = c("是", "否")
a8_15t = pairwiseNominalIndependence(dat8_15,fisher = TRUE,gtest  = FALSE,
                                      chisq  = FALSE, digits = 3)
cldList(comparison = a8_15t$Comparison,
        p.value    = a8_15t$p.adj.Fisher,
        threshold  = 0.05)
