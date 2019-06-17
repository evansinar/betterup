library(qualtRics)
library(DataExplorer)
library(dplyr)
library(sjPlot)
library(likert)
library(summarytools)
library(multicon)
library(gplots)
library(corrplot)
library(stats)
library(lemon)
library(readr)
library(corrr)

raw_categoric <- readSurvey("datafiles/BetterUp_Qual1998-0311FTE_June 13, 2019_11.54 Cat.csv")

categoric_data <- select(raw_categoric, ResponseId,	Q45,	Q46,	Q41,	Q1,	Q2,	Q3,	Q22,	
                         Q23_NPS_GROUP,	Q24_NPS_GROUP,	Q26,	Q27,	Q28,	Q29,	Q30,	Q31,	Q32,	
                         Q44,	Q33,	Q34,	Q35,	Q39, Q40)

raw_numeric <- readSurvey("datafiles/BetterUp_Qual1998-0311FTE_June 13, 2019_11.54 Num.csv")

numeric_data <- select(raw_numeric, ResponseId,	Q4_1,	Q4_2,	Q4_3,	Q4_4,	Q4_5,	Q4_6,	Q4_7,	Q4_8,	Q4_9,	Q4_10,	
                       Q4_11,	Q4_12, Q5_1,	Q5_2,	Q5_3,	Q5_4,	Q5_5,	Q5_6,	Q5_7,	Q5_8,	Q5_9,	Q5_10,	
                       Q5_11,	Q5_12, Q6_1,	Q6_2,	Q6_3,	Q6_4,	Q6_5,	Q6_6,	Q6_7,	Q6_8,	Q6_9,	Q6_10,	
                       Q6_11,	Q6_12, Q7_1,	Q7_2,	Q7_3,	Q7_4,	Q7_5,	Q7_6,	Q7_7,	Q7_8,	Q7_9,	Q7_10,	
                       Q7_11,	Q7_12, Q8_1,	Q8_2,	Q8_3,	Q8_4,	Q8_5,	Q8_6,	Q8_7,	Q8_8,	Q8_9,	Q8_10,	
                       Q8_11,	Q8_12, Q9_1,	Q9_2,	Q9_3,	Q9_4,	Q9_5,	Q9_6,	Q9_7,	Q9_8,	Q9_9,	Q9_10,	
                       Q10_1,	Q10_2, Q10_3,	Q10_4,	Q10_5,	Q10_6,	Q10_7,	Q10_8,	Q10_9,	
                       Q10_10,	Q11_1,	Q11_2,	Q11_3,	Q11_4,	Q11_5,	Q11_6,	Q11_7,	Q11_8,	
                       Q11_9,	Q11_10,	Q11_11,	Q11_12,	Q11_13,	Q11_14,	Q12_1,	Q12_2,	Q12_3,	
                       Q12_4,	Q12_5,	Q12_6,	Q12_7,	Q12_8,	Q12_9,	Q12_10,	Q14_1,	Q14_2,	
                       Q14_3,	Q14_4,	Q14_5,	Q14_6,	Q14_7,	Q14_8,	Q14_9,	Q14_10,	Q14_11,	
                       Q14_12,	Q14_13,	Q14_14,	Q16_1,	Q16_2,	Q16_3,	Q16_4,	Q16_5,		
                       Q16_7,	Q16_8,	Q16_9,	Q16_10,	Q16_11,	Q16_12,	Q16_13,	Q16_14,	Q16_15, Q17, Q18,	
                       Q20,	Q23,	Q24,	Q25,	Q33,	Q36,	Q38,	Q42, gc)

#summary(numeric_data)
#view(dfSummary(numeric_data))
#view(descr(numeric_data, stats = "common", transpose = TRUE, headings = FALSE))

reverse_cols = c('Q7_8', 'Q8_7', 'Q5_2', 'Q6_6', 'Q16_5', 'Q16_1', 'Q16_10', 'Q16_13', 'Q5_8', 'Q7_2', 'Q8_5', 'Q9_5')

numeric_data[ ,reverse_cols] = 6 - numeric_data[ ,reverse_cols]

benchmarking_merged <- left_join(categoric_data,numeric_data,by="ResponseId")

Q32_Factored <- factor(Q32)
levels(Q32_Factored) <- list(NoInterest=c("0/no interest in this"), 
                             FewHours=c("1-3 hours a month", "4-6 hours a month", "7-9 hours a month"),
                             ManyHours=c("10-12 hours a month", "13-15 hours a month", "More than 15 hours a month"))

plot_bar(Q32_Factored)
plot_bar(Q32)

categorical_comp <- function(var1, var2) {
  xtabs(~var1+var2,data=benchmarking_merged) 
  xtabs_table <- xtabs(~var1+var2,data=benchmarking_merged) 
  
  chisq <- chisq.test(xtabs_table) 
  print(chisq)
  corrplot(chisq$residuals, is.cor = FALSE)
  
  contrib <- 100*chisq$residuals^2/chisq$statistic
  round(contrib, 3)
  corrplot(contrib, is.cor = FALSE)
}

pdf("categorical.pdf") 

categorical_comp(Q32_Factored,Q46)
categorical_comp(Q32_Factored,Q41)
categorical_comp(Q32_Factored,Q1)
categorical_comp(Q32_Factored,Q2)
categorical_comp(Q32_Factored,Q3)
categorical_comp(Q32_Factored,Q23_NPS_GROUP)
categorical_comp(Q32_Factored,Q24_NPS_GROUP)
categorical_comp(Q32_Factored,Q26)
categorical_comp(Q32_Factored,Q27)
categorical_comp(Q32_Factored,Q28)
categorical_comp(Q32_Factored,Q29)
categorical_comp(Q32_Factored,Q30)
categorical_comp(Q32_Factored,Q31)
categorical_comp(Q32_Factored,Q44)
categorical_comp(Q32_Factored,Q35)
categorical_comp(Q32_Factored,Q40)

dev.off() 