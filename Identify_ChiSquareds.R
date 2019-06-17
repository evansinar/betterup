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