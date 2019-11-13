###REGRESSION COMPARING TWO MODELS
###REFERENCE https://www.statmethods.net/stats/regression.html

EEI_Reg1 <- lm(Productivity ~ WPM20_Engagement_Comp)
EEI_Reg2 <- lm(Productivity ~ WPM20_Engagement_Comp + WPM20_EmpExpIndex_NoEng)
anova(EEI_Reg1, EEI_Reg2)
summary(EEI_Reg1)
summary(EEI_Reg2)
