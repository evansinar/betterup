###TUKEY COMPARISION WITH LETTERS TO SHOW DIFFERENCES
###REFERENCE https://stackoverflow.com/questions/52533259/wrong-tukey-letter-ordering-in-r-multcompview-package

library(multcompView)

tukey <- TukeyHSD(aov(WPM20_EmpExpIndex ~ Q44))
Tukey.levels <- tukey$Q44[,4]
multcompLetters(Tukey.levels)
