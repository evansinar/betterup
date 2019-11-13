#BetterUp_Qual1998-0311FTE_May+18,+2019_08.12

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
library(Rfast)

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

reverse_cols = c('Q7_8', 'Q8_7', 'Q5_2', 'Q6_6', 'Q16_5', 'Q16_1', 'Q16_10', 'Q16_13', 'Q5_8', 'Q7_2', 'Q8_5', 'Q9_5')

numeric_data[ ,reverse_cols] = 6 - numeric_data[ ,reverse_cols]

Q32_Numeric <- factor(Q32)
Q32_Numeric <- recode_factor(Q32_Numeric, "0/no interest in this" = '1',
                             "1-3 hours a month" = '2', 
                             "4-6 hours a month" = '3',
                             "7-9 hours a month" = '4',
                             "10-12 hours a month" = '5', 
                             "13-15 hours a month" = '6', 
                             "More than 15 hours a month" = '7')
Q32_Numeric <- as.numeric(Q32_Numeric)

benchmarking_merged <- left_join(categoric_data,numeric_data,by="ResponseId")

Productivity <- as.numeric(as.character(benchmarking_merged$'Q24'))
benchmarking_merged$Productivity <- Productivity

TurnoverIntentions <- as.numeric(as.character(benchmarking_merged$'Q9_2'))
benchmarking_merged$TurnoverIntentions <- TurnoverIntentions

JobPerformance <- as.numeric(as.character(benchmarking_merged$'Q16_9'))
benchmarking_merged$JobPerformance <- JobPerformance

JobSatisfaction <- as.numeric(as.character(benchmarking_merged$'Q9_7'))
benchmarking_merged$JobSatisfaction <- JobSatisfaction

Employer_NPS <- as.numeric(as.character(benchmarking_merged$'Q23'))
benchmarking_merged$Employer_NPS <- Employer_NPS

OrgCommitment <- as.numeric(as.character(benchmarking_merged$'Q17'))
benchmarking_merged$OrgCommitment <- OrgCommitment

PcvdMgrRatedPerf <- as.numeric(as.character(benchmarking_merged$'Q25'))
benchmarking_merged$PcvdMgrRatedPerf <- PcvdMgrRatedPerf

attach(benchmarking_merged)

myvars <- c('Q4_7', 'Q8_4', 'Q9_8')
myvars2 <- numeric_data[c('Q4_7', 'Q8_4', 'Q9_8')]
WPM20_Authenticity_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_11', 'Q5_11', 'Q16_3')
myvars2 <- numeric_data[c('Q4_11', 'Q5_11', 'Q16_3')]
WPM20_Cognitive_Agility_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q5_9', 'Q7_12', 'Q9_4')
myvars2 <- numeric_data[c('Q5_9', 'Q7_12', 'Q9_4')]
WPM20_Emotional_Regulation_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q16_2', 'Q16_11', 'Q16_15')
myvars2 <- numeric_data[c('Q16_2', 'Q16_11', 'Q16_15')]
WPM20_Empathy_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q6_3', 'Q6_10', 'Q9_1')
myvars2 <- numeric_data[c('Q6_3', 'Q6_10', 'Q9_1')]
WPM20_Engagement_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_2', 'Q7_5', 'Q7_6')
myvars2 <- numeric_data[c('Q4_2', 'Q7_5', 'Q7_6')]
WPM20_Focus_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_12', 'Q6_1', 'Q7_10')
myvars2 <- numeric_data[c('Q4_12', 'Q6_1', 'Q7_10')]
WPM20_Goal_Attainment_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_1', 'Q4_5', 'Q7_3')
myvars2 <- numeric_data[c('Q4_1', 'Q4_5', 'Q7_3')]
WPM20_Growth_Mindset_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_3', 'Q5_12')
myvars2 <- numeric_data[c('Q4_3', 'Q5_12')]
WPM20_Life_Satisfaction_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q7_4', 'Q8_6')
myvars2 <- numeric_data[c('Q7_4', 'Q8_6')]
WPM20_Locus_of_Control_External_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q7_7', 'Q7_11')
myvars2 <- numeric_data[c('Q7_7', 'Q7_11')]
WPM20_Locus_of_Control_Internal_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q6_2', 'Q8_12', 'Q16_8')
myvars2 <- numeric_data[c('Q6_2', 'Q8_12', 'Q16_8')]
WPM20_Nutrition_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_10', 'Q8_2', 'Q16_7')
myvars2 <- numeric_data[c('Q4_10', 'Q8_2', 'Q16_7')]
WPM20_Physical_Activity_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_6', 'Q5_3', 'Q5_6', 'Q8_8')
myvars2 <- numeric_data[c('Q4_6', 'Q5_3', 'Q5_6', 'Q8_8')]
WPM20_PsyCap_Hope_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q7_9', 'Q8_1')
myvars2 <- numeric_data[c('Q7_9', 'Q8_1')]
WPM20_PsyCap_Optimism_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_9', 'Q5_7', 'Q7_1')
myvars2 <- numeric_data[c('Q4_9', 'Q5_7', 'Q7_1')]
WPM20_Purpose_and_Meaning_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q6_12', 'Q8_9', 'Q8_11')
myvars2 <- numeric_data[c('Q6_12', 'Q8_9', 'Q8_11')]
WPM20_Resilience_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q5_1', 'Q8_3', 'Q9_6')
myvars2 <- numeric_data[c('Q5_1', 'Q8_3', 'Q9_6')]
WPM20_Rest_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q6_4', 'Q6_5', 'Q7_8')
myvars2 <- numeric_data[c('Q6_4', 'Q6_5', 'Q7_8')]
WPM20_Self_Awareness_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q5_2', 'Q6_6', 'Q16_5')
myvars2 <- numeric_data[c('Q5_2', 'Q6_6', 'Q16_5')]
WPM20_Self_Compassion_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q5_4', 'Q5_5', 'Q8_10')
myvars2 <- numeric_data[c('Q5_4', 'Q5_5', 'Q8_10')]
WPM20_Self_Efficacy_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_8', 'Q6_9', 'Q16_4')
myvars2 <- numeric_data[c('Q4_8', 'Q6_9', 'Q16_4')]
WPM20_Social_Connection_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q4_4', 'Q6_7', 'Q6_8')
myvars2 <- numeric_data[c('Q4_4', 'Q6_7', 'Q6_8')]
WPM20_Strategic_Planning_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q16_1', 'Q16_10', 'Q16_13')
myvars2 <- numeric_data[c('Q16_1', 'Q16_10', 'Q16_13')]
WPM20_Stress_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q5_8', 'Q7_2', 'Q8_5', 'Q9_5')
myvars2 <- numeric_data[c('Q5_8', 'Q7_2', 'Q8_5', 'Q9_5')]
WPM20_Work_Life_Balance_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q10_1', 'Q10_10', 'Q11_6', 'Q11_11')
myvars2 <- numeric_data[c('Q10_1', 'Q10_10', 'Q11_6', 'Q11_11')]
WPM20_Mgr_Alignment_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q11_3', 'Q11_10', 'Q11_12', 'Q11_14')
myvars2 <- numeric_data[c('Q11_3', 'Q11_10', 'Q11_12', 'Q11_14')]
WPM20_Mgr_Coaching_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q11_5', 'Q11_7', 'Q11_9')
myvars2 <- numeric_data[c('Q11_5', 'Q11_7', 'Q11_9')]
WPM20_Mgr_Empowerment_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q10_2', 'Q10_5', 'Q10_7', 'Q10_8')
myvars2 <- numeric_data[c('Q10_2', 'Q10_5', 'Q10_7', 'Q10_8')]
WPM20_Mgr_Encouraging_Participation_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q10_3', 'Q11_4', 'Q11_13')
myvars2 <- numeric_data[c('Q10_3', 'Q11_4', 'Q11_13')]
WPM20_Mgr_Problem_Solving_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q11_1', 'Q11_2', 'Q11_8')
myvars2 <- numeric_data[c('Q11_1', 'Q11_2', 'Q11_8')]
WPM20_Mgr_Recognition_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q10_4', 'Q10_6', 'Q10_9')
myvars2 <- numeric_data[c('Q10_4', 'Q10_6', 'Q10_9')]
WPM20_Mgr_Relationship_Building_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q12_6', 'Q12_7', 'Q14_1', 'Q14_7')
myvars2 <- numeric_data[c('Q12_6', 'Q12_7', 'Q14_1', 'Q14_7')]
WPM20_IC_Alignment_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q14_6', 'Q14_8', 'Q14_9', 'Q14_11')
myvars2 <- numeric_data[c('Q14_6', 'Q14_8', 'Q14_9', 'Q14_11')]
WPM20_IC_Coaching_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q14_4', 'Q14_5', 'Q14_12')
myvars2 <- numeric_data[c('Q14_4', 'Q14_5', 'Q14_12')]
WPM20_IC_Empowerment_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q12_1', 'Q12_8', 'Q12_9', 'Q12_10')
myvars2 <- numeric_data[c('Q12_1', 'Q12_8', 'Q12_9', 'Q12_10')]
WPM20_IC_Encouraging_Participation_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q12_5', 'Q14_3', 'Q14_14')
myvars2 <- numeric_data[c('Q12_5', 'Q14_3', 'Q14_14')]
WPM20_IC_Problem_Solving_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q14_2', 'Q14_10', 'Q14_13')
myvars2 <- numeric_data[c('Q14_2', 'Q14_10', 'Q14_13')]
WPM20_IC_Recognition_Comp <- composite(myvars2, rel = TRUE)

myvars <- c('Q12_2', 'Q12_3', 'Q12_4')
myvars2 <- numeric_data[c('Q12_2', 'Q12_3', 'Q12_4')]
WPM20_IC_Relationship_Building_Comp <- composite(myvars2, rel = TRUE)

WPM20_ThrivingSocial_Comp <- 
  (WPM20_Authenticity_Comp + 
  WPM20_Empathy_Comp +
  WPM20_Social_Connection_Comp)/3

WPM20TS_Temp = mean(cbind(WPM20_Authenticity_Comp, WPM20_Empathy_Comp, WPM20_Social_Connection_Comp))

WPM20TS_Temp = round(rowMeans(cbind(WPM20_Authenticity_Comp, WPM20_Empathy_Comp, WPM20_Social_Connection_Comp)),1)

WPM20TS_Temp = mean(cbind(WPM20_Authenticity_Comp, WPM20_Empathy_Comp, WPM20_Social_Connection_Comp))

all(WPM20TS_Temp == round(WPM20_ThrivingSocial_Comp,1))

WPM20_ThrivingCognitive_Comp <- 
  (WPM20_Cognitive_Agility_Comp + 
     WPM20_Focus_Comp +
     WPM20_Strategic_Planning_Comp)/3

WPM20_ThrivingEmotional_Comp <- 
  (WPM20_Emotional_Regulation_Comp + 
     WPM20_Self_Compassion_Comp)/2

WPM20_ThrivingPhysical_Comp <- 
  (WPM20_Nutrition_Comp + 
     WPM20_Physical_Activity_Comp +
     WPM20_Rest_Comp)/3

WPM20_ICInspiringGuidingOthers_Comp <- 
  (WPM20_IC_Alignment_Comp + 
     WPM20_IC_Coaching_Comp +
     WPM20_IC_Problem_Solving_Comp)/3

WPM20_ICInspiringMotivatingOthers_Comp <- 
  (WPM20_IC_Empowerment_Comp + 
     WPM20_IC_Recognition_Comp)/2

WPM20_ICInspiringKIncludingOthers_Comp <- 
  (WPM20_IC_Encouraging_Participation_Comp + 
     WPM20_IC_Relationship_Building_Comp)/2

#write.csv(benchmarking_merged, file = "benchmarking_merged.csv")

#cor(WPM20_Stress_Comp, WPM20_Self_Compassion_Comp)

thedata<-data.frame(Q32_Numeric,
                    WPM20_ThrivingSocial_Comp,
                    WPM20_ThrivingCognitive_Comp,
                    WPM20_ThrivingEmotional_Comp,
                    WPM20_ThrivingPhysical_Comp,
                    WPM20_ICInspiringGuidingOthers_Comp,
                    WPM20_ICInspiringMotivatingOthers_Comp,
                    WPM20_ICInspiringKIncludingOthers_Comp
                    )       

Labels<-names(thedata)[2:length(thedata)]
multRegress<-function(mydata){
  numVar<<-NCOL(mydata)
  Variables<<- names(mydata)[2:numVar]
  
  mydata<-cor(mydata, use="complete.obs")
  RXX<-mydata[2:numVar,2:numVar]
  RXY<-mydata[2:numVar,1]
  
  RXX.eigen<-eigen(RXX)
  D<-diag(RXX.eigen$val)
  delta<-sqrt(D)
  
  lambda<-RXX.eigen$vec%*%delta%*%t(RXX.eigen$vec)
  lambdasq<-lambda^2
  beta<-solve(lambda)%*%RXY
  rsquare<<-sum(beta^2)
  
  RawWgt<-lambdasq%*%beta^2
  import<-(RawWgt/rsquare)*100
  
  result<<-data.frame(Variables, Raw.RelWeight=RawWgt, Rescaled.RelWeight=import)
}


multBootstrap<-function(mydata, indices){
  mydata<-mydata[indices,]
  multWeights<-multRegress(mydata)
  return(multWeights$Raw.RelWeight)
}

multBootrand<-function(mydata, indices){
  mydata<-mydata[indices,]
  multRWeights<-multRegress(mydata)
  multReps<-multRWeights$Raw.RelWeight
  randWeight<-multReps[length(multReps)]
  randStat<-multReps[-(length(multReps))]-randWeight
  return(randStat)
}

#bootstrapping
#install.packages("boot")
library(boot)

multRegress(thedata)
RW.Results<-result

RSQ.Results<-rsquare

#R-squared For the Model
RSQ.Results

#The Raw and Rescaled Weights
RW.Results

thedata<-data.frame(Q32_Numeric,
                    WPM20_Authenticity_Comp,                 
                    WPM20_Cognitive_Agility_Comp,            
                    WPM20_Emotional_Regulation_Comp,         
                    WPM20_Empathy_Comp,                      
                    #  WPM20_Engagement_Comp,                   
                    WPM20_Focus_Comp,                        
                    #  WPM20_Goal_Attainment_Comp,              
                    #  WPM20_Growth_Mindset_Comp,               
                    WPM20_IC_Alignment_Comp,                 
                    WPM20_IC_Coaching_Comp,                  
                    WPM20_IC_Empowerment_Comp,               
                    WPM20_IC_Encouraging_Participation_Comp, 
                    WPM20_IC_Problem_Solving_Comp,           
                    WPM20_IC_Recognition_Comp,               
                    WPM20_IC_Relationship_Building_Comp,     
                    #  WPM20_Life_Satisfaction_Comp,            
                    #  WPM20_Locus_of_Control_External_Comp,    
                    #  WPM20_Locus_of_Control_Internal_Comp,    
                    #  WPM20_Mgr_Alignment_Comp,                
                    #  WPM20_Mgr_Coaching_Comp,                 
                    #  WPM20_Mgr_Empowerment_Comp,              
                    #  WPM20_Mgr_Encouraging_Participation_Comp,
                    #  WPM20_Mgr_Problem_Solving_Comp,          
                    #  WPM20_Mgr_Recognition_Comp,              
                    #  WPM20_Mgr_Relationship_Building_Comp,    
                    WPM20_Nutrition_Comp,                    
                    WPM20_Physical_Activity_Comp,            
                    #  WPM20_PsyCap_Hope_Comp,                  
                    #  WPM20_PsyCap_Optimism_Comp,              
                    #  WPM20_Purpose_and_Meaning_Comp,          
                    #  WPM20_Resilience_Comp,                   
                    WPM20_Rest_Comp,                         
                    #  WPM20_Self_Awareness_Comp,               
                    WPM20_Self_Compassion_Comp,              
                    #  WPM20_Self_Efficacy_Comp,                
                    WPM20_Social_Connection_Comp,            
                    WPM20_Strategic_Planning_Comp           
                    #  WPM20_Stress_Comp,                       
                    #  WPM20_Work_Life_Balance_Comp
)       

Labels<-names(thedata)[2:length(thedata)]
multRegress<-function(mydata){
  numVar<<-NCOL(mydata)
  Variables<<- names(mydata)[2:numVar]
  
  mydata<-cor(mydata, use="complete.obs")
  RXX<-mydata[2:numVar,2:numVar]
  RXY<-mydata[2:numVar,1]
  
  RXX.eigen<-eigen(RXX)
  D<-diag(RXX.eigen$val)
  delta<-sqrt(D)
  
  lambda<-RXX.eigen$vec%*%delta%*%t(RXX.eigen$vec)
  lambdasq<-lambda^2
  beta<-solve(lambda)%*%RXY
  rsquare<<-sum(beta^2)
  
  RawWgt<-lambdasq%*%beta^2
  import<-(RawWgt/rsquare)*100
  
  result<<-data.frame(Variables, Raw.RelWeight=RawWgt, Rescaled.RelWeight=import)
}


multBootstrap<-function(mydata, indices){
  mydata<-mydata[indices,]
  multWeights<-multRegress(mydata)
  return(multWeights$Raw.RelWeight)
}

multBootrand<-function(mydata, indices){
  mydata<-mydata[indices,]
  multRWeights<-multRegress(mydata)
  multReps<-multRWeights$Raw.RelWeight
  randWeight<-multReps[length(multReps)]
  randStat<-multReps[-(length(multReps))]-randWeight
  return(randStat)
}

#bootstrapping
#install.packages("boot")
library(boot)

multRegress(thedata)
RW.Results<-result

RSQ.Results<-rsquare

#R-squared For the Model
RSQ.Results

#The Raw and Rescaled Weights
RW.Results

