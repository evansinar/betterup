#REFERENCE: http://dwoll.de/rexrepos/posts/regressionModMed.html

wants <- c("mediation", "multilevel", "QuantPsyc")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

remove(list = ls())

df <- read.csv(file="datafiles/benchmarking_merged_NoItemLevel_CSV_LdrsOnly.csv", header=TRUE, sep=",")

fitMod <- lm(WPM20_Engagement_Comp ~ WPM20_Authenticity_Comp + 
               RapidChange + WPM20_Authenticity_Comp:RapidChange, data=df)
summary(fitMod)

library(QuantPsyc)
sim.slopes(fitMod, RapidChange)

modreg <- function(dv, iv, modv) {
  fitMod <- lm(dv ~ iv + modv + iv:modv, data=df)
  print(summary(fitMod))
  sim.slopes(fitMod, modv)
}

modreg(WPM20_Engagement_Comp, WPM20_Authenticity_Comp, RapidChange) #SIG STRONGER EFFECT FOR HIGH CHANGE
modreg(WPM20_Engagement_Comp, WPM20_Cognitive_Agility_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Emotional_Regulation_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Empathy_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Focus_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Nutrition_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Physical_Activity_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Rest_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Self_Compassion_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Social_Connection_Comp, RapidChange)
modreg(WPM20_Engagement_Comp, WPM20_Strategic_Planning_Comp, RapidChange)

modreg(Employer_NPS, WPM20_Authenticity_Comp, RapidChange) 
modreg(Employer_NPS, WPM20_Cognitive_Agility_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Emotional_Regulation_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Empathy_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Focus_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Nutrition_Comp, RapidChange) #SIG STRONGER EFFECT FOR HIGH CHANGE
modreg(Employer_NPS, WPM20_Physical_Activity_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Rest_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Self_Compassion_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Social_Connection_Comp, RapidChange)
modreg(Employer_NPS, WPM20_Strategic_Planning_Comp, RapidChange)

modreg(Productivity, WPM20_Authenticity_Comp, RapidChange)
modreg(Productivity, WPM20_Cognitive_Agility_Comp, RapidChange)
modreg(Productivity, WPM20_Emotional_Regulation_Comp, RapidChange)
modreg(Productivity, WPM20_Empathy_Comp, RapidChange)
modreg(Productivity, WPM20_Focus_Comp, RapidChange)
modreg(Productivity, WPM20_Nutrition_Comp, RapidChange) 
modreg(Productivity, WPM20_Physical_Activity_Comp, RapidChange)
modreg(Productivity, WPM20_Rest_Comp, RapidChange)
modreg(Productivity, WPM20_Self_Compassion_Comp, RapidChange)
modreg(Productivity, WPM20_Social_Connection_Comp, RapidChange)
modreg(Productivity, WPM20_Strategic_Planning_Comp, RapidChange)

modreg(JobSatisfaction, WPM20_Authenticity_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Cognitive_Agility_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Emotional_Regulation_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Empathy_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Focus_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Nutrition_Comp, RapidChange) 
modreg(JobSatisfaction, WPM20_Physical_Activity_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Rest_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Self_Compassion_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Social_Connection_Comp, RapidChange)
modreg(JobSatisfaction, WPM20_Strategic_Planning_Comp, RapidChange)

modreg(OrgCommitment, WPM20_Authenticity_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Cognitive_Agility_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Emotional_Regulation_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Empathy_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Focus_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Nutrition_Comp, RapidChange) 
modreg(OrgCommitment, WPM20_Physical_Activity_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Rest_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Self_Compassion_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Social_Connection_Comp, RapidChange)
modreg(OrgCommitment, WPM20_Strategic_Planning_Comp, RapidChange)


