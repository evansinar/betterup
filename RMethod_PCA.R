#Principal Components Analysis

WPMforPCA <- cbind(
  WPM20_Authenticity_Comp,                 
  WPM20_Cognitive_Agility_Comp,            
  WPM20_Emotional_Regulation_Comp,         
  WPM20_Empathy_Comp,                      
  WPM20_Engagement_Comp,                   
  WPM20_Focus_Comp,                        
  WPM20_Goal_Attainment_Comp,              
  WPM20_Growth_Mindset_Comp,               
  #  WPM20_IC_Alignment_Comp,                 
  #  WPM20_IC_Coaching_Comp,                  
  #  WPM20_IC_Empowerment_Comp,               
  #  WPM20_IC_Encouraging_Participation_Comp, 
  #  WPM20_IC_Problem_Solving_Comp,           
  #  WPM20_IC_Recognition_Comp,               
  #  WPM20_IC_Relationship_Building_Comp,     
  WPM20_Life_Satisfaction_Comp,            
  WPM20_Locus_of_Control_External_Comp,    
  WPM20_Locus_of_Control_Internal_Comp,    
  #  WPM20_Mgr_Alignment_Comp,                
  #  WPM20_Mgr_Coaching_Comp,                 
  #  WPM20_Mgr_Empowerment_Comp,              
  #  WPM20_Mgr_Encouraging_Participation_Comp,
  #  WPM20_Mgr_Problem_Solving_Comp,          
  #  WPM20_Mgr_Recognition_Comp,              
  #  WPM20_Mgr_Relationship_Building_Comp,    
  WPM20_Nutrition_Comp,                    
  WPM20_Physical_Activity_Comp,            
  WPM20_PsyCap_Hope_Comp,                  
  WPM20_PsyCap_Optimism_Comp,              
  WPM20_Purpose_and_Meaning_Comp,          
  WPM20_Resilience_Comp,                   
  WPM20_Rest_Comp,                         
  WPM20_Self_Awareness_Comp,               
  WPM20_Self_Compassion_Comp,              
  WPM20_Self_Efficacy_Comp,                
  WPM20_Social_Connection_Comp,            
  WPM20_Strategic_Planning_Comp,           
  WPM20_Stress_Comp,                       
  WPM20_Work_Life_Balance_Comp)

WPM20.pca <- prcomp(WPMforPCA, center = TRUE, scale. = TRUE)
summary(WPM20.pca)
str(WPM20.pca)

library(devtools)
library(factoextra)

fviz_eig(WPM20.pca)

fviz_pca_var(WPM20.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

write.csv(WPM20.pca$rotation, file = "WPM20PCARotation.csv")