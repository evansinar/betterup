library(corrplot)
library(ggplot2)

remove(list = ls())

topics_ID <- read.csv(file="datafiles/Session Coding and ID Variables.csv", header=TRUE, sep=",")

attach(topics_ID)

{plot.new(); dev.off()}

categorical_comp <- function(var1, var2) {
  lab <- deparse(substitute(var2))
  xtabs(~var1+var2,data=topics_ID) 
  xtabs_table <- xtabs(~var1+var2,data=topics_ID) 
  
  chisq <- chisq.test(xtabs_table) 
  print(chisq)
  chisq$residuals[ chisq$residuals < 2 & chisq$residuals > -2 ] = 0
  corrplot(chisq$residuals, tl.cex = 0.6, is.cor = FALSE, cl.pos = "n", 
           title = lab, mar=c(2,2,3,3), col.main = "steelblue3")
}

pdf("categorical_topic_name.pdf") 

categorical_comp(topic_name,job_function) 
categorical_comp(topic_name,role_transition) 
categorical_comp(topic_name,helped_manage_change) 
categorical_comp(topic_name,impacted_by_changes_work) 
categorical_comp(topic_name,working_toward_change) 
categorical_comp(topic_name,uncertainty_work) 
categorical_comp(topic_name,level_of_responsibility) 
categorical_comp(topic_name,general_goal) 
categorical_comp(topic_name,categorize_primary_goal) 
categorical_comp(topic_name,plan_coaching) 
categorical_comp(topic_name,goal_desc_label) 

dev.off() 