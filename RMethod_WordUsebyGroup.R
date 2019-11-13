#REFERENCES: https://stackoverflow.com/questions/52804450/comparison-of-two-groups-using-word-cloud-comparison-r
#https://tutorials.quanteda.io/basic-operations/corpus/corpus/
#https://www.rdocumentation.org/packages/quanteda/versions/0.99/topics/textplot_wordcloud

library(quanteda)
library(readtext)
library(RColorBrewer)

remove(list = ls())

#coaching_workon <- read.csv(file="datafiles/benchmarking_coachinginterestbygender.csv", header=TRUE, sep=",")

#attach(coaching_workon)

data_coachingworkon <- readtext("RMethod Model Scripts/WordUsebyGroup_Data.csv", text_field = "WorkonWithCoach")
names(data_coachingworkon)

corpus_gender_coaching <- corpus(data_coachingworkon)
summary(corpus_gender_coaching, 5)

gender_coaching <- corpus_gender_coaching %>%
  corpus_subset(Gender %in% c("Male", "Female")) %>%
  dfm(remove = stopwords("english"), remove_punct = TRUE, groups = "Gender") %>%
  dfm_trim(min_termfreq = 3)

pdf("textplotbygender.pdf") 

textplot_wordcloud(gender_coaching, comparison = TRUE, 
                   max_words = 100, color = c("blue", "red"), rotation=0)

dev.off()

# Calculate keyness and determine Women as target group
result_keyness <- textstat_keyness(gender_coaching)

write.csv(result_keyness,file="result_keyness_table.csv")

# Plot estimated word keyness

pdf("keynessbygender.pdf") 

textplot_keyness(result_keyness, margin = .2) 

dev.off()