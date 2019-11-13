#REFERENCES: 
#https://stackoverflow.com/questions/51861346/r-pos-tagging-and-tokenizing-in-one-go

library(udpipe)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

df <- read.csv(file="datafiles/MentoringText.csv", header=TRUE, sep=",")

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

x <- udpipe_annotate(udmodel, 
                     df$Comments)
x <- as.data.frame(x)

x %>% select(token, upos)

write.csv(x,'mentoring_pos.csv')

#Just Adjectives
justadj <- subset(x, upos=="ADJ" & token!="brown")

set.seed(1234)
wordcloud(words = justadj$token, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(8, "Dark2"))

#Just Nouns
justnoun <- subset(x, upos=="NOUN")

set.seed(1234)
wordcloud(words = justnoun$token, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(8, "Dark2"))

#Just Verbs
justverb <- subset(x, upos=="VERB")

set.seed(1234)
wordcloud(words = justverb$token, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(8, "Dark2"))