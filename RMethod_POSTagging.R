#REFERENCES: 
#https://stackoverflow.com/questions/51861346/r-pos-tagging-and-tokenizing-in-one-go

library(udpipe)
library(dplyr)

df <- read.csv(file="datafiles/MentoringText.csv", header=TRUE, sep=",")

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

x <- udpipe_annotate(udmodel, 
                     df$Comments)
x <- as.data.frame(x)

x %>% select(token, upos)

write.csv(x,'mentoring_pos.csv')

