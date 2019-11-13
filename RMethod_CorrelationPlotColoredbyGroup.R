# install.packages("ggplot2")
# load package and data

options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
library(ggalt)
theme_set(theme_bw())  # pre-set the bw theme.

df<-read.csv("RMethod Model Scripts/CorrelationPlotColoredbyGroup_Data.csv", header=TRUE)

# Scatterplot
gg <- ggplot(df, aes(x=Duration, y=Top2Box)) + 
  geom_point(aes(col=verb, size=activities_completed_count)) + 
  xlim(c(0, 70)) + 
  ylim(c(0, 1)) + geom_smooth(method = "lm")
  labs(subtitle="Duration Vs Top 2 Box Rating", 
       y="Top 2 Box Rating", 
       x="Duration", 
       title="Scatterplot", 
       caption = "Color = verb")

plot(gg)

# Adding Circle Around Selected Range
high_duration <- df[df$Duration > 15 & df$Duration < 70 & df$Top2Box > 0 & df$Top2Box < 1.0001,]

gg2 <- ggplot(df, aes(x=Duration, y=Top2Box)) + 
  geom_point(aes(col=verb, size=activities_completed_count)) + 
  xlim(c(0, 40)) + 
  ylim(c(0, 1.2)) + geom_smooth(method = "lm") +
  geom_encircle(aes(x=Duration, y=Top2Box), 
              data=high_duration, 
              color="red", 
              size=2, 
              expand=0.08) +   # encircle
  labs(subtitle="Duration Vs Top 2 Box Rating", 
     y="Top 2 Box Rating", 
     x="Duration", 
     title="Scatterplot", 
     caption = "Color = verb")

plot(gg2)