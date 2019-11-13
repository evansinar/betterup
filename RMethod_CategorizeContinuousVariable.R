#Reference https://stackoverflow.com/questions/40380112/categorize-continuous-variable-with-dplyr

df$category[df$a < 0.5] <- "low"
df$category[df$a > 0.5 & df$a < 0.6] <- "middle"
df$category[df$a > 0.6] <- "high"