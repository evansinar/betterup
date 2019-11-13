df <- read.csv(file="RMethod Model Scripts/CorrelationsSplitbyGroup_Data.csv", header=TRUE, sep=",")

bm_white <- subset(df, Race=="White")

bm_white_numeric <- bm_white[, sapply(bm_white, is.numeric)]

write.csv(cor(bm_white_numeric, use = "pairwise.complete.obs", method = "pearson"),file="bm_white.csv")

bm_nonwhite <- subset(df, Race=="Black or African American" 
                      | Race == "Hispanic or Latin")

bm_nonwhite_numeric <- bm_nonwhite[, sapply(bm_nonwhite, is.numeric)]

write.csv(cor(bm_nonwhite_numeric, use = "pairwise.complete.obs", method = "pearson"),file="bm_nonwhite.csv")