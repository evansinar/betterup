###MEANS BY GROUP
###REFERENCE http://rprogramming.net/aggregate-data-in-r-using-data-table/

library(data.table)

df <- read.csv(file="RMethod Model Scripts/MeansbyGroup_Data.csv", header=TRUE, sep=",")

df_dt <- data.table(df)

EEIbyRemote <- as.data.frame(df_dt[, 
              mean(WPM20_EmpExpIndex, na.rm = TRUE),by = RemoteStatus])
EEIbyRemote
