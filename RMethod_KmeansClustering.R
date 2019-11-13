#REFERENCES: https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
  
library(dplyr)

PATH <-"RMethod Model Scripts/KmeansClustering_Data.csv"

df <- read.csv(PATH)

glimpse(df)

rescale_KMeans <- 
  df %>%
  select(-c(Website))

# Ward Hierarchical Clustering
d <- dist(rescale_KMeans, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=6, border="red")

# Determine number of clusters
wss <- (nrow(rescale_KMeans)-1)*sum(apply(rescale_KMeans,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(rescale_KMeans, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Multi-Index Number of Clusters Evaluation (NOTE: Can set min and max clusters)
library(NbClust)
nb <- NbClust(rescale_KMeans, diss=NULL, distance = "euclidean", 
              min.nc=4, max.nc=10, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

kmeans(rescale_KMeans, 6)

pc_cluster <- kmeans(rescale_KMeans, 6)

pc_cluster$cluster
pc_cluster$centers
pc_cluster$size	

center <-pc_cluster$centers

center

library(tidyr)

cluster <- c(1: 52)

library(ggplot2)

library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

pcclusternums <- as.data.frame(pc_cluster$cluster)

write.csv(pcclusternums,'pcclusternums.csv')

library(factoextra) # clustering algorithms & visualization

fviz_cluster(pc_cluster, data = rescale_KMeans)