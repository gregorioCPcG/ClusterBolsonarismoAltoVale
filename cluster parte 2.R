# tecnica 2 - fonte - https://rpubs.com/abdul_yunus/Kmeans_Clustering
# Lets clean the unnecessary items
gc()
rm(list = ls(all = TRUE))


packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(tidyverse) # data manipulation
packages(corrplot)
packages(gridExtra)
packages(GGally)
packages(cluster) # clustering algorithms 
packages(factoextra) # clustering algorithms & visualization
base <- read_excel("dados.xlsx")
dados <- subset(base, select = c(Bolsonaro2018, Neves14, Ciro2018, pop, Lula2006,Hobus18))
dados$Bolsonaro2018 <- dados$Bolsonaro2018*100
head(dados)


# analysis
dados %>%
  gather(attributes, value, 1:5) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = 'lightblue2', color = 'black') +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x="Values", y="Frequency") +
  theme_bw()
corrplot(cor(dados), type = 'upper', method = 'number', tl.cex = 0.9)

# Relationship between Neves14 and Bolsonaro1018
ggplot(dados, aes(x = Neves14, y = Bolsonaro2018)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_bw()

#nromalizar
dadosNorm <- as.data.frame(scale(dados))
head(dadosNorm)

set.seed(123)

dados_K2 <- kmeans(dadosNorm, centers = 2, nstart = 25)
print(dados_K2)

fviz_cluster(dados_K2, data = dadosNorm)

dados_K2$cluster

# Cluster centers
dados_K2$centers

# Cluster size
dados_K2$size

# Between clusters sum of square
dados_K2$betweenss

# Within cluster sum of square
dados_K2$withinss

# Total with sum of square
dados_K2$tot.withinss

# Total sum of square
dados_K2$totss

#varios tipos de numeros de clusters
dados_K3 <- kmeans(dadosNorm, centers = 3, nstart = 25)
dados_K4 <- kmeans(dadosNorm, centers = 4, nstart = 25)
dados_K5 <- kmeans(dadosNorm, centers = 5, nstart = 25)

p1 <- fviz_cluster(dados_K2, geom = "point", data = dadosNorm) + ggtitle(" K = 2")
p2 <- fviz_cluster(dados_K3, geom = "point", data = dadosNorm) + ggtitle(" K = 3")
p3 <- fviz_cluster(dados_K4, geom = "point", data = dadosNorm) + ggtitle(" K = 4")
p4 <- fviz_cluster(dados_K5, geom = "point", data = dadosNorm) + ggtitle(" K = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Determining Optimal Clusters
fviz_nbclust(x = dadosNorm,FUNcluster = kmeans, method = 'wss' )


wssplot <- function(data, nc = 15, set.seed = 12374){
  wss <- (nrow(data) - 1)*sum(apply(data, 2, var))
  for(i in 2:nc) {
    set.seed(1234)
    wss[i] <- sum(kmeans(x = data, centers = i, nstart = 25)$withinss)
  }
  plot(1:nc, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within Group Sum of Square',
       main = 'Elbow Method Plot to Find Optimal Number of Clusters', frame.plot = T,
       col = 'blue', lwd = 1.5)
}

wssplot(dadosNorm)

# Determining Optimal clusters (k) Using Average Silhouette Method

fviz_nbclust(x = dadosNorm,FUNcluster = kmeans, method = 'silhouette' )

#outro metodo
# compute gap statistic
set.seed(12355)
gap_stat <- clusGap(x = dadosNorm, FUN = kmeans, K.max = 15, nstart = 25, B = 50 )

# Print the result
print(gap_stat, method = "firstmax")
# plot the result to determine the optimal number of clusters.
fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 2
set.seed(123)
final <- kmeans(dadosNorm, centers = 2, nstart = 25)
print(final)
fviz_cluster(final, data = dadosNorm)

#We can extract the clusters and add to our initial data to do some descriptive statistics at the cluster level

dadosNorm %>% 
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarize_all('median')
