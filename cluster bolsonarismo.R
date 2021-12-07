#load
library(readxl)
library(tidyverse)
base <- read_excel("dados.xlsx")

str(base) #tudo em votos válidos, pop logaritimizado, não trabalharei com dados dicotômicos

# tentativa de copiar passo a passo - https://rpubs.com/gabrielmartos/ClusterAnalysis

#técnica 1

dados <- subset(base, select = c(CIDADE, pop, Bolsonaro2018, Neves14)) # primeiro farei só com esses três
dados$Bolsonaro2018 <- dados$Bolsonaro2018*100
dados$Neves14 <- dados$Neves14*100
head(dados)
dados<-
  dados %>%
  mutate_at(c(2:4),  ~(scale(.) %>% as.vector)) # reescala (novo objeto dados clusters)

dados.stand <- scale(dados[-1])# To standarize the variables
head(dados)

# K-Means
k.means.fit <- kmeans(dados.stand, 2) # k = 2 dois clusters
head(dados.stand)

attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster
# Cluster size:
k.means.fit$size


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dados.stand, nc=6) 
# realmente 2

library(cluster)
clusplot(dados.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

table(dados[,1],k.means.fit$cluster)# deu erro

d <- dist(dados.stand, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward")

plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=2, border="red")

table(dados[2,3],groups)


# técnica 2 
set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados[,c("Neves14","Bolsonaro2018", "pop")], centers=2, nstart=10)
grpAntiPt

o=order(grpAntiPt$cluster)
data.frame(dados$CIDADE[o],grpAntiPt$cluster[o])


plot(dados$Bolsonaro2018, dados$Neves14, type="n", xlim=c(-3,2), ylim = c(-2,2.4), xlab="Bolsonaro2018", ylab="Neves2014")
text(x=dados$Bolsonaro2018, y=dados$Neves14, labels=dados$CIDADE,col=grpAntiPt$cluster+1)

plot(dados$Bolsonaro2018, dados$pop, type="n", xlim=c(-3,2), ylim = c(-2,2.4), xlab="Bolsonaro2018", ylab="População da Cidade")
text(x=dados$Bolsonaro2018, y=dados$Neves14, labels=dados$CIDADE,col=grpAntiPt$cluster+1)


set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados[,c("Neves14","Bolsonaro2018", "pop")], centers=3, nstart=10)
grpAntiPt

o=order(grpAntiPt$cluster)
data.frame(dados$CIDADE[o],grpAntiPt$cluster[o])


plot(dados$Bolsonaro2018, dados$Neves14, type="n", xlim=c(-3,2), ylim = c(-2,2.4), xlab="Bolsonaro2018", ylab="Neves2014")
text(x=dados$Bolsonaro2018, y=dados$Neves14, labels=dados$CIDADE,col=grpAntiPt$cluster+1)

plot(dados$Bolsonaro2018, dados$pop, type="n", xlim=c(-3,2), ylim = c(-2,3.3), xlab="Bolsonaro2018", ylab="População da Cidade")
text(x=dados$Bolsonaro2018, y=dados$pop, labels=dados$CIDADE,col=grpAntiPt$cluster+1)

# dar zoom para ver melhor os clusters


# posso fazer como eu quiser agora #tirar pop e repetir  tudo 

dados <- subset(base, select = c(CIDADE, Bolsonaro2018, Neves14)) # primeiro farei só com esses três
dados$Bolsonaro2018 <- dados$Bolsonaro2018*100
dados$Neves14 <- dados$Neves14*100
head(dados)
dados<-
  dados %>%
  mutate_at(c(2:3),  ~(scale(.) %>% as.vector)) # reescala (novo objeto dados clusters)

dados.stand <- scale(dados[-1])# To standarize the variables
head(dados)

# K-Means
k.means.fit <- kmeans(dados.stand, 3) # k = ultimo numero
head(dados.stand)

attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster
# Cluster size:
k.means.fit$size


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dados.stand, nc=6) 
# realmente 2

library(cluster)
clusplot(dados.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#table(dados[,1],k.means.fit$cluster)# deu erro

d <- dist(dados.stand, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward")

plot(H.fit, labels = dados$CIDADE, hang = -0.9, cex = 0.7, main = "", sub = NULL,
     xlab = "CIDADE")
rect.hclust(H.fit, k=2, border="blue")


# técnica 2 
set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados[,c("Neves14","Bolsonaro2018")], centers=3, nstart=10)
grpAntiPt

o=order(grpAntiPt$cluster)
data.frame(dados$CIDADE[o],grpAntiPt$cluster[o])


plot(dados$Bolsonaro2018, dados$Neves14, type="n", xlim=c(-3,2), ylim = c(-2,2.4), xlab="Bolsonaro2018", ylab="Neves2014")
text(x=dados$Bolsonaro2018, y=dados$Neves14, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# essa é uma tabela que usarei

# modelão completo , HOBUS 2018 e BOLSONARO 2018 ####
# OBS Hobus principal força política da região

base <- read_excel("dados.xlsx")
str(base)

dados <- subset(base, select = c(CIDADE, Bolsonaro2018, Ciro2018, pop, Lula2006,Hobus18))
dados$Bolsonaro2018 <- dados$Bolsonaro2018*100
head(dados)
dados<-
  dados %>%
  mutate_at(c(2:6),  ~(scale(.) %>% as.vector)) # reescala (novo objeto dados clusters)

dados.stand <- scale(dados[-1])# To standarize the variables
head(dados)

# K-Means
k.means.fit <- kmeans(dados.stand, 2) # k = ultimo numero
head(dados.stand)

attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster
# Cluster size:
k.means.fit$size
# ficou uma divisão boa, vou manter

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dados.stand, nc=6) 
# realmente 2

library(cluster)
clusplot(dados.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#table(dados[,1],k.means.fit$cluster)# deu erro

d <- dist(dados.stand, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward")

plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=2, border="red")

# técnica 2 
set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados[,c("Bolsonaro2018", "Ciro2018","pop","Lula2006","Hobus18")], centers=2, nstart=10)
grpAntiPt

o=order(grpAntiPt$cluster)
data.frame(dados$CIDADE[o],grpAntiPt$cluster[o])

#para xlim e ylim
summary(dados$Hobus18)
summary(dados$Bolsonaro2018)

plot(dados$Bolsonaro2018, dados$Hobus18, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="Hobus2018")
text(x=dados$Bolsonaro2018, y=dados$Hobus18, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# nao tão util

plot(dados$pop, dados$Hobus18, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="pop", ylab="Hobus2018")
text(x=dados$pop, y=dados$Hobus18, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# nao tão util


plot(dados$Bolsonaro2018, dados$pop, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="População")
text(x=dados$Bolsonaro2018, y=dados$pop, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# n

plot(dados$Bolsonaro2018, dados$Lula2006, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="Lula 2006")
text(x=dados$Bolsonaro2018, y=dados$Lula2006, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# OPA utilizada


set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados[,c("Bolsonaro2018", "Ciro2018","pop","Lula2006","Hobus18")], centers=3, nstart=10)
grpAntiPt

plot(dados$Bolsonaro2018, dados$Lula2006, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="Lula 2006")
text(x=dados$Bolsonaro2018, y=dados$Lula2006, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# OPA utilizada
#COM 3 não

plot(dados$Bolsonaro2018, dados$Hobus18, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="Hobus2018")
text(x=dados$Bolsonaro2018, y=dados$Hobus18, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# nao tão util

plot(dados$pop, dados$Hobus18, type="n", xlim=c(-3,3.3),
     ylim = c(-2,4), xlab="pop", ylab="Hobus2018")
text(x=dados$pop, y=dados$Hobus18, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# nao tão util


set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados[,c("Bolsonaro2018", "Ciro2018","pop","Lula2006","Hobus18")], centers=4, nstart=10)
grpAntiPt

plot(dados$Bolsonaro2018, dados$Lula2006, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="Lula 2006")
text(x=dados$Bolsonaro2018, y=dados$Lula2006, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# OPA utilizada
#COM 3 não

plot(dados$Bolsonaro2018, dados$Hobus18, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="Bolsonaro2018", ylab="Hobus2018")
text(x=dados$Bolsonaro2018, y=dados$Hobus18, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# nao tão util

plot(dados$pop, dados$Hobus18, type="n", xlim=c(-3,2),
     ylim = c(-2,3), xlab="pop", ylab="Hobus2018")
text(x=dados$pop, y=dados$Hobus18, labels=dados$CIDADE,col=grpAntiPt$cluster+1)
# nao tão util

