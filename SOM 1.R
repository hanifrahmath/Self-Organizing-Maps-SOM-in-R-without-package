x <- iris

setosa <- subset(x, Species == "setosa")
virginica <- subset(x, Species == "virginica")
versicolor <- subset(x, Species == "versicolor")

set.seed(1)
ind <- sample(2, nrow(x), replace = TRUE, prob = c(0.9, 0.1))
xtrain_sample <- x[ind == 1,]
xtest_sample <- x[ind == 2,]
#ind <- sample(2, nrow(setosa), replace = TRUE, prob = c(0.6, 0.4))
#xtrain_sample <- rbind(setosa[ind == 1,], virginica[ind == 1,], versicolor[ind == 1,])
xtrain <- xtrain_sample[,-5]
#xtest_sample <- rbind(setosa[ind == 2,], virginica[ind == 2,], versicolor[ind == 2,])
xtest <- xtest_sample[,-5]

L <- 3 #number of representative vector
alpha <- 0.05#learning rate
betha <- 0.4 #perubahan learning rate
epoch <- 0
epochtot <- 50 #iterasi maksimum
minalpha <- 10^-9 #alpha minimum

#inisialisasi bobot
#Sepal.Length <- runif(3)+3
#Sepal.Width <- runif(3)+3
#Petal.Length <- runif(3)+3
#Petal.Width <- runif(3)+3
#v <- data.frame(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width) #inisialisasi bobot random
set.seed(1)
#v <- xtrain[sample(nrow(xtrain), 3),] #dari data latih secara random
vc1 <- setosa[sample(nrow(setosa), 1),]
vc2 <- virginica[sample(nrow(virginica), 1),]
vc3 <- versicolor[sample(nrow(versicolor), 1),]
v <- rbind(vc1,vc2,vc3) #dari data latih masing-masing kelas
v <- v[-c(5)] 

d <- c()

#TRAINING
while ((epoch < epochtot) && (alpha > minalpha)) {
  for (i in 1:nrow(xtrain)){
    for (l in 1:L){
      d[l] <- dist(rbind(xtrain[i,], v[l,]), method = "euclidean")
    }
    indMin <- which(d == min(d))
    v[indMin,] <- v[indMin,] + alpha*(xtrain[1,]-v[indMin,])
  }
  alpha <- betha*alpha
  epoch <- epoch + 1
}

#TESTING
cluster <- c()
for (i in 1:nrow(xtest)) {
  for (l in 1:3) {
    d[l] <- dist(rbind(xtest[i,], v[l,]), method = "euclidean")
  } 
  indMin <- which(d == min(d))
  cluster[i] <- indMin
}

xtest$cluster <- cluster
#xtest.clust <- xtest
#for (i in 1:nrow(xtest)){
# if (xtest$cluster[i] == 1){
#  xtest$cluster[i] = "setosa"
#  } else if (xtest$cluster[i] == 3){
#   xtest$cluster[i] = "virginica"
# } else {
#   xtest$cluster[i] = "versicolor"
# }
#}

#for (i in 1:nrow(xtest)){
# if (xtest$cluster[i] == "setosa"){
#   xtest$cluster[i] = 1
# } else if (xtest$cluster[i] == "virginica"){
#   xtest$cluster[i] = "2"
# } else {
#   xtest$cluster[i] = "3"
# }
#}

#xtest.scaled <- xtest_sample
#xtest.scaled$Species <- as.numeric(xtest.scaled$Species)
confusion_matrix <- table(xtest$cluster, cluster)
#clust <- as.numeric(xtest$cluster)
#confusion_matrix_class <- confusionMatrix(as.factor(xtest.scaled$Species), 
#as.factor(xtest$cluster))
confusion_matrix
#confusion_matrix_class

# Cluster validation
#xtest.clust <- xtest
xtest.scaled <- xtest[,-5]
# Compute pairwise-distance matrices
dd <- dist(xtest.scaled, method ="euclidean")
# Statistics for k-means clustering
cstats <- cluster.stats(dd,  xtest$cluster)
# (k-means) within clusters sum of squares
MSE <- cstats$within.cluster.ss/nrow(xtest)
MSE
