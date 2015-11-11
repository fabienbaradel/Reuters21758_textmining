require(cluster)
require(fpc)
library(HSAUR)
require( clValid)


#####################################################
################# K-MEANS ###########################
#####################################################
#####################################################

#take only the test set
bow.norm.clust<-data.frame(x=1:nrow(bow.norm),bow.norm)
bow.norm.clust<-bow.norm.clust[rowTest,]
nrow(bow.norm.clust)
str(bow.norm.clust)



# Determine number of clusters on withinss
wss <- (nrow(bow.norm.clust[,-1])-1)*sum(apply(bow.norm.clust[,-1],2,var))
for (i in 2:100){
  print(i)
  wss[i] <- sum(kmeans(bow.norm.clust[,-1], 
                       centers=i)$withinss)
}
plot(1:100, wss, type="l", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within groups sum of squares depending K")


#compute the distance metrix (euclidan)
dist<-dist(bow.norm.clust[,-1],method="euclidean")


#function which helps to do the internal validation on kmeans
internal.validation<-function(dist, cluster, kmin, kmax, matrixdata){
  n<-kmax-kmin+1
  Dunn<-1:n
  Sil<-1:n
  for(i in 1:n){
    print(i)
    set.seed(42)
    fit <- cluster(matrixdata, kmin+i-1)
    Dunn[i]<-dunn(dist, fit$cluster)
    Sil[i]<-summary(silhouette(fit$cluster, dist))$avg.width
  }
  list<-list(Dunn, Sil)
  return(list)
}


kmeans.internal.validation<-internal.validation(dist, kmeans, 2,100, bow.norm.clust[,-1])
kmeans.internal.validation

plot(2:100, kmeans.internal.validation[[1]], type="l", xlab="Number of Clusters",
     ylab="Dunn", main="Dunn index depending on number of clusters")


plot(2:100, kmeans.internal.validation[[2]], type="l", xlab="Number of Clusters",
     ylab="Silhouette", main="Silhouette index depending on number of clusters")




##### LINK BETWEEN 10MEANS AND THE 10 TOPICS

set.seed(42)
fit <- kmeans(bow.norm, 10) # 10 clusters
Topics<-c("topic.earn","topic.acq","topic.money.fx","topic.grain", "topic.crude","topic.trade","topic.interest","topic.ship",
          "topic.wheat","topic.corn")

data.cluster.KM<-data.frame(data[-nbRowNULL,c("pid","fileName","purpose",Topics)], fit$cluster)
str(data.cluster.KM)

mat.KMEANS.TOPICS<-matrix(0, ncol=10, nrow=10, dimnames = list(1:10, Topics))
for(i in 1:nrow(bow.norm)){
  print(i)
  cluster<-data.cluster.KM[i,ncol(data.cluster.KM)]
  for(j in 1:10){
    mat.KMEANS.TOPICS[cluster,j]<-mat.KMEANS.TOPICS[cluster,j]+data.cluster.KM[i,3+j]
  }
}

sum(data.cluster.KM[,4:13])
sum(mat.KMEANS.TOPICS)#we get the same number, so all have been assigned

mat.KMEANS.TOPICS.bis<-mat.KMEANS.TOPICS
#topics don't have the same weight, so we will put the same weight for each topic
for(i in 1:10){
  mat.KMEANS.TOPICS.bis[,i]<-mat.KMEANS.TOPICS[,i]/sum(mat.KMEANS.TOPICS[,i])
}

#now we will put frequency as probabilities where sum of the probailities
# for a cluster is equals to 1
for(i in 1:10){
  mat.KMEANS.TOPICS.bis[i,]<-mat.KMEANS.TOPICS.bis[i,]/sum(mat.KMEANS.TOPICS.bis[i,])
}

mat.KMEANS.TOPICS.bis<-mat.KMEANS.TOPICS.bis*100


TP<-1:50
for(k in 1:50){
  print(k)
  for(i in 1:10){
    for(j in 1:10){
      if(mat.KMEANS.TOPICS.bis[i,j]>k){
        TP[k]<-TP[k]+mat.KMEANS.TOPICS[i,j]
      }
    }
  }
}

precision<-TP/sum(mat.PAM.TOPICS)

plot(1:50, precision, type="l", xlab="Percentage minimum to be in the cluster for a topic",
     ylab="Precision", main="Precision to predict a document into a cluster depending on cluster definition")






#############################################
########### PAM #############################
#############################################

w<-clara(bow.norm.clust[,-1],5)
w$silinfo #silhouette index
mean(w$clusinfo[,3]) #average dissimilarity between the observations in the cluster and the cluster’s medoid

# Determine number of clusters on withinss
silh <- 1:99
av.diss <- 1:99

for (i in 2:100){
  print(i)
  set.seed(42)
  pam<-clara(bow.norm.clust[,-1],i)
  silh[i-1] <- pam$silinfo$avg.width
  av.diss[i-1] <-mean(pam$clusinfo[,3])
  #dunn[i]<-dunn(dist, pam$cluster)
}
plot(2:100, silh, type="l", xlab="Number of Clusters",
     ylab="Silhouette index", main="Silhouette index depending K")

plot(2:100, av.diss, type="l", xlab="Number of Clusters",
     ylab="Average dissimilarity", main="Average dissimilarity to medoids depending K")


##### LINK BETWEEN 10MEANS AND THE 10 TOPICS

set.seed(42)
fit <- clara(bow.norm, 10) # 10 clusters
Topics<-c("topic.earn","topic.acq","topic.money.fx","topic.grain", "topic.crude","topic.trade","topic.interest","topic.ship",
          "topic.wheat","topic.corn")

data.cluster.PAM<-data.frame(data[-nbRowNULL,c("pid","fileName","purpose",Topics)], fit$cluster)
str(data.cluster.PAM)

mat.PAM.TOPICS<-matrix(0, ncol=10, nrow=10, dimnames = list(1:10, Topics))
for(i in 1:nrow(bow.norm)){
  print(i)
  cluster<-data.cluster.PAM[i,ncol(data.cluster.PAM)]
  for(j in 1:10){
    mat.PAM.TOPICS[cluster,j]<-mat.PAM.TOPICS[cluster,j]+data.cluster.PAM[i,3+j]
  }
}

sum(data.cluster.PAM[,4:13])
sum(mat.PAM.TOPICS)#we get the same number, so all have been assigned

mat.PAM.TOPICS.bis<-mat.PAM.TOPICS
#topics don't have the same weight, so we will put the same weight for each topic
for(i in 1:10){
  mat.PAM.TOPICS.bis[,i]<-mat.PAM.TOPICS[,i]/sum(mat.PAM.TOPICS[,i])
}

#now we will put frequency as probabilities where sum of the probailities
# for a cluster is equals to 1
for(i in 1:10){
  mat.PAM.TOPICS.bis[i,]<-mat.PAM.TOPICS.bis[i,]/sum(mat.PAM.TOPICS.bis[i,])
}

mat.PAM.TOPICS.bis<-mat.PAM.TOPICS.bis*100
mat.PAM.TOPICS.bis

#compute the "precision" for 1

TP<-1:50
for(k in 1:50){
  print(k)
  for(i in 1:10){
    for(j in 1:10){
      if(mat.PAM.TOPICS.bis[i,j]>k){
        TP[k]<-TP[k]+mat.PAM.TOPICS[i,j]
      }
    }
  }
}

precision<-TP/sum(mat.PAM.TOPICS)

plot(1:50, precision, type="l", xlab="Percentage minimum to be in the cluster for a topic",
     ylab="Precision", main="Precision to predict a document into a cluster depending on cluster definition")


############################################
######### HCLUST ###########################
############################################

dist<-dist(t(bow.norm.clust[,-1]),method="euclidean")
library(cluster) 
#using "ward"
fit <- hclust(dist, "ward")   
fit
plot(fit)   
groups <- cutree(fit, k=10)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=10, border="red") # draw dendogram with red borders around the 10 clusters   

#using "ave"
fit <- hclust(dist, "ave")   
fit
plot(fit)   
groups <- cutree(fit, k=10)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=10, border="red") # draw dendogram with red borders around the 10 clusters 