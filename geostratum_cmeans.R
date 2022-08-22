install.packages("geocmeans")

#packages
library(geocmeans)
library(ggplot2)
library(ggpubr)
library(dplyr) # library(tidyverse)
library(viridis)
library(spdep)
#data
data(LyonIris)

#data selecting for  the analysis
AnalysisFields =c("Lden","NO2","PM25","VegHautPrt","Pct0_14",
                   "Pct_65","Pct_Img","TxChom1564","Pct_brevet","NivVieMed")
#data standardization
Data = LyonIris@data[AnalysisFields] # data para el analisis
for (var in names(Data)){
  Data[[var]] = scale(Data[[var]])
}
# preparing some elements for further mapping
LyonIris$OID = as.character(1:nrow(LyonIris))
FortiData = ggplot2::fortify(LyonIris,region="OID") # extraer coordenadas?

#kmeans
#con fines exploratorios y estimar el numero de clusters adecuados

# finding the best k by using the r2 of the classification
# trying for k from 2 to 10

R2s = sapply(2:10,function(k){
  Clust = kmeans(Data,centers=k,iter.max = 150)
  R2 = Clust$betweenss / Clust$totss
  return(R2)
})

# dataframe n de clusters y coef determi.
Df = data.frame(K=2:10,
                 R2 = R2s)
#plot
ggplot(Df)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")

# k = 4 explica aprox el 50 % de varianza de la data.

#mapeo de los clusters

KMeanClust = kmeans(Data,centers=4,iter.max = 150) #modelo elegido k = 4
LyonIris$Cluster=paste("cluster",KMeanClust$cluster,sep="_") # adjuntar el cluster como variable en la data original

DFmapping = merge(FortiData,LyonIris,by.x="id",by.y="OID") #unir los dos dataframe

#mapeo con ggplot

ggplot(data=DFmapping)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=Cluster),color=rgb(0,0,0,0))+
  coord_fixed(ratio = 1)+
  scale_fill_manual(name="Cluster",values = c("cluster_1"="palegreen3",
                                              "cluster_2"="firebrick",
                                              "cluster_3"="orange",
                                              "cluster_4"="steelblue")) +
  theme( axis.title = ggplot2::element_blank(),
         axis.text = ggplot2::element_blank(),
         axis.ticks = ggplot2::element_blank()
  )

#Classical c-means FCM

#funcion CMeans parametro (m) : 1.5

Cmean = CMeans(data=Data,k=4,m=1.5,maxiter =500,standardize = FALSE, seed = 456, tol = 0.00001, verbose = FALSE)
calcqualityIndexes(Data, Cmean$Belongings, m = 1.5) # fclust  pack

#generalized version of the c-means algorithm GFCM

#testear algunos valores para el paramatro beta
# 0.00 0.05 0.10 0.15 0.20 0.25 0.30 
# 0.35 0.40 0.45 0.50 0.55 0.60 0.65 
# 0.70 0.75 0.80 0.85 0.90 0.95 1.00

beta_values = selectParameters("GFCM",data = Data, k = 4, m = 1.5,
                                beta = seq(0,1,0.05), spconsist = FALSE,
                                tol = 0.00001, seed = 456)
beta_values[c("beta","Silhouette.index","XieBeni.index","Explained.inertia")]

# col.names = c("beta", "silhouette index",
#                 "Xie and Beni index", "explained inertia"),digits = 3)

#beta : 7 -> 0.70	Silhouette.index:0.358	XieBeni.index:0.852	Explained.inertia:0.428

# GFCM vs FCM

GCmean = GCMeans(Data,k = 4,m = 1.5, beta = 0.7,500,standardize = FALSE, seed=456,
                  tol = 0.00001, verbose = FALSE)

r1 = calcqualityIndexes(Data,GCmean$Belongings,m=1.5) # GFCM
r2 = calcqualityIndexes(Data,Cmean$Belongings,m=1.5) # FCM

df = cbind(unlist(r1), unlist(r2))

# GFCM provides a solution that is less fuzzy 
# (higher explained inertia and lower partition entropy)

#membership matrices $Belongings y mapClusters() 

# hreshold of 0.45. If an observation only has 
# values below this probability in a membership matrix,
# it will be labeled as "undecided"

cmeansMaps= mapClusters(LyonIris,Cmean$Belongings,undecided = 0.45)
GcmeansMaps= mapClusters(LyonIris,GCmean$Belongings,undecided = 0.45)

#Probability of belonging to cluster 1 cmeansMaps$ProbaMaps[[1]]
# PLOT de probabilidad de pertencia al cluster 1 en Cmean y GCmean
ggarrange(cmeansMaps$ProbaMaps[[1]],GcmeansMaps$ProbaMaps[[1]], 
          nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
#PLOT  de probabilidad de pertencia al cluster 2
#PLOT  de probabilidad de pertencia al cluster 3
#PLOT  de probabilidad de pertencia al cluster 4

#PLOT DE CLUSTERS

ggarrange(cmeansMaps$ClusterPlot,GcmeansMaps$ClusterPlot,
          nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

#Spatial c-means SFCM 

