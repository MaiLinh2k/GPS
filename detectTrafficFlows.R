setwd("~/Documents/R-Codes/extractTrafficFlow1")
source('maputils.R')
#install.packages("gplots")
library(gplots)


#load('totalData.RData')
#totalData <- NULL
#for (i in c(1:length(listOfTrajectories)))
#{
#  df<-listOfTrajectories[[i]] 
#  totalData <- rbind(totalData, cbind(i,df))
#}
#save(totalData,file="totalData.RData", ASCII=F)

#https://www.r-bloggers.com/5-ways-to-do-2d-histograms-in-r/
#https://plot.ly/r/2D-Histogram/

#df<-listOfTrajectories[[5]] 
#df <- totalData
#t<- hist(df$speed, breaks = c(0,5,10,15,20,25,30,35,40,100), plot = FALSE )
#hist2d(df$lon, df$lat, nbins = 200)



######################
######Load Data#######
#####################
load('data/basemap.data');
maputils.plotmap(basemap)
load('data/listOfTrajectories.data')

########################
#  Extract features    #
########################
features <- NULL
for (i in c(1:length(listOfTrajectories)))
{
  df<-listOfTrajectories[[i]] 
  t<- hist(df$speed, breaks = c(0,5,10,15,20,25,30,35,40,1000), plot = FALSE )
  #t<- hist(df$speed, breaks = c(0,5,15,25,40,1000), plot = FALSE )
  features <- rbind(features, c(t$density))
}
features <-NULL

########################
#  Clustering          #
########################
s<-kmeans(features[,1:9], 6)
s<-dbscan(features, MinPts = 3, eps = 0.006) # 5  + 5
#s<-dbscan(features, MinPts = 5, eps = 0.006) # 7 clusters
ncluster <- max(s$cluster)

########################
#  Output              #
#######################
#show the trajectories on the map
maputils.plotmap(basemap)
colors <-c("red", "blue", "darkgreen", "orange", "brown", "black", "darkblue")
for (i in c(1:length(listOfTrajectories)))
{
  df<-listOfTrajectories[[i]] 
  col = "grey"
  if (s$cluster[i] >0) # ==1 
  {# 1: chay truc duong chinh + xa lo - xe khach; 2: xe ca nha
   # 3: xe chay truc duong chinh theo tuyen dinh truoc - bus # 4: xe taxi - chay bat cu duong nao,
   #
      col <- colors[s$cluster[i] %% length(colors)]
    maputils.lines(df,col)
  }
}
#clusCount <- hist(s$cluster[s$cluster>0], breaks = seq(0,max(s$cluster),by=1), plot = FALSE)

#legend("center", cex = 0.8, legend = paste("Cluster ", clusCount$breaks[clusCount$breaks>0], " (", clusCount$counts,")", sep = "" ), text.col = colors[1:max(s$cluster)] )

