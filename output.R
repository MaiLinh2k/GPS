#############Output###############
#install.packages("ggmap") #cai goi tin ggmap
library(ggmap) # thu vien ggmap
#install.packages("ggplot2") # cai goi tin ggplot2
library(ggplot2) # thu vien ggplot2 
if (!exists('mapImageData'))
  mapImageData <- get_googlemap(center = c(lon = median(data1$longitude), lat = median(data1$latitude)),                                                                                            zoom = 15, maptype = c("terrain"))
m <- ggmap(mapImageData, extent = "device")+ scale_colour_manual(values = c("Blue", "Red","Green", "Yellow", "Orange")) 
#ve tung trajectory theo mau cua cluster
tracklist<-unique(data1$track_id)

for (i in tracklist) #max cua track_id
{ 
  df <-subset(data1, track_id==i) #du lieu cua trajectory thu i
  clusID <- paste("Cluster",dens1$cluster[i]%%7)
  df <- cbind(df, clusID)
  if (clusID == "Cluster 1")
    m <- m + 
    geom_path(data = df, linetype="dotted", aes(x = longitude, y = latitude, colour = clusID))
  else if (clusID == "Cluster 2")
    m <- m + 
    geom_path(data = df, aes(x = longitude, y = latitude, colour = clusID))
  
}              
print(m)

