library(XML)
library(OpenStreetMap)
# run: sudo R CMD javareconf
# if error in rJava

#library(lubridate)
#http://www.r-bloggers.com/stay-on-track-plotting-gps-tracks-with-r/


#read gpx file to a dataframe
maputils.gpxread <- function(filename)
{
  pfile <- htmlTreeParse(filename,
                         error = function (...) {}, useInternalNodes = T)
  # Get all elevations, times and coordinates via the respective xpath
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  # Extract latitude and longitude from the coordinates
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  # Put everything in a dataframe and get rid of old variables
  geodf <- data.frame(lat = lats, lon = lons, ele = elevations)
  rm(list=c("elevations", "lats", "lons", "pfile", "coords"))
  maputils.gpxread<- geodf  
}

maputils.csvread<-function(filename)
{
  tf <-read.csv(filename, header = TRUE)
  geodf <- data.frame(lat=tf$LATITUDE, lon = tf$LONGITUDE, ele = tf$ALTITUDE)
}


shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


maputils.openmap <- function(lat1, lon1, lat2, lon2)
{
  map <- openmap(c(lat1, lon1), c(lat2, lon2), type = "osm")
  maputils.openmap <- openproj(map, projection = "+proj=longlat")
}

maputils.getbound <- function(geodf)
{
  r<-0
  r$lat1<-max(geodf$lat)
  r$lon1<-min(geodf$lon)
  r$lat2<-min(geodf$lat)
  r$lon2<-max(geodf$lon)
  maputils.getbound <- r #c(lat1,lon1,lat2,lon2)
}

maputils.openmap2 <- function(geodf)
{
  lat1<-max(geodf$lat)
  lon1<-min(geodf$lon)
  lat2<-min(geodf$lat)
  lon2<-max(geodf$lon)
  maputils.openmap2(lat1,lon1,lat2,lon2)
}

maputils.plotmap <- function(basemap)
{
  plot(basemap, raster=T)
}

maputils.plot <- function(geodf,basemap=NULL)
{
  par(mar = c(4.5,4.5,0.5,0.5))
  if (is.null(basemap))
  {
    # Plot the track without any map, the shape of the track is already visible.
    plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
    box()
  }
  else
  {
    plot(basemap, raster=T)
    lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("blue", .85), lwd = 4)    
  }
}

maputils.lines <- function(geodf,color="blue")
{
    lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha(color, .85), lwd = 2)    
}

maputils.points <- function(geodf,color="red", pch = 21)
{
  points(geodf$lon, geodf$lat, type="p", col = scales::alpha(color, .85), pch)    
}


maputils.distance<- function(x1,y1,x2,y2)
{
  distance <- sqrt((x1-x2)^2+(y1-y2)^2);
}

maputils.haversince_dist <- function(lat1, lon1, lat2, lon2)
{
  dlon = lon2 - lon1 
  dlat = lat2 - lat1 
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
  c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
  d = 6373 * c #in km
}

maputils.diffSeconds <- function(t1, t2)
{
  dt = as.numeric(t2 - t1, units = "secs")
}

#maxdist<-0.01
maputils.extract <-function(geodf, maxdist)
{
  l<-list()
  i=1;
  n=nrow(geodf);
  count<-0
  while(i<n)
  {
    count<-count+1;
    j=i+1;
    while (j<n)
    {
      if (maputils.distance(geodf$lat[j],geodf$lon[j],geodf$lat[j+1],geodf$lon[j+1])>maxdist)
      #if (abs(geodf$lat[j]-geodf$lat[j+1])>0.01)
        {
        break;
      }
      j<-j+1;
    }
    #trajectory from i to j
    l[[count]]<-geodf[c(i:j),]
    i<-j+1;
  }
  maputils.extract <-l
}

library(fpc)
library(fossil)
maputils.extractStayPoints <-function(geodf, mindist)
{
  dist<- earth.dist(geodf, dist=T) #df is dataset containing lat long values
  dens<-dbscan(dist,MinPts=25,eps=0.43,method="dist")
}
