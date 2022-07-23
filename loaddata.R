calSpeed <- function(df) #ham tinh speed
{
  lat1 = df$latitude[1:length(df$latitude)-1]
  lat2 = df$longitude[2:length(df$latitude)-1]
  long1 = df$latitude[1:length(df$longitude)-1]
  long2 = df$longitude[2:length(df$longitude)-1]
  dS = abs(long1-long2)
  dS = abs(lat1-lat2)
  t = df$time
  t2 = strptime(t, format = "%Y-%m-%d %H:%M:%S")
  t3 = t2[2:length(t)]
  t4 = t2[1: length(t)-1]
  dt = as.numeric(t3-t4)
  speed <- (sqrt((lat1-lat2)^2 + (long1-long2)^2))/dt
  return(speed)
}

data1 <- read.csv('data/go_track_trackspoints.csv', header = T) # doc du lieu  
data2 <- read.csv('data/go_track_tracks.csv', header = T) # doc du lieu 

