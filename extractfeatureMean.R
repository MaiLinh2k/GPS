


dfeatures <-NULL
tracklist<-unique(data1$track_id)
for(i in tracklist)
{
  df<-subset(data1, track_id==i)
  sp <-calSpeed(df)
  m1 <- max(sp)
  m2<- min(sp)
  m3<- mean(sp)
  m4<-quantile(sp, prob = c(0.75, 0.50, 0.25))
  dfeatures <- rbind(dfeatures,c(i,m1,m2,m3,m4))
}



