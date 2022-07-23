
dfeatures <-NULL
tracklist<-unique(data1$track_id)
for(i in tracklist)
{
  df<-subset(data1, track_id==i)
  sp <-calSpeed(df)
  sp <- sp[!is.na(sp)]
  sp <- sp[sp<=100]
  if (length(sp)==0)
    sp = c(0)
  t<-hist(sp, breaks = c(0,5,10,15,20,25,30,35,40,1000), plot = FALSE)
  dfeatures<-rbind(dfeatures, c(t$density))
}
