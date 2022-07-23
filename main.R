setwd("~/Documents/R-Codes/extractTrafficFlow1") # chuyen den thu muc R-Codes chua source code
source("loaddata.R")
source("extractfeatureHist_Tool.R")
print('Co dung update')
#tinh so luong id trong mot subset
nrows = dim(data1)[1] %/% 10
s_time <-Sys.time()
data11 <- data1[0:nrows,]
total_features <- extractHist(data11)
total_ids <-  unique(data11$track_id)
dens1<-kmeans(dfeatures, 2)

for(i in (1:9)) #duyet 10 subset
{ 
  
  id_from = i*nrows
  id_to = (i+1)*nrows
  data11 <- data1[id_from:id_to,]
  #data11 chua phan thu i cua data1
  #tinh feature
  dfeatures <- extractHist(data11)
  dids <-unique(data11$track_id)
  #ket hop voi total_features
  #tach cac id chung ra de cong hist
  commonids <- intersect(total_ids,dids)
  comfeature1 <- total_features[match(commonids,total_ids),]
  comfeature2 <- dfeatures[match(commonids,dids),]
  comfeature <- (comfeature1 + comfeature2)/2
  feature1 <-total_features[-match(commonids,total_ids),]
  feature2 <- dfeatures[-match(commonids,dids),]
  total_features <- rbind(feature1,comfeature,feature2)
  total_ids <- union(total_ids,dids)
  dens1<-kmeans(total_features, 2)
  print(Sys.time() - s_time)
}
 

######################
print('Khong dung update')
#Khong dung update
#tinh so luong id trong mot subset
nrows = dim(data1)[1] %/% 10
s_time <-Sys.time()
data11 <- data1[0:nrows,]
total_features <- extractHist(data11)
total_ids <-  unique(data11$track_id)
dens1<-kmeans(dfeatures, 2)

for(i in (1:9)) #duyet 10 subset
{
  id_to = (i+1)*nrows
  data11 <- data1[0:id_to,]
  #data11 chua phanthu i cua data1
  #tinh feature
  dfeatures <- extractHist(data11)
  total_features <-dfeatures
  total_ids <-  unique(data11$track_id)
  dens1<-kmeans(total_features, 2)
  print(Sys.time() - s_time)
}


#############Output###############
#source('output.R')

