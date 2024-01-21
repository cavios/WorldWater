source('S3Functions.R')
datadir<-'testdata'
#path to shape file
myshape<- '~/projects/WorldWater/data/Tajikistan/mask/Tajikistan_2_simple.shp'
myfiles<-dir(datadir,full.names=TRUE,recursive=TRUE,pattern='standard_measurement')
mydat<-lapply(1:length(myfiles),function(i)doOneFileS3(myfiles[i],myshape))
mydat<-do.call(rbind,mydat)
Write.csv(mydat,file='mylake_data.csv',row.names=FALSE, quote=FALSE)
