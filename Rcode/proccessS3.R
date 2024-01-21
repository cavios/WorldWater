
source('S3Functions.R')



datadir<-'testdata'
#path to shape file
myshape<- '~/projects/WorldWater/data/Tajikistan/mask/Tajikistan_2_simple.shp'
myfiles<-dir(datadir,full.names=TRUE,recursive=TRUE,pattern='standard_measurement')

mydat<-lapply(1:length(myfiles),function(i)doOneFileS3(myfiles[i],myshape))
mydat<-do.call(rbind,mydat)


datfiles<-dir('4610001882',recursive=TRUE,full.names=TRUE,pattern='_034_')


tsdat<-lapply(1:length(datfiles),function(i)one(datfiles[i]))
tsdat<-do.call(rbind,tsdat)
tsdat<-na.omit(tsdat)
write.csv(tsdat, file='lakedata_4610001882.csv',row.names=FALSE, quote=FALSE)


#how to generate time series


source('S3Functions.R')
datafile<-'lakedata_4610001882.csv'
dat<-read.csv(datafile)
#add track information to data
dat$track<-as.integer(as.factor(dat$time))
fit<-get.TS(dat)
plot(fit,addError=TRUE)
tsfile<-paste0('ts_',sub(".csv",".dat",datafile))
export.tsHydro(fit,tsfile)

#predict








plot(dat$time,dat$height) #notice that we have some outliers

plot(dat$lon,dat$lat)















