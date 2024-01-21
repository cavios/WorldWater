
source('S3Functions.R')
datafile<-'lakedata_4610001882.csv'
dat<-read.csv(datafile)
#add track information to data
dat$track<-as.integer(as.factor(dat$time))
fit<-get.TS(dat)
plot(fit,addError=TRUE)
tsfile<-paste0('ts_',sub(".csv",".dat",datafile))
export.tsHydro(fit,tsfile)
