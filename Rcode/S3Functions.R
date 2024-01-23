library(lubridate)
library(date)
library(hdf5r)                                        #
library(terra)
library(tsHydro)

readS3<-function(nc,tofile=FALSE, discardNA=TRUE){
    mydat <- h5file(nc,'r')
    lon <- mydat[["lon_20_ku"]]
    lon <- lon[]*h5attr(lon, "scale_factor")
    lon<-ifelse(lon>180,lon-360,lon)
    lat <- mydat[["lat_20_ku"]]
    lat<-lat[]*h5attr(lat, "scale_factor")
    time<-mydat[["time_20_ku"]][] # time since 2000 0101
    lat01<-mydat[["lat_01"]] # 1 Hz latitude
    lat01 <- lat01[]*h5attr(lat01, "scale_factor")
    lon01<-mydat[["lon_01"]] # 1 Hz longitude
    lon01 <- lon01[]*h5attr(lon01, "scale_factor")
    lon01<-ifelse(lon01>180,lon01-360,lon01)
    altitude<-mydat[["alt_20_ku"]]# satellite height above reference ellipsoid
    FV<-h5attr(altitude,"_FillValue")
    altitude<-ifelse(altitude[]==FV,NA, altitude[])*1e-4
    #trange<-mydat["/tracker_range_20_ku"]
    #trange<-ifelse(trange[]==FV,NA, trange[])*1e-4
    ocog<-mydat[["range_ocog_20_ku"]] # Range calculated from the OCOG retracker
    ocog<-ifelse(ocog[]==FV,NA, ocog[])*1e-4
    #atm corr
    wet<-mydat[["mod_wet_tropo_cor_meas_altitude_01"]] # wet troposphere correction
    FV2<-h5attr(wet,"_FillValue")
    wet<-ifelse(wet[]==FV2,NA, wet[])*1e-4
    dry<-mydat[["mod_dry_tropo_cor_meas_altitude_01"]] # dry troposphere correction
    dry<-ifelse(dry[]==FV2,NA, dry[])*1e-4
    ionoGim<-mydat[["iono_cor_gim_01_ku"]] # Gim model ionosphere correction
    ionoGim<-ifelse(ionoGim[]==FV2,NA, ionoGim[])*1e-4
    #tide corr
    poleTide<-mydat[["pole_tide_01"]] # pole tide correction
    poleTide<-ifelse(poleTide[]==FV2,NA, poleTide[])*1e-4
        
        
    LT2<-mydat[["load_tide_sol2_01"]]
    LT2<-ifelse(LT2[]==FV2,NA, LT2[])*1e-4
    solidET<-mydat[["solid_earth_tide_01"]] # solid earth tide correction
    solidET<-ifelse(solidET[]==FV2,NA, solidET[])*1e-4
    #Other
    geoid<-mydat[["geoid_01"]] # geoid model heights
    geoid<-ifelse(geoid[]==FV,NA, geoid[])*1e-4
        
    h5close(mydat) 
     # interpolation of 1 Hz coorections to 20 Hz
    wet20<-approx(lat01,wet,lat)$y
    dry20<-approx(lat01,dry,lat)$y
    ionoGim20<-approx(lat01,ionoGim,lat)$y
    poleTide20<-approx(lat01,poleTide,lat)$y
    solidET20<-approx(lat01,solidET,lat)$y
    LT220<-approx(lat01,LT2,lat)$y
    geoid20<-approx(lat01,geoid,lat)$y


    #construct elevation/water level

    geocorr<-wet20+dry20+ionoGim20+poleTide20+solidET20
    hocog<-altitude-(ocog+geocorr)-geoid20


    decYear<-JulSecToDY(time)
    decYear<-signif(decYear,digits=7)
    nr<-length(time)
    cycle<-rep(substr(basename(dirname(nc)),70,72),nr)
    track<-rep(substr(basename(dirname(nc)),74,76),nr)
    out<-data.frame(timesec=time,time=decYear,cycle=cycle,sattrack=track,lat=lat,lon=lon,height=hocog,geoid=geoid20)
    if(discardNA)out<-na.omit(out)
    if(tofile){
        ofile<-paste0(basename(dirname(nc)),'.dat')
        write.table(out,file=ofile,row.names=FALSE, quote=FALSE)
    }
    
    return(out)    
        
    
}

getInShape<-function(dat,myshape,colid=1,doplot=FALSE){
   #read shape
    s <- vect(myshape)
    pts <- vect(cbind(dat$lon,dat$lat), crs="+proj=longlat")
    e <- extract(s,pts) 
    id<-e[!is.na(e[,2]), 1]
    lakeid<-e[!is.na(e[,2]),colid+1] #
    # data inside shape
    datin<-cbind(dat[id,],lakeid)
    if(doplot){
        ptsin <- vect(cbind(datin$lon,datin$lat), crs="+proj=longlat")
        plot(s)
        points(pts,col='blue',pch='.',cex=2)
        points(ptsin,col='red',pch='.',cex=2)
    }
    return(datin)
}


doOneFileS3<-function(nc,myshape,discardNA=TRUE, tofile=FALSE, colid=1,doplot=FALSE ){
   dat<-readS3(nc,discardNA=discardNA,tofile=tofile)
   datin<-getInShape(dat,myshape,colid=colid,doplot=doplot)
   return(datin)
}


oneLake<-function(datfiles){
    data<-read.table(datfiles)
    out<-data.frame(timesec=data[,3],time=data[,4],cycle=data[,1],sattrack=data[,2],lat=data[,5],lon=data[,6],height=data[,8],geoid=data[,12],lakeid=data[,26])
    return(out)
}


JulSecToDY<-function(time){
    mydate<-as.Date(time/86400, origin="2000-01-01")
    decimalDate<-decimal_date(mydate)
    return(decimalDate)
}

myapprox<-function(x,y,newx){
    if(sum(!is.na(y)) > 1){
        return(approx(x,y,newx))
    }else{
        return(list(y=rep(NA,length(newx))))
    }
}
