#'@title makePumpSettings
#'@description Function to create a continuous time series from pump settings csvs  \\cr \\cr
#'@param libraryPath character string path to BG.library code 
#'@returns `pumpSettings.list` list of all pump settings ('basal','carbRatio','corrFactor') 
#'of pump settings in continuous time series
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'pumpSettings.list<-makePumpSettings(libraryPath)

makePumpSettings<-function(libraryPath){
  #set up time sequence
  allTime<-seq.POSIXt(as.POSIXct("00:00",format="%H:%M"),as.POSIXct("23:00",format="%H:%M"),by = "1 hour")
  allTime59<-allTime+59*60
  allTime<-c(allTime,allTime59)
  allTime<-allTime[order(allTime)]
  allTime<-data.frame(time = allTime)
  
  #import settings
  for (s in c("basal","carbRatio","corrFactor")){
    setting<-read.csv(paste0(libraryPath,"/pumpSettings/",s,".csv"))
    setting$time<-as.POSIXct(setting$time,format="%H:%M")
    time<-setting$time
    
    
    #get allTimes
    settingLong<-merge(allTime,setting, by  = "time", all.x = TRUE)
    
    
    #populate rates
    for (c in 2:length(setting)){  
      
      rate<-na.omit(setting[,c])
      for (r in 1:length(rate)){
        if (r==1){
          allRate<-numeric(0)
        }else{
          whichTime<-which(allTime[[1]]<as.POSIXct(time[r] ,format="%H:%M") & allTime[[1]]>=as.POSIXct(time[r-1] ,format="%H:%M"))
          allRate<-c(allRate,rep(rate[r-1],length(whichTime)))
        }
        
      }#end for
      
      #add to settings with allTimes
      settingLong[,c]<-c(allRate,rate[length(rate)])
    }
    
    eval(parse(text = paste0(s,"<-settingLong")))
    
  }
  
  pumpSettings.list<-named.list(basal,carbRatio,corrFactor)
  return(pumpSettings.list)
  
  
}