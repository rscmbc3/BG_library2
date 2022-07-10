#'@title setTimeStep
#'@description Reformat dateTime columns creating new columns with custom `timeStep`
#'and `period`  \\cr \\cr
#'@param data data.frame with dateTime, time2, Date2 columns
#'@param startTime character string of beginning time for plot (typically startTime = "00:00)
#'@param endTime character string of ending time for plot (typically endTime = "23:00)
#'@param timeStep character string indicating the time step to aggregate data, possible values
#'include c("hour","day")
#'@param period numeric value indicating number of `timeSteps` to aggregate into single step
#'for example : `timeStep = 'hour'`  and `period = 3` outputs plots with tick marks every 3 hours.
#'@return `data` data.frame with new aggregated timeSteps
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'unique(data$hour)
#'data<-setTimeStep(data,timeStep = "hour", period = 3)
#'unique(data$hour)

setTimeStep<-function(data,startTime = "00:00",endTime = "23:00", timeStep, period){
  #regenerate time2, hour, minute based on timeStep and period
  if (timeStep=="hour" & period!=1){
    seqTime<-seq.POSIXt(as.POSIXct("00:00",format="%H:%M"),as.POSIXct("23:00",format="%H:%M"),by = paste0(period," hour"))

    #set time range as decimal hours
    startTime<-as.POSIXlt(startTime,format="%H:%M", origin = "1970-01-01")
    startTime<- startTime$hour + startTime$min/60 
    endTime<-as.POSIXlt(endTime,format="%H:%M",origin = "1970-01-01")
    endTime<- endTime$hour + endTime$min/60
    #subset seqTime
    xticksRange<-as.POSIXlt(seqTime,format="%H:%M",origin = "1970-01-01")
    xticksRange<- xticksRange$hour + xticksRange$min/60
    seqTime<-seqTime[xticksRange>=startTime & xticksRange<=endTime]
    
    #create new time2 column
    data$timeNew<-as.POSIXct(data$time2,format="%H:%M")
    data$timeNew2<-as.numeric(rep(NA,nrow(data)))
    for (s in 2:length(seqTime)){
      if (s==1){
        data<-transform(data, timeNew2 = ifelse(is.na(data$timeNew2) & timeNew<=seqTime[s],seqTime[s],timeNew2))
      }else{
        data<-transform(data, timeNew2 = ifelse(is.na(data$timeNew2) & timeNew<seqTime[s] & timeNew>=seqTime[s-1],seqTime[s-1],timeNew2))
      }
    }
    #add last in sequence
    data<-transform(data, timeNew2 = ifelse(is.na(data$timeNew2) & timeNew>=seqTime[length(seqTime)],seqTime[length(seqTime)],timeNew2))
    #replace core columns
    data$time2<-as.POSIXlt(data$timeNew2, origin = "1970-01-01")
    data$hour<-data$time2$hour
    data$minute<-minute(data$time2)
    data$time2<-format(data$time2,"%H:%M")
    data<-data[,regexpr("timeNew",names(data))<0]
  }else if (timeStep=="day"){
    seqDate<-seq.Date(from = min(data$Date2, na.rm = TRUE),to = max(data$Date2, na.rm = TRUE),by=paste0(period," day"))
    data$dateNew<-data$Date2
    data$dateNew2<-as.Date(rep(NA,nrow(data)),origin = "1970-01-01")
    for (s in 2:length(seqDate)){
      if (s==1){
        data<-transform(data, dateNew2 = ifelse(is.na(data$dateNew2) & dateNew<=seqDate[s],seqDate[s],dateNew2))
      }else{
        data<-transform(data, dateNew2 = ifelse(is.na(data$dateNew2) & dateNew<seqDate[s] & dateNew>=seqDate[s-1],seqDate[s-1],dateNew2))
      }
    }
    #add last in sequence
    data<-transform(data, dateNew2 = ifelse(is.na(data$dateNew2) & dateNew>=seqDate[length(seqDate)],seqDate[length(seqDate)],dateNew2))

    #replace core columns
    data$Date2<-as.Date(data$dateNew2, format = "%Y-%m-%d", origin = "1970-01-01")
    data<-data[,regexpr("dateNew",names(data))<0]
  }#end if timestep date
  return(data)
}