#'@title subsetData
#'@description subset data according to date and time parameters taking into account time step. 
#'`fromChange = TRUE` overrides all other date selection parameters and subsets data from the 
#'most recent pump change.  `!is.na(numberDays)` will overrride `startDate` and `endDate` parameters 
#'and subset data from max(data$Date2) to the date that is `numberDays` away from max(data$Date2).
#'`startDate` and `endDate` are only applied if `fromChange` and `numberDays` are not.  Data
#'is subsetted with `min(data$Date2>=startDate)` and `max(data$Date2)<=endDate` \\cr \\cr
#'@param data data.frame to be subsetted.
#'@param fromChange TRUE/FALSE indicates whether data should be subset with the ealiest date
#'as the most recent pump settings change.  This setting overrides all other date subsetting 
#'parameters, and must be set to `FALSE` to apply other parameter settings (i.e. `numberDays`,
#'`startDate`, and `endDate`)
#'@param numberDays numeric value indicating number of days of data to include.  This parameter will 
#'override `startDate` and `endDate` unless it is set to NA.  The `fromChange` parameter will override 
#'all other parameters that subset the data by date.
#'@param startDate Earliest date included in data.  This setting will only be applied 
#'if `numberDays = NA` and `fromChange = FALSE`
#'@param endDate Latest date included in data.  This setting will only be applied 
#'if `numberDays = NA` and `fromChange = FALSE`
#'@param removeDates character vector of dates in format %Y-%m-%d to remove from data
#'@param startTime character string of beginning time for plot (typically startTime = "00:00)
#'@param endTime character string of ending time for plot (typically endTime = "23:00)
#'@param timeStep character string indicating the time step to aggregate data, possible values
#'include c("hour","day")
#'@param period numeric value indicating number of `timeSteps` to aggregate into single step
#'for example : `timeStep = 'hour'`  and `period = 3` outputs plots with tick marks every 3 hours.
#'@param filterCond character string of R syntax to be applied to filter the data, 
#'example `data[data$BG.Reading..mg.dL.>150 & !is.na(data$BG.Reading..mg.dL.),]`
#'@param libraryPath character string path to BG.library code 
#'@return `data` data.frame subsetted by date
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'range(data$Date2)
#'data<-subsetData(data = data,libraryPath = libraryPath)
#'range(data$Date2)

subsetData<-function(data,fromChange = TRUE,numberDays = NA,
                     startDate = NA,endDate = NA,removeDates = NA,
                     startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
                      filterCond = "",libraryPath){
  
  #subset by date
  data<-fromChangeDateRange(data,fromChange = fromChange,numberDays = numberDays,
                            startDate = startDate, endDate = endDate,removeDates = removeDates,
                            libraryPath = libraryPath)
    
  
  #apply filter condition
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  
   #set time range as decimal hours
  startTimeOrig<-startTime
  endTimeOrig<-endTime
  startTime<-as.POSIXlt(startTime,format="%H:%M")
  startTime<- startTime$hour + startTime$min/60 
  endTime<-as.POSIXlt(endTime,format="%H:%M")
  endTime<- endTime$hour + endTime$min/60
  data$hours<- data$hour + data$minute/60 
  data<-data[which(data$hour>=startTime & data$hour<=endTime),]
  
  #regenerate new time/date columns based on timeStep and period
  data<-setTimeStep(data,startTime = startTimeOrig,endTime= endTimeOrig, timeStep, period)

data<-data[order(data$dateTime),]
  
  return(data)
}