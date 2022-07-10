#'@title summarizeData
#'@description summarize data column (`colName`) by `sumFunc` and output data.frame \\cr \\cr
#'@param data data.frame to be summarized
#'@param colName character string name of column to summarize
#'@param sumFuncs character vector of aggregate functions to apply to `colName`
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
#'@return `data` data.frame of `colName` summarized by time and date
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'BGvalue_Summary<-summarizeData(data, colName = "BG.Reading..mg.dL.", 
#'                               libraryPath = libraryPath)

summarizeData<-function(data, colName, sumFuncs = "min, mean, max, sd", 
                        fromChange=TRUE, numberDays = NA, startDate = NA, 
                        endDate = NA,removeDates = NA,
                        startTime = "00:00", endTime = "23:00",timeStep = "hour", period = 1, 
                        filterCond = "",libraryPath){
  #get dateRange
  data<-fromChangeDateRange(data,fromChange,numberDays,libraryPath = libraryPath,
                            startDate = startDate,endDate = endDate,removeDates = removeDates)
  
  
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  if(nrow(data)!=0){
  #get unique values
  NAMES<-c("dateTime","Date2","time2","hour",colName)
  data<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,startTime = startTime,endTime = endTime,timeStep = timeStep, period = period)
  
  #set time series column to aggregate on
  if (timeStep=="hour"){
     data<-data[c("time2",colName)]
  data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
  }else if (timeStep=="day"){
    data$time3<-data$Date2
  }

  #get only time and colName columns
data<-data[c("time3",colName)] 

  #add remove NAs to sumFuncs call
  if (sumFuncs!="length"){
    sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                      sumFuncs,"),na.rm = TRUE))")
  }else{#length
    sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                      sumFuncs,")))")
    
  } 

#summarize data
  data<-eval(parse(text = sumString))
  
  #format output data timestep column
  if (timeStep=="hour"){
  data$time3<-format(data$time3, "%H:%M")
  }else if (timeStep=="day"){
  names(data)[names(data)=="time3"]<-"Date2"
  data$Date2<-as.Date(data$Date2, format = "%Y-%m-%d")
  }
  
  return(data)
  
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}