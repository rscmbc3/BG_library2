#'@title fromChangeDateRange
#'@description subset data according to date parameters. `fromChange = TRUE` overrides
#'all other date selection parameters and subsets data from the most recent pump change.  
#'`!is.na(numberDays)` will overrride `startDate` and `endDate` parameters and subset data
#'from max(data$Date2) to the date that is `numberDays` away from max(data$Date2).
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
#'@param libraryPath character string path to BG.library code 
#'@return `data` data.frame subsetted by date
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-fromChangeDateRange(data,fromChange = TRUE,numberDays = NA,
#'                          startDate = NA, endDate = NA, libraryPath)
#'range(data$Date2)

fromChangeDateRange<-function(data,fromChange,numberDays,startDate = NA, endDate = NA,
                              removeDates = NA, libraryPath){
  if (!fromChange){
    #set date range, numberDays overrules start/endDates
    if (!is.na(numberDays)){
      data<-data[data$Date2>=max(data$Date2,na.rm = TRUE)-numberDays+1,]
    }else if (!is.na(startDate) | !is.na(endDate)){
      if (!is.na(startDate)){
        startDate<-as.Date(startDate, format = "%Y-%m-%d" )
        data<-data[data$Date2>=startDate,]
      }
      if (!is.na(endDate)){
        endDate<-as.Date(endDate, format = "%Y-%m-%d" )
        data<-data[data$Date2<=endDate,]
      }
    }#end set date range 
    
  }else{
    basal<-makePumpSettings(libraryPath)$basal
    
    #get changes as dates
    allChanges<-character(0)
    class(allChanges)<-"Date"
    for (d in 2:length(basal)){
      change<-names(basal)[d]
      change<-gsub("X","",change)
      change<-gsub("\\.","-",change)
      change<-as.Date(change,format = "%m-%d-%Y",origin = "1970-01-01")
      change<-as.Date(change, format = "%Y-%m-%d" )
      allChanges<-c(allChanges,change)
    }
    
    #get max dates in data
    maxDate<-max(data$Date2, na.rm = TRUE)
    lastChange<-allChanges[allChanges<=maxDate]
    lastChange<-lastChange[length(lastChange)]
    
    #subset data
    data<-data[data$Date2>=lastChange,]

  }
  
  if (!is.na(removeDates)[1]){
    removeDates<-as.Date(removeDates, format = "%Y-%m-%d" )
    data<-data[!data$Date2 %in% removeDates,]
  }
  
return(data)
}