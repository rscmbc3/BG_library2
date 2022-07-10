#'@title prettyTime
#'@description Formats date and time columns for processing from a "POSIXct" "POSIXt" 
#'class dateTime column \\cr \\cr
#'@return data.frame with new formatted date and time columns
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-prettyTime(data,"dateTime")

prettyTime<-function(data,timeCol){
  tempTime<-eval(parse(text = paste0("data$",timeCol)))
  data$Date2<-as.Date(tempTime, format = "%Y-%m-%d" )
  data$time2<-format(tempTime, "%H:%M")
  data$hour<-hour(tempTime)
  data$minute<-minute(tempTime)
  return(data)
}