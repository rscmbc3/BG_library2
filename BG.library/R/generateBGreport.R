#'@title generateBGreport
#'@description Generate BG_report in html format for specific date range.  BG_report
#'includes commonly used tables and interactive plots.\\cr \\cr
#'@param libraryPath character string path to BG.library code 
#'@param filePath character path to csv import file
#'@param outPath character string indicating the directory in which to save the 
#'BG_report.  If not specified report will be saved to "./BG.library/reports/BG_report.html
#'@param outFileName character string to apply to BG_report file name.  If not specified
#'report will be saved to "./BG.library/reports/BG_report.html
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
#'@param data data.frame to be used to generate tables and plots
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'generateBGreport(libraryPath, filePath, data = data)

generateBGreport<-function(libraryPath, BGfilePath, SGfilePath,
                           outPath = NA, outFileName= NA,
                           fromChange = TRUE,  numberDays = NA, 
                           startDate = NA, endDate = NA,removeDates = NA,
                           data){
  #get dateRange
  data<-fromChangeDateRange(data,fromChange,numberDays,libraryPath = libraryPath,
                            startDate = startDate,endDate = endDate, removeDates = removeDates)
  
  reportTitle<-paste0("BG_report for Dates: ",min(data$Date2, na.rm = TRUE)," to ",max(data$Date2, na.rm = TRUE))
  
  replaceTitle(libraryPath, reportTitle,reportName = "BG_report")
  
  reportPath<-paste0(libraryPath,"reports/BG_report.Rmd")
  if (is.na(outPath) | is.na(outFileName)){
    outFileName<-gsub("\\.Rmd","\\.html",reportPath)
  }else{
    outFileName<-paste0(outPath,"/",outFileName,".html")
  }
  
  rmarkdown::render(
    reportPath, params = list(
      libraryPath = libraryPath,
      BGfilePath = BGfilePath,
      SGfilePath = SGfilePath,
      numberDays = numberDays,
      fromChange = fromChange,
      startDate = startDate,
      endDate = endDate,
      removeDates = removeDates
    ),
    output_file = outFileName
  )
  
  shell.exec(outFileName)
  
  }