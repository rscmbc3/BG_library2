#'@title historySeqOut
#'@description Execute series of historical plots through time sequence and output html 
#'document with interactive plots\\cr \\cr
#'@param data data.frame to be plotted
#'@param plotName character string name of saved plot to be executed.   If `plotName = NA` plot to execute is
#'not a saved plot.
#'@param paramList list of plot parameters needed to execute plot.  If executing a saved
#'plot set to `NA`
#'@param plotType character string indicating type of plot to execute (i.e. 'plotLine_ly', 
#''summaryPlot_ly' or 'heatmMap_ly')
#'@param seqType character string indicating the type of historical sequence to output 
#'('change' indicating plots at dates where pump settings where changed or 'days' for plots
#'every `period` number of days)
#'@param seqLength number of plots to output
#'@param period number of days per plot if `seqType = 'days'`
#'@param removeDates character vector of dates in format %Y-%m-%d to remove from data
#'@param reportTitle character string title for html document
#'@param outPath character string directory in which to output html document, if `outPath<-NA`
#'temporary html document will be stored in ./BG.library/reports/historySeqOut.html
#'@param outFileName character string file name if `outFileName<-NA`
#'temporary html document will be stored in ./BG.library/reports/historySeqOut.html
#'@param libraryPath character string path to BG.library code 
#'@param filePath character path to csv import file
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'historySeqOut(plotName = "lineSumSens_SGper_Sett_BG",
#'              reportTitle = "Compare Summary Sensor Line Plot Since Last Pump Setting Change" ,
#'              libraryPath = libraryPath, filePath = filePath)

historySeqOut<-function(data = NA,plotName = NA, paramList = NA, plotType = NA,
                        seqType = "change", seqLength = 2, period = NA,removeDates = NA,
                        reportTitle = "",outPath = NA, outFileName= NA,
                        libraryPath,BGfilePath, SGfilePath) {
  
  
  replaceTitle(libraryPath, reportTitle,reportName = "historySeqOut")
  
  reportPath<-paste0(libraryPath,"reports/historySeqOut.Rmd")
if (is.na(outPath) | is.na(outFileName) | outPath=="" | outFileName == ""){
  outFileName<-gsub("\\.Rmd","\\.html",reportPath)
}else{
  outFileName<-paste0(outPath,"/",outFileName,".html")
}
  
  rmarkdown::render(
    reportPath, params = list(
      libraryPath = libraryPath,
      BGfilePath = BGfilePath,
      SGfilePath = SGfilePath,
      plotName = plotName,
      paramList = paramList,
      plotType = plotType,
      seqType = seqType,
      seqLength = seqLength,
      period = period,
      removeDates = removeDates
    ),
    output_file = outFileName
  )
  
  shell.exec(outFileName)
}



