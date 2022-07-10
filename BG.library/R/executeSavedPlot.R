#'@title executeSavedPlot
#'@description Execute a saved plot from BG.library/data/plotList \\cr \\cr
#'@param data data.frame to be plotted.
#'@param numberDays numeric value indicating number of days of data to include.  This parameter will 
#'override `startDate` and `endDate` unless it is set to NA.  The `fromChange` parameter will override 
#'all other parameters that subset the data by date.
#'@param changeParam.list list of parameters to change in saved plot.  Frequently changed parameters 
#'include `fromChange`, `startDate`, and `endDate`
#'@param libraryPath character string path to BG.library code 
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'executeSavedPlot(data, plotName = "lineSumSens_SGper_Sett_BG", libraryPath = libraryPath)


executeSavedPlot<-function(data, numberDays = NA, plotName,changeParam.list = NA, libraryPath){
  #load saved plotList
  load(paste0(libraryPath,"/data/plotList"))
  
  #make plotName top level in plotList
  eval(parse(text = paste0("plotList<-plotList$",plotName)))
  unPackList(lists = list(plotList = plotList),
             parentObj = list(NA))

  #change parameters
  if (!is.na(changeParam.list[1])){
    for (c in names(changeParam.list)){
      eval(parse(text = paste0("paramList$",c,"<-changeParam.list$",c)))
    }
  }
  
  #unpack params
  unPackList(lists = list(paramList = paramList),
             parentObj = list(NA))


  #format plot params
  paramStr<-character(0)
for (p in 1:length(paramList)){
  pName<-names(paramList)[p]
  pStr<-paste0(pName,"=",pName,",")
  paramStr<-paste0(paramStr,pStr)
}
  paramStr<-paste0(paramStr,"data = data,
                   numberDays = numberDays, 
                   libraryPath = libraryPath")

  #execute plot
  execStr<-paste0(plotType,"(",paramStr,")")
  eval(parse(text = execStr))

}