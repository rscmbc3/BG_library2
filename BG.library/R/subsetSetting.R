#'@title subsetData
#'@description subset settings showing only settings <= max(data$Date2) \\cr \\cr
#'@param data data.frame to be subsetted.
#'@param libraryPath character string path to BG.library code
#'@return `pumpSettings.list` named list of all pump settings
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'pumpSettings.list<-makePumpSettings(libraryPath)
#'print(names(pumpSettings.list$basal))
#'pumpSettings.list<-subsetSetting(data = data,libraryPath = libraryPath)
#'print(names(pumpSettings.list$basal))


subsetSetting<-function(data,libraryPath){
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  endDate<-max(data$Date2, na.rm = TRUE)#data has already been subsetted either by startEndDate or numberdays
  
   #get only relavant pump settings
for (s in c("basal","corrFactor","carbRatio")){
  setting<-eval(parse(text = s))
  settingDates<-names(setting)[2:length(setting)]
  settingDates<-gsub('X','',settingDates)
  settingDates<-gsub('\\.','-',settingDates)
  settingDates<-as.Date(settingDates,format = '%m-%d-%Y',origin = '1970-01-01')
  endSetting<-which(settingDates<endDate)
  if (length(endSetting)!=0){
    endSetting<-max(which(settingDates<endDate))
    setting<-setting[,1:(endSetting+1)]
  }else{
    setting<-setting[,1:2]
  }
  
  eval(parse(text = paste0(s,"<-setting")))
}
  
  pumpSettings.list<-named.list(basal, carbRatio, corrFactor)
  return(pumpSettings.list)
}