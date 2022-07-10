#'@title dataImport
#'@description import csv file and create allData and joinData objects.  
#'allData is pump and sensor data rbinded
#'joinData is a roll join of pump and sensor data on datetime.\\cr \\cr
#'@param BGfilePath character path to csv import file
#'@param libraryPath character string path to BG.library code
#'@return `dataImport.list` list containing 2 data.frames (allData and joinData)
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'BGfilePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(BGfilePath,libraryPath)

dataImport<-function(BGfilePath, SGfilePath, libraryPath){
  #readBG
  BGdata<-read.csv(BGfilePath, header = FALSE)
  
  #get BG table
  BGStartRow<-which(BGdata[,1]=="Index")[1]
  BGEndRow<-max(which(as.numeric(rownames(BGdata))>BGStartRow))
  BGdata<-BGdata[BGStartRow:BGEndRow,]
 
  assign("BGdata",BGdata,envir = .GlobalEnv)
  
  #fix tables
  BGdata<-fixtables(BGdata,libraryPath,formatDT=FALSE)

  #remove extra columns
  BGdata<-BGdata[c(4,49)]
  
  #change names
  names(BGdata)[1]<-"BGvalue"
  
  #import SG data
  SGdata<-read.csv(SGfilePath, header = FALSE)
  
  #get SG table
  SGStartRow<-which(SGdata[,3]=="EGV")[1]
  SGEndRow<-max(which(as.numeric(rownames(SGdata))>SGStartRow))
  SGdata<-SGdata[c(1,SGStartRow:SGEndRow),]
  
  assign("SGdata",SGdata,envir = .GlobalEnv)
  
  #fix tables
  SGdata<-fixtables(SGdata,libraryPath,formatDT=TRUE)
  
  #remove extra columns
  SGdata<-SGdata[c(7,11,16)]
  
  #change  names
  names(SGdata)[1]<-"SGvalue"
  
  #find smallest difference in time and join
  sensorTable<-as.data.table(SGdata)
  BGTable<-as.data.table(BGdata)
  sensorTable$dateTime2<-sensorTable$dateTime
  
  setkey(BGTable, dateTime)
  setkey(sensorTable, dateTime)

  joinData<-BGTable[sensorTable, roll = T]
  
  joinData<-as.data.frame(joinData)
  
  #create allData
  allData<-rbind.fill(BGdata,SGdata)
  allData<-allData[order(allData$dateTime),]
  
  #make extra time date columns in joinData for plotting
  joinData<-prettyTime(joinData,"dateTime2")
  allData<-prettyTime(allData,"dateTime")
  
  #remove dup BGs
  joinData<-removeDupBGs(joinData)
  allData<-removeDupBGs(allData)

  
  dataImport.list<-named.list(allData,joinData)
  return(dataImport.list)
}