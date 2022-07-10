#'@title convertClass
#'@description Converts column classes to match  \\cr \\cr
#'@param cls character string for the desired class conversion
#'@param col charcter string for the column to convert
#'@param sensorData data.frame of sensor BG values
#'@param pumpData data.frame of pumpData
#'@return `convert` vector of converted values with class equal to `cls`
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'#readBG
#'data<-read.csv(filePath, header = FALSE)
#'
#'#get pump table
#'pumpStartRow<-which(data[,1]=="Index")[1]
#'pumpEndRow<-which(data[,1]=="-------" & as.numeric(rownames(data))>pumpStartRow)-1
#'pumpData<-data[pumpStartRow:pumpEndRow,]
#'
#'#get sensor table
#'sensorStartRow<-which(data[,1]=="Index")[2]
#'sensorEndRow<-nrow(data)
#'sensorData<-data[sensorStartRow:sensorEndRow,]
#'
#'#fix tables
#'pumpData<-fixtables(pumpData,libraryPath)
#'sensorData<-fixtables(sensorData,libraryPath)
#'
#'#conform colClasses
#'classTable<-cbind(names(pumpData), sapply(pumpData, class))
#'for (c in 1:(nrow(classTable)-1)){
#'  cls<-as.character(classTable[[c,2]])
#'  col<-as.character(classTable[[c,1]])
#'  sensorData[[c]]<-convertClass(cls,col,sensorData,pumpData)
#'}
#'print(cbind(classTable, sapply(sensorData, class)))

convertClass<-function(cls,col,sensorData,pumpData){
  if (cls=="factor"){
    convert<-eval(parse(text=paste("factor(sensorData$`",col,"`, levels = as.character(levels(pumpData$`",col,"`)))",sep="")))
  }else if (cls=="character"){
    convert<-eval(parse(text=paste("as.character(sensorData$`",col,"`)",sep="")))
  }else if (cls=="integer"){
    convert<-eval(parse(text=paste("as.integer(as.character(sensorData$`",col,"`))",sep="")))
  }else if (cls=="numeric"){
    convert<-eval(parse(text=paste("as.numeric(as.character(sensorData$`",col,"`))",sep="")))
  }else if (cls=="logical"){
    convert<-eval(parse(text=paste("ifelse(as.character(sensorData$`",col,"`),FALSE,TRUE)",sep="")))
  }else if (cls=="Date"){
    convert<-eval(parse(text=paste("as.Date(as.character(sensorData$`",col,"`),format='%Y-%m-%d')",sep="")))
  }
  return(convert)
}