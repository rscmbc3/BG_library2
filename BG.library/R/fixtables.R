#'@title fixtables
#'@description Reformat input tables to create readable data.frame objects. Format dateTime columns as.POSIXct.  \\cr \\cr
#'@param data object from `readLines()` import to convert to readable data.frame.
#'@return `data` data.frame with correct headers and dateTime class columns.
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
#'#fix tables
#'pumpData<-fixtables(pumpData,libraryPath)


fixtables<-function(data,libraryPath,formatDT){
  #replace column names
  for (i in 1:length(data)){
    names(data)[i]<-as.character(data[1,i])
  }
  data<-data[-c(1),]
  
  #write to csv and then read to get colclasses
  tempfile = paste0(libraryPath,"data/temp.csv")
  write.csv(file = tempfile,data, row.names = FALSE)
  
  data<-read.csv(tempfile)
  
  #remove index
  data<-data[,-c(1)]
  #convert date/times
  data<-data[as.character(data[,1])!="",]
  dataOrig<-data
  
  if(!formatDT){
  data$Date<-format(as.POSIXct(as.character(data[,1]),tx = "EDT", origin = "1899-12-30", format = "%m/%d/%Y"),"%Y-%m-%d")
  data$Time<-format(as.POSIXlt(as.character(data[,2]),format="%H:%M"), format="%H:%M")
  
  tryIt<-try({
data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"))
  }, silent =TRUE)

  if (class(tryIt)[1]=="try-error"){
    data$Date<-format(as.POSIXct(as.character(dataOrig[,1]),tx = "EDT", origin = "1899-12-30"),"%Y-%m-%d")
    data$Time<-format(as.POSIXlt(as.character(dataOrig[,2]),format="%H:%M"), format="%H:%M")
    #data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"))
    data$dateTimeStr<-paste(data$Date, data$Time,"EDT")
    dateTime<-lapply(data$dateTimeStr,function(x) as.POSIXct(x, origin = "1899-12-30",tx = "EDT"))
    dateTime2<-unlist(dateTime)
    attributes(dateTime2)<-attributes(dateTime[[1]])
    data$dateTime<-dateTime2
    data<-data[,names(data)!="dateTimeStr"]
  }else{
    data$Date<-format(as.POSIXct(as.character(dataOrig[,1]),tx = "EDT", origin = "1899-12-30", format = "%m/%d/%Y"),"%Y-%m-%d")
    data$Time<-format(as.POSIXlt(as.character(dataOrig[,2]),format="%H:%M"), format="%H:%M")
    data$dateTimeStr<-paste(data$Date, data$Time,"EDT")
    #data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"),format="%Y-%m-%dT%H:%M:%S",tz=Sys.timezone())
    dateTime<-lapply(data$dateTimeStr,function(x) as.POSIXct(x, origin = "1899-12-30",tx = "EDT"))
    dateTime2<-unlist(dateTime)
    attributes(dateTime2)<-attributes(dateTime[[1]])
    data$dateTime<-dateTime2
data<-data[,names(data)!="dateTimeStr"]
    
  }
  }else{#DT exists, but not formated
    data$Date<-sapply(data$Timestamp..YYYY.MM.DDThh.mm.ss., function(x) strsplit(as.character(x),"T")[[1]][1])
    data$Time<-sapply(data$Timestamp..YYYY.MM.DDThh.mm.ss., function(x) strsplit(as.character(x),"T")[[1]][2])
    data$dateTime<-as.POSIXct(paste(data$Date,data$Time),format = "%Y-%m-%d %H:%M:%S")
  
}
  return(data)
}