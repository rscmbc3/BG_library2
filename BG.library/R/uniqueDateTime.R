uniqueDateTime<-function(data, NAMES, replaceNAs, sumFunc = "mean",
                         startTime,endTime, timeStep, period){
  data<-data[,NAMES]
  
  if (replaceNAs){
      #replace NAs with zeros
  for (c in which(names(data)!="Date2")){
    data[,c]<-sapply(data[,c], function(x) as.numeric(ifelse(is.na(x),0,x)))
  }
  }

  dataReplaced<-data
  data<-data[,names(data)!="time2"]
  #set up summary function string
  if (sumFunc!="length"){
    #sumString<-paste0("as.data.frame(data %>% group_by(Date2,hours) %>% summarise_all(funs(",
    #                  sumFunc,"),na.rm = TRUE))")
    sumString<-paste0("as.data.frame(data[,!names(data) %in% c('Date2','hours')] %>% group_by(dateTime) %>% summarise_all(funs(",
                      sumFunc,"),na.rm = TRUE))")
  }else if (sumFunc=="length"){
    #sumString<-paste0("as.data.frame(data %>% group_by(Date2,hours) %>% summarise_all(funs(",
    #                  sumFunc,")))")
    sumString<-paste0("as.data.frame(data[,!names(data) %in% c('Date2','hours')] %>% group_by(dateTime) %>% summarise_all(funs(",
                      sumFunc,")))")
    
  }#end sumString
  
  #apply sumString
  data<-eval(parse(text = sumString))
  data$dateTime<-as.POSIXct(data$dateTime, origin = "1970-01-01")

  data<-prettyTime(data,"dateTime")
  #regenerate time2, hour, minute based on timeStep and period

  data<-setTimeStep(data,startTime,endTime, timeStep, period)
  
  data$hours<- data$hour + data$minute/60 
  data<-data[,NAMES]
  assign("uniqueData",data,envir = .GlobalEnv)
  data<-data[,names(data)!="dateTime"]
  
  
  return(data)
}