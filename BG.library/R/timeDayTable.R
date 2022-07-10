timeDayTable<-function(data, tcol, dcol, valueVar, 
                       sumFunc, naRemove = TRUE,
                       includeTotals = TRUE,
                       numberDays = NA, 
                       startDate = NA, endDate = NA,removeDates = NA,
                       filterCond = "",replaceNAs = FALSE,
                       startTime = "00:00",endTime = "23:00",
                       timeStep = "hour", period = 1,fromChange = TRUE,libraryPath){
  
  #get dateRange
  data<-fromChangeDateRange(data,fromChange,numberDays,startDate = startDate, 
                            endDate = endDate,removeDates = removeDates,libraryPath = libraryPath)
  
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  if(nrow(data)!=0){
  
    #get unique values
    NAMES<-c("dateTime",dcol,tcol,valueVar)
    data<-uniqueDateTime(data, NAMES, replaceNAs = replaceNAs,sumFunc = sumFunc,
                         startTime = startTime,endTime = endTime,timeStep = timeStep, period = period)
    
    
  subdata<-data

  subdata$time<-eval(parse(text = paste0("data$",tcol)))  
  subdata$time<-format(round(as.POSIXct(subdata$time,format="%H:%M"),"hours"),"%H:%M")
  subdata$date<-eval(parse(text = paste0("data$",dcol)))
  subdata$value<-eval(parse(text = paste0("data$",valueVar)))
  
  #format sumFunc
  if (naRemove){
   sumFuncEval<-paste0(sumFunc,"(x, na.rm = TRUE)") 
  }else{
    sumFuncEval<-paste0(sumFunc,"(x)")
  }
  
  
  #subdata<-dcast(subdata, time~date, value.var = "value", fun.aggregate = function(x) mean(x, na.rm = TRUE))
  subdata<-dcast(subdata, time~date, value.var = "value", 
                 fun.aggregate = function(x) eval(parse(text  = sumFuncEval)))
  
  #totals
  if (includeTotals){
  totalX<-matrix(sapply(subdata[-c(1)], function(x) as.numeric(eval(parse(text  = sumFuncEval)))),nrow = 1)
  totalX<-as.data.frame(totalX)  
  totalX<-cbind(data.frame(time = sumFunc),totalX)
  names(totalX)<-names(subdata)
  subdata<-rbind(subdata, totalX)
  
  totalY<-data.frame(matrix(apply(subdata[-c(1)],1, function(x) eval(parse(text  = sumFuncEval))),ncol = 1))
  names(totalY)<-sumFunc
  subdata<-cbind(subdata,totalY)
  }
  
  fixInf<-sapply(subdata[-c(1)], function(x) ifelse(is.infinite(x),NA,as.numeric(x)))
  subdata<-cbind(subdata[c(1)],fixInf)
  
  return(subdata)
  
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}