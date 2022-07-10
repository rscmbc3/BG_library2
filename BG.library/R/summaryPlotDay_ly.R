summaryPlotDay_ly<-function(data,addBarSub,boxBar,
                         numberDays, filterCond = "",
                         startDate = NA, endDate = NA,removeDates = NA,
                         startTime = "00:00", endTime = "23:00",
                         timeStep = "day",period = 1,fromChange,libraryPaths,
                         plotSummary, sumFunc = "length", stackedBar = "",
                         uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = FALSE,addGoodRange = FALSE,
                         legendInset = -0.2,description,descInset){
  
  #subset data by date and filterCond
  data<-subsetData(data,numberDays = numberDays,startDate = startDate,
                   endDate = endDate,filterCond = filterCond,
                   startTime = startTime, endTime = endTime,
                   timeStep = timeStep,period = period, fromChange = fromChange, 
                   removeDates = removeDates,libraryPath =libraryPath)

  #subset settings
  pumpSettings.list<-subsetSetting(data,libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #if filtered data exists
  if(nrow(data)!=0){
    #get hours as number
    data$hours<- data$hour + data$min/60
    
    #save data for xticks
    dataOrig<-data
    basalOrig<-basal
    
    
    if (stackedBar!="insulin" | boxBar!="bar"){
      data$temp<-eval(parse(text = paste0("data$",plotSummary)))
      
      #ignoreNAs
      if (ignoreNAs){
        data<-data[!is.na(data$temp),]
      }
     
      #get uniques
      if (uniqueDT){
        NAMES<-c("dateTime","Date2","hours","hour","temp")
        data<-uniqueDateTime(data, NAMES, replaceNAs,startTime = startTime,endTime = endTime,timeStep = timeStep, period = period)
      }
      
      data<-data[c("Date2","temp")]
      
      
      #set initial y axis range
      if(plotSummary %in% c("BGvalue","SGvalue") & sumFunc!="length"){
        initYrange<-c(0,450)
        
      }else if (sumFunc=="length"){
        rangeData<-as.data.frame(data %>% group_by(Date2) %>% summarise_all(funs(length)))
        initYrange<-c(0,max(rangeData$temp, na.rm = TRUE))
      }else{
        initYrange<-c(0,max(data$temp, na.rm = TRUE))
      }
      
      yTitle<-ifelse(sumFunc!="length",paste0(sumFunc,"_",plotSummary),paste0("number_",plotSummary))
      dataFormat<-data
   
       }else{#stacked insulin
      basal$rate<-basal[[length(basal)]]
      
      #format time
      basal$time2<-as.POSIXlt(basal$time,format="%H:%M")
      basal$hours<- basal$time2$hour + basal$time2$min/60 
      basal$hour<-basal$time2$hour
      basal<-basal[,names(basal)!="time2"]
      basal$Date2<-as.Date(rep(NA,nrow(basal)))
      basalDate<-basal[0,]
      
      #populate all days with rates
      days<-as.Date(xTicks(data, startTime,endTime,timeStep,period)$xticks,format = "%Y-%m-%d")
     
      #get all change dates
      allDateStr<-character(0)
      for (d in names(basal)[!names(basal) %in% c("Date2","rate","time2","time","hours","hour")]){
        basalsubDate<-basal[,names(basal)!="Date2"]
        basalsubDate<-as.data.frame(basalsubDate %>% group_by(hour) %>% summarise_all(funs(mean),na.rm=TRUE))
        dateStr<-gsub("X","",d)
        dateStr<-gsub("\\.","-",dateStr)
        allDateStr<-c(allDateStr,dateStr)
      }
      
      #get rates for all days in plot
        for (dy in days){ 
          whichRate<-allDateStr[as.Date(allDateStr,format = "%m-%d-%Y",origin = "1970-01-01")<=as.Date(dy,format = "%Y-%m-%d",origin = "1970-01-01")]
          whichRate<-max(as.Date(whichRate,format = "%m-%d-%Y",origin = "1970-01-01"))
          whichName<-names(basal)[which(as.Date(allDateStr,format = "%m-%d-%Y",origin = "1970-01-01")==whichRate)]
        basalsubDate$Date2<-as.Date(dy,format = "%Y-%m-%d",origin = "1970-01-01")
        basalsubDate$rate<-eval(parse(text = paste0("basalsubDate$",whichName)))
        basalDate<-rbind(basalDate,basalsubDate)
      }
      basal<-basalDate

      #set up time step
      basal2<-setTimeStep(basalDate, startTime,endTime, timeStep, period)
      
      #get on only relevant columns
      basal2<-basal2[,c("Date2","rate")]
      
      #get total basal per date
      basal2<-as.data.frame(basal2 %>% group_by(Date2) %>% summarise_all(funs(sum),na.rm=TRUE))

      #get hours in data
     # data$hours<- data$hour + data$min/60 
      
      #only look at pump data
      data<-data[is.na(data$SGvalue),]
      
      #get unique values (max) per Date2 and hours
      #NAMES<-c("dateTime","Date2","hours","hour","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
      NAMES<-c("dateTime","Date2","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
      data<-uniqueDateTime(data, NAMES, replaceNAs = TRUE,startTime = startTime,endTime = endTime,timeStep = timeStep, period = period, sumFunc = "max")

       #summarize by sum
      data<-as.data.frame(data %>% group_by(Date2) %>% summarise_all(funs(sum),na.rm=TRUE))

      
      #merge with basal
      data<-merge(data, basal2, by = "Date2", all = TRUE)
      data<-data[c("Date2","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.","rate")]
      names(data)[1]<-"Date2"
      
      
      

      orderVector<-c("Date2","rate","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
      data<-data[,match(orderVector, names(data))]
      
      names(data)[2]<-"basal rate"
      
      #set initial y axis range
      data$totalInsulin<-apply(data[-c(1)],1, function(x) sum(x,na.rm = TRUE))
      initYrange<-c(0,max(data$totalInsulin, na.rm = TRUE))
      data<-data[-c(length(data))]
      yTitle<-"Insulin Units Delivered"
      
      dataFormat<-data

    }
    
    if (!stackedBar %in% c("","insulin")){#stacked bar BG
      yTitle<-""
      totalBG<-data[!is.na(data$temp),]
      totalBG<-as.data.frame(totalBG %>% group_by(Date2) %>% summarise_all(funs(length)))
      initYrange<-c(0,max(totalBG$temp, na.rm = TRUE))
    }
    
    #format time in decimal hours for xaxis tick marks
    xticks.list<-xTicks(data = dataOrig, startTime,endTime,timeStep,period)
    unPackList(lists = list(xticks.list = xticks.list),
               parentObj = list(NA)) 
    
    #get yaxis code string
    yaxisStr.list<-makeYaxesSummary(addSetting = "", settingOverlay = FALSE, percentSetting = NA,addBarSub = FALSE,addBG = FALSE,
                                initYrange,yTitle)
    unPackList(lists = list(yaxisStr.list = yaxisStr.list),
               parentObj = list(NA)) 
    unPackList(lists = list(ay.list = ay.list),
               parentObj = list(NA))
    ay.list<-yaxisStr.list$ay.list
    
    #get xAxis str
    xaxisStr<-makeXaxis(xDomain)
    
    #make title str
    titleStr<-paste0(min(data$Date2, na.rm = TRUE)," -to- ",max(data$Date2, na.rm = TRUE))
    
    ##make layoutstr
    layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = addGoodRange,stackedBar = stackedBar,
                          description = description,descInset = descInset)
    
    #initialize plot
    p<-plot_ly()
    #add layout
    eval(parse(text = layoutStr))
    #get formatted data
    data<-dataFormat

    if(stackedBar=="" | boxBar!="bar") {#general bar plot
      if (boxBar=="bar"){
      #set up summary function string
      if (sumFunc!="length"){
        sumString<-paste0("as.data.frame(data %>% group_by(Date2) %>% summarise_all(funs(",
                          sumFunc,"),na.rm = TRUE))")
      }else if (sumFunc=="length"){
        sumString<-paste0("as.data.frame(data %>% group_by(Date2) %>% summarise_all(funs(",
                          sumFunc,")))")
        
      }#end sumString
      
      #apply sumString
      data<-eval(parse(text = sumString))

      #create plot
      p <- p %>% add_trace(data = data, x = ~Date2, y = ~temp,type = 'bar', 
                           name = paste0(sumFunc,"_",plotSummary))
      }else{#boxplot
        p <- p %>% add_trace(data = data, x = ~Date2, y = ~temp,type = 'box', 
                             name = paste0(sumFunc,"_",plotSummary))
      }
      
      
    }else if (stackedBar=="insulin"){
      cls<-c("white","gray","black")
      p <- addStackbar_ly(p,data,cls,timeStep) 
      
    }else{#stacked bar BG
      data$veryHigh<-ifelse(data$temp>240,1,0)
      data$high<-ifelse(data$temp>150 & data$temp<=240,1,0)
      data$good<-ifelse(data$temp>=80 & data$temp<=150,1,0)
      data$low<-ifelse(data$temp<80,1,0)
      data<-data[,names(data)!="temp"]
      data<-as.data.frame(data %>% group_by(Date2) %>% summarise_all(funs(sum),na.rm=TRUE))
      orderVector<-c("Date2","low","good","high","veryHigh")
      data<-data[,match(names(data),orderVector)]

      cls<-c("blue","white","red","darkred")
      p <- addStackbar_ly(p,data,cls,timeStep) 
      
    }

    return(p)
    
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}