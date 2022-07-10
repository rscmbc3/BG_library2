#'@title addPercentBG_ly
#'@description Outputs a data.frame of grouped (low < 80, 80<=good<=150, 150<high<=240, very high >240) 
#'percentages of BG or SG data or adds percentages as text to plotly interactive plot. \\cr \\cr
#'@param p current plot_ly plot
#'@param data data.frame with BG values in BGvalue and SG values in SGvalue
#'@param addPercentBG character vector of groups to include (c("low","good","high","very high"))
#'@param addPercentType character string of column name with values to group (i.e. "BGvalue")
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
#'@param startTime character string of beginning time for plot (typically startTime = "00:00)
#'@param endTime character string of ending time for plot (typically endTime = "23:00)
#'@param timeStep character string indicating the time step to aggregate data, possible values
#'include c("hour","day")
#'@param period numeric value indicating number of `timeSteps` to aggregate into single step
#'for example : `timeStep = 'hour'`  and `period = 3` outputs plots with tick marks every 3 hours.
#'@param removeDates character vector of dates in format %Y-%m-%d to remove from data
#'@param filterCond character string of R syntax to be applied to filter the data, 
#'example `data[data$BGvalue>150 & !is.na(data$BGvalue),]`
#'@param outputType character string indicating type of output, options are "plot_ly", or "table"
#'@param libraryPath character string path to BG.library code 
#'@return `p` plot_ly interactive plot with text indicating percentages in right-hand corner or
#'`percentOut` a data.frame of percentages by group if `outputTupe=="table"`
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                fromChange = TRUE,libraryPath = libraryPath)
#'#output as 'plot_ly'
#'p<-plot_ly()
#'p<-addBGpoints_ly(p, data)
#'addPercentBG_ly(p,data,libraryPath = libraryPath)
#'
#'#output as 'table'
#'addPercentBG_ly(p = NA,data,outputType = "table",libraryPath = libraryPath)

addPercentBG_ly<-function(p,data,addPercentBG = c("low","good","high","very high"),
                          addPercentType = "BGvalue",
                          fromChange = TRUE,numberDays = NA, 
                          startDate = NA,endDate = NA,
                          startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1,
                          removeDates = NA,filterCond = "",outputType = "plot_ly",libraryPath){
  if (addPercentBG[1]!=""){
    if (outputType != "plot_ly"){
    #subset data by date and filterCond
    data<-subsetData(data,numberDays = numberDays,startDate = startDate,
                     endDate = endDate,filterCond = filterCond,
                     startTime = startTime, endTime = endTime,
                     timeStep = timeStep,period = period, fromChange = fromChange, 
                     removeDates = removeDates,libraryPath =libraryPath)
    }

    if (addPercentType=="BGvalue"){
     data<-data[!is.na(data$BGvalue),] 
     data$value<-data$BGvalue
     typeStr<-"BG"
    }else{
      data<-data[!is.na(data$SGvalue),] 
      data$value<-data$SGvalue
      typeStr<-"SG"
    }
   
    #get unique values
    NAMES<-c("dateTime","hour","value")
    data<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,startTime = startTime,endTime = endTime,timeStep = "hour", period = 1)
    
    yPos<-numeric(0)
    percentStr<-character(0)
    allPercent<-numeric(0)
    for (j in addPercentBG) {
      if (length(grep("very high",addPercentBG))!=0 | length(grep("high",addPercentBG))==0){
        subsetStr<-ifelse(j=="low","<80",
                        ifelse(j=="good",">=80 and <=150",
                               ifelse(j=="high",">150 and <=240",
                                      ifelse(j=="very high",">240",""))))
      }else if (length(grep("high",addPercentBG))!=0){
        subsetStr<-ifelse(j=="low","<80",
                          ifelse(j=="good",">=80 and <=150",
                                 ifelse(j=="high",">150")))
      }
      
      
      rangeStr<-gsub(">","",subsetStr)
      rangeStr<-gsub("<","",rangeStr)
      rangeStr<-gsub("=","",rangeStr)
     
      rangeStr<-gsub("and",paste0("<=",typeStr,"<="),rangeStr)
      rangeStr<-ifelse(regexpr(typeStr,rangeStr)>0,rangeStr,paste0(typeStr," ",rangeStr))
      rangeStr<-ifelse(regexpr(paste0("80<=",typeStr,"<=150"), rangeStr)>0,paste0("80<=",typeStr,"<150"),rangeStr)
      rangeStr<-ifelse(regexpr(paste0(typeStr," 80"),rangeStr)>0,paste0(typeStr,"<80"),rangeStr)
      rangeStr<-ifelse(regexpr(paste0(typeStr," 240"),rangeStr)>0,paste0(typeStr,">240"),rangeStr)
      rangeStr<-ifelse(regexpr(paste0(typeStr," 150"),rangeStr)>0,paste0(typeStr,">150"),rangeStr)

      
      subsetStr<-strsplit(subsetStr," and ")[[1]]
      subsetStr<-paste0("data$value",subsetStr, collapse = " & ")
      subsetStr<-paste0("which(",subsetStr,")")
      subsetValues<-eval(parse(text = subsetStr))
      
      percent<-round(length(subsetValues)/nrow(data)*100)
      allPercent<-c(allPercent,percent)
      
      rangeStr<-paste0(percent,"% ",rangeStr)
      percentStr<-c(percentStr,rangeStr)
      
      i<-which(addPercentBG==j)
      yPos<-c(yPos,450-25*i)
    }#for each addpercentBG
    
    #get positional data
    posData<-data.frame(x = rep(17,length(addPercentBG)), y = yPos, percentStr = percentStr)
    
    if (outputType == "plot_ly"){
    p <- p %>% add_text(data = posData, x = ~x, y = ~y, text = ~percentStr, 
                        textposition = "right",hoverinfo='skip', showlegend = FALSE)
}else{#output table
  if (addPercentType=="BGvalue"){
    percentOut<-data.frame(BGtype = addPercentBG, percent = allPercent)
  }else{
    percentOut<-data.frame(SGtype = addPercentBG, percent = allPercent)
    
  }
}
    
  }#if addPercentBG
  
  if (outputType == "plot_ly"){
  return(p)
  }else{
    return(percentOut)
  }
    
}#end func
