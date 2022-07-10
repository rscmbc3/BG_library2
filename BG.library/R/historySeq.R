#'@title historySeq
#'@description Execute series of historical plots through time sequence \\cr \\cr
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
#'@param libraryPath character string path to BG.library code 
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'historySeq(data = data,  plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
#'           seqType = "change", seqLength = 3,libraryPath)


historySeq<-function(data, plotName, paramList, plotType,
                     seqType, seqLength, period = NA, removeDates = NA,libraryPath){

  
  if (seqType=="change"){
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  dateSeq<-.Date(0)
  dateSeq<-dateSeq[0]
  for (s in names(pumpSettings.list)){
    eval(parse(text = paste0("temp<-pumpSettings.list$",s)))
    dateStr<-gsub("X","",names(temp)[2:length(temp)])
  dateStr<-gsub("\\.","-",dateStr)
  dateStr<-as.Date(dateStr,format = "%m-%d-%Y",origin = "1970-01-01")
    dateSeq<-c(dateSeq,dateStr)
  }
  dateSeq<-c(as.Date(max(data$Date), format = "%Y-%m-%d"),dateSeq) 
  dateSeq<-unique(dateSeq)
  dateSeq<-dateSeq[order(dateSeq, decreasing = TRUE)]
  dateSeq<-dateSeq[dateSeq<=max(data$Date2, na.rm = TRUE)]
  }else{#if seqType = days
    dateSeq<-seq.Date(from = max(data$Date2, na.rm = TRUE),to = min(data$Date2, na.rm = TRUE),by=paste0('-',period,' day'))
  }


  #trim dateSeq
  if (seqLength!='all' & seqType=='change'){
    if (length(dateSeq)>seqLength+1){
      
      dateSeq<-dateSeq[1:(seqLength+1)]
    }
  }else if (seqType!='change' & seqLength!='all'){
    if (length(dateSeq)<seqLength+1){
      dateSeq<-c(dateSeq,min(data$Date2))
    }else{
      dateSeq<-dateSeq[1:(seqLength+1)]
    }
  }

  #plot each pair
    for (d in 1:(length(dateSeq)-1)){#for each pair
      endDate<-as.character(dateSeq[d])
    startDate<-as.character(dateSeq[d+1])


    changeParam.list<-list(startDate = startDate,
                           endDate = endDate,
                           fromChange = FALSE,
                           removeDates = removeDates)
    

    if (!is.na(plotName)){
         p<-executeSavedPlot(data, numberDays =NA, plotName,changeParam.list, libraryPath)
print(p)
    }else{#not saved plot
      
      #change parameters
      if (!is.na(changeParam.list)){
        for (c in names(changeParam.list)){
          eval(parse(text = paste0("paramList$",c,"<-changeParam.list$",c)))
        }
      }
      #unpack paramList
      unPackList(lists = list(paramList = paramList),
                 parentObj = list(NA))
      
      #format plot params
      paramStr<-character(0)
      for (p in 1:length(paramList)){
        pName<-names(paramList)[p]
        pStr<-paste0(pName,"=",pName,",")
        paramStr<-paste0(paramStr,pStr)
      }
      paramStr<-paste0(paramStr,"data = data,numberDays = NA")
      
      #execute plot
      execStr<-paste0(plotType,"(",paramStr,")")
      eval(parse(text = execStr))
    }
      
    }

}