#'@title createSavedPlot
#'@description Save a plot to BG.library/data/plotList.  These plots can be executed with 
#'either `executeSavePlot()` or `shinyPlot()`
#'@param libraryPath character string path to BG.library code 
#'@param plotName character string to assign as name of saved plot
#'@param plotType character string indicating which plotting function to use, 
#'options include 'plotLine_ly','summaryPlot_ly', and 'heatMap_ly'
#'@param description character string description of saved plot, will be added to plot as text
#'@param paramList list of all parameters needed to execute the plot
#'@param default TRUE/FALSE make this plot part of the default list of plots 
#'allowing restoration of the original default plotList, by saving only defualt plots back 
#'to the plotList
#'@examples
#'plotName<-"meanSGheat_hist"
#'plotType<-"heatMap_ly"
#'description<-"Heat map of mean hourly SG values per day with histogram of groups."
#'paramList<-list(brks = c(0,50,80,150,240,300,400,500),
#'                brewerPallete = "RdBu",
#'                revPallete = TRUE,
#'                textCol = "black",
#'                tcol = "time2",
#'                dcol = "Date2",
#'                valueVar = "Sensor.Glucose..mg.dL.",
#'                sumFunc = "mean",
#'                naRemove = TRUE,
#'                includeTotals = TRUE,
#'                filterCond = "")
#'
#'createSavedPlot(libraryPath, plotName,plotType,description, paramList)

createSavedPlot<-function(libraryPath, plotName,plotType, 
                          description, paramList, default = TRUE){
  #file path to plotList
  plotListFile<-paste0(libraryPath,"/data/plotList")
  
  #load plotList
  load(file = plotListFile)
  
  #create new plot for list
  plotListSub<-list(plotType = plotType,
                    description = description,
                    paramList = paramList,
                    default = default)
  
  
  eval(parse(text = paste0(plotName,"<-plotListSub")))
  eval(parse(text = paste0("plotList$",plotName,"<-",plotName)))
  
  #save updated plotList
  save(file = plotListFile,plotList) 
}

