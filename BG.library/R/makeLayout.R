#'@title makeLayout
#'@description creates the character string to execute for plot_ly layout customized according to 
#'data selection with multiple axes and scales. \\cr \\cr
#'@param titleStr character string plot title
#'@param xDomain character string to exectute setting the domain of xaxis (format, "domain = c(0,1),")
#'@param xaxisStr character string to execute to set specifications for xaxis, output
#'from `makeXaxis()`
#'@param yaxisStr character string to execute to set specifications for y axes, output
#'from `makeYaxes()`
#'@param addGoodRange TRUE/FALSE indicating whether shaded polygon for good BG range is plotted
#'@param stackedBar character string indicating type of stacked bar plot, if `stackedBar = ''`
#'no stacked bar plot will be output
#'@param description character string plot description to be output as part of plot
#'@param descInset numeric value to place description below plot
#'@return `layoutStr` character string to execute for plot_ly layout
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'data<-subsetData(data,numberDays = NA,startDate = NA,endDate = NA,filterCond = "",
#'                 startTime = "00:00", endTime = "23:00",timeStep = "hour",period = 1, 
#'                 fromChange = TRUE,libraryPath = libraryPath)
#'#set paramters
#'addSetting<-""
#'settingOverlay<- FALSE
#'legendInset<--0.2
#'
#'#format time in decimal hours
#'xticks.list<-xTicks(data, startTime = "00:00",endTime = "23:00",
#'                    timeStep = "hour",period = 1)
#'unPackList(lists = list(xticks.list = xticks.list),
#'           parentObj = list(NA)) 
#'#make y axis str
#'yaxisStr.list<-makeYaxes(addBolusType = "", addSetting,settingOverlay,
#'                         percentSetting =NA,addBarSub = FALSE,percentBar = NA,yTitle = "")
#'unPackList(lists = list(yaxisStr.list = yaxisStr.list),
#'           parentObj = list(NA)) 
#'unPackList(lists = list(ay.list = ay.list),
#'           parentObj = list(NA))
#'ay.list<-yaxisStr.list$ay.list
#'#get xAxis str
#'xaxisStr<-makeXaxis(xDomain)
#'
#'#make title str
#'titleStr<-paste0(min(data$Date2)," -to- ",max(data$Date2))
#'
#'##make layoutstr
#'layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = FALSE,
#'                      description = "",descInset =NA)
#'p<-plot_ly()
#'#add layout
#'eval(parse(text = layoutStr))
#'addBGpoints_ly(p, data)

makeLayout<-function(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange, stackedBar = "",
                     description, descInset){
  if (addGoodRange){
    shapeStr<-"shapes = list(list(type = 'rect',
                                fillcolor = 'green',
                                line = list(color = 'green'),
                                opacity = 0.2,
                                x0 = 0, x1 = 23,
                                y0 = 80, y1 = 150))"
  }else{
    shapeStr<-"shapes = list()"
  }
  
if (stackedBar!=""){
  barmodeStr<-"barmode = 'stack',"
}else{
  barmodeStr<-""
}
  
  if (description!=""){
    description<-breakStr(description,70,"\n")
    description<-gsub("'",'\\"',description)
    descStr<-paste0("annotations = 
                      list(x = 0, y =",descInset, ", text = '",description,"', 
                           showarrow = F, xref='paper', yref='paper', 
                           xanchor='left', yanchor='auto', xshift=0, yshift=0,
                           font=list(size=15, color='black')),")
  }else{
    descStr<-""
  }
  
  ##make layoutstr
  layoutStr<-paste0("p <- p %>% layout( showlegend=T, legend = list(orientation = 'h',   # show entries horizontally
                    xanchor = 'center',  # use center of legend as anchor
                    x = 0.5,
                    y = legendInset),",
                    xaxisStr,",",
                    yaxisStr,",",
                    shapeStr,",",
                    barmodeStr,
                    descStr,"
                    margin = list(r = 100,b = 200),
                    title = list(text = '",titleStr,"',
                    xanchor  = 'right'))")
  return(layoutStr)
}