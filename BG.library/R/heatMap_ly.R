#'@title heatMap_ly
#'@description Creates interactive heatmap with custom breakpoints and colors.  Optional
#'`addHist` parameter allows for histogram subplot by color group. \\cr \\cr
#'@param data data.frame with data to plot
#'@param tcol character string indicating 'time' column name
#'@param dcol character string indicating 'date' column name
#'@param removeDates character vector of dates in format %Y-%m-%d to remove from data
#'@param valuevar character string indicating column name to plot
#'@param sumFunc character string indicating function to aggregate data for plot
#'@param naRemove TRUE/FALSE whether missing values should be removed
#'@param replaceNAs TRUE/FALSE whether missing values should be replaced with zeros
#'@param addBolusType character string vector of Bolus columns to add as scatter points 
#'addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")
#'@param pointSize scatter point size
#'@return `p` plot_ly interactive plot with BG values added if addBG
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'#load functions
#'devtools::load_all(libraryPath,recompile = FALSE) 
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'heatMap_ly(data = data,valueVar = "BG.Reading..mg.dL.",
#'           sumFunc = "mean",
#'           brks = c(0,50,80,150,240,300,400,500), 
#'           libraryPath = libraryPath)


heatMap_ly<-function(data, tcol = "time2", dcol = "Date2", removeDates = NA,valueVar, 
                     sumFunc, naRemove = TRUE,replaceNAs = FALSE,
                     fromChange = TRUE,numberDays = NA, 
                     startDate = NA, endDate = NA,
                     filterCond = "",
                     timeStep = "hour", period = 1,
                     #heatMap params
                     brks = seq(0,450,50), 
                     brewerPallete = "RdBu", revPallete = TRUE,
                     textCol = "black",addHist = TRUE,libraryPath){
  
  #make title str
  titleStr<-paste0(sumFunc,"_",valueVar)
  
  data<-timeDayTable(data = data, tcol = tcol, dcol = dcol, 
                     valueVar = valueVar, 
                     sumFunc = sumFunc, naRemove = naRemove,
                     includeTotals = TRUE,
                     numberDays = numberDays,
                     startDate = startDate, endDate = endDate,
                     filterCond = filterCond,replaceNAs = replaceNAs,
                     startTime = startTime,endTime = endTime,
                     timeStep = timeStep, period = period,fromChange = fromChange,
                     libraryPath = libraryPath,removeDates = removeDates)
  
  
  
  #format data
  rownames(data)<-data$time
  data<-data[-c(nrow(data)),-c(1,length(data))]
  data<-as.matrix(data, ncol = length(data))
  data<-t(apply(data,1,function(x) ifelse(is.nan(x),NA,as.numeric(x))))
  data<-apply(data,2,round)
  
  #set breaks and make legend character
  myBreaks<-brks
  for (b in 1:(length(myBreaks)-1)){
    br<-paste0(myBreaks[b], " to ",myBreaks[b+1])
    if (b==1){
      legendChar<-br
    }else {
      legendChar<-c(legendChar,br)
    }
  }
  
  
  #set color palette
  # myCol<-brewer.pal(length(myBreaks)-1,"RdBu")
  myCol<-brewer.pal(length(myBreaks),brewerPallete)
  myCol<-myCol[myCol!="#92C5DE"]
  myColLegend<-brewer.pal(length(myBreaks),brewerPallete)
  if (revPallete){
    myCol<-rev(myCol)
    myColLegend<-rev(myColLegend)
  }
  
  
  
  mat.intervals <- cut(data,breaks=brks)
  interval.mat <- matrix(mat.intervals,nrow=nrow(data),ncol=ncol(data),dimnames=list(rownames(data),colnames(data)))
  require(reshape2)
  interval.df <- reshape2::melt(interval.mat,varnames=c("time","date"),value.name="expr")
  interval.df$expr<-factor(interval.df$expr,levels = levels(mat.intervals))
  #interval.cols<-myCol[1:length(na.omit(unique(interval.df$expr)))]
  interval.cols<-myCol[levels(interval.df$expr) %in% na.omit(unique(interval.df$expr))]
  names(interval.cols) <- levels(mat.intervals)[1:length(na.omit(unique(interval.df$expr)))]
  interval.cols2 <- rep(interval.cols, each=ncol(data))
  color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
  color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)
  for (i in 1:(2*length(interval.cols))) {
    currentRange<-names(interval.cols)[i]
    #print(currentRange %in% interval.df$expr)
    color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
    color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
    
  }
  
  data2<-reshape2::melt(data,varnames=c("time","date"),value.name="expr")
  data2$expr<-ifelse(is.na(data2$expr),"",data2$expr)
  
  interval.df$value<-data2$expr
  p<-plot_ly(z=c(interval.df$expr),x=interval.df$date,y=interval.df$time,data = interval.df,
             colors=interval.cols2,
             type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:length(brks)),
                           ticktext=legendChar,len=0.2,outlinecolor="black",
                           bordercolor="white",borderwidth=5,bgcolor="white", y = 1),
             hoverinfo = 'text',
             text = ~paste('</br> Date: ',date,
                           '</br> Time: ',time,
                           '</br> ',sumFunc,'BG value :',value),
             yaxis = "y")
  p<- p %>% layout(xaxis = list(showgrid = FALSE),
                   yaxis = list(showgrid = FALSE,
                                autorange='reversed'),
                   title = list(text = titleStr),
                   margin = list(t = 70), showlegend = FALSE)
  
  
  p<-p %>% add_annotations(data = data2,text = data2$expr, x = data2$date, y = data2$time, 
                           xref = 'x',yref = 'y',showarrow = FALSE, font = list(color = textCol),
                           yaxis = "y")
  p1<-p
  if (addHist){
    histData<-aggregate(interval.df$expr,by = list(interval = interval.df$expr),length)
    names(histData)[2]<-"count"
    # interval.cols3<-data.frame(interval = names(interval.cols),colHex = interval.cols)
    uniqueInter<-na.omit(unique(interval.df$expr))
    uniqueInter<-uniqueInter[order(uniqueInter)]
    
    interval.cols3<-data.frame(interval =uniqueInter,colHex = interval.cols)
    histData<-merge(histData,interval.cols3, by = "interval")
    histData$interval<-factor(histData$interval,levels = levels(mat.intervals))
    histData<-histData[order(histData$interval),]
    histData$text<-legendChar[levels(interval.df$expr) %in% na.omit(unique(interval.df$expr))]
    p2<-plot_ly()
    for (h in 1:length(histData$colHex)){
      p2 <- p2 %>% add_bars(data = histData[h,], x = histData$text[h], y = histData$count[h], 
                            marker = list(color = histData$colHex[h], 
                                          line = list(width = 1, color = "black")),
                            hoverinfo = 'text',
                            text = ~paste(count))
    }
    p2<-p2 %>% layout(xaxis = list(showgrid = FALSE,
                                   ticktext = histData$text,
                                   tickvals = histData$text),
                      title = list(text = titleStr),
                      margin = list(t = 70), showlegend = FALSE, bargap = 0)
  }
  p<-subplot(p1,p2,nrows = 2, heights = c(0.75,0.25))
  p
}