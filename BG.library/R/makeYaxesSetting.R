#'@title makeYaxesSetting
#'@description Create layout lists for yaxes associated with pump settings \\cr \\cr
#'@param addSetting character vector of settings to plot c("basal,"corrFactor","carbRatio")
#'@param settingOverlay TRUE/FALSE whether or not settings overlay main plot
#'@param addBolusType character vector of types of bolus insulin delivered (addBolusType = "" for none)
#'@param addBarSub TRUE/FALSE whether barsubplot is to be included
#'@param allPosition numeric vector of positions of all y axes
#'@param position numeric value for y axes position (0-1 along x axes)
#'@param numberAxes numeric value for current number of y axes
#'@param yDomain2 numeric vector for y domain of second subplot
#'@param yDomain3 numeric vector for y domain of third subplot
#'@param yaxisStr current character string for layout of all y axes
#'@return `settingAxis.list` named.list(yaxisStr, position, numberAxes)
#'@examples
#'#set paramters
#'addSetting<-c("basal","carbRatio")
#'settingOverlay<- TRUE
#'legendInset<--0.2
#'percentSetting<-30
#'percentBar<-NA
#'addBarSub<-FALSE
#'addBolusType<-""
#'yTitle<-""
#'
#'#set yDomain
#'domain.list<-makeYdomain(percentSetting,percentBar,addSetting,settingOverlay,addBarSub)
#'unPackList(lists = list(domain.list = domain.list),
#'           parentObj = list(NA)) 
#'
#'#get number axes
#'if (addSetting[1]!=""){
#'  numberAxes<-length(addSetting[addSetting!=""])
#'}else{
#'  numberAxes<-1
#'}
#'
#'#set position
#'position<-1
#'allPosition<-position
#'
#'#itialize plotly plot y axis
#'yaxisStr<-paste0("yaxis = list(range = c(0,450), ticks = 'outside', zeroline = FALSE,showline = TRUE,
#'                               title = '",yTitle, "',
#'                               domain = c(",paste(yDomain1,collapse = ","),"))")
#'#create y axes for pump settings
#'settingAxis.list<-makeYaxesSetting(addSetting, settingOverlay, addBolusType,addBarSub,
#'                                   allPosition, position,numberAxes,yDomain2,yDomain3,yaxisStr)

makeYaxesSetting<-function(addSetting, settingOverlay, addBolusType,addBarSub,
                           allPosition, position,numberAxes,yDomain2,yDomain3,yaxisStr)

  
  if (addSetting[1]!=""){# if adding settings to plot
    #setting colors basal = green, carbRatio = blue, corrFactor = red
  setColors<-c("green","blue","red") 
  for (s in 1:length(addSetting)){# for each setting 
    numberAxes<-length(addSetting)
    #set index for axes
    if(s==1 & addBolusType[1]!="" & settingOverlay){
      i<-2+1
      numberAxes<-numberAxes+1
    }else if (s==1){
      i<-3
    }else{
      i<-i+1
    }
    
    
    #set position of each y axis on x axis (0-1)
    if (s==length(addSetting)){
      position<-1
    }else{
      position<-1 - 0.1*(i-2)
    }  
    allPosition<-c(allPosition, position)
    
    #set y domain of setting subplot if barsubplot
    if (addBarSub){
      domainStr<-yDomain3
    }else{
      domainStr<-yDomain2
    }
    
    #setting y axis list for layout
    ay <- list(
      tickfont = list(color = setColors[s]),
      color = setColors[s],
      overlaying = 'y',
      side = 'right',
      title = addSetting[s] ,
      showgrid = FALSE,
      showline = TRUE,
      # tickprefix = " ",
      ticks = 'outside',
      anchor = 'free',
      position  = position, 
      domain = domainStr,
      zeroline = FALSE)
    # rangemode =  'tozero')
    
    #reverse y axis for carbRatio and corrFactor, up means handle high bGs
    if (addSetting[s] %in% c("carbRatio","corrFactor")){
      ay$autorange = 'reversed'
    }
    
    #save list with index
    eval(parse(text = paste0("ay",i,"<-ay")))
    
    #set overlay if overlaying settings on plot
    if (!settingOverlay & i>3){
      eval(parse(text = paste0("ay",i,"$overlaying<-'y3'")))
    }else if (!settingOverlay & i==3){
      ay3<-ay3[!names(ay3)=="overlaying"]
    }
    
    #name y axis for each index
    yStr<-paste0("yaxis",i,"=ay",i)
    yaxisStr<-paste0(yaxisStr,",",yStr)
    
  }#for each setting
  
  #make list of all ay objects for output
  ay.list<-list()
  for (a in ls()[startsWith(ls(),"ay")]){
    eval(parse(text = paste0("ay.list$",a,"<-",a)))
  }
  
  settingAxis.list<-named.list(yaxisStr,ay.list, position, numberAxes, allPosition)
  return(settingAxis.list)
}