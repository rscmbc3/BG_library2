#'@title scatterUI
#'@description generate Shiny UI for 'scatter' plots. \\cr \\cr
#'@return `ui` shiny UI for 'scatter' plots.
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)
#'data<-dataImport.list$allData
#'#file path to plotList
#'plotListFile<-paste0(libraryPath,"/data/plotList")
#'#load plotList
#'load(file = plotListFile)
#'
#'shinyApp(ui=shinyUI(
#'  fluidPage(
#'    titlePanel(
#'      h1("Rshiny Interactive BG Plots")),
#'    sidebarLayout(
#'      sidebarPanel(width=6,
#'                   #top level user input
#'                   selectInput("shPlotType","Select Plot Type",
#'                               choices = c("scatter","bar","box","heatmap","Saved Plot"),
#'                               selected = 'scatter'),  
#'                   uiOutput("outputUI")
#'      ),#end sidebar
#'      mainPanel(width = 6,
#'                verbatimTextOutput("txtOut")
#'      )#end main panel
#'    )#end sidebar layout
#'  )#end fluid page
#'),#end shiny ui
#'
#'server=shinyServer(function(input, output,session) {
#'  #render UIs
#'  observe({
#'    if (input$shPlotType=="scatter"){
#'      output$outputUI<-renderUI({
#'        scatterUI()
#'      })
#'    }else if (input$shPlotType=="box" | input$shPlotType=="bar"){
#'      output$outputUI<-renderUI({
#'        boxBarUI(data)
#'      })
#'    }else if (input$shPlotType=="heatmap"){
#'      output$outputUI<-renderUI({
#'        heatmapUI(data)
#'      })
#'    }else{#saved plot
#'      output$outputUI<-renderUI({
#'        savedUI(plotList)
#'      })
#'    }
#'  })
#'  
#'  output$txtOut <- renderText({
#'    paste0("These are settings for ",input$shPlotType," plots")
#'  }) 
#'})#end shiny server
#')#end shiny app


scatterUI<-function(){
  ui=shinyUI(
    fluidPage(

  
  sliderInput("timeRange","Time Range",min = 0, max = 23, value = c(0,23)),
  
  h3("Add data to plot"),
  h4("BG and SG data"),
  
  checkboxInput("scatterOnly","Points Only",value = FALSE),
  
  selectInput("plotSummary","Data type to summarize by min, mean, max lines (leave blank for daily sensor lines)",
              choices = c("","Sensor.Glucose..mg.dL.","BG.Reading..mg.dL."),
              selected = "Sensor.Glucose..mg.dL."),
  
  checkboxInput("addBG","Add BG values as points to plot",value = TRUE),
  
  checkboxInput("addFasting","Add line indicating mean fasting BG value", value = TRUE),
  checkboxInput("addFastingAnnot","Add arrow and text to plot indicating mean fasting BG line",value = TRUE),
  
  
  
  checkboxGroupInput("addPercentBG", "Add grouped BG or SG percentages to plot as text", 
                     choices =  c("low","good","high","very high"),
                     selected = c("low","good","high","very high"),
                     inline=TRUE),
  
  selectInput("addPercentType","Type of reading used to get summary percentages per group (either BG or SG)",
              choices = c("" ,"Sensor.Glucose..mg.dL.","BG.Reading..mg.dL."), selected = "BG.Reading..mg.dL."),
  checkboxInput("addGoodRange","Add colored bar indicating good BG range <80 and >150",value = TRUE),
  
  
  h4("Bolus and Carb Data"),
  selectInput("addBolusType","Type of insulin delivery points to add",
              choices  = c("","Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")),
  checkboxInput("addBarSub","Add subplot of mean carbs consumed per hour",value = FALSE),
  conditionalPanel(#only  if addBarSub
    condition = "input.addBarSub == TRUE",
    sliderInput("percentBar","Percentage of plot area to dedicate to mean carb subplot (0-100)'",
                min = 0, max = 100, value = 30)),
  
  h4("Pump Settings"),
  
  
  checkboxGroupInput("addSetting", "Add Pump Settings", 
                     choices =  c("basal","corrFactor","carbRatio"),
                     selected = c("basal","corrFactor","carbRatio"),
                     inline=TRUE),
  conditionalPanel(#only use numberDays and start End dates if not fromChange
    condition = "input.addSetting.includes('basal') || 
                         input.addSetting.includes('corrFactor') ||
                         input.addSetting.includes('carbRatio')",
    checkboxInput("settingOverlay","Overlay pump settings on top of data plot 
              (if blank settings will plot as subplot below main plot)",value = FALSE),
    sliderInput("percentSetting","Percentage of plot area to dedicate to setting subplot (0-100)",
                min = 0, max = 100,value = 30)
  ),
  
  h3("General Plotting Parameters"),
  numericInput("pointSize","Point Size",value = 10, min = 1),
  
  conditionalPanel(#only use numberDays and start End dates if not fromChange
    condition = "input.plotSummary!='Sensor.Glucose..mg.dL.' & input.scatterOnly!=1",
    textInput("colorPalleteDaily","Select color pallete for daily sensor lines",value = "rainbow")
  ),
  
  textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
  
  numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
  textInput("description","Optional plot description to output below plot area",value = ""),
  numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15)
  

  ))
  return(ui)

}