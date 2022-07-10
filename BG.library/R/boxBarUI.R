#'@title boxBarUI
#'@description generate Shiny UI for 'box' and 'bar plots. \\cr \\cr
#'@param data data.frame to use for shiny plots
#'@return `ui` shiny UI for 'box' and 'bar' plots.
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
#'                               selected = 'bar'),  
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

boxBarUI<-function(data){
  ui=shinyUI(
    fluidPage(
     

      
      sliderInput("timeRange","Time Range",min = 0, max = 23, value = c(0,23)),
      fluidRow(selectInput("timeStep","Time Step",choices = c("hour","day"),selected  = "hour"),
               numericInput("period","Time Step length (period)",value = 1)),
      
      h3("Add data to plot"),
      h4("BG and SG data"),
      
      selectInput("stackedBar","Stacked Bar plot column name (blank for regular barplot)",
                  choices = c("","insulin","BG","SG"),selected = ""),
      checkboxInput("uniqueDT","Get only unique dateTime values",value = TRUE),
      
      selectInput("plotSummary2","Data to summarize",
                  choices = c("",names(data)),
                  selected = "Sensor.Glucose..mg.dL."),
      textInput("sumFunc","Summary Function to be applied per time step",value = "mean"),
      
      checkboxInput("replaceNAs","Replace missing values with zeros",value = FALSE),
      checkboxInput("ignoreNAs","Remove rows with missing data",value = TRUE),
      
      checkboxInput("addBG","Add BG values as points to plot",value = FALSE),
      
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
      textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
      
      numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
      textInput("description","Optional plot description to output below plot area",value = ""),
      numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15)
    
    ))
  return(ui)
  
}