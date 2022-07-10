dateTimeUI<-function(){
ui=shinyUI(
  fluidPage(
    
    h3("Date and Time Parameters"),
    checkboxInput("fromChange","Use data from most recent pump settings change to present",value = TRUE),
    
    conditionalPanel(#only use numberDays and start End dates if not fromChange
      condition = "input.fromChange != 1",
      numericInput("numberDays","Number of Days from Latest Date",value = NA),
      dateRangeInput("daterange", "Date range",
                     format = "mm/dd/yy",
                     separator = " - ")
      
    ),
    airDatepickerInput("removeDates","Remove Dates",multiple = TRUE)
  ))
}