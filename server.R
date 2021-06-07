#
# Scipio
#

library(shiny)
library(pander)
library(syuzhet)
library(plotly)
library(data.table)
library(plyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

##########
#
#Build dataframe for selected dates
#
#
##########

date_selection <- reactive({

  # Create date sequence
  outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
  outSeq <- format(as.Date(outSeq, "%Y_%m_%d"))
  browser()

})


})
