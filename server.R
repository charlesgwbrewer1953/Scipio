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
# Non-Reactive functions
#
#
##########



# If input$date1 < previous earliest date read, indicates database read action required
# Takes earlier of previous earliest date and input$date1 and returns this as the start date
# Takes last earliest dat and returns this as end of lookup sequence date
# If input$date1 not < than previus earliest data returns FALSE indicating no read required
  check_dates <- function(date1, date2){
    if(date1 < global_start_date){
      print("Date < than previous minimum")
      check_date_list = list(Action = TRUE, start_date = date1, end_date = global_start_date)
      global_start_date <<- date1

    }else{
      print("Date >= previous minimum")

      check_date_list = list(Action = FALSE, start_date = date1, end_date = date2)

    }
    print(check_date_list)
    return(check_date_list)
    }

##########
#
#Reactive functions
#
#
##########


##########
#
#Build dataframe for selected dates
#
#
##########

# 1 Get start and end dates

date_selection <- reactive({

  cycle_dates <- check_dates(input$dateRange[1], input$dateRange[2])
  print("Pre first pass test")
  print(first_pass)
  print(cycle_dates)
  ifelse(first_pass, first_pass<<- FALSE, cycle_dates$end_date <- cycle_dates$end_date -1)
  print("Post first pass test")
  print(first_pass)
  print(cycle_dates)
  outSeq <- seq(as.Date(cycle_dates$start_date) , as.Date(cycle_dates$end_date), by = "day")
  outSeq <- format(as.Date(outSeq, "%Y-%m-%d"))
  return(outSeq)

   data_selection_frame <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                      country = character() , region = character(),
                                      syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                      nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                      nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                      nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                      loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                      loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                      hash_value = character())

})



##########
#
#Output functions
#
#
##########


output$dateSelection <- renderTable(date_selection())
})
