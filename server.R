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
  #Read remote database
  #Append
  #
  ##########




#############################################################################
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
  ifelse(first_pass, first_pass<<- TRUE, cycle_dates$end_date <- cycle_dates$end_date -1)
  print("Post first pass test")
  print(first_pass)
  print( cycle_dates)
  outSeq <- seq(as.Date(cycle_dates$start_date) , as.Date(cycle_dates$end_date), by = "day")
  outSeq <- format(as.Date(outSeq, "%Y-%m-%d"))
  return(outSeq)
})


retrieve_Db <- reactive({
  print("server 1 - Set up db connect")
  remoteuserpassword <- "m3t1sz"
  conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
  print("Server 1 - Connected remote 1")
  dbListTables(conR)
  data_selection_frame_append <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                            country = character() , region = character(),
                                            syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                            nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                            nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                            nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                            loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                            loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                            hash_value = character())
  # Reformat dates
  inserted_date_seq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")

  # Read db loop
  for(i in seq_along(inserted_date_seq)){         # Start of read DB loop
    inserted_date <-  as.character( gsub("-", "_", inserted_date_seq[i]  ))


    queryScript <- paste0("SELECT ext_name, item_title,item_date_published, orientation, country,
            syuzhet_score, afinn_score, bing_score,
            nrc_score_anger, nrc_score_anticipation, nrc_score_disgust, nrc_score_fear,
            nrc_score_joy, nrc_score_positive, nrc_score_negative,
            nrc_score_sadness, nrc_score_surprise, nrc_score_trust,
            loughran_frame_constraining, loughran_frame_litigious,
            loughran_frame_negative, loughran_frame_positive, loughran_frame_uncertain,
            md5(concat(item_title, item_date_published)) AS hash_value
                             FROM sa_RSS_library", inserted_date, "
                            ;" )
    tryCatch(
      expr = {
        try_date <- paste0("sa_RSS_library", inserted_date)
        query1  <- dbGetQuery(conR, queryScript)
        query1$item_date_published <- as.Date(query1$item_date_published, format = "%Y-%m-%d")
        data_selection_frame_append <- rbind(data_selection_frame_append, query1)
        print("Data_selection_frame_append", nrow(data_selection_frame_append))
      },
      error = function(e){
        message(paste0("Error message on date: ", inserted_date, " "))
        message(queryScript)
      },
      finally = {
        message("tryCatch database read finished")
      }
    )  # end of tryCatch
  }
  # On first pass
  dbDisconnect(conR)
  if(first_pass == TRUE)({data_selection_frame <<- data_selection_frame_append
                         first_pass <<- FALSE})

  data_selection_frame <<- rbind(data_selection_frame, data_selection_frame_append)

  print("end of loop")
  return(data_selection_frame)
})

##########
#
#Output functions
#
#
##########


output$dateSelection <- renderTable(date_selection())
output$date_lookup <- renderTable(retrieve_Db())
})
