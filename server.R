#
# Scipio
#

library(shiny)
library(pander)
library(syuzhet)
library(plotly)
library(data.table)
library(plyr)
library(dplyr)
library(RMariaDB)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  print("server 1 - Set up db connect")
  # browser()
  remoteuserpassword <- "m3t1sz"
  conR <- dbConnect(RMariaDB::MariaDB(),
                    dbname = 'metis', 'metis',
                    password = remoteuserpassword,
                    host = "178.62.8.181", port = 3306)
  print("Server 1 - Connected remote 1")
  dbListTables(conR)

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
    # browser()
    if(date1 < global_start_date){
      print("Date < than previous minimum")
      check_date_list = list(Action = TRUE, start_date = date1, end_date = global_start_date)
      global_start_date <<- date1
    }else{
      print("Date >= previous minimum")
      first_pass <<- FALSE
      check_date_list = list(Action = FALSE, start_date = date1, end_date = date2)

    }
#    print(check_date_list)
    return(check_date_list)
  }

remote_Connect <- function(){
  print("server 1 - Set up db connect")
  remoteuserpassword <- "m3t1sz"
  conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
  return(conR)
}


  ##########
  #
  #Read remote database
  #
  #
  ##########

read_Remote <- function(inserted_date_seq){
  data_selection_frame_append <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                            country = character() , region = character(),
                                            syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                            nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                            nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                            nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                            loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                            loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                            hash_value = character())
  withProgress(message = "Reading remote database",
               for(i in seq_along(inserted_date_seq)){         # Start of read DB loop
#                 print(paste("Loop count", i))
                 inserted_date <-  as.character( gsub("-", "_", inserted_date_seq[i]  ))
#                 print(paste("Loop", i, "Inserted date", inserted_date))

                  print(paste("Date under retrieval:", inserted_date))
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
                     # print("Data_selection_frame_append")
                     # print(nrow(data_selection_frame_append))
                   },
                   error = function(e){
                     message(paste0("Error message on date: ", inserted_date, " "))
                     message(queryScript)
                   },
                   finally = {
#                     message("tryCatch database read finished")
                     incProgress(1/length(inserted_date_seq))
                   }
                 )       # end of tryCatch
               }         # end of for loop

  ) # End of withProgress
return(data_selection_frame_append)
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
  # browser()
  ifelse(first_pass, first_pass<<- TRUE, cycle_dates$end_date <- cycle_dates$end_date -1)
  outSeq <- as.character(seq(as.Date(cycle_dates$start_date) , as.Date(cycle_dates$end_date), by = "day")) # as.character fixes bug

  return(outSeq)
})


retrieve_Db <- reactive({
  # browser()
  if(input$dateRange[1]< global_start_date | first_pass == TRUE){
    first_pass <<- FALSE   # Set first_pass flag to FALSE
  remote_Connect()         # Connect to remote db
  inserted_date_seq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")


  data_selection_frame_append  <- read_Remote(inserted_date_seq)
  dbDisconnect(conR)
  print("Remote db disconnected")
  data_selection_frame <<- rbind(data_selection_frame, data_selection_frame_append)
  global_start_date <<-input$dateRange[1]}else{
    print("No new db records required")
  }
  data_selection_frame <<- unique(data_selection_frame )



  return(data_selection_frame)
})

# Small return table for test purposes
list_head_DB <- reactive({
  small_out <- retrieve_Db()
  small_out$item_date_published <- as.character(small_out$item_date_published)
  return(head(small_out))
})

##########
#
#Output functions
#
#
##########

output$dateSelection <- renderTable(date_selection())
output$reduced_Table <- renderTable(list_head_DB())

})
