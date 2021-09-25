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
   # # # browser()
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


# check_date2()
# Determines dates for which db must be read
# If first_pass = TRUE, all dates from input screen
# If input$date1 < previous earliest date read, indicates database read action required
# Takes earlier of previous earliest date and input$date1 and returns this as the start date
# Takes last earliest dat and returns this as end of lookup sequence date
# If input$date1 not < than previus earliest data returns FALSE indicating no read required

  check_dates2 <- function(date1, date2){
    print("check_dates2() 43")
    if(first_pass == TRUE){
      print("OPTION 1 - first pass -[check_dates2()]")
      global_start_date <<- date1
      first_pass <<- FALSE
      check_date_list2 <- list(Action = TRUE, start_date = date1, end_date = date2)
    }else{
      if(date1 < global_start_date){
        "OPTION 2 - Read more records - [check_dates2()]"
        check_date_list2 <- list(Action = TRUE, start_date = date1, end_date = global_start_date - 1)
        global_start_date <<- date1 # date 1 becomes the new lowest date read
      }else{
        print("OPTION 3 - no more records - [check_dates2()]")
        check_date_list2 <- list(Action = FALSE, start_date = date1, end_date = date2)
      }
    }
    print(check_date_list2)
#    print(paste("Action List: Action = ", check_date_list2$Action, "date 1", check_date_list2$start_date, , "date 1", check_date_list2$end_date))
    return(check_date_list2)
  }

# connectes to remote database
remote_Connect <- function(){
  print("remote_Connect 65")
  remoteuserpassword <- "m3t1sz"
  conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
  return(conR)
}


  ##########
  #
  #Read remote database
  #
  #Retrieves NEW data items as selected in current sidebar
  # and returns global dataframe additional items as read_Remote()
  #
  ##########

read_Remote <- function(inserted_date_seq){
  print("read_RemoteDb 81")
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
                 msg2 <- paste(i, " of ", length(inserted_date_seq))
                 incProgress(detail = msg2 )

#                 print(paste("Date under retrieval:", inserted_date))
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
                     #                   message(queryScript)
                   },
                   finally = {
                     #                     message("tryCatch database read finished")
                     incProgress(1/length(inserted_date_seq))
                   }
                 )       # end of tryCatch
               }         # end of for loop

  )   # End of withProgress
  index <- match(data_selection_frame_append$country, src_reg$Country)
  data_selection_frame_append$region <- src_reg[index, 2]
  return(data_selection_frame_append)
}  # End of read_Remote()

###########
#
# Imported functions
#
#
##########

# selects data for current
rssSelection <- function(rssSelected,  Source, Orientation, SourceType, Country, Region, Topic){
  print("rssSelection() 141")
  # rssSelected <- rssSelected <- dplyr::filter(rssSelected, item_date_published <= end_date)
  # rssSelected <- rssSelected <- dplyr::filter(rssSelected, item_date_published >= start_date)
 # browser()rss
  print(paste("Function: rssSelection entry  nrows() = ", nrow(rssSelected) ))
  ifelse(is.null(Source), rssSelected <- rssSelected,
         rssSelected <- dplyr::filter(rssSelected, Source == ext_name))
  print(paste("Function: rssSelection S1  nrows() = ", nrow(rssSelected) ))
  ifelse(is.null(Orientation),  rssSelected <- rssSelected,
         rssSelected <- dplyr::filter(rssSelected, Orientation == orientation))
  print(paste("Function: rssSelection S2  nrows() = ", nrow(rssSelected) ))
  ifelse(is.null(SourceType), rssSelected <- rssSelected,
         rssSelected <- dplyr::filter(rssSelected, SourceType == SourceType))
  print(paste("Function: rssSelection S3  nrows() = ", nrow(rssSelected) ))
  ifelse(is.null(Country), rssSelected <- rssSelected,
         rssSelected <- dplyr::filter(rssSelected, Country  == country))
  print(paste("Function: rssSelection S4  nrows() = ", nrow(rssSelected) ))
  ifelse(is.null(Region), rssSelected <- rssSelected,
         rssSelected <- dplyr::filter(rssSelected, Region == region))
  print(paste("Function: rssSelection S5  nrows() = ", nrow(rssSelected) ))
  ifelse(is.null(Topic), rssSelected <- rssSelected,
         rssSelected<- dplyr::filter(rssSelected, str_detect(rssSelected[,"item_title"], regex(Topic, ignore_case = TRUE))))
  print(paste("Function: rssSelection S6  nrows() = ", nrow(rssSelected) ))
 print("rssSelected")
   return(rssSelected)
}

######### Round all values in heterogenous data frame (homogeneous columns)

round_df <- function(x, digits) {
  print("round_df 164")
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
}



######### Compute
cluster_comp <-function(query_in){
  print("query_in() 177")
  cluster_frame <- unique(query_in[,c('ext_name', 'orientation', 'country')])
  cluster_agg <- aggregate(query_in[,c(7:23)], by = list(ext_name=query_in$ext_name), sum)
  cluster_merge <- merge(cluster_agg, cluster_frame)
  cluster_merge_x <- cluster_merge[,2:18]
  cluster_clean <- cluster_merge_x[,apply(cluster_merge_x, 2, var, na.rm = TRUE) !=0]
  model <- prcomp(cluster_clean)
}

#######

# Compute time series data
posneg <- function(SA_scores){
  print("SA_scores() 190")
  neg <- -sum(SA_scores[SA_scores <0])
  pos <- sum(SA_scores[SA_scores>0 ])
  both <- neg + pos
  count <- length(SA_scores)
  posneg <- -(neg-pos)/count
}

f.sumVals <- function(query_in) {
  print("f.sumVals() 198")
  sumVals <- query_in %>%
    group_by(item_date_published) %>%
    dplyr::summarize(
      syuzhet = sum(syuzhet_score),
      afinn = sum(afinn_score),
      bing = sum(bing_score),
      nrc_anger = sum(nrc_score_anger),
      nrc_anticipation = sum(nrc_score_anticipation),
      nrc_disgust = sum(nrc_score_disgust),
      nrc_fear = sum(nrc_score_fear),
      nrc_joy = sum(nrc_score_joy),
      nrc_positive =sum(nrc_score_positive),
      nrc_negative = sum(nrc_score_negative),
      nrc_sadness = sum(nrc_score_sadness),
      nrc_surprise = sum(nrc_score_surprise),
      nrc_trust = sum(nrc_score_trust),
      loughran_constraining = sum(loughran_frame_constraining),
      loughran_litigious = sum(loughran_frame_litigious),
      loughran_negative = sum(loughran_frame_negative),
      loughran_positive = sum(loughran_frame_positive),
      loughran_uncertain = sum(loughran_frame_uncertain),
      ensemble_posneg = sum(ensemble_posneg)
    )

  sumVals <- dplyr::filter(sumVals, item_date_published >= input$dateRange[1]) # Remove items before selection date
  sumVals <-sumVals %>% gather('syuzhet', 'afinn', 'bing', 'nrc_anger', 'nrc_anticipation', 'nrc_disgust', 'nrc_fear', 'nrc_joy',
                               'nrc_positive', 'nrc_negative', 'nrc_sadness', 'nrc_surprise', 'nrc_trust', 'loughran_constraining',
                               'loughran_litigious', 'loughran_negative', 'loughran_positive', 'loughran_uncertain','ensemble_posneg', key = "factorName", value = 'factorValue')
  sumVals$factorName <- as.factor(sumVals$factorName)
  return(sumVals)
}

# Compute aggregated data for time period

f.totVals <- function(query_in){
  print("query_in 234")
  totVals <- query_in %>% gather(syuzhet_score, afinn_score, bing_score,
                                 nrc_score_anger, nrc_score_anticipation, nrc_score_disgust, nrc_score_fear,
                                 nrc_score_joy, nrc_score_positive, nrc_score_negative,
                                 nrc_score_sadness, nrc_score_surprise, nrc_score_trust,
                                 loughran_frame_constraining, loughran_frame_litigious,
                                 loughran_frame_negative, loughran_frame_positive, loughran_frame_uncertain,
                                 nrc_comp, loughran_comp, ensemble_posneg, key = "factorName", value = 'factorValue')

  totVals$factorName <- as.factor(totVals$factorName)
  totValsSums <- tapply(totVals$factorValue, totVals$factorName, FUN = sum, na.rm = TRUE)
  totValsSums1 <- melt(totValsSums)

  colnames(totValsSums1) <- c("Factor", "Value")

  totValsSums1 <- rbind(totValsSums1,
                        data.frame(Factor = "PosNeg", Value = 0),
                        data.frame(Factor = "nrc", Value = 0),
                        data.frame(Factor = "loughran", Value = 0))
  totValsSumsGp <- c(1,1,1,2,
                     2,2,1,1,
                     2,3,3,3,
                     3,3,3,1,
                     1,3,3,3,
                     1,0,0,0) # Allocates value to groups of factors for (eg) colour mapping

  totValsSums1$group1 <- as.factor(totValsSumsGp)
  totValsSums1[totValsSums1$loughran_frame_negative, 2] <-  99 #totValsSums1[totValsSums1$loughran_frame_negative, 2] * -1
  totValsSums1
}

time_Series_graph <- function(sumVals, gtitle, line_col, point_col, point_fill){
  p <- ggplot(sumVals,
              aes(x = item_date_published, y = rollmean(factorValue, input$dateGrouping, na.pad = TRUE) )) +
    geom_smooth(method = input$ismooth, fullrange = TRUE,se = input$iconfidence, level = input$iconfidenceLevel) +
    xlab("Story date") + ylab("Factor score") +
    theme(legend.position = c(0.1,0.95)) +
    labs(colour = "Methods") +
    theme(legend.title = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size = 8)
    )
  if(isTRUE(input$aColumn)){(p <- p + geom_col(position = "dodge"))}
  if(isTRUE(input$aLine)){(p <- p + geom_line(colour = line_col))}
  if(isTRUE(input$aPoint)){(p <- p + geom_point(size = 4, shape = 22, colour = point_col, fill = point_fill))}

  return(ggplotly(p))
}

###### acf / pacf graphics: Source: https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html

ggplot.corr <- function(data, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE, title_line, ...) {

  if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}

  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
  df1$lag.acf[2] <- 0
  df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
  df1$acfstd[1] <- 0
  df1 <- select(df1, lag, acf, acfstd)

  list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
  df2$pacfstd <- sqrt(1/N)

  if(large.sample.size == TRUE) {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("ACF ", title_line) +
      theme_bw()

    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF", title_line) +
      theme_bw()
  }
  else {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N),
                 colour = "sandybrown",
                 linetype = "dashed") +
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N),
                 colour = "sandybrown",
                 linetype = "dashed") +
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("ACF", title_line) +
      theme_bw()

    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N),
                 colour = "sandybrown",
                 linetype = "dashed") +
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N),
                 colour = "sandybrown",
                 linetype = "dashed") +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF", title_line) +
      theme_bw()
  }
  cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
}


### Select internal DB / dataframe items from global variable

query_out_Date2 <- function(){
  print("query_out_Date2 361")
  queryDate <- as.Date(input$dateRange[1])
  print("server 4 - start of query_out_Date")
  queryDate <- format(as.Date(queryDate), "%Y_%m_%d")
  print(paste("queryDate (1) ", queryDate))

  outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
  outSeq <- format(as.Date(outSeq, "%Y_%m_%d"))
  query_out_frame <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                country = character() , region = character(),
                                syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                hash_value = character())
  error_date <- data.frame(fail_date = character())
  ##############
  #
  #   Read database - default date if initial date unavailable
  #
  ##############
  print("Date Internal dataframe read initiated")
  #### Table dates
  inserted_date_seq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
}

##########
#
#Reactive functions
#
#
##########

# sumvals - sums values by selected

sumVals <-  reactive({
  print("server 3 - start of reactive functions")
  query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry,input$iregion, input$itextinput)
  story_Rows <<- nrow(query_in)
  print(paste("Analysis rows 1 ",nrow(query_in)))
  sumVals_rtn <- f.sumVals(query_in)
  sumVals_rtn

})

sumVals2 <-  reactive({
  query_in <- rssSelection(query_out_Date(), input$isource2, input$iorientation2,input$isourcetype2, input$icountry2, input$iregion2, input$itextinput2)
  story_Rows2 <<- nrow(query_in)
  sumVals_rtn <- f.sumVals(query_in)
  print(paste("Analysis rows 2 ",nrow(query_in)))
  sumVals_rtn
})


#totVals - sums values for total period for each SA factor
totVals <- reactive({
  query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry, input$iregion, input$itextinput)
  totVals_rtn <- f.totVals(query_in)
  totVals_rtn

})

totVals2 <- reactive({
  query_in <- rssSelection(query_out_Date(), input$isource2, input$iorientation2,input$isourcetype2, input$icountry2,input$iregion2, input$itextinput2)
  totVals_rtn <- f.totVals(query_in)
  totVals_rtn
})



##########
#
#Build dataframe for selected dates
#
#
##########

# 1 Get start and end dates

date_selection <- reactive({
  print("date_selection() 407")
  outSeq <- as.character(seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")) # as.character fixes bug
print("FILLER")
  return(outSeq)
})


# Retrieves from remote database current date selection - input to secondary selection
retrieve_Db <- reactive({
  print("retrieve_Db() 416")
  check_action <- check_dates2(input$dateRange[1], input$dateRange[2]) # Removed while developing function
  if(check_action$Action == TRUE){
    print("retrieve_Db: retrieving")
    outSeq <- as.character(seq(as.Date(check_action$start_date) , as.Date(check_action$end_date), by = "day"))
    conR <- remote_Connect()         # CRetrieve records for dates
    data_selection_frame_append  <- read_Remote(outSeq)
    dbDisconnect(conR)
    print("Remote db disconnected")
    data_selection_frame <<- rbind(data_selection_frame, data_selection_frame_append)
    global_start_date <<-input$dateRange[1]}else{
      print("No new db records required")
    }
  data_selection_frame <<- unique(data_selection_frame )

  return(data_selection_frame)
})

##############
#
#   Build full dataframe for dates
#
##############
query_out_Date <- reactive({
  print("query_out_Date 440")
  outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
  outSeq <- format(as.Date(outSeq, "%Y_%m_%d"))
  query_out_frame <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                country = character() , region = character(),
                                syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                hash_value = character())
  error_date <- data.frame(fail_date = character())
  ##############
  #
  #   Read database - default date if initial date unavailable
  #
  # ##############

  print("SERVER - query_out_Date")

  ############ New addition

  action_list_read <- check_dates2(input$dateRange[1], input$dateRange[2])
  # Retrieve records for analysis

  if(action_list_read$Action == TRUE){
    outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
    data_selection_frame <<- rbind(data_selection_frame, read_Remote(outSeq))
    data_selection_frame <<- unique(data_selection_frame)
  }
  red_Tab <- dplyr::filter(data_selection_frame, item_date_published >= input$dateRange[1])
  red_Tab <- dplyr::filter(data_selection_frame, item_date_published <= input$dateRange[2])


  ############

  ### Insert new section here
  query_out_frame <- filter(red_Tab, item_date_published >= input$dateRange[1])
  query_out_frame <- filter(query_out_frame, item_date_published <= input$dateRange[2])

  ### End of new section
  # Normalize values for ensemble positive / negative

  query_out_frame$nrc_comp <- query_out_frame$nrc_score_positive - query_out_frame$nrc_score_negative
  query_out_frame$loughran_comp <- query_out_frame$loughran_frame_positive - query_out_frame$loughran_frame_negative
  afinn.norm <- max(abs(query_out_frame$afinn_score))
  syuzhet.norm <- max(abs(query_out_frame$syuzhet_score))
  bing.norm <- max(abs(query_out_frame$bing_score))
  nrc.norm <- max(abs(query_out_frame$nrc_comp))
  loughran.norm <- max(abs(query_out_frame$loughran_comp))
  query_out_frame$ensemble_posneg <- query_out_frame$afinn_score/afinn.norm + query_out_frame$bing_score/bing.norm + query_out_frame$syuzhet_score/syuzhet.norm +
    query_out_frame$nrc_comp/nrc.norm + query_out_frame$loughran_comp/loughran.norm

  query_out_frame <- cbind(query_out_frame, rssSources[match(query_out_frame$ext_name, rssSources$Feed), c(6,7)]) # Add region and source type
  print("End of query_out_Date")
  query_out_frame # returned

  # end of read DB loop
})

############################

##############
#
#   Get data for all graphics
#
# ##############

data_for_graphs <- function(date1, date2){
  action_list <- check_dates2(date1, date2)
    if(action_list$Action == TRUE){
      print("retrieve_Db: retrieving")
      outSeq <- as.character(seq(as.Date(date1) , as.Date(date2), by = "day"))
      conR <- remote_Connect()         # CRetrieve records for dates
      data_selection_frame_append  <- read_Remote(outSeq)
      dbDisconnect(conR)
      print("Remote db disconnected")
      data_selection_frame <<- rbind(data_selection_frame, data_selection_frame_append)
      data_selection_frame <<- unique(data_selection_frame )
      global_start_date <<-input$dateRange[1]}else{
        print("No new db records required")
      }
    return(data_selection_frame <<- unique(data_selection_frame ))
  }





############################ DEVELOPMENT ONLY BELOW

# Small return table for test purposes
list_head_DB <- reactive({
  small_out <- data_for_graphs(input$dateRange[1], input$dateRange[2])

  small_out$item_date_published <- as.character(small_out$item_date_published)
  small_out <- filter(small_out, item_date_published >= input$dateRange[1])
  small_out <- filter(small_out, item_date_published <= input$dateRange[2])
#  small_out <- dplyr::select(small_out, ext_name, item_date_published)
  return((small_out))
})

##########
#
#     ########   ######    ######   ######   #    #   #   ######   ######
#     #          #     #   #    #   #    #   #    #   #   #        #
#     #    ##    ####      ######   ######   ######   #   #        ######
#     #     #    #    #    #    #   #        #    #   #   #             #
#     ########   #     #   #    #   #        #    #   #   ######   ######
#
##########

output$dataSelection <- renderTable(date_selection()) # Does nothing


output$tbl <- DT::renderDT({
  print("Final table")
},
caption = "Standard Statistics",
options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = TRUE))

#############
output$reduced_Table <- DT::renderDT({

  print("reduced_Table Server 550")

  action_list_read <- check_dates2(input$dateRange[1], input$dateRange[2])
  # Retrieve records for analysis

  if(action_list_read$Action == TRUE){
    outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
    data_selection_frame <<- rbind(data_selection_frame, read_Remote(outSeq))
    data_selection_frame <<- unique(data_selection_frame)
  }
  red_Tab <- dplyr::filter(data_selection_frame, item_date_published >= input$dateRange[1])
  red_Tab <- dplyr::filter(data_selection_frame, item_date_published <= input$dateRange[2])
  head(red_Tab)
  # p <- list_head_DB()
  # p
  }#,

  ) # This is being printed

##################### FIRST OUTPUT TAB - "Comparison"
output$SA_by_date_line_comp <- renderPlotly({
#  browser()
  sumValsA <- dplyr::filter(sumVals(), factorName %in% input$iSentimentFactor )
  sumValsA <-mutate(sumValsA, Selection = "S1")
  sumValsB <- dplyr::filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
  sumValsB <-mutate(sumValsB, Selection = "S2")
  sumVals <- rbind(sumValsA, sumValsB)
  if(isTRUE(input$iPosNegNorm)){
    sumVals$factorValue <- sumVals$factorValue * posneg(sumVals$factorValue)
  }

  p <- sumVals %>%
    mutate(mov_avg = rollmean(factorValue, input$dateGrouping, fill = 0)) %>%
    ggplot(aes(x = item_date_published, y = factorValue, group = Selection, fill = Selection, colour = Selection)) +
    xlab("Story date") + ylab("Factor score") +
    theme(legend.position = c(0,0)) +
    geom_smooth(method = input$ismooth, fullrange = TRUE,  show.legend = TRUE,se = input$iconfidence,
                level = input$iconfidenceLevel, aes(colour = Selection)) +
    ggtitle(paste("Time series analysis","\nR/A = ", input$dateGrouping)) +
    theme(legend.title = element_text(size = 8),
          legend.position = c(0,0),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size = 12)
    )

  if(isTRUE(input$aColumn)){(p <- p + geom_col(position = "dodge"))}
  if(isTRUE(input$aLine)){(p <- p + geom_line())}
  #        if(isTRUE(input$aDensity)){(p <- p + geom_density(aes(y = factorValue)))}
  if(isTRUE(input$aPoint)){(p <- p + geom_point(size = 4, shape = 22, colour = "darkblue", fill = "azure"))}
  p + theme(legend.position = c(0.1, 0.1))
  p
})

output$analysisStats <- renderTable({
#browser()
src1 <- nrow( dplyr::filter(sumVals()))
src2 <- nrow(dplyr::filter(sumVals2()))

outputA <- ifelse(is.null(input$icountry), "All",input$icountry)
outputB <- ifelse(is.null(input$iregion), "All",input$iregion)
outputC <- ifelse(is.null(input$iorientation), "All",input$iorientation)
outputD <- ifelse(is.null(input$isourcetype), "All",input$isourcetype)
outputE <- ifelse(is.null(input$itextinput)|is.na(input$itextinput)|input$itextinput=="", "All", input$itextinput)
outputF <-  story_Rows
outputG <- ifelse(is.null(input$isource), "All",input$isource)
outputH <- input$iSentimentFactor
outputL1 <- c("1", outputH, outputA, outputB, outputC, outputD, outputG, outputE, outputF)

outputA2 <- ifelse(is.null(input$icountry2), "All",input$icountry2)
outputB2 <- ifelse(is.null(input$iregion2), "All",input$iregion2)
outputC2 <- ifelse(is.null(input$iorientation2), "All",input$iorientation2)
outputD2 <- ifelse(is.null(input$isourcetype2), "All",input$isourcetype2)
outputE2 <- ifelse(is.null(input$itextinput2)|is.na(input$itextinput2)|input$itextinput2=="", "All", input$itextinput2)
outputF2 <- story_Rows2
outputG2 <- ifelse(is.null(input$isource2), "All",input$isource)
outputH2 <- input$iSentimentFactor2
outputL2 <- c("2",outputH2, outputA2, outputB2, outputC2, outputD2, outputG2, outputE2, outputF2)

outputX <- rbind(outputL1, outputL2)
#outputX <- data.frame(Country = outputA, Region = outputB, Orientation= outputC, Source =  outputD, Text = outputE)
# outputX <- data.frame(Country = "UKX", Region = "AsaiX", Orientation= "centre", Source =  "Press", Text = "Textexample")
print(outputX)
rownames(outputX) <- c("Sel 1", "Sel 2")
colnames(outputX) <- c("Selection", "Method", "Country", "Region", "Orient", "Type","Source", "Text", "Stories")
outputX
})


##################### SECOND OUTPUT TAB - "Individual"

output$SA_by_date_line <- renderPlotly({
  z <- c(input$aColumn, input$aLine, input$aPoint)
  sumVals <- dplyr::filter(sumVals(), factorName %in% input$iSentimentFactor )
  gtitle <- paste("Time series analysis / \nMoving average 1 \nComparison", input$ismooth)
  p <- time_Series_graph(sumVals, gtitle, "red", "firebrick4", point_fill = "deeppink")
  p
})


# Second choice line chart
output$SA_by_date_line2 <- renderPlotly({
  sumVals <- dplyr::filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
  ggtitle <- paste(c("Time series analysis Moving average ", input$ismooth))
  p <- time_Series_graph(sumVals, gtitle, "blue", "deepskyblue", point_fill = "dodgerblue4")
  p
})


#########################

output$SA_summary_by_period <-renderPlotly({
  print("SA_s_b_P")
  x_tick_titles <- ("this")
  q <- ggplot(totVals(), aes(x = Factor, y = Value, fill = group1, colour = "red"))+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Period analysis / Sentiment") +
    scale_x_discrete(limits = c('PosNeg', 'ensemble_posneg', 'afinn_score', 'bing_score', 'syuzhet_score', 'loughran_frame_positive', 'loughran_frame_negative',
                                'nrc_score_positive', 'nrc_score_negative','Sentiment spectrum',
                                'nrc_score_anger', 'nrc_score_anticipation', 'nrc_score_disgust', 'nrc_score_fear',
                                'nrc_score_joy',
                                'nrc_score_sadness', 'nrc_score_surprise', 'nrc_score_trust', 'Financial',
                                'loughran_frame_constraining', 'loughran_frame_litigious', 'loughran_frame_uncertain'
    )) +
    geom_bar(stat = "identity", colour = "black") +
    theme(legend.position = "none")

  q
})

output$SA_summary_by_period2 <-renderPlotly({
  print("server 5 - SA_summary_by_period")
  x_tick_titles <- ("this")
  q <- ggplot(totVals2(), aes(x = Factor, y = Value, fill = group1, colour = "blue"))+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Period analysis / Sentiment 2") +
    scale_x_discrete(limits = c('PosNeg', 'ensemble_posneg', 'afinn_score', 'bing_score', 'syuzhet_score', 'loughran_frame_positive', 'loughran_frame_negative',
                                'nrc_score_positive', 'nrc_score_negative','nrc',
                                'nrc_score_anger', 'nrc_score_anticipation', 'nrc_score_disgust', 'nrc_score_fear',
                                'nrc_score_joy',
                                'nrc_score_sadness', 'nrc_score_surprise', 'nrc_score_trust', 'loughran',
                                'loughran_frame_constraining', 'loughran_frame_litigious', 'loughran_frame_uncertain'
    )) +
    geom_bar(stat = "identity", colour = "black") +
    theme(legend.position = "none")


})

######################  THIRD OUTPUT TAB - "Correlation"

########  Correlation plot
output$SA_correlation <- renderPlotly({
  print("server 5 - SA_correlation")
  sumValsA <- dplyr::filter(sumVals(), factorName %in% input$iSentimentFactor )
  sumValsA <- mutate(sumValsA, SelectionA = "Selection 1")
  sumValsA <- mutate(sumValsA, rank_SFactorA = rank(factorValue))
  sumValsB <- dplyr::filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
  sumValsB <- mutate(sumValsB, SelectionB = "Selection 2")
  sumValsB <- mutate(sumValsB, rank_SFactorB = rank(factorValue))
  sumValsX <- sumValsA %>%
    inner_join(sumValsB, by = c("item_date_published", "factorName"))
  sumValsX$index <- tibble::rowid_to_column(sumValsX, "index")




  p <- ggscatter(sumValsX, x = "factorValue.x", y = "factorValue.y",
                 cor.method = input$icorrelate,
                 conf.int = input$iconfidence,
                 add = switch(input$ismooth, "None" = "", "loess" = "loess", "lm" = "reg.line"),
                 add.params = list(color = "blue"),
                 star.plot = input$aStar,

  ) +
    ggtitle(paste("Correlation")) +
    xlab("Selection 1") + ylab("Selection 2") +
    theme(legend.title = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12)
    )

  if(isTRUE(input$aPoint)){(p <- p + geom_point(size = 4, shape = 22, colour = "darkgreen", fill = "darkseagreen"))}
  #       p + stat_cor( method = input$icorrelate, aes(label = ..r.label.., label.x = 3, label.y = 30),output.type = "expression", p.accuracy = 0.001, r.accuracy = 0.001)
  return(p)
})  ######  End of correlation plot



#######  Correlation statistics table

output$corrStats <- DT::renderDataTable({
  v1 <- c(input$isource,input$isourcetype, input$icountry,input$iregion,  input$iorientation, input$itextinput )
  v2 <- c(input$isource2, input$isourcetype,input$icountry2, input$iregion2, input$iorientation2, input$itextinput2 )
  s <- c(input$icorrelate, input$icorr.alternative)

  sumValsA <- dplyr::filter(sumVals(), factorName %in% input$iSentimentFactor )
  sumValsA <- mutate(sumValsA, SelectionA = "Selection 1")
  sumValsA <- mutate(sumValsA, rank_SFactorA = rank(factorValue))
  sumValsB <- dplyr::filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
  sumValsB <- mutate(sumValsB, SelectionB = "Selection 2")
  sumValsB <- mutate(sumValsB, rank_SFactorB = rank(factorValue))
  sumValsX <- sumValsA %>%
    inner_join(sumValsB, by = c("item_date_published", "factorName"))
  sumValsX$index <- tibble::rowid_to_column(sumValsX, "index")
  #        t <- cor(x = sumValsX$factorValue.x, y = sumValsX$factorValue.y, method = s)
  correlation_result <- cor.test(sumValsX$factorValue.x, sumValsX$factorValue.y,
                                 method = input$icorrelate, alternative = input$icorr.alternate)

  method <- correlation_result$method
  alternative <- correlation_result$alternative
  statistic <-  round_df(correlation_result$statistic, 4)
  p.value <- round_df(correlation_result$p.value,4)
  estimate <- round_df(correlation_result$estimate,4)
  ifelse(input$icorrelate == "pearson", conf.int <- paste(round_df( correlation_result$conf.int[1:1], 4), "\n",
                                                          round_df(correlation_result$conf.int[1:2],4)),
         conf.int <- NA)


  cor.text <- as.data.frame(do.call(rbind,
                                    list( method, alternative,statistic,
                                          p.value, estimate, conf.int)))
  row.names(cor.text) <- c( "Method", "Alternative","Statistic", "p-value", "estimate", "conf int")
  if(input$icorrelate == "pearson"){cor.text <- select(cor.text, V2)}
  cor.text #<- round_df(cor.text, 4)
},
caption = "Correlation Statistics",
options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))

####################### FOURTH OUTPUT TAB - "Autocorrelation"

output$ACF1_large <- renderPlot({
  v1 <- c(input$isource,input$isourcetype, input$icountry,input$iregion,  input$iorientation, input$itextinput )
  v2 <- c(input$isource2, input$isourcetype,input$icountry2, input$iregion2, input$iorientation2, input$itextinput2 )
  s <- c(input$icorrelate, input$icorr.alternative)

  sumVals <- dplyr::filter(sumVals(), factorName %in% input$iSentimentFactor )
  print("ACF_1 large")
  p <- ggplot.corr(sumVals$factorValue, lag.max = 124, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE, title_line = "Selection 1") +
    ggtitle("Autocorrelation / Selection 1")

  p
})

output$ACF2_large <- renderPlot({

  sumVals <- dplyr::filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
  print("ACF2_large")
  p <- ggplot.corr(sumVals$factorValue, lag.max = 124, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE, title_line = "Selection 2") +
    ggtitle("Autocorrelation / Selection 2")

  p
})


##################### FIFTH OUTPUT TAB - " Principal compomnentn analysis"
# Biplot chart
output$PCA <- renderPlot({
  query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry,input$iregion, input$itextinput)
  model <- cluster_comp(query_in)
  p <- ggbiplot(model, labels = rownames(model$scores) )
  p + ggtitle("Principal Components Analysis")
})
# Scree chart
output$PCA_scree <-renderPlot({
  query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry,input$iregion, input$itextinput)

  model <- cluster_comp(query_in)
  #      q <- ggplot(model) + geom_bar(stat = "identity")
  p <- plot(model, main = "PCA scree", las = 2)
})
# Factor analysis
output$PCA_tab <- DT::renderDT({
  query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry,input$iregion, input$itextinput)
  cluster_frame <- unique(query_in[,c('ext_name', 'orientation', 'country')])
  print("PCA_tab")
  cluster_agg <- aggregate(query_in[,c(7:23)], by = list(ext_name=query_in$ext_name), sum)
  cluster_merge <- merge(cluster_agg, cluster_frame)
  cluster_merge <- cluster_merge[,2:18]
  cluster_merge <- cluster_merge[,apply(cluster_merge, 2, var, na.rm = TRUE) !=0]
  cluster_analysis <- factanal(cluster_merge, factors = input$iPCAcount)
  PCA_tab_out <- cluster_analysis$loadings
  r_l <- rownames(PCA_tab_out)
  c_l <- colnames(PCA_tab_out)
  PCA_tab_out_df <- as.data.frame(matrix(PCA_tab_out, length(rownames(PCA_tab_out)), length(colnames(PCA_tab_out))))
  PCA_tab_out_df <- round_df(PCA_tab_out_df, 3)
  rownames(PCA_tab_out_df) <- rownames(PCA_tab_out)
  colnames(PCA_tab_out_df) <- colnames(PCA_tab_out)
  round_df(PCA_tab_out_df, 4)
},
caption = "Principal Component Analysis - Factors Analysis",
options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))
#################### SIXTH OUTPUT TAB - "Source"
output$tbl <- DT::renderDT({
  print("Final table")
  stories1 <- rssSelection(query_out_Date(),  input$isource,input$isourcetype, input$orientation, input$icountry, input$iregion, input$itextinput)
  stories2 <- rssSelection(query_out_Date(),  input$isource2,input$isourcetype2, input$orientation2, input$icountry2, input$iregion2, input$itextinput2)
  stories <- rbind(stories1, stories2)
  stories
},
options  = list(scrollX = TRUE,
                lengthChange = TRUE,
                columnDefs = list(list(width = '800px', targets = 1:4))
))




#######################
})
