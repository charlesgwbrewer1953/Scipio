# Scipio

# Initialization of variables and initial empty dataframe
library(tibble)
library(rlang)
library(tidyverse)
library(RMariaDB)
rm(list = ls())

print( "Global 0 - About to connect - Server/Remote 1")
# Establish connection to Digital Ocean (remote) database
print("Global 1 - Pre creation of conR")

remoteuserpassword <- "m3t1sz"
conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
print("Connected remote 1")
dbListTables(conR)



global_start_date <- Sys.Date()
global_end_date <- Sys.Date()
first_pass = TRUE
story_Rows <- 0
story_Rows2 <- 0
data_selection_frame <- tibble(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                   country = character() , region = character(),
                                   syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                   nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                   nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                   nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                   loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                   loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                   hash_value = character(), stringsAsFactors = FALSE)

data_selection_frame$item_date_published <- as.Date(data_selection_frame$item_date_published, format("%Y-%m-%d"))

dbQuery <- dbSendQuery(conR, "SELECT * FROM rssSources")
rssSources <- dbFetch(dbQuery)
print("RSS Feeds static data retrieved")
rssSources.names <- unique(dplyr::select(rssSources,Feed))
rssSources.names <- as_tibble(sort(rssSources.names[,1]))
colnames(rssSources.names) <- "cname"
rssSources.names$cname <- as_utf8_character(rssSources.names$cname)
rss.SourceTypes <- unique(dplyr::select(rssSources,SourceType))
rss.SourceTypes <- sort(rss.SourceTypes[,1])
rss.Countries <- unique(dplyr::select(rssSources,Country))
rss.Countries <- sort(rss.Countries[,1])
rss.Regions <- unique(dplyr::select(rssSources,Region))
rss.Regions <- sort(rss.Regions[,1])
rss.Orientation <- unique(dplyr::select(rssSources,Orientation))
rss.Orientation <- sort(rss.Orientation[,1])
rss.Lookups <- unique(dplyr::select(rssSources,URL, Orientation))
src_reg <- unique(dplyr::select(rssSources, Country, Region))
print(conflicts(detail = TRUE))
