# Scipio

# Initialization of variables and initial empty dataframe
global_start_date <- Sys.Date()
global_end_date <- Sys.Date()
first_pass = TRUE
data_selection_frame <- tibble(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                   country = character() , region = character(),
                                   syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                   nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                   nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                   nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                   loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                   loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                   hash_value = character(), stringsAsFactors = FALSE)


