#
# Scipio
#

library(shinydashboard)
Sys.setlocale('LC_ALL','C')
library(shinythemes)
library(shinyWidgets)
library(shinysky)
library(dashboardthemes)
library(bslib)
library(tidyverse)
library(plotly)
library(RMariaDB)
library(rlang)

library(DBI)
library(zoo)
library(reshape2)

library(MASS)
library(plotly)
library(ggpubr)
library(ggbiplot)
library(ggrepel)
library(bslib)
library(dashboardthemes)
library(DT)
library(cowplot)

dashboardPage(
  skin = "green",
    dashboardHeader(
        title = "Media Sentiment Analysis- ", titleWidth = 450
    ),   # End of dashboardHeader
#####
              dashboardSidebar(
                  width = 275,
                  shinyDashboardThemes(theme = "purple_gradient"),
                  dateRangeInput(inputId= 'dateRange',
                                 label = "Date range",
                                 start = Sys.Date() - 7,
                                 format = "yyyy_mm_dd"),
                  numericInput(inputId = "dateGrouping",
                               "Rolling average ",
                               value = 5,
                               min = 1,
                               max = 90),

############################


                  dropdown(
                    label = "Selection 1",
                    tags$h3("Selection 1"),


                    selectizeInput("icountry",
                                   "Country 1",
                                   choices = rss.Countries,
                                   multiple = TRUE),
                    selectizeInput("iregion",
                                   'Region 1', choices = rss.Regions,
                                   multiple = TRUE),
                    selectizeInput("iorientation",
                                   "Orientation 1",
                                   choices = rss.Orientation,
                                   multiple = TRUE),
                    selectizeInput("isourcetype",
                                   "Source Type 1",
                                   choices = rss.SourceTypes,
                                   multiple = TRUE),
                    selectizeInput("iSentimentFactor",
                                   "Sentiment factor 1",
                                   c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                                     "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                                     "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                                     "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
                                   multiple = FALSE,
                                   selected = "ensemble_posneg"),
                    selectizeInput("isource",
                                   "Source  1",
                                   choices = rssSources.names,
                                   multiple = TRUE),
                    textInput("itextinput",
                              "Text selection 1",
                              value = " ")

                  ),
 #Dropdown Selection 1
#################
                  dropdown(
                    tooltip = TRUE,
                    label = "Selection 2",
                    tags$h3("Selection 2"),

                    selectizeInput("icountry2",
                                   "Country 2",
                                   choices = rss.Countries,
                                   multiple = TRUE),
                    selectizeInput("iregion2",
                                   'Region 2', choices = rss.Regions,
                                   multiple = TRUE),
                    selectizeInput("iorientation2",
                                   "Orientation 2",
                                   choices = rss.Orientation,
                                   multiple = TRUE),
                    selectizeInput("isourcetype2",
                                   "Source Type 2",
                                   choices = rss.SourceTypes,
                                   multiple = TRUE),
                    selectizeInput("iSentimentFactor2",
                                   "Sentiment factor 2",
                                   c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                                     "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                                     "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                                     "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
                                   multiple = FALSE,
                                   selected = "ensemble_posneg"),
                    selectizeInput("isource",
                                   "Source  2",
                                   choices = rssSources.names,
                                   multiple = TRUE),
                    textInput("itextinput2",
                              "Text selection 2",
                              value = " ")

                  ), #Íourccetype, Country, Region, Orientation, Sentiment factor, Text
##############
                  dropdown(
                    tootip = TRUE,
                    label = "Smooth/Corr",
                    tags$h3("Smoothing"),
                    radioButtons("ismooth", "Method",
                                 c("None"= "", "loess" = "loess", "lm" = "lm","gam" = "gam", "glm" = "glm", "MASS:rlm" = "MASS:rlm" )),

                    numericInput("iconfidenceLevel", label = "Confidence value", value = 0.95, min = 0, max = 1, step = 0.05, width = "30%" ),
                    checkboxInput("iconfidence", label = "On", FALSE),
                    tags$h3("Correlation"),
                    selectizeInput("icorrelate", label = "Method", c("pearson", "kendall", "spearman"), multiple = FALSE),
                    selectizeInput("icorr.alternate", label = "Alternative", c("two.sided", "greater", "less"), multiple = FALSE),
                    numericInput("iPCAcount", label = "PCA factors", value = 3, min = 1, max = 8)
                  ), # Tooltip# Sommthinng
###################
                  dropdown(
                    tooltip = TRUE,
                    label = "Moving averages",
                    selectizeInput("iMSMethod",
                                   "Moving avg method",
                                   c("None" = "None",
                                     "Simple MA" = "SMA", "Exponential MA"= "EMA",
                                     "Weighted MA" = "WMA", "Double exponential MA" = "DEMA",
                                     "Zero-lag exponential MA" = "ZLEMA", "Volume wieghted MA" = "VLEMA",
                                     "Elastic volume-weighted MA" = "EVWMA"),
                                   selected = "None",
                                   multiple = FALSE),
                    numericInput("iMA1",
                                 "Moving avg 1",
                                 value = 5,
                                 min = 1,
                                 max = 100),
                    numericInput("iMA2",
                                 "Moving avg 2",
                                 value = 5,
                                 min = 1,
                                 max = 100)

                  ),

###################

                  dropdown(
                    tooltip = TRUE,
                    label = "Normalize",
                    fluidRow(
                      tags$h3("Normalise"),
                      checkboxInput("iPosNegNorm", "Pos/neg"),
                      checkboxInput("iLRCNorm", "Orientation"),
                      checkboxInput("iCountryNorm", "Countries"))
                  ),
################
                  dropdown(
                    tooltip = TRUE,
                    label = "Format",
                    fluidRow(
                      tags$h3("Chart format"),
                      tags$h5("Time Series"),
                      checkboxInput("aColumn", "Column", FALSE),
                      checkboxInput("aLine", "Line", TRUE),
                      checkboxInput("aPoint", "Points", FALSE),
                      tags$h5("Correlation"),
                      checkboxInput("aStar", "Star", FALSE)
                    )

                  ),
                  sidebarMenu(
                    menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale-left")),
                    menuItem("Individual", tabName = "individual", icon = icon("chart-line")),
                    menuItem("Correlation", tabName = "correlation", icon = icon("chart-line")),
#                    menuItem("Autocorrelation", tabName = "autocorr", icon = icon("chart-bar")),
                    menuItem("Cluster", tabName = "cluster", icon = icon("cogs")),
                    menuItem("Source", tabName = "source", icon = icon("dashboard"))
                  )
                  ###########################
              ), # End of sidebar

####################
#
#
# Main body
#
#
####################


###################
#
#    #####   #  ######  ####   #        ####     #     #
#    #    #  #  #       #   #  #       #    #     #   #
#    #    #  #  ######  ####   #       ######       #
#    #    #  #       #  #      #       #    #       #
#    #####   #  ######  #      ######  #    #       #
#
#################3
    dashboardBody(
      tabItems(
        tabItem(tabName = "comparison",

        fluidRow(
          h4("Comparative content"),
            plotOutput("SA_by_date_line_comp")),
        fluidRow(
          h4("Selection"),
            dataTableOutput("analysisStats")
        )


  #      fluidRow(tableOutput("tbl"))
        ),
      tabItem(tabName = "individual",

              h4("Selection 1"),
              fluidRow(
                column(width = 6, plotlyOutput("SA_by_date_line")),

                column(width = 6, plotlyOutput("SA_summary_by_period"))),
              tableOutput("analysisStatsA"),
              h4("Selection 2"),
              fluidRow(
                column(width = 6, plotlyOutput("SA_by_date_line2")),
                column(width = 6, plotlyOutput("SA_summary_by_period2"))),
              tableOutput("analysisStatsB")
              # End of tab 2
      ),
      tabItem(tabName = "correlation",
              fluidRow(
                h4("Statistics"),
                column(width = 8, plotlyOutput("SA_correlation")),
                column(width = 4, DT::dataTableOutput("corrStats")))
      ),

      tabItem(tabName = "autocorr",
              fluidRow(
                h4("Autocorrelation"),
                column(width = 12, plotOutput("ACF1_large"))
              ),
              fluidRow(
                column(width = 12, plotOutput("ACF2_large"))
              )
      ),

      tabItem(tabName = "cluster",
              fluidRow(
                h4("Principal Components"),

                column(width = 6, plotOutput("PCA")),
                column(width = 6, plotOutput("PCA_scree"))
              ),
              fluidRow(column(width = 12, DT::dataTableOutput("PCA_tab")))

      ),
      tabItem(tabName = "source",
              h4("Source"),
                DT::dataTableOutput("tbl")
      ))
              )) # End of tabItem()# End of tabItem()
    # ) #End of tabItems
    #
    # ) # End of dashboardPage
