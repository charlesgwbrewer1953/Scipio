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
library(plotly)

dashboardPage(
    dashboardHeader(
        title = "Media Sentiment Analysis- ", titleWidth = 450
    ),
              dashboardSidebar(
                  width = 275,
                  shinyDashboardThemes(theme = "purple_gradient"),
                  dateRangeInput(inputId= 'dateRange',
                                 label = "Date range",
                                 start = Sys.Date() - 14,
                                 format = "yyyy_mm_dd"),
                  numericInput(inputId = "dateGrouping",
                               "Rolling average ",
                               value = 5,
                               min = 1,
                               max = 90)

              ), # End of sidebar

    dashboardBody(
        fluidRow(tableOutput("dateSelection"), tableOutput("date_lookup"))
    )
    ) # End of dashboardPage
