# check installed packages and install only necessary ones ####
c_necessary_packages <- c(
  'bupaR',
  'edeaR',
  'processmapR',
  'eventdataR',
  'readr',
  'tidyverse',
  'DiagrammeR',
  'ggplot2',
  'stringr',
  'lubridate',
  'processmonitR'
)
c_missing_packages <- c_necessary_packages[!(c_necessary_packages %in% installed.packages()[,"Package"])]
if(length(c_missing_packages) > 0) install.packages(c_missing_packages)


# load libraries ####
library(bupaR)
library(edeaR)
library(processmapR)
library(eventdataR)
library(readr)
library(tidyverse)
library(DiagrammeR)
library(ggplot2)
library(stringr)
library(lubridate)
library(processmonitR)
library(shiny)

# load BPI Challenge 2017 data set ####
data <- readr::read_csv('./RScripts/loanapplicationfileTest.csv',
                        locale = locale(date_names = 'en',
                                        encoding = 'ISO-8859-1'))

# change timestamp to date var
data$starttimestamp2 = as.POSIXct(data$`starttimestamp`, 
                                 format = "%d/%m/%Y %H:%M")

data$endtimestamp2 = as.POSIXct(data$`endtimestamp`, 
                               format = "%d/%m/%Y %H:%M")

# remove blanks from var names
names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))

events <- bupaR::activities_to_eventlog(
  head(data, n=100),
  case_id = 'Case_ID',
  activity_id = 'Activitys',
  resource_id = 'Resource',
  timestamps = c('starttimestamp2', 'endtimestamp2')
)

cases <- cases(events)[1]

ui <- fluidPage(
        navbarPage("Process Mining",
                           tabPanel("Graph",
                                    fluidRow(
                                      column(4,
                                        selectInput("inSelect", "Select case",cases),
                                        sliderInput(inputId="setGraphActFreq", label="Activity Frequency", min=0.01, max=1, value=1),
                                        sliderInput(inputId="setGraphTraceFreq", label="Trace Frequency", min=0.01, max=1, value=0)),
                                      column(8,
                                        grVizOutput(outputId = "graph")))),
               tabPanel("Variants"),
               tabPanel("Data overview")))
  

server <- function(input,output) {
  output$graph <- reactive({ events %>%
    filter_activity_frequency(percentage = input$setGraphActFreq) %>% # show only most frequent activities
    filter_trace_frequency(percentage = input$setGraphTraceFreq) %>%    # show only the most frequent traces
    process_map(render = T) })
}
shinyApp(ui = ui, server = server)
