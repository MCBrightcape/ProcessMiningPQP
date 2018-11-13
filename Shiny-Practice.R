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



ui <- fluidPage(
        navbarPage("Process Mining",
                           tabPanel("Graph",
                                    fluidRow(
                                      column(3,
                                        selectInput("inSelect", "Select case", seq(0,20,1), selectize = FALSE),
                                        sliderInput(inputId="setGraphActFreq", label="Activity Frequency", min=0.01, max=1, value=0.01),
                                        sliderInput(inputId="setGraphTraceFreq", label="Trace Frequency", min=0.01, max=1, value=0.01),
                                        textOutput("giveme")),
                                      column(9,
                                        grVizOutput('processGraph')))),
               tabPanel("Variants"),
               tabPanel("Data overview",
                        fileInput("file1", "Choose CSV File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
                        dataTableOutput("contents")
                        )))
  

server <- function(input,output,session) {
  options(shiny.maxRequestSize=30*1024^2)
  
  # load BPI Challenge 2017 data set ####
  data <- readr::read_csv('loanapplicationfileTest.csv',
                          locale = locale(date_names = 'en',
                                          encoding = 'ISO-8859-1'))
  
  # change timestamp to date var
  data$starttimestampFormatted = as.POSIXct(data$`starttimestamp`,
                                            format = "%d/%m/%Y %H:%M")
  
  data$endtimestampFormatted = as.POSIXct(data$`endtimestamp`,
                                          format = "%d/%m/%Y %H:%M")
  
  # remove blanks from var names
  names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))
  
  events <- bupaR::activities_to_eventlog(
    head(data, n=10000),
    case_id = 'Case_ID',
    activity_id = 'Activity',
    resource_id = 'Resource',
    timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
  )
  
  
  variants <- traces(events)[order(-traces(events)$relative_frequency),]
  X <- vector(mode="character")
  
  for (i in 1:nrow(variants)){
    combined <- paste(c(i, substr(variants[i,3],0,6)), collapse = ":  Relative freq: ")
    X[i] <- c(combined)
  }
                  
  updateSelectInput(session, "inSelect",
                    choices = X
                    )
  
  output$giveme <- reactive({input$inSelect})
  
  # output$processGraph <-  renderGrViz({
  #   req(input$inSelect)
  #   events %>% filter_case(cases = c(input$inSelect)) %>%
  #   filter_activity_frequency(percentage = input$setGraphActFreq) %>% # show only most frequent activities
  #   filter_trace_frequency(percentage = input$setGraphTraceFreq) %>%     # show only the most frequent traces
  #   process_map(render = T)})
  
  output$contents <- renderDataTable({
    req(input$file1)
    inFile <- input$file1
    read.csv(inFile$datapath)
  })
}
shinyApp(ui = ui, server = server)
