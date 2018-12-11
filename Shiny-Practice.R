source("ServerInit.R")
source("DataInit.R")

ui <- fluidPage(style = c("height:100%", "width:100%"),
        navbarPage("Process Mining",
               tabPanel("Graph",
                        fluidRow(
                          column(2,
                            selectInput(inputId="visType", "Select type", c("Frequency","Performance")),
                            sliderInput(inputId="setGraphActFreq", label="Activity Frequency", min=0.01, max=1, value=0.01),
                            sliderInput(inputId="setGraphTraceFreq", label="Trace Frequency", min=0.01, max=1, value=0.01),
                            selectInput(inputId="measureType", "Select type", c("mean","median","min","max"), selected = "mean"),
                            selectInput(inputId="durationType", "Select type", c("mins", "hours", "days", "weeks"), selected = "days")),
                          column(10,
                              grVizOutput('processGraphVisual', height = "800px")))),
               tabPanel(id = "traceViewer", "Trace Viewer",
                        plotOutput("traceExplorer"),
                        sliderInput(inputId="setTraceFreq", label="Trace Frequency", min=0.1, max=1, value=0.1, step = 0.05)
                        ),
               tabPanel(id = "Variants", "Variants",
                fluidRow(
                  column(2,
                    sliderInput(inputId="setGraphTraceFreq2", label="Trace Frequency", min=0.01, max=1, value=0.01),
                    selectInput("caseSelect", "Select case", "Loading...")),
                  column(10,
                    textOutput("variantNodeList"))),
                    grVizOutput('processGraphVariants')),
               tabPanel("Data Import",
                  sidebarLayout(
                    sidebarPanel(width = 3,
                        fileInput("file1", "Choose CSV File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
                            selectInput("selectCase","Select Case",choices = "Import Data..."),
                            selectInput("selectActivity","Select Activity",choices = "Import Data..."),
                            selectInput("selectResource","Select Resource",choices ="Import Data..."),
                            selectInput("selectTimestamps","Select Timestamp(s)",choices = "Import Data...", multiple = TRUE),
                            actionButton("dataSelected", "Upload Data")
                        ),
                    mainPanel(width = 9,
                      dataTableOutput('importGraphSummary')
                    )
                    ))))
  

server <- function(input,output,session) {

events <- startLocalDatabase()
preproccesEventData(events, session)
  
options(shiny.maxRequestSize=30*1024^2)

output$processGraphVariants <- renderGrViz({createVariantsGraph2(input, output, session, events)})
output$processGraphVisual <-  renderGrViz({createGraph(events, input$setGraphActFreq, input$setGraphTraceFreq, input$visType, input$measureType, input$durationType)})
output$traceExplorer <- renderPlot(events %>% trace_explorer(coverage = input$setTraceFreq))

observeEvent(input$file1, {
  infile <<- input$file1
  importData <<- readr::read_csv(infile$datapath,
                                locale = locale(date_names = 'en',
                                                encoding = 'ISO-8859-1'))
  headers <- as.list(names(importData))
  updateSelectInput(session, "selectCase",choices = headers)
  updateSelectInput(session, "selectActivity",choices = headers)
  updateSelectInput(session, "selectResource",choices = headers)
  updateSelectInput(session, "selectTimestamps",choices = headers)
  output$importGraphSummary <- renderDataTable(importData %>% select(input$selectCase, input$selectActivity, input$selectResource, input$selectTimestamps), 
                                               options = list(pageLength = 5))})
  
observeEvent(input$dataSelected, {
    
  selectedData <- importData %>% select(input$selectCase, input$selectActivity, input$selectResource, input$selectTimestamps)
    
    if((ncol(selectedData)) < 4){
      print("please select the correct headers")
    }else{
      print("Saving data to database...")
      
      headers = list()
      headers$caseID <- str_replace_all(input$selectCase, c(" " = "_" , "," = "" ))
      headers$activityID <- str_replace_all(input$selectActivity, c(" " = "_" , "," = "" ))
      headers$resourceID <- str_replace_all(input$selectResource, c(" " = "_" , "," = "" ))
      headers$timestamps <- input$selectTimestamps
      
      events <<- setDatabase(importData, headers)
      preproccesEventData(events, session)
      
      print("Finished Saving data to database")
    }
  })
}
shinyApp(ui = ui, server = server)
