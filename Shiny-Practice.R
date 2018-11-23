source("ServerInit.R")
source("DataInit.R")

ui <- fluidPage(style = "height:100%",
        navbarPage("Process Mining",
               tabPanel("Graph",
                        fluidRow(
                          column(2,
                            selectInput("dataType", "Select type", c("Frequency","Performance"), selectize = FALSE),
                            sliderInput(inputId="setGraphActFreq", label="Activity Frequency", min=0.01, max=1, value=0.01),
                            sliderInput(inputId="setGraphTraceFreq", label="Trace Frequency", min=0.01, max=1, value=0.01)),
                          column(10,
                              grVizOutput('processGraphVisual', height = "800px")))),
               tabPanel("Variants",
                                 selectInput("inSelect", "Select case", "Loading...", selectize = FALSE),
                                 sliderInput(inputId="setGraphActFreq", label="Activity Frequency", min=0.01, max=1, value=0.01),
                                 sliderInput(inputId="setGraphTraceFreq", label="Trace Frequency", min=0.01, max=1, value=0.01),
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
  startLocalDatabase()
  
  options(shiny.maxRequestSize=30*1024^2)
  
  variants <- traces(events)[order(-traces(events)$relative_frequency),]
  variantsDF <- data.frame(character(nrow(variants)),
                   list(1:nrow(variants)), 
                   stringsAsFactors=FALSE)
  
  colnames(variantsDF)[1] <- "Index"
  colnames(variantsDF)[2] <- "Activities"
  
  for (i in 1:nrow(variants)){
    combined <- paste(c(i, substr(variants[i,3],0,6)), collapse = ":Relative freq:")
    variantsDF[i,1] <- combined
    variantsDF[i,2] <- variants[i,1]
  }
                  
  updateSelectInput(session, "inSelect",
                    choices = variantsDF$Index
                    )
  
  output$processGraphVariants <-  renderGrViz({
    req(input$inSelect) 
    events %>% filter_activity(activities = unlist(strsplit(variantsDF$Activities[which(variantsDF$Index == input$inSelect)],","))) %>%
    filter_activity_frequency(percentage = input$setGraphActFreq) %>% # show only most frequent activities
    filter_trace_frequency(percentage = input$setGraphTraceFreq) %>%     # show only the most frequent traces
    process_map(render = T)})
  
  output$processGraphVisual <-  renderGrViz({
    if(input$dataType == "Frequency"){
      events %>%
        filter_activity_frequency(percentage = input$setGraphActFreq) %>% # show only most frequent activities
        filter_trace_frequency(percentage = input$setGraphTraceFreq) %>%     # show only the most frequent traces
        process_map(render = T)
    }else{
      events %>%
        filter_activity_frequency(percentage = input$setGraphActFreq) %>% # show only most frequent activities
        filter_trace_frequency(percentage = input$setGraphTraceFreq) %>%     # show only the most frequent traces
        process_map(performance(mean, "hours"),render = T)
    }})
  
  observe({
    req(input$file1)
    infile <- input$file1
    importData <- read.csv(infile$datapath)
    headers <- as.list(names(importData))
    updateSelectInput(session, "selectCase",choices = headers)
    updateSelectInput(session, "selectActivity",choices = headers)
    updateSelectInput(session, "selectResource",choices = headers)
    updateSelectInput(session, "selectTimestamps",choices = headers)
    output$importGraphSummary <- renderDataTable(importData %>% select(input$selectCase, input$selectActivity, input$selectResource, input$selectTimestamps), 
                                                 options = list(pageLength = 5))
    
    observeEvent(input$dataSelected, {
      if((importData %>% select(input$selectCase, input$selectActivity, input$selectResource, input$selectTimestamps) %>% ncol) < 4){
        print("please select the correct headers")
      }else{
        print("Saving data to database...")
        selectedData <- importData %>% select(input$selectCase, input$selectActivity, input$selectResource, input$selectTimestamps)
        setDatabase(selectedData, c(input$selectCase, input$selectActivity, input$selectResource, input$selectTimestamps))
        print("Finished Saving data to database")
      }
    })
  })

}
shinyApp(ui = ui, server = server)
