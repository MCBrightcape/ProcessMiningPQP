source("ServerInit.R")
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
               tabPanel("Data overview",
                        fileInput("file1", "Choose CSV File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
                        dataTableOutput("contents")
                        )))
  

server <- function(input,output,session) {
  source("DataInit.R")
  options(shiny.maxRequestSize=30*1024^2)
  
  output$graphSummary <- renderDataTable(events, options = list(pageLength = 5))
  
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

  output$contents <- renderDataTable({
    req(input$file1)
    inFile <- input$file1
    read.csv(inFile$datapath)
  })
}
shinyApp(ui = ui, server = server)
