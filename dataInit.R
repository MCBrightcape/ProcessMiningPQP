#Load data from server

startLocalDatabase <-function(){
  print("Loading Local Data")
  data <- readr::read_csv('ExampleLog.csv',
                          locale = locale(date_names = 'en',
                                          encoding = 'ISO-8859-1'))
  
  # change timestamp to date var
  data$starttimestampFormatted = as.POSIXct(data$Start_Date,
                                            format = "%d.%m.%y %H:%M")
  
  data$endtimestampFormatted = as.POSIXct(data$End_Date,
                                          format = "%d.%m.%y %H:%M")
  
  # remove blanks from var names
  names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))
  
  events <- activities_to_eventlog(
    data,
    case_id = 'Case_ID',
    activity_id = 'Activity',
    resource_id = 'Resource',
    timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
  )
  
  print("Finished Loading Local Data...")
  return(events)
}

setDatabase <- function(data, headers){
  #Get date seperator given in the input
  sep <- substring(gsub('[[:digit:]]+', '', data[headers$timestamps[1]][1,]),1,1)
  format <- paste("%d","%m","%Y %H:%M",sep=sep)
  
  #Weird fix for listing
  for (item in data[headers$timestamps[1]]){
    fixedStarttime <- item
  }
  data$starttimestampFormatted = as.POSIXct(fixedStarttime, format = format)
  
  if(length(headers$timestamps) > 1){
    #Weird fix for listing
    for (item in data[headers$timestamps[2]]){
      fixedEndtime <- item
    }
    data$endtimestampFormatted = as.POSIXct(fixedEndtime, format = format)
  }

  # remove blanks from var names
  names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))
  
  events <<- activities_to_eventlog(
    data,
    case_id = headers$caseID,
    activity_id = headers$activityID,
    resource_id = headers$resourceID,
    timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
  )
}

createGraph <- function(events, setGraphActFreq, setGraphTraceFreq, visType, measureType, durationType){
  return (
    if(visType == "Frequency"){
      events %>%
        filter_activity_frequency(percentage = setGraphActFreq) %>% # show only most frequent activities
        filter_trace_frequency(percentage = setGraphTraceFreq) %>%     # show only the most frequent traces
        process_map(render = T)
    }else{
      events %>%
        filter_activity_frequency(percentage = setGraphActFreq) %>% # show only most frequent activities
        filter_trace_frequency(percentage = setGraphTraceFreq) %>%     # show only the most frequent traces
        process_map(performance(get(measureType), durationType), render = T)
    })
}

createVariantsGraph <- function(input, output, session, events){

  output$variantNodeList <- renderText(variantsDF$Activities[which(variantsDF$Index == input$caseSelect)])
  
  return(
    events %>% filter_activity(activities = unlist(strsplit(variantsDF$Activities[which(variantsDF$Index == input$caseSelect)],","))) %>%
      filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
      filter_trace_frequency(percentage = input$setGraphTraceFreq2) %>%     # show only the most frequent traces
      process_map(render = T)
    )
}

updateCases <- function(input, output, session, events){
  variants <<- traces(events)[order(-traces(events)$relative_frequency),]
  variantsDF <<- data.frame(character(nrow(variants)),
                           list(1:nrow(variants)), 
                           stringsAsFactors=FALSE)
  
  colnames(variantsDF)[1] <<- "Index"
  colnames(variantsDF)[2] <<- "Activities"
  
  for (i in 1:nrow(variants)){
    combined <- paste(c(i, substr(variants[i,3],0,6)), collapse = ":Relative freq:")
    variantsDF[i,1] <<- combined
    variantsDF[i,2] <<- variants[i,1]
  }
  
  updateSelectInput(session, "caseSelect", choices = variantsDF$Index)
}