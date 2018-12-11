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

createVariantsGraph2 <- function(input, output, session, events){
  return(
    events %>% filter_case(cases = unlist(strsplit(allVariants[input$caseSelect,'Cases'],split=", "))) %>%
      filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
      filter_trace_frequency(percentage = input$setGraphTraceFreq2) %>%     # show only the most frequent traces
      process_map(render = T)
  )
}

preproccesEventData <- function(events, session){
  print("Processing Data")

  allCases <- cases(events)
  allVariants <<- data.frame(integer(nrow(events %>% traces())),
                            character(nrow(events %>% traces())),
                            integer(nrow(events %>% traces())),
                            double(nrow(events %>% traces())),
                            stringsAsFactors = FALSE)
  
  colnames(allVariants)[1] <<- "Index"
  colnames(allVariants)[2] <<- "Cases"
  colnames(allVariants)[3] <<- "Frequency"
  colnames(allVariants)[4] <<- "Total_Days"
  
  for(i in 1:nrow(traces(events))){
    caseVector <- c()
    allVariants[i,'Index'] <<- i
    
    for (j in which(allCases['trace_id'] == i)){
      allVariants[i,'Frequency'] <<- allVariants[i,'Frequency'] + 1 
      allVariants[i,'Total_Days'] <<- allVariants[i,'Total_Days'] + allCases[j,'duration_in_days']
      caseVector <- append(caseVector, unlist(allCases[j,'Case_ID'], use.names = FALSE))
    }
    allVariants[i,"Cases"] <<- toString(caseVector)
  }
  
  allVariants <<- allVariants[order(-allVariants$Frequency),]
  choices <<- setNames(allVariants$Index, allVariants$Frequency)
  updateSelectInput(session, "caseSelect", choices = choices)
  
  print("Finished Processing Data")
}