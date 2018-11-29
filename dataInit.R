#Load data from server

startLocalDatabase <-function(){
  print("Loading Local Data")
  data <- readr::read_csv('loanapplicationfileTest.csv',
                          locale = locale(date_names = 'en',
                                          encoding = 'ISO-8859-1'))
  
  # change timestamp to date var
  data$starttimestampFormatted = as.POSIXct(data$starttimestamp,
                                            format = "%d/%m/%Y %H:%M")
  
  data$endtimestampFormatted = as.POSIXct(data$endtimestamp,
                                          format = "%d/%m/%Y %H:%M")
  
  # remove blanks from var names
  names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))
  
  events <- activities_to_eventlog(
    head(data, n=1000),
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
  sep <- substring(gsub('[[:digit:]]+', '', data[headers$timestamps[1]]),1,1)
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
  
  events <- activities_to_eventlog(
    data,
    case_id = headers$caseID,
    activity_id = headers$activityID,
    resource_id = headers$resourceID,
    timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
  )
  
  return(events)
}

createGraph <- function(events, setGraphActFreq, setGraphTraceFreq, visType){
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
        process_map(performance(mean, "hours"),render = T)
    })
}

