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
  # change timestamp to date var
  if(length(headers$timestamps) > 1)
  {
    
    print(headers$timestamps)
    print(unlist(data["starttimestamp"], recursive=FALSE)[2])
    
    timestampsStart <- c(data[headers$timestamps[1]])
    data$starttimestampFormatted = as.POSIXct(timestampsStart,
                                                   format = "%d/%m/%Y %H:%M")
    
    timestampsEnd <- c(data[headers$timestamps[2]])
    data$endtimestampFormatted = as.POSIXct(timestampsEnd,
                                            format = "%d/%m/%Y %H:%M")
  }

  
  # remove blanks from var names
  names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))
  
  events <- activities_to_eventlog(
    head(data, n=10000),
    case_id = headers$caseID,
    activity_id = headers$activityID,
    resource_id = headers$ResourceID,
    timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
  )
  print("Finished loading Data...")
}

