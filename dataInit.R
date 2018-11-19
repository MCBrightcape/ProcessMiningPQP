#Load data from server

print("Initializing Data...")
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

events <- activities_to_eventlog(
  head(data, n=10000),
  case_id = 'Case_ID',
  activity_id = 'Activity',
  resource_id = 'Resource',
  timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
)
print("Finished loading Data...")

# print("Initializing Data...")
# data <- readr::read_csv('TaxiDataChanged.csv',
#                         locale = locale(date_names = 'en',
#                                         encoding = 'ISO-8859-1'))
# 
# # change timestamp to date var
# data$starttimestampFormatted = as.POSIXct(data$FullDateStart,
#                                           format = "%d/%m/%Y %H:%M")
# 
# data$endtimestampFormatted = as.POSIXct(data$FullDateEnd,
#                                         format = "%d/%m/%Y %H:%M")
# 
# # remove blanks from var names
# names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))
# 
# events <- activities_to_eventlog(
#   head(data, n=10000),
#   case_id = 'Werknemer',
#   activity_id = 'Omschrijving uren',
#   resource_id = 'Naam Werknemer',
#   timestamps = c('starttimestampFormatted', 'endtimestampFormatted')
# )
# print("Finished loading Data...")
