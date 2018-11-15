# Server initilazation file
# check installed packages and install only necessary ones ####
print("Initializing Server..")
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

print("Finished Initializing Server..")