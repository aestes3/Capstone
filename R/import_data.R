#### TEST File for Importing Data
# Load Packages
library(tidyverse)
library(here)
library(readxl)

#Import Translated Data 
data <- read_xlsx(here("data-raw/Data Translated.xlsx"), sheet = "data")
#Export excel data to Rdata
saveRDS(data, file = here("data/data.RData"))

