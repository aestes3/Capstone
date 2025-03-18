#### TEST File for Importing Data
# Load Packages
library(tidyverse)
library(here)
library(readxl)

#Import Translated Data 
data <- read_xlsx(here("data-raw/Data Translated.xlsx"), sheet = "data_lowercase",
                  col_names = TRUE)
#Export excel data to Rdata
save(data, file = here("data/data.RData"))

#Recode variables to match
data <- data |> 
    #Create binary variables
    mutate(poc = ifelse(poc == "community center", 0 , 1)) |> 
    mutate(intervention_type = ifelse(intervention_type == "first time", 0, 1)) |>   
    mutate(sex = ifelse(sex == "male", 0, 1)) |>  
    mutate(intersex = ifelse(intersex == "no", 0 ,1)) |> 
    mutate(info_hiv = ifelse(info_hiv == "no", 0 ,1)) |> 
    mutate(lubricants = ifelse(lubricants == "no", 0, 1)) |>
    mutate(condoms = ifelse(condoms == "no", 0 ,1)) |> 
    mutate(info_prep = ifelse(info_prep == "no", 0, 1)) |> 
    mutate(info_pep = ifelse(info_pep == "no", 0 ,1)) |>
    mutate(first_test = ifelse(first_test == "negative", 0 , 1)) |> 
    mutate(care_referral = ifelse(care_referral == "no", 0 ,1 )) |> 
    mutate(care_access = ifelse(care_access == "no", 0 ,1)) |> 
    mutate(treatment_msp = ifelse(treatment_msp == "no",0, 1))
    
    
