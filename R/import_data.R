#### Data import and cleaning ####
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
    mutate(treatment_msp = ifelse(treatment_msp == "no",0, 1)) |> 
    #Code multi-variate variables
    mutate(gender = case_when(
        gender == "male" ~ 0,
        gender == "female" ~ 1,
        gender == "trans female" ~ 2,
        gender == "unknown"~ 3,
        TRUE ~ as.numeric(NA)
    )) |> 
    mutate(sexual_orientation = case_when(
        sexual_orientation == "heterosexual" ~ 0,
        sexual_orientation == "gay" ~ 1,
        sexual_orientation == "lesbian" ~ 2,
        sexual_orientation == "bisexual" ~ 3,
        sexual_orientation == "unknown" ~ 4,
        TRUE ~ as.numeric(NA)
    )) |> 
    mutate(nationality = case_when(
        nationality == "ecuadorian" ~ 0,
        nationality == "colombian" ~ 1,
        nationality == "venezuelan" ~ 2,
        nationality == "unknown" ~ 3,
        TRUE ~ as.numeric(NA)
    )) |> 
    mutate(ethnicity = case_when(
        ethnicity == "white" ~ 0,
        ethnicity == "black" ~ 1,
        ethnicity == "mestizo" ~ 2,
        ethnicity == "indigenous" ~ 3,
        ethnicity == "montubio" ~ 4,
        ethnicity == "other" ~ 5,
        TRUE ~ as.numeric(NA)
     )) |> 
     mutate(population = case_when(
         population == "msm" ~ 0,
         population == "sex_worker" ~ 1,
         population == "trans" ~ 2,
         population == "covid contact" ~ 3,
         TRUE ~ as.numeric(NA)
     )) |> 
    mutate(second_test = case_when(
        second_test == "negative" ~ 0,
        second_test == "positive" ~ 1,
        second_test == "inconclusive" ~ 2,
        second_test == "na" ~ 3,
        TRUE ~ as.numeric(NA)
    ))
    
#Export modified data to Rdata
save(data, file = here("data/data_mod.RData"))


