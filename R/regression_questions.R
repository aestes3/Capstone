#Log-Binomial Regression Test
library(tidyverse)
library(gtsummary) 
library(lubridate)
library(here)
library(janitor)
library(naniar)
library(writexl)
library(readxl)
#Help put models into table
library(broom.helpers)
library(car)
library(parameters)

# Load the cleaned dataset
load(here("data", "clean_data.RData"))
clean <- clean_data
# Remove covid contact (coded as 3) from analysis
clean_use <- clean |> 
    filter(population != 3)


#### GROUP VARIABLES ####
# Combine factor variables to create larger groups
# Remove NAs in specific variables being evaluated
clean_grouped <- clean_use |> 
#Code multi-variate variables
mutate(gender = case_when(
    gender == 0 ~ 0,
    gender == 1 ~ 1,
    gender == 2 ~ 2,
    gender == 3 ~ 2, #Code from unknown to combined category of Transfemale/unknown to other
    TRUE ~ as.numeric(NA)
)) |>
mutate(nationality = case_when(
    nationality == 0 ~ 0, #Native
    nationality == 1 ~ 1, #Forigen 
    nationality == 2 ~ 1,
    nationality == 3 ~ 1,
    TRUE ~ as.numeric(NA)
)) |> 
mutate(sexual_orientation = case_when(
    sexual_orientation == 0 ~ 0, #Sraight Vs Other
    sexual_orientation == 1 ~ 1,
    sexual_orientation == 2 ~ 1,
    sexual_orientation == 3 ~ 1,
    sexual_orientation == 4 ~ 1,
)) 
    

### Question 1: Influence of Demographic factors on HIV intervention components (condoms, lubricants)
model1_condoms <- glm(condoms ~ age + as.factor(gender) + nationality + sexual_orientation,
               family = poisson (link='log'), data=clean_grouped)

#Create condoms table
table_condoms <- model1_condoms |> tbl_regression(exponentiate = TRUE)

model1_lubricants <-glm(lubricants ~ age + as.factor(gender) + nationality + sexual_orientation,
                                 family = poisson (link='log'), data=clean_grouped)
#Create lubricants table
table_lubricants <- model1_lubricants |> tbl_regression(exponentiate = TRUE)

#Merge tables
tbl_merge(
    list(table_condoms, table_lubricants),
    tab_spanner = c("**Condoms Received**", "**Lubricant Recieved**")
)

### Question 2: Factors associated with access to HIV care among individuals who tested pos

df_positive <- filter(clean_grouped, first_test == "1")
model2_careaccess <- glm(care_access ~ age + as.factor(gender) + nationality + sexual_orientation,
                         family = poisson (link='log'), data=df_positive)

model2_careaccess |> tbl_regression(intercept = TRUE, exponentiate = TRUE) |> 
    add_glance_source_note() |> 
    add_global_p()
