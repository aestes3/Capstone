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

### Question 1: Influence of Demographic factors on HIV intervention components (condoms, lubricants)
model1_condoms <- glm(condoms ~ age + sex + gender + nationality + sexual_orientation + ethnicity,
               family = binomial (link='logit'), data=clean_use)

#Create condoms table
table_condoms <- model1_condoms |> tbl_regression()

model1_lubricants <-glm(lubricants ~ age + sex + gender + nationality + sexual_orientation + ethnicity,
                                 family = binomial (link='logit'), data=clean_use)
#Create lubricants table
table_lubricants <- model1_lubricants |> tbl_regression()

#Merge tables
tbl_merge(
    list(table_condoms, table_lubricants),
    tab_spanner = c("**Condoms Received**", "**Lubricant Recieved**")
)

### Question 2: Factors associated with access to HIV care among individuals who tested pos

df_positive <- filter(clean_use, first_test == "1")
model2_careaccess <- glm(care_access ~ age + sex + gender + nationality + sexual_orientation + ethnicity,
                         family = binomial (link='logit'), data=df_positive)

model2_careaccess |> tbl_regression(intercept = TRUE) |> 
    add_glance_source_note() |> 
    add_global_p()
