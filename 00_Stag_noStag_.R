#----------------------------
# Author: Carlos Ortega
# Date:   2022-09-21
# Input: 
# 1) Data from Pierre: official MFRP data + regressors + Main Customers.
#
# Output:  Model with regressors.
# Purpose: To get a forecast projection with different scenarios.  
#----------------------------


#----- Libraries
suppressPackageStartupMessages({
  library(data.table)     # Ultra-fast processing
  library(ggpubr)         # Elegant charts based in ggplot
  library(patchwork)      # To compose different ggplots layouts
  library(lubridate)      # For dates
  library(ggeasy)         # Make easier to change attributes in ggplot
  library(janitor)        # To change names of variables
  library(tictoc)         # To measure times easily
  library(stringi)        # Fast string manipulation
  library(forecast)       # To model with forecast.
  library(tidytable)      # data.table with pipes like dplyr.
  library(tibble)         # data.frame equivalent (to use some of their functions)
})

#---- Read files
salesin <- fread("./data/01_Spain_sales_and_forecast_2022_09_20.csv")
regress <- fread("./data/02_SpainMacroScenarios_Quarter_2022_09_20.csv")

#---- Transform and just select the needed columns.
#---- Sales
salesgd <- salesin %>%
  filter.(managerial_country == "Spain") %>%
  filter.(vertical_segment == "Adecco") %>%
  mutate.( yearqu = as.numeric(paste(year(date), quarter(date), sep = ""))) %>%
  mutate.( salesqu = sum(sales), .by = yearqu) %>%
  select.( yearqu, salesqu) %>%
  distinct.() %>%
  as.data.table()

#---- Regressors
regresgd <- regress %>%
  mutate.( mydate = dmy(date)) %>%
  mutate.( yearqu = as.numeric(paste(year(mydate), quarter(mydate), sep = ""))) %>%
  select.(-date, -mydate) %>%
  as.data.table()


#---- Join Sales and Regressors
salesregres <- merge(
                     regresgd, salesgd,
                     by.x = c('yearqu'), by.y = c('yearqu'),
                     all.x = TRUE
                    ) %>%
mutate.( salestofore = ifelse.(yearqu >= 20223, NA, salesqu )) %>%
as.data.table() 


#----- MODEL -------
