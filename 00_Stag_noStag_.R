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

#-- Save File.
fwrite(
  salesregres,
  file = "./output/Raw_Data_by_Quarter.csv",
  sep = "|"
)


#----- MODELS -------------

#----- No Stagflation ----
#---- Train
datininona <- salesregres %>%
  filter.(!is.na(salestofore)) %>%
  select.(salestofore, gdp, unemployment, inflation ) %>%
  as_tibble() 

# As a multivariate "ts".
datininona_ts <- ts(
                    datininona,
                    start = c(2013, 1), frequency = 4
                   )

reg_nostag <- c('gdp', 'unemployment', 'inflation')
fit_nostag <- auto.arima( datininona_ts[, "salestofore"],
                          xreg = datininona_ts[, reg_nostag])

#---- Predict
datpred <- salesregres %>%
  filter.(is.na(salestofore)) %>%
  select.(gdp, unemployment, inflation ) %>%
  as_tibble() 
              
datpred_ts <- ts(datpred, start(2022,6), frequency = 4)
fcast_nostag <- forecast(fit_nostag, xreg = datpred_ts)


#---- Process Output to save it.
fcastexp_nostag <- fcast_nostag %>% 
  as.data.frame() %>%
  rownames_to_column( var = "fore_date") %>%
  rename.( Forecast = `Point Forecast`, ) %>%
  rename.( Lo_80 = `Lo 80`, Hi_80 = `Hi 80`, Lo_95 = `Lo 95`, Hi_95 = `Hi 95` ) %>%
  as.data.table()

#-- Save File.
fwrite(
  fcastexp_nostag,
  file = "./output/NoStag_Forecast_by_Quarter.csv",
  sep = "|"
)





#----- Stagflation ----
#---- Train
datininona <- salesregres %>%
  filter.(!is.na(salestofore)) %>%
  select.(salestofore, gdp_stagflation, unemployment_stagflation, inflation_stagflation ) %>%
  as_tibble() 

# As a multivariate "ts".
datininona_ts <- ts(
  datininona,
  start = c(2013, 1), frequency = 4
)

reg_stag <- c('gdp_stagflation', 'unemployment_stagflation', 'inflation_stagflation')
fit_stag <- auto.arima( datininona_ts[, "salestofore"],
                          xreg = datininona_ts[, reg_stag])

#---- Predict
datpred <- salesregres %>%
  filter.(is.na(salestofore)) %>%
  select.(gdp_stagflation, unemployment_stagflation, inflation_stagflation ) %>%
  as_tibble() 

datpred_ts <- ts(datpred, start(2022,6), frequency = 4)
fcast_stag <- forecast(fit_stag, xreg = datpred_ts)

#---- Process Output to save it.
fcastexp_stag <- fcast_stag %>% 
  as.data.frame() %>%
  rownames_to_column( var = "fore_date") %>%
  rename.( Forecast = `Point Forecast`, ) %>%
  rename.( Lo_80 = `Lo 80`, Hi_80 = `Hi 80`, Lo_95 = `Lo 95`, Hi_95 = `Hi 95` ) %>%
  as.data.table()

#-- Save File.
fwrite(
  fcastexp_stag,
  file = "./output/Stag_Forecast_by_Quarter.csv",
  sep = "|"
)
