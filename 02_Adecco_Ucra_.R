
adeccoucra <- function(stagflag, aniomes, anio, mes) {
     #----------------------------
     # Author: Carlos Ortega
     # Date:   2022-03-22
     # Input: 
     # 1) Data from Pierre: official MFRP data + regressors + Main Customers.
     # Output:  Model with regressors.
     # Description:  To get a projection by unit, etc. - AdeccoRed and Vincenzo.
     #----------------------------
     
     #---
     #--- This function uses these parameters:
     # stagflag: 0/1 to determine if it is needed to use features with stagflation or without.    
     # aniomes: 202204 - The last year/month with the real data.
     # anio: year of aniomes
     # mes: month of aniomes
     
     # rm(list = ls())
     # gc()
     # # cat("\014")  # ctrl+L
     
     tini <- Sys.time()
     
     #----- Libraries
     suppressPackageStartupMessages({
      library(data.table)     # Ultra-fast processing
      library(ggpubr)         # Elegant charts based in ggplot
      library(ggplot2)        # The chart library
      library(patchwork)      # To compose different ggplots layouts
      library(lubridate)      # For dates
      library(stringr)        # String manipulation - RStudio
      library(ggeasy)         # Make easier to change attributes in ggplot
      library(janitor)        # To change names of variables
      library(tictoc)         # To measure times easily
      library(stringi)        # Fast string manipulation
      library(tidyr)          # For long/wide transformations
      library(dplyr)          # Transformation pipeline
      library(forcats)        # For categorical variables
      library(fasttime)       # To format dates very fast
      library(dtplyr)         # Dplyr sintax for data.table
      library(broom)          # Tidy model results.
      library(readxl)         # To read Excel files - RStudio.
      library(openxlsx)       # To read Excel files -  A little bit faster.
      # library(boostime)       # To model with catboost and lightgbm
      library(modeltime)      # To model with modeltime
      library(modeltime.h2o)  # To model with modeltime.h2o
      library(forecast)       # To model with forecast.
      library(mgcv)           # GAM model.
      library(autostsm)       # Automatic Time Series Structural Modeling
      library(seasonal)       # To measure seasonality of Time Series.
      library(tidytable)      # data.table with pipes like dplyr.
      library(corrplot)       # Correlation plot.
      library(corrr)          # Correlation package from tidymodels
      library(ggcorrplot)     # Correlation plots with ggplot
      library(fpp2)           # To add examples of forecast 2nd Edition.
      library(GGally)         # Scatterplot Matrix.
      library(magrittr)       # Piping for dplyr
      library(readxl)         # To read Excel files.
      library(tidytable)      # dplyr verbs for data.table
      library(tibble)         # data.frame equivalent (to use some of their functions)
     })
     
     
     #--- Read different data files and minimum Transformations.
     datsales <- fread("./data/Spain_sales_and_forecast.csv", nThread = 3)
     # datfeatu <- fread("./data/Spain_stagflation_features.csv", nThread = 3)
     # # dattop10 <- fread("./data/Spain_top_10_clients.csv", nThread = 3)
     # datfeatu <- fread("./data/Spain_features.csv", nThread = 3)
     
     STAGFLATION <- stagflag
     if ( STAGFLATION == 1 ) {
      datfeatu <- fread("./data/Spain_stagflation_features.csv", nThread = 3)
      stag_flag <- "_YesStag_"
     } 
     if ( STAGFLATION == 0 ) {
      datfeatu <- fread("./data/Spain_features.csv", nThread = 3)
      stag_flag <- "_NoStag_"
     }
     
     #--- Just a chart to see sales.
     service_var <- "Adecco"
     type_var     <- "Ucrania_"
     my_title <- paste(type_var, service_var, " - Sales (€)", sep = "")
     
     dattmp <- datsales %>%
      select.( -managerial_country ) %>%
      rename.(vsegment_desc = vertical_segment) %>%
      filter.(vsegment_desc == service_var) %>%
      mutate.(date = lubridate::ymd(date))
     
     ggplot( dattmp, aes(date, sales)) +
      geom_line() + 
      labs(
        x = "Year Month",
        y = "Sales (€)",
        title = my_title,
        subtitle = "Original Time Series"
      ) +
      theme_bw()
     ggsave(paste("./charts/",type_var,service_var, stag_flag, "_Original_TimeSeries_.png", sep = ""))    
     
     
     
     #--- Just Adecco - Sales Data.
     datadecco <- datsales %>% 
      rename.(vsegment_desc = vertical_segment) %>%
      filter.(vsegment_desc == service_var) %>%
      # select.(-man_country_desc, -currency) %>%
      mutate.(year    = lubridate::year(date)) %>%
      mutate.(month   = lubridate::month(date)) %>%
      mutate.(yearmon = 100*year + month) %>%
      as.data.table() 
     
     #--- Transform features to leave it ready to merge with sales data.
     datfeatu %<>%
      mutate.( year    = lubridate::year(date)) %>%
      mutate.( month   = lubridate::month(date)) %>%
      mutate.( yearmon = 100*year + month) %>%
      select.( -year, -month, -date) %>%
      rename.(tradingdays = trading_days) %>%
      as.data.table()
     
     #--- Merge Sales and Features.
     datiniOri <- merge(
      datadecco, datfeatu,
      by.x = c('yearmon'), 
      by.y = c('yearmon'),
      sort = FALSE
     ) 
     
     #--------- SCENARIO CONDITIONS UCRANIA --------------------------
     #---- Scenario's Impact:
     # GPD -> 2 pts. less.
     # Inflation -> 2 pts. more.
     # Unemployment -> 1 pts. more.
     #---- Changes starting in 2022-May.
     
     fct_gdp <- 0.98
     fct_une <- 0.5
     fct_inf <- 1
     
     datini <- datiniOri %>%
         mutate.( GDP          = ifelse.(yearmon > aniomes, GDP * fct_gdp, GDP) ) %>%
         mutate.( Unemployment = ifelse.(yearmon > aniomes, Unemployment + fct_une, Unemployment) ) %>%
         mutate.( Inflation    = ifelse.(yearmon > aniomes, Inflation + fct_inf, Inflation) ) %>%
         as.data.table()
     
     
     #---- CHARTS - Economic Magnitudes - ORIGINAL 
     datiniOri %>%
      select.(yearmon, sales, GDP, Unemployment, Inflation) %>%
      pivot_longer.(cols = -yearmon) %>%
      ggplot( aes( as.factor(yearmon), value ) ) +
        geom_line( group = 1, color = "turquoise4") + 
        facet_wrap(~ name, scales = "free_y") +
        geom_vline( xintercept = 12,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 24,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 36,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 48,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 60,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 72,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 84,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 96,  color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 108, color = "blue", lwd = 0.1) +
        geom_vline( xintercept = 120, color = "blue", lwd = 0.1) +
        labs(
              title = "MACROECONOMIC INDICATORS - ORIGINAL",
              subtitle = "(Sales values from 2022-06 projected - Indicators estimated for 2022)",
              x = "Year_Month",
              y = "Value"
        ) +
        theme_bw() +
        easy_rotate_x_labels( angle = 90) +
        easy_x_axis_labels_size( size = 4) +
        theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
     ggsave(paste("./charts/Economic_Indicators_Original_TimeSeries", stag_flag,"_.png", sep = ""))
     
     #---- CHARTS - Economic Magnitudes - TRANSFORMED
     datini %>%
      select.(yearmon, sales, GDP, Unemployment, Inflation) %>%
      pivot_longer.(cols = -yearmon) %>%
      ggplot( aes( as.factor(yearmon), value ) ) +
      geom_line( group = 1) + 
      facet_wrap(~ name, scales = "free_y") +
      geom_vline( xintercept = 12,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 24,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 36,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 48,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 60,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 72,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 84,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 96,  color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 108, color = "blue", lwd = 0.1) +
      geom_vline( xintercept = 120, color = "blue", lwd = 0.1) +
      labs(
        title = "MACROECONOMIC INDICATORS - TRANSFORMED",
        subtitle = "(Sales values from 2022-06 projected - Indicators estimated for 2022)",
        x = "Year_Month",
        y = "Value"
      ) +
      theme_bw() +
      easy_rotate_x_labels( angle = 90) +
      easy_x_axis_labels_size( size = 4) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
     ggsave(paste("./charts/Economic_Indicators_Transformed_TimeSeries", stag_flag,"_.png", sep = ""))
     
     
     #-------------------------------------------------------------------------------
     #-----------------------------  YES 2020 ---------------------------------------
     #-------------------------------------------------------------------------------
     # Just no NA values for trading_days and just real data for Sales (202201 and 202202) no forecast.
     datininona <- datini %>%
      filter.(!is.na(tradingdays)) %>%
      filter.(yearmon <= aniomes) %>%
      select.(date, GDP, Unemployment, Inflation, sales, tradingdays) %>%
      as_tibble() 
     
     # As a multivariate "ts".
     datininona_ts <- ts(select(datininona, GDP, Unemployment, Inflation, sales, tradingdays), start = c(2017, 1), frequency = 12)
     
     #-- Use 2017, 2018, 2019, 2020 and june-2021 for Train (54 months)
     myts_train <- window( datininona_ts, end = c(2021, 9) )
     #-- Use 2021-jul until 2022-mar for Test. (8 months)
     myts_test  <- window( datininona_ts, start = c(2021,10) )
     
     #------ Combination of all variables.
     myvars <- c("GDP", "Unemployment", "Inflation", "tradingdays")
     
     #---- All posible commbinations - store in a list
     list_comb <- list(
      comb0 = as.data.frame(combn(myvars,0)),
      comb1 = as.data.frame(combn(myvars,1)),
      comb2 = as.data.frame(combn(myvars,2)),
      comb3 = as.data.frame(combn(myvars,3)),
      comb4 = as.data.frame(combn(myvars,4))
     )
     
     #---- For each combination run a model and evaluate train/test score.
     tic()
     allpreds_with <- data.table()
     
     #--- Asses error rate with different combination of regressors.  
     for (i in 1:length(list_comb)) {
      comb_tmp <- as.data.frame(list_comb[i] )
      
      for (j in 1:ncol(comb_tmp)) {
        print(c(i,j))
        reg_tmp <- as.vector(comb_tmp[, j])
        
        # This case is when there is no regressors.
        if (nrow(comb_tmp) < 1) {
          
          #Fit model with Train
          fitno20_train <- auto.arima( myts_train[, "sales"] )
          #Forecast with Test
          foreno_train <- forecast(fitno20_train )
          reg_tmp <- c('None')
          
          # This case is when there are regressors.
        } else {
          
          #Fit model with Train
          fitno20_train <- auto.arima( 
            myts_train[, "sales"],
            xreg = myts_train[ , reg_tmp],
          )
          #Forecast with Test
          foreno_train <- forecast(fitno20_train, 
                                   xreg = myts_test[ , reg_tmp])
          
        } 
        
        # Accuracy Train vs Test
        acc_tmp <- accuracy(foreno_train, myts_test[, "sales"])
        acctmp_df <- as.data.frame(acc_tmp)
        acctmp_df$traintest <- rownames(acctmp_df)
        acctmp_dt <- as.data.table(acctmp_df)
        acctmp_dt[ , vars := paste0(reg_tmp, collapse = "_")]
        acctmp_dt[ , year2020 := "Yes"]
        
        allpreds_with <- rbind(allpreds_with, acctmp_dt)
      } #  for (j in 1:ncol(comb_t
      
     } # for (i in 1:length(list_com
     toc()
     
     minval_mape_test <- allpreds_with[ traintest == "Test set", min(MAPE)]
     best_test        <- allpreds_with[ MAPE == minval_mape_test, ]
     besttest_dt <- allpreds_with[ vars == best_test$vars,]
     besttest_dt
     
     #---- Save table with accuracy.
     library(gridExtra)
     png(
      paste("./charts/",type_var, service_var, "Accuracy_with_2020_best.png", sep = ""), 
      height = 50*nrow(besttest_dt),
      width = 200*ncol(besttest_dt)
     )
     grid.table(besttest_dt)
     dev.off() 
     
     #------------------------------  YES 2020 ---------------------------------------
     
     
     #-------------------------------------------------------------------------------
     #-----------------------------  NO 2020 ----------------------------------------
     #-------------------------------------------------------------------------------
     # Just no NA values and just the needed columns
     datininona <- datini %>%
      filter.( !is.na(tradingdays)) %>%
      filter.( yearmon > 202012 | yearmon <= 201912) %>%
      #-- Just January, February 2022
      filter.(yearmon <= aniomes) %>%
      select.(date, GDP, Unemployment, Inflation, sales, tradingdays) %>% 
      as_tibble() 
     
     # As a multivariate "ts".
     datininona_ts <- ts(select(datininona, GDP, Unemployment, Inflation, sales, tradingdays), start = c(2017, 1), frequency = 12)
     
     #--- Train - 42 months.
     myts_train <- window( datininona_ts, end = c(2020, 9) )
     #--- Test - 8 months.
     myts_test  <- window( datininona_ts, start = c(2020, 10) )
     
     #------ Combination of all variables.
     myvars <- c("GDP", "Unemployment", "Inflation", "tradingdays")
     
     #---- All posible commbinations - store in a list
     list_comb <- list(
      comb0 = as.data.frame(combn(myvars,0)),
      comb1 = as.data.frame(combn(myvars,1)),
      comb2 = as.data.frame(combn(myvars,2)),
      comb3 = as.data.frame(combn(myvars,3)),
      comb4 = as.data.frame(combn(myvars,4))
     )
     
     #---- For each combination run a model and evaluate train/test score.
     tic()
     allpreds <- data.table()
     
     #--- Asses error rate with different combination of regressors.  
     for (i in 1:length(list_comb)) {
      comb_tmp <- as.data.frame(list_comb[i] )
      
      for (j in 1:ncol(comb_tmp)) {
        print(c(i,j))
        reg_tmp <- as.vector(comb_tmp[, j])
        
        # This case is when there is no regressors.
        if (nrow(comb_tmp) < 1) {
          
          #Fit model with Train
          fitno20_train <- auto.arima( myts_train[, "sales"] )
          #Forecast with Test
          foreno_train <- forecast(fitno20_train )
          reg_tmp <- c('None')
          
          # This case is when there are regressors.
        } else {
          
          #Fit model with Train
          fitno20_train <- auto.arima( 
            myts_train[, "sales"],
            xreg = myts_train[ , reg_tmp],
          )
          #Forecast with Test
          foreno_train <- forecast(fitno20_train, 
                                   xreg = myts_test[ , reg_tmp])
          
        } 
        
        # Accuracy Train vs Test
        acc_tmp <- accuracy(foreno_train, myts_test[, "sales"])
        acctmp_df <- as.data.frame(acc_tmp)
        acctmp_df$traintest <- rownames(acctmp_df)
        acctmp_dt <- as.data.table(acctmp_df)
        acctmp_dt[ , vars := paste0(reg_tmp, collapse = "_")]
        acctmp_dt[ , year2020 := "No"]
        
        allpreds <- rbind(allpreds, acctmp_dt)
      } #  for (j in 1:ncol(comb_t
      
     } # for (i in 1:length(list_com
     toc()
     
     minval_mape_test <- allpreds[ traintest == "Test set", min(MAPE)]
     best_test        <- allpreds[ MAPE == minval_mape_test, ]
     besttest_dt <- allpreds[ vars == best_test$vars,]
     besttest_dt
     
     
     #---- Save table with accuracy.
     library(gridExtra)
     png(
      paste("./charts/",type_var, service_var, "_Accuracy_without_2020_best.png", sep = ""), 
      height = 50*nrow(besttest_dt),
      width = 200*ncol(besttest_dt)
     )
     grid.table(besttest_dt)
     dev.off() 
     
     
     #------------------------------  NO 2020 ---------------------------------------
     
     #-------------------------------- COMPARISON -----------------------------------
     #--- Best model with and without year 2020
     allpredsbig      <- rbind(allpreds_with, allpreds)
     minval_mape_test <- allpredsbig[ traintest == "Test set", min(MAPE)]
     best_test        <- allpredsbig[ MAPE == minval_mape_test, ]
     allpredsbig[ vars == best_test$vars & year2020 == best_test$year2020,]
     allpredsbig[ , type := type_var]
     allpredsbig[ , serv := service_var]
     
     
     #--- Save all predictions
     fwrite(
      allpredsbig, 
      file = paste("./output/",type_var,service_var, "_AllPreds_with_and_without_2020_.csv", sep = ""), 
      sep = "|"
     )
     
     #--- Results:
     # Best Model with:
     #      Yes 2020
     #      Inflation as the best predictor.
     
     #-- Table to compare better which is the best.
     allpredsbigred <- allpredsbig %>%
      select.(MAPE, traintest, vars, year2020) %>%
      as.data.table()
     #-- Best Case when "no" year2020  
     allpredsbigred %>%
      filter.( year2020 == "No") %>%
      filter.( traintest == "Test set") %>%
      arrange.(MAPE, vars, -traintest)
     
     
     
     # ---------------- FOR THE BEST MODEL --------------------
     #-- Force to use all variables...
     best_test$vars <- 'GDP_Unemployment_Inflation_tradingdays'
     #-- Force to use 2020...
     best_test$year2020 <- "Yes"
     
     reg_best      <- unlist(stri_split_fixed(best_test$vars, "_"))
     reg_bestsales <- c("sales", reg_best)
     yesno         <- best_test$year2020
     
     datininona <- datini %>%
      filter.( !is.na(tradingdays) ) %>%
      #---- CAMBIAR ESTE FILTRO ------ \/
      filter.( yearmon <= aniomes ) %>%
      select.( date, !!reg_bestsales ) %>% as_tibble() 
     
     # As a multivariate "ts".
     datininona_ts <- ts(select.(datininona, !!reg_bestsales), start = c(2017, 1), frequency = 12)
     
     # Fit model with everything
     fit_best <- auto.arima(datininona_ts[, "sales"],
                           xreg = datininona_ts[ , reg_best])    
     fit_best
     
     #---- To forecast use from "datini" the sales for 2022, starting in June.
     datinipredict <- datini %>%
      #---- CAMBIAR ESTE FILTRO ------ \/
      filter.(yearmon > aniomes) %>%
      # filter.(yearmon > 202112) %>%
      select.(date, !!reg_best) %>% as_tibble() 
     
      #---- CAMBIAR ESTE FILTRO ------ \/
     # As a multivariate "ts" starting in March-2022.
     # datinipredict_ts <- ts(select(datinipredict, !!reg_best), start = c(2022, 1), frequency = 12)
     datinipredict_ts <- ts(select(datinipredict, !!reg_best), start = c(anio, mes), frequency = 12, class = "mts")
     
     fcast_best <- forecast(fit_best, xreg = datinipredict_ts)
     
     #---- Process Output to save it.
     fcast_export <- fcast_best %>% 
      as.data.frame() %>%
      rownames_to_column( var = "fore_date") %>%
      rename.( Forecast = `Point Forecast`, ) %>%
      rename.( Lo_80 = `Lo 80`, Hi_80 = `Hi 80`, Lo_95 = `Lo 95`, Hi_95 = `Hi 95` ) %>%
      mutate.( fore_date = lubridate::my(fore_date)) %>%
      mutate.( yearmon = lubridate::year(fore_date) * 100 + lubridate::month(fore_date),
               .before = fore_date) %>%
      #--- Add service and type
      mutate.( type = type_var) %>%
      mutate.( serv = service_var) %>% 
      #--- Calculate sums by Quarter.
      mutate.( quarter = lubridate::quarter(fore_date)) %>%
      mutate.( sumQ = sum(Forecast), .by = quarter) %>%
      as.data.table()
     
     #-- Save File.
     fwrite(
      fcast_export,
      file = paste("./output/",type_var,service_var, stag_flag, "_Preds_w_Quarter.csv", sep = ""), 
      sep = "|"
     )
     
     tend <- Sys.time() ; tend - tini
     
     #------------------------------------------------------------
     #------------------- DIFFERENT CHARTS -----------------------
     #-- Residuals
     cbind("Regression Errors" = residuals(fit_best, type = "regression"),
          "ARIMA errors" = residuals(fit_best, type = "innovation")) %>%
      autoplot(facets = TRUE) +
      labs( title = my_title ) +
      theme_bw()
     ggsave(paste("./charts/",type_var,service_var, "_Residuals_.png", sep = ""))    
     
     checkresiduals(fit_best) +
      theme_bw()
     ggsave(paste("./charts/",type_var,service_var, "_Residuals_extended_.png", sep = ""))    
     
     #---- Forecast 
     autoplot(fcast_best) +
      labs(
        x = "Year" , 
        y = my_title,
        subtitle = my_title ) +
      theme_bw()
     ggsave(paste("./charts/",type_var,service_var, "_Dynamic_Evolution_Forecast.png", sep = ""))    
     
     #----- Fitted
     tic()
     fit_gr <- autoplot(datininona_ts[, "sales"]) + 
      labs(
        title = my_title,
        # subtitle = "(witht 2020)",
        y = "Total Revenues",
        x = "Year-Month"
      ) + 
      autolayer(fit_best$fitted, colour = TRUE) +
      guides(colour = guide_legend(title = "Adjusted: ")) +
      theme_bw() +
      theme(legend.position = "bottom") 
     print(fit_gr)
     ggsave(paste("./charts/",type_var,service_var, "_Fitted_.png", sep = ""))    
     toc()
     
     tend <- Sys.time() ; tend - tini
     
     #------ END OF FILE -----------
}     
