
#helper functions for MyRide app

##########################################

parse_fit_file <-
  function(path){
    
    # takes user uploaded .fit file as input, parses and returns tidy dataframe
    
    fit_file <-
      FITfileR::readFitFile(path)
    
    fit_file_df <-
      if(class(records(fit_file)) %in% "list"){
        
        purrr::map_dfr(FITfileR::records(fit_file), bind_rows)
        
      } else {
        
        FITfileR::records(fit_file)
        
      }
    
    # checks if 'expected' col names exist, and if not, create with value 0.
    expected_cols <- c("timestamp", "position_lat", "position_long", "distance", "accumulated_power", 
                       "altitude", "speed", "power", "heart_rate", "cadence", "temperature", 
                       "fractional_cadence")
    
    fit_file_df[expected_cols[!(expected_cols %in% colnames(fit_file_df))]] <- 0
    
    # final data prep - rename & impute missing observations (data sometimes missing for a few seconds)
    fit_file_df %>%
      mutate(heart_rate = ifelse(heart_rate == 255, 0, heart_rate),
             cadence_raw = cadence,
             cadence = imputeTS::na_ma(cadence_raw, k = 4, weighting = 'simple'),
             speed_raw = speed,
             speed = imputeTS::na_ma(speed_raw, k = 4, weighting = 'simple')* 2.23694, # change from m/s to mph
             power_raw = power,
             power_adj = ifelse(power_raw > 1500, NA_real_, power_raw), #re-code anything over max power to `NA` - likely bad data
             power = imputeTS::na_ma(power_adj, k = 4, weighting = 'simple')
      )
    
  }

get_metric <-
  function(data, value){
    
    # returns dataframe w/ 3 columns: time, ride_min, and user selected value which gets renamed to 'metric' which is helpful for plotting
    # note: these formatting steps are not implemented in parse_fit_file() b/c output of parse_fit_file() is also used in get_normalized_power() which uses different granularity
    
    vars <- c("time", "ride_sec", {{value}})
  
    data %>%
      as_tibble() %>%
      mutate(timestamp = ymd_hms(timestamp) - hours(5), 
             time = floor_date(timestamp, "1 second")) %>%
      arrange(time) %>%
      mutate(count = 1,
             ride_sec = cumsum(count)) %>%
      select(one_of(vars)) %>% 
      rename(metric = 3) 
  }

get_normalized_power <-
  function(data){
    
    # returns single value of normalized power
    # calc based on: https://medium.com/critical-powers/formulas-from-training-and-racing-with-a-power-meter-2a295c661b46
    
    data %>%
      mutate(window = floor_date(time, "30 seconds"), 
             rolling_30s_avg = slide_index_dbl(metric, window, mean, .before = seconds(30)),
             rolling_avg_powered = rolling_30s_avg^4) %>%
      ungroup() %>%
      mutate(avg_powered_values = mean(rolling_avg_powered),
             NP = avg_powered_values^0.25) %>%
      pull(NP) %>%
      unique()
  }

get_changepoints <-
  function(data, sensitivity){
    
    # returns dataframe with mean of selected metric within each changepoint region
    # based on: http://members.cbio.mines-paristech.fr/~thocking/change-tutorial/RK-CptWorkshop.html
    
    # identifies changepoints for selected metric given selected sensitivity level
    cp_mean <-
      cpt.mean(data$metric,
               test.stat = 'Normal',
               method = 'BinSeg',
               Q = sensitivity,
               penalty = "SIC")
    
    #stores the values of the second in which each changepoint occurs
    changepoint_seconds <-
      cpts(cp_mean)
    
    #calculates avg value of selected metric within each changepoint region
    data %>%
      mutate(changes = ifelse(ride_sec %in% changepoint_seconds, ride_sec, NA_real_)) %>%
      tidyr::fill(changes, .direction = "downup") %>%
      group_by(changes) %>%
      mutate(mean = mean(metric, na.rm = T))
  }