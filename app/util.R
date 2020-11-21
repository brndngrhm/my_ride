
#helper functions for app

##########################################

parse_fit_file <- function(data){
  
  fit_file <- FITfileR::readFitFile(data)
  
  fit_file_df <- if(class(records(fit_file)) %in% "list"){
    
    purrr::map_dfr(records(fit_file), bind_rows)
    
  } else {
    
    records(fit_file)
    
  }
  
  # checks if col names exist, and if not create it with value 0.
  expected_cols <- c("timestamp", "position_lat", "position_long", "distance", "accumulated_power", 
                     "altitude", "speed", "power", "heart_rate", "cadence", "temperature", 
                     "fractional_cadence")
  
  fit_file_df[expected_cols[!(expected_cols %in% colnames(fit_file_df))]] <- 0
  
  fit_file_df %>%
    mutate(heart_rate = ifelse(heart_rate == 255, 0, heart_rate),
           cadence_raw = cadence,
           cadence = imputeTS::na_ma(cadence_raw, k = 4, weighting = 'simple'),
           speed_raw = speed,
           speed = imputeTS::na_ma(speed_raw, k = 4, weighting = 'simple')* 2.23694,
           power_raw = power,
           power_adj = ifelse(power_raw > 1500, NA_real_, power_raw),
           power = imputeTS::na_ma(power_adj, k = 4, weighting = 'simple')
    )
  
}

# minute averages of all numeric variables
get_minute_avgs <- function(data){
  
  fit_file_formatted <- data %>%
    as_tibble() %>%
    mutate(timestamp = ymd_hms(timestamp) - hours(5), 
           time = floor_date(timestamp, "1 second")) %>%
    select(-accumulated_power) %>%
    group_by(time) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = T))) %>%
    mutate(count = 1,
           ride_min = cumsum(count)
    )
  
  return(fit_file_formatted)
  
}

# lets you select 1 variable at a time 
get_specific_value <- function(data, value){
  
  vars <- c("time", "ride_min", {{value}})
  
  data %>%
    select(one_of(vars)) %>% 
    rename(metric_avg = 3)
}

# does changepoint analysis on a variable
get_changepoints <- function(data, sensitivity){
  
  #changepoint analysis - http://members.cbio.mines-paristech.fr/~thocking/change-tutorial/RK-CptWorkshop.html
  cp_mean <- cpt.mean(data$metric_avg,
                      test.stat = 'Normal',
                      method = 'BinSeg',
                      Q = sensitivity,
                      penalty = "SIC")
  
  #store breakpoints in a dataframe
  changepoints <- data.frame("ride_min" = cpts(cp_mean),
                             changes = seq(1, length(cpts(cp_mean)), 1))
  
  #join breakpoints to formatted data, fill in the gaps
  data <- left_join(data, changepoints, by = "ride_min") %>%
    tidyr::fill(changes, .direction = "downup") 
  
  #calculate the mean within each segment, as identified by the changepoints
  changepoint_mean <- data %>%
    group_by(changes) %>%
    summarise(mean = mean(metric_avg, na.rm = T))
  
  #join mean back to dataframe
  left_join(data, changepoint_mean, by = "changes")
  
}

get_normalized_power <- function(data){
  
  # calc based on: https://medium.com/critical-powers/formulas-from-training-and-racing-with-a-power-meter-2a295c661b46
  
  data %>%
    mutate(window = floor_date(time, "30 seconds"), 
           rolling_30s_avg = slide_index_dbl(metric_avg, window, mean, .before = seconds(30)),
           rolling_avg_powered = rolling_30s_avg^4) %>%
    ungroup() %>%
    mutate(avg_powered_values = mean(rolling_avg_powered),
           NP = avg_powered_values^0.25) %>%
    pull(NP) %>%
    unique()
}