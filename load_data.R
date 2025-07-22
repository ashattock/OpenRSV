###########################################################
# LOAD DATA
#
# Load epi data that may - or may not - be required for
# calibration. Also able to generate synthetic data to test
# fitting algorithm.
#
###########################################################

# ---------------------------------------------------------
# Parent function for extracting relevant fitting data
# ---------------------------------------------------------
load_data = function(o, fit, synthetic = NULL) {
  
  # TODO: Add feature to pull data from API / github
  
  # Load epi data regardless of what we're fititng to
  fit = load_epi(o, fit, synthetic)
  
  # Quick plot of fitting data
  # g = ggplot(fit$data) +
  #   aes(x = date, 
  #       y = value, 
  #       colour = country) +
  #   geom_line(
  #     show.legend = FALSE) +
  #   facet_wrap(
  #     facets = vars(metric), 
  #     scales = "free_y")
  
  return(fit)
}

# ---------------------------------------------------------
# Load emperical epidemiological data from source (or generate synthetic data)
# ---------------------------------------------------------
load_epi = function(o, fit, synthetic) {
  
  # All dates we're interested in
  fit$opts$dates = get_data_dates(fit$opts)
  
  # If desired, generate synthetic data instead of actual data
  #
  # NOTE: This is used to test/debug calibration process
  if (fit$opts$synthetic_data == TRUE) {
    fit$data = load_synthetic(o, fit$opts, synthetic)
    
    # We're done, return out early
    return(fit)
  }
  
  # Otherwise load empirical epidemiological data...
  message(" - Loading epi data: ", fit$opts$data_source)
  
  # Load already cached raw data from user-specified source
  raw_data = fread(paste0(o$pth$cache, fit$opts$data_source, ".csv"))
  
  # ---- Source: rsvglobal ----
  
  # Load rsvglobal data from cache
  if (fit$opts$data_source == "rsvglobal") {
    
    # Wrangle raw data into OpenRSV-readable
    fit$data = raw_data %>%
      select(country = COUNTRY_CODE, 
             date    = ISO_WEEKSTARTDATE,
             value   = RSV) %>%
      # Reduce to countries of interest
      filter(country %in% fit$input$data_countries, 
             !is.na(value)) %>%
      # Assuming values are hospital admissions
      mutate(date   = format_date(date), 
             metric = "hospital_admissions") %>%
      select(metric, country, date, value) %>%
      # TEMP: Issue with multiple entries per date - further disaggregations?
      group_by(metric, country, date) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      arrange(metric, country, date) %>%
      as.data.table()
  }
  
  # ---- Sanity checks ----
  
  # Countries for which data is missing
  missing_idx = !fit$input$data_countries %in% fit$data$country
  missing     = fit$input$data_countries[missing_idx]
  
  # Ensure we have at least some data for all countries
  if (length(missing) > 0)
    stop("No data identified for: ", paste(missing, collapse = ", "))
  
  # Check no data is negative
  if (any(fit$data$value < 0))
    stop("Negative data values identified")
  
  return(fit)
}

# ---------------------------------------------------------
# Load synthetic data (and generate it if it doesn't already exist)
# ---------------------------------------------------------
load_synthetic = function(o, opts, synthetic) {
  
  # File name to save data to / load data from
  save_file = paste0(o$pth$fitting, "synthetic_data.rds")
  
  # If file exists we may want to just reload this
  if (file.exists(save_file) && !o$force_regenerate_synthetic) {
    
    message(" - Loading synthetic data")
    
    # Simply load previous saved data
    fit_data = readRDS(save_file)
    
  } else { # Otherwise run the model to generate fake data 
    
    message(" - Generating synthetic data")
    
    # Create new synthetic data by running the model
    fit_data = generate_synthetic(o, opts, synthetic)
    
    # Save this file for potential later re-use
    saveRDS(fit_data, file = save_file)
  }
  
  return(fit_data)
}

# ---------------------------------------------------------
# Generate synthetic data by running the model
# ---------------------------------------------------------
generate_synthetic = function(o, opts, synthetic) {
  
  # Check input is provided and is in list format
  if (!is.list(synthetic) || length(synthetic) == 0)
    stop("Synthetic parameter values must be provided as a non-trivial list")
  
  # Append flag for performing fit
  fit_list = c(synthetic, .perform_fit = TRUE)
  
  # Run model with these pre-defined parameters (see model.R)
  result = model(o, "baseline", seed = 1, fit = fit_list, verbose = "bar")
  
  # Extract model outcomes & offset with some noise - this is what we'll fit to
  fit_data = result$output %>%
    inner_join(y  = opts$dates, 
               by = "day") %>%
    mutate(date = ceiling_date(date, opts$synthetic_period)) %>%
    group_by(date, metric) %>%
    summarise(value = sum(value)) %>% 
    ungroup() %>%
    mutate(value = value * rnorm(
      n    = n(), 
      mean = 1, 
      sd   = opts$synthetic_offset)) %>%
    left_join(y  = opts$dates, 
              by = "date") %>%
    select(date, day, metric, value) %>%
    filter(!is.na(value)) %>%
    setDT()
  
  return(fit_data)
}

# ---------------------------------------------------------
# Filter to only dates of interest
# ---------------------------------------------------------
get_data_dates = function(opts) {
  
  # Dates of data we'll fit to
  date_end   = format_date(opts$data_end)
  date_start = format_date(opts$data_start) - 
    opts$data_burn_in + 1
  
  # Sequence of dates for fitting and plotting
  all_dates = seq(date_start, date_end, by = "day")
  
  # Datatable of fitting dates and day indices
  dates_df = data.table(
    day  = 1 : length(all_dates), 
    date = all_dates) %>%
    filter(day > opts$data_burn_in)
  
  return(dates_df)
}

# ---------------------------------------------------------
# Pull OSI data from API endpoint and save in data cache
# ---------------------------------------------------------
pull_osi = function(o, y) {
  
  message("  > Pulling OSI data")
  
  browser() # Check use of 'date' and 'day' here
  
  # Dates to load data for (only go as far as yesterday - we'll fill the rest)
  date_from = format_date(y$npi_effect$start)
  date_to   = min(date_from + y$n_days - 1, format_date(today() - 1))
  
  # Construct call to API endpoint and convert to json format
  api_call = paste0(o$osi_api, date_from, "/", date_to)
  api_data = fromJSON(rawToChar(httr::GET(api_call)$content))
  
  # Data is horribly hidden, a few lapply's needed to extract what we need
  osi_fn = function(x) rbindlist(lapply(x, as.data.table), fill = TRUE)
  osi_df = rbindlist(lapply(api_data$data, osi_fn), fill = TRUE)
  
  # Save data in cache (needed as cluster nodes don't have internet access)
  saveRDS(osi_df, file = paste0(o$pth$cache, "osi.rds"))
}

