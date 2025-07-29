###########################################################
# POST PROCESS
#
# Process model output ready for plotting.
#
###########################################################

# ---------------------------------------------------------
# Aggregate groupings for appropriate metrics
# ---------------------------------------------------------
aggregate_results = function(input, raw_output) {
  
  # NOTE: This is the first step when processing raw results.
  #       It is in a seperate function because we may want to
  #       only aggregate, and not summarise raw results.
  
  # Metrics that can be aggregated
  agg_metrics = input$metrics$df[aggregate == TRUE, metric]
  
  # Need to aggregate grouped metrics: get first grouping per metric to avoid double counting
  first_grouping = raw_output %>% 
    filter(metric %in% agg_metrics) %>% 
    select(metric, grouping) %>%
    unique() %>%
    filter(!grouping %in% c("none", "na")) %>%
    group_by(metric) %>%
    slice(1) %>%
    ungroup() %>%
    setDT()
  
  # Return out earlier if no groupings
  if (nrow(first_grouping) == 0)
    return(raw_output)
  
  # Aggregate values of first grouping
  agg_df = first_grouping %>%
    left_join(raw_output,
              by = c("metric", "grouping")) %>%
    group_by(metric, day, scenario, seed) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(grouping = "none",
           group    = NA) %>%
    bind_rows(raw_output) %>%
    setDT()
  
  return(agg_df)
}

# ---------------------------------------------------------
# Post process raw model output and append to 'result' list
# ---------------------------------------------------------
process_results = function(result, raw_output) {
  
  # Aggregate groupings for appropriate metrics
  #
  # TODO: Put this variable ordering inside of aggregate_results
  agg_df = aggregate_results(result$input, raw_output) %>%
    select(scenario, metric, day, grouping, group, seed, value)
  
  # Short hand for lower and upper quantiles
  q1 = o$quantiles[1]
  q2 = o$quantiles[2]
  
  # Create key summary statistics across seeds
  result$output = agg_df %>%
    lazy_dt() %>%
    group_not(seed, value) %>%
    summarise(
      mean   = mean(value, na.rm = TRUE),
      median = fquantile(value, 0.5, na.rm = TRUE),
      lower  = fquantile(value, q1,  na.rm = TRUE),
      upper  = fquantile(value, q2,  na.rm = TRUE)) %>%
    ungroup() %>%
    as.data.table()
  
  # Metrics that can be cumulatively summed
  cum_metrics = result$input$metrics$df[cumulative == TRUE, metric]
  
  # Do the same for for cumulative values
  result$cum_output = agg_df %>%
    lazy_dt() %>%
    filter(metric %in% cum_metrics) %>%
    # Cumulatively sum over time...
    group_not(day, value) %>%
    mutate(value = cumsum(value)) %>%
    ungroup() %>%
    # Summarise over seeds...
    group_not(seed, value) %>%
    summarise(
      mean   = mean(value, na.rm = TRUE),
      median = fquantile(value, 0.5, na.rm = TRUE),
      lower  = fquantile(value, q1,  na.rm = TRUE),
      upper  = fquantile(value, q2,  na.rm = TRUE)) %>%
    ungroup() %>%
    as.data.table()
  
  return(result)
}

# ---------------------------------------------------------
# Format model outcomes ready for plotting
# ---------------------------------------------------------
format_results = function(f, results) {
  
  # Which results df to load depends on cumulative flag
  which_output = ifelse(f$cumulative, "cum_output", "output")
  
  # Reduce model output down to what we're interested in
  output_df = results[[which_output]] %>%
    select(day, metric, grouping, group, scenario, 
           value = o$best_estimate_simulation, lower, upper) %>%
    filter(is.na(day) | day >= f$plot_from, 
           is.na(day) | day <= f$plot_to, 
           metric %in% f$metrics, 
           !is.na(value))
  
  # ---- Do scaling and convert proportion to percentage ----
  
  # Subset of metric details: coverages and scaled metrics
  metric_df = results$input$metrics$df %>%
    select(metric, scale, coverage)
  
  # Scaler required (based on number simulated)
  scaler = f$person_days / results$input$population_size
  
  # For appropriate metrics, apply scaler and multiple by 100 
  output_df = output_df %>%
    left_join(metric_df, by = "metric") %>%
    mutate(value = ifelse(scale, value * scaler, value), 
           lower = ifelse(scale, lower * scaler, lower), 
           upper = ifelse(scale, upper * scaler, upper)) %>% 
    mutate(value = ifelse(coverage, value * 100, value), 
           lower = ifelse(coverage, lower * 100, lower), 
           upper = ifelse(coverage, upper * 100, upper)) %>%
    select(-scale, -coverage)
  
  # ---- Select appropriate grouping ----
  
  # Plotting by some disaggregation
  if (f$plot_type == "group") {
    
    # Filter for only this grouping
    output_df = output_df[grouping == f$plot_by, ]
    
    # Levels of factors
    group_levels = results$input$count[[f$plot_by]]
    output_df[, group := factor(group, group_levels)]
    
    # Plotting by age is a special case - group by age classification
    if (f$plot_by == "age")
      output_df = group_ages(output_df)
  }
  
  # No grouping - we want to remove all disaggregations
  if (f$plot_type != "group")
    output_df = output_df[is.na(group), ]
  
  # ---- Format output ----
  
  # Final formatting touches
  output_df = output_df %>%
    select(day, metric, group, value, lower, upper, scenario) %>%
    arrange(scenario, metric, group, day) %>% 
    mutate(metric = factor(metric, levels = f$metrics))
  
  return(output_df)
}

# ---------------------------------------------------------
# Group by user-defined age classifications
# ---------------------------------------------------------
group_ages = function(df, summarised = TRUE) {
  
  # Preallocate and bind age group variable to datatable
  age_df = df %>% 
    mutate(group = as.numeric(group), 
           age_group = "none")
  
  # Loop through age groups to classify into (from youngest to oldest)
  n_ages = length(o$plot_ages)
  for (i in 1 : n_ages) {
    
    # Upper and lower bound of this age group
    age_lower = sort(o$plot_ages)[i]
    age_upper = sort(o$plot_ages)[i + 1]
    
    # Name of this age classification
    if (i == n_ages) age_name = paste0(age_lower, "+")
    else age_name = paste0(age_lower, "-", age_upper)
    
    # Apply this grouping for all qualified individuals
    age_df[group >= age_lower, age_group := age_name]
  }
  
  # Recode with age group rather than age value
  age_df = select(age_df, -group) %>%
    rename(group = age_group) %>%
    mutate(group = paste("Age", group))
  
  # Dealing with already summarised output
  if (summarised == TRUE) {
    
    # Summarise by summing across age groups
    age_df = age_df %>%
      group_by(day, metric, scenario, group) %>%
      summarise(value = sum(value),
                lower = sum(lower),
                upper = sum(upper)) %>%
      ungroup() %>%
      setDT()
  }
  
  # Dealing with raw, non-summarised output
  if (summarised == FALSE) {
    
    # Summarise by summing across age groups but not simulations
    age_df = age_df %>%
      group_by(day, metric, seed, scenario, group) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      setDT()
  }
  
  return(age_df)
}

