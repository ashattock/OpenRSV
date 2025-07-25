###########################################################
# OPTIONS
#
# Set key options for all things model related.
#
# Any numerical, logical, or character value or vector defined
# in the main set_options() function can be overridden through
# the non-mandatory \config\options.yaml file. Note that
# this options yaml file is git ignored (indeed, that is the very
# point of such a file), so each user will need to create one.
#
###########################################################

# ---------------------------------------------------------
# Set model options and assumptions
# ---------------------------------------------------------
set_options = function(do_step = NA, quiet = FALSE) {
  
  if (!quiet) message("* Setting options")
  
  # Reset R's most annoying default options
  options(stringsAsFactors = FALSE, scipen = 999, dplyr.summarise.inform = FALSE)
  
  # Initiate options list
  o = list(do_step = do_step)
  
  # Detect user - required for checking cluster jobs
  o$user = Sys.info()[["user"]]
  
  # Name of analysis to run (cannot contain period symbol)
  o$analysis_name = "demo"
  
  # Set analysis name and create output directory system
  o = set_dirs(o)  # See directories.R
  
  # ---- Data sources ----
  
  # Epidemiological data sources
  o$data_api = list(
    rsvglobal = "https://github.com/RSVGlobalReport/RSVGlobal/raw/main/data/alter_rsv/VIW_FNT.csv")

  # API endpoint for COVID-related national-level Oxford Stringency Index data
  o$osi_api = "https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/"
  
  # ---- Calibration settings ----
  
  # Whether fitting should be reproducible
  o$fit_reproducible = TRUE
  
  # Force the regeneration of synthetic data by running the model
  o$force_regenerate_synthetic = FALSE
  
  # Overwrite existing samples
  o$overwrite_samples = FALSE
  
  # Accept up to x% of samples failing
  o$sample_err_tol = 0.05  # 1-5% is reasonable
  
  # The 'best' parameter set to take from calibration process
  #
  # OPTIONS: 
  #   "emulated" := Global minimum of emulated space
  #  "simulated" := Lowest objective value from all already simulated parameter sets
  o$best_param_set = "emulated"
  
  # Check calibration file consistency before simulating scenarios
  o$check_fit_consistency = FALSE  # Should be more efficient to be a default check
  
  # ---- Uncertainty settings ----
  
  # Flag for reproducible scenarios (consistent randomly sampled seeds)
  o$scenario_reproducible = TRUE
  
  # Statistical summary to use for 'best estimate' projection
  #
  # OPTIONS:
  #  "median" := Median of uncertainty simulations (stochastic and parameter uncertainty)
  #    "mean" := Mean of uncertainty simulations (stochastic and parameter uncertainty)
  o$best_estimate_simulation = "mean"
  
  # Number of seeds to run for each scenario (including baseline)
  o$n_seeds_analysis = 10
  
  # Number of parameters sets to sample when simulating parameter uncertainty
  o$n_parameter_sets = 10  # Best to set to 1 if not simulating parameter uncertainty

  # Flag for simulating each uncertainty parameter set n_seeds times
  #
  # NOTE: If false, each parameter set is simulated only once with a randomly defined seed
  o$full_factorial_uncertainty = FALSE
  
  # Flag to impute vaules for scenarios that did not run (mean across all other seeds)
  o$impute_failed_jobs = TRUE
  
  # Quantiles for credibility intervals
  o$quantiles = c(0.025, 0.975)
  
  # Force quantile summary even if no new simulations have been run
  o$force_summarise = FALSE
  
  # ---- Other outputs ----
  
  # Define for which scenarios we store the contact network
  o$save_network = "baseline"  # OPTIONS: 'all', 'baseline', or 'none'
  
  # ---- Cluster settings ----
  
  # Flag for overwriting any existing simulations
  o$overwrite_simulations = TRUE
  
  # Check YAML file consistency for any existing simulations before skipping
  o$check_yaml_consistency = FALSE  # Should be more efficient to be a default check
  
  # If running on scicore partition, you also need to define a job time 
  # 
  # NOTES:
  #  1) This should be an OVER-ESTIMATE of time needed per job - this is important as jobs over this will fail
  #  2) For more details, see: https://wiki.biozentrum.unibas.ch/display/scicore/4.+Queues+and+partitions
  o$job_time  = "00:30:00"  # Use "HH:MM:SS" or "D-HH:MM:SS" format
  o$job_queue = "30min"  # Queue to use (check out scicore wiki if unfamiliar)
  
  # Memory to allocate for each cluster job
  #
  # NOTES: 
  #  1) General principle - the higher this is set, the less resources you get
  #  2) Anything upto (and incuding) 3.7GB will give maximum resources (~500 cores at a time)
  #  3) For populations sizes of over 1m, you'll likely want something around 8GB
  o$job_memory = "8GB"
  
  # Set an upper limit for jobs that can be run at any one time
  o$job_limit = 600
  
  # Define names for cluster log and error files
  o$log_file = "scicore_log.txt"
  o$err_file = "scicore_error.txt"
  
  # Flag to remove cluster logs when jobs have successfully completed
  o$rm_cluster_log = TRUE  # Set to FALSE to bebug any cluster errors
  
  # Action to take if user is already running cluster jobs
  o$cluster_conflict_action = "error"  # Set to 'none' to turn off
  
  # ---- Plotting settings ----
  
  # Lower bound of age groups for plotting - bounded above by maximum age
  o$plot_ages = c(0, 18, 60)  # Captures 3 age groups as per ECDC request
  
  # Plot a maximum number of scenarios on temporal plots
  o$max_scenarios = 25
  
  # Colour palettes (see colour_scheme in auxiliary.R)
  o$palette_scenario = "pals::cols25"
  o$palette_metric   = "pals::kovesi.rainbow"
  
  # Colour palettes (see colour_scheme in auxiliary.R)
  o$palette_age     = "brewer::set2"
  o$palette_variant = "brewer::set3"
  o$palette_priority_group = "brewer::accent"
  o$palette_vaccine_type   = "brewer::dark2"
  o$palette_vaccine_doses  = "pals::kovesi.rainbow"
  
  # Define some nice properties for baseline metric plots
  o$baseline_name   = "Baseline scenario"
  o$baseline_colour = "grey50"  # Light grey
  
  # Grey colour to represent data
  o$data_colour = "#555555"  # Dark grey

  # Saved figure size
  o$save_width  = 14
  o$save_height = 10
  
  # Units of figures sizes
  o$save_units = "in"
  
  # Units of figures sizes
  o$save_units = "in"
  
  # Plotting resolution (in dpi)
  o$save_resolution = 300
  
  # Image format for saving figure
  # 
  # NOTE: Use a character vector to save with multiple formats at once
  o$figure_format = "png" # Classic options: "png", "pdf", or "svg"
  
  # ---- Plotting flags ----
  
  # Turn figures on or off
  o$plot_baseline    = FALSE  # Standard baseline figures
  o$plot_cumulative  = FALSE  # Plot cumulative outcomes
  o$plot_scenarios   = FALSE  # Plot alternative (non-array) scenarios
  o$plot_arrays      = FALSE  # Plot grid array scenario bundles
  o$plot_heatmaps    = FALSE  # Plot heat maps for multidimension grid arrays
  o$plot_endpoints   = FALSE  # Plot array LHC endpoints across different parameters
  o$plot_assumptions = TRUE  # Model structure and assumptions figures
  o$plot_calibration = FALSE  # Calibration performance and diagnostics
  
  # Flags for custom figures
  o$plot_custom      = FALSE  # Run my_results.R (if it exists)
  
  # ---- Override options ----
  
  # Override options set in options yaml file
  o = override_options(o, quiet = quiet)
  
  # Display analysis details
  if (!quiet) message(" - Analysis name: ", o$analysis_name)
  
  return(o)
}

# ---------------------------------------------------------
# Override options set in options yaml file
# ---------------------------------------------------------
override_options = function(o, quiet = FALSE) {
  
  # If user has a 'my options' file, load it
  if (file.exists(o$pth$my_options)) {
    my_options = read_yaml(o$pth$my_options)
    
    # Continue if we have options to override
    if (length(my_options) > 0) {
      
      if (!quiet) message(" - Overriding options using config/my_options.yaml file")
      
      # Throw an error if there are entries that are not well defined options
      unrecognised = names(my_options)[!names(my_options) %in% names(o)]
      if (length(unrecognised) > 0)
        stop("Unrecognised entries in 'my options' file: ", paste(unrecognised, collapse = ", "))
      
      # Throw an error if there are multiple entries for any one option
      duplicates = names(my_options)[duplicated(names(my_options))]
      if (length(duplicates) > 0)
        stop("Duplicate entries in 'my options' file: ", paste(duplicates, collapse = ", "))
      
      # Variable class of the options we wish to overwrite
      class_conversion = paste0("as.", lapply(o[names(my_options)], class))
      
      # Iterate through the options and overwrite with value of correct class
      for (i in seq_along(my_options))
        o[[names(my_options)[i]]] = get(class_conversion[i])(my_options[[i]])
    }
  }
  
  return(o)
}

