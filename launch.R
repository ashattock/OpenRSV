###########################################################
# LAUNCH
#
# Main launch function for OpenRSV
#
###########################################################

# Set working directory to sourced file
if (interactive()) setwd(getSrcDirectory(function() {}))

# Load all required packages and functions
source("config/dependencies.R")

message("Running OpenRSV v0.1 alpha")

# Set options (see options.R)
o = set_options(do_step = 0)

# Step 0) Test run a single simulation
run_model_test()  # See unit_tests.R

# Step 1) Calibrate model
run_calibration()  # See calibration.R

# Step 2) Run all scenarios
run_scenarios()  # See scenarios.R

# Step 3) Operate on array scenarios
run_arrays()  # See array.R

# Step 4) Plot results
run_results()  # See results.R

# Finish up
message("* Finished!")

