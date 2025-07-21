###########################################################
# LAUNCH
#
# Main launch function for OpenRSV. Currently under development.
#
###########################################################

# Set working directory to sourced file
if (interactive()) setwd(getSrcDirectory(function() {}))

# Load all required packages and functions
source("dependencies.R")

message("Running OpenRSV v0.1 alpha")

# Set options (see options.R)
o = set_options(do_step = 0)

# Step 0) Test run a single simulation
run_model_test(o)  # See unit_tests.R

# Step 1) Calibrate model
run_calibration(o)  # See calibration.R

# Step 2) Run all scenarios
run_scenarios(o)  # See scenarios.R

# Step 3) Operate on array scenarios
run_arrays(o)  # See array.R

# Step 4) Plot results
run_results(o)  # See results.R

# Finish up
message("* Finished!")

