#!/bin/bash

############################################################
# LAUNCH
#
# Launch pipeline from command line.
#
# Command line usage:
#   sh launch.sh
#
############################################################

# Define R version
r_version="4.4.1"

# Identify which cluster we're working from
case "$MODULEPATH" in

  # Load R on SciCore
  *scicore*)
    echo "* Running on SciCore"
    module purge
    module load R/${r_version}-foss-2023b
    ;;
    
  # Load R (and dependencies) on Pawsey-Setonix
  *setonix*)
    echo "* Running on Setonix"
    module load r/${r_version}
    ;;
    
  # Neither recognised
  *)
    echo "! Unknown HPC system"
    ;;
esac

# Call main R launch script
Rscript launch.R

