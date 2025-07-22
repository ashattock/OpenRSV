#!/bin/bash

#SBATCH --job-name=OpenRSV
#SBATCH --account=penny
#SBATCH --partition=scicore
#SBATCH --cpus-per-task=1
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

############################################################
# SUBMIT
#
# Submit array of OpenRSV cluster tasks.
#
# Arguments:
#  JOB_TYPE: Type of job we're submitting - see cluster_task.R
#  LOG_FILE: Text log file that stores IDs of completed tasks
#
# Various sbatch options can be set in options.R (see 'cluster
# settings' section). For example, job memory and slurm queue.
#
############################################################

# Load R
module purge
ml R/4.1.0-foss-2018b

# Extract inputs
job_type=$1
log_file=$2

# Extract array job ID
job_id=$(expr ${SLURM_ARRAY_TASK_ID})

echo "Job ID: $job_id"

# Run R script which calls simulation function
Rscript submit.R $job_type $job_id

# Write job ID to log file
echo "$job_id" >> $log_file

