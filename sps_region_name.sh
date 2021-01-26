#!/bin/bash
#$ -S /bin/bash
#$ -wd /work/$USER
#$ -j y

# ----------------------------------------------------------------------
# qsub arguments
# ----------------------------------------------------------------------

#$ -N sps_region_names
#$ -l h_rt=06:00:00
#$ -l h_vmem=1000G
#$ -binding linear:1
#$ -t 1

# ----------------------------------------------------------------------
# setup job output/error reports
# ----------------------------------------------------------------------

#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID_log.txt
#$ -e /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID_err.txt

# ----------------------------------------------------------------------
# load required modules
# ----------------------------------------------------------------------

 module load R/3.5.1-2

# ----------------------------------------------------------------------
# execute task
# ----------------------------------------------------------------------

# set real index 

Rscript --vanilla /data/idiv_meyer/01_projects/eduardo/2nd_chapter/Select_GBIF_records/sTWIST/sps_region_name.R