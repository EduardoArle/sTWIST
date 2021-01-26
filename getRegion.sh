#!/bin/bash
#$ -S /bin/bash
#$ -wd /work/$USER
#$ -j y

# ----------------------------------------------------------------------
# qsub arguments
# ----------------------------------------------------------------------

#$ -N getRegion
#$ -l h_rt=24:00:00
#$ -l h_vmem=50G
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

 ml purge
 ml foss/2018b
 module load R/3.5.1-2

# ----------------------------------------------------------------------
# execute task
# ----------------------------------------------------------------------

# set real index 

Rscript --vanilla /data/idiv_meyer/01_projects/eduardo/2nd_chapter/Fresh_water/Scripts/getRegion.R