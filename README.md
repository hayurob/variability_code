# variability_code
Data analysis of behavioral data collected by the Brain Behavior Lab at UPenn.

This project, led by Dr. David Roalf, examines reaction time variability across different tasks in healthy older adults, healthy young adults, and older adults with mild cognitive impairment.

This repository contains the following documents:
1. Condensed_IOA.R
  This R code is a compilation of all data analyses for this project. It references functions other_functions.R.
2. other_functions.R
  In an effort to organize and clean code, these functions were created in order to do the same data analysis/cleaning on many similar datasets.
3. Tables_All.Rmd
  This R Markdown file creates tables from the data.
4. EF_variability_scoring.sh
  This bash script scores intra-individual variability using raw reaction time data. This script organizes the data, calls R scripts to score the data, and puts the output into csv files.
