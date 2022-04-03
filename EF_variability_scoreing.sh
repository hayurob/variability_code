#!/bin/sh

#  test_EF.sh
#  
#
#  Created by Heather Robinson on 2/11/22.
#  

############## 1) CREATE BEHAVIORAL DATA DIRECTORIES ##########################

#Retrieve the path containing all subject folders.
echo "Enter path to subject data: "
read subj_path
echo "You entered $subj_path"

#Create behavioral data folders within subject folders.
#If these folders already exist, print that they already exist.
#use this array for later
declare -a arr=()
for i in `ls -d $subj_path/*`; do
    subject=${i##*/}
    
    for j in `ls $i/*xls`; do
        task=${j##*/}
        task=${task%.xls}
        length=${#task}
        task=${task:6:length}
        arr+=( "$task" )
        if [ ! -e $i/behavioral_data/ ]; then
            mkdir $i/behavioral_data
        fi
        
        if [ ! -e $i/behavioral_data/$task ]; then
            mkdir $i/behavioral_data/$task
            cp $i/*$task.xls $i/behavioral_data/$task 2> /dev/null
        else
            echo "behavioral task folders already exist for" $subject
        fi
        
        
    done

done

uniq_arr=($(printf "%s\n" "${arr[@]}" | sort -u))


############### 2) SCORE BEHAVIORAL DATA AND RUN IIV ##########################

#run through each subject, get subject id and bblid to pass to R code
echo "Enter path to R scripts: "
read r_path
echo "You entered $r_path"

##sometimes folders are named 'bblid' and sometimes they are 'bblid_scanid'. The following checks to make sure it gets the correct id.
#for i in `ls -d $subj_path/*`; do
#
#    subject=${i##*/}
#
#    if grep -q '_' $subject; then
#        bblid=`echo $subject | cut -d "_" -f 1 2>`
#    else
#            bblid=$subject
#    fi
#
##run through each task, get raw data file and task name to pass to R code
#        for j in `find $i/behavioral_data/* -type d -name "[a-z]*"`; do
#
#            task=${j##*/}
#            raw_data=`ls -d $j/*"_"$task".xls" 2> /dev/null`
#            data=`ls -d $j/$bblid"_"$task".csv" 2> /dev/null`
#
##if the raw data file (the .xls file) doesn't exist then print out that it is missing
#            if [ ! -e "$raw_data" ]; then
#                echo $subject $task "raw .xls file missing"
#            fi
#
##if the raw data file (the .xls file) doesn't exist, but there is a csv in the wrong format, reformat the csv.
#            if [ ! -e "$raw_data" ] && [ -e "$data" ]; then
#                perl -lpe 's/"/""/g; s/^|$/"/g; s/\t/","/g' < "$data" > $j/test.csv
#                tr -d '"' < $j/test.csv > $j/"$subject"_"$task".csv
#                rm $j/test.csv
#                data=`ls -d $j/$subject"_"$task".csv"`; 2> /dev/null
#            fi
#
#
##if the raw data file (the .xls file) exists, and the converted data file (the .csv file) doesn't exist, then convert the .xls to .csv
#            if [ -e "$raw_data" ] && [ ! -e "$data" ]; then
#                #transform raw data file (output from presentation) to comma separated file
#                mac2unix $raw_data 2> /dev/null
#                tr -s '\t' <$raw_data | tr '\t' ',' > $j/$subject"_"$task".csv"; 2> /dev/null
#                data=`ls -d $j/$subject"_"$task".csv"`; 2> /dev/null
#            fi
#
##skip the following scoring and IIV prep if the task is CCI or GDS
#            if [ "$task" == "CCI" ] || [ "$task" == "GDS" ]; then
#            continue
#            fi
#
##check if each task scoring file doesn't exist and if the raw data exists, if both these conditions are true, run the appropriate R scoring code which scores the task and outputs a scoring and IIV csv for each task for each subject
#        task_output=`ls -d $j/$subject"_"$task"_scored.csv" 2> /dev/null`
#
##if [ "X$task_output" == "X" ] && [ -e "$data" ]; then
#            if [ -e "$data" ]; then
#            echo "scoring" $task "for" $subject "................"
#
#
##july 21 change
##/import/monstrum2/Applications/R3.2.3/bin/R --slave --file=/import/monstrum/IIV/healthy_controls/scripts/scoring/$task"_scoring_code.R" --args "$data" "$j" "$subject" "$task"
#
#        /Library/Frameworks/R.framework/Resources/R --slave --file=$r_path/$task"_scoring_code.R" --args "$data" "$j" "$subject" "$task" "$bblid"
#
#        else
##echo $task "already run for" $subject "OR" $task "data missing........."
#
#        echo $subject $task "data missing"
#
#        fi
#
#
#    done
#
#done

############### 3) AGGREGATE DATA ##########################

#create a group results folder with appropriate sub-folders.
if [ ! -e $subj_path/group_results ]; then
    mkdir $subj_path/group_results
    mkdir $subj_path/group_results/behavioral_data
    mkdir $subj_path/group_results/behavioral_data/scored
    mkdir $subj_path/group_results/behavioral_data/IIV
    mkdir $subj_path/group_results/behavioral_data/IIV/raw_data
    mkdir $subj_path/group_results/behavioral_data/IIV/IIV_data/
else
    echo "group results folder already exist for" $subj_path
fi

#for each task name in the array from earlier, get task name

for i in "${uniq_arr[@]}"; do
    task=$i

#get header for each task from reference file and create new task scoring files and IIV raw data files for each task (which will be a compilation of all subjects)

    header1=`grep -w $task $r_path/task_scoring_headers.csv | cut -d "/" -f 2 | cut -d "," -f 2-`
    header2=`cat $r_path/$task"_IIV_headers.csv" | cut -d "/" -f 2 | cut -d "," -f 2- ` 2>/dev/null

    echo $header1 > $subj_path/group_results/behavioral_data/scored/"$task"_scored_data.csv
    echo $header2 > $subj_path/group_results/behavioral_data/IIV/raw_data/"$task"_raw_data_for_IIV.csv

done
    #populate each task file with each subject's appended scored task data and IIV raw data

for i in `ls -d $subj_path/*`; do
    skip=$(basename $i)
        [[ $skip =~ ^(group_results)$ ]] && continue
    subj=${i##*/}
    
    if grep -q '_' $subj 2>/dev/null; then
        bblid=`echo $subj | cut -d "_" -f 1`
    else
        bblid=$subj
    fi
    for j in `find $i/behavioral_data/* -type d -name "[a-z]*"`; do
        task=${j##*/}
        data=`cat $i/behavioral_data/"$task"/"$subj"_"$task"_scored.csv | tail -1 | cut -d "," -f 2- ` 2>/dev/null
        iiv=`cat $i/behavioral_data/"$task"/"$subj"_"$task"_raw_for_IIV.csv | tail -1 | cut -d "," -f 2- ` 2>/dev/null

        echo $data >> $subj_path/group_results/behavioral_data/scored/"$task"_scored_data.csv 2>/dev/null
        echo $iiv >> $subj_path/group_results/behavioral_data/IIV/raw_data/"$task"_raw_data_for_IIV.csv 2>/dev/null
    done

done

echo 'Enter path to subject csv: '
read subj_csv_path

#calculate IIV for each task (tasks looped through in R script) and output the IIV (calculated on full sample) per task into the group results/IIV/IIV_data/ folder
# R script on own works, but can't get to work in terminal
/Library/Frameworks/R.framework/Resources/R --slave --file=$r_path/IIV_all_tasks_full_sample_calculation_hr.R --args "$subj_path" "$subj_csv_path"

