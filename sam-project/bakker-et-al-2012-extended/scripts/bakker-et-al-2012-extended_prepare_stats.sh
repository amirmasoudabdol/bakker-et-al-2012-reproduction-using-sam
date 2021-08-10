#!/bin/bash

from=$1
n_cores=$2
find outputs -name "*_${from}.csv" | awk 'BEGIN{FS="_"}{print $1}' | xargs  -n 1 -P ${n_cores} -I {} xsv stats --everything {}_${from}.csv -o {}_${from}_Stats.csv
