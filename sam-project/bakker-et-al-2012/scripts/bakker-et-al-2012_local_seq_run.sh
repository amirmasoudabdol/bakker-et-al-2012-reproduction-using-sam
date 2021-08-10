#!/bin/bash

PROJECT_DIR=$(pwd)

# echo
# echo "Running the simulation..."

function progressBar {

# Process data
	let _progress=(${1}*100/${2}*100)/100
	let _done=(${_progress}*4)/10
	let _left=40-$_done

# Build progressbar string lengths
	_done=$(printf "%${_done}s")
	_left=$(printf "%${_left}s")

# 1.2 Build progressbar strings and print the progressBar line
# 1.2.1 Output example:
# 1.2.1.1 Progress : [########################################] 100%
printf "\rProgress : [${_done// /#}${_left// /-}] ${_progress}%% ${3}"

}

# Variables
_number=1

# This accounts as the "totalState" variable for the progressBar function
_end=$(wc -l < configfilenames.pool)

for CONFIG_FILE in "configs/"*.json; do

	progressBar ${_number} ${_end} ${FILE_NAME}

	# Extracting the UUID, i.e., filename without extension
	FILE_NAME=$(basename ${CONFIG_FILE})
	CONFIG_FILE_NAME="${FILE_NAME%%.*}"

	"${PROJECT_DIR}"/SAMrun --config="${CONFIG_FILE}" \
								--output-path="${PROJECT_DIR}/outputs/" \
								--output-prefix="${CONFIG_FILE_NAME}" \
								--update-config

	SIM_FILE="${PROJECT_DIR}/outputs/${CONFIG_FILE_NAME}_sim.csv"

	_number=$(echo $((_number + 1)))

	# echo
	# echo "Running Rscripts"
	# Rscript ${PROJECT_DIR}/rscripts/post-analyzer.R ${SIM_FILE} FALSE

done

echo