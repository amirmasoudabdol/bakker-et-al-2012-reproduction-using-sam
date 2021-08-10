#!/bin/bash

PROJECT_DIR=$(pwd)

OSTYPE=$(uname -s)
if [[ "$OSTYPE" == "Linux" ]]; then
	ncores=$(grep -c ^processor /proc/cpuinfo)
elif [[ "$OSTYPE" == "Darwin" ]]; then
    ncores=$(sysctl -n hw.ncpu)
else
	echo "Cannot deceted the OS."
fi
n_cores=$(echo $(( ${ncores} - 1)) )

echo
echo "Running the simulation on ${n_cores} cores."

find configs -name "*.json" | xargs -t -n 1 -P ${n_cores} -I {} ./SAMrun --config={} --output-path=outputs/

# nconfigfiles=$(wc -l < configfilenames.pool)

# niters=$(echo $(( (nconfigfiles / ncores))))

# for ((i=0; i<=niters; i++)) ; do

# 	slinen=$(echo $(((ncores * i)+1)) )
# 	nlinen=$(echo $((ncores * i + ncores)) )

# 	span="${slinen},${nlinen}p"

# 	FILES=$(sed -n ${span} < configfilenames.pool)

# 	echo "Running $slinen to $nlinen"

# 	for CONFIG_FILE in $FILES; do
# 	(
# 		# Extracting the UUID, i.e., filename without extension
# 		CONFIG_FILE="configs/${CONFIG_FILE}"
# 		FILE_NAME=$(basename ${CONFIG_FILE})
# 		CONFIG_FILE_NAME="${FILE_NAME%%.*}"

# 		echo "${CONFIG_FILE}"

# 		"${PROJECT_DIR}"/SAMrun --config="${CONFIG_FILE}" \
# 									--output-path="${PROJECT_DIR}/outputs/" \
# 									--output-prefix="${CONFIG_FILE_NAME}" \
# 									--update-config
# 		SIM_FILE="${PROJECT_DIR}/outputs/${CONFIG_FILE_NAME}_sim.csv"

# 		# echo
# 		# echo "Running Rscripts"
# 		# Rscript ${PROJECT_DIR}/rscripts/post-analyzer.R ${SIM_FILE} FALSE

# 		echo -e "- \c"

# 	) &
# 	done
# 	wait

# 	echo

# done