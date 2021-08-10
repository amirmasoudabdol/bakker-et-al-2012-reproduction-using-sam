echo "#!/bin/bash"
echo "#SBATCH -N 1"
echo "#SBATCH -n 16"
echo "#SBATCH -p short"
echo "#SBATCH --constraint=avx"
echo "#SBATCH --mail-type=BEGIN,END"
echo "#SBATCH --mail-user=a.m.abdol@uvt.nl"
echo 
echo "# -----------------------------------"
echo "# Setting Paths"
echo
echo "PROJECT_DIR=\$(pwd)"
echo 
echo "FRODO_DIR=\${PROJECT_DIR}"
echo "SAMrr_DIR=\${PROJECT_DIR}/rscripts"
echo 
echo "# -----------------------------------"
echo "# Copying everything to the /scratch"
echo
echo "PROJECT_TMP_DIR=\${TMPDIR}/bakker-et-al-2012-extended-agg-qrp"
echo 
echo "mkdir \${PROJECT_TMP_DIR}"
echo "rsync -r \${PROJECT_DIR} \${PROJECT_TMP_DIR} --exclude configs --exclude outputs --exclude dbs --exclude .git"
echo "mkdir -p \${PROJECT_TMP_DIR}/configs \${PROJECT_TMP_DIR}/outputs"
echo 
echo "echo \"Copying the simulation output file to the \${TMPDIR}\""
echo "cp -v \${PROJECT_DIR}/outputs/${1}_sim.csv \${PROJECT_TMP_DIR}/outputs/"
echo 
echo "echo \"Computing Meta-Analysis Metrics\""
echo "SIM_FILE=\"\${PROJECT_TMP_DIR}/outputs/${1}_sim.csv\""
echo "Rscript \${PROJECT_TMP_DIR}/rscripts/post-analyzer.R \${SIM_FILE} FALSE"
echo "echo"
echo
echo "echo \"Copying back the outputs\""
echo "META_FILE=\"\${PROJECT_TMP_DIR}/outputs/${1}_meta.csv\""
echo "cp -v \${META_FILE} \${PROJECT_DIR}/outputs/"
