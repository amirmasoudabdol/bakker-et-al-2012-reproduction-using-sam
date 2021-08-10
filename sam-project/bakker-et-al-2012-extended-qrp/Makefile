.DEFAULT_GOAL:=help
SHELL:=/bin/bash

<b>:=\033[1m
</b>:=\033[0m

# This Makefile handles some of the common tasks in your project

outputpath=outputs/
configpath=configs/
sampath=build/
currentdatetime:=$(shell date '+%Y-%m-%d_%H-%M%p')

project=bakker-et-al-2012-extended-qrp

name=""
ifeq ($(name),"")
	archivefolder=$(currentdatetime)
else
	archivefolder=$(name)
endif

using=""
ifeq ($(using),"")
	configtemp=scripts/bakker-et-al-2012-extended-qrp_prepare_config_files.py
else
	configtemp=$(using)
endif

# In case you are running a very large simulattion, you can use this parameter
# to run more task on each node
nply=""
ifeq ($(nply),"")
	nply=1
else
	nply=$(nply)
endif

OSTYPE=$(shell uname -s)
ifeq ($(OSTYPE),Linux)
	ncores=$(shell grep -c ^processor /proc/cpuinfo)
endif 

ifeq ($(OSTYPE),Darwin)
    ncores=$(shell sysctl -n hw.ncpu)
endif

# Calculating the number of cores needed for running the job on Lisa
n_configs=$$(wc -l < configfilenames.pool)
n_nodes=$(shell echo $$(( $(n_configs)/(16 * $(nply)) + 1 )) )

# Number of cores to build SAM
n_cores=$(shell echo $$(( $(ncores) - 1)) )

.PHONY: help

help:  ## Display this help
	@printf "\nThis is Frodo too, a handy toolset for controling your newly created project."
	@printf "There are several commands are available for running your simulation locally or on a "
	@printf "cluster. It's also possible to run the simulation sequentially or in"
	@printf "parallel on your local machine."
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<command> \033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
	@echo
	@echo "Example Usage:"
	@echo "    make parameters using=~/projects_config/2012_project.py"
	@echo "    make run-seq-local; make archive name=Feb_5.zip"

##@ [Paramters]

using: ## Path to a new parameter generator
	@echo "Path to a new Python script for generating parameters, example usuage: 'make parameters using=my_config_grid.py"

name: ## The name of the archive
	@echo "Indicates the name of the archive, example usage: 'make archive name=my_archive"


##@ --------------------------------------------------------------
##@ Build 

sam-build: ## Rebuild the binaries, run this if you've modified the code
	# TODO: Run the `cmake` first
	@cmake -DCMAKE_BUILD_TYPE=Release -HSAM/SAM -BSAM/SAM/build
	@cmake --build SAM/SAM/build --parallel $(n_cores)
	@mv SAM/SAM/build/SAMrun .

parameters: clean ## Preparing config files for SAM
	@printf '$(<b>)> Make sure that you have set all the parameters correctly... $(</b>)\n'
	@printf '$(<b>)> Make sure that you have updated the "data" dictionary in "main()"... $(</b>)\n'

	@python3 $(configtemp)

##@ Post-processing

csv: ## Create a set of files `*_prepared.csv` by adding all keys/values to them. Use this when the database is too big
	@printf '$(<b>)> Preparing the extended dataset for $(from)... $(</b>)\n'
	@python3 scripts/bakker-et-al-2012-extended-qrp_to_csv.py $(from)

stack: ## Stack several CSV files into one CSV file
	@printf '$(<b>)> Stacking $(from) output files... $(</b>)\n'
	@xsv cat rows outputs/*_$(from)_prepared.csv -o outputs/$(from)_Stacked.csv

stacker: ## Faster version of stacker. Use this for lager data files
	@printf '$(<b>)> Stacking $(from) output files... $(</b>)\n'
	@touch outputs/$(from)_Stacked.csv
	@./scripts/bakker-et-al-2012-extended-qrp_large_stacker.sh $(from)

database: ## Aggregate output files into a SQLite database
	@$(MAKE) stats from=$(from)
	@$(MAKE) csv from=$(from)_Stats
	@$(MAKE) stack from=$(from)_Stats
	@printf '$(<b>)> Creating a table from $(from) stacked output file... $(</b>)\n'
	@csvsql --db sqlite:///dbs/bakker-et-al-2012-extended-qrp.sqlite --insert outputs/$(from)_Stats_Stacked.csv

summary: ## Runinng the `post-processing.R` script on every file in output/ folder
	@printf '$(<b>)> Summarizing the dataset... $(</b>)\n'
	@Rscript scripts/bakker-et-al-2012-extended-qrp_post_processing.R

stats: ## Use `xsv` tool to group and calculate summary statistics of each file
	@printf '$(<b>)> Preparing statistics from $(from)... $(</b>)\n'
	@./scripts/bakker-et-al-2012-extended-qrp_prepare_stats.sh $(from) $(n_cores)
	@python3 scripts/bakker-et-al-2012-extended-qrp_transpose_csv.py $(from)_Stats

##@ Run

run-seq-local: ## Running the simulation sequentially on the local machine
	@printf '$(<b>)> Running SAM on the local machine... $(</b>)\n'
	@./scripts/bakker-et-al-2012-extended-qrp_local_seq_run.sh

run-par-local: ## Running the simulation in parallel on the local machine
	@./scripts/bakker-et-al-2012-extended-qrp_local_par_run.sh

run-par-lisa: ## Running the simulation in parallel on the Lisa cluster
	@printf '$(<b>)> Preparing Lisa enviroment, and submitting an array job... $(</b>)\n'
	@awk '{gsub(/$(project)_nply/,"$(nply)");}1' scripts/$(project)_lisa_par_run.sh > tmp && mv tmp scripts/$(project)_lisa_par_run.sh
	module load pre2019
	module load stopos
	stopos purge -p bakker-et-al-2012-extended-qrp_pool
	stopos create -p bakker-et-al-2012-extended-qrp_pool
	stopos add -p bakker-et-al-2012-extended-qrp_pool configfilenames.pool
	chmod +x scripts/bakker-et-al-2012-extended-qrp_lisa_par_run.sh
	@echo "Requesting $(n_nodes) nodes..."
	sbatch -a 1-$(n_nodes) scripts/bakker-et-al-2012-extended-qrp_lisa_par_run.sh

run-par-lisa-batch: ## Running the simulation in parallel on the Lisa cluster
	@printf '$(<b>)> Preparing Lisa enviroment, and submitting an array job... $(</b>)\n'
	@sed -i '49s/.*/nply=$(nply)/' scripts/bakker-et-al-2012-extended-qrp_lisa_par_run_batch.sh
	module load 2020
	module load Stopos/0.93-GCC-9.3.0
	stopos purge -p bakker-et-al-2012-extended-qrp_pool
	stopos create -p bakker-et-al-2012-extended-qrp_pool
	stopos add -p bakker-et-al-2012-extended-qrp_pool configfilenames.pool
	chmod +x scripts/bakker-et-al-2012-extended-qrp_lisa_par_run_batch.sh
	@echo "Requesting $(n_nodes) nodes..."
	sbatch -a 1-$(n_nodes) scripts/bakker-et-al-2012-extended-qrp_lisa_par_run_batch.sh

##@ Packaging

archive: ## Archiving the entire project directory to ../bakker-et-al-2012-extended-qrp_archive/archive/CUREENT_DATE_TIME or ../bakker-et-al-2012-extended-qrp_analysis/archive/<name>
	@printf '$(<b>)> Copying everything to ../bakker-et-al-2012-extended-qrp_archive/$(archivefolder)... $(</b>)\n'

	@mkdir -pv ../bakker-et-al-2012-extended-qrp_archive/$(archivefolder)
	@cp -rv * ../bakker-et-al-2012-extended-qrp_archive/$(archivefolder)

compress: ## Compress the outputs/*, logs/* and configs/*
	7z a sam.zip SAM/
	7z a outputs.zip outputs/
	7z a configs.zip configs/
	7z a logs.zip logs/

##@ Cleanup

clean: ## Remove most project specific files, configs, jobs, logs, etc.
	@printf '$(<b>)> Cleaning up configs/* logs/* jobs/*... $(</b>)\n'
	@rm -rf configs
	@rm -rf logs
	@rm -rf jobs

	@mkdir -p configs
	@mkdir -p logs
	@mkdir -p jobs

	@printf '$(<b>)> Removing parameters pool... $(</b>)\n'

	@rm -rf configfilenames.pool

veryclean: clean ## Remove all project files, including outputs 
	@printf '$(<b>)> Cleaning project specific files, outputs/*, dbs/*, ... $(</b>)\n'

	@rm -rf outputs
	@rm -rf dbs

	@mkdir -p outputs
	@mkdir -p dbs
	
	@rm -rf slurm-*.out
