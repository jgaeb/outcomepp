# Set the number of parallel processes / threads
N_THREADS ?= 4
# Set the number of buckets for the monotonicity plot
N_BUCKETS ?= 10
# Set the number of bootstrap replications
N_BOOTSTRAP ?= 1000
# Set the number of tiles in the appendix plots
N_TILES ?= 100
# Set the larger alpha level
ALPHA0 ?= 0.6
# Set the smaller alpha level
ALPHA1 ?= 0.05

.PHONY: all clean

.DEFAULT_GOAL := all

.PRECIOUS: data/clean/ripa.rds \
					 data/clean/lending-mono.rds \
					 data/clean/lending-sim.rds \
					 data/clean/lending-hist.rds \
					 data/clean/lending-quant.rds \
					 data/clean/lending-calib.rds \
					 data/clean/compas-mono.rds \
					 data/clean/compas-sim.rds \
					 data/clean/compas-hist.rds \
					 data/clean/compas-quant.rds \
					 data/clean/compas-calib.rds \
					 data/clean/lsat-mono.rds \
					 data/clean/lsat-sim.rds \
					 data/clean/lsat-hist.rds \
					 data/clean/lsat-quant.rds \
					 data/clean/lsat-calib.rds \
					 data/clean/sqf-mono.rds \
					 data/clean/sqf-sim.rds \
					 data/clean/sqf-hist.rds \
					 data/clean/sqf-quant.rds \
					 data/clean/sqf-calib.rds

################################################################################
# RIPA

RIPA_DATA = data/ripa/12312022\ Supplement\ RIPA\ SD.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Alameda\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Alpine\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Amador\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Butte\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Calaveras\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ CHP\ 2022\ Q1.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ CHP\ 2022\ Q2.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ CHP\ 2022\ Q3.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ CHP\ 2022\ Q4.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Colusa\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Contra\ Costa\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Del\ Norte\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ El\ Dorado\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Fresno\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Glenn\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Humboldt\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Imperial\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Inyo\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Kern\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Kings\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Lake\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Lassen\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Los\ Angeles\ 2022\ Q1.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Los\ Angeles\ 2022\ Q2.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Los\ Angeles\ 2022\ Q3.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Los\ Angeles\ 2022\ Q4.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Madera\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Marin\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Mariposa\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Mendocino\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Merced\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Modoc\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Mono\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Monterey\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Napa\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Nevada\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Orange\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Placer\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Plumas\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Riverside\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Sacramento\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Benito\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Bernardino\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Diego\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Francisco\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Joaquin\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Luis\ Obispo\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ San\ Mateo\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Santa\ Barbara\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Santa\ Clara\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Santa\ Cruz\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Shasta\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Sierra\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Siskiyou\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Solano\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Sonoma\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Stanislaus\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Sutter\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Tehama\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Trinity\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Tulare\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Tuolumne\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Ventura\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Yolo\ 2022.xlsx \
					  data/ripa/RIPA\ Stop\ Data\ _\ Yuba\ 2022.xlsx

data/ripa/%.xlsx: download-ripa.sh
	@echo "Downloading RIPA data"
	@date
	time bash download-ripa.sh

data/clean/ripa.rds: $(RIPA-DATA) ripa.R
	@echo "Processing RIPA data"
	@date
	time Rscript ripa.R

################################################################################
# COMPAS

data/compas/compas.db: download-compas.sh
	@echo "Downloading COMPAS data"
	@date
	time bash download-compas.sh

data/clean/compas%.rds: data/compas/compas.db \
												compas.R \
												setup.R \
												monotonicity.R \
												simulation.R \
												model_checks.R
	@echo "Processing COMPAS data"
	@date
	time Rscript compas.R \
		--n_buckets $(N_BUCKETS) \
		--n_bootstrap $(N_BOOTSTRAP) \
		--n_tiles $(N_TILES) \
		--alpha0 $(ALPHA0) \
		--alpha1 $(ALPHA1)

################################################################################
# LSAT

data/lsat/lsac.csv: download-lsat.sh
	@echo "Downloading LSAT data"
	@date
	time bash download-lsat.sh

data/clean/lsat%.rds: data/lsat/lsac.csv \
											lsat.R \
											setup.R \
											monotonicity.R \
											simulation.R \
											model_checks.R
	@echo "Processing LSAT data"
	@date
	time Rscript lsat.R \
		--n_buckets $(N_BUCKETS) \
		--n_bootstrap $(N_BOOTSTRAP) \
		--n_tiles $(N_TILES) \
		--alpha0 $(ALPHA0) \
		--alpha1 $(ALPHA1)

################################################################################
# SQF

SQF_DATA = data/sqf/2008.csv \
           data/sqf/2009.csv \
           data/sqf/2010.csv \
           data/sqf/2011.csv \
           data/sqf/2012.csv

data/sqf/%.csv:
	@echo "Downloading SQF data"
	@date
	time bash download-sqf.sh

data/clean/sqf%.rds: $(SQF-DATA) sqf.R \
																 setup.R \
																 monotonicity.R \
																 simulation.R
	@echo "Processing SQF data"
	@date
	time Rscript sqf.R \
		--n_buckets $(N_BUCKETS) \
		--n_bootstrap $(N_BOOTSTRAP) \
		--n_tiles $(N_TILES) \
		--alpha0 $(ALPHA0) \
		--alpha1 $(ALPHA1)

################################################################################
# Plots

PLOTS = plots/main-inframarginality.pdf \
				plots/appendix-inframarginality.pdf \
				plots/ripa.pdf \
				plots/monotonicity.pdf \
				plots/threshold-simulation-robust.pdf \
				plots/threshold-simulation-standard.pdf \
				plots/beta-simulation-robust.pdf \
				plots/beta-simulation-standard.pdf \
				plots/calibration.pdf

plots/%.pdf: data/clean/ripa.rds \
						 data/clean/lending-mono.rds \
						 data/clean/lending-sim.rds \
						 data/clean/lending-hist.rds \
						 data/clean/lending-quant.rds \
						 data/clean/lending-calib.rds \
						 data/clean/compas-mono.rds \
						 data/clean/compas-sim.rds \
						 data/clean/compas-hist.rds \
						 data/clean/compas-quant.rds \
						 data/clean/compas-calib.rds \
						 data/clean/lsat-mono.rds \
						 data/clean/lsat-sim.rds \
						 data/clean/lsat-hist.rds \
						 data/clean/lsat-quant.rds \
						 data/clean/lsat-calib.rds \
						 data/clean/sqf-mono.rds \
						 data/clean/sqf-sim.rds \
						 data/clean/sqf-hist.rds \
						 data/clean/sqf-quant.rds \
						 data/clean/sqf-calib.rds \
						 plots.R
	@echo "Creating plots"
	@date
	time Rscript plots.R --n_buckets $(N_BUCKETS) --n_tiles $(N_TILES)

all: $(PLOTS)

clean:
	rm data/clean/compas-*.rds
	rm data/clean/lsat-*.rds
	rm data/clean/sqf-*.rds
	rm data/clean/ripa.rds
	rm plots/*

deep-clean:
	rm data/clean/*
	rm data/ripa/*
	rm data/compas/*
	rm data/lsat/*
	rm data/sqf/*
	rm plots/*
