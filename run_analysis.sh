#!/usr/bin/env bash
set -euo pipefail

#echo -e "\nprepare data"
#Rscript src/prepare-data.R train

#echo -e "\npredictor plots"
#Rscript src/predictors-plot.R train

#echo -e "\npredictor test plot"
#Rscript src/xreg-prediction-test.R train

for mode in test; do
	echo -e "\n***Running analysis on $mode data***"
#	echo "\n***fit models***"
#	Rscript src/fit-fc.R $mode
#	
	if [[ "$mode" == "train" ]]; then 
	echo -e "\n***compute threshold***"
	Rscript src/compute-threshold.R $mode
	fi
	
	echo -e "\n***compute metrics***"
	Rscript src/metrics-compute.R $mode

	echo -e "\n***predict risk***"
	Rscript src/risk-predict.R $mode

	echo -e "\n***plot metrics***"
	Rscript src/metrics-plot.R $mode

	echo -e "\n***plot forecasts***"
	Rscript src/fc-plot.R $mode

	echo -e "\n***plot risk predictions***"
	Rscript src/risk-plot.R $mode
done


