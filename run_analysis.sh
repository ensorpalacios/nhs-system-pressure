#!/usr/bin/env bash
set -euo pipefail

echo -e "\nprepare data"
Rscript src/prepare-data.R train

echo -e "\npredictor plots"
Rscript src/predictors-plot.R train

echo -e "\npredictor test plot"
Rscript src/xreg-prediction-test.R train

#for mode in train test; do
#	echo -e "\nRunning analysis on $mode data"
#	echo "fit models"
#	Rscript src/fit_fc.R mode
#	
#	echo -e "\ncompute threshold"
#	Rscript src/compute-threshold.R mode
#	
#	echo -e "\ncompute metrics"
#	Rscript src/metrics-compute.R mode
#
#	echo -e "\npredict risk"
#	Rscript src/risk-predict.R mode
#
#	echo -e "\nplot metrics"
#	Rscript src/metrics-plot.R train
#
#	echo -e "\nplot forecasts"
#	Rscript src/fc-plot.R mode
#
#	echo -e "\nplot risk predictions"
#	Rscript src/risk-plot.R mode
#done


