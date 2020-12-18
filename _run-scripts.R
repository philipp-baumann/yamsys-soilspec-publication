
################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Publication title: "Estimation of soil properties with mid-infrared
##   soilspectroscopy across yam production landscapes in WestAfrica"
## Description: Create summary plot of chemical reference analysis data, which
##   are later used for developing spectroscopic reference models for the YAMSYS
##   pilot landscapes
################################################################################

# Packages
source("packages.R")

# Functions
R.utils::sourceDirectory("R")

# Set up parallization
future::plan(future::multicore)
doFuture::registerDoFuture()
doParallel::registerDoParallel()

# Source all analysis scripts
R.utils::sourceDirectory("code")