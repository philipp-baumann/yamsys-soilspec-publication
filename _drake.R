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

# Script-based workflow: convert to drake plan
# all assigned R objects in scripts are drake targets
scripts <- R.utils::listDirectory("code", fullNames = TRUE)
plans <- lapply(setNames(object = scripts, nm = scripts), drake::code_to_plan)

# Define and make the plan ====================================================

# Set up parallization
future::plan(future::multicore)
doFuture::registerDoFuture()
doParallel::registerDoParallel()

## Finalize into one master drake plan, make configuration, and run workflow ===

# Combine multiple plans into one; use fast object serialization with qs package
plan <- do.call(rbind, plans) %>% mutate(format = "qs")

## Build targets (R objects) listed in plan ====================================

# Within-target parallelism with multicore or local socket cluster
drake::make(plan, cache_log_file = TRUE, lock_envir = FALSE)