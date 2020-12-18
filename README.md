# Overview

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1174869.svg)](https://doi.org/10.5281/zenodo.1174869)

This repository contains all reference soil chemical and spectral data, metadata, code and text to reproduce the 
calibration models and the manuscript for the soil spectral library that was 
created in order to assess soil-fertility related properties in four landscapes
across the West African yam belt. The study related to this repository was conducted within the [YAMSYS project](http://www.yamsys.org), and aimed to deliver a spectroscopy library that allows to cost-effectively evaluate current soil status and effects of innovative agronomic yam practices on key soil properties. Two of
the sampled landscapes, covering 20 fields within an area of about 10 times 10 square kilometers each, are in Côte d'Ivoire, named Liliyo and
Tieningboué, and two in Burkina Faso, named Midebdo and Léo.

All analyses in this project were conducted within the [R software environment for statistical computing](https://www.r-project.org), R version 3.6.0 (2019-04-26). 

The spectroscopic model library presented in this manuscript is built upon the [`simplerspec`](https://github.com/philipp-baumann/simplerspec) R package, a framework that aims to
streamline data processing and modeling for infrared diffuse reflectance spectroscopy applications.


# Project directory structure and important files

Below is a short overview of how folders and files are organized. The project
root folder (`./`) contains every file necessary to run the analysis and produce
the manuscript including table and figure outputs.

```
.
├── LICENSE
├── R
│   ├── utils-graph.R
│   ├── utils-misc.R
│   ├── utils-model-results.R
│   └── vip-wrappers.R
├── README.html
├── README.md
├── _drake.R
├── _make.R
├── code
│   ├── 10_compile-ref-data.R
│   ├── 11_summarize-ref-data.R
│   ├── 12_create-sampling-maps.R
│   ├── 20_build-spc-models.R
│   ├── 21_evaluate-spc-models.R
│   └── 22_interpret-spc-models-vip.R
├── data
│   ├── metadata-field
│   ├── soilchem
│   └── spectra
├── models
│   ├── rep-kfold-cv
│   └── rep-kfold-cv-all-vars
├── out
│   ├── data
│   ├── figs
│   ├── files
│   └── tables
├── packages.R
├── renv
│   ├── library
│   └── settings.dcf
└── yamsys-soilspec-publication.Rproj
```

Here is a short description of key files contained in sub-folders of the 
project main directory:

| Folder path | File | Description |
| -------- | -------------------- | --------------------------------------- |
| `./` | `...` | Project root directory |
| `./code/` | [`10_compile-ref-data.R`](10_compile-ref-data.R) | Compile chemical reference analysis data prior to developing spectroscopic reference models for the YAMSYS pilot landscapes. |
| `./code/` | [`11_summarize-ref-data.R`](11_summarize-ref-data.R) | Summarize chemical reference analysis data using boxplots by soil property and landscape. |
| `./code/` | [`12_create-sampling-maps.R`](12_create-sampling-maps.R) | Create geographical maps that depict positions of sampled fields within the four pilot landscapes.
| `./code/` | [`20_build-spc-models.R`](20_build-spc-models.R) | Build spectroscopic reference models covering sampled fields within the four YAMSYS pilot landscapes. Tune PLS regression models using 5 times repeated 10-fold cross-validation, and derive final models at optimal number of components, develop one final model for each soil property. |
| `./code/` |  [`21_evaluate-spc-models.R`](21_evaluate-spc-models.R) | Make model evaluation summary (predicted vs. observed) plots for models with R-squared higher than 0.75). |
| `./code/` |  [`22_interpret-spc-models-vip.R`](22_interpret-spc-models-vip.R) | Compute and plot Variable Importance in the Projection (VIP) scores of PLS regression models for total soil C, total N and clay content, including overlaid raw and preprocessed spectra. |
| [`./data/`](data) | ... | Contains all input data required for data transformation, analysis and modeling within the R environment for statistical computing. |
| [`./data/metadata-field/`](data/metadata-field) | [`metadata-field-yamsys.csv`](data/metadata-field/metadata-field-yamsys.csv) | Contains metadata about sampled yam fields as `.csv` text file. |
| [`./data/soilchem/`](data/soilchem) | [`metadata_soilchem_yamsys.txt`](data/soilchem/metadata_soilchem_yamsys.txt) | Metadata about the laboratory reference data set in `./data/soilchem/soilchem_yamsys.csv`. Description of column names in header (IDs, covariates and soil properties) and details about the laboratory reference analyses. Simple text file.
| [`./data/soilchem/`](data/soilchem) | [`soilchem_yamsys.csv`](data/soilchem/soilchem_yamsys.csv) | Soil chemical reference data set as `.csv` text file.
| [`.data/spectra/`](data/spectra) | `<sample_id>.<spc_rep_number>` | Bruker OPUS binary spectrometer files containing all spectral information. File extensions, `<spc_rep_numbers>`, are sequentially by `<sample_id>` numbered integers, starting with `0`, and representing replicate spectroscopic measurements of a sample. Bruker OPUS files contain the entire set of measurement parameters and all saved spectra of different types, for this project sample mid-infrared reflectance spectra, reference (KBr) reflectance spectra, non-atmospherically (CO<sub>2</sub>, water absorption bands) corrected spectra, and final atmospherically corrected spectra. The binary files are read using [this file reader function](https://github.com/philipp-baumann/simplerspec/blob/master/R/read-opus-universal.R) available in the [simplerspec](https://github.com/philipp-baumann/simplerspec) R package. |
| [`./manuscript/`](manuscript) | `...` | Contains all files to re-create the manuscript of the publication on the YAMSYS soil spectroscopy reference library. Reproducibilty of text and results is achieved by combining GNU make, R Markdown and LaTex.
| [`./models/rep-kfold-cv`](models/rep-kfold-cv) | `pls_<soil_property>.Rds` | Contains R model outputs from all developed PLS regression models. The `.Rds` files are a binary representation of R list output from `simplerspec::fit_pls()` and i.e. contain list element `model`, which contains the `caret::train()` output that can for example be used to make predictions from new spectra using the calibration models developed in this spectral library. The custom function `simplerspec::predict_from_spc()` can e.g. be used to return predictions from multiple soil property spectroscopic models in the form of tidy data frames, including metadata such as `sample_id` or `unique_id` (`sample_id` combined with measurement date and time). |
| [`./out`](out) | `...` | Contains miscellaneous file outputs of intermediary processed data and figures that are generated from code in primary R scripts located in `./`. |
| [`./out/data/`](out/data) | [`spec_chem.Rds`](out/data/spec_chem.Rds) | Tibble data frame containing the following columns for each replicate spectrum (row): measurement IDs (columns `unique_id`, `file_id`, `sample_id`); spectral metadata (list-column column `metadata`; list of tibble data frames); raw, resampled and preprocessed spectral data as lists of `data.tables` (list-columns `spc`, `spc_rs` and `spc_pre`); wavenumber vectors of raw, resampled and preprocessed spectra (list-columns `wavenumbers`, `wavenumbers_rs` and `xvalues_pre`). |
| [`./out/figs/`](out/figs) | `.pdf` | Figure output for the manuscript, the same duplicate files are also generated within `./manuscript/figs/`, but here named verbosely instead of following figure naming submission requirements of the journal. |
| `./` |  [`yamsys-soilspec-publication.Rproj`](yamsys-soilspec-publication.Rproj) | RStudio project file that can be used as a shortcut for opening the project directly from the file system. Double-clicking this file after downloading, the entire self-contained project will open RStudio and automatically set the R working directory to the project root. This avoids hard-coding file paths beneath the project directory hierarchy within all R scripts, thereby facilitating code transfer for spectroscopic models and reproducibility. See [here](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) for details. |
| `./` | [`packages.R`](packages.R) | Script that loads all R packages required for the analysis. |

# R session info

Below is the R `sessionInfo()` output after loading required packages ([tidyverse collection of packages](https://www.tidyverse.org) and [simplerspec](https://github.com/philipp-baumann/simplerspec)) with `library()`.
This was the computational environment based on which the analysis of this project was conduced and reported accordingly in the manuscript.

```
\> sessionInfo()
R version 3.6.0 (2019-04-26)
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS/LAPACK: /usr/lib64/R/lib/libRblas.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] xtable_1.8-4             ChemometricsWithR_0.1.13 caret_6.0-85            
 [4] lattice_0.20-38          data.table_1.12.8        doFuture_0.10.0         
 [7] future_1.21.0            simplerspec_0.1.0.9001   foreach_1.5.1           
[10] ggrepel_0.8.2            cowplot_1.0.0            R.utils_2.10.1          
[13] R.oo_1.24.0              R.methodsS3_1.8.1        here_0.1                
[16] forcats_0.4.0            stringr_1.4.0            dplyr_1.0.0             
[19] purrr_0.3.3              readr_1.3.1              tidyr_1.0.0             
[22] tibble_3.0.2             ggplot2_3.3.2            tidyverse_1.3.0         
[25] drake_7.12.7            

loaded via a namespace (and not attached):
 [1] colorspace_1.4-1          prospectr_0.1.3           ellipsis_0.3.1           
 [4] class_7.3-17              rprojroot_1.3-2           RcppArmadillo_0.9.800.3.0
 [7] pls_2.7-2                 fs_1.4.2                  rstudioapi_0.13          
[10] listenv_0.8.0             farver_2.0.1              remotes_2.2.0            
[13] prodlim_2019.11.13        fansi_0.4.1               lubridate_1.7.4          
[16] xml2_1.2.2                codetools_0.2-16          splines_3.6.0            
[19] doParallel_1.0.14         pkgload_1.1.0             jsonlite_1.7.2           
[22] pROC_1.16.0               broom_0.7.2               dbplyr_1.4.2             
[25] compiler_3.6.0            httr_1.4.2                backports_1.1.5          
[28] assertthat_0.2.1          Matrix_1.2-17             cli_2.2.0                
[31] prettyunits_1.1.0         tools_3.6.0               igraph_1.2.6             
[34] gtable_0.3.0              glue_1.4.1                reshape2_1.4.3           
[37] Rcpp_1.0.5                cellranger_1.1.0          vctrs_0.3.1              
[40] nlme_3.1-140              iterators_1.0.12          timeDate_3043.102        
[43] xfun_0.11                 gower_0.2.1               globals_0.14.0           
[46] ps_1.5.0                  testthat_3.0.0            rvest_0.3.5              
[49] lifecycle_0.2.0           renv_0.9.3                devtools_2.3.2           
[52] MASS_7.3-53               scales_1.1.0              ipred_0.9-9              
[55] kohonen_3.0.10            hms_0.5.3                 parallel_3.6.0           
[58] memoise_1.1.0             rpart_4.1-15              stringi_1.4.3            
[61] desc_1.2.0                e1071_1.7-4               filelock_1.0.2           
[64] pkgbuild_1.1.0            lava_1.6.6                storr_1.2.5              
[67] rlang_0.4.9               pkgconfig_2.0.3           recipes_0.1.9            
[70] labeling_0.3              processx_3.4.5            tidyselect_1.1.0         
[73] parallelly_1.21.0         plyr_1.8.5                magrittr_2.0.1           
[76] R6_2.4.1                  generics_0.0.2            base64url_1.4            
[79] txtq_0.2.3                DBI_1.1.0                 pillar_1.4.3             
[82] haven_2.2.0               withr_2.3.0               survival_2.43-3          
[85] nnet_7.3-12               modelr_0.1.5              crayon_1.3.4             
[88] utf8_1.1.4                usethis_2.0.0             progress_1.2.2           
[91] grid_3.6.0                readxl_1.3.1              callr_3.5.1              
[94] ModelMetrics_1.2.2        reprex_0.3.0              digest_0.6.23            
[97] stats4_3.6.0              munsell_0.5.0             sessioninfo_1.1.1
```

