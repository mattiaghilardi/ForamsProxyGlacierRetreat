
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Taxonomic and functional diversity of benthic foraminifera as a promising proxy for tidewater glacier retreat

<!-- badges: start -->

[![License: MIT + file
LICENSE](https://img.shields.io/badge/License-MIT%20+%20file%20LICENSE-blue.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The goal of this project is to reproduce all analyses and figures of the
paper

> **Fossile E<sup>†</sup>, Ghilardi M<sup>†</sup>, Mojtahid M, Howa H,
> Baltzer A, Nardelli MP** Taxonomic and functional diversity of benthic
> foraminifera as a promising proxy for tidewater glacier retreat.
> *Submitted*
>
> <sup>†</sup>Equal authorship

## Content

This repository is structured as follow:

- [:file_folder:
  data/](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/data):
  contains all data used in the analyses (more info in the folder
  [README](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/data/README.md)).

- [:file_folder:
  derived_data/](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/derived_data):
  contains intermediate data generated with code (more info in the
  folder
  [README](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/derived_data/README.md)).

- [:file_folder:
  analysis/](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/analysis):
  contains R scripts to reproduce the analyses.

- [:file_folder:
  output/](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/output):
  contains all the outputs created during the analyses, including
  figures and tables.

- [:file_folder:
  R/](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/tree/main/R):
  contains R functions developed for this project.

- [:page_facing_up:
  DESCRIPTION](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/blob/main/DESCRIPTION):
  contains project metadata.

- [:page_facing_up:
  make.R](https://github.com/mattiaghilardi/ForamsProxyGlacierRetreat/blob/main/make.R):
  main R script to run the entire project.

## Instructions

- Clone this repository (for those not familiar with GitHub, click on
  the green button `Code` on the project main page on GitHub and then on
  `Download ZIP` to download the entire repository, thus unzip it).

- Open the `.Rproj` file in RStudio or open an R session with working
  directory set to the root of the project.

- Reproduce the entire project by running:

``` r
source("make.R")
```

## Notes

- When this project is launched, the appropriate version of
  [`renv`](https://rstudio.github.io/renv/articles/renv.html) should be
  automatically downloaded and installed into the project library. After
  this has completed, running `source("make.R")` will first restore the
  project library locally on the machine.
- All required packages and R functions will be loaded.
- A few lines of code are commented out in the scripts, as they are only
  needed to run scripts independently.

## Working environment

    #> R version 4.4.0 (2024-04-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 22.04.4 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
    #>  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
    #>  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: Europe/Paris
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices datasets  utils     methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.4.0    fastmap_1.2.0     cli_3.6.2         htmltools_0.5.8.1
    #>  [5] tools_4.4.0       rstudioapi_0.16.0 yaml_2.3.10       rmarkdown_2.27   
    #>  [9] knitr_1.48        xfun_0.44         digest_0.6.35     rlang_1.1.4      
    #> [13] renv_1.0.7        evaluate_0.24.0
