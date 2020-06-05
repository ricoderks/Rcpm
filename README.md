### Introduction

The Rcpm package contains some general functions for use within CPM. As of June 5th 2020 all `xcms` related functions are removed. `xcms` uses a new api now and these functions where not usefull anymore. Maybe I will update these functions to the new api and release them as a seperate package.

### Installation

The Rcpm package depends on quite some packages and it may happen that you need to install some of them first. 

If your running Windows you need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed.

**Bioconductor** packages should be installed automatically now. For this to work SVN needs to be installed on your system. If you want to install them manually, below are the packages which you need to install :

 * `install.packages("BiocManager")`
 * `BiocManager::install(c("preprocessCore", "pcaMethods", "multtest"))`
 * If it asks to update, do not update packages. Make sure that you have a clean R open (no packages loaded)! 

For the installation of `Rcpm` you need the `devtools` package which you can install from the CRAN repository.
The Rcpm package can be installed with :

* `devtools::install_github("ricoderks/Rcpm")`

### Current status

Currently this package is updated frequently. So make sure to update the package frequently.
