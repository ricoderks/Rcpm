### Introduction

The Rcpm package contains some general functions for use within CPM.

### Installation

The Rcpm package depends on quite some packages and it may happen that you need to install some of them first. 

The packages from **Bioconductor** you need to install yourself first :
 * `source("http://bioconductor.org/biocLite.R")`
 * `biocLite(c("preprocessCore", "pcaMethods", "xcms", "multtest"))`
 * if it asks to update, update all packages. Make sure that you have a clean R open (no packages loaded)!
 Keep in mind that updating packages can be risky!

For the installation you need the **devtools** package which you can install from the CRAN repository.
The Rcpm package can be installed with :

* `devtools::install_git(url = "https://git.lumc.nl/rjederks/Rcpm.git", dependencies = TRUE)`

### Current status

Currently this package is updated frequently. So make sure to update the package frequently.