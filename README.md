### Introduction

The Rcpm package contains some general functions for use within CPM.

### Installation

The Rcpm package depends on quite some packages and it may happen that you need to install some of them first. 

**Bioconductor** packages should be installed automatically now. For this to work SVN needs to be installed on your system. If you want to install them manually, below are the packages which you need to install :

 * `source("http://bioconductor.org/biocLite.R")`
 * `biocLite(c("preprocessCore", "pcaMethods", "xcms", "multtest"))`
 * if it asks to update, update all packages. Make sure that you have a clean R open (no packages loaded)!
 Keep in mind that updating packages can be risky!
 * for the development version of ggplot2 use: `devtools::install_github("tidyverse/ggplot2")`

For the installation of **Rcpm** you need the **devtools** package which you can install from the CRAN repository.
The Rcpm package can be installed with :

* `devtools::install_github("ricoderks/Rcpm")`

### Current status

Currently this package is updated frequently. So make sure to update the package frequently.

### Functions under development

Because of this I installend the development package of ggplot2 (v2.2.1.9000).

- bubble_plot
- diff_plot
- qc_plot

They can still change a lot so be carefull when you use them. I also still need to add some examples on how to use them.
