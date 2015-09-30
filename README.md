### Introduction

The Rcpm package contains some general functions for use within CPM.

### Installation

The Rcpm package depends on quite some packages and they need to be installed first:

* `install.packages(c("plyr", "ggplot2", "grid", "reshape2", "pls", "knitr"))`.
Some packages are hosted by BioConductor, install like:

* `source("http://bioconductor.org/biocLite.R")`
* `biocLite(c("grid", "preProcessCore", "pcaMethods"))`
* if it asks to update, update all packages.

Finally the R package is installed with :

* `install.packages("Rcpm", repos="http://cpm.lumc.nl/R/", type="source")`


### Current status

Currently this package is updated frequently. So make sure to update the package frequently.

### Repository

Create repository with : `tools::write_PACKAGES(dir="location of package", type="source")`