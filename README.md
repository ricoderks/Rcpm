### Introduction

The Rcpm package contains some general functions for use within CPM.

### Installation

The Rcpm package depends on quite some packages and they need to be installed first:
* check if you can load the package grid with `library(grid)`. It the package doesn't load you need to upgrade your R installation.

* `install.packages(c("plyr", "ggplot2", "reshape2", "pls", "knitr"))`.

Some packages are hosted by BioConductor, install like:

* `source("http://bioconductor.org/biocLite.R")`
* `biocLite(c("preprocessCore", "pcaMethods"))`
* if it asks to update, update all packages.

Finally the R package is installed with :

* `install.packages("Rcpm", repos="http://cpm.lumc.nl/R/", type="source")`


### Current status

Currently this package is updated frequently. So make sure to update the package frequently.

### Repository

Create repository with : `tools::write_PACKAGES(dir="location of package", type="source")`