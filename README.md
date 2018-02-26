# SBpiper - R package for SBpipe

## Introduction

R code for SBpipe (see: [https://pdp10.github.io/sbpipe](https://pdp10.github.io/sbpipe)). 
This package contains a collection of R utility functions used by SBpipe 
for plotting and generating statistics. Originally, this source code was distributed within SBpipe. 
SBpiper was conceived to improve modularity and enable the invokation of SBpipe functions for data analysis and plot generation directly as an R library.

## How to use this package with SBpipe
This dependency library is automatically installed by SBpipe via provided script or using conda, so no further step is needed. See the above link for instructions on how to install SBpipe. 

## How to use this package as an R library
The below information should be used by those users who intend to use this library directly in their 
R source code and do NOT plan to install SBpipe.
SBpiper can directly be installed via github:
```
> install.packages("devtools")
> devtools::install_github("pdp10/sbpiper")
```

or via conda:
```
$ conda install sbpiper -c pdp10 -c r -c defaults
```

The R package is loaded as usual:
```
> library(sbpiper)
```

## How to build this package (for developers)
Developers can check and build SBpiper using the following commands: 
```
> devtools::check("sbpiper")
> devtools::build("sbpiper")
```

or outside R with the commands:
```
$ R CMD build .
$ R CMD check *tar.gz --as-cran
```

Finally, sbpiper is installed with the command: 
```
$ R CMD INSTALL sbpiper_X.Y.Z.tar.gz
```

Conda recipe for SBpiper retrieves the code from the github branch: `develop`. 
Anaconda client is needed and can be installed with the following commands:
```
$ conda install anaconda-client
$ anaconda login
```
Build conda package:
```
$ conda-build conda_recipe/meta.yaml -c pdp10 -c r -c defaults
```



**How to cite SBpipe / SBpiper:**

Dalle Pezze, P and Le Novère, N. (2017) BMC Systems Biology 11:46. SBpipe: a collection of pipelines for automating repetitive simulation and analysis tasks. DOI:10.1186/s12918-017-0423-3

Thanks for using sbpiper!
