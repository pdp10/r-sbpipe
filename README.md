# SBpiper - Data analysis functions for SBpipe

## Introduction
This R package provides an API for analysing repetitive parameter estimations and simulations of mathematical models. Examples of mathematical models are Ordinary Differential equations (ODEs) or Stochastic Differential Equations (SDEs) models. Among the analyses for parameter estimation, SBpiper calculates statistics and generates plots for parameter density, PCA of the best fits, parameter profile likelihood estimations (PLEs), and 2D parameter PLEs. These results can be generated using all or a subset of the best computed parameter sets. Among the analyses for model simulation, SBpiper calculates statistics and generates plots for deterministic and stochastic time courses via cartesian and heatmap plots. Plots for the scan of one or two model parameters can also be generated. This package is primarily used by the software [SBpipe](http://sbpipe.readthedocs.io).

**Citation:** Dalle Pezze P, Le Novère N. SBpipe: a collection of pipelines for automating repetitive
simulation and analysis tasks. *BMC Systems Biology*. 2017 Apr;11:46. [DOI:10.1186/s12918-017-0423-3](https://doi.org/10.1186/s12918-017-0423-3)


## Using this package within SBpipe
This dependency library is automatically installed by SBpipe via provided script or using conda, so no further step is needed. To install SBpipe, see [here](http://sbpipe.readthedocs.io). 


## Installation
SBpiper requires the installation of [R](https://www.r-project.org/) (≥ 3.2.0) environment. 
SBpiper is available on [CRAN](https://cran.r-project.org/package=sbpiper) and can be installed as follows: 
```
> install.packages("sbpiper")
```

Alternatively, users can install the latest version of SBpiper directly from GitHub:
```
> install.packages("devtools")
> devtools::install_github("pdp10/sbpiper")
```

or via conda:
```
conda install sbpiper -c conda-forge -c defaults
```

The R package is loaded as usual:
```
> library(sbpiper)
```

## Package building (developers)
Developers can check and build SBpiper using the following commands: 
```
> devtools::check("sbpiper")
> devtools::build("sbpiper")
```

or outside R with the commands:
```
R CMD build .
R CMD check *tar.gz --as-cran
```

Finally, sbpiper is installed with the command: 
```
R CMD INSTALL sbpiper_X.Y.Z.tar.gz
```

Conda recipe for SBpiper retrieves the code from the github branch: `develop`. 
```
# install anaconda-client
conda install anaconda-client
anaconda login

# build conda package:
conda-build conda_recipe/meta.yaml -c conda-forge -c defaults
```
