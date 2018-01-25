# sbpiper
[![LGPLv3 License](http://img.shields.io/badge/license-LGPLv3-blue.svg)](https://www.gnu.org/licenses/lgpl.html)

[![Build Status](https://travis-ci.org/pdp10/sbpipe.svg?branch=master)](https://travis-ci.org/pdp10/sbpipe)

[![Anaconda Cloud](https://anaconda.org/pdp10/sbpipe/badges/version.svg)](https://anaconda.org/pdp10/sbpipe)

R code for SBpipe (see: https://pdp10.github.io/sbpipe).

## Users

#### Installation: 
- install.packages("devtools")
- devtools::install_github("pdp10/sbpiper")

#### Installation using Conda: 
- conda install -c pdp10 sbpiper


## Developers

#### Packaging:
Within R: 
- `devtools::check("sbpiper")`
- `devtools::build("sbpiper")`

Then:
- `R CMD INSTALL sbpiper_X.Y.Z.tar.gz`

#### Packaging for Conda: 
- conda metapackage sbpiper X.Y.Z --dependencies r-essentials --summary "R functions for pdp10/SBpipe"
- conda install anaconda-client
- anaconda login
- anaconda upload sbpiper-X.Y.Z-0.tar.bz2
