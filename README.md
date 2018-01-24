# r-sbpipe
[![LGPLv3 License](http://img.shields.io/badge/license-LGPLv3-blue.svg)](https://www.gnu.org/licenses/lgpl.html)

[![Build Status](https://travis-ci.org/pdp10/sbpipe.svg?branch=master)](https://travis-ci.org/pdp10/sbpipe)

[![Anaconda Cloud](https://anaconda.org/pdp10/sbpipe/badges/version.svg)](https://anaconda.org/pdp10/sbpipe)

R code for SBpipe (see: https://pdp10.github.io/sbpipe).


### Installation: 
- `install.packages("devtools")`
- `devtools::install_github("pdp10/r-sbpipe")`


### Packaging:
Within R: 
- `devtools::check("r-sbpipe")`
- `devtools::build("r-sbpipe")`

Then:
- `R CMD INSTALL r-sbpipe_X.Y.tar.gz`

