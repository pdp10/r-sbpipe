# This file is part of sbpipe.
#
# sbpipe is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# sbpipe is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with sbpipe.  If not, see <http://www.gnu.org/licenses/>.
#
#
#
# $Revision: 3.0 $
# $Author: Piero Dalle Pezze $
# $Date: 2016-07-01 14:14:32 $


## retrieve SBpipe folder containing R scripts
#args <- commandArgs(trailingOnly = FALSE)
#SBPIPE_R <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
#source(file.path(SBPIPE_R, 'sbpipe_pe.r'))




# R Script to run model parameter estimation analysis and plot results. This script analyses
# best fits
#
# :model: the model name without extension.
# :finalfits_filenamein: the dataset containing the best parameter fits
# :plots_dir: the directory to save the generated plots.
# :best_fits_percent: the percent of best fits to analyse.
# :logspace: true if parameters should be plotted in logspace.
# :scientific_notation: true if axis labels should be plotted in scientific notation.
sbpipe_pe_main_final_fits <- function(model, finalfits_filenamein, plots_dir, best_fits_percent, 
                 logspace, scientific_notation) {

  if(logspace == 'True' || logspace == 'TRUE' || logspace == 'true') {
    logspace = TRUE
  } else {
    logspace = FALSE
  }
  
  if(scientific_notation == 'True' || scientific_notation == 'TRUE' || scientific_notation == 'true') {
    scientific_notation = TRUE
  } else {
    scientific_notation = FALSE
  }  
  

  dim_final_fits = dim(read.table(finalfits_filenamein, sep="\t"))[1]
  if(dim_final_fits-1 <= 1) {
      warning('Best fits analysis requires at least two parameter estimations. Skip.')
      stop()
  }

  df_final_fits = read.table(finalfits_filenamein, header=TRUE, dec=".", sep="\t")
  final_fits_analysis(model, df_final_fits, plots_dir, best_fits_percent, logspace, scientific_notation)

}


#main(commandArgs(TRUE))
## Clean the environment
#rm ( list=ls ( ) )

