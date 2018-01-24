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


# retrieve SBpipe folder containing R scripts
#args <- commandArgs(trailingOnly = FALSE)
#SBPIPE_R <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
#source(file.path(SBPIPE_R, 'sbpipe_pe.r'))




# R Script to run model parameter estimation analysis and plot results. This script analyses
# all fits and the best fits.
#
# :model: the model name without extension.
# :finalfits_filenamein: the dataset containing the best parameter fits
# :allfits_filenamein: the dataset containing all the parameter fits
# :plots_dir: the directory to save the generated plots.
# :data_point_num: the number of data points used for parameterise the model.
# :fileout_param_estim_details: the name of the file containing the detailed statistics for the estimated parameters.
# :fileout_param_estim_summary: the name of the file containing the summary for the parameter estimation.
# :best_fits_percent: the percent of best fits to analyse.
# :plot_2d_66cl_corr: true if the 2D parameter correlation plots for 66% confidence intervals should be plotted.
# :plot_2d_95cl_corr: true if the 2D parameter correlation plots for 95% confidence intervals should be plotted.
# :plot_2d_99cl_corr: true if the 2D parameter correlation plots for 99% confidence intervals should be plotted.
# :logspace: true if parameters should be plotted in logspace.
# :scientific_notation: true if axis labels should be plotted in scientific notation.
sbpipe_pe_main <- function(model, finalfits_filenamein, allfits_filenamein, plots_dir, 
                           data_point_num, fileout_param_estim_details, fileout_param_estim_summary, 
                           best_fits_percent, plot_2d_66cl_corr, plot_2d_95cl_corr, plot_2d_99cl_corr, 
                           logspace, scientific_notation) {
  
  if(plot_2d_66cl_corr == 'True' || plot_2d_66cl_corr == 'TRUE' || plot_2d_66cl_corr == 'true') {
    plot_2d_66cl_corr = TRUE
  } else {
    plot_2d_66cl_corr = FALSE
  }

  if(plot_2d_95cl_corr == 'True' || plot_2d_95cl_corr == 'TRUE' || plot_2d_95cl_corr == 'true') {
    plot_2d_95cl_corr = TRUE
  } else {
    plot_2d_95cl_corr = FALSE
  }

  if(plot_2d_99cl_corr == 'True' || plot_2d_99cl_corr == 'TRUE' || plot_2d_99cl_corr == 'true') {
    plot_2d_99cl_corr = TRUE
  } else {
    plot_2d_99cl_corr = FALSE
  }  
  
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
  
  fits_analysis(model, finalfits_filenamein, allfits_filenamein, plots_dir, data_point_num, fileout_param_estim_details,
                    fileout_param_estim_summary, best_fits_percent, plot_2d_66cl_corr, plot_2d_95cl_corr, plot_2d_99cl_corr,
                    logspace, scientific_notation)
}


#main(commandArgs(TRUE))
## Clean the environment
#rm ( list=ls ( ) )

