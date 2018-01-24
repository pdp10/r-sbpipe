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
# $Date: 2015-11-16 12:14:32 $


## retrieve SBpipe folder containing R scripts
#args <- commandArgs(trailingOnly = FALSE)
#SBPIPE_R <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
#source(file.path(SBPIPE_R, 'sbpipe_ps2.r'))



# R Script to plot model double parameter scan time courses.
#
# :model_noext: the model name without extension
# :scanned_par1: the 1st scanned parameter
# :scanned_par2: the 2nd scanned parameter
# :inputdir: the input directory
# :outputdir: the output directory
# :run: the simulation run
sbpipe_ps2_main <- function(model_noext, scanned_par1, scanned_par2, inputdir, outputdir, run) {

    # Add controls here if any
    
    plot_double_param_scan_data(model_noext, scanned_par1, scanned_par2, 
				inputdir, outputdir, run)
}


#main(commandArgs(TRUE))
## Clean the environment
#rm ( list=ls ( ) )

