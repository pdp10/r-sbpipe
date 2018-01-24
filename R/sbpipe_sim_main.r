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
# $Date: 2016-07-7 16:14:32 $


## retrieve SBpipe folder containing R scripts
#args <- commandArgs(trailingOnly = FALSE)
#SBPIPE_R <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
#source(file.path(SBPIPE_R,'sbpipe_sim.r'))



# R Script to plot time courses and collect statistics.
#
# :model_noext: the model name without extension
# :inputdir: the input directory
# :outputdir: the output directory
# :outputfile: the output file name containing the statistics
# :repeats_file_template: the output template file storing the summary of model simulation repeats
# :exp_dataset: the file containing the experimental data.
# :plot_exp_dataset: TRUE if the experimental data should also be plotted
# :exp_dataset_alpha: the alpha level for the data set
# :xaxis_label: the label for the x axis (e.g. Time [min])
# :yaxis_label: the label for the y axis (e.g. Level [a.u.])
# :column_to_read: the name of the column to process
sbpipe_sim_main <- function(model_noext, inputdir, outputdir, outputfile, repeats_file_template, 
                 exp_dataset, plot_exp_dataset, exp_dataset_alpha, xaxis_label, yaxis_label, 
                 column_to_read) {

    if(plot_exp_dataset == 'True' || plot_exp_dataset == 'TRUE' || plot_exp_dataset == 'true') {
       print('experimental dataset will also be plotted')
       plot_exp_dataset = TRUE
    } else {
       plot_exp_dataset = FALSE
    }

    print('generating a table of statistics')
    gen_stats_table(inputdir, outputdir, model_noext, outputfile, xaxis_label, yaxis_label, column_to_read)

    print('summarising the time course repeats in tables')
    summarise_data(inputdir, model_noext, repeats_file_template, column_to_read)

    files <- list.files( path=inputdir, pattern=model_noext )
    if(length(files) > 1) {
        print('plotting separate time courses')
        plot_sep_sims(dirname(repeats_file_template), outputdir, model_noext, exp_dataset, plot_exp_dataset,
                      exp_dataset_alpha, xaxis_label, yaxis_label, column_to_read)
    }
    print('plotting combined time courses')
    plot_comb_sims(dirname(repeats_file_template), outputdir, model_noext, exp_dataset, plot_exp_dataset,
                   exp_dataset_alpha, xaxis_label, yaxis_label, column_to_read)
}


#main(commandArgs(TRUE))
## Clean the environment
#rm ( list=ls ( ) )

