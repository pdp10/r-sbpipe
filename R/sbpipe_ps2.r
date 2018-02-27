# This file is part of sbpiper.
#
# sbpiper is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# sbpiper is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with sbpiper.  If not, see <http://www.gnu.org/licenses/>.
#
#
#
# $Revision: 3.0 $
# $Author: Piero Dalle Pezze $
# $Date: 2016-07-6 12:14:32 $



######################
# EXPORTED FUNCTIONS #
######################



#' Plot model double parameter scan time courses.
#'
#' @param model the model name without extension
#' @param scanned_par1 the 1st scanned parameter
#' @param scanned_par2 the 2nd scanned parameter
#' @param inputdir the input directory
#' @param outputdir the output directory
#' @param run the simulation run
#' @examples
#' data(insulin_receptor_ps2_tp2)
#' dir.create(file.path("ps2_datasets"))
#' write.table(insulin_receptor_ps2_tp2, 
#'             file=file.path("ps2_datasets", 
#'                            "insulin_receptor_InsulinPercent__IRbetaPercent__rep_1__tp2.csv"), 
#'             row.names=FALSE)
#' plot_double_param_scan_data(model="insulin_receptor_InsulinPercent__IRbetaPercent", 
#'                             scanned_par1="InsulinPercent", 
#'                             scanned_par2="IRbetaPercent", 
#'                             inputdir="ps2_datasets", 
#'                             outputdir="plots", 
#'                             run=1)
#' @export
plot_double_param_scan_data <- function(model, scanned_par1, scanned_par2, inputdir, outputdir, run) {

    theme_set(basic_theme(36))

    writeLines(paste("1st var: ", scanned_par1, sep=""))
    writeLines(paste("2st var: ", scanned_par2, sep=""))
    # create the directory of output
    if (!file.exists(outputdir)){
        dir.create(outputdir)
    }

    # READ the files containing the time points. These will be plotted.

    # Iterate for each file representing time point data
    files <- list.files(path=inputdir, pattern=paste(model, '__rep_', run, '__tp_', sep=""))
    files <- sort(files)

    if(length(files) < 1) {
        return
    }

    for(j in 1:length(files)) {
      print(paste('Processing file:', files[j], sep=" "))
      # Read variable
      df.tp <- as.data.frame(data.table::fread(file.path(inputdir, files[j])))
      
      # Extract the column names (except for time)
      # discard the first column (Time) and the columns of the two scanned parameters
      columns2discard <- c(colnames(df.tp)[1], scanned_par1, scanned_par2)
      # columns to plot. drop=FALSE is necessary because we don't want R converts 1 data frames into an array.
      columns <- colnames(df.tp[,!(colnames(df.tp) %in% columns2discard), drop = FALSE])

      tp <- regmatches(files[j],regexec("__tp_ (.*?) .csv",files[j]))[[1]][2]
      print(files[j])
      print(tp)
      for(k in 1:length(columns)) {
        g <- scatterplot_w_colour(df.tp, ggplot(), scanned_par1, scanned_par2, columns[k]) +
            ggtitle(paste(columns[k], ", t=", tp, sep="")) +
            theme(legend.title=element_blank(), 
                  legend.text=element_text(size=30),
                  legend.key.width = unit(0.5, "in"), 
                  legend.key.height = unit(0.65, "in"), 
                  plot.title = element_text(hjust = 0.5))
        ggsave(file.path(outputdir, paste(model, "__eval_", columns[k], '__rep_', run, "__tp_", j-1, ".png", sep="" )),
            dpi=300,  width=8, height=6)
      }
  }
}

#' Main R function for SBpipe pipeline: parameter_scan2().
#'
#' @param model_noext the model name without extension
#' @param scanned_par1 the 1st scanned parameter
#' @param scanned_par2 the 2nd scanned parameter
#' @param inputdir the input directory
#' @param outputdir the output directory
#' @param run the simulation run
#' @examples
#' data(insulin_receptor_ps2_tp2)
#' dir.create(file.path("ps2_datasets"))
#' write.table(insulin_receptor_ps2_tp2, 
#'             file=file.path("ps2_datasets", 
#'                            "insulin_receptor_InsulinPercent__IRbetaPercent__rep_1__tp2.csv"), 
#'             row.names=FALSE)
#' sbpipe_ps2(model="insulin_receptor_InsulinPercent__IRbetaPercent", 
#'            scanned_par1="InsulinPercent", 
#'            scanned_par2="IRbetaPercent", 
#'            inputdir="ps2_datasets", 
#'            outputdir="plots", 
#'            run=1)
#' @export
#' @export
sbpipe_ps2 <- function(model_noext, scanned_par1, scanned_par2, inputdir, outputdir, run) {

  plot_double_param_scan_data(model_noext, scanned_par1, scanned_par2, 
                              inputdir, outputdir, run)
}

