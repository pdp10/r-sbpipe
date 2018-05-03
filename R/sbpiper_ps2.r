# Copyright (c) 2018 Piero Dalle Pezze
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


######################
# EXPORTED FUNCTIONS #
######################



#' Main R function for SBpipe pipeline: parameter_scan2().
#'
#' @param model the model name
#' @param scanned_par1 the 1st scanned parameter
#' @param scanned_par2 the 2nd scanned parameter
#' @param inputdir the input directory
#' @param outputdir the output directory
#' @param run the simulation run
#' @examples
#' \donttest{
#' data(insulin_receptor_ps2_tp2)
#' dir.create(file.path("ps2_datasets"))
#' write.table(insulin_receptor_ps2_tp2, 
#'             file=file.path("ps2_datasets", 
#'                            "insulin_receptor_InsulinPercent__IRbetaPercent__rep_1__tp_2.csv"), 
#'             row.names=FALSE)
#' sbpiper_ps2(model="insulin_receptor_InsulinPercent__IRbetaPercent", 
#'            scanned_par1="InsulinPercent", 
#'            scanned_par2="IRbetaPercent", 
#'            inputdir="ps2_datasets", 
#'            outputdir="ps2_plots", 
#'            run=1)
#' }
#' @export
sbpiper_ps2 <- function(model, scanned_par1, scanned_par2, inputdir, outputdir, run) {
  
  plot_double_param_scan_data(model, scanned_par1, scanned_par2, 
                              inputdir, outputdir, run)
}


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
#'                            "insulin_receptor_InsulinPercent__IRbetaPercent__rep_1__tp_2.csv"), 
#'             row.names=FALSE)
#' plot_double_param_scan_data(model="insulin_receptor_InsulinPercent__IRbetaPercent", 
#'                             scanned_par1="InsulinPercent", 
#'                             scanned_par2="IRbetaPercent", 
#'                             inputdir="ps2_datasets", 
#'                             outputdir="ps2_plots", 
#'                             run=1)
#' @export
plot_double_param_scan_data <- function(model, scanned_par1, scanned_par2, inputdir, outputdir, run) {

    theme_set(basic_theme(36))

    writeLines(paste0("1st var: ", scanned_par1))
    writeLines(paste0("2st var: ", scanned_par2))
    # create the directory of output
    if (!file.exists(outputdir)){
        dir.create(outputdir)
    }

    # READ the files containing the time points. These will be plotted.

    # Iterate for each file representing time point data
    files <- list.files(path=inputdir, pattern=paste0(model, '__rep_', run, '__tp_'))
    files <- sort(files)
    
    if(length(files) < 1) {
        return()
    }

    lapply(files, function(filein) {
      
      print(paste('Processing file:', filein))
      # Read variable
      df.tp <- as.data.frame(data.table::fread(file.path(inputdir, filein)))
      
      # Extract the column names (except for time)
      # discard the first column (Time) and the columns of the two scanned parameters
      columns2discard <- c(colnames(df.tp)[1], scanned_par1, scanned_par2)
      # columns to plot. drop=FALSE is necessary because we don't want R converts 1 data frames into an array.
      columns <- colnames(df.tp[,!(colnames(df.tp) %in% columns2discard), drop = FALSE])

      # Extract the time point from the file name
      tp <- stringr::str_match(filein, "__tp_(.*?).csv")[, 2]
      
      lapply(columns, function(column) {
        g <- scatterplot_w_colour(df.tp, ggplot(), scanned_par1, scanned_par2, column) +
            ggtitle(paste0(column, ", t=", tp)) +
            theme(legend.title=element_blank(), 
                  legend.text=element_text(size=30),
                  legend.key.width = unit(0.5, "in"), 
                  legend.key.height = unit(0.65, "in"), 
                  plot.title = element_text(hjust = 0.5))
        ggsave(file.path(outputdir, paste0(model, "__eval_", column, '__rep_', run, "__tp_", tp, ".pdf")),
            dpi=300,  width=8, height=6)
      })
    })
    return()
}

