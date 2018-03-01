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



#' Main R function for SBpipe pipeline: parameter_scan1(). 
#'
#' @param model the model name
#' @param inhibition_only true if the scanning only decreases the variable amount (inhibition only)
#' @param inputdir the input directory containing the simulated data
#' @param outputdir the output directory that will contain the simulated plots
#' @param run the simulation number
#' @param percent_levels true if scanning levels are in percent
#' @param min_level the minimum level
#' @param max_level the maximum level
#' @param levels_number the number of levels
#' @param homogeneous_lines true if lines should be plotted homogeneously
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @export
#' \donttest{
#' data(insulin_receptor_ps1_l0)
#' data(insulin_receptor_ps1_l1)
#' data(insulin_receptor_ps1_l3)
#' data(insulin_receptor_ps1_l4)
#' data(insulin_receptor_ps1_l6)
#' data(insulin_receptor_ps1_l8)
#' data(insulin_receptor_ps1_l9)
#' data(insulin_receptor_ps1_l11)
#' data(insulin_receptor_ps1_l13)
#' data(insulin_receptor_ps1_l14)
#' data(insulin_receptor_ps1_l16)
#' dir.create(file.path("ps1_datasets"))
#' write.table(insulin_receptor_ps1_l0, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_0.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l1, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_1.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l3, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_3.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l4, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_4.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l6, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_6.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l8, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_8.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l9, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_9.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l11, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_11.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l13, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_13.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l14, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_14.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l16, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_16.csv"), 
#'             row.names=FALSE)
#' sbpiper_ps1(
#'        model="insulin_receptor_scan_IR_beta", 
#'        inhibition_only=FALSE,
#'        inputdir="ps1_datasets", 
#'        outputdir="ps1_plots",
#'        run=1,
#'        percent_levels=TRUE, 
#'        min_level=0, 
#'        max_level=250, 
#'        levels_number=10,
#'        homogeneous_lines=FALSE,
#'        xaxis_label="Time [m]", 
#'        yaxis_label="Level [a.u.]")
#' }
sbpiper_ps1 <- function(model, 
                       inhibition_only, 
                       inputdir, 
                       outputdir, 
                       run, 
                       percent_levels, 
                       min_level, 
                       max_level, 
                       levels_number, 
                       homogeneous_lines, 
                       xaxis_label, 
                       yaxis_label) {
  
  if(homogeneous_lines) {
    plot_single_param_scan_data_homogen(model,
                                        inputdir, 
                                        outputdir, 
                                        run,
                                        xaxis_label, 
                                        yaxis_label)
  } else {    
    plot_single_param_scan_data(model, 
                                inhibition_only,
                                inputdir, 
                                outputdir, 
                                run,
                                percent_levels, 
                                min_level,
                                max_level, 
                                levels_number,
                                xaxis_label, 
                                yaxis_label)
  }
}


#' Plot model single parameter scan time courses
#'
#' @param model The model name
#' @param inhibition_only true if the scanning only decreases the variable amount (inhibition only)
#' @param inputdir the input directory containing the simulated data
#' @param outputdir the output directory that will contain the simulated plots
#' @param run the simulation number
#' @param percent_levels true if scanning levels are in percent (default TRUE)
#' @param min_level the minimum level (default: 0)
#' @param max_level the maximum level (default: 100)
#' @param levels_number the number of levels (default: 10)
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @examples 
#' data(insulin_receptor_ps1_l0)
#' data(insulin_receptor_ps1_l1)
#' data(insulin_receptor_ps1_l3)
#' data(insulin_receptor_ps1_l4)
#' data(insulin_receptor_ps1_l6)
#' data(insulin_receptor_ps1_l8)
#' data(insulin_receptor_ps1_l9)
#' data(insulin_receptor_ps1_l11)
#' data(insulin_receptor_ps1_l13)
#' data(insulin_receptor_ps1_l14)
#' data(insulin_receptor_ps1_l16)
#' dir.create(file.path("ps1_datasets"))
#' write.table(insulin_receptor_ps1_l0, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_0.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l1, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_1.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l3, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_3.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l4, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_4.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l6, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_6.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l8, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_8.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l9, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_9.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l11, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_11.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l13, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_13.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l14, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_14.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_ps1_l16, 
#'             file=file.path("ps1_datasets",
#'                            "insulin_receptor_scan_IR_beta__rep_1__level_16.csv"), 
#'             row.names=FALSE)
#' plot_single_param_scan_data(
#'        model="insulin_receptor_scan_IR_beta", 
#'        inhibition_only=FALSE,
#'        inputdir="ps1_datasets", 
#'        outputdir="ps1_plots",
#'        run=1,
#'        percent_levels=TRUE, 
#'        min_level=0, 
#'        max_level=250, 
#'        levels_number=10, 
#'        xaxis_label="Time [m]", 
#'        yaxis_label="Level [a.u.]")
#' @export
plot_single_param_scan_data <- function(model, 
                                        inhibition_only,
                                        inputdir,
                                        outputdir, 
                                        run,
                                        percent_levels=TRUE, 
                                        min_level=0, 
                                        max_level=100, 
                                        levels_number=10, 
                                        xaxis_label="", 
                                        yaxis_label="") {
  
  writeLines(paste0("Model: ", model, ".cps"))
  #writeLines(outputdir)
  
  # create the directory of output
  if (!file.exists(outputdir)){ dir.create(outputdir) }
  
  files <- list.files( path=inputdir, pattern=paste0(model, '__rep_', run, '__level_'))
  #print(files)
  levels.index <- get_sorted_level_indexes(files)
  #print(levels.index)
  
  
  # Set the labels for the plot legend
  labels <- seq(as.numeric(min_level), as.numeric(max_level), (as.numeric(max_level)-as.numeric(min_level))/(as.numeric(levels_number)))
  labels <- round(labels, digits = 0)
  # Add percentages to the labels
  if(percent_levels) {
    labels <- paste0(labels, " %")
  }
  
  # Set the color and linetype for the plot
  colors <- c()
  linetype <- c()
  
  # deepskyblue.blue3 palette
  palette.deepskyblue.blue3 <- grDevices::colorRampPalette(c("deepskyblue", "blue3"))
  
  # Scanning using a virtual variable (A_percent_level) defining the percent level of its corresponding real variable (A).
  # The scanninig is therefore done by percent levels and at the beginning.
  # NOTE: A_percent_level=0  ==> A is knocked out (so 0%)
  if(inhibition_only) {
    #linetype <- c(1,6,4,3,2,1,6,4,3,2,1)
    # create my colours from palette
    colors <- c(palette.deepskyblue.blue3(as.numeric(levels_number)), "black")
  } else {
    #linetype <- c(6,4,3,2,1,6,5,4,3,2,6)
    # darkorchid.magenta palette
    palette.darkorchid.magenta <- grDevices::colorRampPalette(c("darkorchid", "magenta"))
    # create my colours from the dodger.blue3 palette and the darkorchid.magenta palette
    colors1 <- palette.deepskyblue.blue3(as.numeric(levels_number)/2)
    colors2 <- palette.darkorchid.magenta(as.numeric(levels_number)/2+1)
    colors <- c(colors1, "black", colors2)
  }
  
  # Read variable
  timecourses <- data.frame(data.table::fread(file.path(inputdir, files[1])))
  column <- names(timecourses)
  my_time <- timecourses[,c('Time')]
  # let's plot now! :)
  
  theme_set(tc_theme(36)) #28
  
  for(j in 2:length(column)) {
    print(column[j])
    
    # extract the data
    df <- data.frame(time=my_time, check.names=FALSE)
    for(m in 1:length(levels.index)) {
      #print(files[levels.index[m]])
      col.level <- data.table::fread(file.path(inputdir,files[levels.index[m]]), select=c(column[j]))
      df <- cbind(df, a=col.level)
      colnames(df)[ncol(df)] <- as.character(m+10)
    }
    
    # plot the data
    df.melt <- reshape2::melt(df, id=c("time"), variable.name='variable', value.name='value')
    g <- ggplot() + 
      geom_line(data=df.melt, aes_string(x="time", y="value", group="variable", color="variable"), size=1.0) +
      scale_colour_manual("Levels", values=colors, labels=labels) +
      # scale_linetype_manual("Levels", values=linetype, labels=labels) +
      xlab(xaxis_label) + ylab(yaxis_label) + ggtitle(column[j]) +
      theme(legend.title=element_blank(), legend.position="bottom", legend.key.height=unit(0.5, "in")) +
      ggsave(file.path(outputdir, paste0(model, "__rep_", run, "__eval_", column[j], ".png")),
             dpi=300,  width=8, height=8)
  }
}


#' Plot model single parameter scan time courses using homogeneous lines.
#'
#' @param model The model name
#' @param inputdir the input directory containing the simulated data
#' @param outputdir the output directory that will contain the simulated plots
#' @param run the simulation number
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @export
plot_single_param_scan_data_homogen <- function(model,
                                                inputdir,
                                                outputdir, 
                                                run,
                                                xaxis_label="", 
                                                yaxis_label="") {
  
  writeLines(paste0("Model: ", model, ".cps"))
  #writeLines(outputdir)

  # create the directory of output
  if (!file.exists(outputdir)){ dir.create(outputdir) }
  
  theme_set(tc_theme(36)) #28
  
  files <- list.files( path=inputdir, pattern=paste0(model, '__rep_', run, '__level_'))
  # Read variable
  timecourses <- data.frame(data.table::fread(file.path(inputdir, files[1])))
  column <- names(timecourses)
  my_time <- timecourses[,c('Time')]
  
  for(j in 2:length(column)) {
    print(column[j])
    
    # extract the data
    df <- data.frame(time=my_time, check.names=FALSE)
    for(m in 1:length(files)) {
      #print(files[levels.index[m]])
      col.level <- data.table::fread(file.path(inputdir,files[m]), select=c(column[j]))
      df <- cbind(df, a=col.level)
      colnames(df)[ncol(df)] <- as.character(m)
    }
    
    # plot the data
    df.melt <- reshape2::melt(df, id=c("time"), variable.name='variable', value.name='value')
    g <- ggplot() + 
      geom_line(data=df.melt, aes_string(x="time", y="value", group="variable"), color='blue', size=1.0) +
      xlab(xaxis_label) + ylab(yaxis_label) + ggtitle(column[j]) +
      ggsave(file.path(outputdir, paste0(model, "__rep_", run, "__eval_", column[j], ".png")),
             dpi=300,  width=8, height=8)
  }
}



#####################
# UTILITY FUNCTIONS #
#####################



#' Return the indexes of the files as sorted by levels.
#'
#' @param files the scanned files.
#' @return the index of the levels
get_sorted_level_indexes <- function(files) {
    levels <- c()
    levels.index <- c()

    # the array files MUST be sorted. Required to convert the string into numeric.
    # this is important because the legend must represent variable's knockdown in order.
    for(i in 1:length(files)) {
        num_of_underscores <- length(gregexpr("_", files[i])[[1]])
        levels <- c(levels, as.numeric(gsub(".csv", "", strsplit( files[i], "_")[[1]][num_of_underscores + 1]) ))
    }
    levels.temp <- c(levels)
    newmax <- max(levels)+1
    for(i in 1:length(levels)) {
        min <- which.min(levels.temp)
        #print(min(levels.temp))
        levels.index <- c(levels.index, min)
        levels.temp[min] <- newmax
    }
    return(levels.index)
}

