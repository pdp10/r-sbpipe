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
# $Date: 2016-07-7 11:14:32 $



######################
# EXPORTED FUNCTIONS #
######################



#' Main R function for SBpipe pipeline: simulate(). 
#'
#' @param model the model name
#' @param inputdir the input directory
#' @param outputdir the output directory
#' @param outputfile_stats the output file containing the statistics
#' @param outputfile_repeats the output file storing the model simulation repeats
#' @param exp_dataset the file containing the experimental data.
#' @param plot_exp_dataset TRUE if the experimental data should also be plotted
#' @param exp_dataset_alpha the alpha level for the data set
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @param column_to_read the name of the column to process
#' @examples
#' \donttest{
#' data(insulin_receptor_1)
#' data(insulin_receptor_2)
#' data(insulin_receptor_3)
#' data(insulin_receptor_exp_dataset)
#' dir.create(file.path("sim_datasets"))
#' dir.create(file.path("sim_datasets_sum"))
#' write.table(insulin_receptor_1, 
#'             file=file.path("sim_datasets", "insulin_receptor_1.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_2, 
#'             file=file.path("sim_datasets", "insulin_receptor_2.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_3, 
#'             file=file.path("sim_datasets", "insulin_receptor_3.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_exp_dataset, 
#'             file="insulin_receptor_exp_dataset.csv", 
#'             row.names=FALSE)
#' sbpiper_sim(model="insulin_receptor", 
#'            inputdir="sim_datasets", 
#'            outputdir="sim_plots", 
#'            outputfile_stats="insulin_receptor_IR_beta_pY1146_stats.csv", 
#'            outputfile_repeats=file.path("sim_datasets_sum", 
#'                                         "insulin_receptor_IR_beta_pY1146.csv"), 
#'            exp_dataset="insulin_receptor_exp_dataset.csv", 
#'            plot_exp_dataset=TRUE, 
#'            exp_dataset_alpha=1.0, 
#'            xaxis_label=NULL, 
#'            yaxis_label=NULL, 
#'            column_to_read="IR_beta_pY1146")
#' }
#' @export
sbpiper_sim <- function(model, inputdir, outputdir, outputfile_stats, outputfile_repeats, 
                       exp_dataset, plot_exp_dataset, exp_dataset_alpha, xaxis_label, yaxis_label, 
                       column_to_read) {
  
  if(plot_exp_dataset == 'True' || plot_exp_dataset == 'TRUE' || plot_exp_dataset == 'true') {
    print('experimental dataset will also be plotted')
    plot_exp_dataset = TRUE
  } else {
    plot_exp_dataset = FALSE
  }
  
  print('summarising time course repeats in tables')
  summarise_data(inputdir, model, outputfile_repeats, column_to_read)
  
  print('generating table of statistics')
  gen_stats_table(outputfile_repeats, outputfile_stats, column_to_read)
  
  files <- list.files( path=inputdir, pattern=model )
  if(length(files) > 1) {
    print('plotting separate time courses')
    plot_sep_sims(dirname(outputfile_repeats), outputdir, model, exp_dataset, plot_exp_dataset,
                  exp_dataset_alpha, xaxis_label, yaxis_label, column_to_read)
  }
  print('plotting combined time courses')
  plot_comb_sims(dirname(outputfile_repeats), outputdir, model, exp_dataset, plot_exp_dataset,
                 exp_dataset_alpha, xaxis_label, yaxis_label, column_to_read)
}


#' Summarise the model simulation repeats in a single file.
#'
#' @param inputdir the input directory containing the time course files
#' @param model the model name
#' @param outputfile the file to store the simulated repeats
#' @param column_to_read the name of the column to process
#' @examples
#' data(insulin_receptor_1)
#' data(insulin_receptor_2)
#' data(insulin_receptor_3)
#' dir.create(file.path("sim_datasets"))
#' dir.create(file.path("sim_datasets_sum"))
#' write.table(insulin_receptor_1, 
#'             file=file.path("sim_datasets","insulin_receptor_1.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_2, 
#'             file=file.path("sim_datasets","insulin_receptor_2.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_3, 
#'             file=file.path("sim_datasets","insulin_receptor_3.csv"), 
#'             row.names=FALSE)
#' summarise_data(inputdir="sim_datasets", 
#'                model="insulin_receptor", 
#'                outputfile=file.path("sim_datasets_sum", 
#'                                     "insulin_receptor_IR_beta_pY1146.csv"), 
#'                column_to_read="IR_beta_pY1146")
#' @export
summarise_data <- function(inputdir, model, outputfile, column_to_read='X1') {
  
  # collect all files in the directory
  files <- list.files( path=inputdir, pattern=model )
  # print(files)
  
  # Read the simulated time course data sets
  timecourses <- data.table::fread(file.path(inputdir, files[1]), select=c('Time'))
  #print(timepoints)
  
  # the repeats for this readout
  collect.repeats <- function(mat) {
    for(i in 1:length(files)) {
      mat[,1+i] <- as.data.frame(data.table::fread(file.path(inputdir,files[i]), select=c(column_to_read)))[,1]
    }
    return(mat)
  }
  repeats <- matrix(data=NA, nrow=nrow(timecourses), ncol=1+length(files))
  repeats[, 1] <- timecourses$Time
  repeats <- as.data.frame(collect.repeats(repeats))
  # print(repeats)
  
  names(repeats) <- c("Time", paste('X', seq(1, length(files), 1), sep=""))
  write.table(repeats, file=outputfile, sep="\t", row.names=FALSE, quote=FALSE)
}


#' Generate a table of statistics for each model readout.
#'
#' @param inputfile the file to store the simulated repeats
#' @param outputfile the file to store the statistics
#' @param column_to_read the name of the column to process
#' @examples
#' data(insulin_receptor_1)
#' data(insulin_receptor_2)
#' data(insulin_receptor_3)
#' dir.create(file.path("sim_datasets"))
#' dir.create(file.path("sim_datasets_sum"))
#' write.table(insulin_receptor_1, 
#'             file=file.path("sim_datasets","insulin_receptor_1.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_2, 
#'             file=file.path("sim_datasets","insulin_receptor_2.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_3, 
#'             file=file.path("sim_datasets","insulin_receptor_3.csv"), 
#'             row.names=FALSE)
#' summarise_data(inputdir="sim_datasets", 
#'                model="insulin_receptor", 
#'                outputfile=file.path("sim_datasets_sum", 
#'                                     "insulin_receptor_IR_beta_pY1146.csv"), 
#'                column_to_read="IR_beta_pY1146")
#' gen_stats_table(inputfile=file.path("sim_datasets_sum", 
#'                                     "insulin_receptor_IR_beta_pY1146.csv"), 
#'                 outputfile="insulin_receptor_IR_beta_pY1146_stats.csv", 
#'                 column_to_read="IR_beta_pY1146")
#' @export
gen_stats_table <- function(inputfile, outputfile, column_to_read="X1") {
  
  # Read the summarised simulated time course data set
  repeats <- data.table::fread(inputfile)
  time.col <- repeats$Time
  # equivalent to repeats[, Time :=  NULL] .
  repeats <- as.matrix(data.table::set(repeats, j=c(1L), value=NULL))
  
  # the statistics
  compute.stats <- function(mat) {
    for(i in 1:nrow(mat)) {
      timepoint.values <- repeats[i,]
      mat[i,2] = mean(timepoint.values, na.rm = TRUE)
      mat[i,3] = sd(timepoint.values, na.rm = TRUE)
      mat[i,4] = var(timepoint.values, na.rm = TRUE)
      mat[i,5] = skewness(timepoint.values, na.rm = TRUE)
      mat[i,6] = kurtosis(timepoint.values, na.rm = TRUE)
      mat[i,7] = qnorm(0.975)*mat[i,2]/sqrt(ncol(mat)) # quantile normal distribution (lot of samples)
      mat[i,8] = mat[i,2] / mat[i,1]
      mat[i,9] = min(timepoint.values, na.rm = TRUE)
      mat[i,10] = quantile(timepoint.values, na.rm = TRUE)[2]  # Q1
      mat[i,11] = median(timepoint.values, na.rm = TRUE)  # Q2 or quantile(timepoint.values)[3]
      mat[i,12] = quantile(timepoint.values, na.rm = TRUE)[4]  # Q3
      mat[i,13] = max(timepoint.values, na.rm = TRUE)
    }
    return(mat) 
  }
  statistics <- matrix(data=NA, nrow=nrow(repeats), ncol=13)
  statistics[,1] <- time.col
  statistics <- as.data.frame(compute.stats(statistics))
  colnames(statistics) <- c ("Time", "Mean", "StdDev", "Variance", "Skewness", "Kurtosis", 
                             "n-dist_CI95", "CoeffVar", "Minimum", "1stQuantile", 
                             "Median", "3rdQuantile", "Maximum")  
  # print(statistics)
  write.table(statistics, outputfile, sep="\t", row.names = FALSE)
}


#' Plot the simulation time courses using a heatmap representation.
#'
#' @param inputdir the input directory containing the time course files
#' @param outputdir the output directory
#' @param model the model name
#' @param exp_dataset a full path file containing the experimental data.
#' @param plot_exp_dataset TRUE if the experimental data should also be plotted
#' @param exp_dataset_alpha the alpha level for the data set
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @param column_to_read the name of the column to process
#' @param yaxis.min the lower limit for the y axis
#' @param yaxis.max the upper limit for the y axis
#' @examples
#' data(insulin_receptor_IR_beta_pY1146)
#' data(insulin_receptor_exp_dataset)
#' dir.create(file.path("sim_datasets_sum"))
#' write.table(insulin_receptor_IR_beta_pY1146, 
#'             file=file.path("sim_datasets_sum","insulin_receptor_IR_beta_pY1146.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_exp_dataset, 
#'             file="insulin_receptor_exp_dataset.csv", 
#'             row.names=FALSE)
#' plot_comb_sims(inputdir="sim_datasets_sum", 
#'                outputdir="sim_plots",
#'                model="insulin_receptor",
#'                exp_dataset="insulin_receptor_exp_dataset.csv",
#'                plot_exp_dataset=TRUE, 
#'                exp_dataset_alpha=1.0, 
#'                xaxis_label="Time [m]", 
#'                yaxis_label="Level [a.u.]", 
#'                column_to_read='IR_beta_pY1146', 
#'                yaxis.min=NULL, 
#'                yaxis.max=NULL)
#' @export
plot_comb_sims <- function(inputdir, outputdir, model, exp_dataset, plot_exp_dataset=FALSE,
                           exp_dataset_alpha=1.0, xaxis_label='', yaxis_label='', column_to_read='X1',
                           yaxis.min=NULL, yaxis.max=NULL) {
  
  theme_set(tc_theme(36)) #28
  
  # create the directory of output
  if (!file.exists(outputdir)){
    dir.create(outputdir)
  }
  
  filein <- file.path(inputdir, paste0(model, "_", column_to_read, ".csv"))
  fileout <- file.path(outputdir, gsub('.csv', '.pdf', basename(filein)))
  # print(filein)
  
  df_exp_dataset <- load_exp_dataset(exp_dataset, plot_exp_dataset)
  
  df <- data.table::fread( filein )
  # print(df)

  # mean
  fileoutM <- gsub('.pdf', '_mean.pdf', fileout)
  gM <- ggplot()
  gM <- plot_combined_tc(df, gM, column_to_read, xaxis_label, yaxis_label, 'mean', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
  ggsave(fileoutM, dpi=300, width=8, height=6)#, bg = "transparent")
  
  # mean_sd
  fileoutMSD <- gsub('.pdf', '_mean_sd.pdf', fileout)
  gMSD <- ggplot()
  gMSD <- plot_combined_tc(df, gMSD, column_to_read, xaxis_label, yaxis_label, 'mean_sd', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
  ggsave(fileoutMSD, dpi=300, width=8, height=6)#, bg = "transparent")
  
  # mean_sd_ci95
  fileoutMSDCI <- gsub('.pdf', '_mean_sd_ci95.pdf', fileout)
  gMSDCI <- ggplot()
  gMSDCI <- plot_combined_tc(df, gMSDCI, column_to_read, xaxis_label, yaxis_label, 'mean_sd_ci95', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
  ggsave(fileoutMSDCI, dpi=300, width=8, height=6)#, bg = "transparent")
  
  if(column_to_read %in% colnames(df_exp_dataset)) {
    # mean
    # we make this plot again because we want the line in front.
    gM <- ggplot()
    gM <- plot_raw_dataset(df_exp_dataset, gM, column_to_read, max(df$Time), alpha=exp_dataset_alpha)
    gM <- plot_combined_tc(df, gM, column_to_read, xaxis_label, yaxis_label, 'mean', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(gsub('.pdf', '_w_exp_data.pdf', fileoutM), dpi=300, width=8, height=6)#, bg = "transparent")
    
    # mean_sd
    gMSD <- ggplot()
    gMSD <- plot_raw_dataset(df_exp_dataset, gMSD, column_to_read, max(df$Time), alpha=exp_dataset_alpha)
    gMSD <- plot_combined_tc(df, gMSD, column_to_read, xaxis_label, yaxis_label, 'mean_sd', alpha=0.6, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(gsub('.pdf', '_w_exp_data.pdf', fileoutMSD), dpi=300, width=8, height=6)#, bg = "transparent")
    
    # mean_sd_ci95
    gMSDCI <- ggplot()
    gMSDCI <- plot_raw_dataset(df_exp_dataset, gMSDCI, column_to_read, max(df$Time), alpha=exp_dataset_alpha)
    gMSDCI <- plot_combined_tc(df, gMSDCI, column_to_read, xaxis_label, yaxis_label, 'mean_sd_ci95', alpha=0.6, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(gsub('.pdf', '_w_exp_data.pdf', fileoutMSDCI), dpi=300, width=8, height=6)#, bg = "transparent")
  }
  
}


#' Plot the simulations time course separately.
#'
#' @param inputdir the input directory containing the time course files
#' @param outputdir the output directory
#' @param model the model name
#' @param exp_dataset a full path file containing the experimental data.
#' @param plot_exp_dataset TRUE if the experimental data should also be plotted
#' @param exp_dataset_alpha the alpha level for the data set
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @param column_to_read the name of the column to process
#' @param yaxis.min the lower limit for the y axis
#' @param yaxis.max the upper limit for the y axis
#' @examples
#' data(insulin_receptor_IR_beta_pY1146)
#' data(insulin_receptor_exp_dataset)
#' dir.create(file.path("sim_datasets_sum"))
#' write.table(insulin_receptor_IR_beta_pY1146, 
#'             file=file.path("sim_datasets_sum","insulin_receptor_IR_beta_pY1146.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_exp_dataset, 
#'             file="insulin_receptor_exp_dataset.csv", 
#'             row.names=FALSE)
#' plot_sep_sims(inputdir="sim_datasets_sum", 
#'               outputdir="sim_plots",
#'               model="insulin_receptor",
#'               exp_dataset="insulin_receptor_exp_dataset.csv",
#'               plot_exp_dataset=TRUE, 
#'               exp_dataset_alpha=1.0, 
#'               xaxis_label="Time [m]", 
#'               yaxis_label="Level [a.u.]", 
#'               column_to_read='IR_beta_pY1146', 
#'               yaxis.min=NULL, 
#'               yaxis.max=NULL)
#' @export
plot_sep_sims <- function(inputdir, outputdir, model, exp_dataset, plot_exp_dataset=FALSE,
                          exp_dataset_alpha=1.0, xaxis_label='', yaxis_label='', column_to_read='X1',
                          yaxis.min=NULL, yaxis.max=NULL) {
  
  theme_set(tc_theme(36)) #28
  
  # create the directory of output
  if (!file.exists(outputdir)){
    dir.create(outputdir)
  }
  # collect all files in the directory
  filein <- file.path(inputdir, paste0(model, "_", column_to_read, ".csv"))
  fileout <- file.path(outputdir, gsub('.csv', '.pdf', basename(filein)))  
  # print(filein)
  
  df_exp_dataset <- load_exp_dataset(exp_dataset, plot_exp_dataset)
  
  df <- data.table::fread( filein )
  # print(df)
  
  g <- plot_repeated_tc(df, ggplot(), column_to_read, xaxis_label, yaxis_label, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
  ggsave(fileout, dpi=300,  width=8, height=6)#, bg = "transparent")
  
  if(column_to_read %in% colnames(df_exp_dataset)) {
    g <- plot_raw_dataset(df_exp_dataset, g, column_to_read, max(df$Time), alpha=exp_dataset_alpha, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    g <- plot_repeated_tc(df, g, column_to_read, xaxis_label, yaxis_label, alpha=0.2, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(gsub('.pdf', '_w_exp_data.pdf', fileout), dpi=300, width=8, height=6)#, bg = "transparent")
  }
  
  g <- plot_heatmap_tc(df, ggplot(), TRUE, column_to_read, xaxis_label, 'repeats')
  ggsave(gsub('.pdf', '_heatmap_scaled.pdf', fileout), dpi=300,  width=8, height=6)#, bg = "transparent")
  
  g <- plot_heatmap_tc(df, ggplot(), FALSE, column_to_read, xaxis_label, 'repeats')
  ggsave(gsub('.pdf', '_heatmap.pdf', fileout), dpi=300,  width=8, height=6)#, bg = "transparent")
    
}



#####################
# UTILITY FUNCTIONS #
#####################


#' Calculate the skewness of a numeric vector
#'
#' @param x the numeric vector
#' @param na.rm TRUE if NA values should be discarded
#' @return the skewness
#' @examples 
#' skewness(x=c(1,2.4,5,NA), na.rm=TRUE)
#' @export
skewness <- function(x, na.rm=FALSE) {
  if (na.rm) x = x[!is.na(x)]
  return(sum((x-mean(x))^3/length(x))/sqrt(var(x))^3)
}


#' Calculate the kurtosis of a numeric vector
#'
#' @param x the numeric vector
#' @param na.rm TRUE if NA values should be discarded
#' @return the kurtosis
#' @examples 
#' kurtosis(x=c(1,2.4,5,NA), na.rm=TRUE)
#' @export
kurtosis <- function(x, na.rm=FALSE) {
  if (na.rm) x = x[!is.na(x)]
  return(sum((x-mean(x))^4/length(x))/sqrt(var(x))^4)
}


#' Check that the experimental data set exists.
#'
#' @param exp_dataset a full path file containing the experimental data.
#' @param plot_exp_dataset TRUE if the data set file should be plotted.
#' @return TRUE if the file exists.
check_exp_dataset <- function(exp_dataset, plot_exp_dataset=FALSE) {
    if (plot_exp_dataset) {
        # check that exp_dataset exists and that the file ends with .csv (it is not a dir!)
        if (file.exists(exp_dataset) && grepl('.csv$', exp_dataset)){
            return(TRUE)
        } else {
            print(paste("Warning: experimental data set file ", exp_dataset, " does not exist or not specified. Skip.", sep=""))
            return(FALSE)
        }
    }
    return(FALSE)
}


#' Load the experimental data set.
#'
#' @param exp_dataset a full path file containing the experimental data.
#' @param plot_exp_dataset TRUE if the experimental data should also be plotted
#' @return the loaded data set.
load_exp_dataset <- function(exp_dataset, plot_exp_dataset=FALSE) {
    df_exp_dataset <- data.frame()
    if(check_exp_dataset(exp_dataset, plot_exp_dataset)) {
        df_exp_dataset <- data.frame(data.table::fread(exp_dataset))
    }
    return (df_exp_dataset)
}

