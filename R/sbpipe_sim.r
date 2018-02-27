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
#' sbpipe_sim(model="insulin_receptor", 
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
#' @export
sbpipe_sim <- function(model, inputdir, outputdir, outputfile_stats, outputfile_repeats, 
                       exp_dataset, plot_exp_dataset, exp_dataset_alpha, xaxis_label, yaxis_label, 
                       column_to_read) {
  
  if(plot_exp_dataset == 'True' || plot_exp_dataset == 'TRUE' || plot_exp_dataset == 'true') {
    print('experimental dataset will also be plotted')
    plot_exp_dataset = TRUE
  } else {
    plot_exp_dataset = FALSE
  }
  
  print('generating a table of statistics')
  gen_stats_table(inputdir, outputdir, model, outputfile_stats, xaxis_label, yaxis_label, column_to_read)
  
  print('summarising the time course repeats in tables')
  summarise_data(inputdir, model, outputfile_repeats, column_to_read)
  
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


#' Generate a table of statistics for each model readout.
#'
#' @param inputdir the input directory containing the time course files
#' @param outputdir the output directory
#' @param model the model name
#' @param outputfile the name of the file to store the statistics
#' @param xaxis_label the label for the x axis (e.g. Time (min))
#' @param yaxis_label the label for the y axis (e.g. Level (a.u.))
#' @param column_to_read the name of the column to process
#' @examples
#' data(insulin_receptor_1)
#' data(insulin_receptor_2)
#' data(insulin_receptor_3)
#' dir.create(file.path("sim_datasets"))
#' write.table(insulin_receptor_1, 
#'             file=file.path("sim_datasets","insulin_receptor_1.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_2, 
#'             file=file.path("sim_datasets","insulin_receptor_2.csv"), 
#'             row.names=FALSE)
#' write.table(insulin_receptor_3, 
#'             file=file.path("sim_datasets","insulin_receptor_3.csv"), 
#'             row.names=FALSE)
#' gen_stats_table(inputdir="sim_datasets", 
#'                 outputdir="sim_plots", 
#'                 model="insulin_receptor",
#'                 outputfile="insulin_receptor_IR_beta_pY1146_stats.csv", 
#'                 xaxis_label="Time [m]", 
#'                 yaxis_label="Level [a. u.]", 
#'                 column_to_read="IR_beta_pY1146")
#' @export
gen_stats_table <- function(inputdir, outputdir, model, outputfile, xaxis_label="", yaxis_label="", column_to_read="X1") {
  
  theme_set(tc_theme(36)) #28
  
  # create the directory of output
  if (!file.exists(outputdir)){ 
    dir.create(outputdir) 
  }
  
  # collect all files in the directory
  files <- list.files( path=inputdir, pattern=model )
  print(files)
  
  # Read the simulated time course data sets
  timecourses <- data.table::fread(file.path(inputdir, files[1]), select=c('Time', column_to_read))
  
  column <- names (timecourses)
  
  column.names <- c ("Time")
  timepoints <- timecourses$Time
  #print(timepoints)
  
  time_length <- length(timepoints)

  # statistical table (to export)
  statistics <- matrix( nrow=time_length, ncol=(((length(column)-1)*13)+1) )
  statistics[,1] <- timepoints
  colidx <- 2
  
  # an empty colum that we need for creating a data.frame of length(timecourses$Time) rows
  na <- c(rep(NA, length(timecourses$Time)))
  
  for(j in 1:length(column)) {
    if(column[j] != "Time") {
      #print(column[j])
      
      # Extract column[j] for each file.
      dataset <- data.frame(na)
      for(i in 1:length(files)) {
        dataset <- data.frame(dataset, data.table::fread(file.path(inputdir,files[i]), select=c(column[j])))
      }
      # remove the first column (na)
      dataset <- subset(dataset, select=-c(na))
      
      #print(dataset)
      # structures
      data <-list("mean"=c(),"sd"=c(),"var"=c(),"skew"=c(),"kurt"=c(),"ci95"=c(),
                  "coeffvar"=c(),"min"=c(),"stquantile"=c(),"median"=c(),"rdquantile"=c(),"max"=c())
      k <- 1
      # for each computed timepoint
      for( l in 1:length ( timecourses$Time ) ) {
        
        timepoint.values <- c ( )
        
        if ( k <= length( timepoints ) && as.character(timepoints[k]) == as.character(timecourses$Time[l]) ) {
          # for each Sample
          for(m in 1:length(files)) {
            timepoint.values <- c(timepoint.values, dataset[l,m])  
          }
          timepoint <- compute_descriptive_statistics(timepoint.values, length(files))
          # put data in lists
          data$mean <- c ( data$mean, timepoint$mean )
          data$sd <- c ( data$sd, timepoint$sd )
          data$var <- c ( data$var, timepoint$var )
          data$skew <- c ( data$skew, timepoint$skew )
          data$kurt <- c ( data$kurt, timepoint$kurt )
          data$ci95 <- c ( data$ci95, timepoint$ci95 )
          data$coeffvar <- c ( data$coeffvar, timepoint$coeffvar )
          data$min <- c ( data$min, timepoint$min )
          data$stquantile <- c ( data$stquantile, timepoint$stquantile )
          data$median <- c ( data$median, timepoint$median )
          data$rdquantile <- c ( data$rdquantile, timepoint$rdquantile )
          data$max <- c ( data$max, timepoint$max )
          
          #print(data)
          k <- k + 1
        }
      }
      column.names <- get_column_names_statistics(column.names, column[j])
      statistics <- get_stats(statistics, data, colidx)
      colidx <- colidx+13
    }
  }
  #print (statistics)
  write.table(statistics, outputfile, sep="\t", col.names = column.names, row.names = FALSE) 
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
  # collect all files in the directory
  files <- list.files( path=inputdir, pattern=column_to_read )
  #print(files)
  
  df_exp_dataset <- load_exp_dataset(exp_dataset, plot_exp_dataset)
  
  for(i in 1:length(files)) {
    df <- data.table::fread( file.path(inputdir, files[i]))
    readout <- gsub(paste(model, '_', sep=''), '', gsub('.csv', '', basename(files[i])))
    template_filename <- file.path(outputdir, gsub('.csv', '.png', basename(files[i])))
    
    # mean
    fileoutM <- gsub('.png', '_mean.png', template_filename)
    gM <- ggplot()
    gM <- plot_combined_tc(df, gM, readout, xaxis_label, yaxis_label, 'mean', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(fileoutM, dpi=300, width=8, height=6)#, bg = "transparent")
    
    # mean_sd
    fileoutMSD <- gsub('.png', '_mean_sd.png', template_filename)
    gMSD <- ggplot()
    gMSD <- plot_combined_tc(df, gMSD, readout, xaxis_label, yaxis_label, 'mean_sd', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(fileoutMSD, dpi=300, width=8, height=6)#, bg = "transparent")
    
    # mean_sd_ci95
    fileoutMSDCI <- gsub('.png', '_mean_sd_ci95.png', template_filename)
    gMSDCI <- ggplot()
    gMSDCI <- plot_combined_tc(df, gMSDCI, readout, xaxis_label, yaxis_label, 'mean_sd_ci95', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(fileoutMSDCI, dpi=300, width=8, height=6)#, bg = "transparent")
    
    if(readout %in% colnames(df_exp_dataset)) {
      # mean
      # we make this plot again because we want the line in front.
      gM <- ggplot()
      gM <- plot_raw_dataset(df_exp_dataset, gM, readout, max(df$Time), alpha=exp_dataset_alpha)
      gM <- plot_combined_tc(df, gM, readout, xaxis_label, yaxis_label, 'mean', yaxis.min=yaxis.min, yaxis.max=yaxis.max)
      ggsave(gsub('.png', '_w_exp_data.png', fileoutM), dpi=300, width=8, height=6)#, bg = "transparent")
      
      # mean_sd
      gMSD <- ggplot()
      gMSD <- plot_raw_dataset(df_exp_dataset, gMSD, readout, max(df$Time), alpha=exp_dataset_alpha)
      gMSD <- plot_combined_tc(df, gMSD, readout, xaxis_label, yaxis_label, 'mean_sd', alpha=0.6, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
      ggsave(gsub('.png', '_w_exp_data.png', fileoutMSD), dpi=300, width=8, height=6)#, bg = "transparent")
      
      # mean_sd_ci95
      gMSDCI <- ggplot()
      gMSDCI <- plot_raw_dataset(df_exp_dataset, gMSDCI, readout, max(df$Time), alpha=exp_dataset_alpha)
      gMSDCI <- plot_combined_tc(df, gMSDCI, readout, xaxis_label, yaxis_label, 'mean_sd_ci95', alpha=0.6, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
      ggsave(gsub('.png', '_w_exp_data.png', fileoutMSDCI), dpi=300, width=8, height=6)#, bg = "transparent")
    }
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
  files <- list.files( path=inputdir, pattern=column_to_read)
  print(files)
  
  df_exp_dataset <- load_exp_dataset(exp_dataset, plot_exp_dataset)
  
  for(i in 1:length(files)) {
    df <- data.table::fread( file.path(inputdir, files[i]) )
    # print(df)
    readout <- gsub(paste(model, '_', sep=''), '', gsub('.csv', '', basename(files[i])))
    fileout <- file.path(outputdir, gsub('.csv', '.png', basename(files[i])))
    
    g <- plot_repeated_tc(df, ggplot(), readout, xaxis_label, yaxis_label, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
    ggsave(fileout, dpi=300,  width=8, height=6)#, bg = "transparent")
    
    if(readout %in% colnames(df_exp_dataset)) {
      g <- plot_raw_dataset(df_exp_dataset, g, readout, max(df$Time), alpha=exp_dataset_alpha, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
      g <- plot_repeated_tc(df, g, readout, xaxis_label, yaxis_label, alpha=0.2, yaxis.min=yaxis.min, yaxis.max=yaxis.max)
      ggsave(gsub('.png', '_w_exp_data.png', fileout), dpi=300, width=8, height=6)#, bg = "transparent")
    }
    
    g <- plot_heatmap_tc(df, ggplot(), TRUE, readout, xaxis_label, 'repeats')
    ggsave(gsub('.png', '_heatmap_scaled.png', fileout), dpi=300,  width=8, height=6)#, bg = "transparent")
    
    g <- plot_heatmap_tc(df, ggplot(), FALSE, readout, xaxis_label, 'repeats')
    ggsave(gsub('.png', '_heatmap.png', fileout), dpi=300,  width=8, height=6)#, bg = "transparent")
    
  }
}


#' Summarise the model simulation repeats in a single file.
#'
#' @param inputdir the input directory containing the time course files
#' @param model the model name
#' @param outputfile the name of the file to store the simulations
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
  #print(files)
  
  # Read the simulated time course data sets
  timecourses <- data.table::fread( file.path(inputdir, files[1]), select=c('Time', column_to_read))
  column <- names (timecourses)
  
  timepoints <- timecourses$Time
  #print(timepoints)
  time_length <- length(timepoints)
  
  for(i in 1:length(column)){
    if(column[i] != "Time") {
      print(column[i])
      summary <- NULL
      cbind(summary, timepoints) -> summary
      for(j in 1:length(files)) {
        sim_file <- data.table::fread( file.path(inputdir, files[j]), select=c(column[i]))
        summary <- cbind(summary, sim_file)
      }
      summary <- data.frame(summary)
      names(summary) <- c("Time", paste('X', seq(1, length(files), 1), sep=""))
      write.table(summary, file=outputfile, sep="\t", row.names=FALSE, quote=FALSE)
    }
  }
}



#####################
# UTILITY FUNCTIONS #
#####################



#' For each time point compute the most relevant descriptive statistics: mean, sd, var, skew, kurt, ci95, coeffvar, 
#' min, 1st quantile, median, 3rd quantile, and max.
#'
#' @param timepoint.values array of values for a certain time point
#' @param nfiles the number of files (samples) 
#' @return the statistics for the array of values for a specific time point
compute_descriptive_statistics <- function(timepoint.values, nfiles) {
	timepoint <- list("mean"=0,"sd"=0,"var"=0,"skew"=0,"kurt"=0,"ci95"=0,
			  "coeffvar"=0,"min"=0,"stquantile"=0,"median"=0,"rdquantile"=0,"max"=0)
    # compute mean, standard deviation, error, error.left, error.right
    timepoint$mean <- mean(timepoint.values, na.rm = TRUE)
    timepoint$sd <- sd(timepoint.values, na.rm = TRUE)
    timepoint$var <- var(timepoint.values, na.rm = TRUE)
    #y <- timepoint.values - timepoint.mean
    timepoint$skew <- mean(timepoint.values^3, na.rm = TRUE)/mean(timepoint.values^2, na.rm = TRUE)^1.5
    timepoint$kurt <- mean(timepoint.values^4, na.rm = TRUE)/mean(timepoint.values^2, na.rm = TRUE)^2 -3
    # 0.95 confidence level 
    #timepoint$ci95 <- qt(0.975, df=nfiles-1)*timepoint$sd/sqrt(nfiles)  # quantile t-distribution (few sample, stddev unknown exactly)
    timepoint$ci95 <- qnorm(0.975)*timepoint$sd/sqrt(nfiles) # quantile normal distribution (lot of samples)
    timepoint$coeffvar <- timepoint$sd / timepoint$mean
    timepoint$min <- min(timepoint.values, na.rm = TRUE)
    timepoint$stquantile <- quantile(timepoint.values, na.rm = TRUE)[2]  # Q1
    timepoint$median <- median(timepoint.values, na.rm = TRUE)  # Q2 or quantile(timepoint.values)[3]
    timepoint$rdquantile <- quantile(timepoint.values, na.rm = TRUE)[4]  # Q3
    timepoint$max <- max(timepoint.values, na.rm = TRUE)

    return (timepoint)
}


#' Return the column names of the statitics to calculate.
#'
#' @param column.names an array of column names
#' @param readout the name of the readout
#' @return the column names including the readout name
get_column_names_statistics <- function(column.names, readout) {    
    column.names <- c (column.names,
                       paste(readout, "_Mean", sep=""),
                       paste(readout, "_StdDev", sep=""),
                       paste(readout, "_Variance", sep=""),
                       paste(readout, "_Skewness", sep=""),
                       paste(readout, "_Kurtosis", sep=""),                       
                       paste(readout, "_t-dist_CI95%", sep=""),
                       paste(readout, "_StdErr", sep=""),
                       paste(readout, "_CoeffVar", sep=""),
                       paste(readout, "_Minimum", sep=""),
                       paste(readout, "_1stQuantile", sep=""),
                       paste(readout, "_Median", sep=""),
                       paste(readout, "_3rdQuantile", sep=""),
                       paste(readout, "_Maximum", sep=""))
    #print(readout)
    return (column.names)
}


#' Add the statistics for a readout to the table of statistics. The first column is Time.
#'
#' @param statistics the table of statistics to fill up
#' @param readout the statistics for this readout.
#' @param colidx the position in the table to put the readout statistics
#' @return The table of statistics including this readout.
get_stats <- function(statistics, readout, colidx=2) {
    #print(readout$mean) 
    statistics[,colidx]   <- readout$mean
    statistics[,colidx+1] <- readout$sd
    statistics[,colidx+2] <- readout$var
    statistics[,colidx+3] <- readout$skew
    statistics[,colidx+4] <- readout$kurt
    statistics[,colidx+5] <- readout$ci95
    statistics[,colidx+6] <- readout$coeffvar
    statistics[,colidx+7] <- readout$min
    statistics[,colidx+8] <- readout$stquantile
    statistics[,colidx+9] <- readout$median
    statistics[,colidx+10] <- readout$rdquantile
    statistics[,colidx+11] <- readout$max
    return (statistics)
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

