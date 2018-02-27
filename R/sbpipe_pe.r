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



######################
# EXPORTED FUNCTIONS #
######################



#####################
# UTILITY FUNCTIONS #
#####################






#' The name of the Objective Value column
objval.col <- "ObjVal"

#' Rename data frame columns. `ObjectiveValue` is renamed as `ObjVal`. Substrings `Values.` and `..InitialValue` are
#' removed.
#'
#' @param df.cols The columns of a data frame.
#' @return the renamed columns
replace_colnames <- function(df.cols) {
  df.cols <- gsub("ObjectiveValue", objval.col, df.cols)
  # global variables
  df.cols <- gsub("Values.", "", df.cols)
  df.cols <- gsub("..InitialValue", "", df.cols)
  # compartments
  df.cols <- gsub("Compartments.", "", df.cols)
  df.cols <- gsub("..InitialVolume", "", df.cols)
  # species
  df.cols <- gsub("X.", "", df.cols)
  df.cols <- gsub("._0", "", df.cols)
  df.cols <- gsub(".InitialParticleNumber", "", df.cols)
}


#' Compute the fratio threshold for the confidence level.
#'
#' @param m number of model parameters
#' @param n number of data points
#' @param p significance level
#' @return the f-ratio threshold
#' @examples 
#' compute_fratio_threshold(5, 100)
#' compute_fratio_threshold(5, 100, p=0.01)
#' @export
compute_fratio_threshold <- function(m, n, p=0.05) {
  if(n-m < 1) {
    warning("`data_point_num` is less than the number of estimated parameters. Skipping thresholds.")
    0
  } else {
    1 + (m/(n-m)) * qf(1.0-p, df1=m, df2=n-m)
  }
}

#' Compute the confidence level based on the minimum objective value.
#'
#' @param min_objval the minimum objective value
#' @param params the number of parameters
#' @param data_points the number of data points
#' @param level the confidence level threshold (e.g. 0.01, 0.05)
#' @return the confidence level based on minimum objective value
#' @export
compute_cl_objval <- function(min_objval, params, data_points, level=0.05) {
  min_objval * compute_fratio_threshold(params, data_points, level)
}


#' Compute the Akaike Information Criterion. Assuming additive Gaussian
#' measurement noise of width 1, the term -2ln(L(theta|y)) ~ SSR ~ Chi^2
#'
#' @param chi2 the Chi^2 for the model
#' @param k the number of model parameters
#' @return the AIC
#' @export
compute_aic <- function(chi2, k) {
  chi2 + 2*k
}


#' Compute the corrected Akaike Information Criterion. Assuming additive Gaussian
#' measurement noise of width 1, the term -2ln(L(theta|y)) ~ SSR ~ Chi^2
#'
#' @param chi2 the Chi^2 for the model
#' @param k the number of model parameters
#' @param n the number of data points
#' @return the AICc
#' @export
compute_aicc <- function(chi2, k, n) {
  compute_aic(chi2, k) + (2*k*(k+1))/(n-k-1)
}


#' Compute the Bayesian Information Criterion. Assuming additive Gaussian
#' measurement noise of width 1, the term -2ln(L(theta|y)) ~ SSR ~ Chi^2
#'
#' @param chi2 the Chi^2 for the model
#' @param k the number of model parameters
#' @param n the number of data points
#' @return the BIC
#' @export
compute_bic <- function(chi2, k, n) {
  chi2 + k*log(n)
}


#' Parameter estimation pre-processing. It renames the data set columns, and applies a log10 transformation if logspace is TRUE. 
#' If all.fits is true, it also computes the confidence levels.
#'
#' @param filename the dataset filename containing the fits sequence
#' @param param.names the list of estimated parameter names
#' @param logspace true if the data set shoud be log10-transformed.
#' @param all.fits true if filename contains all fits, false otherwise
#' @param data_point_num the number of data points used for parameterise the model. Ignored if all.fits is false
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation. Ignored if all.fits is false
# #' @examples 
# #' sbpipe_pe_ds_preproc(filename="all_estim_collection.csv", param.names=c('k1', 'k2', 'k3'), logspace=TRUE, all.fits=TRUE, data_point_num=33, fileout_param_estim_summary="param_estim_summary.csv")
# #' sbpipe_pe_ds_preproc(filename="final_estim_collection.csv", param.names=c('k1', 'k2', 'k3'), logspace=TRUE, all.fits=FALSE)
#' @export
sbpipe_pe_ds_preproc <- function(filename, param.names=c(), logspace=TRUE, all.fits=FALSE, data_point_num=0, fileout_param_estim_summary="param_estim_summary.csv") {
  dt <- data.table::fread(filename)
  colnames(dt) <- replace_colnames(colnames(dt))
  dt.log10 <- dt
  data.table::fwrite(dt, filename)
  
  if(logspace) {
    dt.log10[, param.names] <- log10(dt.log10[, param.names])
    data.table::fwrite(dt.log10, gsub('.csv', '_log10.csv', filename))
  }
  
  if(all.fits) {
    
    data_point_num <- as.numeric(data_point_num)
    if(data_point_num < 0) {
      stop("`data_point_num` must be >= 0.")
    }
    
    param.num = length(param.names)
    if(data_point_num < param.num) {
      warning("To visualise thresholds, `data_point_num` must be greater than the number of estimated parameters.")
    }
    
    # note: with=F is necessary, otherwise data.table interprets objval.col as a column name in dt. 
    objval.min <- min(dt[,objval.col, with=F])  
    
    # compute the confidence levels
    cl99_objval <- compute_cl_objval(objval.min, param.num, data_point_num, 0.01)
    cl95_objval <- compute_cl_objval(objval.min, param.num, data_point_num, 0.05)
    cl66_objval <- compute_cl_objval(objval.min, param.num, data_point_num, 0.33)
 
    # Write global statistics for the parameter estimation, including the confidence levels
    fileoutPLE <- sink(fileout_param_estim_summary)
    cat(paste("MinObjVal", 
              "AIC", 
              "AICc", 
              "BIC", 
              "ParamNum", "DataPointNum", 
              "CL66ObjVal", "CL66FitsNum", 
              "CL95ObjVal", "CL95FitsNum", 
              "CL99ObjVal", "CL99FitsNum\n", sep="\t"))
    cat(paste(objval.min, 
              compute_aic(objval.min, param.num), 
              compute_aicc(objval.min, param.num, data_point_num), 
              compute_bic(objval.min, param.num, data_point_num), 
              param.num, data_point_num, 
              cl66_objval, sum(dt[,objval.col, with=F] <= cl66_objval), 
              cl95_objval, sum(dt[,objval.col, with=F] <= cl95_objval), 
              cl99_objval, sum(dt[,objval.col, with=F] <= cl99_objval), sep="\t"), append=TRUE)
    cat("\n", append=TRUE)
    sink()
  }
}


#' Plot the sampled profile likelihood estimations (PLE). The table is made of two columns: ObjVal | Parameter
#'
#' @param df99 the 99\% confidence level data frame
#' @param cl66_objval the 66\% confidence level objective value
#' @param cl95_objval the 95\% confidence level objective value
#' @param cl99_objval the 99\% confidence level objective value
#' @param plots_dir the directory to save the generated plots
#' @param model the model name
#' @param logspace true if parameters should be plotted in logspace. (default: TRUE)
#' @param scientific_notation true if the axis labels should be plotted in scientific notation (default: TRUE)
#' @export
plot_sampled_ple <- function(df99, cl66_objval, cl95_objval, cl99_objval, plots_dir, model,
                             logspace=TRUE, scientific_notation=TRUE) {
  
  parameter <- colnames(df99)[2]

  print(paste('sampled PLE for', parameter))
  fileout <- file.path(plots_dir, paste(model, "_approx_ple_", parameter, ".png", sep=""))

  theme_set(basic_theme(36))
  g <- scatterplot_ple(df99, ggplot(), parameter, objval.col, cl66_objval, cl95_objval, cl99_objval) +
    theme(legend.key.height = unit(0.5, "in"))
  if(logspace) {
    g <- g + xlab(paste("log10(",parameter,")",sep=""))
  }
  if(scientific_notation) {
    g <- g + scale_x_continuous(labels=scales::scientific) + scale_y_continuous(labels=scales::scientific)
  }
  g <- g + ggtitle("PLE (sampled)")
  ggsave(fileout, dpi=300, width=8, height=6)
  
  # Add density information (removed as it was not showing much more..)
  #g <- g + stat_density2d(color="green")
  #fileout = gsub('.png', '_density.png', fileout)
  #ggsave(fileout, dpi=300, width=8, height=6)
}


#' Return the left value of the parameter confidence interval. The provided dataset has two columns: ObjVal | ParamValue
#'
#' @param smallest.param.value the smallest parameter value within the specified confidence level
#' @param full_dataset the full dataset
#' @param objval_conf_level the objective function confidence level
#' @return the left confidence interval
#' @export
leftCI <- function(smallest.param.value, full_dataset, objval_conf_level) {
  min_ci <- smallest.param.value
  # retrieve the objective function values of the parameters with value smaller than smallest.param.value, within the full dataset.
  # ...[min95, )  (we are retrieving those ...)
  lt_min_objvals <- full_dataset[full_dataset[,2] < min_ci, objval.col]
  if(length(lt_min_objvals) == 0 || min(lt_min_objvals) <= objval_conf_level) {
    min_ci <- "-inf"
  }
  min_ci
}


#' Return the right value of the parameter confidence interval. The provided dataset has two columns: ObjVal | ParamValue
#'
#' @param largest.param.value the largest parameter value within the specified confidence level
#' @param full_dataset the full dataset
#' @param objval_conf_level the objective function confidence level
#' @return the right confidence interval
#' @export
rightCI <- function(largest.param.value, full_dataset, objval_conf_level) {
  max_ci <- largest.param.value
  # retrieve the objective function of the parameters with value greater than largest.param.value, within the full dataset.
  # (, max95]...  (we are retrieving those ...)
  gt_max_objvals <- full_dataset[full_dataset[,2] > max_ci, objval.col]
  if(length(gt_max_objvals) == 0 || min(gt_max_objvals) <= objval_conf_level) {
    max_ci <- "+inf"
  }
  max_ci
}


#' Compute the table for the sampled PLE statistics.
#'
#' @param df the complete data frame
#' @param min_objval the minimum objective value
#' @param cl66_objval the 66\% confidence level objective value
#' @param cl95_objval the 95\% confidence level objective value
#' @param cl99_objval the 99\% confidence level objective value
#' @param logspace true if parameters are plotted in logspace (default: TRUE)
#' @return the list of parameter values with their confidence intervals
#' @export
compute_sampled_ple_stats <- function(df, min_objval, cl66_objval, cl95_objval, cl99_objval, logspace=TRUE) {
  
  # extract the optimum value for the parameter (the parameter set giving the minimum objective value)
  par_value <- min(df[df[, objval.col] <= min_objval, 2])
  
  df66 <- df[df[,objval.col] <= cl66_objval, ]
  df95 <- df[df[,objval.col] <= cl95_objval, ]
  df99 <- df[df[,objval.col] <= cl99_objval, ]
  
  min_ci_66 <- leftCI(min(df66[,2]), df95, cl66_objval)
  max_ci_66 <- rightCI(max(df66[,2]), df95, cl66_objval)
  min_ci_95 <- "-inf"
  max_ci_95 <- "+inf"
  min_ci_99 <- "-inf"
  max_ci_99 <- "+inf"
  if(is.numeric(min_ci_66)) { min_ci_95 <- leftCI(min(df95), df99, cl95_objval) }
  if(is.numeric(max_ci_66)) { max_ci_95 <- rightCI(max(df95), df99, cl95_objval) }
  if(is.numeric(min_ci_95)) { min_ci_99 <- leftCI(min(df99), df, cl99_objval) }
  if(is.numeric(max_ci_95)) { max_ci_99 <- rightCI(max(df99), df, cl99_objval) }
  
  if(logspace) {
    # log10 inverse
    par_value <- 10^par_value
    if(is.numeric(min_ci_99)) { min_ci_99 <- 10^min_ci_99 }
    if(is.numeric(max_ci_99)) { max_ci_99 <- 10^max_ci_99 }
    if(is.numeric(min_ci_95)) { min_ci_95 <- 10^min_ci_95 }
    if(is.numeric(max_ci_95)) { max_ci_95 <- 10^max_ci_95 }
    if(is.numeric(min_ci_66)) { min_ci_66 <- 10^min_ci_66 }
    if(is.numeric(max_ci_66)) { max_ci_66 <- 10^max_ci_66 }
  }
  min_ci_99_par_value_ratio <- "-inf"
  max_ci_99_par_value_ratio <- "+inf"
  min_ci_95_par_value_ratio <- "-inf"
  max_ci_95_par_value_ratio <- "+inf"
  min_ci_66_par_value_ratio <- "-inf"
  max_ci_66_par_value_ratio <- "+inf"
  if(is.numeric(min_ci_99) && min_ci_99 != 0) {
    min_ci_99_par_value_ratio <- round(par_value/min_ci_99, digits=6)
  }
  if(is.numeric(max_ci_99) && par_value != 0) {
    max_ci_99_par_value_ratio <- round(max_ci_99/par_value, digits=6)
  }
  if(is.numeric(min_ci_95) && min_ci_95 != 0) {
    min_ci_95_par_value_ratio <- round(par_value/min_ci_95, digits=6)
  }
  if(is.numeric(max_ci_95) && par_value != 0) {
    max_ci_95_par_value_ratio <- round(max_ci_95/par_value, digits=6)
  }
  if(is.numeric(min_ci_66) && min_ci_66 != 0) {
    min_ci_66_par_value_ratio <- round(par_value/min_ci_66, digits=6)
  }
  if(is.numeric(max_ci_66) && par_value != 0) {
    max_ci_66_par_value_ratio <- round(max_ci_66/par_value, digits=6)
  }
  
  ret_list <- list("par_value"=par_value,
                   "min_ci_66"=min_ci_66, "max_ci_66"=max_ci_66,
                   "min_ci_95"=min_ci_95, "max_ci_95"=max_ci_95,
                   "min_ci_99"=min_ci_99, "max_ci_99"=max_ci_99,
                   "min_ci_66_par_value_ratio"=min_ci_66_par_value_ratio, "max_ci_66_par_value_ratio"=max_ci_66_par_value_ratio,
                   "min_ci_95_par_value_ratio"=min_ci_95_par_value_ratio, "max_ci_95_par_value_ratio"=max_ci_95_par_value_ratio,
                   "min_ci_99_par_value_ratio"=min_ci_99_par_value_ratio, "max_ci_99_par_value_ratio"=max_ci_99_par_value_ratio)
  return(ret_list)
}


#' Run the profile likelihood estimation analysis.
#'
#' @param model_name the model name without extension
#' @param filename the filename containing the fits sequence
#' @param parameter the parameter to compute the PLE analysis
#' @param plots_dir the directory to save the generated plots
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation
#' @param logspace true if parameters should be plotted in logspace. (default: TRUE)
#' @param scientific_notation true if the axis labels should be plotted in scientific notation (default: TRUE)
# #' @examples 
# #' sbpipe_sampled_ple_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter="k1", plots_dir="param_estim_plots", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
#' @export
sbpipe_sampled_ple_analysis <- function(model_name, filename, parameter, 
                                        plots_dir, fileout_param_estim_summary,
                                        logspace=TRUE, scientific_notation=TRUE) {

  # load the fits for this parameter
  df <- as.data.frame(data.table::fread(filename, select=c(objval.col, parameter)))
  
  # load the global statistics for the parameter estimation
  dt.stats <- data.table::fread(fileout_param_estim_summary, select=c("MinObjVal", "CL66ObjVal", "CL95ObjVal", "CL99ObjVal"))
  
  # Plot the sampled profile likelihood estimations (PLE)
  plot_sampled_ple(df[df[ ,objval.col] <= dt.stats$CL99ObjVal, ], 
                   dt.stats$CL66ObjVal, dt.stats$CL95ObjVal, dt.stats$CL99ObjVal, 
                   plots_dir, model_name, logspace, scientific_notation)

  # compute the confidence levels and the value for the best parameter
  ci_obj <- compute_sampled_ple_stats(df, dt.stats$MinObjVal, dt.stats$CL66ObjVal, dt.stats$CL95ObjVal, dt.stats$CL99ObjVal,logspace)
  
  # Save the sampled profile likelihood estimations (PLE) statistics
  fileoutPLE <- sink(file.path(plots_dir, paste0(model_name, "_approx_ple_", parameter,".csv")))
  cat(paste("Parameter", "Value", "LeftCI66", "RightCI66", "LeftCI95", "RightCI95", "LeftCI99", "RightCI99", 
            "Value_LeftCI66_ratio", "RightCI66_Value_ratio", "Value_LeftCI95_ratio", "RightCI95_Value_ratio", "Value_LeftCI99_ratio", "RightCI99_Value_ratio\n", sep="\t"), append=TRUE)

  # write on file
  cat(paste(parameter, ci_obj$par_value, ci_obj$min_ci_66, ci_obj$max_ci_66, ci_obj$min_ci_95, ci_obj$max_ci_95,
            ci_obj$min_ci_99, ci_obj$max_ci_99, ci_obj$min_ci_66_par_value_ratio, ci_obj$max_ci_66_par_value_ratio,
            ci_obj$min_ci_95_par_value_ratio, ci_obj$max_ci_95_par_value_ratio, ci_obj$min_ci_99_par_value_ratio,
            ci_obj$max_ci_99_par_value_ratio, sep="\t"), append=TRUE)
  cat("\n", append=TRUE)
  sink()
  
}


#' Combine the statistics for the parameter estimation details
#'
#' @param plots_dir the directory to save the generated plots
#' @param fileout_param_estim_details the name of the file containing the detailed statistics for the estimated parameters
# #' @examples 
# #' sbpipe_combine_param_ple_stats(plots_dir="param_estim_plots", fileout_param_estim_details="param_estim_details.csv")
#' @export
sbpipe_combine_param_ple_stats <- function(plots_dir, fileout_param_estim_details) {
  
  files <- list.files(plots_dir, pattern="\\.csv$")
  if(length(files) < 0) { return }
  
  for(i in 1:length(files)) {
    if(i==1) {
      dt <- data.table::fread(file.path(plots_dir, files[1])) 
    } else {
      dt <- rbind(dt, data.table::fread(file.path(plots_dir, files[i])))
    }
  }

  data.table::fwrite(dt, fileout_param_estim_details)
}


#' Plot parameter density.
#'
#' @param df the data set containing the parameter estimates to plot.
#' @param parameter the name of the parameter to plot the density
#' @param fileout the output file
#' @param title the plot title (default: "")
#' @param logspace true if the parameters should be plotted in logspace (default: TRUE)
#' @param scientific_notation true if the axis labels should be plotted in scientific notation (default: TRUE)
#' @export
plot_parameter_density <- function(df, parameter, fileout, title="", logspace=TRUE, scientific_notation=TRUE) {
  theme_set(basic_theme(36))
  g <- histogramplot(df[parameter], ggplot()) + ggtitle(title)
  if(logspace) {
    g <- g + xlab(paste("log10(",parameter,")",sep=""))
  }
  if(scientific_notation) {
    g <- g + scale_x_continuous(labels=scales::scientific) + scale_y_continuous(labels=scales::scientific)
  }
  ggsave(fileout, dpi=300, width=8, height=6)
}


#' Parameter density analysis.
#'
#' @param model_name the model name without extension
#' @param filename the filename containing the fits sequence
#' @param parameter the name of the parameter to plot the density
#' @param plots_dir the directory for storing the plots
#' @param thres the threshold used to filter the dataset. Values: "BestFits", "CL66", "CL95", "CL99", "All".
#' @param best_fits_percent the percent of best fits to analyse. Only used if thres="BestFits".
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation. Only used if thres!="BestFits".
#' @param logspace true if the parameters should be plotted in logspace
#' @param scientific_notation true if the axis labels should be plotted in scientific notation
# #' @examples
# #' sbpipe_parameter_density_analysis(model_name="insulin_receptor", filename="final_estim_collection_log10.csv", parameter="k1", plots_dir="param_estim_plots", thres="BestFits", best_fits_percent=75, logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_parameter_density_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter="k1", plots_dir="param_estim_plots", thres="CL66", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_parameter_density_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter="k1", plots_dir="param_estim_plots", thres="CL95", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_parameter_density_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter="k1", plots_dir="param_estim_plots", thres="CL99", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_parameter_density_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter="k1", plots_dir="param_estim_plots", thres="All", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
#' @export
sbpipe_parameter_density_analysis <- function(model_name, filename, parameter,  
                                       plots_dir, thres="BestFits", best_fits_percent=100,
                                       fileout_param_estim_summary="",
                                       logspace=TRUE, scientific_notation=TRUE) {
  
  # load the fits for this parameter
  df <- as.data.frame(data.table::fread(filename, select=c(objval.col, parameter)))
  
  if(thres != "BestFits") {
    
    # load the global statistics for the parameter estimation
    dt.stats <- data.table::fread(fileout_param_estim_summary, select=c("CL66ObjVal", "CL95ObjVal", "CL99ObjVal"))
    
    if(dt.stats$CL99ObjVal != 0) {
      if(thres == "CL66") {
        df <- df[df[ , objval.col] <= dt.stats$CL66ObjVal, ]
        fileout <- file.path(plots_dir, paste(model_name, "_cl66_fits_", parameter, ".png", sep=""))
        title <- expression("fits"<="CL66%")
      } else if(thres == "CL95") {
        df <- df[df[ , objval.col] <= dt.stats$CL95ObjVal, ]
        fileout <- file.path(plots_dir, paste(model_name, "_cl95_fits_", parameter, ".png", sep=""))
        title <- expression("fits"<="CL95%")
      } else if(thres == "CL99") {
        df <- df[df[ , objval.col] <= dt.stats$CL99ObjVal, ]
        fileout <- file.path(plots_dir, paste(model_name, "_cl99_fits_", parameter, ".png", sep=""))
        title <- expression("fits"<="CL99%")
      } else if(thres == "All") {
        # no filtering, but we assume that filename contains all the fits
        fileout <- file.path(plots_dir, paste(model_name, "_all_fits_", parameter, ".png", sep=""))
        title <- expression("all fits")
      } else {
        warning("thres should be one of : BestFits, CL66, CL95, CL99, All.")
        return
      }
    }
  } else { 
    if(best_fits_percent <= 0.0 || best_fits_percent > 100.0) {
      warning("best_fits_percent is not in (0, 100]. Now set to 100")
      best_fits_percent = 100
    }
    # Calculate the number of rows to extract.
    selected_rows <- nrow(df)*best_fits_percent/100
    # sort by descending objective value so that the low objective values
    # (which are the most important) are on top. Then extract the tail from the data frame.
    df <- df[order(-df[,objval.col]),]
    df <- tail(df, selected_rows)
    
    fileout <- file.path(plots_dir, paste(model_name, "_best_fits_", parameter, ".png", sep=""))
    title <- expression("best fits")
  }
  
  print(paste('density analysis for', parameter, '(', thres, ')'))
  plot_parameter_density(df, parameter, fileout, title, logspace, scientific_notation)
}


#' Plot 2D profile likelihood estimations.
#'
#' @param df the data set containing the parameter estimates to plot.
#' @param parameter1 the name of the first parameter
#' @param parameter2 the name of the second parameter
#' @param fileout the output file
#' @param title the plot title (default: "")
#' @param logspace true if the parameters should be plotted in logspace (default: TRUE)
#' @param scientific_notation true if the axis labels should be plotted in scientific notation (default: TRUE)
#' @export
plot_sampled_2d_ple <- function(df, parameter1, parameter2, 
                                fileout, title="", 
                                logspace=TRUE, scientific_notation=TRUE) {
  theme_set(basic_theme(36))
  g <- scatterplot_w_colour(df, ggplot(), parameter1, parameter2, objval.col) +
    ggtitle(title) +
    theme(legend.title=element_blank(), 
          legend.text=element_text(size=30),
          legend.key.width = unit(0.4, "in"), 
          legend.key.height = unit(0.5, "in"))
  if(logspace) {
    g <- g + xlab(paste("log10(",parameter1,")",sep="")) + ylab(paste("log10(",parameter2,")",sep=""))
  }
  if(scientific_notation) {
    g <- g + scale_x_continuous(labels=scales::scientific) + scale_y_continuous(labels=scales::scientific)
  }
  ggsave(fileout, dpi=300, width=8, height=6)
}


#' 2D profile likelihood estimation analysis.
#'
#' @param model_name the model name without extension
#' @param filename the filename containing the fits sequence
#' @param parameter1 the name of the first parameter
#' @param parameter2 the name of the second parameter
#' @param plots_dir the directory for storing the plots
#' @param thres the threshold used to filter the dataset. Values: "BestFits", "CL66", "CL95", "CL99", "All".
#' @param best_fits_percent the percent of best fits to analyse. Only used if thres="BestFits".
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation. Only used if thres!="BestFits".
#' @param logspace true if the parameters should be plotted in logspace
#' @param scientific_notation true if the axis labels should be plotted in scientific notation
# #' @examples
# #' sbpipe_sampled_2d_ple_analysis(model_name="insulin_receptor", filename="final_estim_collection_log10.csv", parameter1="k1", parameter2="k2", plots_dir="param_estim_plots", thres="BestFits", best_fits_percent=75, logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_sampled_2d_ple_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter1="k1", parameter2="k2", plots_dir="param_estim_plots", thres="CL66", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_sampled_2d_ple_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter1="k1", parameter2="k2", plots_dir="param_estim_plots", thres="CL95", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_sampled_2d_ple_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter1="k1", parameter2="k2", plots_dir="param_estim_plots", thres="CL99", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
# #' sbpipe_sampled_2d_ple_analysis(model_name="insulin_receptor", filename="all_estim_collection_log10.csv", parameter1="k1", parameter2="k2", plots_dir="param_estim_plots", thres="All", fileout_param_estim_summary="param_estim_summary.csv", logspace=TRUE, scientific_notation=TRUE)
#' @export
sbpipe_sampled_2d_ple_analysis <- function(model_name, filename, 
                                    parameter1, parameter2, 
                                    plots_dir, thres="BestFits", best_fits_percent=100, 
                                    fileout_param_estim_summary="",  
                                    logspace=TRUE, scientific_notation=TRUE) {
  
  # load the fits for this parameter
  df <- as.data.frame(data.table::fread(filename, select=c(objval.col, parameter1, parameter2)))
  
  if(thres != "BestFits") {
    
    # load the global statistics for the parameter estimation
    dt.stats <- data.table::fread(fileout_param_estim_summary, select=c("CL66ObjVal", "CL95ObjVal", "CL99ObjVal"))
    
    if(dt.stats$CL99ObjVal != 0) {
      if(thres == "CL66") {
        df <- df[df[ , objval.col] <= dt.stats$CL66ObjVal, ]
        fileout <- file.path(plots_dir, paste(model_name, "_ple_2d_cl66_fits_", parameter1, "_", parameter2, ".png", sep=""))
        title <- expression("fits"<="CL66%")
      } else if(thres == "CL95") {
        df <- df[df[ , objval.col] <= dt.stats$CL95ObjVal, ]
        fileout <- file.path(plots_dir, paste(model_name, "_ple_2d_cl95_fits_", parameter1, "_", parameter2, ".png", sep=""))
        title <- expression("fits"<="CL95%")
      } else if(thres == "CL99") {
        df <- df[df[ , objval.col] <= dt.stats$CL99ObjVal, ]
        fileout <- file.path(plots_dir, paste(model_name, "_ple_2d_cl99_fits_", parameter1, "_", parameter2, ".png", sep=""))
        title <- expression("fits"<="CL99%")
      } else if(thres == "All") {
        # no filtering, but we assume that filename contains all the fits
        fileout <- file.path(plots_dir, paste(model_name, "_ple_2d_all_fits_", parameter1, "_", parameter2, ".png", sep=""))
        title <- expression("all fits")
      } else {
        warning("thres should be one of : BestFits, CL66, CL95, CL99, All.")
        return
      }
    }
  } else {
    if(best_fits_percent <= 0.0 || best_fits_percent > 100.0) {
      warning("best_fits_percent is not in (0, 100]. Now set to 100")
      best_fits_percent = 100
    }
    # Calculate the number of rows to extract.
    selected_rows <- nrow(df)*best_fits_percent/100
    # sort by descending objective value so that the low objective values
    # (which are the most important) are on top. Then extract the tail from the data frame.
    df <- df[order(-df[,objval.col]),]
    df <- tail(df, selected_rows)
    
    fileout <- file.path(plots_dir, paste(model_name, "_ple_2d_best_fits_", parameter1, "_", parameter2, ".png", sep=""))
    title <- expression("best fits")
  }
  
  print(paste('2D sampled PLE', parameter1, "-", parameter2, '(', thres, ')'))
  # we order df by decreasing objective value, so that best fits will be shown "on top" of the plot
  plot_sampled_2d_ple(df[order(-df[, objval.col]),], parameter1, parameter2, fileout, title, logspace, scientific_notation)
}


#' Plot the Objective values vs Iterations
#'
#' @param objval.vec the vector containing the objective values
#' @param model_name the model name without extension
#' @param plots_dir the directory to save the generated plots
#' @export
plot_objval_vs_iters <- function(objval.vec, model_name, plots_dir) {
  theme_set(basic_theme(36))
  g <- plot_fits(objval.vec, ggplot())
  ggsave(file.path(plots_dir, paste(model_name, "_objval_vs_iter.png", sep="")), dpi=300, width=8, height=6)
}


#' Analysis of the Objective values vs Iterations.
#'
#' @param model_name the model name without extension
#' @param filename the filename containing the fits sequence
#' @param plots_dir the directory to save the generated plots
# #' @examples 
# #' sbpipe_objval_vs_iters_analysis(model_name="insulin_receptor", filename="all_estim_collection.csv", plots_dir="param_estim_plots")
#' @export
sbpipe_objval_vs_iters_analysis <- function(model_name, filename, plots_dir) {
  # load the fits for this parameter
  dt <- data.table::fread(filename, select=c(objval.col))
  
  print('plotting objective value vs iteration')
  plot_objval_vs_iters(unlist(c(dt)), model_name, plots_dir)
}


#' Get parameter names
#'
#' @param filename the filename containing the best fits
#' @return the parameter names
#' @export
get_param_names <- function(filename) {
  # load the fits for this parameter
  names <- colnames(data.table::fread(filename))
  names <- replace_colnames(names)
  remove <- c("Estimation", "ObjVal")
  return(setdiff(names, remove))
}


#' Main R function for SBpipe pipeline: parameter_estimation(). 
#'
#' @param model_name the name of the model
#' @param finalfits_filenamein the dataset containing the best parameter fits
#' @param allfits_filenamein the dataset containing all the parameter fits
#' @param plots_dir the directory to save the generated plots.
#' @param data_point_num the number of data points used for parameterise the model.
#' @param fileout_param_estim_details the name of the file containing the detailed statistics for the estimated parameters.
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation.
#' @param best_fits_percent the percent of best fits to analyse.
#' @param plot_2d_66cl_corr true if the 2D parameter correlation plots for 66\% confidence intervals should be plotted.
#' @param plot_2d_95cl_corr true if the 2D parameter correlation plots for 95\% confidence intervals should be plotted.
#' @param plot_2d_99cl_corr true if the 2D parameter correlation plots for 99\% confidence intervals should be plotted.
#' @param logspace true if parameters should be plotted in logspace.
#' @param scientific_notation true if axis labels should be plotted in scientific notation.
#' @export
sbpipe_pe <- function(model_name, finalfits_filenamein, allfits_filenamein, plots_dir, 
                      data_point_num, fileout_param_estim_details, fileout_param_estim_summary, 
                      best_fits_percent, plot_2d_66cl_corr, plot_2d_95cl_corr, plot_2d_99cl_corr, 
                      logspace, scientific_notation) {
  
  ### ------------ ###
  
  # Run some controls first
  
  dim_final_fits = dim(read.table(finalfits_filenamein, sep="\t"))[1]
  dim_all_fits = dim(read.table(allfits_filenamein, header=TRUE, sep="\t"))[1]
  
  if(dim_final_fits-1 <= 1) {
    warning('Best fits analysis requires at least two parameter estimations. Skip.')
    finalfits = FALSE
  }
  if(dim_all_fits-1 <= 0) {
    warning('All fits analysis requires at least one parameter set. Cannot continue.')
    stop()
  }
  
  df_all_fits = read.table(allfits_filenamein, header=TRUE, dec=".", sep="\t")
  
  # non-positive entries test
  # If so, logspace will be set to FALSE, otherwise SBpipe will fail due to NaN values.
  # This is set once for all
  nonpos_entries <- sum(df_all_fits <= 0)
  if(nonpos_entries > 0) {
    warning('Non-positive values found for one or more parameters. `logspace` option set to FALSE')
    logspace = FALSE
  }
  
  if(data_point_num < 0.0) {
    warning("`data_point_num` must be >= 0. To visualise thresholds, `data_point_num` must be greater than the number of estimated parameters.")
    stop()
  }
  
  if(best_fits_percent <= 0.0 || best_fits_percent > 100.0) {
    warning("best_fits_percent is not in (0, 100]. Now set to 100")
    best_fits_percent = 100
  }
  
  ### ------------ ###
  
  
  # retrieve the list of parameter names
  param.names <- get_param_names(finalfits_filenamein)
  
  # data preprocessing  
  sbpipe_pe_ds_preproc(filename=allfits_filenamein, param.names=param.names, logspace=logspace, 
                       all.fits=TRUE, data_point_num=data_point_num, fileout_param_estim_summary=fileout_param_estim_summary)
  sbpipe_pe_ds_preproc(filename=finalfits_filenamein, param.names=param.names, logspace=logspace, all.fits=FALSE)

  if(logspace) {
    finalfits_filenamein <- gsub(".csv", "_log10.csv", finalfits_filenamein)
    allfits_filenamein <- gsub(".csv", "_log10.csv", allfits_filenamein)
  }
  
  # objective value vs iterations analysis
  sbpipe_objval_vs_iters_analysis(model_name=model_name, filename=allfits_filenamein, plots_dir=plots_dir)
  
  for(i in seq_along(param.names)) {
    # PLE analysis
    sbpipe_sampled_ple_analysis(model_name=model_name, filename=allfits_filenamein, parameter=param.names[i], 
                                plots_dir=plots_dir, fileout_param_estim_summary=fileout_param_estim_summary, 
                                logspace=logspace, scientific_notation=scientific_notation)
    
    # parameter density analysis
    sbpipe_parameter_density_analysis(model_name=model_name, filename=finalfits_filenamein, parameter=param.names[i], plots_dir=plots_dir, thres="BestFits", best_fits_percent=best_fits_percent, logspace=logspace, scientific_notation=scientific_notation)
    sbpipe_parameter_density_analysis(model_name=model_name, filename=allfits_filenamein, parameter=param.names[i], plots_dir=plots_dir, thres="CL66", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    sbpipe_parameter_density_analysis(model_name=model_name, filename=allfits_filenamein, parameter=param.names[i], plots_dir=plots_dir, thres="CL95", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    sbpipe_parameter_density_analysis(model_name=model_name, filename=allfits_filenamein, parameter=param.names[i], plots_dir=plots_dir, thres="CL99", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    # sbpipe_parameter_density_analysis(model_name=model_name, filename=allfits_filenamein, parameter=param.names[i], plots_dir=plots_dir, thres="All", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
  }
  
  # create summary file containing parameter PLE stats
  sbpipe_combine_param_ple_stats(plots_dir=plots_dir, fileout_param_estim_details=fileout_param_estim_details)
  
  # 2D PLE analysis
  for(i in 1:(length(param.names)-1)) {
    for(j in (i+1):length(param.names)) {
      sbpipe_sampled_2d_ple_analysis(model_name=model_name, filename=finalfits_filenamein, parameter1=param.names[i], parameter2=param.names[j], plots_dir=plots_dir, thres="BestFits", best_fits_percent=best_fits_percent, logspace=logspace, scientific_notation=scientific_notation)
      if(plot_2d_66cl_corr) 
        sbpipe_sampled_2d_ple_analysis(model_name=model_name, filename=allfits_filenamein, parameter1=param.names[i], parameter2=param.names[j], plots_dir=plots_dir, thres="CL66", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
      if(plot_2d_95cl_corr)
        sbpipe_sampled_2d_ple_analysis(model_name=model_name, filename=allfits_filenamein, parameter1=param.names[i], parameter2=param.names[j], plots_dir=plots_dir, thres="CL95", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
      if(plot_2d_99cl_corr)
        sbpipe_sampled_2d_ple_analysis(model_name=model_name, filename=allfits_filenamein, parameter1=param.names[i], parameter2=param.names[j], plots_dir=plots_dir, thres="CL99", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
      # sbpipe_sampled_2d_ple_analysis(model_name=model_name, filename=allfits_filenamein, parameter1=param.names[i], parameter2=param.names[j], plots_dir=plots_dir, thres="All", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    }
  }
 
}

