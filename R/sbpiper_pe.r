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


#' The name of the Objective Value column
objval.col <- "ObjVal"


######################
# EXPORTED FUNCTIONS #
######################



#' Main R function for SBpipe pipeline: parameter_estimation(). 
#'
#' @param model the name of the model
#' @param finalfits_filenamein the dataset containing the best parameter fits
#' @param allfits_filenamein the dataset containing all the parameter fits
#' @param plots_dir the directory to save the generated plots.
#' @param data_point_num the number of data points used for parameterise the model.
#' @param fileout_param_estim_best_fits_details the name of the file for the statistics of the parameters best fits.
#' @param fileout_param_estim_details the name of the file containing the detailed statistics for the estimated parameters.
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation.
#' @param best_fits_percent the percent of best fits to analyse.
#' @param plot_2d_66cl_corr true if the 2D parameter correlation plots for 66\% confidence intervals should be plotted.
#' @param plot_2d_95cl_corr true if the 2D parameter correlation plots for 95\% confidence intervals should be plotted.
#' @param plot_2d_99cl_corr true if the 2D parameter correlation plots for 99\% confidence intervals should be plotted.
#' @param logspace true if parameters should be plotted in logspace.
#' @param scientific_notation true if axis labels should be plotted in scientific notation.
#' @examples 
#' \donttest{
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_best_fits)
#' write.table(insulin_receptor_best_fits, 
#'             file=file.path("pe_datasets", "best_fits.csv"), 
#'             row.names=FALSE)
#' data(insulin_receptor_all_fits)
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' sbpiper_pe(model="ir_beta", 
#'            finalfits_filenamein=file.path("pe_datasets", "best_fits.csv"), 
#'            allfits_filenamein=file.path("pe_datasets", "all_fits.csv"), 
#'            plots_dir="pe_plots", 
#'            data_point_num=33, 
#'            fileout_param_estim_best_fits_details=file.path("pe_datasets", 
#'                                                  "param_estim_best_fits_details.csv"), 
#'            fileout_param_estim_details=file.path("pe_datasets", 
#'                                                  "param_estim_details.csv"), 
#'            fileout_param_estim_summary=file.path("pe_datasets", 
#'                                                  "param_estim_summary.csv"), 
#'            best_fits_percent=50, 
#'            plot_2d_66cl_corr=TRUE, 
#'            plot_2d_95cl_corr=TRUE, 
#'            plot_2d_99cl_corr=TRUE, 
#'            logspace=TRUE, 
#'            scientific_notation=TRUE)
#' }
#' @export
sbpiper_pe <- function(model, 
                       finalfits_filenamein, 
                       allfits_filenamein, 
                       plots_dir, 
                       data_point_num, 
                       fileout_param_estim_best_fits_details="param_estim_best_fits_details.csv",
                       fileout_param_estim_details="param_estim_details.csv", 
                       fileout_param_estim_summary="param_estim_summary.csv", 
                       best_fits_percent=50, 
                       plot_2d_66cl_corr=TRUE, 
                       plot_2d_95cl_corr=TRUE, 
                       plot_2d_99cl_corr=TRUE, 
                       logspace=TRUE, 
                       scientific_notation=TRUE) {

  ### ------------ ###
  
  # Run some controls first
  
  dim_final_fits = dim(data.table::fread(finalfits_filenamein))[1]

  df_all_fits <- data.table::fread(allfits_filenamein)
  dim_all_fits = dim(df_all_fits)[1]
  
  if(dim_final_fits-1 <= 1) {
    warning('Best fits analysis requires at least two parameter estimations. Skip.')
    finalfits = FALSE
  }
  if(dim_all_fits-1 <= 0) {
    warning('All fits analysis requires at least one parameter set. Cannot continue.')
    stop()
  }
  
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
    warning("best_fits_percent is not in (0, 100]. Now set to 50")
    best_fits_percent = 50
  }
  
  ### ------------ ###
  
  
  # retrieve the list of parameter names
  param.names <- get_param_names(finalfits_filenamein)
  
  # data preprocessing  
  pe_ds_preproc(filename=allfits_filenamein, param.names=param.names, logspace=logspace, 
                all.fits=TRUE, data_point_num=data_point_num, fileout_param_estim_summary=fileout_param_estim_summary)
  pe_ds_preproc(filename=finalfits_filenamein, param.names=param.names, logspace=logspace, all.fits=FALSE)
  
  if(logspace) {
    finalfits_filenamein <- gsub(".csv", "_log10.csv", finalfits_filenamein)
    allfits_filenamein <- gsub(".csv", "_log10.csv", allfits_filenamein)
  }
  
  # objective value vs iterations analysis
  objval_vs_iters_analysis(model=model, filename=allfits_filenamein, plots_dir=plots_dir)
  
  # PCA analysis
  parameter_pca_analysis(model=model, filename=finalfits_filenamein, plots_dir=plots_dir, best_fits_percent=best_fits_percent)
  lapply(param.names, function(param) {
    # PLE analysis
    sampled_ple_analysis(model=model, filename=allfits_filenamein, parameter=param, 
                         plots_dir=plots_dir, fileout_param_estim_summary=fileout_param_estim_summary, 
                         logspace=logspace, scientific_notation=scientific_notation)
    
    # parameter density analysis
    parameter_density_analysis(model=model, filename=finalfits_filenamein, parameter=param, plots_dir=plots_dir, thres="BestFits", best_fits_percent=best_fits_percent, logspace=logspace, scientific_notation=scientific_notation)
    parameter_density_analysis(model=model, filename=allfits_filenamein, parameter=param, plots_dir=plots_dir, thres="CL66", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    parameter_density_analysis(model=model, filename=allfits_filenamein, parameter=param, plots_dir=plots_dir, thres="CL95", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    parameter_density_analysis(model=model, filename=allfits_filenamein, parameter=param, plots_dir=plots_dir, thres="CL99", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
    # parameter_density_analysis(model=model, filename=allfits_filenamein, parameter=param, plots_dir=plots_dir, thres="All", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation) 
  })

  # create summary file containing parameter best fits stats
  combine_param_best_fits_stats(plots_dir=plots_dir, fileout_param_estim_best_fits_details=fileout_param_estim_best_fits_details)
  
  # create summary file containing parameter PLE stats
  combine_param_ple_stats(plots_dir=plots_dir, fileout_param_estim_details=fileout_param_estim_details)
  
  # 2D PLE analysis
  # create all the combinations we need. This will be a matrix(length(param.names) x 2), where each column is 
  # a pair.
  if(length(param.names) >= 2) {
    ind <- combn(length(param.names), 2)
    apply(ind, 2, function(x) {
      sampled_2d_ple_analysis(model=model, filename=finalfits_filenamein, parameter1=param.names[x[1]], parameter2=param.names[x[2]], plots_dir=plots_dir, thres="BestFits", best_fits_percent=best_fits_percent, logspace=logspace, scientific_notation=scientific_notation)
      if(plot_2d_66cl_corr) 
        sampled_2d_ple_analysis(model=model, filename=allfits_filenamein, parameter1=param.names[x[1]], parameter2=param.names[x[2]], plots_dir=plots_dir, thres="CL66", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
      if(plot_2d_95cl_corr)
        sampled_2d_ple_analysis(model=model, filename=allfits_filenamein, parameter1=param.names[x[1]], parameter2=param.names[x[2]], plots_dir=plots_dir, thres="CL95", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
      if(plot_2d_99cl_corr)
        sampled_2d_ple_analysis(model=model, filename=allfits_filenamein, parameter1=param.names[x[1]], parameter2=param.names[x[2]], plots_dir=plots_dir, thres="CL99", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)
      # sampled_2d_ple_analysis(model=model, filename=allfits_filenamein, parameter1=param.names[x[1]], parameter2=param.names[x[2]], plots_dir=plots_dir, thres="All", fileout_param_estim_summary=fileout_param_estim_summary, logspace=logspace, scientific_notation=scientific_notation)    
    })
  }
  return()
}


#' Compute the fratio threshold for the confidence level.
#'
#' @param params the number of parameters
#' @param data_points the number of data points
#' @param level the confidence level threshold (e.g. 0.01, 0.05)
#' @return the f-ratio threshold
#' @examples 
#' compute_fratio_threshold(params=5, data_points=100)
#' compute_fratio_threshold(params=5, data_points=100, level=0.01)
#' @export
compute_fratio_threshold <- function(params=1, data_points=2, level=0.05) {
  if(data_points-params < 1) {
    warning("`data_point_num` is less than the number of estimated parameters. Skipping thresholds.")
    0
  } else {
    1 + (params/(data_points-params)) * qf(1.0-level, df1=params, df2=data_points-params)
  }
}

#' Compute the confidence level based on the minimum objective value.
#'
#' @param min_objval the minimum objective value
#' @param params the number of parameters
#' @param data_points the number of data points
#' @param level the confidence level threshold (e.g. 0.01, 0.05)
#' @return the confidence level based on minimum objective value
#' @examples 
#' compute_cl_objval(min_objval=10, params=5, data_points=100)
#' @export
compute_cl_objval <- function(min_objval=0, params=1, data_points=2, level=0.05) {
  min_objval * compute_fratio_threshold(params, data_points, level)
}


#' Compute the Akaike Information Criterion. Assuming additive Gaussian
#' measurement noise of width 1, the term -2ln(L(theta|y)) ~ SSR ~ Chi^2
#'
#' @param chi2 the Chi^2 for the model
#' @param params the number of model parameters
#' @return the AIC
#' @examples 
#' compute_aic(chi2=10, params=5)
#' @export
compute_aic <- function(chi2=0, params=0) {
  chi2 + 2*params
}


#' Compute the corrected Akaike Information Criterion. Assuming additive Gaussian
#' measurement noise of width 1, the term -2ln(L(theta|y)) ~ SSR ~ Chi^2
#'
#' @param chi2 the Chi^2 for the model
#' @param params the number of model parameters
#' @param data_points the number of data points
#' @return the AICc
#' @examples 
#' compute_aicc(chi2=10, params=5, data_points=100)
#' @export
compute_aicc <- function(chi2=0, params=0, data_points=0) {
  compute_aic(chi2, params) + (2*params*(params+1))/(data_points-params-1)
}


#' Compute the Bayesian Information Criterion. Assuming additive Gaussian
#' measurement noise of width 1, the term -2ln(L(theta|y)) ~ SSR ~ Chi^2
#'
#' @param chi2 the Chi^2 for the model
#' @param params the number of model parameters
#' @param data_points the number of data points
#' @return the BIC
#' @examples 
#' compute_bic(chi2=10, params=5, data_points=100)
#' @export
compute_bic <- function(chi2=0, params=1, data_points=1) {
  chi2 + params*log(data_points)
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
#' @examples 
#' dir.create(file.path("pe_datasets"))
#' data(insulin_receptor_all_fits)
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' pe_ds_preproc(filename=file.path("pe_datasets", "all_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=TRUE, 
#'               data_point_num=33, 
#'               fileout_param_estim_summary=file.path("pe_datasets", "param_estim_summary.csv"))
#' data(insulin_receptor_best_fits)
#' write.table(insulin_receptor_best_fits, 
#'             file=file.path("pe_datasets", "best_fits.csv"), 
#'             row.names=FALSE)
#' pe_ds_preproc(filename=file.path("pe_datasets", "best_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=FALSE)
#' @export
pe_ds_preproc <- function(filename, param.names=c(), logspace=TRUE, all.fits=FALSE, data_point_num=0, fileout_param_estim_summary="param_estim_summary.csv") {
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
    df.stats <- data.frame(MinObjVal=objval.min, 
                           AIC=compute_aic(objval.min, param.num), 
                           AICc=compute_aicc(objval.min, param.num, data_point_num), 
                           BIC=compute_bic(objval.min, param.num, data_point_num), 
                           ParamNum=param.num, 
                           DataPointNum=data_point_num, 
                           CL66ObjVal=cl66_objval, 
                           CL66FitsNum=sum(dt[,objval.col, with=F] <= cl66_objval), 
                           CL95ObjVal=cl95_objval, 
                           CL95FitsNum=sum(dt[,objval.col, with=F] <= cl95_objval), 
                           CL99ObjVal=cl99_objval, 
                           CL99FitsNum=sum(dt[,objval.col, with=F] <= cl99_objval))
    write.table(df.stats,
                file=fileout_param_estim_summary,
                row.names=FALSE,
                sep="\t")
  }
}


#' Plot the sampled profile likelihood estimations (PLE). The table is made of two columns: ObjVal | Parameter
#'
#' @param df99 the data set including the fits within 99\% confidence level
#' @param cl66_objval the objective value at 66\% confidence level
#' @param cl95_objval the objective value at 95\% confidence level
#' @param cl99_objval the objective value at 99\% confidence level
#' @param plots_dir the directory to save the generated plots
#' @param model the model name
#' @param logspace true if parameters should be plotted in logspace
#' @param scientific_notation true if the axis labels should be plotted in scientific notation
#' @examples 
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "all_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=TRUE, 
#'               data_point_num=33, 
#'               fileout_param_estim_summary=file.path("pe_datasets", "param_estim_summary.csv"))
#' # load the fits for this parameter
#' df <- as.data.frame(data.table::fread(file.path("pe_datasets", "all_fits_log10.csv"), 
#'                                       select=c("ObjVal", "k2")))
#' # load the global statistics for the parameter estimation
#' dt.stats <- data.table::fread(file.path("pe_datasets", "param_estim_summary.csv"), 
#'                               select=c("MinObjVal", "CL66ObjVal", "CL95ObjVal", "CL99ObjVal"))
#' df99 <- df[df[ ,"ObjVal"] <= dt.stats$CL99ObjVal, ]
#' # compute the stats for parameter k2. 
#' plot_sampled_ple(df99=df99, 
#'                  cl66_objval=dt.stats$CL66ObjVal, 
#'                  cl95_objval=dt.stats$CL95ObjVal, 
#'                  cl99_objval=dt.stats$CL99ObjVal, 
#'                  plots_dir="pe_plots",
#'                  model="ir_beta",
#'                  logspace=TRUE)
#' @export
plot_sampled_ple <- function(df99, cl66_objval, cl95_objval, cl99_objval, plots_dir, model,
                             logspace=TRUE, scientific_notation=TRUE) {
  
  parameter <- colnames(df99)[2]
  
  print(paste('sampled PLE for', parameter))
  fileout <- file.path(plots_dir, paste(model, "_approx_ple_", parameter, ".pdf", sep=""))
  
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
  #fileout = gsub('.pdf', '_density.pdf', fileout)
  #ggsave(fileout, dpi=300, width=8, height=6)
}


#' Return the left value of the parameter confidence interval. The provided dataset has two columns: ObjVal | ParamValue
#'
#' @param smallest.param.value the smallest parameter value within the specified confidence level
#' @param full_dataset the full dataset
#' @param cl_objval the objective value at the desired confidence level
#' @return the left confidence interval
#' @examples 
#' data(insulin_receptor_all_fits)
#' colnames(insulin_receptor_all_fits)[1] <- "ObjVal"
#' min_objval <- min(insulin_receptor_all_fits[,1])
#' # compute the stats for parameter k2. 
#' insulin_receptor_all_fits <- subset(insulin_receptor_all_fits, select=c(1,3))
#' leftCI(smallest.param.value=0.466971, 
#'         full_dataset=insulin_receptor_all_fits, 
#'         cl_objval=min_objval+0.01)
#' leftCI(smallest.param.value=0.467000, 
#'         full_dataset=insulin_receptor_all_fits, 
#'         cl_objval=min_objval+0.01)
#' @export
leftCI <- function(smallest.param.value, full_dataset, cl_objval) {
  min_ci <- smallest.param.value
  # retrieve the objective function values of the parameters with value smaller than smallest.param.value, within the full dataset.
  # ...[min95, )  (we are retrieving those ...)
  lt_min_objvals <- full_dataset[full_dataset[,2] < min_ci, objval.col]
  if(length(lt_min_objvals) == 0 || min(lt_min_objvals) <= cl_objval) {
    min_ci <- "-inf"
  }
  min_ci
}


#' Return the right value of the parameter confidence interval. The provided dataset has two columns: ObjVal | ParamValue
#'
#' @param largest.param.value the largest parameter value within the specified confidence level
#' @param full_dataset the full dataset
#' @param cl_objval the objective value at the desired confidence level
#' @return the right confidence interval
#' @examples 
#' data(insulin_receptor_all_fits)
#' colnames(insulin_receptor_all_fits)[1] <- "ObjVal"
#' min_objval <- min(insulin_receptor_all_fits[,1])
#' # compute the stats for parameter k2. 
#' insulin_receptor_all_fits <- subset(insulin_receptor_all_fits, select=c(1,3))
#' rightCI(largest.param.value=0.477115, 
#'         full_dataset=insulin_receptor_all_fits, 
#'         cl_objval=min_objval+0.01)
#' rightCI(largest.param.value=0.467000, 
#'         full_dataset=insulin_receptor_all_fits, 
#'         cl_objval=min_objval+0.01)
#' @export
rightCI <- function(largest.param.value, full_dataset, cl_objval) {
  max_ci <- largest.param.value
  # retrieve the objective function of the parameters with value greater than largest.param.value, within the full dataset.
  # (, max95]...  (we are retrieving those ...)
  gt_max_objvals <- full_dataset[full_dataset[,2] > max_ci, objval.col]
  if(length(gt_max_objvals) == 0 || min(gt_max_objvals) <= cl_objval) {
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
#' @examples 
#' data(insulin_receptor_all_fits)
#' colnames(insulin_receptor_all_fits)[1] <- "ObjVal"
#' min_objval <- min(insulin_receptor_all_fits[,1])
#' # compute the stats for parameter k2. 
#' insulin_receptor_all_fits <- subset(insulin_receptor_all_fits, select=c(1,3))
#' compute_sampled_ple_stats(df=insulin_receptor_all_fits, 
#'                           min_objval=min_objval, 
#'                           cl66_objval=min_objval+0.01, 
#'                           cl95_objval=min_objval+0.02, 
#'                           cl99_objval=min_objval+0.03, 
#'                           logspace=FALSE)
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
#' @param model the model name
#' @param filename the filename containing the fits sequence
#' @param parameter the parameter to compute the PLE analysis
#' @param plots_dir the directory to save the generated plots
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation
#' @param logspace true if parameters should be plotted in logspace. (default: TRUE)
#' @param scientific_notation true if the axis labels should be plotted in scientific notation (default: TRUE)
#' @examples 
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "all_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=TRUE, 
#'               data_point_num=33, 
#'               fileout_param_estim_summary=file.path("pe_datasets", "param_estim_summary.csv"))
#' sampled_ple_analysis(model="ir_beta", 
#'                      filename=file.path("pe_datasets", "all_fits_log10.csv"), 
#'                      parameter="k1", 
#'                      plots_dir="pe_plots", 
#'                      fileout_param_estim_summary=file.path("pe_datasets", 
#'                                                            "param_estim_summary.csv"),
#'                      logspace=TRUE)
#' @export
sampled_ple_analysis <- function(model, 
                                 filename, 
                                 parameter, 
                                 plots_dir, 
                                 fileout_param_estim_summary,
                                 logspace=TRUE, 
                                 scientific_notation=TRUE) {
  
  # load the fits for this parameter
  df <- as.data.frame(data.table::fread(filename, select=c(objval.col, parameter)))
  
  # load the global statistics for the parameter estimation
  dt.stats <- data.table::fread(fileout_param_estim_summary, select=c("MinObjVal", "CL66ObjVal", "CL95ObjVal", "CL99ObjVal"))
  
  # Plot the sampled profile likelihood estimations (PLE)
  plot_sampled_ple(df[df[ ,objval.col] <= dt.stats$CL99ObjVal, ], 
                   dt.stats$CL66ObjVal, dt.stats$CL95ObjVal, dt.stats$CL99ObjVal, 
                   plots_dir, model, logspace, scientific_notation)
  
  # compute the confidence levels and the value for the best parameter
  ci_obj <- compute_sampled_ple_stats(df, dt.stats$MinObjVal, dt.stats$CL66ObjVal, dt.stats$CL95ObjVal, dt.stats$CL99ObjVal,logspace)
  
  # Save the sampled profile likelihood estimations (PLE) statistics
  df.ple.stats <- data.frame(Parameter=parameter, 
                             Value=ci_obj$par_value, 
                             LeftCI66=ci_obj$min_ci_66, 
                             RightCI66=ci_obj$max_ci_66, 
                             LeftCI95=ci_obj$min_ci_95, 
                             RightCI95=ci_obj$max_ci_95, 
                             LeftCI99=ci_obj$min_ci_99, 
                             RightCI99=ci_obj$max_ci_99, 
                             Value_LeftCI66_ratio=ci_obj$min_ci_66_par_value_ratio, 
                             RightCI66_Value_ratio=ci_obj$max_ci_66_par_value_ratio, 
                             Value_LeftCI95_ratio=ci_obj$min_ci_95_par_value_ratio, 
                             RightCI95_Value_ratio=ci_obj$max_ci_95_par_value_ratio, 
                             Value_LeftCI99_ratio=ci_obj$min_ci_99_par_value_ratio, 
                             RightCI99_Value_ratio=ci_obj$max_ci_99_par_value_ratio)
  write.table(df.ple.stats,
              file=file.path(plots_dir, paste0(model, "_approx_ple_", parameter,".csv")),
              row.names=FALSE,
              sep="\t")
}


#' Plot parameter density.
#'
#' @param df the data set containing the parameter estimates to plot.
#' @param parameter the name of the parameter to plot the density
#' @param fileout the output file
#' @param title the plot title (default: "")
#' @param logspace true if the parameters should be plotted in logspace (default: TRUE)
#' @param scientific_notation true if the axis labels should be plotted in scientific notation (default: TRUE)
#' @examples 
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' colnames(insulin_receptor_all_fits)[1] <- "ObjVal"
#' insulin_receptor_all_fits[,2:4] <- log10(insulin_receptor_all_fits[,2:4])
#' fileout <- file.path("pe_plots", "dens_k1.pdf")
#' plot_parameter_density(df=insulin_receptor_all_fits, 
#'                        parameter="k1", 
#'                        fileout=fileout) 
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
#' @param model the model name
#' @param filename the filename containing the fits sequence
#' @param parameter the name of the parameter to plot the density
#' @param plots_dir the directory for storing the plots
#' @param thres the threshold used to filter the dataset. Values: "BestFits", "CL66", "CL95", "CL99", "All".
#' @param best_fits_percent the percent of best fits to analyse. Only used if thres="BestFits".
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation. Only used if thres!="BestFits".
#' @param logspace true if the parameters should be plotted in logspace
#' @param scientific_notation true if the axis labels should be plotted in scientific notation
#' @examples 
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "all_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=TRUE, 
#'               data_point_num=33, 
#'               fileout_param_estim_summary=file.path("pe_datasets", "param_estim_summary.csv"))
#' parameter_density_analysis(model="ir_beta", 
#'                            filename=file.path("pe_datasets", "all_fits_log10.csv"), 
#'                            parameter="k1", 
#'                            plots_dir="pe_plots", 
#'                            thres="CL95",
#'                            fileout_param_estim_summary=file.path("pe_datasets", 
#'                                                                  "param_estim_summary.csv"),
#'                            logspace=TRUE)
#'                            
#' data(insulin_receptor_best_fits)
#' write.table(insulin_receptor_best_fits, 
#'             file=file.path("pe_datasets", "best_fits.csv"), 
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "best_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=FALSE)
#' parameter_density_analysis(model="ir_beta", 
#'                            filename=file.path("pe_datasets", "best_fits_log10.csv"), 
#'                            parameter="k1", 
#'                            plots_dir="pe_plots", 
#'                            thres="BestFits",
#'                            best_fits_percent=50,
#'                            logspace=TRUE)
#' @export
parameter_density_analysis <- function(model, 
                                       filename, 
                                       parameter,  
                                       plots_dir, 
                                       thres="BestFits", 
                                       best_fits_percent=50,
                                       fileout_param_estim_summary="",
                                       logspace=TRUE, 
                                       scientific_notation=TRUE) {
  
  # load the fits for this parameter
  df <- as.data.frame(data.table::fread(filename, select=c(objval.col, parameter)))
  
  if(thres != "BestFits") {
    
    # load the global statistics for the parameter estimation
    dt.stats <- data.table::fread(fileout_param_estim_summary, select=c("CL66ObjVal", "CL95ObjVal", "CL99ObjVal"))
    
    if(dt.stats$CL99ObjVal != 0) {
      if(thres == "CL66") {
        df <- df[df[ , objval.col] <= dt.stats$CL66ObjVal, ]
        fileout <- file.path(plots_dir, paste(model, "_cl66_fits_", parameter, ".pdf", sep=""))
        title <- expression("fits"<="CL66%")
      } else if(thres == "CL95") {
        df <- df[df[ , objval.col] <= dt.stats$CL95ObjVal, ]
        fileout <- file.path(plots_dir, paste(model, "_cl95_fits_", parameter, ".pdf", sep=""))
        title <- expression("fits"<="CL95%")
      } else if(thres == "CL99") {
        df <- df[df[ , objval.col] <= dt.stats$CL99ObjVal, ]
        fileout <- file.path(plots_dir, paste(model, "_cl99_fits_", parameter, ".pdf", sep=""))
        title <- expression("fits"<="CL99%")
      } else if(thres == "All") {
        # no filtering, but we assume that filename contains all the fits
        fileout <- file.path(plots_dir, paste(model, "_all_fits_", parameter, ".pdf", sep=""))
        title <- expression("all fits")
      } else {
        warning("thres should be one of : BestFits, CL66, CL95, CL99, All.")
        return
      }
    }
  } else { 
    if(best_fits_percent <= 0.0 || best_fits_percent > 100.0) {
      warning("best_fits_percent is not in (0, 100]. Now set to 50")
      best_fits_percent = 50
    }
    # Calculate the number of rows to extract.
    selected_rows <- (nrow(df)*best_fits_percent/100)
    # sort by descending objective value so that the low objective values
    # (which are the most important) are on top. Then extract the tail from the data frame.
    df <- df[order(-df[,objval.col]),]
    df <- tail(df, selected_rows)
    
    fileout <- file.path(plots_dir, paste(model, "_best_fits_", parameter, ".pdf", sep=""))
    title <- expression("best fits")
    
    # Save basic statistics for the parameter based on the best fits data set
    if(logspace) {
      param.values <- 10^df[,2]
    } else {
      param.values <- df[,2]
    }
    df.stats <- data.frame(Parameter=parameter,
                           mean=mean(param.values),
                           sd=sd(param.values),
                           median=median(param.values),
                           mad=mad(param.values))
    write.table(df.stats, 
                file=file.path(plots_dir, paste0(model, "_best_fits_", parameter,".csv")),
                row.names=FALSE,
                sep="\t")
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
#' @examples 
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' colnames(insulin_receptor_all_fits)[1] <- "ObjVal"
#' insulin_receptor_all_fits[,2:4] <- log10(insulin_receptor_all_fits[,2:4])
#' fileout <- file.path("pe_plots", "2d_ple_k1_k2.pdf")
#' plot_sampled_2d_ple(df=insulin_receptor_all_fits, 
#'                     parameter1="k1", 
#'                     parameter2="k2", 
#'                     fileout=fileout) 
#' @export
plot_sampled_2d_ple <- function(df, 
                                parameter1, 
                                parameter2, 
                                fileout, 
                                title="", 
                                logspace=TRUE, 
                                scientific_notation=TRUE) {
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
#' @param model the model name
#' @param filename the filename containing the fits sequence
#' @param parameter1 the name of the first parameter
#' @param parameter2 the name of the second parameter
#' @param plots_dir the directory for storing the plots
#' @param thres the threshold used to filter the dataset. Values: "BestFits", "CL66", "CL95", "CL99", "All".
#' @param best_fits_percent the percent of best fits to analyse. Only used if thres="BestFits".
#' @param fileout_param_estim_summary the name of the file containing the summary for the parameter estimation. Only used if thres!="BestFits".
#' @param logspace true if the parameters should be plotted in logspace
#' @param scientific_notation true if the axis labels should be plotted in scientific notation
#' @examples
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "all_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=TRUE, 
#'               data_point_num=33, 
#'               fileout_param_estim_summary=file.path("pe_datasets", "param_estim_summary.csv"))
#' sampled_2d_ple_analysis(model="ir_beta", 
#'                         filename=file.path("pe_datasets", "all_fits_log10.csv"), 
#'                         parameter1="k1",
#'                         parameter2="k2", 
#'                         plots_dir="pe_plots", 
#'                         thres="CL95",
#'                         fileout_param_estim_summary=file.path("pe_datasets", 
#'                                                               "param_estim_summary.csv"),
#'                         logspace=TRUE)
#'                            
#' data(insulin_receptor_best_fits)
#' write.table(insulin_receptor_best_fits, 
#'             file=file.path("pe_datasets", "best_fits.csv"), 
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "best_fits.csv"), 
#'               param.names=c('k1', 'k2', 'k3'), 
#'               logspace=TRUE, 
#'               all.fits=FALSE)
#' sampled_2d_ple_analysis(model="ir_beta", 
#'                         filename=file.path("pe_datasets", "best_fits_log10.csv"), 
#'                         parameter1="k1",
#'                         parameter2="k2",
#'                         plots_dir="pe_plots", 
#'                         thres="BestFits",
#'                         best_fits_percent=50,
#'                         logspace=TRUE)
#' @export
sampled_2d_ple_analysis <- function(model, filename, 
                                    parameter1, parameter2, 
                                    plots_dir, thres="BestFits", best_fits_percent=50, 
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
        fileout <- file.path(plots_dir, paste(model, "_ple_2d_cl66_fits_", parameter1, "_", parameter2, ".pdf", sep=""))
        title <- expression("fits"<="CL66%")
      } else if(thres == "CL95") {
        df <- df[df[ , objval.col] <= dt.stats$CL95ObjVal, ]
        fileout <- file.path(plots_dir, paste(model, "_ple_2d_cl95_fits_", parameter1, "_", parameter2, ".pdf", sep=""))
        title <- expression("fits"<="CL95%")
      } else if(thres == "CL99") {
        df <- df[df[ , objval.col] <= dt.stats$CL99ObjVal, ]
        fileout <- file.path(plots_dir, paste(model, "_ple_2d_cl99_fits_", parameter1, "_", parameter2, ".pdf", sep=""))
        title <- expression("fits"<="CL99%")
      } else if(thres == "All") {
        # no filtering, but we assume that filename contains all the fits
        fileout <- file.path(plots_dir, paste(model, "_ple_2d_all_fits_", parameter1, "_", parameter2, ".pdf", sep=""))
        title <- expression("all fits")
      } else {
        warning("thres should be one of : BestFits, CL66, CL95, CL99, All.")
        return
      }
    }
  } else {
    if(best_fits_percent <= 0.0 || best_fits_percent > 100.0) {
      warning("best_fits_percent is not in (0, 100]. Now set to 50")
      best_fits_percent = 50
    }
    # Calculate the number of rows to extract.
    selected_rows <- nrow(df)*best_fits_percent/100
    # sort by descending objective value so that the low objective values
    # (which are the most important) are on top. Then extract the tail from the data frame.
    df <- df[order(-df[,objval.col]),]
    df <- tail(df, selected_rows)
    
    fileout <- file.path(plots_dir, paste(model, "_ple_2d_best_fits_", parameter1, "_", parameter2, ".pdf", sep=""))
    title <- expression("best fits")
  }
  
  print(paste('2D sampled PLE', parameter1, "-", parameter2, '(', thres, ')'))
  # we order df by decreasing objective value, so that best fits will be shown "on top" of the plot
  plot_sampled_2d_ple(df[order(-df[, objval.col]),], parameter1, parameter2, fileout, title, logspace, scientific_notation)
}


#' Plot the Objective values vs Iterations
#'
#' @param objval.vec the vector containing the objective values
#' @param model the model name
#' @param plots_dir the directory to save the generated plots
#' @examples 
#' dir.create(file.path("pe_plots"))
#' v <- 10*(rnorm(10000))^4 + 10
#' plot_objval_vs_iters(objval.vec=v, model="model", plots_dir="pe_plots")
#' @export
plot_objval_vs_iters <- function(objval.vec, model, plots_dir) {
  theme_set(basic_theme(36))
  g <- plot_fits(objval.vec, ggplot())
  ggsave(file.path(plots_dir, paste(model, "_objval_vs_iter.pdf", sep="")), dpi=300, width=8, height=6)
}


#' Analysis of the Objective values vs Iterations.
#'
#' @param model the model name
#' @param filename the filename containing the fits sequence
#' @param plots_dir the directory to save the generated plots
#' @examples 
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_all_fits)
#' colnames(insulin_receptor_all_fits)[1] <- "ObjVal"
#' write.table(insulin_receptor_all_fits, 
#'             file=file.path("pe_datasets", "all_fits.csv"), 
#'             row.names=FALSE)
#' objval_vs_iters_analysis(model="model", 
#'                          filename=file.path("pe_datasets", "all_fits.csv"), 
#'                          plots_dir="pe_plots")
#' @export
objval_vs_iters_analysis <- function(model, filename, plots_dir) {
  dt <- data.table::fread(filename, select=c(objval.col))
  print('plotting objective value vs iteration')
  plot_objval_vs_iters(unlist(c(dt)), model, plots_dir)
}


#' Combine the parameter PLE statistics. 
#'
#' @param plots_dir the directory to save the generated plots
#' @param fileout_param_estim_details the name of the file containing the detailed statistics for the estimated parameters
#' @export
combine_param_ple_stats <- function(plots_dir, fileout_param_estim_details) {
  
  files <- list.files(plots_dir, pattern="_approx_ple_.*csv$")
  if(length(files) < 0) { return }
  
  for(i in 1:length(files)) {
    if(i==1) {
      df <- read.table(file.path(plots_dir, files[1]), header=TRUE, sep="\t")
    } else {
      df <- rbind(df, read.table(file.path(plots_dir, files[i]), header=TRUE, sep="\t"))
    }
  }
  write.table(df, file=fileout_param_estim_details, sep="\t", row.names=FALSE)
}


#' Combine the parameter best fits statistics. 
#'
#' @param plots_dir the directory to save the generated plots
#' @param fileout_param_estim_best_fits_details the name of the file containing the statistics for the parameter best fits.
#' @export
combine_param_best_fits_stats <- function(plots_dir, fileout_param_estim_best_fits_details) {
  
  files <- list.files(plots_dir, pattern="_best_fits_.*csv$")
  if(length(files) < 0) { return }
  
  for(i in 1:length(files)) {
    if(i==1) {
      df <- read.table(file.path(plots_dir, files[1]), header=TRUE, sep="\t")
    } else {
      df <- rbind(df, read.table(file.path(plots_dir, files[i]), header=TRUE, sep="\t"))
    }
  }
  
  write.table(df, file=fileout_param_estim_best_fits_details, sep="\t", row.names=FALSE)
}


#' PCA for the parameters. These plots rely on factoextra fviz functions.
#'
#' @param model the model name
#' @param filename the filename containing the fits sequence
#' @param plots_dir the directory to save the generated plots
#' @param best_fits_percent the percent of best fits to analyse.
#' @param label.ind parameter `label` passed to factoextra::fviz_pca_ind(). Labels shown if <= 75 and select.ind is NULL.
#' @param select.ind parameter `select.ind` passed to factoextra::fviz_pca_ind().
#' @param repel.ind parameter `repel` passed to factoextra::fviz_pca_ind()
#' @param label.var parameter `label` passed to factoextra::fviz_pca_var().
#' @param select.var parameter `select.var` passed to factoextra::fviz_pca_var().
#' @param repel.var parameter `repel` passed to factoextra::fviz_pca_var() 
#' dir.create(file.path("pe_datasets"))
#' dir.create(file.path("pe_plots"))
#' data(insulin_receptor_best_fits)
#' write.table(insulin_receptor_best_fits,
#'             file=file.path("pe_datasets", "best_fits.csv"),
#'             row.names=FALSE)
#' # generate the global statistics for the parameter estimation
#' pe_ds_preproc(filename=file.path("pe_datasets", "best_fits.csv"),
#'               param.names=c('k1', 'k2', 'k3'),
#'               logspace=TRUE,
#'               all.fits=FALSE)
#' parameter_pca_analysis(model="ir_beta",
#'                        filename=file.path("pe_datasets", "best_fits_log10.csv"),
#'                        plots_dir="pe_plots",
#'                        best_fits_percent=50)
#' @export
parameter_pca_analysis <- function(model, 
                                   filename, 
                                   plots_dir, 
                                   best_fits_percent=50,
                                   label.ind = "all",
                                   select.ind = NULL,
                                   repel.ind = TRUE,
                                   label.var = "all",
                                   select.var = NULL,
                                   repel.var = TRUE) {

  # extract the best parameter values
  df <- as.data.frame(data.table::fread(filename))

  if(ncol(df) <= 3) {
    warning("PCA analysis requires more than 1 parameter")
    return()
  }
  
  # df filtering
  if(best_fits_percent <= 0.0 || best_fits_percent > 100.0) {
    warning("best_fits_percent is not in (0, 100]. Now set to 50")
    best_fits_percent = 50
  }
  # Calculate the number of rows to extract.
  selected_rows <- (nrow(df)*best_fits_percent/100)
  # sort by descending objective value so that the low objective values
  # (which are the most important) are on top. Then extract the tail from the data frame.
  df <- df[order(-df[,objval.col]),]
  df <- tail(df, selected_rows)

  # remove the first two columns as these are not used for the PCA
  df <- df[-c(1,2)]
    
  print('PCA analysis')

  # Compute PCA
  #pca <- prcomp(df, center=TRUE, scale.=TRUE)
  #write.csv(pca$loadings, file=paste0(gsub('.csv', '', filename),"_PCA_loadings.csv"), quote=FALSE)
  
  pca <- FactoMineR::PCA(df, scale.unit=TRUE, graph=FALSE)
  
  # Write the PCA individuals (repeats)
  write.csv(pca$ind$coord, file=paste0(gsub('.csv', '', filename),"_PCA_individuals_coord.csv"), quote=FALSE)
  # Write the PCA variables (vars)
  write.csv(pca$var$coord, file=paste0(gsub('.csv', '', filename),"_PCA_variables_coord.csv"), quote=FALSE)  
  # Write the eigenvalues
  write.csv(pca$eig, file=paste0(gsub('.csv', '', filename),"_PCA_eigenvalues.csv"), quote=FALSE)

  # Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
  factoextra::fviz_eig(pca) + labs(y="Variance (%)") + pca_theme(36)
  ggsave(file.path(plots_dir, paste0(model, "_eigenvalues.pdf")), dpi=300, width=8, height=6)
  
  # only plots the labels if the individuals are not too many.
  if(is.null(select.ind) && nrow(pca$ind$coord) > 75) {
    label.ind = "none"
    repel.ind = FALSE
  }

  # PCA plots by components

  ndims <- ncol(pca$var$coord)
  if(ndims >= 2) {
    
    for(i in 1:ndims) {
      # Contributions of variables to PCi
      factoextra::fviz_contrib(pca, choice="var", axes=i) + 
        labs(title=paste0("PC", as.character(i))) +
        pca_theme(36)
      ggsave(file.path(plots_dir, paste0(model, "_contrib_var_PC", as.character(i),".pdf")), dpi=300, width=8, height=6)
    }    
    
    # create all the combinations we need. This will be a matrix(length(param.names) x 2), where each column is 
    # a pair.
    ind <- combn(ndims, 2)
    apply(ind, 2, function(x) {
  
      print(paste0('PC components : PC', as.character(x[1]), ' vs PC', as.character(x[2])))
      # Graph of individuals. Individuals with a similar profile are grouped together.
      factoextra::fviz_pca_ind(pca,
                               axes=c(x[1],x[2]),
                               col.ind = "contrib", # Color by contributions to the individuals
                               gradient.cols = c("blue", "red"),
                               label = label.ind,
                               select.ind = select.ind,
                               repel = repel.ind     # Avoid text overlapping
      ) +
        labs(title="PCA - indiv") +
        pca_theme(36)
      ggsave(file.path(plots_dir, paste0(model, "_individuals_PC", as.character(x[1]), "_PC", as.character(x[2]),".pdf")), dpi=300, width=8, height=6)
      
      # Graph of variables. Positive correlated variables point to the same side of the plot.
      # Negative correlated variables point to opposite sides of the graph.
      factoextra::fviz_pca_var(pca,
                               axes=c(x[1],x[2]),
                               col.var = "contrib", # Color by contributions to the PC
                               gradient.cols = c("blue", "red"),
                               label = label.var,
                               select.var = select.var,
                               repel = repel.var     # Avoid text overlapping
      ) + 
        labs(title="PCA - vars") +
        pca_theme(36)
      ggsave(file.path(plots_dir, paste0(model, "_variables_PC", as.character(x[1]), "_PC", as.character(x[2]),".pdf")), dpi=300, width=8, height=6)
      
      # Biplot of individuals and variables
      factoextra::fviz_pca_biplot(pca,
                                  axes=c(x[1],x[2]),
                                  label = "var", 
                                  repel = repel.var,
                                  col.var = "#2E9FDF", # Variables color
                                  col.ind = "#696969"  # Individuals color
      ) + 
        labs(title="PCA - biplot") +
        pca_theme(36)
      ggsave(file.path(plots_dir, paste0(model, "_biplot_PC", as.character(x[1]), "_PC", as.character(x[2]),".pdf")), dpi=300, width=8, height=6)
      
  
    })
  }
  return()
}



#####################
# UTILITY FUNCTIONS #
#####################



#' Get parameter names
#'
#' @param filename the filename containing the best fits
#' @return the parameter names
get_param_names <- function(filename) {
  # load the fits for this parameter
  names <- colnames(data.table::fread(filename))
  names <- replace_colnames(names)
  remove <- c("Estimation", "ObjVal")
  return(setdiff(names, remove))
}


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
  return(df.cols)
}

