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
# Object: Plots
#
# $Revision: 3.0 $
# $Author: Piero Dalle Pezze $
# $Date: 2016-07-01 14:14:32 $


# Roxygen2 will import the functions of the following package in the namespace of this package
#' @import ggplot2
#' @importFrom stats density median qf qnorm quantile sd var
#' @importFrom utils read.table tail write.table
#' @importFrom colorRamps matlab.like 


#library(reshape2)
#library(ggplot2)
#library(gridExtra)

require(graphics)



#' Normalise a vector within 0 and 1
#'
#' @param vec the vector to normalise 
#' @param na.rm TRUE if NA values should be discarded
#' @return the normalised vector
#' @examples 
#' normalise_vec(vec=c(-4,2,10,25,9,NA))
#' @export
normalise_vec <- function(vec, na.rm = TRUE) {
  ranx <- range(vec, na.rm = na.rm)
  vec.norm <- (vec - ranx[1]) / diff(ranx)
  return(vec.norm)
}


#' Plot a generic histogram
#'
#' @param dfCol a data frame with exactly one column.
#' @param g the current ggplot to overlap
#' @return the plot
#' @export
histogramplot <- function(dfCol, g=ggplot()) {
    g <- g +
        geom_histogram(data=dfCol, aes_string(x=colnames(dfCol)), binwidth=density(dfCol[,])$bw, colour="black", fill="blue") +
        theme(axis.text.x=element_text(vjust = 1))
    return(g)
}



#' Plot a scatter plot using a coloured palette
#'
#' @param df a data frame
#' @param g the current ggplot to overlap
#' @param colNameX the name of the column for the X axis
#' @param colNameY the name of the column for the Y axis
#' @param colNameColor the name of the column whose values are used as 3rd dimension
#' @param dot_size the size of the dots in the scatterplot
#' @param colours the palette to use
#' @param limits the limits for the palette (NULL if no limit is used)
#' @return the plot
#' @export
scatterplot_w_colour <- function(df, 
                                 g=ggplot(), 
                                 colNameX, 
                                 colNameY, 
                                 colNameColor, 
                                 dot_size=1.0, 
                                 colours=colorRamps::matlab.like(256), 
                                 limits=NULL) {

# If the third coordinate has equal values, then use the first value (default: red)
  colorCol <- df[,c(colNameColor)]
  countFirst <- length(grep(colorCol[1], colorCol))
  if(countFirst == length(colorCol)) {
    colours <- colours[0:1]
  }

  g <- g + geom_point(data=df, aes_string(x=colNameX, y=colNameY, color=colNameColor), size=dot_size) +
      scale_colour_gradientn(colours=colours, limits) +
       #geom_rug(col="darkblue",alpha=.1) +    
       theme(axis.text.x=element_text(vjust = 1))  
  # #add marginal histograms
  #ggExtra::ggMarginal(g, type = "histogram")
  return(g)
}


#' Plot a profile likelihood estimation (PLE) scatter plot
#'
#' @param df a data frame
#' @param g the current ggplot to overlap
#' @param colNameX the name of the column for the X axis
#' @param colNameY the name of the column for the Y axis
#' @param conf_level_66 the 66\% confidence level to plot
#' @param conf_level_95 the 95\% confidence level to plot
#' @param conf_level_99 the 99\% confidence level to plot
#' @param dot_size the size of the dots in the scatterplot
#' @return the plot
#' @export
scatterplot_ple <- function(df, 
                            g=ggplot(), 
                            colNameX, 
                            colNameY, 
                            conf_level_66, 
                            conf_level_95, 
                            conf_level_99, 
                            dot_size=0.1) {
  df.thresholds <- data.frame(conf_level_66, conf_level_95, conf_level_99)
  g <- g + geom_point(data=df, aes_string(x=colNameX, y=colNameY), size=dot_size)

  if (conf_level_66 > 0) {
      g <- g +
          geom_hline(data=df.thresholds, aes(yintercept=conf_level_66, color="_66", linetype="_66"), size=2, show.legend=TRUE) +
          geom_hline(data=df.thresholds, aes(yintercept=conf_level_95, color="_95", linetype="_95"), size=2, show.legend=TRUE) +
          geom_hline(data=df.thresholds, aes(yintercept=conf_level_99, color="_99", linetype="_99"), size=2, show.legend=TRUE) +
          scale_colour_manual(name="", labels=c("_99"="CL99%","_95"="CL95%","_66"="CL66%"), values=c("_99"="dodgerblue","_95"="green2","_66"="magenta1")) +
    #      scale_linetype_manual(name="", labels=c("_99"="CL99%","_95"="CL95%","_66"="CL66%"), values=c("_99"="solid", "_95"="solid", "_66"="solid")) +
          scale_linetype_manual(name="", labels=c("_99"="CL99%","_95"="CL95%","_66"="CL66%"), values=c("_99"="dashed", "_95"="dashed", "_66"="dashed")) +
          guides(colour = guide_legend(reverse=T), linetype = guide_legend(reverse=T))
  }
  g <- g +
      ylab('obj val') +
      theme(axis.text.x=element_text(vjust = 1))
  return(g)
}



#' Plot a generic scatter plot
#'
#' @param df a data frame
#' @param g the current ggplot to overlap
#' @param colNameX the name of the column for the X axis
#' @param colNameY the name of the column for the Y axis
#' @param dot_size the size of the dots in the scatterplot
#' @return the plot
#' @export
scatterplot <-function(df, g=ggplot(), colNameX, colNameY, dot_size=0.5) {
  g <- g +
       geom_point(data=df, aes_string(x=colNameX, y=colNameY), size=dot_size) +
       theme(axis.text.x=element_text(vjust = 1))
  return(g)
}



#' Plot a generic scatter plot in log10 scale
#'
#' @param df a data frame to trasform to log10 scale
#' @param g the current ggplot to overlap
#' @param colNameX the name of the column for the X axis
#' @param colNameY the name of the column for the Y axis
#' @param dot_size the size of the dots in the scatterplot
#' @return the plot
#' @export
scatterplot_log10 <-function(df, g=ggplot(), colNameX, colNameY, dot_size=0.5) {
  df <- log10(df)
  g <- scatterplot(df, g, colNameX, colNameY, dot_size) +
       #scale_x_log10() +
       #scale_y_log10() +
       xlab(paste("log10(", colNameX, ")", sep="")) +
       ylab(paste("log10(", colNameY, ")", sep="")) #+
       #annotation_logticks()
  return(g)
}



#' Plot the number of iterations vs objective values in log10 scale.
#'
#' @param objval_array the array of objective function values.
#' @param g the current ggplot to overlap
#' @return the plot
#' @export
plot_fits <- function(objval_array, g=ggplot()) {
  iters <- c()
  j <- 0
  k <- 0

  # We intentionally consider only the objective values below 100*median(objval_array).
  # Often the very first objective values can be extremely large (e^[hundreds]). When so,
  # ggsave() does not process correctly, potentially due to a bug.
  med_objval <- median(objval_array[is.finite(objval_array)])
  objval_array <- objval_array[objval_array < med_objval*100]

  for(i in 1:length(objval_array)) {
    if(k < objval_array[i]) {
      j <- 0
    }
    iters <- c(iters, j)
    j <- j+1
    k <- objval_array[i]
  }
  df <- data.frame(Iter=iters, ObjVal=objval_array)
  g <- scatterplot_log10(df, g, "Iter", "ObjVal") +
            # Re paint the y lab as we want to use the Greek letter chi.
            ylab("log10(ObjVal)")
  return(g)
}



#' Add experimental data points to a plot. The length of the experimental time course to plot is limited by the length of the simulated time course (=max_sim_tp).
#'
#' @param df_exp_dataset the experimental data set
#' @param g the current ggplot to overlap
#' @param readout the name of the readout
#' @param max_sim_tp the maximum simulated time point
#' @param alpha the amount of alpha transparency
#' @return the plot
#' @export
plot_raw_dataset <- function(df_exp_dataset, g=ggplot(), readout="time", max_sim_tp=0, alpha=1) {
    # Let's add the experimental data set to the plot
    time <- colnames(df_exp_dataset)[1]
    df_exp_dataset <- df_exp_dataset[df_exp_dataset[1] <= max_sim_tp,]
    g <- g + geom_point(data=df_exp_dataset, aes_string(x=time, y=readout),
                        shape=1, size=2, stroke=1, colour='red2', alpha=alpha)
    return(g)
}



#' Plot repeated time courses in the same plot with mean, 1 standard deviation, and 95\% confidence intervals.
#'
#' @param df a data frame
#' @param g the current ggplot to overlap
#' @param title the title
#' @param xaxis_label the xaxis label
#' @param yaxis_label the yaxis label
#' @param bar_type the type of bar ("mean", "mean_sd", "mean_sd_ci95")
#' @param alpha the amount of alpha transparency
#' @return the plot
#' @export
plot_combined_tc <- function(df, 
                             g=ggplot(), 
                             title="", 
                             xaxis_label="", 
                             yaxis_label="", 
                             bar_type="mean", 
                             alpha=1) {
    mdf <- reshape2::melt(df,id.vars="Time",variable.name="variable",value.name="value")
    if(bar_type == "mean_sd" || bar_type == "mean_sd_ci95") {
        g <- g + stat_summary(data=mdf, aes_string(x="Time", y="value"),
                              geom="ribbon", fun.data = mean_sdl, fill="#99CCFF", alpha=alpha)
        if(bar_type == "mean_sd_ci95") {
            g <- g + stat_summary(data=mdf, aes_string(x="Time", y="value"),
                                  geom="ribbon", fun.data=mean_cl_normal,
                                  fun.args=list(conf.int=0.95), fill="#5588CC", alpha=alpha)   # #CCFFFF
        }
    }
    g <- g + stat_summary(data=mdf, aes_string(x="Time", y="value"), geom="line", fun.y=mean, size=1.0, color="black") +
         xlab(xaxis_label) + ylab(yaxis_label) + ggtitle(title)
    return(g)
}



#' Plot repeated time courses in the same plot separately. First column is Time.
#'
#' @param df a data frame
#' @param g the current ggplot to overlap
#' @param title the title of the plot 
#' @param xaxis_label the xaxis label of the plot
#' @param yaxis_label the yaxis label of the plot
#' @param alpha the amount of alpha transparency
#' @return the plot
#' @export
plot_repeated_tc <- function(df, 
                             g=ggplot(), 
                             title='', 
                             xaxis_label="", 
                             yaxis_label="", 
                             alpha=1) {
    mdf <- reshape2::melt(df,id.vars="Time",variable.name="variable",value.name="value")
    g <- g + geom_line(data=mdf,aes_string(x="Time",y="value",color="variable"), size=1.0, alpha=alpha) +
         xlab(xaxis_label) + ylab(yaxis_label) + ggtitle(title) +
         theme(legend.position="none")
    return(g)
}





#' Plot time courses organised as data frame columns with a heatmap.
#'
#' @param df a data frame, with Time as first column
#' @param g the current ggplot to overlap
#' @param scaled TRUE if the time course values should be scaled within 0 and 1.
#' @param title the title of the plot 
#' @param xaxis_label the xaxis label of the plot
#' @param yaxis_label the yaxis label of the plot
#' @return the plot
#' @export
plot_heatmap_tc <- function(df, 
                            g=ggplot(), 
                            scaled=TRUE, 
                            title='', 
                            xaxis_label='', 
                            yaxis_label='') {
    if(scaled) {
        # normalise within 0 and 1
        df.norm <- cbind(data.frame(df[1]), apply(df[-1], 2, normalise_vec))
        colnames(df.norm)[1] <- "Time"
        mdf <- reshape2::melt(df.norm,id.vars="Time",variable.name="variable",value.name="value")
        # use geom_raster() for generating a heatmap    
        g <- g + geom_raster(data=mdf, aes_string(x="variable", y="Time", fill="value"))    
    } else {
        mdf <- reshape2::melt(df,id.vars="Time",variable.name="variable",value.name="value")      
        # use geom_raster() for generating a heatmap    
        g <- g + geom_raster(data=mdf, aes_string(x="variable", y="Time", fill="value"))
    }
    # remove gaps between axis and plot area
    g <- g + scale_y_continuous(expand = c(0, 0)) 
    g <- g + scale_fill_gradientn(colours = matlab.like(256)) +
    #g <- g + scale_fill_gradient(low = "white", high = "steelblue") +      # OLD style 
             # NOTE we will flip the coordinates later.
             xlab(yaxis_label) + ylab(xaxis_label) + ggtitle(title) +
             # guides(fill = guide_legend(title=legend_label,
             #                            title.position='left',
             #                            title.theme = element_text(size = 25, angle = 90))) +
             #guides(guide_legend(title=NULL,
            #                             label.theme = element_text(size=20, angle=0))) +
             theme(legend.title = element_blank(), legend.text=element_text(size=25),
                   legend.key.size = unit(0.48, "in"),
                   axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
             coord_flip()
    return(g)
}


