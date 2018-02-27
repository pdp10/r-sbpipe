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


#' A parameter estimation data set including all the evaluated fits.
#' 
#' Estimated parameters for a mini model of the insulin receptor (IRbeta).
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{ObjectiveValue}{the objective value for this parameter set}
#'   \item{k1}{First estimated parameter: kinetic rate constant for IRbeta phosphorylation.}
#'   \item{k2}{Second estimated parameter: kinetic rate constant for IRbeta refractory status.}
#'   \item{k3}{Third estimated parameter: kinetic rate constant for IRbeta dephosphorylation.}
#' }
"IRbeta_all_fits"


#' A parameter estimation data set including only the best evaluated fits.
#' 
#' Estimated parameters for a mini model of the insulin receptor (IRbeta).
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{Estimation}{the number of estimated parameter sets}
#'   \item{ObjectiveValue}{the best objective value for this parameter estimation}
#'   \item{k1}{First estimated parameter: kinetic rate constant for IRbeta phosphorylation.}
#'   \item{k2}{Second estimated parameter: kinetic rate constant for IRbeta refractory status.}
#'   \item{k3}{Third estimated parameter: kinetic rate constant for IRbeta dephosphorylation.}
#' }
"IRbeta_best_fits"


#' A stochastic simulation data set for the insulin receptor beta phosphorylated at pY1146.
#'
#' Independent stochastic simulation time courses for the phosphorylated insulin receptor upon insulin stimulation.
#'
#' @format A data frame with 41 variables:
#' \describe{
#'   \item{Time}{The time variable}
#'   \item{X1}{A stochastic simulation}
#'   \item{X2}{A stochastic simulation}
#'   \item{X3}{A stochastic simulation}
#'   \item{X4}{A stochastic simulation}
#'   \item{X5}{A stochastic simulation}
#'   \item{X6}{A stochastic simulation}
#'   \item{X7}{A stochastic simulation}
#'   \item{X8}{A stochastic simulation}
#'   \item{X9}{A stochastic simulation}
#'   \item{X10}{A stochastic simulation}
#'   \item{X11}{A stochastic simulation}
#'   \item{X12}{A stochastic simulation}
#'   \item{X13}{A stochastic simulation}
#'   \item{X14}{A stochastic simulation}
#'   \item{X15}{A stochastic simulation}
#'   \item{X16}{A stochastic simulation}
#'   \item{X17}{A stochastic simulation}
#'   \item{X18}{A stochastic simulation}
#'   \item{X19}{A stochastic simulation}
#'   \item{X20}{A stochastic simulation}
#'   \item{X21}{A stochastic simulation}
#'   \item{X22}{A stochastic simulation}
#'   \item{X23}{A stochastic simulation}
#'   \item{X24}{A stochastic simulation}
#'   \item{X25}{A stochastic simulation}
#'   \item{X26}{A stochastic simulation}
#'   \item{X27}{A stochastic simulation}
#'   \item{X28}{A stochastic simulation}
#'   \item{X29}{A stochastic simulation}
#'   \item{X30}{A stochastic simulation}
#'   \item{X31}{A stochastic simulation}
#'   \item{X32}{A stochastic simulation}
#'   \item{X33}{A stochastic simulation}
#'   \item{X34}{A stochastic simulation}
#'   \item{X35}{A stochastic simulation}
#'   \item{X36}{A stochastic simulation}
#'   \item{X37}{A stochastic simulation}
#'   \item{X38}{A stochastic simulation}
#'   \item{X39}{A stochastic simulation}
#'   \item{X40}{A stochastic simulation}
#' }
"IRbeta_pY1146"
