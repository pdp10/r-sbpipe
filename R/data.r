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
"insulin_receptor_all_fits"


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
"insulin_receptor_best_fits"


#' A stochastic simulation data set for the insulin receptor beta phosphorylated at pY1146.
#'
#' Independent stochastic simulation time courses for the phosphorylated insulin receptor upon insulin stimulation. This data set is summarised.
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
"insulin_receptor_IR_beta_pY1146"


#' Experimental data set for the insulin receptor beta phosphorylated at pY1146 as published in 
#' Dalle Pezze et al. Science Signaling 2012.
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{Time}{The time variable}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_exp_dataset"


#' A stochastic model simulation
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{Time}{The time variable}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_1"


#' A stochastic model simulation
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{Time}{The time variable}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_2"


#' A stochastic model simulation
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{Time}{The time variable}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_3"

#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 0.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l0"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 1.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l1"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 3.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l3"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 4.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l4"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 6.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l6"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 8.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l8"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 9.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l9"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 11.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l11"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 13.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l13"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 14.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l14"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 1 model parameter. The initial amount of IR-beta is approx 16.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Time}{The time variable.}
#'   \item{IR_beta}{The unphosphorylated state of the insulin receptor beta. The scanned variable.}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#' }
"insulin_receptor_ps1_l16"


#' A deterministic simulation of the insulin receptor model upon scanning of 
#' 2 model parameters.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{Time}{The time variable. This dataset is at time T=2min upon insulin stimulation}
#'   \item{IR_beta_pY1146}{The insulin receptor beta phosphorylated at pY1146}
#'   \item{IRbetaPercent}{The percent of available IR_beta amount.}
#'   \item{InsulinPercent}{The percent of available insulin amount.}
#' }
"insulin_receptor_ps2_tp2"

