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


library(devtools)

# experimental data set
insulin_receptor_exp_dataset <- read.delim("insulin_receptor_dataset.csv")
devtools::use_data(insulin_receptor_exp_dataset, overwrite = TRUE)


# model simulation (40 repeats). Summarised data set.
insulin_receptor_IR_beta_pY1146 <- read.delim("sge_insulin_receptor_stoch_IR_beta_pY1146.csv")
devtools::use_data(insulin_receptor_IR_beta_pY1146, overwrite = TRUE)


# model simulation (3 independent repeats). 
insulin_receptor_1 <- read.delim("insulin_receptor_stoch_1.csv")
insulin_receptor_2 <- read.delim("insulin_receptor_stoch_2.csv")
insulin_receptor_3 <- read.delim("insulin_receptor_stoch_3.csv")
devtools::use_data(insulin_receptor_1, overwrite = TRUE)
devtools::use_data(insulin_receptor_2, overwrite = TRUE)
devtools::use_data(insulin_receptor_3, overwrite = TRUE)


# model double parameter scan
insulin_receptor_ps2_tp2 <- 
  read.delim("insulin_receptor_InsulinPercent__IRbetaPercent__rep_1__tp_2.csv")
devtools::use_data(insulin_receptor_ps2_tp2, overwrite = TRUE)


# model parameter estimation
insulin_receptor_all_fits <- read.delim("all_estim_collection.csv")
insulin_receptor_best_fits <- read.delim("final_estim_collection.csv")
devtools::use_data(insulin_receptor_all_fits, overwrite = TRUE)
devtools::use_data(insulin_receptor_best_fits, overwrite = TRUE)

