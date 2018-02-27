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
# Object: data set generation. 





library(devtools)

# model simulation
# save
IRbeta_pY1146 <- read.delim("sge_insulin_receptor_stoch_IR_beta_pY1146.csv")
devtools::use_data(IRbeta_pY1146, overwrite = TRUE)

# save/load
#save(IRbeta_pY1146, file=file.path("..", "data", "IRbeta_pY1146.rda"))
#load(file=file.path(".", "IRbeta_pY1146.rda"))



# model parameter estimation
# save
IRbeta_all_fits <- read.delim("all_estim_collection.csv")
IRbeta_best_fits <- read.delim("final_estim_collection.csv")
devtools::use_data(IRbeta_all_fits, overwrite = TRUE)
devtools::use_data(IRbeta_best_fits, overwrite = TRUE)

