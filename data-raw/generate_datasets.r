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
insulin_receptor_sim <- read.delim("sge_insulin_receptor_stoch_IR_beta_pY1146.csv")
#save(insulin_receptor_sim, file=file.path("..", "data", "insulin_receptor_sim.rda"))

devtools::use_data(insulin_receptor_sim, overwrite = TRUE)

# load
#load(file=file.path(".", "insulin_receptor_sim.rda"))



# model parameter estimation
# save
all_fits_pe <- read.delim("all_estim_collection.csv")
best_fits_pe <- read.delim("final_estim_collection.csv")
devtools::use_data(all_fits_pe, overwrite = TRUE)
devtools::use_data(best_fits_pe, overwrite = TRUE)




