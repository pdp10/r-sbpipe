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


# model single parameter scan
insulin_receptor_ps1_l0 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_0.csv")
insulin_receptor_ps1_l1 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_1.csv")
insulin_receptor_ps1_l3 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_3.csv")
insulin_receptor_ps1_l4 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_4.csv")
insulin_receptor_ps1_l6 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_6.csv")
insulin_receptor_ps1_l8 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_8.csv")
insulin_receptor_ps1_l9 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_9.csv")
insulin_receptor_ps1_l11 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_11.csv")
insulin_receptor_ps1_l13 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_13.csv")
insulin_receptor_ps1_l14 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_14.csv")
insulin_receptor_ps1_l16 <- read.delim("insulin_receptor_scan_IR_beta__rep_1__level_16.csv")
devtools::use_data(insulin_receptor_ps1_l0, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l1, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l3, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l4, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l6, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l8, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l9, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l11, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l13, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l14, overwrite = TRUE)
devtools::use_data(insulin_receptor_ps1_l16, overwrite = TRUE)


# model double parameter scan
insulin_receptor_ps2_tp2 <- 
  read.delim("insulin_receptor_InsulinPercent__IRbetaPercent__rep_1__tp_2.csv")
devtools::use_data(insulin_receptor_ps2_tp2, overwrite = TRUE)


# model parameter estimation
insulin_receptor_all_fits <- read.delim("all_estim_collection.csv")
insulin_receptor_best_fits <- read.delim("final_estim_collection.csv")
devtools::use_data(insulin_receptor_all_fits, overwrite = TRUE)
devtools::use_data(insulin_receptor_best_fits, overwrite = TRUE)

