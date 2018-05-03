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


######################
# EXPORTED FUNCTIONS #
######################



#' A theme for time courses. It extends ggplot2 theme_classic().
#'
#' @param base_size the font size
#' @param base_family the font family
#' @examples
#' library(ggplot2)
#' theme_set(tc_theme(36))
#' @export
tc_theme <- function (base_size=12, base_family="") {
  theme_classic(base_size=base_size, base_family=base_family) %+replace% 
  theme(aspect.ratio = 0.5,
        axis.line = element_line(colour="black", size=1.0),
        axis.text.x=element_text(angle=-45, hjust=0, vjust=0.8),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0.5,0.5,0.8,0.5), "in")
        )
}


#' A generic basic theme for time courses. It extends ggplot2 theme_classic().
#'
#' @param base_size the font size
#' @param base_family the font family
#' @examples
#' library(ggplot2)
#' theme_set(basic_theme(36))
#' @export
basic_theme <- function (base_size=12, base_family="") {
  theme_classic(base_size=base_size, base_family=base_family) %+replace% 
  theme(aspect.ratio = 1,
        axis.line = element_line(colour = "black", size=1.0),
        axis.text.x=element_text(angle=-45, hjust=0, vjust=0.8),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)
        )
}


#' A generic basic theme for pca. It extends ggplot2 theme_classic().
#'
#' @param base_size the font size
#' @param base_family the font family
#' @examples
#' library(ggplot2)
#' theme_set(pca_theme(36))
#' @export
pca_theme <- function (base_size=12, base_family="") {
  theme_classic(base_size=base_size, base_family=base_family) %+replace% 
  theme(aspect.ratio = 1,
        axis.line = element_line(colour = "black", size=1.0),
        axis.text.x=element_text(angle=-45, hjust=0, vjust=0.8),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        legend.text=element_text(size=30),
        legend.key.width = unit(0.4, "in"), 
        legend.key.height = unit(0.5, "in")
        )
}
