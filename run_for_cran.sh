#!/bin/bash
#
#
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
#
#
# This script removes CRAN non-standard files/directories found at top level.
# It should be executed before the following commands:
# 
# R CMD build .
# R CMD check *tar.gz --as-cran


# remove files for github
rm -f CODE_OF_CONDUCT.md
rm -f CONTRIBUTING.md
rm -f ISSUE_TEMPLATE.md
rm -f .travis.yml

# remove files for conda
rm -rf conda_recipe
rm -f environment.yaml
rm -f miniconda.sh

# remove this script
rm -f run_for_cran.sh
