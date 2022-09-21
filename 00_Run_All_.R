#----------------------------
# Author: Carlos Ortega
# Date:   2022-06-27
# Input:  Use functions in 01_Adecco_AsIs_.R + 02_Adecco_Ucra_.R to get outputs 
#         with and without stagflation.
# Purpose: Iteration with may data.
#----------------------------
#-- Run everything.

source("./01_Adecco_AsIs_.R")
#-- Run function
adeccoasis(0, 202205, 2022, 5)
adeccoasis(1, 202205, 2022, 5)

source("./02_Adecco_Ucra_.R")
#-- Run function
adeccoucra(0, 202205, 2022, 5)
adeccoucra(1, 202205, 2022, 5)

#-- CSVs in output directory.
#-- Charts in charts directory.

#-------- END OF FILE -----------
