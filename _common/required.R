############################## Required libraries ##############################

# to install the non-present libraries: install.packages(<library>)
if (!("here" %in% (.packages()))) library(here) # to handle paths relative to project ROOT

#library(zip) # to avoid some warnings with openxlsx and readxls packages
library(tidyverse)
library(janitor)
library(openxlsx)
library(data.table)
library(scales)
library(lubridate)