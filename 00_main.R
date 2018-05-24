### MAIN

# Loading Packages -------------------------------------------------------------

library('readxl')
library('dplyr')
library('reshape2')
library('tempdisagg')
library('zoo')
library('ggplot2')
library('forcats')
library('magrittr')
library('ggfortify')
library('forecast')
library('stargazer')
library('Cairo')
library('lubridate')
library('urca')
library('ggcorrplot')


source('00_functions.R')


# INPUT ------------------------------------------------------------------------

## importing data
source('00_input.R')

## cleaning imported data
source('01_data_cleaning.R')


# PLOTS ------------------------------------------------------------------------

# source('01_plots.R')


# DATA MANIPULATION ------------------------------------------------------------

source('01_data_manipulation.R')


# DATA DISAGGREGATION ----------------------------------------------------------

source('01_disaggregation.R')


# STATIONARITY -----------------------------------------------------------------

source('01_stationarity.R')


# CORRELATION ------------------------------------------------------------------

source('01_correlation.R')


# MODEL ------------------------------------------------------------------------

source('01_model.R')








