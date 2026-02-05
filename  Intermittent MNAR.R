#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

# load packages
library(ggplot2)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath1 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-intermittent.csv'
filepath2 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-intermittent.csv'

# create data frame from github data
growth <- read.csv(filepath1, stringsAsFactors = T)
intensive <- read.csv(filepath2, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# ESTIMATE ICCS ----
#------------------------------------------------------------------------------#

icc_growth <- rblimp(
  data = growth,
  clusterid = 'id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)
output(icc_growth)

icc_intensive <- rblimp(
  data = intensive,
  clusterid = 'id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)
output(icc_intensive)
