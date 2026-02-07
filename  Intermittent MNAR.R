#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

# load packages
library(ggplot2)
library(patchwork)
library(rblimp)
# set_blimp('/applications/blimp/blimp-nightly')
# remotes::update_packages('rblimp')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath1 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-intermittent.csv'
filepath2 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-intermittent.csv'

# create data frame from github data
growth <- read.csv(filepath1, stringsAsFactors = T)
intensive <- read.csv(filepath2, stringsAsFactors = T)
growth$occasion <- growth$time
intensive$occasion <- intensive$time

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar plotting.R')

#------------------------------------------------------------------------------#
# MAR (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

growth_mar <- rblimp(
  data = growth,
  clusterid = 'id', 
  ordinal = 'd',
  latent = 'id = b0i b1i',
  fixed = 'd time',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(growth_mar)

#------------------------------------------------------------------------------#
# ESTIMATE MISSINGNESS ICC (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_growth <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(icc_growth)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# dummy coded time
growth_tdummy <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  nominal = 'occasion',
  latent = 'id = b0i b1i',
  fixed = 'occasion d',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept occasion d occasion*d | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(growth_tdummy)

# linear trend
growth_tlin <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  latent = 'id = b0i b1i',
  fixed = 'time d',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept time d time*d | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(growth_tlin)

# quadratic trend
growth_tquad <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  latent = 'id = b0i b1i',
  fixed = 'time d',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept time time^2 d time*d time^2*d | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(growth_tquad)

names(growth_tdummy)

gro_sat <- plot_means(m ~ time | d, 
           model = growth_tdummy,
           ylab = "Missingness Probability",
           title = "A. Observed Probabilities",
           group_labels = c("0" = "Group 0", "1" = "Group 1")) + ylim(0,.30)

gro_dum <- plot_means(m.1.probability ~ time | d, 
           model = growth_tdummy,
           ylab = "Missingness Probability",
           title = "Dummy Coded Time",
           group_labels = c("0" = "Group 0", "1" = "Group 1")) + ylim(0,.30)

# plot marginal probabilities (average individual probabilities) by time and group
bivariate_plot(m ~ time | d, model = growth_tdummy, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)
bivariate_plot(m.1.probability ~ time | d, model = growth_tdummy, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)
bivariate_plot(m.1.probability ~ time | d, model = growth_tlin, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)
bivariate_plot(m.1.probability ~ time | d, model = growth_tquad, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)

# compute marginal probabilities (average individual probabilities) by time and group
miss_growth_sat <- aggregate(m ~ time + d, data = growth_tdummy@average_imp, mean)
miss_growth_tdummy <- aggregate(m.1.probability ~ time + d, data = growth_tdummy@average_imp, mean)
miss_growth_tlin <- aggregate(m.1.probability ~ time + d, data = growth_tlin@average_imp, mean)
miss_growth_tquad <- aggregate(m.1.probability ~ time + d, data = growth_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_gro_tdummy <- sqrt(mean((miss_growth_tdummy$m.1.probability - miss_growth_sat$m)^2))
rmse_gro_tlin <- sqrt(mean((miss_growth_tlin$m.1.probability - miss_growth_sat$m)^2))
rmse_gro_tquad <- sqrt(mean((miss_growth_tquad$m.1.probability - miss_growth_sat$m)^2))
rmse_gro_tdummy; rmse_gro_tlin; rmse_gro_tquad

# summarize difference between marginal vs. observed probabilities
summary(miss_growth_tdummy$m.1.probability - miss_growth_sat$m)
summary(miss_growth_tlin$m.1.probability - miss_growth_sat$m)
summary(miss_growth_tquad$m.1.probability - miss_growth_sat$m)

#------------------------------------------------------------------------------#
# WU-CARROLL MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# wu-carroll model
growth_wc <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  nominal = 'occasion',
  latent = 'id = b0i b1i',
  fixed = 'd time occasion',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept occasion d occasion*d b0i b1i | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(growth_wc)

#------------------------------------------------------------------------------#
# MAR (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_mar <- rblimp(
  data = intensive,
  clusterid = 'id', 
  ordinal = 'd',
  latent = 'id = b0i b1i',
  fixed = 'd',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_mar)

#------------------------------------------------------------------------------#
# ESTIMATE MISSINGNESS ICC (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_intensive <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(icc_intensive)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# dummy coded time
intensive_tdummy <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  nominal = 'occasion',
  latent = 'id = b0i b1i',
  fixed = 'occasion d',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept occasion d occasion*d | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_tdummy)

# linear trend
intensive_tlin <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  latent = 'id = b0i b1i',
  fixed = 'time d',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept time d time*d | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_tlin)

# quadratic trend
intensive_tquad <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm d',
  latent = 'id = b0i b1i',
  fixed = 'time d',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept time time^2 d time*d time^2*d | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_tquad)

# plot marginal probabilities (average individual probabilities) by time and group
bivariate_plot(m ~ time | d, model = intensive_tdummy, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)
bivariate_plot(m.1.probability ~ time | d, model = intensive_tdummy, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)
bivariate_plot(m.1.probability ~ time | d, model = intensive_tlin, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)
bivariate_plot(m.1.probability ~ time | d, model = intensive_tquad, discrete_x = 'time', points = F, ci = F) + ylim(0,.30)

# compute marginal probabilities (average individual probabilities) by time and group
miss_intensive_sat <- aggregate(m ~ time + d, data = intensive_tdummy@average_imp, mean)
miss_intensive_tdummy <- aggregate(m.1.probability ~ time + d, data = intensive_tdummy@average_imp, mean)
miss_intensive_tlin <- aggregate(m.1.probability ~ time + d, data = intensive_tlin@average_imp, mean)
miss_intensive_tquad <- aggregate(m.1.probability ~ time + d, data = intensive_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_int_tdummy <- sqrt(mean((miss_intensive_tdummy$m.1.probability - miss_intensive_sat$m)^2))
rmse_int_tlin <- sqrt(mean((miss_intensive_tlin$m.1.probability - miss_intensive_sat$m)^2))
rmse_int_tquad <- sqrt(mean((miss_intensive_tquad$m.1.probability - miss_intensive_sat$m)^2))
rmse_int_tdummy; rmse_int_tlin; rmse_int_tquad

# summarize difference between marginal vs. observed probabilities
summary(miss_intensive_tdummy$m.1.probability - miss_intensive_sat$m)
summary(miss_intensive_tlin$m.1.probability - miss_intensive_sat$m)
summary(miss_intensive_tquad$m.1.probability - miss_intensive_sat$m)

#------------------------------------------------------------------------------#
# WU-CARROLL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_wc <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  ordinal = 'm d',
  nominal = 'occasion',
  latent = 'id = b0i b1i',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  fixed = 'time d',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 d;
    b1i ~ 1 d;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept occasion d occasion*d b0i b1i | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_wc)
