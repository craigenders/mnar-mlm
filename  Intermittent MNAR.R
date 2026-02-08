#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options (scipen = 999)

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
growth$group <- growth$d
intensive$group <- intensive$d

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# MAR (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

growth_mar <- rblimp(
  data = growth,
  clusterid = 'id', 
  ordinal = 'group',
  latent = 'id = b0i b1i',
  fixed = 'group time',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
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
# ICC FOR THE MISSINGNESS INDICATOR (LONGITUDINAL GROWTH) ----
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
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i',
  fixed = 'occasion time group',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept occasion group occasion*group | intercept;',
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
  ordinal = 'm group',
  latent = 'id = b0i b1i',
  fixed = 'time group',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept time group time*group | intercept;',
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
  ordinal = 'm group',
  latent = 'id = b0i b1i',
  fixed = 'time group',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(growth_tquad)

#------------------------------------------------------------------------------#
# CURSIO ET AL. MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

cursio_1pl <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i u0i',
  fixed = 'occasion group time',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    u0i ~ intercept;
    m ~ intercept@u0i occasion;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(cursio_1pl)

# cursio 2pl model
cursio_2pl <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i u0i',
  fixed = 'time occasion group',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    u0i ~ intercept;
    m ~ intercept@u0i occasion occasion*u0i;',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(cursio_2pl)

# cursio 2pl model
growth_cursio_2pl_cent <- rblimp(
  data = growth,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i u0i',
  fixed = 'time occasion group',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    u0i ~ intercept@0;
    m ~ intercept occasion u0i@1 occasion*u0i | intercept@0;',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(growth_cursio_2pl_cent)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

gro_obs <- plot_means(m ~ time | group, 
           model = growth_tdummy,
           ylab = "Missingness Probability",
           title = "A. Observed Probabilities (Growth Data)",
           group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

gro_obs_f3 <- plot_means(m ~ time | group, 
                      model = growth_tdummy,
                      ylab = "Missingness Probability",
                      title = "Observed Probabilities",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

gro_dum <- plot_means(m.1.probability ~ time | group, 
           model = growth_tdummy,
           ylab = "Missingness Probability",
           title = "Dummy Coded Time",
           group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

gro_lin <- plot_means(m.1.probability ~ time | group, 
                      model = growth_tlin,
                      ylab = "Missingness Probability",
                      title = "Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

gro_quad <- plot_means(m.1.probability ~ time | group, 
                      model = growth_tquad,
                      ylab = "Missingness Probability",
                      title = "Quadratic Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_growth_obs <- aggregate(m ~ time + group, data = growth_tdummy@average_imp, mean)
pmiss_growth_tdummy <- aggregate(m.1.probability ~ time + group, data = growth_tdummy@average_imp, mean)
pmiss_growth_tlin <- aggregate(m.1.probability ~ time + group, data = growth_tlin@average_imp, mean)
pmiss_growth_tquad <- aggregate(m.1.probability ~ time + group, data = growth_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_gro_tdummy <- sqrt(mean((pmiss_growth_tdummy$m.1.probability - pmiss_growth_obs$m)^2))
rmse_gro_tlin <- sqrt(mean((pmiss_growth_tlin$m.1.probability - pmiss_growth_obs$m)^2))
rmse_gro_tquad <- sqrt(mean((pmiss_growth_tquad$m.1.probability - pmiss_growth_obs$m)^2))
rmse_gro_tdummy; rmse_gro_tlin; rmse_gro_tquad

# summarize difference between marginal vs. observed probabilities
summary(pmiss_growth_tdummy$m.1.probability - pmiss_growth_obs$m)
summary(pmiss_growth_tlin$m.1.probability - pmiss_growth_obs$m)
summary(pmiss_growth_tquad$m.1.probability - pmiss_growth_obs$m)

#------------------------------------------------------------------------------#
# WU-CARROLL MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# # wu-carroll model
# growth_wc <- rblimp(
#   data = growth,
#   clusterid = 'id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'time',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'id = b0i b1i',
#   fixed = 'group time occasion',
#   model = '
#     level2:
#     b0i ~ 1 group;
#     b1i ~ 1 group;
#     b0i ~~ b1i;
#     level1:
#     y ~ 1@b0i time@b1i;
#     missingness:
#     m ~ intercept occasion group occasion*group b0i b1i | intercept;',
#   seed = 90291,
#   burn = 10000,
#   iter = 10000,
#   nimps = 20)
# 
# # print output
# output(growth_wc)

#------------------------------------------------------------------------------#
# MAR (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_mar <- rblimp(
  data = intensive,
  clusterid = 'id', 
  ordinal = 'group',
  latent = 'id = b0i b1i',
  fixed = 'group',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
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
# ICC FOR THE MISSINGNESS INDICATOR (INTENSIVE MEASUREMENTS) ----
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
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i',
  fixed = 'occasion group',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept occasion group occasion*group | intercept;',
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
  ordinal = 'm group',
  latent = 'id = b0i b1i',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept time group time*group | intercept;',
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
  ordinal = 'm group',
  latent = 'id = b0i b1i',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i x@b1i;
    missingness:
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_tquad)

#------------------------------------------------------------------------------#
# CURSIO ET AL. MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

intensive_cursio_1pl <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i u0i',
  fixed = 'occasion group time',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    u0i ~ intercept;
    m ~ intercept@u0i occasion;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_cursio_1pl)

# cursio 2pl model
intensive_cursio_2pl <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'occasion',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
  nominal = 'occasion',
  latent = 'id = b0i b1i u0i',
  fixed = 'time occasion group',
  model = '
    level2:
    b0i ~ 1 group;
    b1i ~ 1 group;
    b0i ~~ b1i;
    level1:
    y ~ 1@b0i time@b1i;
    missingness:
    u0i ~ intercept;
    m ~ intercept@u0i occasion occasion*u0i;',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(intensive_cursio_2pl)


#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

int_obs <- plot_means(m ~ time | group, 
                      model = intensive_tdummy,
                      ylab = "Missingness Probability",
                      title = "B. Observed Probabilities (Intensive Data)",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

int_obs_f4 <- plot_means(m ~ time | group, 
                      model = intensive_tdummy,
                      ylab = "Missingness Probability",
                      title = "Observed Probabilities",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

int_dum <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_tdummy,
                      ylab = "Missingness Probability",
                      title = "Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

int_lin <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_tlin,
                      ylab = "Missingness Probability",
                      title = "Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

int_quad <- plot_means(m.1.probability ~ time | group, 
                       model = intensive_tquad,
                       ylab = "Missingness Probability",
                       title = "Quadratic Time",
                       group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_obs <- aggregate(m ~ time + group, data = intensive_tdummy@average_imp, mean)
pmiss_intensive_tdummy <- aggregate(m.1.probability ~ time + group, data = intensive_tdummy@average_imp, mean)
pmiss_intensive_tlin <- aggregate(m.1.probability ~ time + group, data = intensive_tlin@average_imp, mean)
pmiss_intensive_tquad <- aggregate(m.1.probability ~ time + group, data = intensive_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_int_tdummy <- sqrt(mean((pmiss_intensive_tdummy$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tlin <- sqrt(mean((pmiss_intensive_tlin$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tquad <- sqrt(mean((pmiss_intensive_tquad$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tdummy; rmse_int_tlin; rmse_int_tquad

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_tdummy$m.1.probability - pmiss_intensive_obs$m)
summary(pmiss_intensive_tlin$m.1.probability - pmiss_intensive_obs$m)
summary(pmiss_intensive_tquad$m.1.probability - pmiss_intensive_obs$m)

#------------------------------------------------------------------------------#
# WU-CARROLL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# intensive_wc <- rblimp(
#   data = intensive,
#   clusterid = 'id', 
#   transform = 'm = ismissing(y)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'id = b0i b1i',
#   # timeid = 'time',
#   # dropout = 'm = y (missing)',
#   fixed = 'time group',
#   center = 'groupmean = x',
#   model = '
#     level2:
#     b0i ~ 1 group;
#     b1i ~ 1 group;
#     b0i ~~ b1i;
#     level1:
#     y ~ 1@b0i x@b1i;
#     missingness:
#     m ~ intercept occasion group occasion*group b0i b1i | intercept;',
#   seed = 90291,
#   burn = 10000,
#   iter = 10000,
#   nimps = 20)
# 
# # print output
# output(intensive_wc)

#------------------------------------------------------------------------------#
# FIGURE 2 ----
#------------------------------------------------------------------------------#

figure2 <- gro_obs / int_obs

# ggsave(
#   filename = "~/desktop/Figure 2. Obs Missingness.pdf",
#   plot = figure2,
#   width = 8.5,
#   height = 11,
#   units = "in"
# )

#------------------------------------------------------------------------------#
# FIGURE 3 AND 4 ----
#------------------------------------------------------------------------------#

figure3 <- gro_obs_f3 / gro_lin / gro_quad / gro_dum
figure4 <- int_obs_f4 / int_lin / int_quad / int_dum

ggsave(
  filename = "~/desktop/Figure 3. Time Related (Growth).pdf",
  plot = figure3,
  width = 8.5,
  height = 11,
  units = "in"
)

ggsave(
  filename = "~/desktop/Figure 4. Time Related (Intermittent).pdf",
  plot = figure4,
  width = 8.5,
  height = 11,
  units = "in"
)
