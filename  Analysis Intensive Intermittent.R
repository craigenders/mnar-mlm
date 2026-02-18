#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options (scipen = 999)

# load packages
library(ggplot2)
library(patchwork)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')
# remotes::update_packages('rblimp')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-intermittent.csv'

# create data frame from github data
intensive <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_com <- rblimp(
  data = intensive,
  clusterid = 'id', 
  latent = 'id = alpha beta logvar',
  fixed = 'group',
  center = 'groupmean = xcom',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    logvar ~ intercept;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    ycom ~ intercept@alpha xcom@beta;
    var(ycom) ~ intercept@logvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_com)

#------------------------------------------------------------------------------#
# MAR (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_mar <- rblimp(
  data = intensive,
  clusterid = 'id', 
  latent = 'id = alpha beta logvar',
  fixed = 'group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    logvar ~ intercept;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@logvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

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

# linear trend
intensive_tlin <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  latent = 'id = alpha beta',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha x@beta;
    missingness:
    m ~ intercept time group time*group | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_tlin)

# quadratic trend
intensive_tquad <- rblimp(
  data = intensive,
  clusterid = 'id', 
  transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  latent = 'id = alpha beta',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha x@beta;
    missingness:
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_tquad)

# dummy coded time
intensive_tdumf <- rblimp(
  data = intensive,
  clusterid = 'id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha x@beta;
    missingness:
    m ~ intercept group | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  waldtest = '
    m ~ intercept group | intercept;
    { t in 1:24 } : m ~ (time == [t]);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_tdumf)

# dummy coded time
intensive_tdum <- rblimp(
  data = intensive,
  clusterid = 'id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha x@beta;
    missingness:
    m ~ intercept group | intercept;
    { t in 1:24 } : m ~ (time == [t]);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_tdum)


#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .40
ymin <- 0

int_obs <- plot_means(m ~ time | group, 
                      model = intensive_tdum,
                      ylab = "Missingness Probability",
                      title = "A. Observed Probabilities (Growth Data)",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

int_obs_f3 <- plot_means(m ~ time | group, 
                         model = intensive_tdum,
                         ylab = "Missingness Probability",
                         title = "Observed Probabilities",
                         group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

int_dumf <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_tdumf,
                      ylab = "Missingness Probability",
                      title = "Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

int_dum <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_tdum,
                      ylab = "Missingness Probability",
                      title = "Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

int_lin <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_tlin,
                      ylab = "Missingness Probability",
                      title = "Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

int_quad <- plot_means(m.1.probability ~ time | group, 
                       model = intensive_tquad,
                       ylab = "Missingness Probability",
                       title = "Quadratic Time",
                       group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

int_obs; int_dum; int_lin; int_quad

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_obs <- aggregate(m ~ time + group, data = intensive_tdum@average_imp, mean)
pmiss_intensive_tlin <- aggregate(m.1.probability ~ time + group, data = intensive_tlin@average_imp, mean)
pmiss_intensive_tquad <- aggregate(m.1.probability ~ time + group, data = intensive_tquad@average_imp, mean)
pmiss_intensive_tdumf <- aggregate(m.1.probability ~ time + group, data = intensive_tdumf@average_imp, mean)
pmiss_intensive_tdum <- aggregate(m.1.probability ~ time + group, data = intensive_tdum@average_imp, mean)


# compute rmse of marginal vs. observed probabilities
rmse_int_tlin <- sqrt(mean((pmiss_intensive_tlin$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tquad <- sqrt(mean((pmiss_intensive_tquad$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tdumf <- sqrt(mean((pmiss_intensive_tdumf$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tdum <- sqrt(mean((pmiss_intensive_tdum$m.1.probability - pmiss_intensive_obs$m)^2))
rmse_int_tlin; rmse_int_tquad; rmse_int_tdumf; rmse_int_tdum

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_tlin$m.1.probability - pmiss_intensive_obs$m)
summary(pmiss_intensive_tquad$m.1.probability - pmiss_intensive_obs$m)
summary(pmiss_intensive_tdumf$m.1.probability - pmiss_intensive_obs$m)
summary(pmiss_intensive_tdum$m.1.probability - pmiss_intensive_obs$m)

#------------------------------------------------------------------------------#
# WU-CARROLL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_wcl <- rblimp(
  data = intensive,
  clusterid = 'id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta logvar',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    logvar ~ intercept@g0o;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@logvar;
    missingness:
    m ~ intercept group alpha logvar | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_wcl)

intensive_wcr <- rblimp(
  data = intensive,
  clusterid = 'id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta logvar',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    logvar ~ intercept@g0o;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@logvar;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    logvar_res = logvar - g0o;
    m ~ intercept group alpha_res logvar_res | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_wcx)

intensive_wcx <- rblimp(
  data = intensive,
  clusterid = 'id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta logvar',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    logvar ~ intercept@g0o;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@logvar;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    logvar_res = logvar - g0o;
    m ~ intercept group alpha_res logvar_res x.mean | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_wcx)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# diggle-kenward model
intensive_dky <- rblimp(
  data = intensive,
  clusterid = 'id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta logvar',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    logvar ~ intercept@g0o;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@logvar;
    missingness:
    m ~ intercept group y y.lag | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(intensive_dky)

# residual diggle-kenward model
intensive_dkr <- rblimp(
  data = intensive,
  clusterid = 'id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'id = alpha beta logvar',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    logvar ~ intercept@g0o;
    alpha beta logvar ~~ alpha beta logvar;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@logvar;
    missingness:
    m ~ intercept group (y - alpha) (y.lag - alpha) | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 75000,
  iter = 75000)

# print output
output(intensive_dkr)

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

extract_growth_params <- function(object, method) {
  
  tab <- object@estimates
  
  rows <- c(
    "alpha ~ Intercept",
    "beta ~ Intercept",
    "alpha ~ group",
    "beta ~ group",
    "alpha residual variance",
    "beta residual variance",
    "Cor( alpha, beta )",
    "y residual variance",
    "Parameter: diff"
  )
  
  res <- round(tab[rows, c("Estimate", "StdDev"), drop = FALSE],2)
  
  # Rename rows to cleaner presentation labels
  rownames(res) <- c(
    "Intercept (G = 0)",
    "Slope (G = 0)",
    "Intercept Diff.",
    "Slope Diff.",
    "Var(Intercept)",
    "Var(Slope)",
    "Cor(Intercept, Slope)",
    "Var(Residual)",
    "Endpoint Mean Diff."
  )
  
  colnames(res) <- c(
    paste0("Est_", method),
    paste0("SD_", method)
  )
  
  res
}

com_tab <- extract_growth_params(growth_com, "COM")
mar_tab <- extract_growth_params(growth_mar, "MAR")
dum_tab <- extract_growth_params(growth_tdum, "DUM")
wcl_tab <- extract_growth_params(growth_wcl, "WCL")
wcr_tab <- extract_growth_params(growth_wcr, "WCR")
dky_tab <- extract_growth_params(growth_dky, "DKL")
dkr_tab <- extract_growth_params(growth_dkr, "DKR")
dis_tab <- extract_growth_params(growth_dis, "DIS")

cbind(com_tab,mar_tab,dum_tab,wcl_tab,wcr_tab,dky_tab,dkr_tab,dis_tab)




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

#------------------------------------------------------------------------------#
# CURSIO ET AL. MODEL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# intensive_cursio_1pl <- rblimp(
#   data = intensive,
#   clusterid = 'id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'occasion',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'id = alpha beta u0i',
#   fixed = 'occasion group time',
#   model = '
#     level2:
#     alpha ~ intercept group;
#     beta ~ intercept group;
#     alpha ~~ beta;
#     level1:
#     y ~ intercept@alpha time@beta;
#     missingness:
#     u0i ~ intercept;
#     m ~ intercept@u0i occasion;',
#   seed = 90291,
#   burn = 10000,
#   iter = 10000,
#   nimps = 20)
# 
# # print output
# output(intensive_cursio_1pl)
# 
# # cursio 2pl model
# intensive_cursio_2pl <- rblimp(
#   data = intensive,
#   clusterid = 'id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'occasion',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'id = alpha beta u0i',
#   fixed = 'time occasion group',
#   model = '
#     level2:
#     alpha ~ intercept group;
#     beta ~ intercept group;
#     alpha ~~ beta;
#     level1:
#     y ~ intercept@alpha time@beta;
#     missingness:
#     u0i ~ intercept;
#     m ~ intercept@u0i occasion occasion*u0i;',
#   seed = 90291,
#   burn = 50000,
#   iter = 50000,
#   nimps = 20)
# 
# # print output
# output(intensive_cursio_2pl)
