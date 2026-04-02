#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options (scipen = 999)

# load packages
library(ggplot2)
library(patchwork)
library(rblimp)
set_blimp('/applications/blimp/blimp')
# set_blimp('/applications/blimp/blimp-nightly')
# remotes::update_packages('rblimp')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-intermittent.csv'

# create data frame from github data
intensive_i <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_i_com <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta omega',
  fixed = 'group',
  center = 'groupmean = xcom',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    omega ~ intercept;
    alpha beta omega ~~ alpha beta omega;
    level1:
    ycom ~ intercept@alpha xcom@beta;
    var(ycom) ~ intercept@omega;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_i_com)

#------------------------------------------------------------------------------#
# MAR (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_i_mar <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta omega',
  fixed = 'group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    omega ~ intercept;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_i_mar)

#------------------------------------------------------------------------------#
# ICC FOR THE MISSINGNESS INDICATOR (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_intensive_i <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(icc_intensive_i)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# linear trend
intensive_i_tlin <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
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
output(intensive_i_tlin)

# quadratic trend
intensive_i_tquad <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
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
output(intensive_i_tquad)

# dummy coded time
intensive_i_tdum <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
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
output(intensive_i_tdum)

# dummy coded time
intensive_i_tdumr <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
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
output(intensive_i_tdumr)


#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .35
ymin <- 0

# int_i_obs <- plot_means(m ~ time | group, 
#                       model = intensive_i_tdum,
#                       ylab = "Probability",
#                       title = "A. Observed Probabilities (Growth Data)",
#                       group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21)

int_i_obs <- plot_means(m ~ time | group, 
                         model = intensive_i_tdum,
                         ylab = "Probability",
                         title = "B. Observed Probabilities",
                         group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_i_dumr <- plot_means(m.1.probability ~ time | group,
                      model = intensive_i_tdumr,
                      ylab = "Probability",
                      title = "Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_i_dum <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_i_tdum,
                      ylab = "Probability",
                      title = "H. Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_i_lin <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_i_tlin,
                      ylab = "Probability",
                      title = "D. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_i_quad <- plot_means(m.1.probability ~ time | group, 
                       model = intensive_i_tquad,
                       ylab = "Probability",
                       title = "F. Quadratic Time",
                       group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_i_obs; int_i_dum; int_i_lin; int_i_quad

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_i_obs <- aggregate(m ~ time + group, data = intensive_i_tdum@average_imp, mean)
pmiss_intensive_i_tlin <- aggregate(m.1.probability ~ time + group, data = intensive_i_tlin@average_imp, mean)
pmiss_intensive_i_tquad <- aggregate(m.1.probability ~ time + group, data = intensive_i_tquad@average_imp, mean)
pmiss_intensive_i_tdumr <- aggregate(m.1.probability ~ time + group, data = intensive_i_tdumr@average_imp, mean)
pmiss_intensive_i_tdum <- aggregate(m.1.probability ~ time + group, data = intensive_i_tdum@average_imp, mean)


# compute rmse of marginal vs. observed probabilities
rmse_int_i_tlin <- sqrt(mean((pmiss_intensive_i_tlin$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tquad <- sqrt(mean((pmiss_intensive_i_tquad$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tdumr <- sqrt(mean((pmiss_intensive_i_tdumr$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tdum <- sqrt(mean((pmiss_intensive_i_tdum$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tlin; rmse_int_i_tquad; rmse_int_i_tdumr; rmse_int_i_tdum

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_i_tlin$m.1.probability - pmiss_intensive_i_obs$m)
summary(pmiss_intensive_i_tquad$m.1.probability - pmiss_intensive_i_obs$m)
summary(pmiss_intensive_i_tdumr$m.1.probability - pmiss_intensive_i_obs$m)
summary(pmiss_intensive_i_tdum$m.1.probability - pmiss_intensive_i_obs$m)

#------------------------------------------------------------------------------#
# WU-CARROLL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_i_wcl <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    m ~ intercept group alpha omega | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_i_wcl)

intensive_i_wcr <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    omega_res = omega - g0o;
    m ~ intercept group alpha_res omega_res | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_i_wcx)

intensive_i_wcx <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    omega_res = omega - g0o;
    m ~ intercept group alpha_res omega_res x.mean | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_i_wcx)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# diggle-kenward model
intensive_i_dky <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    m ~ intercept group y y.lag | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(intensive_i_dky)

# residual diggle-kenward model
intensive_i_dkr <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    m ~ intercept group (y - alpha) (y.lag - alpha) | intercept;
    { t in 1:24 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291,
  burn = 75000,
  iter = 75000)

# print output
output(intensive_i_dkr)

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
# FIGURE 3 ----
#------------------------------------------------------------------------------#

figure3 <- int_i_obs / int_i_lin / int_i_quad / int_i_dum

ggsave(
  filename = "~/desktop/Figure 3. Time Related (Intensive IM).pdf",
  plot = figure3,
  width = 8.5,
  height = 11,
  units = "in"
)



#------------------------------------------------------------------------------#
# CURSIO ET AL. MODEL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# intensive_i_cursio_1pl <- rblimp(
#   data = intensive_i,
#   clusterid = 'l2id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'occasion',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'l2id = alpha beta u0i',
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
# output(intensive_i_cursio_1pl)
# 
# # cursio 2pl model
# intensive_i_cursio_2pl <- rblimp(
#   data = intensive_i,
#   clusterid = 'l2id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'occasion',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'l2id = alpha beta u0i',
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
# output(intensive_i_cursio_2pl)
