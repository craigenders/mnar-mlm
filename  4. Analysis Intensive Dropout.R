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
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-dropout.csv'

# create data frame from github data
intensive_d <- read.csv(filepath, stringsAsFactors = T)

intensive_d_comp <- intensive_d

intensive_d <- intensive_d[!is.na(intensive_d$m),]

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_d_mar <- rblimp(
  data = intensive_d_comp,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta',
  fixed = 'group',
  center = 'groupmean = xcom',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    ycom ~ intercept@alpha xcom@beta;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_d_mar)

#------------------------------------------------------------------------------#
# MAR (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

intensive_d_mar <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta',
  fixed = 'group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha x@beta;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_d_mar)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# linear trend
intensive_d_tlin <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
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
    d = ifelse(time > 4, 1, 0);
    m ~ intercept@-3 d d*group d*(time - 5) d*(time - 5)*group | intercept@0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_d_tlin)

# quadratic trend
intensive_d_tquad <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
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
    d = ifelse(time > 4, 1, 0);
    m ~ intercept@-3 d d*group d*(time - 5) d*(time - 5)^2 d*(time - 5)*group d*(time - 5)^2*group | intercept@0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(intensive_d_tquad)

# dummy coded time
intensive_d_tdum <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
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
    d = ifelse(time > 4, 1, 0);
    m ~ intercept@-3 | intercept@0;
    { t in 1:24 } : m ~ d*(time == [t]) d*(time == [t])*group;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_d_tdum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .20
ymin <- 0

# int_d_obs <- plot_means(m ~ time | group, 
#                       model = intensive_d_tdum,
#                       ylab = "Probability",
#                       title = "B. Observed Probabilities (Intensive Data)",
#                       group_labels = c("0" = "0", "1" = "1")) + ylim(0,.30)

int_d_obs <- plot_means(m ~ time | group, 
                      model = intensive_d_tdum,
                      ylab = "Probability",
                      title = "B. Observed Probabilities",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_d_dum <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_d_tdum,
                      ylab = "Probability",
                      title = "H. Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_d_lin <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_d_tlin,
                      ylab = "Probability",
                      title = "D. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

int_d_quad <- plot_means(m.1.probability ~ time | group, 
                       model = intensive_d_tquad,
                       ylab = "Probability",
                       title = "F. Quadratic Time",
                       group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_d_obs <- aggregate(m ~ time + group, data = intensive_d_tdum@average_imp, mean)
pmiss_intensive_d_tdum <- aggregate(m.1.probability ~ time + group, data = intensive_d_tdum@average_imp, mean)
pmiss_intensive_d_tlin <- aggregate(m.1.probability ~ time + group, data = intensive_d_tlin@average_imp, mean)
pmiss_intensive_d_tquad <- aggregate(m.1.probability ~ time + group, data = intensive_d_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_int_d_tdum <- sqrt(mean((pmiss_intensive_d_tdum$m.1.probability - pmiss_intensive_d_obs$m)^2))
rmse_int_d_tlin <- sqrt(mean((pmiss_intensive_d_tlin$m.1.probability - pmiss_intensive_d_obs$m)^2))
rmse_int_d_tquad <- sqrt(mean((pmiss_intensive_d_tquad$m.1.probability - pmiss_intensive_d_obs$m)^2))
rmse_int_d_tdum; rmse_int_d_tlin; rmse_int_d_tquad

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_d_tdum$m.1.probability - pmiss_intensive_d_obs$m)
summary(pmiss_intensive_d_tlin$m.1.probability - pmiss_intensive_d_obs$m)
summary(pmiss_intensive_d_tquad$m.1.probability - pmiss_intensive_d_obs$m)

#------------------------------------------------------------------------------#
# WU-CARROLL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# residual wu-carroll
intensive_d_wcr <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # timeid = 'time',
  # dropout = 'm = y (binary)',
  ordinal = 'm',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ 1@g0a group@g1a;
    beta ~ 1@g0b group@g1b;
    omega ~ 1;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ 1@alpha x@beta;
    var(y) ~ 1@omega;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    beta_res = beta - (g0b + g1b*group);
    d = ifelse(time > 4, 1, 0);
    m ~ intercept@-3 d*alpha_res d*beta_res | intercept@0;
    { t in 1:24 } : m ~ d*(time == [t]) d*(time == [t])*group;',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(intensive_d_wcr)

# wu-carroll
intensive_d_wcl <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # timeid = 'time',
  # dropout = 'm = y (binary)',
  ordinal = 'm',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ 1@g0a group@g1a;
    beta ~ 1@g0b group@g1b;
    omega ~ 1;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ 1@alpha x@beta;
    var(y) ~ 1@omega;
    missingness:
    d = ifelse(time > 4, 1, 0);
    m ~ intercept@-3 d*alpha d*beta | intercept@0;
    { t in 1:24 } : m ~ d*(time == [t]) d*(time == [t])*group;',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(intensive_d_wcl)


#------------------------------------------------------------------------------#
# FIGURE 5 ----
#------------------------------------------------------------------------------#

figure5 <- gro_obs / int_d_obs

ggsave(
  filename = "~/desktop/Figure 5. Obs Missingness.pdf",
  plot = figure5,
  width = 8.5,
  height = 11,
  units = "in"
)

#------------------------------------------------------------------------------#
# FIGURE 6 AND 7 ----
#------------------------------------------------------------------------------#

figure6 <- gro_obs_f3 / gro_lin / gro_quad / gro_dum
figure7 <- int_d_obs_f4 / int_d_lin / int_d_quad / int_d_dum

ggsave(
  filename = "~/desktop/Figure 6. Time Related (Growth).pdf",
  plot = figure6,
  width = 8.5,
  height = 11,
  units = "in"
)

ggsave(
  filename = "~/desktop/Figure 7. Time Related (Intermittent).pdf",
  plot = figure7,
  width = 8.5,
  height = 11,
  units = "in"
)


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

cbind(com_tab,mar_tab,dum_tab,wcl_tab,wcr_tab,dky_tab,dkr_tab)

#

