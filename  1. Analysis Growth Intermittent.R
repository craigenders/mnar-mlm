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
filepath1 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-intermittent.csv'

# create data frame from github data
growth_i <- read.csv(filepath1, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

growth_i_com <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    ycom ~ intercept@alpha time@beta;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(ycom.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_com)

#------------------------------------------------------------------------------#
# MAR (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

growth_i_mar <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_mar)

#------------------------------------------------------------------------------#
# ICC FOR THE MISSINGNESS INDICATOR (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_growth_i <- rblimp(
  data = growth_i,
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
output(icc_growth_i)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# linear trend
growth_i_tlin <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept time group time*group | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_tlin)

# quadratic trend
growth_i_tquad <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ 1 group;
    beta ~ 1 group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_tquad)

# dummy coded time
growth_i_tdum <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept group | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_tdum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

ymax <- .35
ymin <- 0

# gro_i_obs <- plot_means(m ~ time | group, 
#            model = growth_i_tdum,
#            ylab = "Probability",
#            title = "A. Observed Probabilities (Growth Data)",
#            group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax)

gro_i_obs <- plot_means(m ~ time | group, 
                      model = growth_i_tdum,
                      ylab = "Probability",
                      title = "A. Observed Probabilities",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
                      theme(legend.position = "top",legend.justification = "center") +
                      scale_linetype_manual(values = c("dotted", "solid")) +
                      geom_line(linewidth = .25)

gro_i_dum <- plot_means(m.1.probability ~ time | group, 
           model = growth_i_tdum,
           ylab = "Probability",
           title = "G. Dummy Coded Time",
           group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

gro_i_lin <- plot_means(m.1.probability ~ time | group, 
                      model = growth_i_tlin,
                      ylab = "Probability",
                      title = "C. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

gro_i_quad <- plot_means(m.1.probability ~ time | group, 
                      model = growth_i_tquad,
                      ylab = "Probability",
                      title = "E. Quadratic Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  geom_line(linewidth = .25)

gro_i_obs; gro_i_dum; gro_i_lin; gro_i_quad

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_growth_i_obs <- aggregate(m ~ time + group, data = growth_i_tdum@average_imp, mean)
pmiss_growth_i_tdum <- aggregate(m.1.probability ~ time + group, data = growth_i_tdum@average_imp, mean)
pmiss_growth_i_tlin <- aggregate(m.1.probability ~ time + group, data = growth_i_tlin@average_imp, mean)
pmiss_growth_i_tquad <- aggregate(m.1.probability ~ time + group, data = growth_i_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_gro_i_tdum <- sqrt(mean((pmiss_growth_i_tdum$m.1.probability - pmiss_growth_i_obs$m)^2))
rmse_gro_i_tlin <- sqrt(mean((pmiss_growth_i_tlin$m.1.probability - pmiss_growth_i_obs$m)^2))
rmse_gro_i_tquad <- sqrt(mean((pmiss_growth_i_tquad$m.1.probability - pmiss_growth_i_obs$m)^2))
rmse_gro_i_tdum; rmse_gro_i_tlin; rmse_gro_i_tquad

# summarize difference between marginal vs. observed probabilities
summary(pmiss_growth_i_tdum$m.1.probability - pmiss_growth_i_obs$m)
summary(pmiss_growth_i_tlin$m.1.probability - pmiss_growth_i_obs$m)
summary(pmiss_growth_i_tquad$m.1.probability - pmiss_growth_i_obs$m)

#------------------------------------------------------------------------------#
# FIGURE 2 ----
#------------------------------------------------------------------------------#

fig2col1 <- gro_i_obs / gro_i_lin / gro_i_quad / gro_i_dum
fig2col2 <- int_i_obs / int_i_lin / int_i_quad / int_i_dum
figure2 <- fig2col1 | fig2col2

ggsave(
  filename = "~/desktop/Figure 2. Time Related (IM).pdf",
  plot = figure2,
  width = 8.5,
  height = 11,
  units = "in"
)

#------------------------------------------------------------------------------#
# WU-CARROLL MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# wu-carroll model using full latent variable
growth_i_wcl <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept group alpha beta | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_wcl)

# residual wu-carroll model
growth_i_wcr <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    beta_res = beta - (g0b + g1b*group);
    m ~ intercept group alpha_res beta_res | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_wcr)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# diggle-kenward model
growth_i_dky <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time > 0, 1, 0);
    m ~ intercept group y y.lag | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_dky)

# diggle-kenward model
growth_i_dkr <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time > 0, 1, 0);
    yhat = alpha + beta*time;
    ylaghat = alpha + beta*(time - 1);
    m ~ intercept group (y - yhat) (y.lag - ylaghat) | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 50000,
  iter = 50000)

# print output
output(growth_i_dkr)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# diggle-kenward model
growth_i_dis <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    beta_res = beta - (g0b + g1b*group);
    d = ifelse(time > 0, 1, 0);
    yhat = alpha + beta*time;
    ylaghat = alpha + beta*(time - 1);
    m ~ intercept group alpha_res beta_res (y - yhat) (y.lag - ylaghat) | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 50000,
  iter = 50000)

# print output
output(growth_i_dis)

#------------------------------------------------------------------------------#
# PLOT GROWTH CURVES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

p_gro_i_mar <- plot_means(y.predicted ~ time | group,
                        model = growth_i_mar,
                        ylab = "Y",
                        title = "MAR Model-Implied Means (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

p_gro_i_dum <- plot_means(y.predicted ~ time | group, 
                        model = growth_i_tdum,
                        ylab = "Y",
                        title = "Time Dummy Model-Implied Means (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

p_gro_i_wcl <- plot_means(y.predicted ~ time | group, 
                        model = growth_i_wcl,
                        ylab = "Y",
                        title = "W-C Model-Implied Means (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

p_gro_i_wcr <- plot_means(y.predicted ~ time | group, 
                        model = growth_i_wcr,
                        ylab = "Y",
                        title = "Res W-C Model-Implied Means (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

p_gro_i_dky <- plot_means(y.predicted ~ time | group, 
                        model = growth_i_dky,
                        ylab = "Y",
                        title = "W-C Model-Implied Means (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

p_gro_i_dkr <- plot_means(y.predicted ~ time | group, 
                        model = growth_i_dkr,
                        ylab = "Y",
                        title = "Res W-C Model-Implied Means (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

p_gro_i_mar; p_gro_i_dum; p_gro_i_wcl; p_gro_i_wcr; p_gro_i_dky; p_gro_i_dkr

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

extract_growth_i_params <- function(object, method) {
  
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
    "Parameter: diff",
    "Parameter: d_diff"
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
    "Endpoint Mean Diff.",
    "Std. Mean Diff."
  )
  
  colnames(res) <- c(
    paste0("Est_", method),
    paste0("SD_", method)
  )
  
  res
}

com_tab <- extract_growth_i_params(growth_i_com, "COM")
mar_tab <- extract_growth_i_params(growth_i_mar, "MAR")
dum_tab <- extract_growth_i_params(growth_i_tdum, "DUM")
wcl_tab <- extract_growth_i_params(growth_i_wcl, "WCL")
wcr_tab <- extract_growth_i_params(growth_i_wcr, "WCR")
dky_tab <- extract_growth_i_params(growth_i_dky, "DKL")
dkr_tab <- extract_growth_i_params(growth_i_dkr, "DKR")
dis_tab <- extract_growth_i_params(growth_i_dis, "DIS")

cbind(com_tab,mar_tab,dum_tab,wcl_tab,wcr_tab,dky_tab,dkr_tab,dis_tab)


#------------------------------------------------------------------------------#
# CURSIO ET AL. MODEL (LONGITUDINAL GROWTH) ----
#------------------------------------------------------------------------------#

# cursio_1pl <- rblimp(
#   data = growth_i,
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
#     alpha ~ 1 group;
#     beta ~ 1 group;
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
# output(cursio_1pl)
# 
# # cursio 2pl model
# cursio_2pl <- rblimp(
#   data = growth_i,
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
#     alpha ~ 1 group;
#     beta ~ 1 group;
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
# output(cursio_2pl)
# 
# # cursio 2pl model
# growth_i_cursio_2pl_cent <- rblimp(
#   data = growth_i,
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
#     alpha ~ 1 group;
#     beta ~ 1 group;
#     alpha ~~ beta;
#     level1:
#     y ~ intercept@alpha time@beta;
#     missingness:
#     u0i ~ intercept@0;
#     m ~ intercept occasion u0i@1 occasion*u0i | intercept@0;',
#   seed = 90291,
#   burn = 50000,
#   iter = 50000,
#   nimps = 20)
# 
# # print output
# output(growth_i_cursio_2pl_cent)
