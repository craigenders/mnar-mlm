#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options (scipen = 999)

# load packages
library(ggplot2)
library(patchwork)
library(rblimp)

# set_blimp('/applications/blimp/blimp')
# set_blimp('/applications/blimp/blimp-nightly')
# remotes::update_packages('rblimp')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath1 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-dropout.csv'

# create data frame from github data
growth_d <- read.csv(filepath1, stringsAsFactors = T)

growth_comp <- growth_d
growth_d <- growth_d[!is.na(growth_d$m),]

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA ----
#------------------------------------------------------------------------------#

growth_d_com <- rblimp(
  data = growth_comp,
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
output(growth_d_com)

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# Model 1: CMAR ----
growth_d_mar <- rblimp(
  data = growth_d,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta',
  ordinal = 'group',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    m ~ intercept | 1@0',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_d_mar)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES ----
#------------------------------------------------------------------------------#

# linear trend
growth_d_tlin <- rblimp(
  data = growth_d,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept@-3 time@b1 time*group@b2 | intercept@0;',
  parameters = '
    # group 0
    p0g0 = phi(-3 + b1*0 + b2*0*0);  # time=0, group=0
    p1g0 = phi(-3 + b1*1 + b2*1*0);  # time=1, group=0
    p2g0 = phi(-3 + b1*2 + b2*2*0);  # time=2, group=0
    p3g0 = phi(-3 + b1*3 + b2*3*0);  # time=3, group=0
    p4g0 = phi(-3 + b1*4 + b2*4*0);  # time=4, group=0
    # group 1
    p0g1 = phi(-3 + b1*0 + b2*0*1);  # time=0, group=1
    p1g1 = phi(-3 + b1*1 + b2*1*1);  # time=1, group=1
    p2g1 = phi(-3 + b1*2 + b2*2*1);  # time=2, group=1
    p3g1 = phi(-3 + b1*3 + b2*3*1);  # time=3, group=1
    p4g1 = phi(-3 + b1*4 + b2*4*1);  # time=4, group=1
  ',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_d_tlin)

# quadratic trend
growth_d_tquad <- rblimp(
  data = growth_d,
  clusterid = 'l2id', 
  # dropout = 'm = y (missing)',
  ordinal = 'm',
  # timeid = 'time',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept@-3 time time^2 time*group time^2*group | intercept@0;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_d_tquad)

# dummy coded time
growth_d_tdum <- rblimp(
  data = growth_d,
  clusterid = 'l2id', 
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (binary)',
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
    m ~ intercept@-3 | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_d_tdum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES ----
#------------------------------------------------------------------------------#

ymax <- .5
ymin <- 0

gro_d_obs <- plot_means(m ~ time | group, 
                      model = growth_d_tdum,
                      ylab = "Probability",
                      title = "A. Observed Probabilities",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_d_dum <- plot_means(m.1.probability ~ time | group, 
           model = growth_d_tdum,
           ylab = "Probability",
           title = "G. Dummy Coded Time",
           group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_d_lin <- plot_means(m.1.probability ~ time | group, 
                      model = growth_d_tlin,
                      ylab = "Probability",
                      title = "C. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_d_quad <- plot_means(m.1.probability ~ time | group, 
                      model = growth_d_tquad,
                      ylab = "Probability",
                      title = "E. Quadratic Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_d_obs; gro_d_dum; gro_d_lin; gro_d_quad

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_growth_d_obs <- aggregate(m ~ time + group, data = growth_d_tdum@average_imp, mean)
pmiss_growth_d_tdum <- aggregate(m.1.probability ~ time + group, data = growth_d_tdum@average_imp, mean)
pmiss_growth_d_tlin <- aggregate(m.1.probability ~ time + group, data = growth_d_tlin@average_imp, mean)
pmiss_growth_d_tquad <- aggregate(m.1.probability ~ time + group, data = growth_d_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_gro_d_tdum <- sqrt(mean((pmiss_growth_d_tdum$m.1.probability - pmiss_growth_d_obs$m)^2))
rmse_gro_d_tlin <- sqrt(mean((pmiss_growth_d_tlin$m.1.probability - pmiss_growth_d_obs$m)^2))
rmse_gro_d_tquad <- sqrt(mean((pmiss_growth_d_tquad$m.1.probability - pmiss_growth_d_obs$m)^2))
rmse_gro_d_tdum; rmse_gro_d_tlin; rmse_gro_d_tquad

# summarize difference between marginal vs. observed probabilities
summary(pmiss_growth_d_tdum$m.1.probability - pmiss_growth_d_obs$m)
summary(pmiss_growth_d_tlin$m.1.probability - pmiss_growth_d_obs$m)
summary(pmiss_growth_d_tquad$m.1.probability - pmiss_growth_d_obs$m)

#------------------------------------------------------------------------------#
# FIGURE 4 ----
#------------------------------------------------------------------------------#

# fig4col1 <- gro_d_obs / gro_d_lin / gro_d_quad / gro_d_dum
# fig4col2 <- int_d_obs / int_d_lin / int_d_quad / int_d_dum
# figure4 <- fig4col1 | fig4col2
# 
# ggsave(
#   filename = "~/desktop/Figure 4. Time Related (DO).pdf",
#   plot = figure4,
#   width = 8.5,
#   height = 11,
#   units = "in"
# )

#------------------------------------------------------------------------------#
# SHARED PARAMETER MODEL ----
#------------------------------------------------------------------------------#

# Model 2: Shared Parameter Model ----
growth_d_wc <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  # timeid = 'time',
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
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 alpha*d beta*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_d_wc)

# Model 3: Quadratic Shared Parameter Model ----
growth_d_wcq <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  # timeid = 'time',
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
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 alpha*d alpha*d^2 beta*d beta*d^2 | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 100000,
  iter = 100000)

# print output
output(growth_d_wcq)

# Model 4: Residualized Shared Parameter Model ----
growth_d_wcr <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  # timeid = 'time',
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
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 alpha*d_res beta*d_res | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 100000,
  iter = 100000)

# print output
output(growth_d_wcr)

#------------------------------------------------------------------------------#
# SELECTION MODEL ----
#------------------------------------------------------------------------------#

# Model 5: Diggle-Kenward Model ----
growth_d_dk <- rblimp(
  data = growth_d,
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
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 y*d y.lag*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_d_dk)

# Model 6: Quadratic Diggle-Kenward Model ----
growth_d_dkq <- rblimp(
  data = growth_d,
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
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 y*d y^2*d y.lag*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 40000,
  iter = 40000)

# print output
output(growth_d_dkq)

# Model 7: Residual Diggle-Kenward Model ----
growth_d_dkd <- rblimp(
  data = growth_d,
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
    d = ifelse(time < 1, 0, 1);
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept@-3 y*dw d*lagyw | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 200000,
  iter = 200000)

# print output
output(growth_d_dkd)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# Model 8: Disaggregated Model ----
growth_d_dis <- rblimp(
  data = growth_d,
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
    d = ifelse(time < 1, 0, 1);
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept@-3 y*dw d*lagyw alpha*d beta*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 100000,
  iter = 100000)

# print output
output(growth_d_dis)

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES ----
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
    "Parameter: diff",
    "Parameter: d_diff",
    "m R2: Coefficients"
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
    "Mean Diff.",
    "Std. Mean Diff.",
    "Pseudo-Rsq"
  )
  
  colnames(res) <- c(
    paste0("Est_", method),
    paste0("SD_", method)
  )
  
  res
}

# main summary table ----

# com_tab <- extract_growth_d_params(growth_d_com, "COM")
mar_tab <- extract_growth_params(growth_d_mar, "MAR")
wc_tab  <- extract_growth_params(growth_d_wc, "WC")
wcq_tab <- extract_growth_params(growth_d_wcq, "WCQ")
wcr_tab <- extract_growth_params(growth_d_wcr, "WCR")
dk_tab  <- extract_growth_params(growth_d_dk, "DK")
dkq_tab <- extract_growth_params(growth_d_dkq, "DKQ")
dkd_tab <- extract_growth_params(growth_d_dkd, "DKD")
dis_tab <- extract_growth_params(growth_d_dis, "DIS")

tab <- cbind(mar_tab,wc_tab,wcq_tab,wcr_tab,dk_tab,dkq_tab,dkd_tab,dis_tab)
tab_growth_do <- tab

# write.csv(tab_growth_do,file = '~/desktop/MNAR Results/tab_growth_do.csv')

# mean difference table ----

# rearrange for table

# Extract rows
mean_row   <- tab["Mean Diff.", ]
std_row    <- tab["Std. Mean Diff.", ]
pseudo_row <- tab["Pseudo-Rsq", ]

# Get method names
col_names <- colnames(tab)
methods <- unique(sub("^(Est|SD)_", "", col_names))

# Build output table
out <- do.call(rbind, lapply(methods, function(m) {
  c(
    Mean_Diff      = mean_row[paste0("Est_", m)],
    SD_Mean_Diff   = mean_row[paste0("SD_", m)],
    Std_Mean_Diff  = std_row[paste0("Est_", m)],
    SD_Std_Mean    = std_row[paste0("SD_", m)],
    Pseudo_Rsq     = pseudo_row[paste0("Est_", m)]
  )
}))

# Final formatting
rownames(out) <- methods
out <- as.data.frame(out)
colnames(out) <- c("Mean_Diff", "SD", "Std_Mean_Diff", "SD", "Pseudo_R²")

out

es_growth_do <- out
# write.csv(es_growth_do,file = '~/desktop/MNAR Results/es_growth_do.csv')

# diagnostics table ----

extract_convergence <- function(object, method) {
  
  neff <- object@estimates[, "N_Eff"]
  neff <- neff[is.finite(neff)]
  
  psr_row <- as.numeric(object@psr[20, ])
  psr_row <- psr_row[is.finite(psr_row)]
  
  data.frame(
    Min_Neff = round(min(neff),    0),
    Max_Neff = round(max(neff),    0),
    Min_PSR  = round(min(psr_row), 3),
    Max_PSR  = round(max(psr_row), 3),
    row.names = method
  )
}

conv_growth_do <- rbind(
    extract_convergence(growth_d_mar, "MAR"),
    extract_convergence(growth_d_wc, "WC"),
    extract_convergence(growth_d_wcq, "WCQ"),
    extract_convergence(growth_d_wcr, "WCR"),
    extract_convergence(growth_d_dk, "DK"),
    extract_convergence(growth_d_dkq, "DKQ"),
    extract_convergence(growth_d_dkd, "DKD"),
    extract_convergence(growth_d_dis, "DIS"),
  )

# write.csv(conv_growth_do,file = '~/desktop/MNAR Results/conv_growth_do.csv')




#------------------------------------------------------------------------------#
# PLOT GROWTH CURVES ----
#------------------------------------------------------------------------------#

# p_gro_d_mar <- plot_means(y.predicted ~ time | group,
#                       model = growth_d_mar,
#                       ylab = "Y",
#                       title = "MAR Model-Implied Means (Growth Data)",
#                       group_labels = c("0" = "0", "1" = "1"),
#                       use_latent_growth = TRUE) + ylim(0, 7)

# p_gro_d_dum <- plot_means(y.predicted ~ time | group, 
#                       model = growth_d_tdum,
#                       ylab = "Y",
#                       title = "Time Dummy Model-Implied Means (Growth Data)",
#                       group_labels = c("0" = "0", "1" = "1"),
#                       use_latent_growth = TRUE) + ylim(0, 7)
# 
# p_gro_d_wcl <- plot_means(y.predicted ~ time | group, 
#                         model = growth_d_wcl,
#                         ylab = "Y",
#                         title = "W-C Model-Implied Means (Growth Data)",
#                         group_labels = c("0" = "0", "1" = "1"),
#                         use_latent_growth = TRUE) + ylim(0, 7)
# 
# p_gro_d_wcr <- plot_means(y.predicted ~ time | group, 
#                      model = growth_d_wcr,
#                      ylab = "Y",
#                      title = "Res W-C Model-Implied Means (Growth Data)",
#                      group_labels = c("0" = "0", "1" = "1"),
#                      use_latent_growth = TRUE) + ylim(0, 7)
# 
# p_gro_d_dky <- plot_means(y.predicted ~ time | group, 
#                         model = growth_d_dky,
#                         ylab = "Y",
#                         title = "W-C Model-Implied Means (Growth Data)",
#                         group_labels = c("0" = "0", "1" = "1"),
#                         use_latent_growth = TRUE) + ylim(0, 7)
# 
# p_gro_d_dkr <- plot_means(y.predicted ~ time | group, 
#                         model = growth_d_dkr,
#                         ylab = "Y",
#                         title = "Res W-C Model-Implied Means (Growth Data)",
#                         group_labels = c("0" = "0", "1" = "1"),
#                         use_latent_growth = TRUE) + ylim(0, 7)
# 
# p_gro_d_mar; p_gro_d_dum; p_gro_d_wcl; p_gro_d_wcr; p_gro_d_dky; p_gro_d_dkr
# 



