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
growth_i_norows <- growth_i[!is.na(growth_i$y),]
names(growth_i_norows)[names(growth_i_norows) == "m"] <- "m_"

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA ----
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
    av = alpha.totalvar;
    yv = ycom.totalvar;
    sdy = sqrt(ycom.totalvar + alpha.totalvar);
    d_diff = diff / sqrt(ycom.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_com)

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# Model 1: CMAR ----
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
    y ~ intercept@alpha time@beta;
    m ~ intercept',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(growth_i_mar)

#------------------------------------------------------------------------------#
# ICC FOR THE MISSINGNESS INDICATOR ----
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
# TIME-RELATED CHANGES ----
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
# PLOT MISSINGNESS PROBABILITIES ----
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
                      scale_linetype_manual(values = c("dashed", "solid")) +
                      geom_line(linewidth = .25)

gro_i_dum <- plot_means(m.1.probability ~ time | group, 
           model = growth_i_tdum,
           ylab = "Probability",
           title = "G. Dummy Coded Time",
           group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_i_lin <- plot_means(m.1.probability ~ time | group, 
                      model = growth_i_tlin,
                      ylab = "Probability",
                      title = "C. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_i_quad <- plot_means(m.1.probability ~ time | group, 
                      model = growth_i_tquad,
                      ylab = "Probability",
                      title = "E. Quadratic Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
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
# FIGURE 3 ----
#------------------------------------------------------------------------------#

fig3col1 <- gro_i_obs / gro_i_lin / gro_i_quad / gro_i_dum
fig3col2 <- int_i_obs / int_i_lin / int_i_quad / int_i_dum
figure3 <- fig3col1 | fig3col2

ggsave(
  filename = "~/desktop/Figure 3. Time Related (IM).pdf",
  plot = figure3,
  width = 8.5,
  height = 11,
  units = "in"
)

#------------------------------------------------------------------------------#
# SHARED PARAMETER MODEL ----
#------------------------------------------------------------------------------#

set_blimp('/applications/blimp/blimp')
# Model 2: Shared Parameter Model ----
growth_i_wc <- rblimp(
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
output(growth_i_wc)

set_blimp('/applications/blimp/blimp-nightly')
# Model 2: Shared Parameter Model TEST ----
growth_i_wc_nr <- rblimp(
  data = growth_i_norows,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
output(growth_i_wc_nr)

test <- growth_i_wc_nr@average_imp

# Model 3: Quadratic Shared Parameter Model ----
growth_i_wcq <- rblimp(
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
    m ~ intercept group alpha alpha^2 beta beta^2 | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_wcq)

# Model 4: Residualized Shared Parameter Model ----
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
# SELECTION MODEL ----
#------------------------------------------------------------------------------#

# Model 5: Diggle-Kenward Model ----
growth_i_dk <- rblimp(
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
    m ~ intercept group y y.lag | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_dk)

# Model 5: Diggle-Kenward Model Prior Removed (Dummy Switch) ----
growth_i_dk_noprior <- rblimp(
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
    y_lag = ifelse(time == 0, 0, y.lag);
    m ~ intercept group y y_lag | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_dk_noprior)

# Model 6: Quadratic Diggle-Kenward Model ----
growth_i_dkq <- rblimp(
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
    m ~ intercept group y y^2 y.lag | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(growth_i_dkq)

# Model 7: Residual Diggle-Kenward Model ----
growth_i_dkd <- rblimp(
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
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept group yw lagyw | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 200000,
  iter = 200000)

# print output
output(growth_i_dkd)

#------------------------------------------------------------------------------#
# PLOT DK PROBABILITIES ----
#------------------------------------------------------------------------------#

ymin = 0
ymax = .25

gro_i_obs <- plot_means(m ~ time | group, 
                        model = growth_i_tdum,
                        ylab = "Probability",
                        title = "A. Observed Probabilities",
                        group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_i_dk <- plot_means(m.1.probability ~ time | group, 
                               model = growth_i_dk,
                               ylab = "Probability",
                               title = "B. Default Prior",
                               group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

gro_i_dk_noprior <- plot_means(m.1.probability ~ time | group, 
                        model = growth_i_dk_noprior,
                        ylab = "Probability",
                        title = "C. Prior Removed",
                        group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

fig_dk_probs <- gro_i_obs / gro_i_dk / gro_i_dk_noprior
fig_dk_probs

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# Model 8: Disaggregated Model ----
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
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept group yw lagyw alpha beta | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  burn = 200000,
  iter = 200000)

# print output
output(growth_i_dis)

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

# com_tab <- extract_growth_d_params(growth_d_com, "COM")
mar_tab <- extract_growth_params(growth_i_mar, "MAR")
wc_tab <- extract_growth_params(growth_i_wc, "WC")
wcq_tab <- extract_growth_params(growth_i_wcq, "WCQ")
wcr_tab <- extract_growth_params(growth_i_wcr, "WCR")
dk_tab <- extract_growth_params(growth_i_dk, "DK")
dkq_tab <- extract_growth_params(growth_i_dkq, "DKQ")
dkd_tab <- extract_growth_params(growth_i_dkd, "DKD")
dis_tab <- extract_growth_params(growth_i_dis, "DIS")

tab <- cbind(mar_tab,wc_tab,wcq_tab,wcr_tab,dk_tab,dkq_tab,dkd_tab,dis_tab)
tab_growth_im <- tab

write.csv(tab_growth_im,file = '~/desktop/tab_growth_im.csv')

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
colnames(out) <- c("Mean Diff", "SD", "Std. Mean Diff", "SD", "Pseudo R²")

out



#------------------------------------------------------------------------------#
# PLOT REGRESSION LINES ----
#------------------------------------------------------------------------------#

means_gro_i_mar <- plot_means(y.predicted ~ time | group,
                        model = growth_i_mar,
                        ylab = "Y",
                        title = "A. Group-Specifc Trajectories (Growth Data)",
                        group_labels = c("0" = "0", "1" = "1"),
                        use_latent_growth = TRUE) + ylim(0, 7)

means_int_i_mar <- plot_means(y.predicted ~ xcent | group,
                          model = intensive_i_mar,
                          ylab = "Y",
                          title = "B. Group-Specifc Regressions (Intensive Data)",
                          group_labels = c("0" = "0", "1" = "1"),
                          use_latent_growth = T) + ylim(3, 5)

#------------------------------------------------------------------------------#
# FIGURE 1 ----
#------------------------------------------------------------------------------#

figure1 <- means_gro_i_mar / means_int_i_mar

ggsave(
  filename = "~/desktop/Figure 1. CMAR.pdf",
  plot = figure1,
  width = 8.5,
  height = 11,
  units = "in"
)

#------------------------------------------------------------------------------#
# PLOT GROWTH CURVES OLD ----
#------------------------------------------------------------------------------#

p_gro_i_mar <- plot_means(y.predicted ~ time | group,
                          model = growth_i_mar,
                          ylab = "Y",
                          title = "A. Group-Specifc Trajectories (Growth Data)",
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
# EXTRACT ESTIMATES ----
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
# CURSIO ET AL. MODEL ----
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
