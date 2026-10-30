#------------------------------------------------------------------------------#
# IRT MISSINGNESS MODELS (CURSIO ET AL., 2019) ----
#------------------------------------------------------------------------------#
#
# This script fits dummy-coded missingness models and their one- and
# two-parameter IRT equivalents to the T = 20 intensive longitudinal data with
# intermittent missingness. Only the missingness model is specified; there is
# no focal (substantive) model. Group effects are omitted so that the
# dummy-coded model is fully equivalent to a 1PL IRT model.
#
# Model A: dummy-coded missingness model with a random intercept (MLM form)
# Model B: 1PL IRT model with a standardized latent propensity (IRT form)
#          -> equivalent to Model A; Var(random intercept) = (common loading)^2
# Model C: 2PL IRT model with a standardized latent propensity and
#          occasion-specific loadings (IRT form); cross-level interactions
#          between the latent propensity and the occasion dummy codes give
#          the occasion-specific loadings
#
# To obtain the multiple-group versions described in the paper (metric
# invariance with scalar noninvariance), add group and (time == [t])*group
# to the right-hand side of each m equation.
#

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options(scipen = 999)

# load packages
library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath <- 'https://raw.githubusercontent.com/blimp-stats/mnar-mlm/main/data/intensive-intermittent.csv'

# create data frame from github data
intensive_intermittent <- read.csv(filepath, stringsAsFactors = T)

# number of occasions (time is coded 0 to T - 1)
T <- length(unique(intensive_intermittent$time))

#------------------------------------------------------------------------------#
# MODEL A: DUMMY-CODED MISSINGNESS MODEL (MLM FORM) ----
#------------------------------------------------------------------------------#
#
# Occasion-specific intercepts (occasion 0 is the reference) and a random
# intercept with a common (unit) loading at every occasion.
#
model_dummy <- rblimp(
  data = intensive_intermittent,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
  fixed = 'time',
  model = '
    m ~ intercept | intercept;
    { t in 1:19 } : m ~ (time == [t]);',
  seed = 90291,
  burn = 25000,
  iter = 25000)

# print output
output(model_dummy)

#------------------------------------------------------------------------------#
# MODEL B: 1PL IRT MODEL (IRT FORM) ----
#------------------------------------------------------------------------------#
#
# A standardized latent propensity u (mean 0, variance 1) replaces the random
# intercept. A single common loading (discrimination) is estimated for u, and
# the occasion dummy codes give occasion-specific intercepts (difficulties).
# The default random intercept for m is fixed to 0 because u is the only
# source of between-person variation. Equivalence with Model A:
# Var(random intercept) = loading^2, and the fixed intercepts are identical.
#
model_1pl <- rblimp(
  data = intensive_intermittent,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
  latent = 'l2id = u',
  fixed = 'time',
  model = '
    structural:
    u ~ intercept@0;
    u ~~ u@1;
    measurement:
    m ~ intercept u | intercept@0;
    { t in 1:19 } : m ~ (time == [t]);',
  seed = 90291,
  burn = 25000,
  iter = 25000)

# print output
output(model_1pl)

#------------------------------------------------------------------------------#
# MODEL C: 2PL IRT MODEL (IRT FORM) ----
#------------------------------------------------------------------------------#
#
# The latent propensity u is standardized (mean 0, variance 1) and every
# occasion gets its own loading (discrimination) and intercept (difficulty).
# The loading at occasion 0 is the main effect of u; the loading at occasion
# t is the main effect plus the interaction coefficient. Model B is nested
# within this model (all interaction coefficients equal to 0).
#
model_2pl <- rblimp(
  data = intensive_intermittent,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
  latent = 'l2id = u',
  fixed = 'time',
  model = '
    structural:
    u ~ intercept@0;
    u ~~ u@1;
    measurement:
    m ~ intercept@0 (time == 0) (time == 0)*u@lo0prior | intercept@0;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*u;',
  parameters = 'lo0prior ~ trunc(0,inf)',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  options = 'pinfo')

# print output
output(model_2pl)

trace_plot(model_2pl)

#------------------------------------------------------------------------------#
# COMPARE MODEL FIT ----
#------------------------------------------------------------------------------#
#
# Equivalent parameterizations (A vs. B) should produce the same DIC and WAIC
# within MCMC error. The 1PL vs. 2PL comparison tests whether
# occasion-specific loadings are needed.
#
# (uncomment once the slot names are confirmed for the installed rblimp)
# rbind(
#   A_dummy   = model_dummy@fit,
#   B_1pl     = model_1pl@fit,
#   C_2pl     = model_2pl@fit
# )
