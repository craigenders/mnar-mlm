# MULTILEVEL MODEL WITH HETEROGENEOUS WITHIN-CLUSTER VARIATION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MODEL ----
#------------------------------------------------------------------------------#

# location-scale model
modhet <- rblimp(
  data = diary,
  clusterid = 'person',
  transform = 'strcat = stress >= 2.20',
  latent = 'person = b0i b1i logvi;',
  center = 'groupmean = pain;',
  model = '
    level2:
    b0i ~ intercept strcat;
    b1i ~ intercept strcat;
    b0i ~~ b1i;
    level1:
    posaff ~ intercept@b0i pain@b1i;
    scale:
    logvi ~ intercept;
    logvi ~~ b0i b1i;
    var(posaff) ~ intercept@logvi;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(modhet)

modlat <- rblimp(
  data = diary,
  clusterid = 'person',
  latent = 'person = b0i b1i;',
  center = 'groupmean = pain; grandmean = stress',
  model = '
    level2:
    b0i ~ intercept stress;
    b1i ~ intercept stress;
    b0i ~~ b1i;
    level1:
    posaff ~ intercept@b0i pain@b1i;',
  parameters = '
    b0i_cv = b0i.coefvar;
    b0i_iv = b0i.iceptvar;
    b0i_sv = b0i.slopevar;
    b0i_rv = b0i.residvar;
    b0i_tv = b0i.totalvar;
    b1i_cv = b1i.coefvar;
    b1i_iv = b1i.iceptvar;
    b1i_sv = b1i.slopevar;
    b1i_rv = b1i.residvar;
    b1i_tv = b1i.totalvar;
    posaff_cv = posaff.coefvar;
    posaff_iv = posaff.iceptvar;
    posaff_sv = posaff.slopevar;
    posaff_rv = posaff.residvar;
    posaff_tv = posaff.totalvar;
    ',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

output(modlat)

modmix <- rblimp(
  data = diary,
  clusterid = 'person',
  center = 'groupmean = pain; grandmean = stress',
  model = '
    posaff ~ intercept pain stress pain*stress | pain',
  parameters = '
    posaff_cv = posaff.coefvar; 
    posaff_iv = posaff.iceptvar;
    posaff_sv = posaff.slopevar;
    posaff_rv = posaff.residvar;
    posaff_tv = posaff.totalvar;
    rsqare_cv = posaff.coefvar / posaff.totalvar;
    rsqare_iv = posaff.iceptvar / posaff.totalvar;
    rsqare_sv = posaff.slopevar / posaff.totalvar;
    rsqare_rv = posaff.residvar / posaff.totalvar;
    rsqare_tv = posaff.totalvar / posaff.totalvar;
    ',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

output(modmix)

