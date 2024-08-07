# regression output
source('procedureintegrity_did.R')
source('callintegrity_did.R')
source('advertperiod_did.R')
source('totalbidderintegrity_did.R')
rm(list = ls(), envir = globalenv())  # Clean up
gc()
# install.packages('stargazer')
dir.create('output', showWarnings = F)
procedureintegrity_did = readRDS('1_procedureintegrity_did.Rds')
callintegrity_did = readRDS('2_callintegrity_did.Rds')
advertperiod_did = readRDS('3_advertperiod_did.Rds')
totalbidderintegrity_did = readRDS('4_totalbidderintegrity_did.Rds')
stargazer::stargazer(
  procedureintegrity_did,
  callintegrity_did,
  # advertperiod_did,
  # totalbidderintegrity_did,
  type = 'text', keep = c('loc:timing', 'timing', 'loc'), digits = 4,
  order = c('loc:timing', 'timing', 'loc'),
  out = 'output/did_1.html',
  covariate.labels = c('Diff-in-diff (full Period)', 'Timing (full Period)', 'Location (full Period)'),
  dep.var.labels = c('Non-open procedure', 'No tender call')
)

stargazer::stargazer(
  # procedureintegrity_did,
  # callintegrity_did,
  advertperiod_did,
  totalbidderintegrity_did,
  type = 'text', keep = c('loc:timing', 'timing', 'loc'), digits = 4,
  order = c('loc:timing', 'timing', 'loc'),
  out = 'output/did_2.html',
  covariate.labels = c('Diff-in-diff (full Period)', 'Timing (full Period)', 'Location (full Period)'),
  dep.var.labels = c('Too-short advertisemnt period', 'Single bidding')
)


procedureintegrity_did_se = readRDS('1_procedureintegrity_did_clustered_se.Rds')
callintegrity_did_se = readRDS('2_callintegrity_did_clustered_se.Rds')
advertperiod_did_se = readRDS('3_advertperiod_did_clustered_se.Rds')
totalbidderintegrity_did_se = readRDS('4_totalbidderintegrity_did_clustered_se.Rds')
stargazer::stargazer(
  procedureintegrity_did_se,
  callintegrity_did_se,
  # advertperiod_did,
  # totalbidderintegrity_did,
  type = 'text', keep = c('loc:timing', 'timing', 'loc'), digits = 4,
  out = 'output/se_1.html',
  order = c('loc:timing', 'timing', 'loc'),
  covariate.labels = c('Diff-in-diff (full Period)', 'Timing (full Period)', 'Location (full Period)'), 
  column.labels = c('Non-open procedure', 'No tender call')
)

stargazer::stargazer(
  # procedureintegrity_did,
  # callintegrity_did,
  advertperiod_did_se,
  totalbidderintegrity_did_se,
  type = 'text', keep = c('loc:timing', 'timing', 'loc'), digits = 4,
  out = 'output/se_2.html',
  order = c('loc:timing', 'timing', 'loc'),
  covariate.labels = c('Diff-in-diff (full Period)', 'Timing (full Period)', 'Location (full Period)'),
  column.labels = c('Too-short advertisemnt period', 'Single bidding')
)

# The matching has the following comment:
# The data contain missing values. CEM will match on them; see the manual for other options.
