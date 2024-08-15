####
#' Author: Ahmed Al-Shaibani
####

# Load Libs
library(magrittr)

library(stargazer)

# regression output ----
source('did_procedureintegrity.R.R')
source('did_callintegrity.R.R')
source('did_advertperiod.R.R')
source('did_totalbidderintegrity.R.R')

source('cont_did_advertintegrity.R')
source('cont_did_totalbidderintegrity.R')

rm(list = ls(), envir = globalenv())  # Clean up
gc()
# install.packages('stargazer')

dir.create('output', showWarnings = F)

# TABLE 9 ----
# Load models
procedureintegrity_did = readRDS('1_procedureintegrity_did.Rds')
callintegrity_did = readRDS('2_callintegrity_did.Rds')
advertperiod_did = readRDS('3_advertperiod_did.Rds')
totalbidderintegrity_did = readRDS('4_totalbidderintegrity_did.Rds')


m = list(
  procedureintegrity_did,
  callintegrity_did,
  advertperiod_did,
  totalbidderintegrity_did
)

# Generate initial regression table in LaTex
c = stargazer::stargazer(
  m,
  header = FALSE,
  keep.stat = c('n', 'adj.rsq'),
  type = 'latex',
  object.names = F,
  table.layout = "=-ld-mcta-s-n-",
  notes.align = 'l',
  keep = c('loc:timing', 'timing', 'loc'),
  digits = 3,
  order = c('loc:timing', 'timing', 'loc'),
  covariate.labels = c(
    'Diff-in-diff (full Period)',
    'Timing (full Period)',
    'Location (full Period)'
  ),
  dep.var.caption = "\\textit{(diff-in-diff (OLS))}",
  dep.var.labels = c(
    '\\makecell[cl]{\\bf Non-open \\\\ \\bf procedure}',
    '\\makecell[cl]{\\bf No tender \\\\ \\bf call}',
    '\\makecell[cl]{\\bf Too-short \\\\ \\bf advertisement \\\\ \\bf period}',
    '\\makecell[cl]{\\bf Single \\\\ \\bf bidding}'
  ),
  model.numbers = F,
  # align = TRUE,
  no.space = TRUE,
  font.size = "small",
  # column.sep.width = "5pt",
  add.lines = list(
    c("\\textit{Control Variables-}", rep("", 4)),
    c("Contract Value (log)", rep("Y", 4)),
    c("Main Sector CPV", rep("Y", 4)),
    c("Contract Month", rep("Y", 4)),
    c("Buyer Type", rep("Y", 4)),
    c("Contract Year", rep("Y", 4))
  )
)
c = gsub('Observations', '\\\\textit{N (total)}', c)
c = gsub('Adjusted R\\$\\^\\{2\\}\\$', '\\\\textit{adj R$^{2}$}', c)
c = gsub(
  '\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcccc\\}',
  '\\\\begin{tabular}{@{\\\\extracolsep{5pt}}lllll}',
  c
)
write(c, 'output/table_9_did.tex')


# TABLE 17 ----
# Cont didreg
cont_did_advertperiod = readRDS('5_cont_did_advertperiod.Rds')
cont_did_totalbidderintegrity = readRDS('6_cont_did_totalbidderintegrity.Rds')

m = list(cont_did_advertperiod,
         cont_did_totalbidderintegrity)

# Generate initial regression table in LaTex
c = stargazer::stargazer(
  m,
  header = FALSE,
  keep.stat = c('n', 'adj.rsq'),
  type = 'latex',
  object.names = F,
  table.layout = "=-ld-mcta-s-n-",
  notes.align = 'l',
  keep = c('loc:timing', 'timing', 'loc'),
  digits = 3,
  order = c('loc:timing', 'timing', 'loc'),
  covariate.labels = c(
    'Diff-in-diff (full Period)                         ',
    'Timing (full Period)',
    'Location (full Period)'
  ),
  dep.var.caption = "\\textit{(diff-in-diff (OLS))}",
  dep.var.labels = c(
    '\\makecell[cl]{\\bf Advertisement \\\\ \\bf period \\\\ \\bf days/365}',
    '\\makecell[cl]{\\bf Number \\\\ \\bf of \\\\ \\bf bidders}'
  ),
  model.numbers = F,
  no.space = TRUE,
  font.size = "small",
  add.lines = list(
    c("\\textit{Control Variables-}", rep("", 4)),
    c("Contract Value (log)", rep("Y", 4)),
    c("Main Sector CPV", rep("Y", 4)),
    c("Contract Month", rep("Y", 4)),
    c("Buyer Type", rep("Y", 4)),
    c("Contract Year", rep("Y", 4))
  )
)
c = gsub('Observations', '\\\\textit{N (total)}', c)
c = gsub('Adjusted R\\$\\^\\{2\\}\\$', '\\\\textit{adj R$^{2}$}', c)
c = gsub(
  '\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\}',
  '\\\\begin{tabular}{p{200pt}p{60pt}p{60pt}}',
  c
)
write(c, 'output/table_17_cont_did.tex')


# TABLE 18 ----
# Load clustered SE
procedureintegrity_did_se = readRDS('1_procedureintegrity_did_clustered_se.Rds')
callintegrity_did_se = readRDS('2_callintegrity_did_clustered_se.Rds')
advertperiod_did_se = readRDS('3_advertperiod_did_clustered_se.Rds')
totalbidderintegrity_did_se = readRDS('4_totalbidderintegrity_did_clustered_se.Rds')

m = list(
  procedureintegrity_did_se,
  callintegrity_did_se,
  advertperiod_did_se,
  totalbidderintegrity_did_se
)
adj.r.sq_procedureintegrity_did = summary(procedureintegrity_did)$adj.r.squared
adj.r.sq_procedureintegrity_did = round(adj.r.sq_procedureintegrity_did, 3)

n_total_procedureintegrity_did = procedureintegrity_did$model %>% nrow()
n_total_procedureintegrity_did = format(n_total_procedureintegrity_did, big.mark = ",")

adj.r.sq_callintegrity_did = summary(callintegrity_did)$adj.r.squared
adj.r.sq_callintegrity_did = round(adj.r.sq_callintegrity_did, 3)

n_total_callintegrity_did = callintegrity_did$model %>% nrow()
n_total_callintegrity_did = format(n_total_callintegrity_did, big.mark = ",")

adj.r.sq_advertperiod_did = summary(advertperiod_did)$adj.r.squared
adj.r.sq_advertperiod_did = round(adj.r.sq_advertperiod_did, 3)

n_total_advertperiod_did = advertperiod_did$model %>% nrow()
n_total_advertperiod_did = format(n_total_advertperiod_did, big.mark = ",")

adj.r.sq_totalbidderintegrity_did = summary(totalbidderintegrity_did)$adj.r.squared
adj.r.sq_totalbidderintegrity_did = round(adj.r.sq_totalbidderintegrity_did, 3)

n_total_totalbidderintegrity_did = totalbidderintegrity_did$model %>% nrow()
n_total_totalbidderintegrity_did = format(n_total_totalbidderintegrity_did, big.mark = ",")

# Generate Clustered SE table in HTML for rendering and checking
c = stargazer::stargazer(
  m,
  header = FALSE,
  keep.stat = c('n', 'adj.rsq'),
  type = 'latex',
  object.names = F,
  table.layout = "=-lc-mta-s-n-",
  notes.align = 'l',
  keep = c('loc:timing', 'timing', 'loc'),
  digits = 3,
  order = c('loc:timing', 'timing', 'loc'),
  covariate.labels = c(
    'Diff-in-diff (full Period)',
    'Timing (full Period)',
    'Location (full Period)'
  ),
  dep.var.caption = "\\textit{(diff-in-diff (OLS))}",
  column.labels = c(
    '\\makecell[cl]{\\bf Non-open \\\\ \\bf procedure}',
    '\\makecell[cl]{\\bf No tender \\\\ \\bf call}',
    '\\makecell[cl]{\\bf Too-short \\\\ \\bf advertisement \\\\ \\bf period}',
    '\\makecell[cl]{\\bf Single \\\\ \\bf bidding}'
  ),
  model.numbers = F,
  model.names = F,
  no.space = TRUE,
  font.size = "small",
  add.lines = list(
    c("\\textit{Control Variables-}", rep("", 4)),
    c("Contract Value (log)", rep("Y", 4)),
    c("Main Sector CPV", rep("Y", 4)),
    c("Contract Month", rep("Y", 4)),
    c("Buyer Type", rep("Y", 4)),
    c("Contract Year", rep("Y", 4)),
    '\\hline \\\\[-1.8ex]',
    c(
      "\\textit{N (total)}",
      n_total_procedureintegrity_did,
      n_total_callintegrity_did,
      n_total_advertperiod_did,
      n_total_totalbidderintegrity_did
    ),
    c(
      'Adjusted R$^{2}$',
      adj.r.sq_procedureintegrity_did,
      adj.r.sq_callintegrity_did,
      adj.r.sq_advertperiod_did,
      adj.r.sq_totalbidderintegrity_did
    )
  )
)
c = gsub('\\\\hline \\\\\\\\\\[-1.8ex\\] &  &  &  &  \\\\\\\\ ',
         '\\\\hline \\\\\\\\[-1.8ex\\]',
         c)
c = gsub(
  '\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcccc\\}',
  '\\\\begin{tabular}{@{\\\\extracolsep{5pt}}lllll}',
  c
)
write(c, 'output/table_18_clustered_se.tex')
