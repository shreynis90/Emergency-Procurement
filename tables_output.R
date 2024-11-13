####
#' Author: Ahmed Al-Shaibani
####

# Load Libs
library(tidyverse)

library(stargazer)

library(kableExtra)

# install.packages("devtools")
devtools::install_github("ChandlerLutz/starpolishr")
library(starpolishr)
#
# custom functions ----
# Make a function that produces the stars for the regression marginal effects coefficients
gen_stars = function(p_value) {
  m_stars = case_when(p_value <= 0.01 ~ "***",
                      p_value <= 0.05 ~ "**",
                      p_value <= 0.1 ~ "*",
                      TRUE ~ "")
  return(m_stars)
}
#
# regression output ----
source('did_procedureintegrity.R')
source('did_callintegrity.R')
source('did_advertperiod.R')
source('did_totalbidderintegrity.R')

source('cont_did_advertintegrity.R')
source('cont_did_totalbidderintegrity.R')

rm(list = ls(), envir = globalenv())  # Clean up
gc()
# install.packages('stargazer')

dir.create('output', showWarnings = F)

# TABLE 8 ----
# Load models
procedureintegrity_did = readRDS('tables/table_8/1_t_test_did_procedureintegrity.rds')
procedureintegrity_did = procedureintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Non-open procedure (after - before)' = 1)

callintegrity_did = readRDS('tables/table_8/2_t_test_did_callintegrity.rds')
callintegrity_did = callintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('No tender call (after - before)' = 1)

advertperiod_did = readRDS('tables/table_8/3_t_test_did_advertperiod.rds')
advertperiod_did = advertperiod_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Too-short Advertisement Period (after - before)' = 1)

totalbidderintegrity_did = readRDS('tables/table_8/4_t_test_did_totalbidderintegrity.rds')
totalbidderintegrity_did = totalbidderintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Single bidding' = 1)

m = procedureintegrity_did %>% 
  bind_cols(callintegrity_did) %>% 
  bind_cols(advertperiod_did) %>% 
  bind_cols(totalbidderintegrity_did) 
m = m %>% 
  mutate(row_name = rownames(m)) %>% 
  select(row_name, everything())

write_excel_csv(m, 'tables/table_8/table_8_simple.csv')
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
  table.placement = 'h',
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

c = gsub('\\\\begin\\{table\\}\\[h\\] \\\\centering ', '', c)
c = gsub('\\\\caption\\{\\}', '', c)
c = gsub('\\\\label\\{\\}', '', c)
c = gsub('\\\\small', '', c)

c = gsub('\\\\end\\{table\\}', '', c)

write(c, 'output/table_9_did.tex')

# TABLE 13 ----
# Load models
procedureintegrity_did = readRDS('tables/table_13/1_t_test_did_procedureintegrity.rds')
procedureintegrity_did = procedureintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Non-open procedure' = 1)

callintegrity_did = readRDS('tables/table_13/2_t_test_did_callintegrity.rds')
callintegrity_did = callintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('No tender call (after - before)' = 1)

advertperiod_did = readRDS('tables/table_13/3_t_test_did_advertperiod.rds')
advertperiod_did = advertperiod_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Too-short Advertisement Period (after - before)' = 1)

totalbidderintegrity_did = readRDS('tables/table_13/4_t_test_did_totalbidderintegrity.rds')
totalbidderintegrity_did = totalbidderintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Single bidding' = 1)

m = procedureintegrity_did %>% 
  bind_cols(callintegrity_did) %>% 
  bind_cols(advertperiod_did) %>% 
  bind_cols(totalbidderintegrity_did) 
m = m %>% 
  mutate(row_name = rownames(m)) %>% 
  select(row_name, everything())

write_excel_csv(m, 'tables/table_13/table_13_simple.csv')

# TABLE 14 ----
# Load models
procedureintegrity_did = readRDS('tables/table_14/1_t_test_did_procedureintegrity.rds')
procedureintegrity_did = procedureintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Non-open procedure (after - before)' = 1)

callintegrity_did = readRDS('tables/table_14/2_t_test_did_callintegrity.rds')
callintegrity_did = callintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('No tender call (after - before)' = 1)

advertperiod_did = readRDS('tables/table_14/3_t_test_did_advertperiod.rds')
advertperiod_did = advertperiod_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Too-short Advertisement Period (after - before)' = 1)

totalbidderintegrity_did = readRDS('tables/table_14/4_t_test_did_totalbidderintegrity.rds')
totalbidderintegrity_did = totalbidderintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Single bidding' = 1)

m = procedureintegrity_did %>% 
  bind_cols(callintegrity_did) %>% 
  bind_cols(advertperiod_did) %>% 
  bind_cols(totalbidderintegrity_did) 
m = m %>% 
  mutate(row_name = rownames(m)) %>% 
  select(row_name, everything())

write_excel_csv(m, 'tables/table_14/table_14_simple.csv')


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
  table.placement = 'h',
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
  '\\\\begin{tabular}{p{200pt}p{75pt}p{75pt}}',
  c
)

c = gsub('\\\\begin\\{table\\}\\[h\\] \\\\centering ', '', c)
c = gsub('\\\\caption\\{\\}', '', c)
c = gsub('\\\\label\\{\\}', '', c)
c = gsub('\\\\small', '', c)

c = gsub('\\\\end\\{table\\}', '', c)

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
  table.placement = 'h',
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
 
c = gsub('\\\\begin\\{table\\}\\[h\\] \\\\centering ', '', c)
c = gsub('\\\\caption\\{\\}', '', c)
c = gsub('\\\\label\\{\\}', '', c)
c = gsub('\\\\small', '', c)

c = gsub('\\\\end\\{table\\}', '', c)

write(c, 'output/table_18_clustered_se.tex')


# ROBUSTNESS TABLE 21 (7) ----

logit_procedureintegrity_model = readRDS('tables/robustness/table_21/1_logit_procedureintegrity_01_model.rds')

logit_procedureintegrity_se = readRDS('tables/robustness/table_21/1_logit_procedureintegrity_01_se.rds')
logit_procedureintegrity_se = round(logit_procedureintegrity_se$CoxSnell, 3)

logit_procedureintegrity_n_after = readRDS('tables/robustness/table_21/1_logit_procedureintegrity_01_n_after.rds')

logit_procedureintegrity = readRDS('tables/robustness/table_21/1_logit_procedureintegrity_01.rds')
logit_procedureintegrity = logit_procedureintegrity %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')'))


logit_callintegrity_model = readRDS('tables/robustness/table_21/2_logit_callintegrity_01_model.rds')

logit_callintegrity_se = readRDS('tables/robustness/table_21/2_logit_callintegrity_01_se.rds')
logit_callintegrity_se = round(logit_callintegrity_se$CoxSnell, 3)

logit_callintegrity_n_after = readRDS('tables/robustness/table_21/2_logit_callintegrity_01_n_after.rds')

logit_callintegrity = readRDS('tables/robustness/table_21/2_logit_callintegrity_01.rds')
logit_callintegrity = logit_callintegrity %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')'))


logit_advertintegrity_model = readRDS('tables/robustness/table_21/3_logit_advertintegrity_01_model.rds')

logit_advertintegrity_se = readRDS('tables/robustness/table_21/3_logit_advertintegrity_01_se.rds')
logit_advertintegrity_se = round(logit_advertintegrity_se$CoxSnell, 3)

logit_advertintegrity_n_after = readRDS('tables/robustness/table_21/3_logit_advertintegrity_01_n_after.rds')

logit_advertintegrity = readRDS('tables/robustness/table_21/3_logit_advertintegrity_01.rds')
logit_advertintegrity = logit_advertintegrity %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')'))


logit_totalbidderintergrity_model = readRDS('tables/robustness/table_21/4_logit_totalbidderintergrity_01_model.rds')

logit_totalbidderintergrity_se = readRDS('tables/robustness/table_21/4_logit_totalbidderintergrity_01_se.rds')
logit_totalbidderintergrity_se = round(logit_totalbidderintergrity_se$CoxSnell, 3)

logit_totalbidderintergrity_n_after = readRDS('tables/robustness/table_21/4_logit_totalbidderintergrity_01_n_after.rds')

logit_totalbidderintergrity = readRDS('tables/robustness/table_21/4_logit_totalbidderintergrity_01.rds')
logit_totalbidderintergrity = logit_totalbidderintergrity %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')'))

covariate.labels = "Treatment (full period)"
# column.labels = c(
#   logit_procedureintegrity$term,
#   logit_callintegrity$term,
#   logit_advertintegrity$term,
#   logit_totalbidderintergrity$term
# )
dep.var.labels = c(
  '\\makecell[cl]{\\bf Non-open \\\\ \\bf procedure}',
  '\\makecell[cl]{\\bf No tender \\\\ \\bf call}',
  '\\makecell[cl]{\\bf Too-short \\\\ \\bf advertisement \\\\ \\bf period}',
  '\\makecell[cl]{\\bf Single \\\\ \\bf bidding}'
)
add_lines = list(
  c("Treatment (full period)",
    logit_procedureintegrity$t_score,
    logit_callintegrity$t_score,
    logit_advertintegrity$t_score,
    logit_totalbidderintergrity$t_score
  ),
  c("",
    logit_procedureintegrity$t_value,
    logit_callintegrity$t_value,
    logit_advertintegrity$t_value,
    logit_totalbidderintergrity$t_value
    ),
  rep("", 5),
  c("\\textit{Control Variables-}", rep("", 4)),
  c("Contract Value (log)", rep("Y", 4)),
  c("CPV (Medical or not)", rep("Y", 4)),
  c("Contract Month", rep("Y", 4)),
  c("Contract Year", rep("Y", 4)),
  c("Buyer Type (Regional or not)", rep("Y", 4))
  )

m = list(
  logit_procedureintegrity_model,
  logit_callintegrity_model,
  logit_advertintegrity_model,
  logit_totalbidderintergrity_model
)
c = stargazer(
  m,
  header = FALSE,
  table.placement = 'h',
  model.numbers = F,
  keep.stat = c('n', 'adj.rsq'),
  type = 'latex',
  # type = "text",
  object.names = F,
  
  colnames = TRUE,
  dep.var.caption = "\\textit{Binary Logistic Regression}",
  dep.var.labels = dep.var.labels,
  covariate.labels = covariate.labels,
  add.lines = add_lines,
  # notes = notes,
  notes.align = "l",
  table.layout = "=-ld-mcta-s-n-",
  keep = c('loc:timing'),
  digits = 3,
  order = c('loc:timing'),
  model.names = FALSE,
  # omit = omitted_vars,
  # table.layout = "=ldc-ats-n",
  font.size = "footnotesize",
  column.sep.width = "1pt"
)
# 

c = gsub('Observations', '\\\\textit{N (total)}', c)
c = gsub('Adjusted R\\$\\^\\{2\\}\\$', '\\\\textit{adj R$^{2}$}', c)

c = gsub(
  '\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{1pt\\}\\}lcccc\\} ',
  '\\\\begin{tabular}{@{\\\\extracolsep{5pt}}lllll}',
  c
)

c = gsub('\\\\begin\\{table\\}\\[h\\] \\\\centering ', '', c)
c = gsub('\\\\caption\\{\\}', '', c)
c = gsub('\\\\label\\{\\}', '', c)
c = gsub('\\\\small', '', c)

c = gsub('\\\\end\\{table\\}', '', c)

n_after = sprintf('\\textit{N(after disaster)} & %s & %s & %s & %s \\\\ ',
                  format(logit_procedureintegrity_n_after, big.mark = ","),
                  format(logit_callintegrity_n_after, big.mark = ","),
                  format(logit_advertintegrity_n_after, big.mark = ","),
                  format(logit_totalbidderintergrity_n_after, big.mark = ",")
)

R2 = sprintf('Cox \\& Snell’s \\textit{R$^{2}$} & %s & %s & %s & %s \\\\ ',
             logit_procedureintegrity_se,
             logit_callintegrity_se,
             logit_advertintegrity_se,
             logit_totalbidderintergrity_se
)

c %>% 
  star_insert_row(insert.after=23, R2) %>% 
  star_insert_row(insert.after=23, n_after) %>% 
  cat(file='output/table_21_logit.tex',sep='\n')

# write(c, 'output/table_21_logit.tex')

#
# ROBUSTNESS TABLE 22 (8) ----
# Load models
procedureintegrity_did = readRDS('tables/robustness/table_22/1_t_test_did_procedureintegrity.rds')
procedureintegrity_did = procedureintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Non-open procedure (after - before)' = 1)

callintegrity_did = readRDS('tables/robustness/table_22/2_t_test_did_callintegrity.rds')
callintegrity_did = callintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('No tender call (after - before)' = 1)

advertperiod_did = readRDS('tables/robustness/table_22/3_t_test_did_advertperiod.rds')
advertperiod_did = advertperiod_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Too-short Advertisement Period (after - before)' = 1)

totalbidderintegrity_did = readRDS('tables/robustness/table_22/4_t_test_did_totalbidderintegrity.rds')
totalbidderintegrity_did = totalbidderintegrity_did %>% 
  mutate(t_score = paste0(round(estimate, 3), gen_stars(p.value)),
         t_value = paste0('(', round(statistic, 3), ')')) %>% 
  select(t_score, t_value, `N (total)`, `N (after disaster)`) %>% 
  t() %>% 
  data.frame() %>% 
  rename('Single bidding' = 1)

m = procedureintegrity_did %>% 
  bind_cols(callintegrity_did) %>% 
  bind_cols(advertperiod_did) %>% 
  bind_cols(totalbidderintegrity_did) 
m = m %>% 
  mutate(row_name = rownames(m)) %>% 
  select(row_name, everything())

write_excel_csv(m, 'tables/robustness/table_22/table_22_simple.csv')
# ROBUSTNESS TABLE 23 (9) ----
# Load models
procedureintegrity_did = readRDS('tables/robustness/table_23/1_procedureintegrity_did.Rds')
callintegrity_did = readRDS('tables/robustness/table_23/2_callintegrity_did.Rds')
advertperiod_did = readRDS('tables/robustness/table_23/3_advertperiod_did.Rds')
totalbidderintegrity_did = readRDS('tables/robustness/table_23/4_totalbidderintegrity_did.Rds')

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
  table.placement = 'h',
  table.layout = "=-ld-mcta-s-n-",
  notes.align = 'l',
  keep = c('loc:timing', 'timing', 'loc'),
  digits = 3,
  order = c('loc:timing', 'timing', 'loc'),
  covariate.labels = c(
    'Diff-in-diff (first year omitted)',
    'Timing (first year omitted)',
    'Location (first year omitted)'
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

c = gsub('\\\\begin\\{table\\}\\[h\\] \\\\centering ', '', c)
c = gsub('\\\\caption\\{\\}', '', c)
c = gsub('\\\\label\\{\\}', '', c)
c = gsub('\\\\small', '', c)

c = gsub('\\\\end\\{table\\}', '', c)

write(c, 'output/table_23_did.tex')

