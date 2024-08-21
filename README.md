# EmergencyProcurement

1. Files starting with 'did' refer to difference-in-differences. In these files,
  * The variable matched_comparison gives the before-after t-tests.
  * The variable didreg gives the results of the diff-in-diff regressions.
  * The variable didreg2 gives the results of the diff-in-diff regressions with standard errors clustered at the NUTS 3 level
2. Files starting with 'logit' refer to logit models.
  * The output of 'summary(margins(model_logit))' gives the average margin effects for the logit regression.
  * The output of 'summary(margins(model3_logit))' gives the average margin effects for the three-year before-after logit regression.
  * The output of 'summary(margins(model2_logit))' gives the average margin effects for the two-year before-after logit regression.
  * The output of 'summary(margins(model1_logit))' gives the average margin effects for the one-year before-after logit regression.
3. Files starting with 'cont' refer to continuous dependent variables for advert integrity and total bidder integrity.
  * The variable matched_comparison gives the before-after t-tests.
  * The variable didreg gives the results of the diff-in-diff regressions.
  * The variable didreg2 gives the results of the diff-in-diff regressions with standard errors clustered at the NUTS 3 level
