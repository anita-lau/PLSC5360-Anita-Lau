library(tidyverse)
library(modelsummary)
library(gt)
library(ggplot2)
library(Synth)
library(SCtools)

silva_nl <- read.csv("data/data_reproduce_EPSR.csv", header = TRUE)


### Table 2: Country weights for synthetic Netherlands -----

### Agreement -----

# Execute dataprep for the agreement measurement of polarization
dat_agree_out <-
  dataprep(
    foo = silva_nl,
    predictors = c("gdpcap", "effectpt", "gini", "unemp"),
    predictors.op = "mean",
    special.predictors = list(
      list("age", 2001, "abs"),
      list("frac", 2001, "abs"),
      list("eijk", 1998.5, "mean"),
      list("eijk", 2000, "mean"),
      list("eijk", 2002, "mean")
    ),
    dependent = "eijk",
    unit.variable = "ctycode",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = 3,
    controls.identifier = c(1:2, 4, 6, 8:16),
    time.predictors.prior = 1995:2001,
    time.optimize.ssr = seq(1997, 2002, by = 0.5),
    time.plot = seq(1995, 2012, by = 0.5)
  )

# Run the synth command to create the synthetic control for agreement
synth_agree_out <- synth(data.prep.obj = dat_agree_out)

# Produce table of weights for the potential controls under the agreement outcome
agree_tab_wt <- synth.tab(
  synth.res = synth_agree_out,
  dataprep.res = dat_agree_out
)

# Weights from the original paper under agreeement
weights1_paper <- c(
  Austria          = 0,
  Belgium          = 0.001,
  Finland          = 0.36,
  France           = 0,
  Germany          = 0.351,
  Greece           = 0,
  Ireland          = 0.168,
  Italy            = 0,
  Luxembourg       = 0.12,
  Portugal         = 0,
  Spain            = 0,
  Sweden           = 0,
  "United Kingdom" = 0
)

# Create table of original and replication weights
tab_w1 <- agree_tab_wt$tab.w |>
  left_join(
    tibble(
      country = names(weights1_paper),
      weights_paper = unname(weights1_paper)
    ),
    by = c("unit.names" = "country")
  ) |>
  select(
    unit.names,
    weights_replication_2002 = w.weights,
    weights_paper,
    everything()
  )

### Kurtosis -----

# Execute dataprep for the kurtosis measurement of polarization
dat_kurt_out <-
  dataprep(
    foo = silva_nl,
    predictors = c("gdpcap", "effectpt", "gini", "unemp"),
    predictors.op = "mean",
    special.predictors = list(
      list("age", 2001, "abs"),
      list("frac", 2001, "abs"),
      list("r.kt", 1998.5, "mean"),
      list("r.kt", 2000, "mean"),
      list("r.kt", 2002, "mean")
    ),
    dependent = "r.kt",
    unit.variable = "ctycode",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = 3,
    controls.identifier = c(1:2, 4, 6, 8:16),
    time.predictors.prior = 1995:2001,
    time.optimize.ssr = seq(1997, 2001.5, by = 0.5),
    time.plot = seq(1995, 2012, by = 0.5)
  )

# Run the synth command to create the synthetic control for kurtosis
synth_kurt_out <- synth(data.prep.obj = dat_kurt_out)

# Produce table of weights for the potential controls under the kurtosis outcome
kurt_tab_wt <- synth.tab(
  synth.res = synth_kurt_out,
  dataprep.res = dat_kurt_out
)

# weights from the original paper under kurtosis
weights2_paper <- c(
  Austria          = 0.05,
  Belgium          = 0,
  Finland          = 0,
  France           = 0,
  Germany          = 0.267,
  Greece           = 0,
  Ireland          = 0.219,
  Italy            = 0.464,
  Luxembourg       = 0,
  Portugal         = 0,
  Spain            = 0,
  Sweden           = 0,
  "United Kingdom" = 0
)

# Create a table of original and replication weights
tab_w2 <- kurt_tab_wt$tab.w |>
  left_join(
    tibble(
      country = names(weights2_paper),
      weights_paper = unname(weights2_paper)
    ),
    by = c("unit.names" = "country")
  ) |>  select(
    unit.names,
    weights_replication_2001.5 = w.weights,
    weights_paper,
    everything()
  )

### Table 1: Description of Synthetic Netherlands -----
agree_pred <- as.data.frame(agree_tab_wt$tab.pred) |>
  rownames_to_column(var = "predictors")

kurt_pred <- as.data.frame(kurt_tab_wt$tab.pred) |>
  rownames_to_column(var = "predictors")

tab_pred <- full_join(
  agree_pred,
  kurt_pred,
  by = "predictors"
) |>
  column_to_rownames("predictors") |>
  mutate(
    Netherlands = coalesce(Treated.x, Treated.y),
    "Sample mean" = coalesce(`Sample Mean.x`, `Sample Mean.y`)
  ) |>
  select(
    Netherlands,
    "Synthetic (agreement)" = Synthetic.x,
    "Synthetic (kurtosis)" = Synthetic.y,
    "Sample mean"
  )

### Figure 1: Polarization in the Netherlands, synthetic unit, and sample mean -----

# Find the agreement and kurtosis trend for sample mean
Y0 <- dat_agree_out$Y0plot
time_agree <- dat_agree_out$tag$time.plot
agree_sample <- rowMeans(Y0)

Y0_kurt <- dat_kurt_out$Y0plot
time_kurt <- dat_kurt_out$tag$time.plot
kurt_sample <- rowMeans(Y0_kurt)

# Save figure
png(
  filename = "figures/figure1.png",
  width  = 9 * 300,
  height = 4.5 * 300,
  res = 300
)

# Agreement results
layout(
  matrix(c(1, 2,3, 3), nrow = 2, byrow = TRUE),
  heights = c(1, 0.18)
)

par(
  mar   = c(4, 4, 2, 1),
  cex.axis = 0.85,
  cex.lab  = 0.95
)

path.plot(
  synth.res = synth_agree_out,
  dataprep.res = dat_agree_out,
  Ylab = "Agreement",
  Xlab = NA,
  Legend = NA
)
axis(side = 1, at = 2002, labels = "2002")
abline(v = 2002, lty = 4, lwd = 2)

lines(time_agree, agree_sample, lty = 3, lwd = 2)

# Kurtosis results
path.plot(
  synth.res = synth_kurt_out,
  dataprep.res = dat_kurt_out,
  Ylab = "Kurtosis",
  Xlab = NA,
  Ylim = c(-1, 1),
  Legend = NA
)
axis(side = 1, at = 2002, labels = "2002")
abline(v = 2002, lty = 4, lwd = 2)

lines(time_kurt, kurt_sample, lty = 3, lwd = 2)

par(xpd = NA)

par(mar = c(0, 0, 0, 0))
plot.new()

legend(
  "center",
  legend = c("Netherlands", "Sample mean", "Synthetic Netherlands"),
  lty    = c(1, 3, 2),
  lwd    = c(2, 2, 2),
  ncol   = 3,
  bty    = "n",
  cex    = 1,
  x.intersp = 0.9,
  seg.len   = 2.5
)

par(xpd = FALSE)
dev.off()

### Calculate the average treatment effect -----
gap_agree <- dat_agree_out$Y1plot - (Y0 %*% synth_agree_out$solution.w)
effect_agree <- gap_agree[which(time_agree >= 2002.5)]
mean(effect_agree)

gap_kurt <- dat_kurt_out$Y1plot - (Y0_kurt %*% synth_kurt_out$solution.w)
effect_kurt <- gap_kurt[which(time_kurt >= 2002.5)]
mean(effect_kurt)

### Figure 3: Polarization gap in the Netherlands and 13 control countries -----

# Placebo test for agreement outcome
png(
  filename = "figures/agree_placebo.png",
  width  = 5.5 * 270,
  height = 3.5 * 270,
  res    = 300
)

agree_placebo <- generate_placebos(
  dat_agree_out,
  synth_agree_out,
  Sigf.ipop = 5,
  strategy = "sequential"
)

plot_placebos(
  agree_placebo,
  discard.extreme = TRUE,
  xlab = "Year",
  ylab = "Gap in Agreement"
)

dev.off()

# Placebo test for kurtosis outcome

png(
  filename = "figures/kurt_placebo.png",
  width  = 5.5 * 270,
  height = 3.5 * 270,
  res    = 300
)

kurt_placebo <- generate.placebos(
  dat_kurt_out,
  synth_kurt_out,
  Sigf.ipop = 5,
  strategy = "sequential"
)

plot_placebos(
  kurt_placebo,
  discard.extreme = TRUE,
  xlab = "Year",
  ylab = "Gap in Kurtosis"
)

dev.off()

### Figure 5: Sensitivity test ---------------------------

### Agreement -----

# Execute dataprep for changing intervention to 1998
dat_agree_lag <-
  dataprep(
    foo = silva_nl,
    predictors = c("gdpcap", "effectpt", "gini", "unemp"),
    predictors.op = "mean",
    special.predictors = list(
      list("age", 2001, "abs"),
      list("frac", 2001, "abs"),
      list("eijk", 1998.5, "mean"),
      list("eijk", 2000, "mean"),
      list("eijk", 2002, "mean")
    ),
    dependent = "eijk",
    unit.variable = "ctycode",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = 3,
    controls.identifier = c(1:2, 4, 6, 8:16),
    time.predictors.prior = 1995:1997,
    time.optimize.ssr = seq(1995, 1998, by = 0.5),
    time.plot = seq(1995, 2012, by = 0.5)
  )

# Run the synth command to create the synthetic control for agreement
synth_agree_lag <- synth(data.prep.obj = dat_agree_lag)

# Find the agreement trend for sample mean
Y0_lag <- dat_agree_lag$Y0plot
time_agree_lag <- dat_agree_lag$tag$time.plot
agree_sample_lag <- rowMeans(Y0_lag)

### Kurtosis -----

# Execute dataprep for changing the intervention to 1998
dat_kurt_lag <-
  dataprep(
    foo = silva_nl,
    predictors = c("gdpcap", "effectpt", "gini", "unemp"),
    predictors.op = "mean",
    special.predictors = list(
      list("age", 2001, "abs"),
      list("frac", 2001, "abs"),
      list("r.kt", 1998.5, "mean"),
      list("r.kt", 2000, "mean"),
      list("r.kt", 2002, "mean")
    ),
    dependent = "r.kt",
    unit.variable = "ctycode",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = 3,
    controls.identifier = c(1:2, 4, 6, 8:16),
    time.predictors.prior = 1995:1998,
    time.optimize.ssr = seq(1995, 1998, by = 0.5),
    time.plot = seq(1995, 2012, by = 0.5)
  )

# Run the synth command to create the synthetic control for kurtosis
synth_kurt_lag <- synth(data.prep.obj = dat_kurt_lag)

# Find the kurtosis trend for sample mean
Y0_kurt_lag <- dat_kurt_lag$Y0plot
time_kurt_lag <- dat_kurt_lag$tag$time.plot
kurt_sample_lag <- rowMeans(Y0_kurt_lag)

# Plot the lag of the intervention
png(
  filename = "figures/figureA2.png",
  width  = 5 * 200,
  height = 3 * 200,
  res = 300
)

layout(
  matrix(c(1, 2,3, 3), nrow = 2, byrow = TRUE),
  heights = c(1, 0.18)
)

par(
  mar   = c(4, 4, 2, 1),
  cex.axis = 0.85,
  cex.lab  = 0.95
)

path.plot(
  synth.res = synth_agree_lag,
  dataprep.res = dat_agree_lag,
  Ylab = "Agreement",
  Xlab = NA,
  Legend = NA,
)
axis(side = 1, at = 1998, labels = "1998")
abline(v = 1998, lty = 4, lwd = 2)
lines(time_agree_lag, agree_sample_lag, lty = 3, lwd = 2)

path.plot(
  synth.res = synth_kurt_lag,
  dataprep.res = dat_kurt_lag,
  Ylab = "Kurtosis",
  Xlab = NA,
  Ylim = c(-1, 1),
  Legend = NA,
)
axis(side = 1, at = 1998, labels = "1998")
abline(v = 1998, lty = 4, lwd = 2)
lines(time_kurt_lag, kurt_sample_lag, lty = 3, lwd = 2)

par(xpd = NA)

par(mar = c(0, 0, 0, 0))
plot.new()

legend(
  "center",
  legend = c("Netherlands", "Sample mean", "Synthetic Netherlands"),
  lty    = c(1, 3, 2),
  lwd    = c(2, 2, 2),
  ncol   = 3,
  bty    = "n",
  cex    = 1,
  x.intersp = 0.9,
  seg.len   = 2.5
)

dev.off()
