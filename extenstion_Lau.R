library(tidyverse)
library(modelsummary)
library(gt)
library(ggplot2)
library(Synth)
library(SCtools)

silva_nl_new <- read.csv("data/data_extenstion_EPSR.csv")

### Table 2: Country weights for synthetic Netherlands --------------------

### Agreement -----

# Execute dataprep for the agreement measurement of polarization
dat_agree_out2 <-
  dataprep(
    foo = silva_nl_new,
    predictors = c("gdpcap", "effectpt", "gini", "unemp", "immig_salience_avg"),
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
synth_agree_out2 <- synth(data.prep.obj = dat_agree_out2)

# Produce table of weights for the potential controls under the agreement outcome
agree_tab_wt2 <- synth.tab(
  synth.res = synth_agree_out2,
  dataprep.res = dat_agree_out2
)

# Weights from the replication under agreeement
weights1_ext <- c(
  Austria          = 0,
  Belgium          = 0,
  Finland          = 0.357,
  France           = 0,
  Germany          = 0.352,
  Greece           = 0,
  Ireland          = 0.165,
  Italy            = 0,
  Luxembourg       = 0.126,
  Portugal         = 0,
  Spain            = 0,
  Sweden           = 0,
  "United Kingdom" = 0
)

# Create table of replication and extension weights
tab_w1 <- agree_tab_wt2$tab.w |>
  left_join(
    tibble(
      country = names(weights1_ext),
      weights_replication = unname(weights1_ext)
    ),
    by = c("unit.names" = "country")
  ) |>
  select(
    unit.names,
    weights_extension_2002 = w.weights,
    weights_replication
  )

### Kurtosis -----

# Execute dataprep for the kurtosis measurement of polarization
dat_kurt_out2 <-
  dataprep(
    foo = silva_nl_new,
    predictors = c("gdpcap", "effectpt", "gini", "unemp", "immig_salience_avg"),
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
synth_kurt_out2 <- synth(data.prep.obj = dat_kurt_out2)

# Produce table of weights for the potential controls under the kurtosis outcome
kurt_tab_wt2 <- synth.tab(
  synth.res = synth_kurt_out2,
  dataprep.res = dat_kurt_out2
)

# weights from the replication under kurtosis
weights2_ext <- c(
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

# Create a table of replication and extension weights
tab_w2 <- kurt_tab_wt2$tab.w |>
  left_join(
    tibble(
      country = names(weights2_ext),
      weights_replication = unname(weights2_ext)
    ),
    by = c("unit.names" = "country")
  ) |>  select(
    unit.names,
    weights_extension_2001.5 = w.weights,
    weights_replication
  )

# Join Table 1 and Table 2
join_tab_w <- left_join(
  tab_w1,
  tab_w2,
  by = "unit.names"
) |>
  mutate(
    across(
      -unit.names,
      ~ ifelse(. == 0, "0", formatC(., format = "f", digits = 3))
    )
  ) |>
  arrange(unit.names)

gt_tab_w <- join_tab_w |>
  gt() |>
  tab_header(title = "Table 3 : Country weights for synthetic Netherlands") |>
  cols_label(
    unit.names = "Country",
    weights_replication.x    = "Agreement",
    weights_replication.y    = "Kurtosis",
    weights_extension_2002   = "Agreement",
    weights_extension_2001.5 = "Kurtosis",
  ) |>
  tab_spanner(
    label = "Replication",
    columns = c(weights_replication.x, weights_replication.y)
  ) |>
  tab_spanner(
    label = "Extension",
    columns = c(weights_extension_2002, weights_extension_2001.5)
  ) |>
  tab_spanner(
    label = "Weight",
    columns = c(
      weights_replication.x,
      weights_replication.y,
      weights_extension_2002,
      weights_extension_2001.5,
    )
  ) |>
  cols_width(
    unit.names ~ px(100),
    everything() ~ px(70)
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_options(
    table.font.size = px(9),
    data_row.padding = px(2),
    column_labels.padding = px(3),
    heading.align = "center"
  )

# Save table
gtsave(gt_tab_w, "figures/table3.png")


### Table 1: Description of Synthetic Netherlands ----------------------------
agree_pred2 <- as.data.frame(agree_tab_wt2$tab.pred) |>
  rownames_to_column(var = "predictors")

kurt_pred2 <- as.data.frame(kurt_tab_wt2$tab.pred) |>
  rownames_to_column(var = "predictors")

tab_pred2 <- full_join(
  agree_pred2,
  kurt_pred2,
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

tab_pred <- tab_pred |>
  rownames_to_column(var = "predictors")

tab_pred2 <- tab_pred2 |>
  rownames_to_column(var = "predictors")

# Join the two tables together
tab_pred_final <- full_join(
  tab_pred,
  tab_pred2,
  by = "predictors"
) |>
  mutate(predictors = case_match(
    predictors,
    "gdpcap" ~ "GDP per capita",
    "effectpt" ~ "Effective number of political parties",
    "gini" ~ "Income Inequality",
    "unemp" ~ "Unemployment rate",
    "special.age.2001" ~ "Age of democracy",
    "special.frac.2001" ~ "Fractionalization",
    "immig_salience_avg" ~ "Elite Immigration salience",
    "special.eijk.1998.5" ~ "Agreement 1998.5",
    "special.eijk.2000" ~ "Agreement 2000",
    "special.eijk.2002" ~ "Agreement 2002",
    "special.r.kt.1998.5" ~ "Kurtosis 1998.5",
    "special.r.kt.2000" ~ "Kurtosis 2000",
    "special.r.kt.2002" ~ "Kurtosis 2002"
  ),
    Netherlands = coalesce(Netherlands.x, Netherlands.y),
    "Sample mean" = coalesce(`Sample mean.x`, `Sample mean.y`)
  ) |>
  select(
    predictors,
    Netherlands,
    "Synthetic (agreement).x",
    "Synthetic (agreement).y",
    "Synthetic (kurtosis).x",
    "Synthetic (kurtosis).y",
    "Sample mean"
  ) |>
  mutate(
    across(
      -predictors,
      ~ ifelse(is.na(.),"", sprintf("%.2f", as.numeric(.)))
    )
  ) |>
  column_to_rownames(var = "predictors")

gt_pred_final <- tab_pred_final |>
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |>
  gt(rownames_to_stub = TRUE) |>
  tab_header(title = "Table 2: Description of synthetic Netherlands") |>
  cols_label(
    Netherlands = "The Netherlands",
    `Synthetic (agreement).x` = "Synthetic Netherlands (agreement)",
    `Synthetic (agreement).y` = "Synthetic Netherlands (agreement)",
    `Synthetic (kurtosis).x`  = "Synthetic Netherlands (kurtosis)",
    `Synthetic (kurtosis).y`  = "Synthetic Netherlands (kurtosis)",
    `Sample mean` = "Sample mean"
  ) |>
  tab_options(
    heading.align = "left",
    table.font.size = px(15),
    data_row.padding = px(4),
    stub.border.style = "none"
    )

# Save the table
gtsave(gt_pred_final, "figures/table2.png")


### Figure 1: Polarization in the Netherlands, synthetic unit, and sample mean -----

# Find the agreement and kurtosis trend for sample mean
Y0 <- dat_agree_out2$Y0plot
time_agree <- dat_agree_out2$tag$time.plot
agree_sample <- rowMeans(Y0)

Y0_kurt <- dat_kurt_out2$Y0plot
time_kurt <- dat_kurt_out2$tag$time.plot
kurt_sample <- rowMeans(Y0_kurt)

# Save figure
png(
  filename = "figures/figure1_extension.png",
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
  mar = c(4, 4, 2, 1),
  cex.axis = 0.85,
  cex.lab  = 0.95
)

path.plot(
  synth.res = synth_agree_out2,
  dataprep.res = dat_agree_out2,
  Ylab = "Agreement",
  Xlab = NA,
  Legend = NA,
)
axis(side = 1, at = 2002, labels = "2002")
abline(v = 2002, lty = 4, lwd = 2)

lines(time_agree, agree_sample, lty = 3, lwd = 2)

# Kurtosis results
path.plot(
  synth.res = synth_kurt_out2,
  dataprep.res = dat_kurt_out2,
  Ylab = "Kurtosis",
  Xlab = NA,
  Ylim = c(-1, 1),
  Legend = NA,
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
gap_agree2 <- dat_agree_out2$Y1plot - (Y0 %*% synth_agree_out2$solution.w)
effect_agree2 <- gap_agree2[which(time_agree >= 2002.5)]
mean(effect_agree2)

gap_kurt2 <- dat_kurt_out2$Y1plot - (Y0_kurt%*% synth_kurt_out2$solution.w)
effect_kurt2 <- gap_kurt2[which(time_kurt >= 2002.5)]
mean(effect_kurt2)

### Figure 3: Polarization gap in the Netherlands and 13 control countries -----

# Placebo test for agreement outcome
png(
  filename = "figures/agree_placebo_ext.png",
  width  = 6 * 300,
  height = 4 * 300,
  res    = 300
)

agree_placebo2 <- generate.placebos(
  dat_agree_out2,
  synth_agree_out2,
  Sigf.ipop = 5,
  strategy = "sequential"
)

plot_placebos(
  agree_placebo2,
  discard.extreme = TRUE,
  xlab = "Year",
  ylab = "Gap in Agreement"
)

dev.off()

# Placebo test for kurtosis outcome
png(
  filename = "figures/kurt_placebo_ext.png",
  width  = 5.5 * 300,
  height = 4 * 300,
  res    = 300
)

kurt_placebo2 <- generate.placebos(
  dat_kurt_out2,
  synth_kurt_out2,
  Sigf.ipop = 5,
  strategy = "sequential"
)

plot_placebos(
  kurt_placebo2,
  discard.extreme = TRUE,
  xlab = "Year",
  ylab = "Gap in Kurtosis"
)

dev.off()

### Figure 5: Sensitivity test -------------------------------

### Agreement -----

# Execute dataprep for lag of the intervention
dat_agree_lag2 <-
  dataprep(
    foo = silva_nl_new,
    predictors = c("gdpcap", "effectpt", "gini", "unemp", "immig_salience_avg"),
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
    time.predictors.prior = 1995:1999,
    time.optimize.ssr = seq(1995, 2000, by = 0.5),
    time.plot = seq(1995, 2012, by = 0.5)
  )

# Run the synth command to create the synthetic control for agreement
synth_agree_lag2 <- synth(data.prep.obj = dat_agree_lag2)

# Find agreement trend for sample mean
Y0_lag <- dat_agree_lag2$Y0plot
time_agree_lag <- dat_agree_lag2$tag$time.plot
agree_sample_lag <- rowMeans(Y0_lag)

### Kurtosis -----

# Execute dataprep for lag of the intervention
dat_kurt_lag2 <-
  dataprep(
    foo = silva_nl_new,
    predictors = c("gdpcap", "effectpt", "gini", "unemp", "immig_salience_avg"),
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
    time.predictors.prior = 1995:1999,
    time.optimize.ssr = seq(1995, 1999.5, by = 0.5),
    time.plot = seq(1995, 2012, by = 0.5)
  )

# Run the synth command to create the synthetic control for kurtosis
synth_kurt_lag2 <- synth(data.prep.obj = dat_kurt_lag2)

# Find the kurtosis trend for sample mean
Y0_kurt_lag <- dat_kurt_lag2$Y0plot
time_kurt_lag <- dat_kurt_lag2$tag$time.plot
kurt_sample_lag <- rowMeans(Y0_kurt_lag)

# Plot the lag of the intervention
png(
  filename = "figures/figureA2_ext.png",
  width  = 9 * 300,
  height = 4.5 * 300,
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
  synth.res = synth_agree_lag2,
  dataprep.res = dat_agree_lag2,
  Ylab = "Agreement",
  Xlab = NA,
  Legend = NA
)
abline(v = 2000, lty = 4, lwd = 2)
lines(time_agree_lag, agree_sample_lag, lty = 3, lwd = 2)

path.plot(
  synth.res = synth_kurt_lag2,
  dataprep.res = dat_kurt_lag2,
  Ylab = "Kurtosis",
  Xlab = NA,
  Ylim = c(-1, 1),
  Legend = NA
)
abline(v = 2000, lty = 4, lwd = 2)
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
