library(tidyverse)
library(modelsummary)
library(gt)
library(ggplot2)
library(manifestoR)

silva_nl <- read.csv("data/data_reproduce_EPSR.csv", header = TRUE)

mp_setapikey("data/manifesto_apikey.txt")

### Define countries and years of interest ------

countries <- c("Netherlands", "Austria", "Belgium", "Finland", "France",
              "Germany", "Greece", "Ireland", "Italy", "Luxembourg", "Portugal",
              "Spain", "Sweden", "United Kingdom", "Denmark")

year <- 199501:201212

# Download data from Manifesto Project
m_data <- mp_maindataset() |>
  filter(countryname %in% countries & date %in% year)

# Content analytical variables relevant to immigration
immigration_codes <- c("per601", "per602", "per607", "per608")

m_data <- m_data |>
  select(countryname, date, party, pervote, all_of(immigration_codes))

# Compute measure of elite immigration salience
m_data <- m_data |>
  mutate(
    immig_salience = per601 + per602 + per607 + per608,
    year = date %/% 100
  )

# Weighted average of salience measure by country-year level
m_data <- m_data |>
  mutate(
    wt = pervote / sum(pervote, na.rm = TRUE),
    immig_salience_wt = wt * immig_salience,
    .by = c(countryname, year)
  ) |>
  summarise(
    immig_salience_avg = sum(immig_salience_wt, na.rm = TRUE),
    .by = c(countryname, year)
  )

### Merge dataset with Castanho Silva's dataset -----
silva_nl <- silva_nl |>
  mutate(
    year_merge = floor(year)
  ) |>
  left_join(
    m_data,
    by = c("country" = "countryname", "year_merge" = "year")
  ) |>
  select(-year_merge)

### Table 1: Summary Statistics of the data -----

stats <- silva_nl |>
  select(where(is.numeric), -c(ctycode, kurtosis)) |>
    pivot_longer(cols = everything()) |>
    mutate(name = case_match(
      name,
      "year" ~ "Year",
      "gdpcap" ~ "GDP per capita, PPP",
      "effectpt" ~ "Effective number of political parties",
      "gini" ~ "Income Inequality (Gini index score) ",
      "unemp" ~ "Long-term unemployment rate",
      "age" ~ "Age of democracy",
      "frac" ~ "Fractionalization index score",
      "immig_salience_avg" ~ "Immigration salience (% of manifesto)",
      "eijk" ~ "Agreement index score",
      "r.kt" ~ "Kurtosis",
    )) |>
    summarise(
      Mean = mean(value, na.rm = TRUE),
      "Std. Dev." = sd(value, na.rm = TRUE),
      Min = min(value, na.rm = TRUE),
      Max = max(value, na.rm = TRUE),
      N = n(),
      .by = name,
    )

summary_stats <- stats |>
  gt() |>
  tab_header(title = "Table 1 : Summary Statistics") |>
  cols_label(
    "name" ~ "Variable"
    ) |>
  tab_row_group(
    label = "A. Country Characteristics",
    rows = name %in% c(
      "Year",
      "GDP per capita, PPP",
      "Effective number of political parties",
      "Income Inequality (Gini index score) ",
      "Long-term unemployment rate",
      "Age of democracy",
      "Fractionalization index score",
      "Immigration salience (% of manifesto)"
    ),
    id = "country"
  ) |>
  tab_row_group(
    label = "B. Polarization",
    rows = name %in% c(
      "Agreement index score",
      "Kurtosis"
    ),
    id = "polarization"
  ) |>
  row_group_order(
    groups = c("country", "polarization")
  ) |>
  fmt_number(
      columns = c(Mean, "Std. Dev."),
      decimals = 3,
      use_seps = FALSE,
    ) |>
  fmt_number(
    columns = c(Min, Max),
    decimals = 2,
    use_seps = FALSE,
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "darkgrey",
        weight = px(2)
      )
    ),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "darkgrey",
      weight = px(2)
    ),
    locations = cells_row_groups()
  ) |>
  tab_options(
    table.font.size = px(10),
    data_row.padding = px(3),
    row_group.padding = px(4),
    column_labels.padding = px(4)
  )

# Save table
gtsave(summary_stats, filename = "figures/table1.png")

# Save new merged dataset
write_csv(silva_nl, "data/data_extenstion_EPSR.csv")

