## Import Packages
library(tis)
library(DT)
library(plotly)
library(ggplot2)
library(fixest)
library(broom)
library(lubridate)
library(purrr)
library(glue)
library(dplyr)
library(tidyr)
library(shiny)

################################################################################

plot_var <- list()
plot_var$heatrisk_colors <- c(
  "None" = "palegreen3",
  "Minor" = "darkgoldenrod2",
  "Moderate" = "darkorange2",
  "Major" = "red1",
  "Extreme" = "purple4",
  "None or Minor" = "yellowgreen"
)

################################################################################

FormatData <- function(data) {
  ### Clean and format data
  data <- data %>%
    ## Rename HeatRisk so we can make a labels column
    rename(HeatRisk_num = HeatRisk) %>%
    ## Label the HeatRisk
    mutate(
      HeatRisk = case_match(HeatRisk_num,
        0 ~ "None",
        1 ~ "Minor",
        2 ~ "Moderate",
        3 ~ "Major",
        4 ~ "Extreme",
        .default = NA
      ),
      ## Change to factor for plotting order
      HeatRisk = factor(HeatRisk, levels = c("None", "Minor", "Moderate", "Major", "Extreme")),
      ## Make sure data is formatted correctly
      Date = mdy(Date)
    ) %>%
    ## Make sure that all dates exist in data
    complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
    arrange(Date)

  return(data)
}

################################################################################

HolidaysToNA <- function(data, holiday_dates) {
  data <- data %>%
    ## Exclude HeatRisk so we don't mess with factor levels,
    ## otherwise write columns to NA if holiday
    mutate(across(c(-Date, -HeatRisk), ~ ifelse(Date %in% holiday_dates, NA, .)))
}

################################################################################

WeekendsToNA <- function(data) {
  data <- data %>%
    ## Exclude HeatRisk so we don't mess with factor levels,
    ## otherwise write columns to NA if on a Saturday or Sunday
    mutate(across(
      c(-Date, -HeatRisk),
      ~ ifelse(wday(Date, label = TRUE, abbr = TRUE) %in% c("Sat", "Sun"), NA, .)
    ))
}

################################################################################

GetControlObservations <- function(data, control_days, outcome_var) {
  ## Create lead and lag control for each control day
  for (control_day in control_days) {
    ## Create names for dynamic variables
    lag_name <- as.name(glue(".diff#_#lag#_#{control_day}"))
    lead_name <- as.name(glue(".diff#_#lead#_#{control_day}"))
    outcome_var <- as.name(outcome_var)

    ## Day can be a control day if the HeatRisk score is low enough (None or minor),
    ## Otherwise it's NA so it's dropped.
    data <- data %>%
      ## Lags
      mutate(
        !!lag_name := ifelse(lag(HeatRisk_num, n = control_day, default = NA) %in% c(0, 1),
          !!outcome_var - lag(!!outcome_var, n = control_day, default = NA),
          NA
        ),
        ## Leads
        !!lead_name := ifelse(lead(HeatRisk_num, n = control_day, default = NA) %in% c(0, 1),
          !!outcome_var - lead(!!outcome_var, n = control_day, default = NA),
          NA
        )
      )
  }

  return(data)
}

################################################################################

FilterDate <- function(data, start_date = NULL, end_date = NULL) {
  ## Filter to start date
  if (!is.null(start_date)) {
    data <- data %>%
      filter(Date >= start_date)
  }

  ## Filter to end date
  if (!is.null(end_date)) {
    data <- data %>%
      filter(Date <= end_date)
  }

  return(data)
}

################################################################################

GetHeatCoefficients <- function(data, current_outcome, other_outcomes,
                                combine_reference = FALSE) {
  current_outcome_name <- as.name(current_outcome)

  if (combine_reference) {
    data <- data %>%
      mutate(
        HeatRisk = as.character(HeatRisk),
        HeatRisk = ifelse(HeatRisk %in% c("None", "Minor"), "None or Minor", HeatRisk),
        HeatRisk = factor(HeatRisk, levels = c("None or Minor", "Moderate", "Major", "Extreme"))
      )
  }

  data <- data %>%
    ## Remove other outcomes so we can pivot
    select(-all_of(other_outcomes)) %>%
    ## Calculate the control mean so we can remove observations with no non-NA controls
    mutate(diff_mean = rowMeans(select(., starts_with(".diff#_#")), na.rm = TRUE)) %>%
    ## Remove treated days with no value, or days where there are no matched controls
    filter(
      !is.na(diff_mean),
      !is.na(!!current_outcome_name)
    ) %>%
    ## Pivot so each row is a control or treatment observation
    pivot_longer(starts_with(".diff#_#")) %>%
    ## Remove controls with no observation
    filter(
      !is.na(value),
      !is.na(HeatRisk),
    ) %>%
    ## Calculate day_ids, by first splitting the control name
    separate_wider_delim(name,
      names = c("control", "lead_lag", "offset"),
      delim = "#_#",
      too_few = "align_start",
      cols_remove = TRUE
    ) %>%
    ## Then assign the day ID as the date
    mutate(
      day_id = as.numeric(Date),
      offset = as.numeric(offset),
      ## Offset the day_id by the lead or lag amount
      day_id = ifelse(lead_lag == "lead", day_id + offset, day_id),
      day_id = ifelse(lead_lag == "lag", day_id - offset, day_id)
    ) %>%
    group_by(Date) %>%
    mutate(weight = 1 / n()) %>%
    ungroup()

  # Run the regression, clustering at the day_id level, to account for repeated observations
  regression_mod <- feols(value ~ HeatRisk + 0,
    cluster = c("Date", "day_id"),
    data = data,
    weights = ~weight
  )

  ## Use tidy to get the regresssion coefficients in a data.frame and estimate
  ## confidence intervals
  regression_coef <- tidy(regression_mod,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    ## Remove HeatRisk from terms and instead make it the column title
    mutate(term = gsub("^HeatRisk", "", term)) %>%
    rename(
      HeatRisk = term,
      Estimate = estimate
    ) %>%
    mutate(across(c(Estimate, std.error, conf.low, conf.high), \(x) signif(x, digits = 4)))

  if (combine_reference) {
    regression_coef <- regression_coef %>%
      ## Set order of HeatRisk for plotting
      mutate(HeatRisk = factor(HeatRisk, levels = c("None or Minor", "Moderate", "Major", "Extreme")))
  } else {
    regression_coef <- regression_coef %>%
      ## Set order of HeatRisk for plotting
      mutate(HeatRisk = factor(HeatRisk, levels = c("None", "Minor", "Moderate", "Major", "Extreme")))
  }

  ## Report percentage change
  percentage_change <- data %>%
    group_by(Date) %>%
    ## denominator is the control observation
    mutate(control_obs := !!current_outcome_name - value) %>%
    ## Caluclate weighted average of difference and control observation
    group_by(HeatRisk) %>%
    summarize(mean_denom := sum(control_obs * weight) / sum(weight),
      mean_diff = sum(value * weight) / sum(weight)
    ) %>%
    ## Percentage change is one divided by the other over the entire dataset
    ## This is more robust to small numbers
    mutate(percent_change = mean_diff / mean_denom) %>%
    select(HeatRisk, percent_change)

  regression_coef <- regression_coef %>%
    left_join(percentage_change, by = "HeatRisk")


  return(regression_coef)
}

################################################################################

FormatCoefficientTable <- function(data, heat_coefficients, current_outcome) {
  daily_outcome_column_name <- as.name(glue("Daily change in {current_outcome}"))
  total_outcome_column_name <- as.name(glue("Total change in {current_outcome}"))
  median_outcome_column_name <- as.name(glue("Median {current_outcome}\n(IQR)"))
  current_outcome_name <- as.name(current_outcome)

  hr_stats <- data %>%
    filter(!is.na(!!current_outcome_name)) %>%
    group_by(HeatRisk) %>%
    summarize(
      days := n(),
      median := median(!!current_outcome_name, na.rm = TRUE),
      iqr := IQR(!!current_outcome_name, na.rm = TRUE)
    ) %>%
    filter(HeatRisk %in% c("Moderate", "Major", "Extreme"))

  coefficient_table <- heat_coefficients %>%
    inner_join(hr_stats, by = "HeatRisk") %>%
    mutate(!!median_outcome_column_name := glue("{median}</br>
                                               ({iqr})"),
      !!daily_outcome_column_name := glue("{Estimate}</br>
                                               [{conf.low}, {conf.high}]"),
      !!total_outcome_column_name := glue("{signif(Estimate * days, digits = 4)}</br>
                                               [{signif(conf.low * days, digits = 4)}, {signif(conf.high * days, digits = 4)}]"),
      `Percent change` = scales::percent(percent_change, accuracy = 0.1)
    ) %>%
    rename(`Observed days` = days) %>%
    select(
      HeatRisk, `Observed days`, !!median_outcome_column_name, !!daily_outcome_column_name,
      !!total_outcome_column_name, `Percent change`
    )

  return(coefficient_table)
}

################################################################################

CustomToolTheme <- function() {
  theme_grey(base_size = 14, base_family = "") %+replace%
    theme(
      text = element_text(
        family = "", face = "plain",
        color = "black", size = 14, hjust = 0.5,
        vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(),
        debug = FALSE
      ),
      axis.line = element_line(
        color = "black",
        linewidth = 0.5, lineend = "square"
      ),
      axis.text = element_text(
        color = "black",
        size = 12.5
      ),
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.3
      ),
      axis.ticks.length = unit(3, "pt"),
      legend.background = element_blank(),
      legend.spacing = unit(14, "pt"), legend.margin = margin(
        0, 0, 0, 0
      ),
      legend.key = element_blank(), legend.key.size = unit(16, "pt"),
      legend.text = element_text(size = rel(6 / 7)),
      legend.justification = c(
        "left",
        "center"
      ),
      legend.box.margin = margin(
        0, 0, 0, 0
      ),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(14, "pt"), panel.background = element_blank(),
      complete = TRUE
    )
}


################################################################################

PlotTimeline <- function(data,
                         outcome_var,
                         plot_var) {
  ## Make sure outcome var is a name for dynamic variable
  outcome_var <- as.name(outcome_var)

  ## Plot
  timeseries_fig <- data %>%
    ggplot(aes(x = Date, y = !!outcome_var)) +
    geom_col(aes(fill = HeatRisk), color = NA) +

    #### Axis formatting
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +

    ## Set axis labels
    xlab("") +

    ## Manually specify colors
    scale_fill_manual(
      name = "HeatRisk",
      values = plot_var$heatrisk_colors
    ) +
    ## Theme
    CustomToolTheme()

  ## Covert to plotly plot
  timeseries_fig <- ggplotly(timeseries_fig) %>%
    ## Fix axes
    layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    ) %>%
    ## Remove modebar
    config(displayModeBar = FALSE)

  return(timeseries_fig)
}

################################################################################

PlotCoef <- function(regression_coef,
                     outcome_var,
                     plot_var) {
  #### Create plot
  estimates_fig <- regression_coef %>%
    ggplot(aes(x = HeatRisk, y = Estimate, color = HeatRisk, group = HeatRisk)) +
    ## Add points
    geom_point(size = 1.5) +
    ## Add error bars
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6) +
    ## Add a horizontal line at 1
    geom_hline(yintercept = 0, linetype = "dashed") +

    #### Axis formatting
    ## Set axis labels
    xlab("HeatRisk") +
    ylab(outcome_var) +
    ## Set colors
    scale_color_manual(
      name = "HeatRisk",
      values = plot_var$heatrisk_colors
    ) +
    ## Theme and remove legend
    CustomToolTheme() +
    theme(legend.position = "none")

  ## Convert to plotly plot
  estimates_fig <- ggplotly(estimates_fig,
    ## Set tooltip manually to avoid duplication
    tooltip = c("color", "y")
  ) %>%
    ## Fix axes
    layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    ) %>%
    ## Remove modebar
    config(displayModeBar = FALSE)

  return(estimates_fig)
}
