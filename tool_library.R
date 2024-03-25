## Import Packages
library(cowplot)
library(DT)
library(plotly)
library(ggplot2)
library(broom)
library(lubridate)
library(purrr)
library(glue)
library(dplyr)
library(tidyr)
library(shiny)

################################################################################

plot_var <- list()
plot_var$heatrisk_colors <- c("None" = "palegreen3",
                              "Minor" = "darkgoldenrod2",
                              "Moderate" = "darkorange2",
                              "Major" = "red1",
                              "Extreme" = "purple4")

################################################################################

FormatData <- function(data){
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
      Date = mdy(Date)) %>%
    ## Make sure that all dates exist in data
    complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
    arrange(Date)
  
  return(data)
}

################################################################################

CalculateControlMean <- function(data, control_days, outcome_var){
  
  ## Create lead and lag control for each control day
  for(control_day in control_days){
    
    ## Create names for dynamic variables
    lag_name <- as.name(glue(".control_lag_{control_day}"))
    lead_name <- as.name(glue(".control_lead_{control_day}"))
    outcome_var <- as.name(outcome_var)
    
    ## Day can be a control day if the HeatRisk score is low enough (None or minor),
    ## Otherwise it's NA so it's dropped.
    data <- data %>%
      ## Lags
      mutate(!!lag_name := ifelse(lag(HeatRisk_num, n = control_day, default = NA) %in% c(0, 1), 
                                  lag(!!outcome_var, n = control_day, default = NA),
                                  NA),
             ## Leads
             !!lead_name := ifelse(lead(HeatRisk_num, n = control_day, default = NA) %in% c(0, 1), 
                                   lead(!!outcome_var, n = control_day, default = NA),
                                   NA))
  }
  
  ## Calculate control mean
  data <- data %>%
    ## Control mean is the mean of all controls, removing NAs
    mutate(control_mean = rowMeans(select(., starts_with(".control_")), na.rm = TRUE),
           ## The difference is the effect
           diff := !!outcome_var - control_mean) %>%
    ## Remove invidividual control observations
    select(-starts_with(".control_"))
  
  return(data)
}

################################################################################

FilterDate <- function(data, start_date = NULL, end_date = NULL){
  
  ## Filter to start date
  if(!is.null(start_date)){
    data <- data %>%
      filter(Date >= start_date)
  }
  
  ## Filter to end date
  if(!is.null(end_date)){
    data <- data %>%
      filter(Date <= end_date)
  }
  
  return(data)
}

################################################################################

GetHeatCoefficients <- function(data){
  
  ## Fit regression model, "+0" tells it not to add an intercept
  regression_mod <- lm("diff ~ HeatRisk + 0", data = data)
  
  ## Use tidy to get the regresssion coefficients in a data.frame and estimate 
  ## confidence intervals
  regression_coef <- tidy(regression_mod, conf.int = TRUE,
                          conf.level = 0.95) %>%
    ## Remove HeatRisk from terms and instead make it the column title
    mutate(term = gsub("^HeatRisk", "", term)) %>%
    rename(HeatRisk = term) %>%
    ## Set order of HeatRisk for plotting
    mutate(HeatRisk = factor(HeatRisk, levels = c("None", "Minor", "Moderate", "Major", "Extreme"))) %>%
    rename(Estimate = estimate) %>%
    mutate(across(c(Estimate, std.error, conf.low, conf.high), \(x) signif(x, digits = 4)))
  
  return(regression_coef)
}

################################################################################

FormatCoefficientTable <- function(data, heat_coefficients, current_outcome){
  
  daily_outcome_column_name <- as.name(glue("Daily change in {current_outcome}"))
  total_outcome_column_name <- as.name(glue("Total change in {current_outcome}"))
  
  num_days <- data %>%
    group_by(HeatRisk) %>%
    count() %>%
    filter(HeatRisk %in% c("Moderate", "Major", "Extreme")) %>%
    rename(days = n)
  
  coefficient_table <- heat_coefficients %>%
    inner_join(num_days, by = "HeatRisk") %>%
    mutate(!!daily_outcome_column_name := glue("{Estimate}</br>
                                               [{conf.low}, {conf.high}]"),
           !!total_outcome_column_name := glue("{signif(Estimate * days, digits = 4)}</br>
                                               [{signif(conf.low * days, digits = 4)}, {signif(conf.high * days, digits = 4)}]")) %>%
    rename(`Observed days` = days) %>%
    select(HeatRisk, !!daily_outcome_column_name,
           `Observed days`, !!total_outcome_column_name)
  
}

################################################################################

PlotTimeline <- function(data,
                         outcome_var,
                         plot_var){
  
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
    scale_fill_manual(name = "HeatRisk",
                      values = plot_var$heatrisk_colors) +
    ## Theme
    theme_cowplot()
  
  ## Covert to plotly plot
  timeseries_fig <- ggplotly(timeseries_fig) %>%
    ## Fix axes
    layout(xaxis = list(fixedrange = TRUE), 
           yaxis = list(fixedrange = TRUE)) %>%
    ## Remove modebar
    config(displayModeBar = FALSE)
  
  return(timeseries_fig)
}

################################################################################

PlotCoef <- function(regression_coef,
                          outcome_var,
                          plot_var){
  
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
    scale_color_manual(name = "HeatRisk",
                       values = plot_var$heatrisk_colors) +
    ## Theme and remove legend
    theme_cowplot() +
    theme(legend.position = "none")
  
  ## Convert to plotly plot
  estimates_fig <- ggplotly(estimates_fig, 
                            ## Set tooltip manually to avoid duplication
                            tooltip=c("color", "y")) %>%
    ## Fix axes
    layout(xaxis = list(fixedrange = TRUE), 
           yaxis = list(fixedrange = TRUE)) %>%
    ## Remove modebar
    config(displayModeBar = FALSE)
  
  return(estimates_fig)
}
