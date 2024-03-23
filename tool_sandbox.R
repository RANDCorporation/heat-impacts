## To do
## Check that no input columns start with control


## Clear objects
remove(list = objects())
## Set here address
library(here)
## Import Packages
library(cowplot)
library(plotly)
library(ggplot2)
library(broom)
library(lubridate)
library(purrr)
library(glue)
library(dplyr)
library(tidyr)


start_date <- ymd("2010-01-01")
end_date <- ymd("2023-01-01")
weeks_of_controls <- 3

control_days <- seq(from = 7, to = 7*weeks_of_controls, by = 7)


### Read in data
heatrisk <- read.csv("./data/heat_risk_v2-5.csv")
outcome <- read.csv("./data/coroner_mortality.csv")

## Get outcome names
outcome_names <- colnames(outcome)[colnames(outcome) != "Date"]
## Set to the first for now
current_outcome <- outcome_names[1]
current_outcome <- as.name(current_outcome)

### Join data
data <- heatrisk %>%
  inner_join(outcome, by = "Date") %>%
  rename(HeatRisk_num = HeatRisk) %>%
  mutate(
    HeatRisk = case_match(HeatRisk_num,
                          0 ~ "None",
                          1 ~ "Minor",
                          2 ~ "Moderate",
                          3 ~ "Major",
                          4 ~ "Extreme",
                          .default = NA
    ),
  ## Change to factor
  HeatRisk = factor(HeatRisk, levels = c("None", "Minor", "Moderate", "Major", "Extreme")),
  Date = as_date(Date)) %>%
  ## Make sure that all dates exist in data
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  arrange(Date)

for(control_day in control_days){
  
  lag_name <- as.name(glue(".control_lag_{control_day}"))
  lead_name <- as.name(glue(".control_lead_{control_day}"))
  
  data <- data %>%
    mutate(!!lag_name := ifelse(lag(HeatRisk_num, n = control_day, default = NA) %in% c(0, 1), 
                                  lag(Deaths, n = control_day, default = NA),
                                  NA),
           !!lead_name := ifelse(lead(HeatRisk_num, n = control_day, default = NA) %in% c(0, 1), 
                                   lead(Deaths, n = control_day, default = NA),
                                   NA))
  
}

data <- data %>%
  mutate(control_mean = rowMeans(select(., starts_with(".control_")), na.rm = TRUE),
         diff = Deaths - control_mean) %>%
  select(-starts_with(".control_"))


if(!is.null(start_date)){
  data <- data %>%
    filter(Date >= start_date)
}

if(!is.null(end_date)){
  data <- data %>%
    filter(Date <= end_date)
}

regression_mod <- lm("diff ~ HeatRisk + 0", data = data)
regression_coef <- tidy(regression_mod, conf.int = TRUE,
                        conf.level = 0.95) %>%
  mutate(term = gsub("^HeatRisk", "", term)) %>%
  rename(HeatRisk = term) %>%
  mutate(HeatRisk = factor(HeatRisk, levels = c("None", "Minor", "Moderate", "Major", "Extreme")))


#### Create plot
estimates_fig <- regression_coef %>%
  ggplot(aes(x = HeatRisk, y = estimate, color = HeatRisk, group = HeatRisk)) +
  ## Add points
  geom_point(size = 1.5) +
  ## Add error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6) +
  ## Add a horizontal line at 1
  geom_hline(yintercept = 0
             , linetype = "dashed") +
  
  
  #### Axis formatting
  ## Set axis labels
  xlab("HeatRisk") +
  ylab("Mortality relative to no HeatRisk") +
  
  scale_color_manual(name = "HeatRisk",
                    values = c("None" = "palegreen3",
                               "Minor" = "darkgoldenrod2",
                               "Moderate" = "darkorange2",
                               "Major" = "red1",
                               "Extreme" = "purple4")) +
  theme_cowplot()

ggplotly(estimates_fig) %>%
  layout(xaxis = list(fixedrange = TRUE), 
         yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE)


timeseries_fig <- data %>%
  ggplot(aes(x = Date, y = !!current_outcome)) +
  geom_col(aes(fill = HeatRisk), color = NA, linewidth = 0.1) +
  
  #### Axis formatting
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  ## Set axis labels
  xlab("") +
  
  ## Manually specify colors
  scale_fill_manual(name = "HeatRisk",
                    values = c("None" = "palegreen3",
                               "Minor" = "darkgoldenrod2",
                               "Moderate" = "darkorange2",
                               "Major" = "red1",
                               "Extreme" = "purple4")) +
  ## Theme
  theme_cowplot()

ggplotly(timeseries_fig) %>%
  layout(xaxis = list(fixedrange = TRUE), 
         yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE)
