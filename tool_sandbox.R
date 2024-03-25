## To do
## Check that no input columns start with control
## Give it a default set of HeatRisk data to join with

## Clear objects
remove(list = objects())

## Source library
source("./tool_library.R")


start_date <- ymd("2010-01-01")
end_date <- NULL
weeks_of_controls <- 3

control_days <- seq(from = 7, to = 7 * weeks_of_controls, by = 7)

data <- list()
heat_coefficients <- list()
coefficient_table <- list()
timeline_plots <- list()
coef_plots <- list()

### Read in data
set.seed(1)
# heatrisk <- read.csv("./data/heat_risk_v2-5.csv")
# outcome <- read.csv("./data/coroner_mortality.csv")
data$base <- read.csv("./data/input_data_1.csv", check.names = FALSE)

## Get outcome names
outcome_names <- colnames(data$base)[!(colnames(data$base) %in% c("Date", "HeatRisk"))]

data$base <- FormatData(data = data$base)

for (current_outcome in outcome_names) {
  data[[current_outcome]] <- CalculateControlMean(
    data = data$base,
    control_days = control_days,
    outcome_var = current_outcome
  )


  data[[current_outcome]] <- FilterDate(
    data = data[[current_outcome]],
    start_date = start_date,
    end_date = end_date
  )

heat_coefficients[[current_outcome]] <- GetHeatCoefficients(data = data[[current_outcome]])
  
coefficient_table[[current_outcome]] <- FormatCoefficientTable(data = data[[current_outcome]], 
                                                               heat_coefficients = heat_coefficients[[current_outcome]], 
                                                               current_outcome = current_outcome)
  


  timeline_plots[[current_outcome]] <- PlotTimeline(
    data = data[[current_outcome]],
    outcome_var = current_outcome,
    plot_var = plot_var
  )

  coef_plots[[current_outcome]] <- PlotCoef(
    regression_coef = heat_coefficients[[current_outcome]],
    outcome_var = current_outcome,
    plot_var = plot_var
  )
}
