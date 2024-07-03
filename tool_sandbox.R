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
exclude_holidays <- FALSE
combine_reference <- TRUE

control_days <- seq(from = 7, to = 7 * weeks_of_controls, by = 7)

### Read in data
base_data <- read.csv("./data/input_data_1.csv", check.names = FALSE)

## Get outcome names
outcome_names <- colnames(base_data)[!(colnames(base_data) %in% c("Date", "HeatRisk"))]
current_outcome <- "EMS Calls"
other_outcomes <- setdiff(outcome_names, current_outcome)

base_data <- FormatData(data = base_data)

## Check
base_data %>%
  filter(HeatRisk_num == 4) 

holiday_dates <- ymd(federalHolidays(
  years = year(min(base_data$Date)):year(max(base_data$Date)),
  businessOnly = FALSE)
  )

if(exclude_holidays){
  base_data <- HolidaysToNA(data = base_data, 
                            holiday_dates = holiday_dates)
}

data <- GetControlObservations(
    data = base_data,
    control_days = control_days,
    outcome_var = current_outcome
)

# data <- CalculateControlMeans(
#   data = data,
#   outcome_var = current_outcome
# )

data <- FilterDate(
  data = data,
  start_date = start_date,
  end_date = end_date
)

heat_coefficients <- GetHeatCoefficients(data = data,
                                         current_outcome = current_outcome,
                                         other_outcomes = other_outcomes,
                                         combine_reference = TRUE)
  
coefficient_table <- FormatCoefficientTable(data = data, 
                                            heat_coefficients = heat_coefficients, 
                                            current_outcome = current_outcome)
  

PlotTimeline(
    data = data,
    outcome_var = current_outcome,
    plot_var = plot_var
  )

PlotCoef(
    regression_coef = heat_coefficients,
    outcome_var = current_outcome,
    plot_var = plot_var
  )
