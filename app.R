## Get functions and packages from tool library
source("./tool_library.R", local = TRUE)
## Get blocks of text from tool text file
source("./tool_text.R", local = TRUE)

################################################# UI

ui <- fluidPage(

  ################### Styling
  tags$head(
    tags$style(HTML("
      strong {
        font-size: 1.25em; /* Adjust the size as needed */
      }

      /* Reduce padding around horizontal lines */
      hr {
        margin-top: 0px;
        margin-bottom: 0px;
      }

      /* Reduce lower margins around headings */
      .reduce-margin {
            margin-bottom: 0px;
      }
      
      /* Create div options for a scrollable table if cut off */
      .scrollable-table {
        overflow-x: auto;
        width: 100%;
    "))
  ),
  fluidPage(

    ################### Headings
    title = "Heat Impacts",
    h2("Estimating The Impact of Heat on Public Services"),

    ################### Side-bar
    sidebarLayout(
      position = "left",
      sidebarPanel(
        class = "side-bar-panel",
        tags$style("
          .well {
              background-color:#FFFFFF;
              -webkit-box-shadow:none;
              box-shadow:none;
            }
        "),
        fixedRow(
          column(
            12,
            ########## File input
            strong("Upload CSV dataset"),
            fileInput("data_file",
              label = NULL,
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),

            ########## Controls
            uiOutput("data_controls")
          ),
        )
      ),

      ################### Main panel
      mainPanel(
        tabsetPanel(
          tabPanel(
            "App",
            uiOutput("main_panel")
          ),
          tabPanel(
            "How does this work?",
            tool_text$goal,
            tool_text$data,
            tool_text$methods,
            tool_text$specification_options,
          ),
          tabPanel(
            "License & Attribution",
            tool_text$license_attribution
          )
        )
      )
    )
  )
)


################################################# Server

server <- function(input, output, session) {
  ################### Reading in data

  ## Try reading in file, and return a safe error if it's incorrect
  base_data <- reactive({
    req(input$data_file)
    tryCatch(
      {
        base_data <- read.csv(input$data_file$datapath, check.names = FALSE)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    ## Apply formatting
    base_data <- FormatData(data = base_data)

    return(base_data)
  })


  ########## Get outcome names
  outcome_names <- reactive({
    req(input$data_file)
    colnames(base_data()[!(colnames(base_data()) %in% c("Date", "HeatRisk", "HeatRisk_num"))])
  })
  other_outcomes <- reactive({
    req(input$data_file)
    setdiff(outcome_names(), input$current_outcome)
  })

  holiday_dates <- reactive({
    req(input$data_file)
    ymd(federalHolidays(
      years = year(min(base_data()$Date)):year(max(base_data()$Date)),
      businessOnly = FALSE
    ))
  })

  exclusion_data <- reactive({
    req(input$data_file)
    exclusion_data <- base_data()

    if (input$holiday_exclude) {
      exclusion_data <- HolidaysToNA(
        data = exclusion_data,
        holiday_dates = holiday_dates()
      )
    }

    if (input$weekend_exclude) {
      exclusion_data <- WeekendsToNA(data = exclusion_data)
    }

    return(exclusion_data)
  })

  current_outcome_dates <- reactive({
    req(input$data_file, input$current_outcome)

    ## Find the minimum and maximum date based on this outcome
    current_outcome_dates <- base_data() %>%
      select(Date, all_of(input$current_outcome)) %>%
      filter(!is.na(!!sym(input$current_outcome)))

    current_outcome_dates <- list(
      min =  min(current_outcome_dates$Date, na.rm = TRUE),
      max =  max(current_outcome_dates$Date, na.rm = TRUE)
    )

    return(current_outcome_dates)
  })

  ################### Upon new data, update buttons
  observe({
    ## Stops error when file is reuploaded
    freezeReactiveValue(input, "current_outcome")
    ########## Set the selection options to the outcomes, and select the first
    updateSelectInput(session, "current_outcome",
      choices = outcome_names(),
      select = outcome_names()[1]
    )

  }) %>% bindEvent(input$data_file)

  ########## Update dates
  observe({
    ## Ensures that dates are only changed once, avoiding stuttering
    freezeReactiveValue(input, "date_fields")
    updateDateRangeInput(session, "date_fields",
      min = current_outcome_dates()$min,
      max = current_outcome_dates()$max,
      start = current_outcome_dates()$min,
      end = current_outcome_dates()$max
    )
  }) %>% bindEvent(current_outcome_dates())

  ################### Update current data based on controls, outcome, and dates

  ########## Control weeks update
  control_days <- reactive({
    seq(from = 7, to = 7 * as.numeric(input$weeks_of_controls), by = 7)
  })

  ########## Update data
  current_data <- reactive({
    ## Get control observations
    data <- GetControlObservations(
      data = exclusion_data(),
      control_days = control_days(),
      outcome_var = input$current_outcome
    )

    ## Filter dates
    data <- FilterDate(
      data = data,
      start_date = input$date_fields[1],
      end_date = input$date_fields[2]
    )
    return(data)
  }) %>% throttle(500)

  ########## Fit models and get coefficients
  heat_coefficients <- reactive({
    GetHeatCoefficients(
      data = current_data(),
      current_outcome = input$current_outcome,
      combine_reference = input$combine_reference,
      other_outcomes = other_outcomes()
    )
  })

  ################### Create tables and plots based on current data

  ########## Compile coefficient table
  coefficient_table <- reactive({
    FormatCoefficientTable(
      data = current_data(),
      heat_coefficients = heat_coefficients(),
      current_outcome = input$current_outcome
    )
  })

  ########## Create timeline plot
  timeline_plot <- reactive({
    PlotTimeline(
      data = current_data(),
      outcome_var = input$current_outcome,
      plot_var = plot_var
    )
  })

  ########## Create coefficient plot
  coef_plot <- reactive({
    PlotCoef(
      regression_coef = heat_coefficients(),
      outcome_var = input$current_outcome,
      plot_var = plot_var
    )
  })


  ################### Render outputs

  ########## Output timeline
  output$timeline_plot <- renderPlotly({
    ## Only run if there is input data
    req(input$data_file)

    return(timeline_plot())
  })

  ########## Output coefficient plot
  output$coef_plot <- renderPlotly({
    ## Only run if there is input data
    req(input$data_file)

    return(coef_plot())
  })

  ########## Output coefficient table
 output$coefficient_table <- renderTable(
    {
      ## Only run if there is input data
      req(input$data_file)

      return(coefficient_table())
    },
    ## Makes sure linebreaks are not removed
    sanitize.text.function = identity,
    width = "100%"
  )
 
  ########## Example table
  output$example_table <- renderTable({
    example_table <- tool_text$example_table

    return(example_table)
  })


  ################################################# Reactive UI elements


  ################### Specify data controls
  output$data_controls <- renderUI({
    ## Only run if there is input data
    req(input$data_file)

    return(
      list(
        hr(),
        h3("Specification Options"),
        strong("Outcome"),
        ########## Outcome
        selectInput("current_outcome",
          label = NULL,
          choices = NULL,
        ),

        ########## Dates, default values are placeholders
        strong("Dates"),
        dateRangeInput("date_fields",
          label = NULL,
          start = as.Date("2000-01-01", "%Y-%m-%d"),
          end = as.Date("2001-01-01", "%Y-%m-%d"),
          format = "yyyy-m-d"
        ),

        ########## Weeks of controls
        strong("Number of weekly lagging and leading controls"),
        selectInput("weeks_of_controls",
          label = NULL,
          choices = 1:6,
          selected = 3
        ),

        ########## Exclude holidays
        strong("Exclusions"),
        checkboxInput("holiday_exclude",
          "Exclude federal holidays from calculations",
          value = FALSE
        ),
        checkboxInput("weekend_exclude",
          "Exclude weekends from calculations",
          value = FALSE
        ),

        ########## Reference group
        strong("Reference group"),
        checkboxInput("combine_reference",
          "Combine None and Minor categories",
          value = TRUE
        )
      )
    )
  })

  ################### Main panel before and after data is uploaded
  output$main_panel <- renderUI({
    ########## Before data is uploaded
    if (is.null(input$data_file)) {
      return(
        list(
          tool_text$introduction,
          tool_text$file_upload_format
        )
      )
    }

    ########## After data is uploaded
    return(
      list(

        ## Timeline plot
        fixedRow(
          column(
            12,
            h2("Timeline", class = "reduce-margin"),
            # Output: Data file ----
            plotlyOutput("timeline_plot",
              height = "320px"
            )
          )
        ),

        ## Coefficient table
        hr(),
        fixedRow(
          column(
            6,
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h2("Results"),
              downloadButton("download_results", "Download results table")
            ),
            div(class = "scrollable-table", tableOutput("coefficient_table"))
          ),

          ## Coefficient plot
          column(
            6,
            # Output: Data file ----
            plotlyOutput("coef_plot",
              height = "380px"
            )
          )
        )
      )
    )
  })


  output$download_results <- downloadHandler(
    filename = function() {
      paste("heatrisk_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      ## Clean the HTML line breaks
      cleaned_table <- coefficient_table() %>%
        mutate(across(everything(), ~ gsub("</?br\\s*/?>", "", .)))

      write.csv(cleaned_table, file, row.names = FALSE)
    }
  )
}

################################################# Run app
shinyApp(ui, server)
