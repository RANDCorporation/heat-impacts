## Get functions and packages from tool library
source("./tool_library.R", local = TRUE)

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
    "))
  ),
  
  fluidPage(

    ################### Headings
    title = "Heat Impacts",
    h2("Estimating Heat Impacts"),
    h3( tags$a(href="https://docs.posit.co/shinyapps.io/security-and-compliance.html", 
               "Shinyapps security and compliance policy")),

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
      mainPanel(uiOutput("main_panel"))
    )
  )
)


################################################# Server

server <- function(input, output, session) {
  
  ################### Reading in data
  
  ## Try reading in file, and return a safe error if it's incorrect
  base_data <- reactive({
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
    colnames(base_data()[!(colnames(base_data()) %in% c("Date", "HeatRisk", "HeatRisk_num"))])
  })
  other_outcomes <- reactive({
    setdiff(outcome_names(), input$current_outcome)
  })
  
  holiday_dates <- reactive({
    ymd(federalHolidays(years = year(min(base_data()$Date)):year(max(base_data()$Date)),
    businessOnly = FALSE)
    )
  })
  
  exclusion_data <- reactive({
    
    exclusion_data <- base_data()
    
    if(input$holiday_exclude){
      exclusion_data <- HolidaysToNA(data = exclusion_data, 
                                    holiday_dates = holiday_dates())
      
    }
    
    if(input$weekend_exclude){
      exclusion_data <- WeekendsToNA(data = exclusion_data)
    }
    
    return(exclusion_data)
  })

  ################### Upon new data, update buttons  
  observe({
    ########## Set the selection options to the outcomes, and select the first
    updateSelectInput(session, "current_outcome",
      choices = outcome_names(),
      select = outcome_names()[1]
    )

    ########## Update dates
    updateDateRangeInput(session, "date_fields",
                         min = min(base_data()$Date),
                         max = max(base_data()$Date),
                         start = min(base_data()$Date),
                         end = max(base_data()$Date))
    
    updateSliderInput(session, "filter_dates",
      min = min(base_data()$Date),
      max = max(base_data()$Date),
      value = c(min(base_data()$Date), max(base_data()$Date))
    )
    
  }) %>% bindEvent(input$data_file)
  
  ## When new dates are entered into the date fields, update the slider
  observe({


    ########## Set the selection options to the outcomes, and select the first
    updateSliderInput(session, "filter_dates",
                      min = min(base_data()$Date),
                      max = max(base_data()$Date),
                      value = c(input$date_fields[1], input$date_fields[2])
                      )
  }) %>% bindEvent(input$date_fields)
  
  ## When new dates are entered into the slider, update the date fields
  observe({
    
    
    ########## Set the selection options to the outcomes, and select the first
    updateDateRangeInput(session, "date_fields",
                      min = min(base_data()$Date),
                      max = max(base_data()$Date),
                      start = input$filter_dates[1],
                      end = input$filter_dates[2]
    )
    
  }) %>% bindEvent(input$filter_dates)

  
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
      start_date = input$filter_dates[1],
      end_date = input$filter_dates[2]
    )
    return(data)
  })

  ########## Fit models and get coefficients
  heat_coefficients <- reactive({
    GetHeatCoefficients(data = current_data(),
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
    sanitize.text.function = identity
  )

  ########## Example table
  output$example_table <- renderTable({
    
    example_table <- data.frame("Date" = c("1/1/2015", "1/2/2015", "...", "12/31/2023"),
                                "HeatRisk" = c("0", "2", "...", "1"),
                                "EMS Calls" = c("834", "775", "...", "903"),
                                "Mental Health Crises" = c("79", "106", "...", "66"),
                                "Medical Examiner Deaths" = c("36", "21", "...", "42"), 
                                check.names = FALSE)
      
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
        h3("Data controls"),
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

        sliderInput("filter_dates",
          label = NULL,
          min = as.Date("2000-01-01", "%Y-%m-%d"),
          max = as.Date("2001-01-01", "%Y-%m-%d"),
          value = c(
            as.Date("2000-01-01", "%Y-%m-%d"),
            as.Date("2001-01-01", "%Y-%m-%d")
          ),
          timeFormat = "%Y-%m-%d"
        ),
        
        ########## Weeks of controls
        strong("Number of weekly lagging and leading controls"),
        selectInput("weeks_of_controls",
          label = NULL,
          choices = 1:6,
          selected = 3
        ),
        
        ########## Reference group
        strong("Reference group"),
        checkboxInput("combine_reference",
                      "Combine None and Minor categories",
                      value = FALSE
        ),
        
        ########## Exclude holidays
        strong("Exclusions"),
        checkboxInput("holiday_exclude",
                      "Exclude federal holidays from calculations",
                      value = FALSE),
        checkboxInput("weekend_exclude",
                      "Exclude weekends from calculations",
                      value = FALSE)
      )
    )
  })

  ################### Main panel before and after data is uploaded
  output$main_panel <- renderUI({
    
    ########## Before data is uploaded
    if (is.null(input$data_file)) {
      return(
        list(
         h2("File upload format"),
         p("This is some placeholder text"),
         p("The file you upload should be a .CSV with the first two columns called 
         'Date' and 'HeatRisk', subsequent columns should contain numeric outcome counts."),
         p("The column headings for these outcome columns will be used on figures.
           You can have as many outcome columns as you like, and you can switch between
           them. Dates should be in a 'MM/DD/YYYY' format. HeatRisk should be numeric, running
           from 0 (None) to 4 (Extreme). An example table structure is:"),
         
         tableOutput("example_table")
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
            h2("Timeline"),
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
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h2("Results"),
                downloadButton("download_results", "Download results table")
            ),
            tableOutput("coefficient_table")
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
      paste("heatrisk_results_", Sys.Date(), ".csv", sep="")
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
