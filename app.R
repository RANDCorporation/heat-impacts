## Get functions and packages from tool library
source("./tool_library.R", local = TRUE)

################################################# UI

ui <- fluidPage(
  fluidPage(

    ################### Headings
    title = "HeatRisk in LA County",
    h2("How does heat affect Los Angeles County?"),

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
            h3("Upload dataset"),
            fileInput("data_file", "Choose CSV File",
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
  })

  
  ########## Get outcome names
  outcome_names <- reactive({
    colnames(base_data()[!(colnames(base_data()) %in% c("Date", "HeatRisk", "HeatRisk_num"))])
  })

  ################### Upon new data, update buttons  
  observe({
    ########## Set the selection options to the outcomes, and select the first
    updateSelectInput(session, "current_outcome",
      choices = outcome_names(),
      select = outcome_names()[1]
    )

    ########## Set the selection options to the outcomes, and select the first
    updateSliderInput(session, "filter_dates",
      min = min(base_data()$Date),
      max = max(base_data()$Date),
      value = c(min(base_data()$Date), max(base_data()$Date))
    )
  }) %>% bindEvent(input$data_file)

  
  ################### Update current data based on controls, outcome, and dates
  
  ########## Control weeks update
  control_days <- reactive({
    seq(from = 7, to = 7 * as.numeric(input$weeks_of_controls), by = 7)
  })

  ########## Update data
  current_data <- reactive({
    
    ## Calculate means
    data <- CalculateControlMean(
      data = base_data(),
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
    GetHeatCoefficients(data = current_data())
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
                                "Coronor Deaths" = c("36", "21", "...", "42"), 
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
        
        ########## Outcome
        selectInput("current_outcome",
          "Outcome",
          choices = NULL,
        ),
        
        ########## Dates, default values are placeholders
        sliderInput("filter_dates",
          "Date range",
          min = as.Date("2000-01-01", "%Y-%m-%d"),
          max = as.Date("2001-01-01", "%Y-%m-%d"),
          value = c(
            as.Date("2000-01-01", "%Y-%m-%d"),
            as.Date("2001-01-01", "%Y-%m-%d")
          ),
          timeFormat = "%Y-%m-%d"
        ),
        
        ########## Weeks of controls
        selectInput("weeks_of_controls",
          "Number of weekly lagging and leading controls",
          choices = 1:8,
          selected = 3
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
            5,
            h2("Results"),
            tableOutput("coefficient_table")
          ),
          
          ## Coefficient plot
          column(
            7,
            # Output: Data file ----
            plotlyOutput("coef_plot",
              height = "400px"
            )
          )
        )
      )
    )
  })
}

################################################# Run app
shinyApp(ui, server)
