source("./tool_library.R", local = TRUE)

ui <- fluidPage(
  title = "HeatRisk in LA County",
  fluidPage(
    h2("How does heat affect Los Angeles County?"),
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
            h3("Upload dataset"),
            # Input: Select a file ----
            fileInput("data_file", "Choose CSV File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),

            uiOutput("data_controls")
            
          ),
        )
      ),
    
    mainPanel(uiOutput("main_panel"))
    
    
    )
  )
)


server <- function(input, output, session) {
  
  base_data <- reactive({
    
    tryCatch(
      {
        base_data <- read.csv(input$data_file$datapath, check.names=FALSE)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    base_data <- FormatData(data = base_data)
    
  })
    
  ## Get outcome names
  outcome_names <- reactive({colnames(base_data()[!(colnames(base_data()) %in% c("Date", "HeatRisk", "HeatRisk_num"))])}) 
   
  observe({
    updateSelectInput(session, "current_outcome", 
                      choices = outcome_names(),
                      select = outcome_names()[1])
    
    updateSliderInput(session, "filter_dates",
                      min = min(base_data()$Date),
                      max = max(base_data()$Date),
                      value = c(min(base_data()$Date), max(base_data()$Date))
                      )
  }) %>% bindEvent(input$data_file)
  
  control_days <- reactive({seq(from = 7, to = 7 * as.numeric(input$weeks_of_controls), by = 7)})

    


    ## Get current data based on outcome and date ranges
    current_data <- reactive({
      
      data <- CalculateControlMean(
        data = base_data(),
        control_days = control_days(),
        outcome_var = input$current_outcome
        )

      data <- FilterDate(
        data =  data,
        start_date = input$filter_dates[1],
        end_date = input$filter_dates[2]
      )

      return(data)
      }) 
      
    heat_coefficients <- reactive({GetHeatCoefficients(data = current_data())})
      
    coefficient_table <- reactive({FormatCoefficientTable(data = current_data(), 
                                                          heat_coefficients = heat_coefficients(), 
                                                          current_outcome = input$current_outcome)})
    

      
    timeline_plot <- reactive({
      PlotTimeline(
        data = current_data(),
        outcome_var = input$current_outcome,
        plot_var = plot_var
      )
    }) 
      
    coef_plot <- reactive({

      PlotCoef(
        regression_coef = heat_coefficients(),
        outcome_var = input$current_outcome,
        plot_var = plot_var)
    })
    
  
  output$timeline_plot <- renderPlotly({
    
    # input$file1 will be NULL initially
    
    req(input$data_file)
    
    return(timeline_plot())
    
  })
  
  
  output$coef_plot <- renderPlotly({
    
    # input$file1 will be NULL initially
    
    req(input$data_file)
    
    return(coef_plot())
    
  })
  
  output$coefficient_table <- renderTable({
    
    # input$file1 will be NULL initially
    
    req(input$data_file)
    
    return(coefficient_table())
    
  }, sanitize.text.function=identity)
  
  
  
  output$data_controls <- renderUI({
    
    req(input$data_file)
    
    return(
      list(
        hr(),
        h3("Data controls"),
        selectInput("current_outcome",
                    "Outcome",
                    choices = NULL,
        ),
        sliderInput("filter_dates",
                    "Date range",
                    min = as.Date("2000-01-01", "%Y-%m-%d"),
                    max = as.Date("2001-01-01", "%Y-%m-%d"),
                    value = c(as.Date("2000-01-01", "%Y-%m-%d"), 
                              as.Date("2001-01-01", "%Y-%m-%d")),
                    timeFormat = "%Y-%m-%d"
        ),
        selectInput("weeks_of_controls",
                    "Number of weekly lagging and leading controls",
                    choices = 1:8,
                    selected = 3
        )
      )
      
    )
  })
  
  output$main_panel <- renderUI({

    if(is.null(input$data_file)){

      return(NULL)
    }

    return(

      list(
        fixedRow(
          column(
            12,
            h2("Timeline"),
            # Output: Data file ----
            plotlyOutput("timeline_plot",
                         height = "320px")

          )
        ),
        hr(),
        fixedRow(
          column(
            5,
            h2("Results"),
            tableOutput("coefficient_table")


          ),
          column(
            7,
            # Output: Data file ----
            plotlyOutput("coef_plot",
                         height = "400px")

          )
        )
      )

    )

  })

  
}

shinyApp(ui, server)
