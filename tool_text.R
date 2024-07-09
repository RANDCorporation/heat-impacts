tool_text <- list()

tool_text$example_table <- data.frame(
  "Date" = c("1/1/2015", "1/2/2015", "...", "12/31/2023"),
  "HeatRisk" = c("0", "2", "...", "1"),
  "EMS Calls" = c("834", "775", "...", "903"),
  "Mental Health Crises" = c("79", "106", "...", "66"),
  "Medical Examiner Deaths" = c("", "", "...", "42"),
  check.names = FALSE
)

tool_text$introduction <- list(
  h2("Introduction"),
  p("This app is designed to help local government departments estimate how heat
    impacts their operations and clients. It uses a matching method, comparing
    hot days with similar cooler days, to estimate how HeatRisk (a heat index developed
    by the National Weather Service) is associated with outcomes. To use the app,
    simply upload a .csv file in the format specified below. For more details on
    the methodology, please refer to the \"How does this work?\" tab.")
)


tool_text$file_upload_format <- list(
  h2("File upload format"),
  p("The uploaded file should be in .CSV format. The first two columns must be
    labeled \"Date\" and \"HeatRisk\". The \"Date\" column should contain dates in
    the \"MM/DD/YYYY\" format. The \"HeatRisk\" column should contain numeric values
    ranging from 0 (None) to 4 (Extreme)."),
  p("Subsequent columns should contain numeric outcome counts, with the column
    headings used to name the outcomes. You can include as many outcome columns
    as needed, and you can switch between them during the analysis. If any values
    are missing, the corresponding cells must be left blank. An example table structure is:"),
  tableOutput("example_table"),
  p("In this example, \"EMS Calls\", \"Mental Health Crises\", and \"Medical Examiner Deaths\"
  are the names of the outcome columns,
    and the numeric values represent the corresponding outcome counts for each date and
    HeatRisk level. Note the blank cell in the \"Medical Examiner Deaths\" column, indicating a missing value.")
)

tool_text$goal <- list(
  h2("Goal"),
  p("Climate change is intensifying heat events, but quantifying the impact of
    heat on specific outcomes can be challenging. This tool is designed to help
    departments understand how heat affects their operations, enabling them to
    better plan for a warming climate. Specifically, this tool provides estimates
    of how outcomes change at each level of the National Weather Service\"s HeatRisk
    index, which includes five levels ranging from \"None\" to \"Extreme\".")
)


tool_text$data <- list(
  h2("Data"),
  p(HTML("To enable wide and free distribution of this app, we require users to upload their own outcome data.
  A prototype of this app is hosted on shinyapps.io, where
  <a href=\"https://docs.posit.co/shinyapps.io/guide/security_and_compliance/index.html\">(data is stored securely)</a>,
  but future versions could be hosted elsewhere. Before uploading any sensitive data, ensure that you
  trust the app\"s source. The file upload format is explained on the \"App\" tab.")),
  p(HTML("Current and future HeatRisk data for local stations can be obtained from the
  <a href=\"https://www.wpc.ncep.noaa.gov/heatrisk/\">National Weather Service</a>
  or <a href=\"https://ephtracking.cdc.gov/DataExplorer/\">National Environmental Public Health Tracking Network</a>.
    Historical HeatRisk data are currently not publicly available,
    although the National Weather Service may make these data accessible through an API in the future."))
)

tool_text$methods <- list(
  h2("Methods"),
  h3("Overview"),
  p("This app compares days with high HeatRisk to days with low HeatRisk using a matching method.
    The matching only compares days that occur within a few weeks of each other, making this
    method relatively robust to changes in data, such as long-term population shifts or changes
    in collection methods. To help explore potential data quality issues, we provide a timeline
    view of the data, where the y-axis represents the selected outcome, the x-axis shows the date,
    color indicates the corresponding HeatRisk on that day, and blank spaces signify NA or zero values."),
  h3("Identifying matched control days"),
  p("For each day in the uploaded time series, the app identifies matched controls as days that meet the following criteria:"),
  tags$ul(
    tags$li("Occur within 3 weeks before or after the target day (this value can be modified using the specification options)"),
    tags$li("Fall on the same weekday (for instance, Tuesdays are only matched to other Tuesdays)"),
    tags$li("Have a HeatRisk level of \"None\" or \"Minor\"")
  ),
  p("Due to the last criterion, the number of matched controls can vary for each day, ranging from 0 to 6
    (for a three-week window). Days at the start or end of the uploaded time series also have fewer
    potential matches. Observations for which there are no matches are excluded from the calculation."),
  h3("Estimating Coefficients"),
  p("After identifying matched controls, the app assembles a dataset where each row represents a
    matched observation and control pair. The same observation will appear in multiple rows if
    it has multiple matched controls. This dataset includes:"),
  tags$ul(
    tags$li("The date of the observation"),
    tags$li("The HeatRisk level of the observation day"),
    tags$li("The value of the selected outcome on the observation day"),
    tags$li("The value of the selected outcome on the control day"),
    tags$li(HTML("The difference between the selected outcome on the observation and control day
                 (referred to below as <i>Difference</i>)")),
    tags$li(HTML("A <i>weight</i> which is the inverse of the number of times an observation appears
       (the weight is 1 if the observation appears once, 0.5 if it appears twice, 0.2 if it appears five times, etc.)"))
  ),
  p(HTML("To estimate the effect of HeatRisk on the outcome, we run the following ordinary least
         squares regression, weighted by <i>weight</i>:")),
  withMathJax("$$Difference = HeatRisk + \\epsilon$$"),
  p("No intercept is included so that we obtain an estimate for each level of HeatRisk.
    We estimate heteroskedastic standard errors, clustering by observation date
    (to account for days with multiple controls) and control date (to account for
    controls which are used multiple times in different observations). This regression
    approach is fast, explicit in how it aggregates data results, and allows for the
    estimation of standard errors and corresponding confidence intervals."),
  h3("Results table"),
  p("The results table includes statistics for each level of HeatRisk not used as a control. The columns include:"),
  tags$ul(
    tags$li(tags$b("Observed days:"), " The number of days in the selected date range with that level of HeatRisk."),
    tags$li(tags$b("Median (IQR):"), " The median value and interquartile range of the outcome on days with that level of HeatRisk."),
    tags$li(tags$b("Daily change:"), " The estimate from the ordinary least squares regression described above, with 95% confidence intervals based on the standard errors."),
    tags$li(tags$b("Total change:"), " The daily change and corresponding confidence intervals multiplied by the number of observed days."),
    tags$li(tags$b("Percent change:"), " The weighted mean of the difference between observed and control day outcomes, expressed as a percentage of control day outcomes.")
  ),
  p("A .csv file of the results table can be downloaded using the corresponding button."),
  p("The results plot shows the daily change results for each level of HeatRisk and their corresponding 95% confidence intervals.")
)

tool_text$specification_options <- list(
  h2("Specification options"),
  h3("Outcome"),
  p("The input dataset can contain multiple outcome columns. Switch between these
    outcomes using the dropdown menu."),
  h3("Dates"),
  p("Limit the date range of the analysis. This is useful when analyzing a specific period,
    such as identifying the impact of a single heatwave. The default date range is set to
    the maximum and minimum valid dates for the currently selected outcome. Dates are
    modified either by entering data into the two date fields.
    Care should be taken when using narrow date ranges, as this will limit sample sizes,
    especially for the highest levels of HeatRisk. Point estimates and estimates of
    uncertainty are less accurate and robust with small sample sizes."),
  h3("Number of controls"),
  p("This option specifies the number of weeks before and after the observed date
    that can be used as controls. The number of potentially available controls is
    twice this value because both leading and lagging controls are used. Increasing
    the number of controls may generate more precise estimates but also decreases
    the likelihood that control days are valid counterfactuals."),
  h3("Exclusions"),
  p("It may be desirable to exclude some days from the estimation. For example,
    federal holidays may have different outcome levels due to changes in the general
    population\"s behavior or differences in staffing levels. Excluding federal
    holidays means that they are not used as either observations or controls."),
  p("Similarly, weekends may have different absolute levels than weekdays. While
    weekends are never used as control observations for weekdays (and vice versa),
    the difference in absolute levels may result in different estimated coefficients.
    Therefore, weekends can be excluded from the analysis."),
  h3("Reference group"),
  p("This option specifies whether the \"None\" and \"Minor\" HeatRisk categories
    should be combined in the results plot. When combined, the estimate for this
    group should always be close to zero since it is the composition of the control
    group. The default behavior is to leave these groups split because it may be
    useful to understand if there are differences between minor and control days.")
)

tool_text$license_attribution <- list(
  h2("License"),
  p("The source code for this tool is available under a GPL-3 license."),
  h2("Attribution"),
  p("This tool was developed by Lawrence Baker and Roland Sturm as part of the
    RAND project \"Health and Social Services During Heat Events\". Funding was
    provided by the Los Angeles County Chief Sustainability Office through the Los
    Angeles County Quality and Productivity Commission."),
  p(HTML("The matching approach used in this app is based on
         <a href='https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/Climate-Health-Equity/CDPH-2022-Heat-Wave-Excess-Mortality-Report.pdf'>
         a method developed by the California Department of Public Health</a>")),
  p("We thank Ali Frazzini, Karen Chang, and Meredith Milet for their feedback on earlier
    versions of this tool.")
)
