# Estimating The Impact of Heat on Public Services

## Introduction

This app is designed to help local government departments estimate how heat impacts their operations and clients. It has a simple GUI, allows departments to upload their own data, and produces plots and results tables. It can be hosted by anyone with access to an R Shiny server. To use the app with their data, users simply upload a .csv file.
 
## Data

The uploaded file should be in .CSV format. The first two columns must be labeled "Date" and "HeatRisk". The "Date" column should contain dates in the "MM/DD/YYYY" format. The "HeatRisk" column should contain numeric values ranging from 0 (None) to 4 (Extreme).

Subsequent columns should contain numeric outcome counts, with the column headings used to name the outcomes. You can include as many outcome columns as needed, and you can switch between them during the analysis. If any values are missing, the corresponding cells must be left blank. An example table structure is:

Date	| HeatRisk	| EMS Calls | Mental Health Crises | Medical Examiner Deaths
1/1/2015 | 0 | 838 | 89	
1/2/2015	| 2	| 776	| 103	
...	| ... | ... | ... | ...
12/31/2023	| 1	| 904	| 64 | 41

In this example, "EMS Calls", "Mental Health Crises", and "Medical Examiner Deaths" are the names of the outcome columns, and the numeric values represent the corresponding outcome counts for each date and HeatRisk level. Note the blank cell in the "Medical Examiner Deaths" column, indicating a missing value.

A regression results table can optionally be exported in .CSV format.

## Hosting
We encourage the hosting of this application on a public Shiny server to make it more broadly available

## Methods & Usage

### Overview
This app compares days with high HeatRisk to days with low HeatRisk using a matching method. The matching only compares days that occur within a few weeks of each other, making this method relatively robust to long-term shifts in data generation, such as population shifts or changes in collection methods. To help explore potential data quality issues, we provide a timeline view of the data, where the y-axis represents the selected outcome, the x-axis shows the date, color indicates the corresponding HeatRisk on that day, and blank spaces signify NA or zero values.

### Identifying matched control days
For each day in the uploaded time series, the app identifies matched controls as days that meet the following criteria:

- Occur within 3 weeks before or after the target day (this value can be modified using the specification options)
- Fall on the same weekday (for instance, Tuesdays are only matched to other Tuesdays)
- Have a HeatRisk level of "None" or "Minor"
- Due to the last criterion, the number of matched controls can vary for each day, ranging from 0 to 6 (for a three-week window). Days at the start or end of the uploaded time series also have fewer potential matches. Observations for which there are no matches are excluded from the calculation.

### Estimating Coefficients
After identifying matched controls, the app assembles a dataset where each row represents a matched observation and control pair. The same observation will appear in multiple rows if it has multiple matched controls. This dataset includes:

- The date of the observation
- The HeatRisk level of the observation day
- The value of the selected outcome on the observation day
- The value of the selected outcome on the control day
- The difference between the selected outcome on the observation and control day (referred to below as Difference)
- A weight which is the inverse of the number of times an observation appears (the weight is 1 if the observation appears once, 0.5 if it appears twice, 0.2 if it appears five times, etc.)

To estimate the effect of HeatRisk on the outcome, we regress this difference on HeatRisk, weighted by weight.

No intercept is included so that we obtain an estimate for each level of HeatRisk. We estimate heteroskedastic standard errors, clustering by observation date (to account for days with multiple controls) and control date (to account for controls which are used multiple times in different observations). This regression approach is fast, explicit in how it aggregates data results, and allows for the estimation of standard errors and corresponding confidence intervals.

Note that days with Moderate, Major, and Extreme HeatRisk are often disproportionately likely to have Mild, rather than None, HeatRisk days used as controls. This means that estimates from this matching method tend to be conservative.

### Results table
The results table includes statistics for each level of HeatRisk not used as a control. The columns include:

- Observed days: The number of days in the selected date range with that level of HeatRisk.
- Median (IQR): The median value and interquartile range of the outcome on days with that level of HeatRisk.
- Daily change: The estimate from the ordinary least squares regression described above, with 95% confidence intervals based on the standard errors.
- Total change: The daily change and corresponding confidence intervals multiplied by the number of observed days.
- Percent change: The weighted mean of the difference between observed and control day outcomes, expressed as a percentage of control day outcomes.
A .csv file of the results table can be downloaded using the corresponding button.

The results plot shows the daily change results for each level of HeatRisk and their corresponding 95% confidence intervals.

### Specification options
#### Outcome
The input dataset can contain multiple outcome columns. Switch between these outcomes using the dropdown menu.

#### Dates
Limit the date range of the analysis. This is useful when analyzing a specific period, such as identifying the impact of a single heatwave. The default date range is set to the maximum and minimum valid dates for the currently selected outcome. Dates are modified either by entering data into the two date fields. Care should be taken when using narrow date ranges, as this will limit sample sizes, especially for the highest levels of HeatRisk. Point estimates and estimates of uncertainty are less accurate and robust with small sample sizes.

#### Number of controls
This option specifies the number of weeks before and after the observed date that can be used as controls. The number of potentially available controls is twice this value because both leading and lagging controls are used. Increasing the number of controls may generate more precise estimates but also decreases the likelihood that control days are valid counterfactuals.

#### Exclusions
It may be desirable to exclude some days from the estimation. For example, federal holidays may have different outcome levels due to changes in the general population"s behavior or differences in staffing levels. Excluding federal holidays means that they are not used as either observations or controls.

Similarly, weekends may have different absolute levels than weekdays. While weekends are never used as control observations for weekdays (and vice versa), the difference in absolute levels may result in different estimated coefficients. Therefore, weekends can be excluded from the analysis.

#### Reference group
This option specifies whether the "None" and "Minor" HeatRisk categories should be combined in the results plot. When combined, the estimate for this group should be close to zero since this is the composition of the control group. The default behavior is to combine these groups, because the matching method method may underestimate the difference between None and Minor HeatRisk if None days are disproportionately likely to have other None days as controls and Minor days are disproportionately likely to have other Minor days as controls.
 

## Project information

This tool was developed by Lawrence Baker (lbaker@rand.org) and Roland Sturm as part of the RAND project "Health and Social Services During Heat Events". Funding was provided by the Los Angeles County Chief Sustainability Office through the Los Angeles County Quality and Productivity Commission.

The correponding report, Health and Social Services During Heat Events: Demand for Services in Los Angeles County, can be found [here](https://www.rand.org/pubs/research_reports/RRA3406-1.html)

The matching approach used in this app is based on [a method developed by the California Department of Public Health](https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/Climate-Health-Equity/CDPH-2022-Heat-Wave-Excess-Mortality-Report.pdf)

We thank Ali Frazzini, Karen Chang, and Meredith Milet for their feedback on earlier versions of this tool.


## Copyright and License

Copyright (C) 2024 RAND Corporation. This code is made available under the GPL-3 license.

## Authors and Reference

Lawrence Baker (lbaker@rand.org)
Roland Sturm (sturm@rand.org)

You can cite this tool using:

@techreport{bakerHeatImpacts2024,
  title = {{{Estimating}} the {{Impact of Heat}} on {{Public Services}}},
  author = {Baker, Lawrence and Sturm, Roland},
  year = {2024},
  institution = {RAND Corporation},
  abstract = {{$<$}p{$>$}This online, interactive tool allows users to estimate how heat impacts their operations and clients. It has a simple GUI, allows departments to upload their own data, and produces plots and results tables. {$<$}/p{$>$}},
  langid = {english}
}

The associated RAND report can be cited using:

@techreport{sturmHealthSocialServices2024,
  title = {Health and {{Social Services During Heat Events}}: {{Demand}} for {{Services}} in {{Los Angeles County}}},
  shorttitle = {Health and {{Social Services During Heat Events}}},
  author = {Sturm, Roland and Baker, Lawrence and Krovetz, Avery},
  year = {2024},
  month = sep,
  institution = {RAND Corporation},
  url = {https://www.rand.org/pubs/research_reports/RRA3406-1.html},
  abstract = {{$<$}p{$>$}The authors analyze the relationship between heat events in Los Angeles County and (1) emergency medical services, (2) emergency room visits, (3) deaths investigated by the medical examiner, and (4) bookings for violent offenses. Heat events are classified according to the National Weather Service HeatRisk system. Days classified as moderate, major, and severe HeatRisk days are associated with worse results for all these outcomes.{$<$}/p{$>$}},
  langid = {english},
  keywords = {Emergency Medical Services,Global Climate Change,Los Angeles,Meteorology and Weather,Students}
}

