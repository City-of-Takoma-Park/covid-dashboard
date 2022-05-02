#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# rsconnect::deployApp(appName = "covid_dashboard", account = "takomapark") 

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(tpfuncts)
library(plotly)
library(glue)
library(plotlywrappers)

zip_fields <- readRDS("./data/zip_fields.rds")

zip_vax <- read_rds("./data/zip_vax.rds")

zip_vax_data <- zip_vax %>%
    mutate(istp = case_when(grepl("20912", ZIPCODE1) ~ "20912",
                            T ~ "Other zip codes")) %>%
    arrange(desc(AtleastOneDose_PercentPop)) %>%
    mutate(partialrank = round(percent_rank(AtleastOneDose_PercentPop) * 100, 0),
           fullrank = round(percent_rank(FullyVaccinated_PercentPop) * 100, 0)) %>%
    arrange(ZIPCODE1) %>%
    mutate(ZIPCODE1 = factor(ZIPCODE1, .$ZIPCODE1, .$ZIPCODE1))

zip_tp <- zip_vax_data %>%
    filter(grepl("20912", ZIPCODE1))

zip_tp_pop <- zip_tp$Population

zip_long <- zip_fields %>%
    pivot_longer(cols = colnames(zip_fields)[-c(1:2)]) %>%
    mutate(date_new = gsub("F", "", name),
           date_new = gsub("total", "", date_new),
           date_new = gsub("_", "-", date_new),
           date_new = as.Date(date_new, format = "%m-%d-%Y")) %>%
    arrange(date_new) %>%
    mutate(case_change = value - lag(value),
           case_change_7day = case_change + lag(case_change) + lag(case_change, 2) + lag(case_change, 3) + lag(case_change, 4) + lag(case_change, 5) + lag(case_change, 6),
           case_change_7day = case_change_7day,
           case_change_7day_norm = round(case_change_7day / zip_tp_pop * 100000, 2),
           transmiss_level = case_when(case_change_7day_norm <= 10 ~ "Low",
                                       case_change_7day_norm < 50 ~ "Moderate",
                                       case_change_7day_norm < 100 ~ "Substantial",
                                       case_change_7day_norm > 100 ~ "High")) %>%
    arrange(desc(date_new)) %>%
    mutate(thurs_reference = as.Date("06-01-2023", format = "%m-%d-%Y"),
           thurs_diff = floor(abs(date_new - thurs_reference) / 7)) %>%
    group_by(thurs_diff) %>%
    mutate(week_end_date = first(date_new),
           week_end = week_end_date %>%
               gsub("(2022-)|(2021-)|(2020-)", "", .) %>%
               gsub("^0", "", .),
           week_start_date = last(date_new),
           week_start = week_start_date  %>%
               gsub("(2022-)|(2021-)|(2020-)", "", .) %>%
               gsub("^0", "", .),
           week_range = paste0(week_start, " - ", week_end),
           week_new = sum(case_change, na.rm = T),
           month_data = format(date_new, "%m-%Y")) %>%
    group_by(month_data) %>%
    mutate(month_cases = sum(case_change, na.rm = T))

most_recent <- zip_long %>%
    ungroup() %>%
    slice(1)

zip_long_last180days <- zip_long %>%
    filter(date_new > most_recent$date_new - 120) %>%
    ungroup %>%
    select(
        thurs_diff,
        week_start_date,
        week_end_date,
        week_range,
        week_new) %>%
    distinct() %>%
    arrange(desc(thurs_diff)) %>%
    mutate(week_diff = week_end_date - week_start_date) %>%
    filter(week_diff == 6) %>%
    mutate(pct_chng = round((week_new - lag(week_new)) * 100 / lag(week_new), 1),
           week_range = factor(week_range, .$week_range, .$week_range))

plot_weekchange <- plotly::plot_ly(zip_long_last180days,
                           x = ~ week_range,
                           y = ~ week_new,
                           type = "scatter",
                           text = ~ glue(
                           "Percent change since
                           previous week: {pct_chng}%"),
                           mode = "line+marker") %>%
    layout(xaxis = list(title = "Week"),
           yaxis = list(title = "New cases"),
           title = "New cases by week in 20912 zip code in last 120 days")

zip_long_start2021 <- zip_long %>%
    ungroup %>%
    arrange(date_new) %>%
    select(month_data, month_cases) %>%
    distinct() %>%
    mutate(pct_chng = pct_round((month_cases - lag(month_cases)), lag(month_cases)),
           month_data = factor(month_data, .$month_data, .$month_data))

plot_monthchange <- plotly::plot_ly(zip_long_start2021,
                            x = ~ month_data,
                            y = ~ month_cases,
                            text = ~ glue(
                            "Percent change
                            since previous month: {pct_chng}%"),
                            type = 'scatter',
                            mode = "line+marker") %>%
    layout(xaxis = list(title = "Month"),
           yaxis = list(title = "New cases"),
           title = "New cases by month in 20912 zip code since start of pandemic")

zip_last60days <- zip_long %>%
    ungroup %>%
    filter(date_new >= most_recent$date_new - 90) %>%
    arrange(date_new) %>%
    select(
        date_new, value, case_change, case_change_7day, case_change_7day_norm, transmiss_level
    ) %>%
    mutate(pct_chng = pct_round((case_change - lag(case_change)), lag(case_change)),
           date_char = as.character(date_new) %>%
               gsub("(2021-)|(2022-)", "", .)) %>%
    mutate(date_char = factor(date_char, .$date_char, .$date_char))

plot_last_60_days <- plotly::plot_ly(zip_last60days, 
                             x = ~ date_char,
                             y = ~ case_change,
                             type = "scatter",
                             mode = "line+marker",
                             showlegend = F,
                             text = ~ glue(
                             "Percent change since 
                             previous day: {pct_chng}%
                             Total cases since pandemic start: {value %>% commafy}")) %>%
    layout(title = "New cases in 20912 zip code in last 90 days",
           xaxis = list(title = "Date"),
           yaxis = list(title = "New cases since previous day")) 


plot_7daynorm_last_60_days <- plotly::plot_ly(zip_last60days, 
                             x = ~ date_char,
                             y = ~ case_change_7day_norm,
                             type = "scatter",
                             showlegend = F,
                             mode = "line+marker",
                             text = ~ glue(
                                 "Transmission level: {transmiss_level}
                             Total cases last 7 days: {case_change_7day %>% commafy}")) %>%
    layout(title = "New cases in last 7-days per 100,000 residents",
           xaxis = list(title = "Date"),
           yaxis = list(title = "New cases in last 7-days per 100,000 residents"))
    #        shapes = list(
    #            list(
    #                type = "line", 
    #                y0 = 10, y1 = 10, x0 = 0 - 0.5, x1 = length(zip_last60days$date_char) - 
    #                    0.5, xref = "x", line = list(dash = "dash")
    #            ),
    #            list(
    #                type = "line", 
    #                y0 = 50, y1 = 50, x0 = 0 - 0.5, x1 = length(zip_last60days$date_char) - 
    #                    0.5, xref = "x", line = list(dash = "dash")
    #            ),
    #            list(
    #                type = "line", 
    #                y0 = 100, y1 = 100, x0 = 0 - 0.5, x1 = length(zip_last60days$date_char) - 
    #                    0.5, xref = "x", line = list(dash = "dash")
    #            )
    #        )
    #        ) %>%
    # # add_annotations(text = "Low", 
    # #                 x = length(zip_last60days$date_char) - length(zip_last60days$date_char)/2, yshift = 4, xshift = 3, y = 0, showarrow = F,
    # #                 font = list(size = 9)) %>%
    # add_annotations(text = "Moderate", 
    #                 x = length(zip_last60days$date_char) - length(zip_last60days$date_char)/2, yshift = 4, xshift = 3, 
    #                 y = 10, showarrow = F,
    #                 font = list(size = 9)) %>%
    # add_annotations(text = "Substantial", 
    #                 x = length(zip_last60days$date_char) - length(zip_last60days$date_char)/2, yshift = 5, xshift = 3, 
    #                 y = 50, showarrow = F,
    #                 font = list(size = 9)) %>%
    # add_annotations(text = "High", 
    #                 x = length(zip_last60days$date_char) - length(zip_last60days$date_char)/2, yshift = 8, xshift = 3, 
    #                 y = 100, showarrow = F,
    #                 font = list(size = 9))
    # 


# last 7 days
zip_last7 <- zip_long %>%
    filter(date_new > most_recent$date_new - 7) %>%
    pull(case_change) %>%
    sum()

# vax data
tp_first_dose <- zip_tp$AtleastOneDose_PercentPop %>%
    round(., 2)

tp_second_dose <- zip_tp$FullyVaccinated_PercentPop %>%
    round(., 2)

plot_partial_vax <- plotly::plot_ly(
    zip_vax_data,
    x = ~ ZIPCODE1,
    y = ~ round(AtleastOneDose_PercentPop, 2),
    name = ~ ZIPCODE1,
    colors = "Set1",
    text = ~ glue("Zipname: {ZIPName}
    Zip population: {Population %>% commafy}
    Percentile-rank full-vaccination: {fullrank}%
    Percentile-rank partial-vaccination: {partialrank}%"),
    color = ~ istp,
    legendgroup = ~ ZIPCODE1,
    type = "bar"
) %>%
    layout(xaxis = list(title = "Zip code"),
           yaxis = list(title = ""),
           legend = list(font = list(size = 10)),
           title = "At-least first dose percent-vaccinated by zip code")

plot_full_vax <- plotly::plot_ly(
    zip_vax_data,
    x = ~ ZIPCODE1,
    y = ~ round(FullyVaccinated_PercentPop, 2),
    name = ~ ZIPCODE1,
    colors = "Set1",
    text = ~ glue("Zipname: {ZIPName}
                Zip population: {Population %>% commafy}
                Percentile-rank full-vaccination: {fullrank}%
                Percentile-rank partial-vaccination: {partialrank}%"),
    color = ~ istp,
    legendgroup = ~ ZIPCODE1,
    type = "bar"
) %>%
    layout(xaxis = list(title = "Zip code"),
           yaxis = list(title = ""),
           legend = list(font = list(size = 10)),
           title = "Fully-vaccinated percent by zip code")


# read in and analyze community level data
comm_level <- readRDS("./data/community_level.rds")

comm_level_point_time <- comm_level %>%
  group_by(county) %>%
  arrange(desc(date_updated)) %>%
  slice_head(n = 1)

comm_level_mc <- comm_level %>%
  filter(grepl("Montgomery", county))

curr_level_mc <- comm_level_point_time %>%
  filter(county == "Montgomery County")

curr_level_mc_level <- curr_level_mc %>%
  pull(covid_19_community_level)

curr_level_cases <- curr_level_mc %>%
  pull(covid_cases_per_100k)

curr_level_mc_update <- curr_level_mc %>%
  pull(date_format)

plot_commlevels_mc <- function(vareval, varstring, subtitle, basechng){
  
  basepct <- paste0(basechng, "_pct")
  
    returnplot <- plot_ly(comm_level_mc %>%
            arrange(date_updated),
         x = ~ date_updated,
         y = vareval,
         text = ~ glue(paste0("Change since last week: {", basechng, " %>% commafy}
                         Percent change since last week: {", basepct, " %>% commafy}%")),
         showlegend = F,
         type = "scatter",
         mode = "line+marker") 
  
  
  maxval <- case_when(varstring == "covid_cases_per_100k" ~ 250,
                      varstring == "covid_hospital_admissions_per_100k" ~ 25,
                      varstring == "covid_inpatient_bed_utilization" ~ 20)
  
  
  
  if (max(comm_level_mc[[varstring]]) < maxval){
    returnplot <- returnplot %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "",
                          tickvals = seq(0, maxval, maxval / 5),
                          range = c(0, maxval)),
             title = "")
  }
  
  else {
    returnplot <- returnplot %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = ""),
             title = "")
  }
    
  returnplot %>%
    subplot_title(subtitle)

}

lines_approp <- function(baseplot, varstring){
  
  # browser()
  
  if (varstring == "covid_cases_per_100k"){
    returnplot <- baseplot %>%
      dotted_line(df = comm_level_mc, x_col = "date_updated", line_val = 200, gluetext = "", .showarrow = F)
  }
  
  else{
    if (curr_level_cases < 200){
      
      var_med <- case_when(varstring == "covid_hospital_admissions_per_100k" ~ 10,
                           varstring == "covid_inpatient_bed_utilization" ~ 10)
      
      var_high <- case_when(varstring == "covid_hospital_admissions_per_100k" ~ 20,
                           varstring == "covid_inpatient_bed_utilization" ~ 15)
      

      returnplot <- baseplot %>%
        multi_line(df = comm_level_mc, x_col = "date_updated", yvec = c(var_med, var_high), c("Medium", "High"))
    }
    
    if (curr_level_cases >= 200){
      var_high <- case_when(varstring == "covid_hospital_admissions_per_100k" ~ 10,
                            varstring == "covid_inpatient_bed_utilization" ~ 10)
      
      returnplot <- baseplot %>%
        dotted_line(df = comm_level_mc, x_col = "date_updated", line_val = var_high, gluetext = "High", .showarrow = F)
      
    }
    
  }
  
  return(returnplot)
  
}

plot_lines_commlevelsmc <- function(vareval, varstring, subtitle, basechng){
  plot_commlevels_mc(vareval = vareval, varstring = varstring, subtitle = subtitle, basechng = basechng) %>%
    lines_approp(varstring = varstring)
}

plot_mc_covid100k <- plot_lines_commlevelsmc(~ covid_cases_per_100k, "covid_cases_per_100k", "COVID cases\nper 100k\n(elevated risk at 200)", "chng_covid")

plot_mc_inpatient <- plot_lines_commlevelsmc(~ covid_inpatient_bed_utilization, "covid_inpatient_bed_utilization", "Percent of beds\noccupied by\nCOVID patients", "chng_inpatient") 

plot_mc_admits <- plot_lines_commlevelsmc(~ covid_hospital_admissions_per_100k,"covid_hospital_admissions_per_100k", "COVID hospital\nadmissions per\n100k", "chng_hospadmit")

sub_commlevel <- plotly::subplot(plot_mc_covid100k,
                         plot_mc_inpatient,
                         plot_mc_admits, shareX = T, nrows = 1) %>%
  layout(title = glue::glue("Montgomery County Community Level: {curr_level_mc_level}"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(version = 3, bootswatch = "flatly"),
    
    # Application title
    titlePanel("Takoma Park Zip-code Level Data Dashboard"),
    
    tagList("Due to the effects of the Maryland Department of Public Health hack, the City of Takoma Park is currently unable to update our normal data-dashboard with city-level COVID case data. As a result, we have temporarily designed this dashboard to provide updated vaccination and case-data at the 20912 zip-code level using data from the state. The 20912 zip code covers most of Takoma Park, but also includes unincorporated areas outside of Takoma Park and does not include a small part of northwest Takoma Park. When we are able to receive city-level data again from the County, we will resume updating the normal dashboard at ", a("this link", href = "https://app.smartsheet.com/b/publish?EQBCT=42a594afc3ad4c59ba7b1ca9965b7837"), ". Data comes from the Marlyand Department of Public Health. The code used to generate this app can be found on the ", a("City's GithHub page", href = ""), ". If you have any questions or feedback, please contact Public Administation Specialist Dan Powers at ", a("danielp@takomaparkmd.gov", href = "mailto:danielp@takomaparkmd.gov"), "."),
    
    h3("Key metrics"),
    
    strong(paste0("Montgomery County "), tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/science/community-levels.html", "Community Level"), " as of ", curr_level_mc_update, ":", curr_level_mc_level, style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    strong(paste0("At-least first-dose vaccination rate in Takoma Park based on vaccination rates in Census tracts, as of January 24, 2022: 81%"), style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    strong(paste0("At least first-dose vaccination rate in 20912 zip code: ", tp_first_dose, "%"), style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    strong(paste0("Fully-vaccinated rate in 20912 zip code: ", tp_second_dose, "%"), style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    strong(paste0("New cases in last 7 days: ", zip_last7), style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    strong(paste0("New cases per 100,000 residents in last 7 days: ", most_recent$case_change_7day_norm %>% commafy), style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    strong(paste0("Community transmission levels in 20912 zip code based on CDC guidance: ", most_recent$transmiss_level), style = "font-size:16px;"),
    
    br(),
    
    br(),
    
    tabsetPanel(
        selected = "covid",
        tabPanel(title = "COVID cases",
                 
                 br(),
                 
                 "Montgomery County's Community Level is based on three metrics: COVID-19 cases, hospital admissions due to COVID-19, and the percent of staffed inpatient beds occupied by COVID-19 patients.",
                 
                 br(),
                 
                 br(),
                 
                 sub_commlevel,
                 
                 br(),
                 
                 br(),
                 
                 subplot(plot_last_60_days %>% subplot_title("New daily cases"),  
                         plot_7daynorm_last_60_days %>% subplot_title("New cases last 7-days\nper 100k residents"), nrows = 2, shareX = T) %>%
                     layout(title = "New cases in 20912 zip code in last 90 days"),
                 
                 br(),
                 br(),
                 
                 plot_weekchange,
                 
                 br(),
                 
                 br(),
                 
                 plot_monthchange,
                 value = "covid"
        ),
        tabPanel("Zip-code level vaccinations",
                 
                 br(),
                 
                 plot_partial_vax,
                 
                 br(),
                 br(),
                 
                 plot_full_vax,
                 value = "vax"
        )
        
        # tabPanel("County community-levels",
        #          
        #          br(),
        #          
        #          
        #          
        # )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
