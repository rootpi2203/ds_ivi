library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplotlyExtra)
library(ggplot2)
library(esquisse)
library(lubridate)

# functions file
source("chart_functions.R")
#---------------------------------

# read csv's original
df_covid19_vaccine <- read.csv("data/covid19_vaccine.csv")
df_world_population <- read.csv("data/world_population.csv")
df_coronavirus <- read.csv("data/coronavirus.csv")
# read csv's with eda
df_covid_vaccine <- clean_data_covid_vaccine()
df_covid_vaccine_country <- get_covid_vaccine_country(df_covid_vaccine)
df_worldwide <- get_data_worldwide(df_covid_vaccine)


# presentation tab: dataframes for wranngled data plots
# Plot 1
people_fully_vaccinated <- get_people_fully_vaccinated(df_covid_vaccine)
people_partially_vaccinated <- get_people_partially_vaccinated(df_covid_vaccine)
worldwide <- get_worldwide_merge(df_covid_vaccine, people_partially_vaccinated, people_fully_vaccinated)

# Plot 2
people_fully_vaccinated_death <- get_people_fully_vaccinated_death(df_covid_vaccine)
people_partially_vaccinated_death <- get_people_partially_vaccinated_death(df_covid_vaccine)
worldwide_deaths <- get_worldwide_death_merge(df_covid_vaccine, people_partially_vaccinated_death, people_fully_vaccinated_death)

# Plot 3
covid_country_sum <- get_covid_country_sum(df_covid_vaccine)

# colors
color_blue <- "#1e81b0"
color_lblue <- "#80e5ff"
color_red <- "#ff8080"

###################
# build shiny page
#--------------------------------- shiny ui() function --------------------------------
ui <- fluidPage(
  titlePanel("Dashboard - Covid 19"),
  # shinythemes::themeSelector(),

  tabsetPanel(
    tabPanel(
      "Dashboard",
      # Input selections
      fluidRow(
        h3("Fallzahlen pro Land"),
        h4("Setze eine Landeswahl und/oder einen Zeitraum fest:"),
        br(),
        column(
          3,
          selectInput("country1", "Land", unique(df_covid_vaccine$country), "Switzerland")
        ),
        column(
          3,
          dateRangeInput(
            "daterange",
            "Zeitraum",
            start = "2020-01-22",
            end = "2022-09-21",
            min = "2020-01-22",
            max = "2022-11-21",
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 1,
            language = "en",
            width = NULL,
            autoclose = TRUE
          )
        ),
        # column(3, selectInput("country2", "Land 2", unique(df_covid_vaccine$country), "China")),
      ), # fluid row

      # Plots
      plotly::plotlyOutput("plot_tot_cases_country_cases"),
      h1(""),
      plotly::plotlyOutput("plot_tot_cases_country_deaths"),
      hr(),

      # World Maps
      h3("Worldmaps"),
      plotly::plotlyOutput("plot_map_worldwide_cases"),
      h1(""),
      plotly::plotlyOutput("plot_map_worldwide_vaccinated_norm"),
    ),
    tabPanel(
      "Praesentation",
      h3("Covid-19 Report"),
      # Lineplots worldwide
      h3(" Frage1: Welche Einfluss hat die Impfung auf die Fallzahlen?"),
      plotly::plotlyOutput("plot_tot_cases_worldwide_vaccine"),
      h1(""),
      p(""),
      h3(" Frage2:  Welche Wirkung hat die Impfung auf die Todesfaelle?"),
      plotly::plotlyOutput("plot_tot_cases_worldwide_death"),
      h1(""),
      p(""),
      h3(" Frage3: Welche Laender zeigen die tiefsten Fallzahlen?"),
      plotly::plotlyOutput("covid_total_cases_per_country"),
      h1(""),
      p(""),
    ),
    tabPanel(
      "Daten",
      tabsetPanel(
        tabPanel(
          "wrangled data",
          h2("connected dataframes"),
          DT::DTOutput("covid_vaccine")
        ),
        tabPanel(
          "original data",
          h2("vaccine dataframe"),
          DT::DTOutput("vaccine"),
          h2("world_population dataframe"),
          DT::DTOutput("world_population"),
          h2("covid dataframe"),
          DT::DTOutput("covid")
        )
      )
    ),
  )
)

#--------------------------------- shiny server() function --------------------------------
server <- function(input, output, session) {
  # (reactive) functions data filter
  df_covid_vaccine_filter_date <- reactive({
    df_covid_vaccine %>%
      filter(date >= dateToYearWeek(input$daterange[1]) & date <= dateToYearWeek(input$daterange[2]))
  })

  df_covid_vaccine_filter_date_country <- reactive({
    df_covid_vaccine %>%
      filter(date >= dateToYearWeek(input$daterange[1]) & date <= dateToYearWeek(input$daterange[2])) %>%
      filter(country == input$country1)
  })


  # Render plot output
  # Dashboard Tab
  # line plots
  output$plot_tot_cases_country_cases <- plotly::renderPlotly({
    plotly::plot_ly(
      data = df_covid_vaccine_filter_date_country() %>% filter(type == "confirmed"),
      x = ~date,
      y = ~cases,
      type = "scatter",
      mode = "lines",
      name = "Fallzahlen"
    ) %>%
      add_trace(
        y = ~people_partially_vaccinated,
        name = "Anzahl teilweise geimpfte",
        yaxis = "y2"
      ) %>%
      add_trace(
        y = ~people_fully_vaccinated,
        name = "Anzahl vollstaendig geimpfte",
        yaxis = "y2"
      ) %>%
      layout(
        title = paste("Covid-19 Faelle in: ", char(input$country1)),
        xaxis = list(title = "Datum"),
        yaxis = list(title = "Fallzahlen"),
        yaxis2 = list(
          title = "Anzahl Personen geimpft",
          overlaying = "y", side = "right"
        )
      )
  })

  output$plot_tot_cases_country_deaths <- plotly::renderPlotly({
    plotly::plot_ly(
      data = df_covid_vaccine_filter_date_country() %>% filter(type == "death"),
      x = ~date,
      y = ~cases,
      type = "scatter",
      mode = "lines",
      name = "Fallzahlen"
    ) %>%
      add_trace(
        y = ~people_partially_vaccinated,
        name = "Anzahl teilweise geimpfte",
        yaxis = "y2"
      ) %>%
      add_trace(
        y = ~people_fully_vaccinated,
        name = "Anzahl vollstaendig geimpfte",
        yaxis = "y2"
      ) %>%
      layout(
        title = paste("Covid-19 Tode in: ", char(input$country1)),
        xaxis = list(title = "Datum"),
        yaxis = list(title = "Fallzahlen"),
        yaxis2 = list(
          title = "Anzahl Personen geimpft",
          overlaying = "y", side = "right"
        )
      )
  })

  # World map plots
  output$plot_map_worldwide_cases <- plotly::renderPlotly({
    plotly::plot_ly(
      data = df_covid_vaccine_country,
      locations = ~CODE,
      z = ~total_cases,
      text = ~country,
      type = "choropleth",
      colorscale = "Reds",
      zmin = 0,
      zmax = 130000000
    ) %>%
      layout(
        title = "Covid-19 Fallzahlen pro Land",
        geo = list(
          scope = "world",
          projection = list(type = "natural earth"),
          showlakes = TRUE,
          lakecolor = "rgb(255, 255, 255)"
        )
      ) %>%
      colorbar(title = "Fallzahlen")
  })


  output$plot_map_worldwide_vaccinated_norm <- plotly::renderPlotly({
    plotly::plot_ly(
      data = df_covid_vaccine_country,
      locations = ~CODE,
      z = ~ total_fully_vaccinated / population,
      text = ~country, type = "choropleth",
      colorscale = list(
        c(0, "rgb(255,114,118)"),
        list(1, "rgb(124,252,0)")
      )
    ) %>%
      layout(
        title = "Impfquoten der verschiedenen Laender",
        geo = list(
          scope = "world",
          projection = list(type = "natural earth"),
          showlakes = TRUE,
          lakecolor = "rgb(255, 255, 255)"
        )
      ) %>%
      colorbar(title = "Impfquote")
  })


  # Praesentation Tab
  # row 597
  output$plot_tot_cases_worldwide_vaccine <- plotly::renderPlotly({
    plotly::plot_ly(
      data = worldwide,
      x = ~date,
      y = ~total_cases_covid,
      type = "scatter",
      mode = "lines",
      name = "Cases"
    ) %>%
      add_trace(
        y = ~people_fully_vaccinated,
        name = "Vollstaendig geimpfte Personen",
        yaxis = "y2"
      ) %>%
      layout(
        title = "Covid-Fallzahlen gegen Impfquote",
        xaxis = list(title = "Datum"),
        yaxis = list(title = "Fallzahlen"),
        yaxis2 = list(
          title = "Anzahl vollstaendig geimpfte Personen",
          overlaying = "y", side = "right"
        )
      ) 
  })

  # row 667
  output$plot_tot_cases_worldwide_death <- plotly::renderPlotly({
    plotly::plot_ly(
      data = worldwide_deaths,
      x = ~date,
      y = ~total_cases,
      type = "scatter",
      mode = "lines",
      name = "Tode"
    ) %>%
      add_trace(
        y = ~people_fully_vaccinated,
        name = "Vollstaendig geimpfte Personen",
        yaxis = "y2"
      ) %>%
      layout(
        title = "Covid-Tode gegen Impfung",
        xaxis = list(title = "Datum"),
        yaxis = list(title = "Anzahl Tode"),
        yaxis2 = list(
          title = "Anzahl vollstaendig geimpfte Personen",
          overlaying = "y", side = "right"
        )
      )
  })


  output$covid_total_cases_per_country <- plotly::renderPlotly({
    plotly::plot_ly(
      data = covid_country_sum,
      x = ~country,
      y = ~cases,
      type = "bar"
    ) %>%
      layout(
        title = "Laender mit den tiefsten Corona Infektionen",
        xaxis = list(title = "Land"),
        yaxis = list(title = "Anzahl Infektionen")
      )
  })


  # Data Tab
  # wrangled data tab
  output$covid_vaccine <- DT::renderDT({
    df_covid_vaccine
  })

  # original data tab
  output$vaccine <- DT::renderDT({
    df_covid19_vaccine
  })
  output$world_population <- DT::renderDT({
    df_world_population
  })
  output$covid <- DT::renderDT({
    df_coronavirus
  })
} # end server

# run app
shinyApp(ui = ui, server = server)
