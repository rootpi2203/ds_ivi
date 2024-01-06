library(dplyr)



# functions to change date to year-calender_week
dateToYearWeek <- function(date) {
  calendar_week <- strftime(date, format = "%V")
  year <- format(as.POSIXct(date, format = "%Y-%m-%d"), format = "%Y")
  date_week <- paste(year, calendar_week, sep = "-")
  return(date_week)
}

# read and clean wrangled dataframe covid_vaccine2
clean_data_covid_vaccine <- function() {
  covid_vaccine <- read.csv("data/covid_vaccine2.csv")
  country_code <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")

  # set country codes
  covid_vaccine$country <- ifelse(covid_vaccine$country == "US", "United States", covid_vaccine$country)
  covid_vaccine$country <- ifelse(covid_vaccine$country == "Congo (Kinshasa)", "Congo, Democratic Republic of the", covid_vaccine$country)
  covid_vaccine <- left_join(covid_vaccine, country_code, by = c("country" = "COUNTRY"))

  # remove na
  covid_vaccine <- covid_vaccine %>%
    replace_na(list(people_partially_vaccinated = 0, people_fully_vaccinated = 0, population = 0))

  # missing data in week 53 - 2021
  covid_vaccine <- covid_vaccine %>%
    filter(date != "2022-52", date != "2021-53")

  return(covid_vaccine)
}

# aggregated dataframes to get countrys
get_covid_vaccine_country <- function(covid_vaccine) {
  df_covid_vaccine_country <- covid_vaccine %>%
    filter(type == "confirmed") %>%
    select(
      date, country, type, cases, doses_admin, people_partially_vaccinated,
      people_fully_vaccinated, CODE, population
    ) %>%
    group_by(country, type, CODE) %>%
    summarise(
      total_cases = sum(cases), total_partially_vaccinated = max(people_partially_vaccinated),
      total_fully_vaccinated = max(people_fully_vaccinated), population = max(population)
    ) %>%
    ungroup()
  df_covid_vaccine_country$population <- ifelse(df_covid_vaccine_country$country == "Australia", 25890773, df_covid_vaccine_country$population)
  df_covid_vaccine_country$population <- ifelse(df_covid_vaccine_country$country == "China", 1452735285, df_covid_vaccine_country$population)
  df_covid_vaccine_country$population <- ifelse(df_covid_vaccine_country$country == "Netherlands", 17591394, df_covid_vaccine_country$population)
  df_covid_vaccine_country$population <- ifelse(df_covid_vaccine_country$country == "Philippines", 109035343, df_covid_vaccine_country$population)

  return(df_covid_vaccine_country)
}

# aggregated dataframes to get worldwide data
get_data_worldwide <- function(df_covid_vaccine) {
  df_worldwide <- df_covid_vaccine %>%
    filter(type == "confirmed") %>%
    group_by(date) %>%
    summarise(total_cases = sum(cases), people_partially_vaccinated = sum(people_partially_vaccinated), people_fully_vaccinated = sum(people_fully_vaccinated)) %>%
    filter(!(people_partially_vaccinated == 0 & date > "2021-06-27")) %>%
    filter(!(people_fully_vaccinated == 0 & date > "2021-06-27")) %>%
    filter(!(people_partially_vaccinated < 5000000000 & date > "2021-10-28")) %>%
    filter(!(people_fully_vaccinated < 5000000000 & date > "2021-10-28")) %>%
    arrange(date) %>%
    ungroup()

  return(df_worldwide)
}

# aggregated dataframes to get people_fully_vaccinated
get_people_fully_vaccinated <- function(covid_vaccine) {
  people_fully_vaccinated <- covid_vaccine %>%
    filter(type == "confirmed") %>%
    group_by(date) %>%
    summarise(people_fully_vaccinated = unique(people_fully_vaccinated)) %>%
    arrange(date)

  people_fully_vaccinated <- people_fully_vaccinated %>%
    summarise(people_fully_vaccinated = sum(people_fully_vaccinated)) %>%
    filter(date < "2022-39") %>%
    arrange(date)

  return(people_fully_vaccinated)
}

# aggregated dataframes to get people_partially_vaccinated
get_people_partially_vaccinated <- function(covid_vaccine) {
  people_partially_vaccinated <- covid_vaccine %>%
    filter(type == "confirmed") %>%
    group_by(date) %>%
    summarise(people_partially_vaccinated = unique(people_partially_vaccinated)) %>%
    arrange(date)

  people_partially_vaccinated <- people_partially_vaccinated %>%
    summarise(people_partially_vaccinated = sum(people_partially_vaccinated)) %>%
    filter(date < "2022-39") %>%
    arrange(date)

  return(people_partially_vaccinated)
}

# aggregated dataframes to get worldwide_merge
get_worldwide_merge <- function(covid_vaccine, people_partially_vaccinated, people_fully_vaccinated) {
  worldwide <- covid_vaccine %>%
    filter(type == "confirmed") %>%
    group_by(date) %>%
    summarise(total_cases_covid = sum(cases), ) %>%
    filter(date < "2022-39") %>%
    arrange(date)

  merged_temp <- merge(people_partially_vaccinated, people_fully_vaccinated, by = "date")

  worldwide <- merge(worldwide, merged_temp, by = "date")

  return(worldwide)
}

# aggregated dataframes to get people_fully_vaccinated_death
get_people_fully_vaccinated_death <- function(covid_vaccine) {
  people_fully_vaccinated_death <- covid_vaccine %>%
    filter(type == "death") %>%
    group_by(date) %>%
    summarise(people_fully_vaccinated = unique(people_fully_vaccinated)) %>%
    arrange(date)

  people_fully_vaccinated_death <- people_fully_vaccinated_death %>%
    summarise(people_fully_vaccinated = sum(people_fully_vaccinated)) %>%
    filter(date < "2022-39") %>%
    arrange(date)

  return(people_fully_vaccinated_death)
}

# aggregated dataframes to get people_partially_vaccinated_death
get_people_partially_vaccinated_death <- function(covid_vaccine) {
  people_partially_vaccinated_death <- covid_vaccine %>%
    filter(type == "death") %>%
    group_by(date) %>%
    summarise(people_partially_vaccinated = unique(people_partially_vaccinated)) %>%
    arrange(date)

  people_partially_vaccinated_death <- people_partially_vaccinated_death %>%
    summarise(people_partially_vaccinated = sum(people_partially_vaccinated)) %>%
    filter(date < "2022-39") %>%
    arrange(date)

  return(people_partially_vaccinated_death)
}

# aggregated dataframes to get worldwide_death_merge
get_worldwide_death_merge <- function(covid_vaccine, people_partially_vaccinated_death, people_fully_vaccinated_death) {
  worldwide_deaths <- covid_vaccine %>%
    filter(type == "death") %>%
    group_by(date) %>%
    summarise(total_cases = sum(cases), ) %>%
    filter(date < "2022-39") %>%
    arrange(date)

  merged_temp_death <- merge(people_partially_vaccinated_death, people_fully_vaccinated_death, by = "date")

  worldwide_deaths <- merge(worldwide_deaths, merged_temp_death, by = "date")

  return(worldwide_deaths)
}

# aggregated dataframes to get covid_country_sum
get_covid_country_sum <- function(df_covid_vaccine) {
  covid_country_sum <- df_covid_vaccine %>%
    filter(type == "confirmed") %>%
    select(country, cases) %>%
    group_by(country) %>%
    mutate(cases = sum(cases)) %>%
    as.data.frame()

  covid_country_sum <- distinct(covid_country_sum)

  covid_country_sum <- covid_country_sum %>%
    arrange(cases) %>%
    filter(cases != 0) %>%
    slice(1:15)
  covid_country_sum$country <- as.vector(covid_country_sum$country)
  covid_country_sum$country <- factor(covid_country_sum$country, covid_country_sum$country)

  return(covid_country_sum)
}


#---- test ---
# test for reaktive plots (not used)
plot_tot_cases_worldwide <- function(df_covid_vaccine, start, end) {
  df_covid_vaccine %>%
    filter(date >= start & date <= end) %>%
    ggplot(aes(x = date, y = cases)) +
    geom_line(color = "blue") +
    scale_y_continuous() +
    labs(
      title = "Total cases Worldwide",
      y = "total cases", x = "date"
    )
}
