library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(purrr)

#' Get COVID-19 data as data frame
#'
#' Retrieve covid data in pandas dataframe format witg tge time periods provided
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_data()
get_data <- function() {
    url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
    
    tryCatch(
        {
            df <- readr::read_csv(url)
        },
        error = function(e) {
            stop("The link to the data is broken.")
        }
    )
    
    columns <- c(
        "iso_code",
        "continent",
        "location",
        "date",
        "total_cases",
        "new_cases",
        "total_deaths",
        "new_deaths",
        "total_cases_per_million",
        "new_cases_per_million",
        "total_deaths_per_million",
        "new_deaths_per_million",
        "icu_patients",
        "icu_patients_per_million",
        "hosp_patients",
        "hosp_patients_per_million",
        "weekly_icu_admissions",
        "weekly_icu_admissions_per_million",
        "weekly_hosp_admissions",
        "weekly_hosp_admissions_per_million",
        "total_vaccinations",
        "people_vaccinated",
        "people_fully_vaccinated",
        "new_vaccinations",
        "population"
    )
    
    df <- df %>% dplyr::select(all_of(columns))
    df <- dplyr::filter(df, !stringr::str_detect(iso_code, "^OWID"))
    df <- df %>% replace(is.na(.), 0)
}

#' Get COVID-19 data as data frame
#'
#' Retrieve covid data in pandas dataframe format witg tge time periods provided
#'
#' @param date_from Start date of the data range with format like '2021-10-31'.
#' @param date_to End date of data range with format like '2021-10-31'.
#' @param countries Charactor vector of target country names. By default it retrieves all countries
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_data(date_from = "2022-01-01", date_to = "2022-01-07", location = c("Canada", "United State"))
filter_data <- function(df, date_from, date_to, countries) {
    if (missing(date_from)) {
        date_from <- df$date %>% min()
    }
    
    if (missing(date_to)) {
        date_to <- df$date %>% max()
    }
    
    df <- df %>%
        dplyr::filter(date >= date_from, date <= date_to)
    
    if (!missing(countries)) {
        df <- df %>%
            dplyr::filter(location %in% countries)
    }
    
    df
}

df <- get_data()

# Feature dropdown functions
feature_labels <- c("Total confirmed cases",
                    "Total confirmed cases per million people",
                    "Daily confirmed cases",
                    "Daily confirmed cases per million people",
                    "Total deaths",
                    "Total deaths per million people",
                    "Daily deaths",
                    "Daily deaths per million people"
)

feature_values <- c("total_cases",
                    "total_cases_per_million",
                    "new_cases",
                    "new_cases_per_million",
                    "total_deaths",
                    "total_deaths_per_million",
                    "new_deaths",
                    "new_deaths_per_million"
)

feature_mapping <- function(label, value) {
    list(label = label, value = value)
}

data_type_labels <- c("Linear", "Log")

data_type_values <- c("identity", "log")

data_type_mapping <- function(label, value) {
    list(label = label, value = value)
}

# feature dropdown2
feature_dropdown2 = dccDropdown(
    id = "feature-dropdown2",
    value = "total_cases_per_million",
    options = purrr::map2(feature_labels, feature_values, feature_mapping)
)


# Country selector
country_selector <- dccDropdown(
    id = "country-selector",
    multi = TRUE,
    options = df$location %>% unique() %>% purrr::map(function(col) list(label = col, value = col)),
    value=c("Canada", "United States", "United Kingdom", "France", "Singapore"),
)

#Linear/Log Selector
scale_line_radio = dbcRadioItems(
    id = "scale-line-radio",
    options = purrr::map2(data_type_labels, data_type_values, data_type_mapping),
    value="identity",
)

# Tabs and sidebars
sidebar <- dbcCol(dbcRow(
    list(
        htmlBr(),
        htmlP(" "),
        htmlP(" "),
        htmlH3(
            "World COVID-19 Dashboard", style = list("font" = "Helvetica", "font-size" = "25px", "text-align" = "center")
        ),
        htmlP(" "),
        htmlP(" "),
        htmlBr(),
        htmlBr(),
        htmlP(
            "Explore the global situation of COVID-19 using this interactive dashboard. Compare selected countries and indicators across different date ranges to observe the effect of policy, and vaccination rate.",
            style = list("text-align" = "justify")),    
        htmlHr(),
        htmlBr(),
        htmlBr(),
        htmlB("Country Filter"),
        htmlP(
            "Use this filter to add or remove a country from the analysis",
        ),
        htmlBr(),
        htmlBr(),      
        country_selector
    )
),
width = 2,
style = list(
    "border-width" = "0",
    "backgroundColor" = "#d3e9ff"
),
)

# Line tab
line_tab <- dbcRow(
    list(
        htmlP(" "),
        htmlP(
            "Line Plot",
            style = list("font-size" = "25px"),
        ),
        htmlP(
            "The line plot below depicts the selected COVID-19 indicator for the selected countries. Click the legend to highlight particular countries.",
        ),
        htmlB("Indicator:"),
        htmlP(
            "Select an indicator to explore on the map and line plot using the dropdown below.",
        ),
        htmlBr(),
        htmlBr(),
        feature_dropdown2,
        htmlP(
            " ",
        ),
        dbcCol(
            list(htmlP(" ",),
                 htmlB("Data Scale"),
                 scale_line_radio),
            width = 1,
        ),
        dbcCol(
            dccLoading(
                dccGraph(
                    id = "line-plot",
                    style = list("height" = "70vh"),
                )
            )
        )
    )
)


# APP codes
app <- Dash$new(external_stylesheets = dbcThemes$FLATLY)

app$layout(
    dbcContainer(
        dbcRow(
            list(
                sidebar,
                dbcCol(
                    list(
                        dbcRow(
                            list(
                                htmlP(" "),
                                htmlP(" "),
                                htmlBr(),
                                htmlBr(),
                                htmlP(" "),
                                htmlP(" "),
                                htmlBr(),
                                htmlBr(),
                                htmlP(" "),
                                dbcTabs(
                                    list(
                                        dbcTab(
                                            line_tab,
                                            label="Global COVID-19 Plot",
                                            tab_id="line-plot"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    width = 10
                )
            )
        ),
        fluid=TRUE
    )
)

app$callback(
    output('line-plot', 'figure'),
    list(input('feature-dropdown2', 'value'),
         input('country-selector', 'value'),
         input('scale-line-radio', 'value')
    ),
    function(ycol, countries, scale_type) {
        #  max_date <- df$date %>% max()
        # min_date <- df$date %>% min()
        filter_df <- filter_data(df, countries=countries)
        filter_df$hover <- with(filter_df, paste(" Date:", date, '<br>',
                                                 "Location: ", location, '<br>' 
        ))
        line_plot <- ggplot(filter_df,
                            aes(x = date,
                                y = !!sym(ycol),
                                color = location)) +
            geom_line(stat = 'summary', fun = mean) +
            ggtitle(paste0("Country data for ", ycol)) +
            scale_y_continuous(trans = scale_type) +
            scale_color_discrete(name="")
        
        line_plot <- line_plot %>%
            ggplotly()
    }
)

app$run_server(host = "0.0.0.0")