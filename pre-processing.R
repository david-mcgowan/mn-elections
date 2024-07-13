### trying to run the app yourself?
### use Ctrl+F to find "MODIFY FILE LOCATION IF NEEDED"
### then, as you might expect, modify those lines if needed

# building all packages from binary instead of source, for deployment
options(repos = "https://cran.rstudio.com/", type = "binary")


library(tidyverse)
library(readxl)
# library(shiny)
library(sf)
library(leaflet)
library(RColorBrewer)
library(knitr)
library(kableExtra)

# for blanking out NAs in tables
options(knitr.kable.NA = "")

# a function I often find handy...
`%notin%` <- Negate(`%in%`)

# function for sorting character vectors with elements like "1A", "10B"
alphanumeric <- function(vector) {
  # split the vector into two parts: numeric and non-numeric
  numeric_part <- str_extract(vector, "\\d+")
  alpha_part <- str_remove(vector, "\\d+")
  
  # combine the two parts into a dataframe and sort
  sorted_df <- tibble(numeric_part, alpha_part) %>% 
    mutate(numeric_part = as.integer(numeric_part)) %>%
    arrange(numeric_part, alpha_part)
  
  # put it together
  sorted_vec <- str_c(sorted_df$numeric_part, sorted_df$alpha_part)
  return(sorted_vec)
}

results_2020 <- st_read("Precinct Shapefiles/2010 Census/general_election_results_by_precinct_2020.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("usprs"), starts_with("ussen"),
                starts_with("usrep"), starts_with("mnsen"),
                starts_with("mnleg"), geometry) %>%
  st_transform(crs = 4326) # long and lat

results_2022 <- st_read("Precinct Shapefiles/2020 Census/general_election_results_by_precinct_2022.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("usrep"), starts_with("mnsen"),
                starts_with("mnleg"), starts_with("mngov"),
                starts_with("mnsos"), starts_with("mnaud"),
                starts_with("mnag"), geometry) %>%
  st_transform(crs = 4326)

counties <- st_read("County Shapefiles/mn_county_boundaries.shp") %>%
  st_transform(crs = 4326)

# we're leaving out the "metro only" feature of the old app
#  (I just don't think it's useful enough to port over)

# TODO: deal with candidates for every race

# code for district lookup here

# code for statewide table here

# code for statewide precinct map here

# code for statewide county map here

# code for congressional table here

# code for congressional map here

# code for state senate table here

# code for state senate map here

# code for state house table here

# code for state house map here
