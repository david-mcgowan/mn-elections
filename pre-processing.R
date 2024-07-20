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

add_party_polygons <- function(leaf, df) {
  # add any DFL constituencies
  if(sum(df$winner == "DFL") > 0) {
    leaf <- leaf %>%
      addPolygons(data = filter(df,
                                winner == "DFL"),
                  label = ~map(label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Blues",
                                            domain = 0:(max(abs_margin) + 10))(abs_margin),
                  fillOpacity = 3)
  }
  
  # add any Republican constituencies
  if(sum(df$winner == "Republican") > 0) {
    leaf <- leaf %>%
      addPolygons(data = filter(df,
                                winner == "Republican"),
                  label = ~map(label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Reds",
                                            domain = 0:(max(abs_margin) + 10))(abs_margin),
                  fillOpacity = 3)
  }
  
  # add any tied constituencies
  if(sum(df$winner == "Tie") > 0) {
    leaf <- leaf %>%
      addPolygons(data = filter(df,
                                winner == "Tie"),
                  label = ~map(label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = "#CCCCCC",
                  fillOpacity = 3)
  }
}

results_2012 <- st_read("Precinct Shapefiles/2010 Census/general_election_results_by_precinct_2012.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("usprs"), starts_with("ussen"),
                starts_with("usrep"), starts_with("mnsen"),
                starts_with("mnleg"), geometry) %>%
  mutate(Year = 2012) %>%
  st_transform(crs = 4326) # long and lat

results_2014 <- st_read("Precinct Shapefiles/2010 Census/general_election_results_by_precinct_2014.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("ussen"), starts_with("usrep"),
                starts_with("mnleg"), starts_with("mngov"),
                starts_with("mnsos"), starts_with("mnaud"),
                starts_with("mnag"), geometry) %>%
  mutate(Year = 2014) %>%
  st_transform(crs = 4326)

results_2016 <- st_read("Precinct Shapefiles/2010 Census/general_election_results_by_precinct_2016.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("usprs"), starts_with("usrep"),
                starts_with("mnsen"), starts_with("mnleg"), geometry) %>%
  mutate(Year = 2016) %>%
  st_transform(crs = 4326)

results_2018 <- st_read("Precinct Shapefiles/2010 Census/general_election_results_by_precinct_2018.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("ussen"), starts_with("ussse"),
                starts_with("usrep"), starts_with("mnsen"),
                starts_with("mnleg"), starts_with("mngov"),
                starts_with("mnsos"), starts_with("mnaud"),
                starts_with("mnag"), geometry) %>%
  mutate(Year = 2018) %>%
  st_transform(crs = 4326)

results_2020 <- st_read("Precinct Shapefiles/2010 Census/general_election_results_by_precinct_2020.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("usprs"), starts_with("ussen"),
                starts_with("usrep"), starts_with("mnsen"),
                starts_with("mnleg"), geometry) %>%
  # rename cols to match other datasets
  rename(usreptotal = usreptot,
         mnlegtotal = mnlegtot,
         mnsentotal = mnsentot) %>%
  mutate(Year = 2020) %>%
  st_transform(crs = 4326)

results_2022 <- st_read("Precinct Shapefiles/2020 Census/general_election_results_by_precinct_2022.shp") %>%
  janitor::clean_names() %>%
  dplyr::select(pctname, countyname, congdist, mnsendist, mnlegdist,
                totvoting, starts_with("usrep"), starts_with("mnsen"),
                starts_with("mnleg"), starts_with("mngov"),
                starts_with("mnsos"), starts_with("mnaud"),
                starts_with("mnag"), geometry) %>%
  mutate(Year = 2022) %>%
  st_transform(crs = 4326)

all_results <- bind_rows(results_2012, results_2014,
                         results_2016, results_2018,
                         results_2020, results_2022)

# remove single-year datasets from environment
rm(results_2012, results_2014, results_2016,
   results_2018, results_2020, results_2022)

# get statewide totals for statewide table
statewide_totals <- all_results %>%
  dplyr::select(-c(congdist, mnsendist, geometry), # remove numeric vars
                -starts_with("mnleg"),
                -starts_with("mnsen")) %>%
  as_tibble() %>% # remove spatial structure
  group_by(Year) %>%
  summarize(across(where(is.numeric),
                   ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = !Year,
               names_to = "Category",
               values_to = "Votes") %>%
  mutate(Office = case_when(str_detect(Category, "usprs") ~ "President",
                            str_detect(Category, "ussen") ~ "U.S. Senate",
                            str_detect(Category, "ussse") ~ "U.S. Senate (special)",
                            str_detect(Category, "usrep") ~ "U.S. House",
                            str_detect(Category, "mnag") ~ "Attorney General",
                            str_detect(Category, "mnaud") ~ "State Auditor",
                            str_detect(Category, "mngov") ~ "Governor",
                            str_detect(Category, "mnsos") ~ "Secretary of State",
                            Category == "totvoting" ~ "Total Votes"),
         Party = case_when(str_detect(Category, "total$") ~ "Total",
                           str_detect(Category, "dfl$") ~ "DFL",
                           str_detect(Category, "lib$") ~ "Libertarian",
                           str_detect(Category, "gp$") ~ "Green", # also for mgp$
                           str_detect(Category, "indkw$") ~ "Independent (Kanye West)",
                           str_detect(Category, "indbp$") ~ "Independent (Brock Pierce)",
                           str_detect(Category, "slp$") ~ "Socialism and Liberation",
                           str_detect(Category, "swp$") ~ "Socialist Workers",
                           str_detect(Category, "ia$") ~ "Independence-Alliance",
                           str_detect(Category, "lmn$") ~ "Legal Marijuana Now",
                           str_detect(Category, "glc$") ~ "Grassroots - Legalize Cannabis",
                           str_detect(Category, "wi$") ~ "Write-in",
                           str_detect(Category, "cg$") ~ "Constitutional Government",
                           str_detect(Category, "cp$") ~ "Constitution",
                           str_detect(Category, "jp$") ~ "Justice",
                           str_detect(Category, "adp$") ~ "American Delta",
                           str_detect(Category, "gr$") ~ "Grassroots",
                           str_detect(Category, "mop$") ~ "Open Progressives",
                           str_detect(Category, "ip$") ~ "Independence",
                           str_detect(Category, "lp$") ~ "Libertarian",
                           str_detect(Category, "ua$") ~ "Independent (Jerry Trooien)",
                           str_detect(Category, "sl$") ~ "Socialism and Liberation",
                           str_detect(Category, "r$") ~ "Republican",
                           Category == "totvoting" ~ "Total")) %>%
  dplyr::select(-Category)

totals_per_office <- statewide_totals %>%
  filter(Office != "Total Votes",
         Party == "Total") %>%
  select(Year, Office,
         Total = Votes) %>%
  filter(Total > 0)

statewide_totals <- statewide_totals %>%
  left_join(totals_per_office,
            by = c("Year", "Office")) %>%
  filter(!(is.na(Total) & Office != "Total Votes")) %>%
  mutate(Percentage = round(100 * Votes / Total, digits = 2))

# get percentages and labels for each race at each precinct
results_with_pcts <- all_results %>%
  mutate(r_pct_pres = round(100 * (usprsr / usprstotal),
                            digits = 2),
         dfl_pct_pres = round(100 * (usprsdfl / usprstotal),
                              digits = 2),
         other_pct_pres = round(100 - (r_pct_pres + dfl_pct_pres),
                                digits = 2),
         r_pct_sen = round(100 * (ussenr / ussentotal),
                           digits = 2),
         dfl_pct_sen = round(100 * (ussendfl / ussentotal),
                             digits = 2),
         other_pct_sen = round(100 - (r_pct_sen + dfl_pct_sen),
                               digits = 2),
         # r_pct_sen_spec = round(100 * (ussser / usssetotal),
         #                        digits = 2),
         # dfl_pct_sen_spec = round(100 * (usssedfl / usssetotal),
         #                          digits = 2),
         # other_pct_sen_spec = round(100 - (r_pct_sen_spec + dfl_pct_sen_spec),
         #                            digits = 2),
         r_pct_gov = round(100 * (mngovr / mngovtotal),
                           digits = 2),
         dfl_pct_gov = round(100 * (mngovdfl / mngovtotal),
                             digits = 2),
         other_pct_gov = round(100 - (r_pct_gov + dfl_pct_gov),
                               digits = 2),
         r_pct_sos = round(100 * (mnsosr / mnsostotal),
                           digits = 2),
         dfl_pct_sos = round(100 * (mnsosdfl / mnsostotal),
                             digits = 2),
         other_pct_sos = round(100 - (r_pct_sos + dfl_pct_sos),
                               digits = 2),
         r_pct_aud = round(100 * (mnaudr / mnaudtotal),
                           digits = 2),
         dfl_pct_aud = round(100 * (mnauddfl / mnaudtotal),
                             digits = 2),
         other_pct_aud = round(100 - (r_pct_aud + dfl_pct_aud),
                               digits = 2),
         r_pct_ag = round(100 * (mnagr / mnagtotal),
                          digits = 2),
         dfl_pct_ag = round(100 * (mnagdfl / mnagtotal),
                            digits = 2),
         other_pct_ag = round(100 - (r_pct_ag + dfl_pct_ag),
                              digits = 2),
         r_pct_congress = round(100 * (usrepr / usreptotal),
                                digits = 2),
         dfl_pct_congress = round(100 * (usrepdfl / usreptotal),
                                  digits = 2),
         other_pct_congress = round(100 - (r_pct_congress + dfl_pct_congress),
                                    digits = 2),
         r_pct_mnsen = round(100 * (mnsenr / mnsentotal),
                             digits = 2),
         dfl_pct_mnsen = round(100 * (mnsendfl / mnsentotal),
                               digits = 2),
         other_pct_mnsen = round(100 - (r_pct_mnsen + dfl_pct_mnsen),
                                 digits = 2),
         r_pct_mnhouse = round(100 * (mnlegr / mnlegtotal),
                               digits = 2),
         dfl_pct_mnhouse = round(100 * (mnlegdfl / mnlegtotal),
                                 digits = 2),
         other_pct_mnhouse = round(100 - (r_pct_mnhouse + dfl_pct_mnhouse),
                                   digits = 2)) %>%
  
  # fix pesky NaNs in precincts with 0 votes
  mutate(across(where(is.numeric),
                ~ ifelse(is.nan(.x),
                         0,
                         .x))) %>%
  
  # now, we need a margin variable for the plot
  mutate(pres_margin = dfl_pct_pres - r_pct_pres,
         sen_margin = dfl_pct_sen - r_pct_sen,
         # sen_spec_margin = dfl_pct_sen_spec - r_pct_sen_spec,
         gov_margin = dfl_pct_gov - r_pct_gov,
         sos_margin = dfl_pct_sos - r_pct_sos,
         aud_margin = dfl_pct_aud - r_pct_aud,
         ag_margin = dfl_pct_ag - r_pct_ag,
         congress_margin = dfl_pct_congress - r_pct_congress,
         mnsen_margin = dfl_pct_mnsen - r_pct_mnsen,
         mnhouse_margin = dfl_pct_mnhouse - r_pct_mnhouse,
         
         # indicating winning party
         pres_winner = case_when(pres_margin > 0 ~ "DFL",
                                 pres_margin < 0 ~ "Republican",
                                 pres_margin == 0 ~ "Tie"),
         sen_winner = case_when(sen_margin > 0 ~ "DFL",
                                sen_margin < 0 ~ "Republican",
                                sen_margin == 0 ~ "Tie"),
         # sen_spec_winner = case_when(sen_spec_margin > 0 ~ "DFL",
         #                             sen_spec_margin < 0 ~ "Republican",
         #                             sen_spec_margin == 0 ~ "Tie"),
         gov_winner = case_when(gov_margin > 0 ~ "DFL",
                                gov_margin < 0 ~ "Republican",
                                gov_margin == 0 ~ "Tie"),
         sos_winner = case_when(sos_margin > 0 ~ "DFL",
                                sos_margin < 0 ~ "Republican",
                                sos_margin == 0 ~ "Tie"),
         aud_winner = case_when(aud_margin > 0 ~ "DFL",
                                aud_margin < 0 ~ "Republican",
                                aud_margin == 0 ~ "Tie"),
         ag_winner = case_when(ag_margin > 0 ~ "DFL",
                               ag_margin < 0 ~ "Republican",
                               ag_margin == 0 ~ "Tie"),
         congress_winner = case_when(congress_margin > 0 ~ "DFL",
                                     congress_margin < 0 ~ "Republican",
                                     congress_margin == 0 ~ "Tie"),
         mnsen_winner = case_when(mnsen_margin > 0 ~ "DFL",
                                  mnsen_margin < 0 ~ "Republican",
                                  mnsen_margin == 0 ~ "Tie"),
         mnhouse_winner = case_when(mnhouse_margin > 0 ~ "DFL",
                                    mnhouse_margin < 0 ~ "Republican",
                                    mnhouse_margin == 0 ~ "Tie"),
         
         # converting margin to absolute value
         abs_pres_margin = abs(pres_margin),
         abs_sen_margin = abs(sen_margin),
         # abs_sen_spec_margin = abs(sen_spec_margin),
         abs_gov_margin = abs(gov_margin),
         abs_sos_margin = abs(sos_margin),
         abs_aud_margin = abs(aud_margin),
         abs_ag_margin = abs(ag_margin),
         abs_congress_margin = abs(congress_margin),
         abs_mnsen_margin = abs(mnsen_margin),
         abs_mnhouse_margin = abs(mnhouse_margin)) %>%
  # adding labels for our maps
  mutate(pres_label = str_c("Precinct: ",
                            pctname,
                            "<br/>",
                            "DFL: ",
                            usprsdfl,
                            " (",
                            dfl_pct_pres,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            usprsr,
                            " (",
                            r_pct_pres,
                            "%)",
                            "<br/>",
                            "Other ",
                            usprstotal - (usprsdfl + usprsr),
                            " (",
                            other_pct_pres,
                            "%)"),
         sen_label = str_c("Precinct: ",
                           pctname,
                           "<br/>",
                           "DFL: ",
                           ussendfl,
                           " (",
                           dfl_pct_sen,
                           "%)",
                           "<br/>",
                           "Republican: ",
                           ussenr,
                           " (",
                           r_pct_sen,
                           "%)",
                           "<br/>",
                           "Other ",
                           ussentotal - (ussendfl + ussenr),
                           " (",
                           other_pct_sen,
                           "%)"),
         # sen_spec_label = str_c("Precinct: ",
         #                        pctname,
         #                        "<br/>",
         #                        "DFL: ",
         #                        usssedfl,
         #                        " (",
         #                        dfl_pct_sen_spec,
         #                        "%)",
         #                        "<br/>",
         #                        "Republican: ",
         #                        ussser,
         #                        " (",
         #                        r_pct_sen_spec,
         #                        "%)",
         #                        "<br/>",
         #                        "Other: ",
         #                        usssetotal - (usssedfl + ussser),
         #                        " (",
         #                        other_pct_sen_spec,
         #                        "%)"),
         gov_label = str_c("Precinct: ",
                           pctname,
                           "<br/>",
                           "DFL: ",
                           mngovdfl,
                           " (",
                           dfl_pct_gov,
                           "%)",
                           "<br/>",
                           "Republican: ",
                           mngovr,
                           " (",
                           r_pct_gov,
                           "%)",
                           "<br/>",
                           "Other: ",
                           mngovtotal - (mngovdfl + mngovr),
                           " (",
                           other_pct_gov,
                           "%)"),
         sos_label = str_c("Precinct: ",
                           pctname,
                           "<br/>",
                           "DFL: ",
                           mnsosdfl,
                           " (",
                           dfl_pct_sos,
                           "%)",
                           "<br/>",
                           "Republican: ",
                           mnsosr,
                           " (",
                           r_pct_sos,
                           "%)",
                           "<br/>",
                           "Other: ",
                           mnsostotal - (mnsosdfl + mnsosr),
                           " (",
                           other_pct_sos,
                           "%)"),
         aud_label = str_c("Precinct: ",
                           pctname,
                           "<br/>",
                           "DFL: ",
                           mnauddfl,
                           " (",
                           dfl_pct_aud,
                           "%)",
                           "<br/>",
                           "Republican: ",
                           mnaudr,
                           " (",
                           r_pct_aud,
                           "%)",
                           "<br/>",
                           "Other: ",
                           mnaudtotal - (mnauddfl + mnaudr),
                           " (",
                           other_pct_aud,
                           "%)"),
         ag_label = str_c("Precinct: ",
                          pctname,
                          "<br/>",
                          "DFL: ",
                          mnagdfl,
                          " (",
                          dfl_pct_ag,
                          "%)",
                          "<br/>",
                          "Republican: ",
                          mnagr,
                          " (",
                          r_pct_ag,
                          "%)",
                          "<br/>",
                          "Other: ",
                          mnagtotal - (mnagdfl + mnagr),
                          " (",
                          other_pct_ag,
                          "%)"),
         congress_label = str_c("Precinct: ",
                                pctname,
                                "<br/>",
                                "DFL: ",
                                usrepdfl,
                                " (",
                                dfl_pct_congress,
                                "%)",
                                "<br/>",
                                "Republican: ",
                                usrepr,
                                " (",
                                r_pct_congress,
                                "%)",
                                "<br/>",
                                "Other: ",
                                usreptotal - (usrepdfl + usrepr),
                                " (",
                                other_pct_congress,
                                "%)"),
         mnsen_label = str_c("Precinct: ",
                             pctname,
                             "<br/>",
                             "DFL: ",
                             mnsendfl,
                             " (",
                             dfl_pct_mnsen,
                             "%)",
                             "<br/>",
                             "Republican: ",
                             mnsenr,
                             " (",
                             r_pct_mnsen,
                             "%)",
                             "<br/>",
                             "Other: ",
                             mnsentotal - (mnsendfl + mnsenr),
                             " (",
                             other_pct_mnsen,
                             "%)"),
         mnhouse_label = str_c("Precinct: ",
                               pctname,
                               "<br/>",
                               "DFL: ",
                               mnlegdfl,
                               " (",
                               dfl_pct_mnhouse,
                               "%)",
                               "<br/>",
                               "Republican: ",
                               mnlegr,
                               " (",
                               r_pct_mnhouse,
                               "%)",
                               "<br/>",
                               "Other: ",
                               mnlegtotal - (mnlegdfl + mnlegr),
                               " (",
                               other_pct_mnhouse,
                               "%)"))

counties <- st_read("County Shapefiles/mn_county_boundaries.shp") %>%
  st_transform(crs = 4326)

# we're leaving out the "metro only" feature of the old app
#  (I just don't think it's useful enough to port over)

# TODO: deal with candidates for every race

# code for district lookup here

# code for statewide table here

statewide_table.fcn <- function(office, year) {
  table_data <- statewide_totals %>%
    filter(Office == office,
           Year == year) %>%
    select(-c(Office, Year)) %>%
    arrange(desc(Percentage)) %>%
    arrange(Percentage == 100) %>% # move total votes to bottom of the table
    mutate(Votes = formatC(Votes, # formatting large numbers
                           format = "d",
                           big.mark = ","),
           Percentage = str_c(as.character(Percentage),
                              "%"))
  
  rowtotal <- nrow(table_data)
  
  kbl(table_data,
      format = "html",
      booktabs = TRUE,
      caption = str_c("Votes by Party for ",
                      office,
                      " in 2022"),
      align = "l") %>%
    kable_styling() %>%
    row_spec(rowtotal, bold = TRUE) # bolding row for total votes
}

# code for statewide precinct map here

statewide_map_precincts.fcn <- function(office, year) {
  # if(office %notin% c("Governor", "Secretary of State", "State Auditor", "Attorney General")) {
  #   stop("Oops! That wasn't a statewide election in 2022.")
  # }
  
  map_data <- results_with_pcts %>%
    filter(Year == year)
  
  if(office == "President") {
    map_data <- map_data %>%
      select(countyname, abs_pres_margin, pres_winner, pres_label) %>%
      rename(abs_margin = abs_pres_margin,
             winner = pres_winner,
             label = pres_label)
  } else if(office == "U.S. Senate") {
    map_data <- map_data %>%
      select(countyname, abs_sen_margin, sen_winner, sen_label) %>%
      rename(abs_margin = abs_sen_margin,
             winner = sen_winner,
             label = sen_label)
  } else if(office == "U.S. Senate (special)") {
    map_data <- map_data %>%
      select(countyname, abs_sen_spec_margin, sen_spec_winner, sen_spec_label) %>%
      rename(abs_margin = abs_sen_spec_margin,
             winner = sen_spec_winner,
             label = sen_spec_label)
  } else if(office == "Governor") {
    map_data <- map_data %>%
      select(countyname, abs_gov_margin, gov_winner, gov_label) %>%
      rename(abs_margin = abs_gov_margin,
             winner = gov_winner,
             label = gov_label)
  } else if(office == "Secretary of State") {
    map_data <- map_data %>%
      select(countyname, abs_sos_margin, sos_winner, sos_label) %>%
      rename(abs_margin = abs_sos_margin,
             winner = sos_winner,
             label = sos_label)
  } else if(office == "State Auditor") {
    map_data <- map_data %>%
      select(countyname, abs_aud_margin, aud_winner, aud_label) %>%
      rename(abs_margin = abs_aud_margin,
             winner = aud_winner,
             label = aud_label)
  } else if(office == "Attorney General") {
    map_data <- map_data %>%
      select(countyname, abs_ag_margin, ag_winner, ag_label) %>%
      rename(abs_margin = abs_ag_margin,
             winner = ag_winner,
             label = ag_label)
  }
  
  
  statewide_leaflet <- map_data %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron")
  
  statewide_leaflet <- add_party_polygons(statewide_leaflet, map_data)
  
  return(statewide_leaflet)
}

# code for statewide county map here

county_map_data <- all_results %>%
  dplyr::select(-geometry) %>%
  as_tibble() %>%
  group_by(Year, countyname) %>%
  summarize(across(where(is.numeric),
                   ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  arrange(countyname) %>%
  right_join(counties, # right join bc Lake and Cook counties have multiple geometries
             by = c("countyname" = "CTY_NAME"),
             relationship = "many-to-many") %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  # now let's get percentages for everything...
  mutate(r_pct_pres = round(100 * (usprsr / usprstotal),
                            digits = 2),
         dfl_pct_pres = round(100 * (usprsdfl / usprstotal),
                              digits = 2),
         other_pct_pres = round(100 - (r_pct_pres + dfl_pct_pres),
                                digits = 2),
         r_pct_sen = round(100 * (ussenr / ussentotal),
                           digits = 2),
         dfl_pct_sen = round(100 * (ussendfl / ussentotal),
                             digits = 2),
         other_pct_sen = round(100 - (r_pct_sen + dfl_pct_sen),
                               digits = 2),
         # r_pct_sen_spec = round(100 * (ussser / usssetotal),
         #                        digits = 2),
         # dfl_pct_sen_spec = round(100 * (usssedfl / usssetotal),
         #                          digits = 2),
         # other_pct_sen_spec = round(100 - (r_pct_sen_spec + dfl_pct_sen_spec),
         #                            digits = 2),
         r_pct_gov = round(100 * (mngovr / mngovtotal),
                           digits = 2),
         dfl_pct_gov = round(100 * (mngovdfl / mngovtotal),
                             digits = 2),
         other_pct_gov = round(100 - (r_pct_gov + dfl_pct_gov),
                               digits = 2),
         r_pct_sos = round(100 * (mnsosr / mnsostotal),
                           digits = 2),
         dfl_pct_sos = round(100 * (mnsosdfl / mnsostotal),
                             digits = 2),
         other_pct_sos = round(100 - (r_pct_sos + dfl_pct_sos),
                               digits = 2),
         r_pct_aud = round(100 * (mnaudr / mnaudtotal),
                           digits = 2),
         dfl_pct_aud = round(100 * (mnauddfl / mnaudtotal),
                             digits = 2),
         other_pct_aud = round(100 - (r_pct_aud + dfl_pct_aud),
                               digits = 2),
         r_pct_ag = round(100 * (mnagr / mnagtotal),
                          digits = 2),
         dfl_pct_ag = round(100 * (mnagdfl / mnagtotal),
                            digits = 2),
         other_pct_ag = round(100 - (r_pct_ag + dfl_pct_ag),
                              digits = 2)) %>%
  
  # now, we need a margin variable for the plot
  mutate(pres_margin = dfl_pct_pres - r_pct_pres,
         sen_margin = dfl_pct_sen - r_pct_sen,
         # sen_spec_margin = dfl_pct_sen_spec - r_pct_sen_spec,
         gov_margin = dfl_pct_gov - r_pct_gov,
         sos_margin = dfl_pct_sos - r_pct_sos,
         aud_margin = dfl_pct_aud - r_pct_aud,
         ag_margin = dfl_pct_ag - r_pct_ag,
         
         # indicating winning party
         pres_winner = case_when(pres_margin > 0 ~ "DFL",
                                 pres_margin < 0 ~ "Republican",
                                 pres_margin == 0 ~ "Tie"),
         sen_winner = case_when(sen_margin > 0 ~ "DFL",
                                sen_margin < 0 ~ "Republican",
                                sen_margin == 0 ~ "Tie"),
         # sen_spec_winner = case_when(sen_spec_margin > 0 ~ "DFL",
         #                             sen_spec_margin < 0 ~ "Republican",
         #                             sen_spec_margin == 0 ~ "Tie"),
         gov_winner = case_when(gov_margin > 0 ~ "DFL",
                                gov_margin < 0 ~ "Republican",
                                gov_margin == 0 ~ "Tie"),
         sos_winner = case_when(sos_margin > 0 ~ "DFL",
                                sos_margin < 0 ~ "Republican",
                                sos_margin == 0 ~ "Tie"),
         aud_winner = case_when(aud_margin > 0 ~ "DFL",
                                aud_margin < 0 ~ "Republican",
                                aud_margin == 0 ~ "Tie"),
         ag_winner = case_when(ag_margin > 0 ~ "DFL",
                               ag_margin < 0 ~ "Republican",
                               ag_margin == 0 ~ "Tie"),
         
         # converting margin to absolute value
         abs_pres_margin = abs(pres_margin),
         abs_sen_margin = abs(sen_margin),
         # abs_sen_spec_margin = abs(sen_spec_margin),
         abs_gov_margin = abs(gov_margin),
         abs_sos_margin = abs(sos_margin),
         abs_aud_margin = abs(aud_margin),
         abs_ag_margin = abs(ag_margin)) %>%
  # adding labels for our maps
  mutate(pres_label = str_c(countyname,
                            " County",
                            "<br/>",
                            "DFL: ",
                            usprsdfl,
                            " (",
                            dfl_pct_pres,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            usprsr,
                            " (",
                            r_pct_pres,
                            "%)",
                            "<br/>",
                            "Other ",
                            usprstotal - (usprsdfl + usprsr),
                            " (",
                            other_pct_pres,
                            "%)"),
         sen_label = str_c(countyname,
                           " County",
                           "<br/>",
                           "DFL: ",
                           ussendfl,
                           " (",
                           dfl_pct_sen,
                           "%)",
                           "<br/>",
                           "Republican: ",
                           ussenr,
                           " (",
                           r_pct_sen,
                           "%)",
                           "<br/>",
                           "Other ",
                           ussentotal - (ussendfl + ussenr),
                           " (",
                           other_pct_sen,
                           "%)"),
         # sen_spec_label = str_c("Precinct: ",
         #                        pctname,
         #                        "<br/>",
         #                        "DFL: ",
         #                        usssedfl,
         #                        " (",
         #                        dfl_pct_sen_spec,
         #                        "%)",
         #                        "<br/>",
         #                        "Republican: ",
         #                        ussser,
         #                        " (",
         #                        r_pct_sen_spec,
         #                        "%)",
         #                        "<br/>",
         #                        "Other: ",
         #                        usssetotal - (usssedfl + ussser),
         #                        " (",
         #                        other_pct_sen_spec,
         #                        "%)"),
         gov_label = str_c(countyname,
                            " County",
                            "<br/>",
                            "DFL: ",
                            mngovdfl,
                            " (",
                            dfl_pct_gov,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            mngovr,
                            " (",
                            r_pct_gov,
                            "%)",
                            "<br/>",
                            "Other: ",
                            mngovtotal - (mngovdfl + mngovr),
                            " (",
                            other_pct_gov,
                            "%)"),
         sos_label = str_c(countyname,
                            " County",
                            "<br/>",
                            "DFL: ",
                            mnsosdfl,
                            " (",
                            dfl_pct_sos,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            mnsosr,
                            " (",
                            r_pct_sos,
                            "%)",
                            "<br/>",
                            "Other: ",
                            mnsostotal - (mnsosdfl + mnsosr),
                            " (",
                            other_pct_sos,
                            "%)"),
         aud_label = str_c(countyname,
                            " County",
                            "<br/>",
                            "DFL: ",
                            mnauddfl,
                            " (",
                            dfl_pct_aud,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            mnaudr,
                            " (",
                            r_pct_aud,
                            "%)",
                            "<br/>",
                            "Other: ",
                            mnaudtotal - (mnauddfl + mnaudr),
                            " (",
                            other_pct_aud,
                            "%)"),
         ag_label = str_c(countyname,
                            " County",
                            "<br/>",
                            "DFL: ",
                            mnagdfl,
                            " (",
                            dfl_pct_ag,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            mnagr,
                            " (",
                            r_pct_ag,
                            "%)",
                            "<br/>",
                            "Other: ",
                            mnagtotal - (mnagdfl + mnagr),
                            " (",
                            other_pct_ag,
                            "%)"))

statewide_map_counties.fcn <- function(office, year) {
  # if(office %notin% c("Governor", "Secretary of State", "State Auditor", "Attorney General")) {
  #   stop("Oops! That wasn't a statewide election in 2022.")
  # }
  
  map_data <- county_map_data %>%
    filter(Year == year)
  
  if(office == "President") {
    map_data <- map_data %>%
      select(countyname, abs_pres_margin, pres_winner, pres_label) %>%
      rename(abs_margin = abs_pres_margin,
             winner = pres_winner,
             label = pres_label)
  } else if(office == "U.S. Senate") {
    map_data <- map_data %>%
      select(countyname, abs_sen_margin, sen_winner, sen_label) %>%
      rename(abs_margin = abs_sen_margin,
             winner = sen_winner,
             label = sen_label)
  } else if(office == "U.S. Senate (special)") {
    map_data <- map_data %>%
      select(countyname, abs_sen_spec_margin, sen_spec_winner, sen_spec_label) %>%
      rename(abs_margin = abs_sen_spec_margin,
             winner = sen_spec_winner,
             label = sen_spec_label)
  } else if(office == "Governor") {
    map_data <- map_data %>%
      select(countyname, abs_gov_margin, gov_winner, gov_label) %>%
      rename(abs_margin = abs_gov_margin,
             winner = gov_winner,
             label = gov_label)
  } else if(office == "Secretary of State") {
    map_data <- map_data %>%
      select(countyname, abs_sos_margin, sos_winner, sos_label) %>%
      rename(abs_margin = abs_sos_margin,
             winner = sos_winner,
             label = sos_label)
  } else if(office == "State Auditor") {
    map_data <- map_data %>%
      select(countyname, abs_aud_margin, aud_winner, aud_label) %>%
      rename(abs_margin = abs_aud_margin,
             winner = aud_winner,
             label = aud_label)
  } else if(office == "Attorney General") {
    map_data <- map_data %>%
      select(countyname, abs_ag_margin, ag_winner, ag_label) %>%
      rename(abs_margin = abs_ag_margin,
             winner = ag_winner,
             label = ag_label)
  }
  
  
  statewide_leaflet <- map_data %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron")
  
  statewide_leaflet <- add_party_polygons(statewide_leaflet, map_data)
  
  return(statewide_leaflet)
}

# code for congressional table here

# code for congressional map here

# code for state senate table here

# code for state senate map here

# code for state house table here

# code for state house map here
