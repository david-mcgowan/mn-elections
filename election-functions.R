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


# reading in main data
### MODIFY FILE LOCATION IF NEEDED
precincts <- read_xlsx("2022-general-federal-state-results-by-precinct-official.xlsx",
                       sheet = 2,
                       trim_ws = TRUE, # remove whitespace
                       col_names = TRUE) # col names from sheet

# fixing up tibble
precincts <- precincts %>%
  rename_all(.funs = str_to_lower) %>% # lowercase col names
  select(-c(countycode, ctycomdist, juddist, swcdist, # remove unnecessary cols
            ward, hospdist, parkdist, tabmodel,
            mailballot, reg7am, edr, signatures,
            ab_mb, fedonlyab, totvoting)) %>%
  # fixing capitalization of some county names
  mutate(countyname = str_replace(countyname,
                                  "Lac Qui Parle",
                                  "Lac qui Parle"),
         countyname = str_replace(countyname,
                                  "Lake Of The Woods",
                                  "Lake of the Woods"))

precincts <- precincts[-4104,] # removing empty row

# totalling votes in all categories across all precincts
vote_totals <- precincts %>%
  select(starts_with("mngov"), starts_with("mnsos"),
         starts_with("mnaud"), starts_with("mnag")) %>%
  colSums()

# dataset for statewide elections tables
statewide_table <- tibble(Office = c("Governor",
                                     "Secretary of State",
                                     "State Auditor",
                                     "Attorney General"),
                          DFL = vote_totals[c("mngovdfl",
                                              "mnsosdfl",
                                              "mnauddfl",
                                              "mnagdfl")],
                          Republican = vote_totals[c("mngovr",
                                                     "mnsosr",
                                                     "mnaudr",
                                                     "mnagr")],
                          `Grassroots - Legalize Cannabis` = c(vote_totals["mngovglc"],
                                                               NA,
                                                               vote_totals["mnaudglc"],
                                                               NA),
                          `Legal Marijuana Now` = c(vote_totals["mngovlmn"],
                                                    NA,
                                                    vote_totals["mnaudlmn"],
                                                    NA),
                          `Independence-Alliance` = c(vote_totals["mngovia"],
                                                      NA,
                                                      NA,
                                                      NA),
                          `Socialist Workers Party` = c(vote_totals["mngovswp"],
                                                        NA,
                                                        NA,
                                                        NA),
                          `Write-in` = vote_totals[c("mngovwi",
                                                     "mnsoswi",
                                                     "mnaudwi",
                                                     "mnagwi")],
                          Total = vote_totals[c("mngovtotal",
                                                "mnsostotal",
                                                "mnaudtotal",
                                                "mnagtotal")]) %>%
  mutate(dfl_pct = DFL / Total,
         r_pct = Republican / Total,
         glc_pct = `Grassroots - Legalize Cannabis` / Total,
         lmn_pct = `Legal Marijuana Now` / Total,
         ia_pct = `Independence-Alliance` / Total,
         swp_pct = `Socialist Workers Party` / Total,
         wi_pct = `Write-in` / Total,
         total_pct = NA)

# this will help us get percentages into the upcoming pivoted dataset
pcts <- rep(NA, 8 * 4) # 8 parties for 4 offices

for(i in 1:4) {
  pcts[8 * (i-1) + 1] <- statewide_table$dfl_pct[i]
  pcts[8 * (i-1) + 2] <- statewide_table$r_pct[i]
  pcts[8 * (i-1) + 3] <- statewide_table$glc_pct[i]
  pcts[8 * (i-1) + 4] <- statewide_table$lmn_pct[i]
  pcts[8 * (i-1) + 5] <- statewide_table$ia_pct[i]
  pcts[8 * (i-1) + 6] <- statewide_table$swp_pct[i]
  pcts[8 * (i-1) + 7] <- statewide_table$wi_pct[i]
  pcts[8 * (i-1) + 8] <- NA
}

# here's our final dataset for making statewide tables!
statewide_table <- statewide_table %>%
  pivot_longer(cols = -c(Office, contains("_pct")),
               names_to = "Party",
               values_to = "Votes") %>%
  select(Office, Party, Votes) %>%
  bind_cols(pcts) %>%
  mutate(Percentage = round(100 * pcts, digits = 2)) %>%
  select(Office, Party, Votes, Percentage) %>%
  cbind(Candidate = c("Tim Walz (i)", # candidates for governor
                      "Scott Jensen",
                      "Steve Patterson",
                      "James McCaskel",
                      "Hugh McTavish",
                      "Gabrielle Prosser",
                      NA,
                      NA,
                      "Steve Simon (i)", # candidates for SOS
                      "Kim Crockett",
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      "Julie Blaha (i)", # candidates for auditor
                      "Ryan Wilson",
                      "Will Finn",
                      "Tim Davis",
                      NA,
                      NA,
                      NA,
                      NA,
                      "Keith Ellison (i)", # candidates for AG
                      "Jim Schultz",
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA)) %>%
  as_tibble() %>%
  select(Office, Party, Candidate, everything())

# ... after removing a few pointless rows (no candidate of a party)
statewide_table <- statewide_table[-c(11:14, 21:22, 27:30),]
  
# function for making a statewide election table!
statewide_table.fcn <- function(office) {
  table_data <- statewide_table %>%
    filter(Office == office) %>%
    select(-Office) %>%
    arrange(desc(Percentage)) %>%
    mutate(Votes = formatC(Votes, # formatting large numbers
                           format = "d",
                           big.mark = ","),
           Percentage = str_c(as.character(Percentage),
                              "%"))
  
  rowtotal <- nrow(table_data)
  
  kbl(table_data,
      format = "html",
      booktabs = TRUE,
      caption = str_c("Votes by Party for Minnesota ",
                      office,
                      " in 2022"),
      align = "l") %>%
    kable_styling() %>%
    row_spec(rowtotal, bold = TRUE) # bolding row for total votes
}



# dataset for statewide elections maps
statewide_counties <- precincts %>%
  select(countyname,
         starts_with("mngov"), starts_with("mnsos"),
         starts_with("mnaud"), starts_with("mnag")) %>%
  group_by(countyname) %>%
  summarize(across(starts_with("mn"),
                   sum)) %>%
  # now let's get percentages for everything...
  mutate(r_pct_gov = round(100 * (mngovr / mngovtotal),
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
  mutate(gov_margin = dfl_pct_gov - r_pct_gov,
         sos_margin = dfl_pct_sos - r_pct_sos,
         aud_margin = dfl_pct_aud - r_pct_aud,
         ag_margin = dfl_pct_ag - r_pct_ag,
         
         # indicating winning party
         gov_winner = ifelse(gov_margin > 0,
                             "DFL",
                             "Republican"),
         sos_winner = ifelse(sos_margin > 0,
                             "DFL",
                             "Republican"),
         aud_winner = ifelse(aud_margin > 0,
                             "DFL",
                             "Republican"),
         ag_winner = ifelse(ag_margin > 0,
                            "DFL",
                            "Republican"),
         
         # converting margin to absolute value
         abs_gov_margin = abs(gov_margin),
         abs_sos_margin = abs(sos_margin),
         abs_aud_margin = abs(aud_margin),
         abs_ag_margin = abs(ag_margin))

# reading in county-level shapefile
### MODIFY FILE LOCATION IF NEEDED
mn_counties <- st_read("County Shapefiles/mn_counties.shp") %>%
  janitor::clean_names() %>%
  select(c(name, geometry)) %>%
  arrange(name) %>%
  inner_join(statewide_counties,
             by = c("name" = "countyname")) %>%
  # adding labels for our maps
  mutate(gov_label = str_c(name,
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
         sos_label = str_c(name,
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
         aud_label = str_c(name,
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
         ag_label = str_c(name,
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

# converting coords to longitude and latitude
mn_counties <- st_transform(mn_counties,
                            crs = 4326) # longitude and latitude


# function for making a statewide election map!
statewide_map.fcn <- function(office) {
  if(office %notin% c("Governor", "Secretary of State", "State Auditor", "Attorney General")) {
    stop("Oops! That wasn't a statewide election in 2022.")
  }
  
  
  if(office == "Governor") {
    map_data <- mn_counties %>%
      select(name, abs_gov_margin, gov_winner, gov_label) %>%
      rename(abs_margin = abs_gov_margin,
             winner = gov_winner,
             label = gov_label)
  } else if(office == "Secretary of State") {
    map_data <- mn_counties %>%
      select(name, abs_sos_margin, sos_winner, sos_label) %>%
      rename(abs_margin = abs_sos_margin,
             winner = sos_winner,
             label = sos_label)
  } else if(office == "State Auditor") {
    map_data <- mn_counties %>%
      select(name, abs_aud_margin, aud_winner, aud_label) %>%
      rename(abs_margin = abs_aud_margin,
             winner = aud_winner,
             label = aud_label)
  } else {
    map_data <- mn_counties %>%
      select(name, abs_ag_margin, ag_winner, ag_label) %>%
      rename(abs_margin = abs_ag_margin,
             winner = ag_winner,
             label = ag_label)
  }
  
  
  statewide_leaflet <- map_data %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    # adding DFL counties
    addPolygons(data = filter(map_data,
                              winner == "DFL"),
                label = ~map(label, HTML),
                color = "black",
                weight = 2,
                fillColor = ~colorNumeric("Blues",
                                          domain = 0:(max(abs_margin) + 10))(abs_margin),
                fillOpacity = 3) %>%
    # adding Republican counties
    addPolygons(data = filter(map_data,
                              winner == "Republican"),
                label = ~map(label, HTML),
                color = "black",
                weight = 2,
                fillColor = ~colorNumeric("Reds",
                                          domain = 0:(max(abs_margin) + 10))(abs_margin),
                fillOpacity = 3)
  
  return(statewide_leaflet)
}

### data and functions for MN Senate and House tabs

senate_full_table <- precincts %>%
  group_by(mnsendist) %>%
  summarize(r_votes = sum(mnsenr),
            dfl_votes = sum(mnsendfl),
            total_votes = sum(mnsentotal),
            other_votes = total_votes - (r_votes + dfl_votes),
            winner = ifelse(dfl_votes > r_votes,
                            "DFL",
                            "Republican"),
            r_pct = 100 * r_votes / total_votes,
            dfl_pct = 100 * dfl_votes / total_votes,
            other_pct = 100 * other_votes / total_votes) %>%
  mutate(r_pct = round(r_pct, digits = 2),
         dfl_pct = round(dfl_pct, digits = 2),
         other_pct = round(other_pct, digits = 2)) %>%
  select(mnsendist, winner,
         r_votes, r_pct,
         dfl_votes, dfl_pct,
         other_votes, other_pct,
         total_votes) %>%
  arrange(as.numeric(mnsendist))

# manually decided which house and senate districts were part of the metro
house_metro <- c("48A", "48B",
                 "45A", "37A",
                 "42A", "34A",
                 "31A", "35A",
                 "35B", "32A",
                 "32B", "36A",
                 "33A", "33B",
                 "41A", "41B",
                 "44A", "44B",
                 "47B", "53B",
                 "53A", "56B",
                 "56A", "57A",
                 "57B", "54B",
                 "54A", "55A",
                 "55B", "47A",
                 "37B", "34B",
                 "40A", "40B",
                 "39A", "38A",
                 "38B", "36B",
                 "43A", "42B",
                 "39B", "43B",
                 "45B", "46A",
                 "46B", "49A",
                 "49B", "50A",
                 "50B", "51A",
                 "51B", "52A",
                 "52B", "59A",
                 "59B", "60A",
                 "60B", "61A",
                 "61B", "62A",
                 "62B", "63A",
                 "63B", "64A",
                 "64B", "65A",
                 "65B", "66A",
                 "66B", "67A",
                 "67B", "31B") %>%
  alphanumeric()
house_nonmetro <- setdiff(c(str_c(1:67, "A"),
                            str_c(1:67, "B")),
                          house_metro) %>%
  alphanumeric()

senate_metro <- c(31:57, 59:67) # district 58 is not metro
senate_nonmetro <- c(1:30, 58)


# here, I scraped legislature candidate names! then saved as csv for easy loading

# senate_html <- read_html("https://ballotpedia.org/Minnesota_State_Senate_elections,_2022")
# senate_nodes <- html_nodes(senate_html, "td:nth-child(2) .candidate")
# senate_dfl_candidates <- html_text(senate_nodes)[1:64] # finagling the input
# senate_dfl_candidates <- c("no candidate", # accounting for districts with no DFL candidate
#                            senate_dfl_candidates[1:19],
#                            "no candidate",
#                            "no candidate",
#                            senate_dfl_candidates[20:64])
# senate_dfl_candidates <- str_remove_all(senate_dfl_candidates,
#                                         "\\t") %>% # removing whitespace
#                          str_trim() # removing more whitespace!
# 
# senate_nodes <- html_nodes(senate_html, "td:nth-child(3) .candidate")
# senate_r_candidates <- html_text(senate_nodes)[1:61]
# senate_r_candidates <- c(senate_r_candidates[1:42],
#                          "no candidate",
#                          senate_r_candidates[43:44],
#                          "no candidate",
#                          senate_r_candidates[45:56],
#                          "no candidate",
#                          "no candidate",
#                          "no candidate",
#                          senate_r_candidates[57:61],
#                          "no candidate")
# senate_r_candidates <- str_remove_all(senate_r_candidates,
#                                       "\\t") %>% # removing whitespace
#                        str_trim() # removing more whitespace!
#
# write_csv(as.data.frame(senate_dfl_candidates), "./final/senate_dfl_candidates.csv")
# write_csv(as.data.frame(senate_r_candidates), "./final/senate_r_candidates.csv")
# 
#
# house_html <- read_html("https://ballotpedia.org/Minnesota_House_of_Representatives_elections,_2022")
# house_dfl_nodes <- html_nodes(house_html,
#                           "td:nth-child(2) .candidate")
# house_dfl_candidates <- html_text(house_dfl_nodes)[1:129] %>%
#   str_trim() %>%
#   str_remove_all("\\*")
# house_dfl_candidates <- c(house_dfl_candidates[1:9],
#                           "no candidate",
#                           house_dfl_candidates[10:17],
#                           "no candidate",
#                           house_dfl_candidates[18:31],
#                           "no candidate",
#                           house_dfl_candidates[32:48],
#                           "no candidate",
#                           house_dfl_candidates[49:53],
#                           "no candidate",
#                           house_dfl_candidates[54:129])
# house_r_nodes <- html_nodes(house_html,
#                             "td:nth-child(3) .candidate")
# house_r_candidates <- html_text(house_r_nodes)[1:122] %>%
#   str_trim() %>%
#   str_remove_all("\\*")
# house_r_candidates <- c(house_r_candidates[1:74],
#                         "no candidate",
#                         house_r_candidates[75:83],
#                         "no candidate",
#                         "no candidate",
#                         house_r_candidates[84:87],
#                         "no candidate",
#                         "no candidate",
#                         house_r_candidates[88:111],
#                         "no candidate",
#                         "no candidate",
#                         house_r_candidates[112],
#                         "no candidate",
#                         "no candidate",
#                         "no candidate",
#                         "no candidate",
#                         house_r_candidates[113:114],
#                         "no candidate",
#                         house_r_candidates[115:122])
# 
# write_csv(as.data.frame(house_dfl_candidates), "./final/house_dfl_candidates.csv")
# write_csv(as.data.frame(house_r_candidates), "./final/house_r_candidates.csv")

### MODIFY FILE LOCATION IF NEEDED
senate_r_candidates <- read_csv("Candidate Files/senate_r_candidates.csv")
senate_dfl_candidates <- read_csv("Candidate Files/senate_dfl_candidates.csv")
house_r_candidates <- read_csv("Candidate Files/house_r_candidates.csv")
house_dfl_candidates <- read_csv("Candidate Files/house_dfl_candidates.csv")


# adding candidate names to dataset
senate_full_table <- cbind(senate_full_table,
                           senate_dfl_candidates,
                           senate_r_candidates) %>%
  as_tibble() %>%
  rename(senate_dfl_candidate = senate_dfl_candidates,
         senate_r_candidate = senate_r_candidates)

# function for making a senate election table!
senate_table.fcn <- function(district) {
  table <- tibble(Party = c("Republican", "DFL", "Other", "Total"),
                  Candidate = c(senate_full_table$senate_r_candidate[district],
                                senate_full_table$senate_dfl_candidate[district],
                                NA, NA),
                  Votes = c(senate_full_table$r_votes[district],
                            senate_full_table$dfl_votes[district],
                            senate_full_table$other_votes[district],
                            senate_full_table$total_votes[district]),
                  Percentage = c(senate_full_table$r_pct[district],
                                 senate_full_table$dfl_pct[district],
                                 senate_full_table$other_pct[district],
                                 NA)) %>%
    arrange(desc(Percentage)) %>%
    mutate(Votes = formatC(Votes, # formatting large numbers
                           format = "d",
                           big.mark = ","),
           Percentage = str_c(as.character(Percentage),
                              "%")) %>%
    # cleaning up table if candidate not fielded
    mutate(Votes = ifelse(Votes == "0" & Party != "Other",
                          NA,
                          Votes),
           Percentage = ifelse(Votes == "0" & Party != "Other",
                               NA,
                               Percentage))
  
  kable <- kbl(table,
               format = "html",
               booktabs = TRUE,
               caption = str_c("Votes in MN Senate District ",
                               district,
                               " in 2022"),
               align = "l") %>%
    kable_styling() %>%
    row_spec(4, bold = TRUE)
  
  # if one party didn't field a candidate, italicize their row
  if(sum(table$Candidate == "no candidate", na.rm = TRUE) == 1) {
    kable <- kable %>%
      row_spec(3, italic = TRUE, font_size = 13)
  }
  
  return(kable)
}


# senate mapping

### MODIFY FILE LOCATION IF NEEDED
legislative_precincts <- st_read("Precinct Shapefiles/bdry_votingdistrictis.shp") %>%
  janitor::clean_names() %>%
  select(vtdid,
         pctname, pctcode,
         countyname, countyfips,
         mnsendist, mnlegdist,
         geometry) %>%
  inner_join(precincts, by = "vtdid") %>%
  select(vtdid, pctname.x, pctcode.x,
         countyname.x, countyfips,
         mnsendist.x, mnlegdist.x,
         starts_with("mnsen"),
         starts_with("mnleg"),
         starts_with("usrep"),
         congdist) %>%
  rename(pctname = pctname.x,
         pctcode = pctcode.x,
         countyname = countyname.x,
         mnsendist = mnsendist.x,
         mnlegdist = mnlegdist.x)

legislative_precincts <- legislative_precincts %>%
  # adding percentage variables
  mutate(r_pct_senate = round(100 * mnsenr / mnsentotal,
                              digits = 2),
         r_pct_senate = ifelse(is.nan(r_pct_senate) == TRUE,
                               0,
                               r_pct_senate),
         
         dfl_pct_senate = round(100 * mnsendfl / mnsentotal,
                                digits = 2),
         dfl_pct_senate = ifelse(is.nan(dfl_pct_senate) == TRUE,
                                 0,
                                 dfl_pct_senate),
         
         other_pct_senate = round(100 - (r_pct_senate + dfl_pct_senate),
                                  digits = 2),
         other_pct_senate = ifelse(is.nan(other_pct_senate) == TRUE,
                                   0,
                                   other_pct_senate),
         
         r_pct_house = round(100 * mnlegr / mnlegtotal,
                             digits = 2),
         r_pct_house = ifelse(is.nan(r_pct_house) == TRUE,
                              0,
                              r_pct_house),
         
         dfl_pct_house = round(100 * mnlegdfl / mnlegtotal,
                               digits = 2),
         dfl_pct_house = ifelse(is.nan(dfl_pct_house) == TRUE,
                                0,
                                dfl_pct_house),
         
         other_pct_house = round(100 - (r_pct_house + dfl_pct_house),
                                 digits = 2),
         other_pct_house = ifelse(is.nan(other_pct_house) == TRUE,
                                  0,
                                  other_pct_house),
         
         r_pct_congress = round(100 * usrepr / usreptotal,
                                digits = 2),
         r_pct_congress = ifelse(is.nan(r_pct_congress) == TRUE,
                                 0,
                                 r_pct_congress),
         
         dfl_pct_congress = round(100 * usrepdfl / usreptotal,
                               digits = 2),
         dfl_pct_congress = ifelse(is.nan(dfl_pct_congress) == TRUE,
                                0,
                                dfl_pct_congress),
         
         other_pct_congress = round(100 - (r_pct_congress + dfl_pct_congress),
                                 digits = 2),
         other_pct_congress = ifelse(is.nan(other_pct_congress) == TRUE,
                                  0,
                                  other_pct_congress)) %>%
  
  # adding margin and winner variables
  mutate(senate_margin = dfl_pct_senate - r_pct_senate,
         house_margin = dfl_pct_house - r_pct_house,
         congress_margin = dfl_pct_congress - r_pct_congress,
         
         # indicating winning party
         senate_winner = ifelse(senate_margin > 0,
                             "DFL",
                             "Republican"),
         senate_winner = ifelse(senate_margin == 0,
                                "Tie",
                                senate_winner),
         house_winner = ifelse(house_margin > 0,
                             "DFL",
                             "Republican"),
         house_winner = ifelse(house_margin == 0,
                               "Tie",
                               house_winner),
         congress_winner = ifelse(congress_margin > 0,
                                  "DFL",
                                  "Republican"),
         congress_winner = ifelse(congress_margin == 0,
                                  "Tie",
                                  congress_winner),
         
         # converting margin to absolute value
         abs_senate_margin = abs(senate_margin),
         abs_house_margin = abs(house_margin),
         abs_congress_margin = abs(congress_margin),
         
         # label variables for mapping
         senate_label = str_c("Precinct: ",
                            pctname,
                            "<br/>",
                            "DFL: ",
                            mnsendfl,
                            " (",
                            dfl_pct_senate,
                            "%)",
                            "<br/>",
                            "Republican: ",
                            mnsenr,
                            " (",
                            r_pct_senate,
                            "%)",
                            "<br/>",
                            "Other: ",
                            mnsentotal - (mnsendfl + mnsenr),
                            " (",
                            other_pct_senate,
                            "%)"),
         
         house_label = str_c("Precinct: ",
                              pctname,
                              "<br/>",
                              "DFL: ",
                              mnlegdfl,
                              " (",
                              dfl_pct_house,
                              "%)",
                              "<br/>",
                              "Republican: ",
                              mnlegr,
                              " (",
                              r_pct_house,
                              "%)",
                              "<br/>",
                              "Other: ",
                              mnlegtotal - (mnlegdfl + mnlegr),
                              " (",
                              other_pct_house,
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
                             "%)"))

# converting to longitude and latitude
legislative_precincts <- st_transform(legislative_precincts,
                                      crs = 4326) # long and lat

# function for senate leaflet
senate_map.fcn <- function(district) {
  if(is.numeric(district) == FALSE) {
    stop("That's not a Senate district, silly!")
  } else {
    district_data <- legislative_precincts %>%
      filter(mnsendist == district)
    
    # initialize leaflet
    district_leaflet <- district_data %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron")
  }
  
  # add any DFL precincts
  if(sum(district_data$senate_winner == "DFL") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                senate_winner == "DFL"),
                  label = ~map(senate_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Blues",
                                            domain = 0:(max(abs_senate_margin) + 10))(abs_senate_margin),
                  fillOpacity = 3)
  }
  
  # add any Republican precincts
  if(sum(district_data$senate_winner == "Republican") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                senate_winner == "Republican"),
                  label = ~map(senate_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Reds",
                                            domain = 0:(max(abs_senate_margin) + 10))(abs_senate_margin),
                  fillOpacity = 3)
  }
  
  # add any tied precincts
  if(sum(district_data$senate_winner == "Tie") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                senate_winner == "Tie"),
                  label = ~map(senate_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = "#CCCCCC",
                  fillOpacity = 3)
  }
    return(district_leaflet)
}


# house table time
house_full_table <- precincts %>%
  group_by(mnlegdist) %>%
  summarize(r_votes = sum(mnlegr),
            dfl_votes = sum(mnlegdfl),
            total_votes = sum(mnlegtotal),
            other_votes = total_votes - (r_votes + dfl_votes),
            winner = ifelse(dfl_votes > r_votes,
                            "DFL",
                            "Republican"),
            r_pct = 100 * r_votes / total_votes,
            dfl_pct = 100 * dfl_votes / total_votes,
            other_pct = 100 * other_votes / total_votes) %>%
  mutate(r_pct = round(r_pct, digits = 2),
         dfl_pct = round(dfl_pct, digits = 2),
         other_pct = round(other_pct, digits = 2)) %>%
  select(mnlegdist, winner,
         r_votes, r_pct,
         dfl_votes, dfl_pct,
         other_votes, other_pct,
         total_votes) %>%
  # long-winded way to sort by district number
  # (legacy from before I wrote the alphanumeric() fcn)
  arrange(as.numeric(str_sub(mnlegdist, 1, -2))) %>%
  cbind(house_dfl_candidates,
        house_r_candidates) %>%
  as_tibble() %>%
  rename(house_dfl_candidate = house_dfl_candidates,
         house_r_candidate = house_r_candidates)

# function for making a house election table!
house_table.fcn <- function(district) {
  # need to use district number to calculate location in dataset
  if(str_sub(district, -1) == "A") {
    district_rownum <- as.integer(str_sub(district, 1, -2)) * 2 - 1
  } else {
    district_rownum <- as.integer(str_sub(district, 1, -2)) * 2
  }
  
  table <- tibble(Party = c("Republican", "DFL", "Other", "Total"),
                  Candidate = c(house_full_table$house_r_candidate[district_rownum],
                                house_full_table$house_dfl_candidate[district_rownum],
                                NA, NA),
                  Votes = c(house_full_table$r_votes[district_rownum],
                            house_full_table$dfl_votes[district_rownum],
                            house_full_table$other_votes[district_rownum],
                            house_full_table$total_votes[district_rownum]),
                  Percentage = c(house_full_table$r_pct[district_rownum],
                                 house_full_table$dfl_pct[district_rownum],
                                 house_full_table$other_pct[district_rownum],
                                 NA)) %>%
    arrange(desc(Percentage)) %>%
    mutate(Votes = formatC(Votes, # formatting large numbers
                           format = "d",
                           big.mark = ","),
           Percentage = str_c(as.character(Percentage),
                              "%")) %>%
    # cleaning up table if candidate not fielded
    mutate(Votes = ifelse(Votes == "0" & Party != "Other",
                          NA,
                          Votes),
           Percentage = ifelse(Votes == "0" & Party != "Other",
                               NA,
                               Percentage))

  kable <- kbl(table,
               format = "html",
               booktabs = TRUE,
               caption = str_c("Votes in MN House District ",
                               district,
                               " in 2022"),
               align = "l") %>%
    kable_styling() %>%
    row_spec(4, bold = TRUE) # bold total votes row
  
  # customizing table if no candidate fielded by one party
  if(sum(table$Candidate == "no candidate", na.rm = TRUE) == 1) {
    kable <- kable %>%
      row_spec(3, italic = TRUE, font_size = 13)
  }
  
  return(kable)
}


# function for house leaflet
house_map.fcn <- function(district) {
  if(as.numeric(str_sub(district, 1, -2)) %notin% 1:67 | str_sub(district, -1, -1) %notin% c("A", "B")) {
    stop("That's not a House district, silly!")
  } else {
    district_data <- legislative_precincts %>%
      filter(mnlegdist == district)
    
    # initializing leaflet
    district_leaflet <- district_data %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron")
  }
  
  # add any DFL precincts
  if(sum(district_data$house_winner == "DFL") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                house_winner == "DFL"),
                  label = ~map(house_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Blues",
                                            domain = 0:(max(abs_house_margin) + 10))(abs_house_margin),
                  fillOpacity = 3)
  }
  
  # add any Republican precincts
  if(sum(district_data$house_winner == "Republican") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                house_winner == "Republican"),
                  label = ~map(house_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Reds",
                                            domain = 0:(max(abs_house_margin) + 10))(abs_house_margin),
                  fillOpacity = 3)
  }
  
  # add any tied precincts
  if(sum(district_data$house_winner == "Tie") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                house_winner == "Tie"),
                  label = ~map(house_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = "#CCCCCC",
                  fillOpacity = 3)
  }
  return(district_leaflet)
}

# adding city search feature in house and senate tabs

### MODIFY FILE LOCATION IF NEEDED
# reading in cities (and potential future code for zipcode lookup)

# house_zipcodes <- read_csv("Municipality Files/housezip.csv") %>%
#   mutate(district = ifelse(str_detect(district, "^0") == TRUE,
#                            str_sub(district, 2, -1),
#                            district)) %>%
#   select(zip, district)

house_towns <- read_csv("Municipality Files/housemcd.csv") %>%
  # remove townships
  filter(str_detect(mcdname, "City$") == TRUE) %>%
  # clean up district spelling
  mutate(district = ifelse(str_detect(district, "^0") == TRUE,
                           str_sub(district, 2, -1),
                           district),
         mcdname = str_remove(mcdname, " City$")) %>%
  select(mcdname, district) %>%
  distinct() # remove duplicate rows

# adding "St. Paul" to table since that's a major city and a common spelling
house_stpaul <- house_towns %>%
  filter(mcdname == "Saint Paul") %>%
  mutate(mcdname = "St. Paul")

house_towns <- rbind(house_towns,
                     house_stpaul)

senate_towns <- read_csv("Municipality Files/senatemcd.csv") %>%
  # remove townships
  filter(str_detect(mcdname, "City$") == TRUE) %>%
  # clean up district spelling
  mutate(district = ifelse(str_detect(district, "^0") == TRUE,
                           str_sub(district, 2, -1),
                           district),
         mcdname = str_remove(mcdname, " City$")) %>%
  select(mcdname, district) %>%
  distinct() # remove duplicate rows

# adding "St. Paul" to table since that's a major city and a common spelling
senate_stpaul <- senate_towns %>%
  filter(mcdname == "Saint Paul") %>%
  mutate(mcdname = "St. Paul")

senate_towns <- rbind(senate_towns,
                     senate_stpaul)

congress_towns <- read_csv("Municipality Files/congressmcd.csv") %>%
  # remove townships
  filter(str_detect(mcd, "City$") == TRUE) %>%
  mutate(mcd = str_remove(mcd, " City$")) %>%
  select(mcd, district) %>%
  distinct() # remove duplicate rows

# adding "St. Paul" to table since that's a major city and a common spelling
congress_stpaul <- congress_towns %>%
  filter(mcd == "Saint Paul") %>%
  mutate(mcd = "St. Paul")

congress_towns <- rbind(congress_towns,
                        congress_stpaul)


# find districts given a city name
house_match.fcn <- function(city) {
  
  # if input is empty, return empty (for app initialization)
  if(city == "") {
    return("")
  }
  
  # find city and grab districts
  districts <- house_towns %>%
    filter(mcdname == city) %>%
    .$district
  
  if(length(districts) == 0) { # for non-city names
    string <- str_c("Uh-oh -- I don't have a record of ",
                    city,
                    " being a city in Minnesota.")
  } else if(length(districts) == 1) { # for cities in one district
    string <- str_c("The city of ",
                    city,
                    " is in district ",
                    districts,
                    ".")
  } else if (length(districts) == 2) { # for cities in two districts
    string <- str_c("The city of ",
                    city,
                    " is in districts ",
                    districts[1],
                    " and ",
                    districts[2],
                    ".")
  } else { # for cities in 3+ districts
    n_matches <- length(districts)
    string <- str_c("The city of ",
                    city,
                    " is in districts ",
                    str_c(districts[1:n_matches-1], collapse = ", "),
                    ", and ",
                    districts[n_matches],
                    ".")
  }
  
  return(string)
}


senate_match.fcn <- function(city) {
  
  # if input is empty, return empty (for app initialization)
  if(city == "") {
    return("")
  }
  
  # find city and grab districts
  districts <- senate_towns %>%
    filter(mcdname == city) %>%
    .$district
  
  if(length(districts) == 0) { # for non-city names
    string <- str_c("Uh-oh -- I don't have a record of ",
                    city,
                    " being a city in Minnesota.")
  } else if(length(districts) == 1) { # for cities in one district
    string <- str_c("The city of ",
                    city,
                    " is in district ",
                    districts,
                    ".")
  } else if (length(districts) == 2) { # for cities in two districts
    string <- str_c("The city of ",
                    city,
                    " is in districts ",
                    districts[1],
                    " and ",
                    districts[2],
                    ".")
  } else { # for cities in 3+ districts
    n_matches <- length(districts)
    string <- str_c("The city of ",
                    city,
                    " is in districts ",
                    str_c(districts[1:n_matches-1], collapse = ", "),
                    ", and ",
                    districts[n_matches],
                    ".")
  }
  
  return(string)
}

congress_match.fcn <- function(city) {
  
  # if input is empty, return empty (for app initialization)
  if(city == "") {
    return("")
  }
  
  # find city and grab districts
  districts <- congress_towns %>%
    filter(mcd == city) %>%
    .$district
  
  if(length(districts) == 0) { # for non-city names
    string <- str_c("Uh-oh -- I don't have a record of ",
                    city,
                    " being a city in Minnesota.")
  } else if(length(districts) == 1) { # for cities in one district
    string <- str_c("The city of ",
                    city,
                    " is in district ",
                    districts,
                    ".")
  } else if (length(districts) == 2) { # for cities in two districts
    string <- str_c("The city of ",
                    city,
                    " is in districts ",
                    districts[1],
                    " and ",
                    districts[2],
                    ".")
  } else { # for cities in 3+ districts
    n_matches <- length(districts)
    string <- str_c("The city of ",
                    city,
                    " is in districts ",
                    str_c(districts[1:n_matches-1], collapse = ", "),
                    ", and ",
                    districts[n_matches],
                    ".")
  }
  
  return(string)
}

## work for congress tab

congress_full_table <- precincts %>%
  group_by(congdist) %>%
  summarize(r_votes = sum(usrepr),
            dfl_votes = sum(usrepdfl),
            total_votes = sum(usreptotal),
            # other_votes = total_votes - (r_votes + dfl_votes),
            lmn_votes = sum(usreplmn),
            glc_votes = sum(usrepglc),
            writein_votes = sum(usrepwi),
            winner = ifelse(dfl_votes > r_votes,
                            "DFL",
                            "Republican"),
            r_pct = 100 * r_votes / total_votes,
            dfl_pct = 100 * dfl_votes / total_votes,
            lmn_pct = 100 * lmn_votes / total_votes,
            glc_pct = 100 * glc_votes / total_votes,
            writein_pct = 100 * writein_votes / total_votes) %>%
  mutate(r_pct = round(r_pct, digits = 2),
         dfl_pct = round(dfl_pct, digits = 2),
         lmn_pct = round(lmn_pct, digits = 2),
         glc_pct = round(glc_pct, digits = 2),
         writein_pct = round(writein_pct, digits = 2)) %>%
  select(congdist, winner,
         r_votes, r_pct,
         dfl_votes, dfl_pct,
         lmn_votes, lmn_pct,
         glc_votes, glc_pct,
         writein_votes, writein_pct,
         total_votes) %>%
  arrange(as.numeric(congdist))

# candidate vectors
congress_dfl_candidates <- c("Jeff Ettinger",
                             "Angie Craig (i)",
                             "Dean Phillips (i)",
                             "Betty McCollum (i)",
                             "Ilhan Omar (i)",
                             "Jeanne Hendricks",
                             "Jill Abahsain",
                             "Jennifer Schultz")
congress_r_candidates <- c("Brad Finstad (i)",
                           "Tyler Kistner",
                           "Tom Weiler",
                           "May Lor Xiong",
                           "Cicely Davis",
                           "Tom Emmer (i)",
                           "Michelle Fischbach (i)",
                           "Pete Stauber (i)")
congress_lmn_candidates <- c("Richard Reisdorf",
                             "Paula Overby",
                             "",
                             "",
                             "",
                             "",
                             "Travis Johnson",
                             "")
congress_glc_candidates <- c("Brian Abrahamson",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "")

# adding candidate names to dataset
congress_full_table <- cbind(congress_full_table,
                           congress_dfl_candidates,
                           congress_r_candidates,
                           congress_lmn_candidates,
                           congress_glc_candidates) %>%
  as_tibble() %>%
  rename(congress_dfl_candidate = congress_dfl_candidates,
         congress_r_candidate = congress_r_candidates,
         congress_lmn_candidate = congress_lmn_candidates,
         congress_glc_candidate = congress_glc_candidates)

# function for making a congressional election table!
congress_table.fcn <- function(district) {
  table <- tibble(Party = c("Republican",
                            "DFL",
                            "Legal Marijuana Now",
                            "Grassroots - Legalize Cannabis",
                            "Write-in",
                            "Total"),
                  Candidate = c(congress_full_table$congress_r_candidate[district],
                                congress_full_table$congress_dfl_candidate[district],
                                congress_full_table$congress_lmn_candidate[district],
                                congress_full_table$congress_glc_candidate[district],
                                NA,
                                NA),
                  Votes = c(congress_full_table$r_votes[district],
                            congress_full_table$dfl_votes[district],
                            congress_full_table$lmn_votes[district],
                            congress_full_table$glc_votes[district],
                            congress_full_table$writein_votes[district],
                            congress_full_table$total_votes[district]),
                  Percentage = c(congress_full_table$r_pct[district],
                                 congress_full_table$dfl_pct[district],
                                 congress_full_table$lmn_pct[district],
                                 congress_full_table$glc_pct[district],
                                 congress_full_table$writein_pct[district],
                                 NA)) %>%
    arrange(desc(Percentage)) %>%
    mutate(Votes = formatC(Votes, # formatting large numbers
                           format = "d",
                           big.mark = ","),
           Percentage = str_c(as.character(Percentage),
                              "%")) %>%
    filter(Votes != 0) # removing empty rows (no LMN or GLC candidate)
  
  kable <- kbl(table,
               format = "html",
               booktabs = TRUE,
               caption = str_c("Votes in Congressional District ",
                               district,
                               " in 2022"),
               align = "l") %>%
    kable_styling() %>%
    row_spec(nrow(table), bold = TRUE) # bolding total votes row
  
  return(kable)
}


# function for congressional district leaflet
congress_map.fcn <- function(district) {
  if(district %notin% 1:8) {
    stop("There are only 8 congressional districts in Minnesota!")
  } else {
    district_data <- legislative_precincts %>%
      filter(congdist == district)
    
    # initializing leaflet
    district_leaflet <- district_data %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron")
  }
  
  # add any DFL precincts
  if(sum(district_data$congress_winner == "DFL") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                congress_winner == "DFL"),
                  label = ~map(congress_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Blues",
                                            domain = 0:(max(abs_congress_margin) + 10))(abs_congress_margin),
                  fillOpacity = 3)
  }
  
  # add any Republican precincts
  if(sum(district_data$congress_winner == "Republican") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                congress_winner == "Republican"),
                  label = ~map(congress_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = ~colorNumeric("Reds",
                                            domain = 0:(max(abs_congress_margin) + 10))(abs_congress_margin),
                  fillOpacity = 3)
  }
  
  # add any tied precincts
  if(sum(district_data$congress_winner == "Tie") > 0) {
    district_leaflet <- district_leaflet %>%
      addPolygons(data = filter(district_data,
                                congress_winner == "Tie"),
                  label = ~map(congress_label, HTML),
                  color = "black",
                  weight = 2,
                  fillColor = "#CCCCCC",
                  fillOpacity = 3)
  }
  return(district_leaflet)
}
