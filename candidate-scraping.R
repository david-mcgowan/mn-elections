library(tidyverse)

districtize <- function(df) {
  new_df <- df %>%
    mutate(District = str_c(ceiling(row_number() / 2),
                            ifelse(row_number() %% 2 == 1,
                                   "A",
                                   "B"))) %>%
    select(District, everything())
  
  return(new_df)
}

# 2016----------------------------

html_2016 <- read_html("https://ballotpedia.org/Minnesota_House_of_Representatives_elections,_2016")
dfl_nodes <- html_nodes(html_2016, "td:nth-child(2)")
dfl_candidates <- html_text(dfl_nodes)

dfl_candidates <- dfl_candidates[22:155]

dfl_tibble <- tibble(Candidate = str_extract(dfl_candidates, "^[^:]*"),
                     Incumbency = str_detect(dfl_candidates, "(I)")) %>%
  mutate(Candidate = ifelse(str_detect(Candidate, " a$"), # fix races with no opposition
                            str_sub(Candidate, 1, -7),
                            Candidate),
         Candidate = ifelse(Incumbency == TRUE, # add incumbency marker
                            str_c(Candidate, " (i)"),
                            Candidate),
         Candidate = ifelse(Candidate == "No candidate", # turn lack of nominee into NA
                            NA,
                            Candidate),
         District = str_c(ceiling(row_number() / 2),
                          ifelse(row_number() %% 2 == 1,
                                 "A",
                                 "B")))

r_nodes <- html_nodes(html_2016, "td:nth-child(3)")
r_candidates <- html_text(r_nodes)

r_candidates <- r_candidates[20:153]

r_tibble <- tibble(Candidate = str_extract(r_candidates, "^[^:]*"),
                   Incumbency = str_detect(r_candidates, "(I)")) %>%
  mutate(Candidate = ifelse(str_detect(Candidate, " a$"), # fix races with no opposition
                            str_sub(Candidate, 1, -7),
                            Candidate),
         Candidate = ifelse(Incumbency == TRUE, # add incumbency marker
                            str_c(Candidate, " (i)"),
                            Candidate),
         Candidate = ifelse(Candidate == "No candidate", # turn lack of nominee into NA
                            NA,
                            Candidate),
         District = str_c(ceiling(row_number() / 2),
                          ifelse(row_number() %% 2 == 1,
                                 "A",
                                 "B")))

bind_rows(dfl_tibble, r_tibble) %>%
  dplyr::select(District, Candidate) %>%
  write_csv("2016-candidates.csv")

# 2018--------------------------------

html_2018 <- read_html("https://ballotpedia.org/Minnesota_House_of_Representatives_elections,_2018")
dfl_nodes <- html_nodes(html_2018, "td:nth-child(2)")
dfl_candidates <- html_text(dfl_nodes)

dfl_candidates <- dfl_candidates[4:137] %>%
  str_trim()

dfl_tibble <- tibble(Candidate = dfl_candidates) %>%
  districtize()

r_nodes <- html_nodes(html_2018, "td:nth-child(3)")
r_candidates <- html_text(r_nodes)

r_candidates <- r_candidates[2:135] %>%
  str_trim()

r_tibble <- tibble(Candidate = r_candidates) %>%
  districtize()

bind_rows(dfl_tibble, r_tibble) %>%
  write_csv("2018-candidates.csv")

# 2020-----------------------------

html_2020 <- read_html("https://ballotpedia.org/Minnesota_House_of_Representatives_elections,_2020")
dfl_nodes <- html_nodes(html_2020, "td:nth-child(2)")
dfl_candidates <- html_text(dfl_nodes)

dfl_candidates <- dfl_candidates[8:141] %>%
  str_trim()

dfl_tibble <- tibble(Candidate = dfl_candidates) %>%
  districtize()

r_nodes <- html_nodes(html_2020, "td:nth-child(3)")
r_candidates <- html_text(r_nodes)

r_candidates <- r_candidates[5:138] %>%
  str_trim()

r_tibble <- tibble(Candidate = r_candidates) %>%
  districtize()

bind_rows(dfl_tibble, r_tibble) %>%
  write_csv("2020-candidates.csv")

# 2022 (previous work)---------------------

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