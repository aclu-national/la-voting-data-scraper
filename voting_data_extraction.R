#--------------------------------------------------------------------------
# This script is used to download election data from the March 28th, 2025
# election in Louisiana for four constitutional amendments at a precinct
# level. We use this information to determine voting at district levels.
#--------------------------------------------------------------------------

# ---------------------- Loading Libraries + Data -------------------------
# Loading Libraries
library(jsonlite)
library(tidyverse)
library(httr)
library(janitor)

# Downloading districts/precincts
districts <- read_csv("district_precincts - 4th.csv") %>%
  mutate(Precinct = substr(Precinct, 4, nchar(Precinct)),
         Precinct = str_replace(Precinct, " ", "/"),
         Parish = str_to_title(Parish)
         ) %>%
  clean_names()

# Defining Parishes from A -> Z
parishes <- c("Acadia", "Allen", "Ascension", "Assumption", "Avoyelles", 
              "Beauregard", "Bienville", "Bossier", "Caddo", "Calcasieu", 
              "Caldwell", "Cameron", "Catahoula", "Claiborne", "Concordia", 
              "De Soto", "East Baton Rouge", "East Carroll", "East Feliciana", 
              "Evangeline", "Franklin", "Grant", "Iberia", "Iberville", 
              "Jackson", "Jefferson", "Jefferson Davis", "Lafayette", 
              "Lafourche", "Lasalle", "Lincoln", "Livingston", "Madison", 
              "Morehouse", "Natchitoches", "Orleans", "Ouachita", 
              "Plaquemines", "Pointe Coupee", "Rapides", "Red River", 
              "Richland", "Sabine", "St. Bernard", "St. Charles", 
              "St. Helena", "St. James", "St. John The Baptist", "St. Landry", 
              "St. Martin", "St. Mary", "St. Tammany", "Tangipahoa", 
              "Tensas", "Terrebonne", "Union", "Vermilion", "Vernon", 
              "Washington", "Webster", "West Baton Rouge", "West Carroll", 
              "West Feliciana", "Winn")

# ------------------------- Defining Functions -------------------------

# Defining get_analysis function
get_analysis <- function(df){
  df %>%
    mutate(n = yes+no) %>%
    group_by(district) %>%
    summarize(
      n_yes = sum(yes),
      n_no = sum(no),
      n_sum = sum(n),
      pct_yes = round(100*n_yes/n_sum,2),
      pct_no = round(100*n_no/n_sum,2)
    )
}

# Defining the get_precinct_data function
get_precinct_data <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    json_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(json_data)
  } else {
    warning("Failed for ", url)
    return(NULL)
  }
}

# Defining the get_dataframe function
get_dataframe <- function(precinct, act_data) {
  parish <- parishes[[precinct]] # Getting the parish 
  data <- act_data[[precinct]] # Gathering the data
  
  # Creating a dataframe for important variables
  table <- data$Precincts$Precinct %>%
    select(precinct = Precinct, 
           qualified_voters = VoterCountQualified, 
           count_voted = VoterCountVoted, 
           reported = HasReported)
  
  # Creating the votes dataframe
  precinct_votes <- data$Precincts$Precinct$Choice
  
  votes <- precinct_votes %>%
    bind_rows(.id = "precinct") %>%
    mutate(vote = case_when(
      ID == "3" ~ "yes",
      ID == "4" ~ "no",
      TRUE ~ NA_character_
    )) %>%
    mutate(VoteTotal = as.numeric(VoteTotal),
           precinct = as.numeric(precinct)) %>%
    group_by(precinct, vote) %>%
    summarise(count = sum(VoteTotal, na.rm = TRUE)) %>%
    pivot_wider(names_from = vote, values_from = count, values_fill = list(count = 0)) %>%
    ungroup() %>%
    arrange(precinct) %>%
    select(-precinct)
  
  # Binding the dataframes into one
  return(cbind(table, votes) %>%
           mutate(parish))
}

# --------------------------- Loading Data ------------------------------

# Creating links to pull from
n <- sprintf("%02d", 01:64)
end_link <- ".htm"

act_1_base_link <- "https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob=20250329/VotesRaceByPrecinct/Votes_13280_"
act_2_base_link <- "https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob=20250329/VotesRaceByPrecinct/Votes_13281_"
act_3_base_link <- "https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob=20250329/VotesRaceByPrecinct/Votes_13282_"
act_4_base_link <- "https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob=20250329/VotesRaceByPrecinct/Votes_13283_"

act_1_links <- paste0(act_1_base_link,n,end_link)
act_2_links <- paste0(act_2_base_link,n,end_link)
act_3_links <- paste0(act_3_base_link,n,end_link)
act_4_links <- paste0(act_4_base_link,n,end_link)

# Pulling datafrom links
act_1_precinct_data <- lapply(act_1_links, get_precinct_data)
act_2_precinct_data <- lapply(act_2_links, get_precinct_data)
act_3_precinct_data <- lapply(act_3_links, get_precinct_data)
act_4_precinct_data <- lapply(act_4_links, get_precinct_data)

# Downloading JSON's
write_json(act_1_precinct_data, "act_1_precinct_data.json")
write_json(act_2_precinct_data, "act_2_precinct_data.json")
write_json(act_3_precinct_data, "act_3_precinct_data.json")
write_json(act_4_precinct_data, "act_4_precinct_data.json")

# --------------------- Cleaning Data into Dataframes ---------------------

# Turning the data into lists of dataframes
act_1_df_list <- lapply(1:64, function(i) get_dataframe(i, act_1_precinct_data))
act_2_df_list <- lapply(1:64, function(i) get_dataframe(i, act_2_precinct_data))
act_3_df_list <- lapply(1:64, function(i) get_dataframe(i, act_3_precinct_data))
act_4_df_list <- lapply(1:64, function(i) get_dataframe(i, act_4_precinct_data))

# Binding each list of dataframes
act_1_df <- bind_rows(act_1_df_list)
act_2_df <- bind_rows(act_2_df_list)
act_3_df <- bind_rows(act_3_df_list)
act_4_df <- bind_rows(act_4_df_list)

# Defining districts dataframes
districts_act_1_df <- districts %>%
  left_join(
    act_1_df,
    by = c("parish","precinct")
  )

districts_act_2_df <- districts %>%
  left_join(
    act_2_df,
    by = c("parish","precinct")
  )

districts_act_3_df <- districts %>%
  left_join(
    act_3_df,
    by = c("parish","precinct")
  )

districts_act_4_df <- districts %>%
  left_join(
    act_4_df,
    by = c("parish","precinct")
  )

# Defining districts analyses
act_1_analysis <- districts_act_1_df %>%
  get_analysis()

act_2_analysis <- districts_act_2_df %>%
  get_analysis()

act_3_analysis <- districts_act_3_df %>%
  get_analysis()

act_4_analysis <- districts_act_4_df %>%
  get_analysis()

# ------------------------ Downloading Data -------------------------

# Downloading vote data
write_csv(act_1_df, "act_1_votes.csv")
write_csv(act_2_df, "act_2_votes.csv")
write_csv(act_3_df, "act_3_votes.csv")
write_csv(act_4_df, "act_4_votes.csv")

# Downloading district analysis data
write_csv(act_1_analysis, "act_1_analysis.csv")
write_csv(act_2_analysis, "act_2_analysis.csv")
write_csv(act_3_analysis, "act_3_analysis.csv")
write_csv(act_4_analysis, "act_3_analysis.csv")