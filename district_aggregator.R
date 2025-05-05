# ------------ Loading libraries, voting data, and defining functions ----------
# Loading Libraries
library(tidyverse)
library(janitor)

# Reading voting data
act_1_data <- read_csv("voting_data/act_1_votes.csv") %>%
  filter(precinct != "Early Voting")
act_2_data <- read_csv("voting_data/act_2_votes.csv") %>%
  filter(precinct != "Early Voting")
act_3_data <- read_csv("voting_data/act_3_votes.csv") %>%
  filter(precinct != "Early Voting")
act_4_data <- read_csv("voting_data/act_4_votes.csv") %>%
  filter(precinct != "Early Voting")

# Defining a function to take a base dataframe and a voting dataframe and combine them
district_x_voting <- function(base_df, voting_df) {
  df1 <- base_df %>%
    filter(precinct != "*ALL/*ALL") %>%
    left_join(
      voting_df %>% filter(precinct != "Early Voting"),
      by = c("parish","precinct")
    )
  
  df2 <- base_df %>%
    filter(precinct == "*ALL/*ALL") %>%
    select(-precinct) %>%
    left_join(
      voting_df %>% filter(precinct != "Early Voting"),
      by = c("parish")
    )
  
  house_final <- bind_rows(df1,df2)
  
  return(house_final)
}

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

# -------------------------- Loading and cleaning data -------------------------

# House Dataframe
house_df <- read_csv("precinct_data_1.csv") %>%
  clean_names() %>%
  
  # Removing unnecessary data
  filter(!str_detect(tolower(louisiana_secretary_of_state), 
                     "precinct count|total parishes|total precincts|production|office jurisdiction report|for election")) %>%
  
  # Adding a district Column
  mutate(
    district = ifelse(
      str_detect(louisiana_secretary_of_state, "State Representative"),
      louisiana_secretary_of_state,
      NA)
    ) %>%
  
  # Filling in missing districts downwards
  fill(district) %>%
  
  # Removing the districts from the "louisiana_secretary_of_state" column
  filter(!str_detect(louisiana_secretary_of_state, "State Representative")) %>%
  
  # Adding a parish Column
  mutate(
    parish = ifelse(
      is.na(as.numeric(substring(louisiana_secretary_of_state,1,1))),
      louisiana_secretary_of_state,
      NA)
    ) %>%
  
  # Filling in missing parishes downwards
  fill(parish) %>%
  
  # Removing the parishes from the "louisiana_secretary_of_state" column
  filter(!is.na(as.numeric(substring(louisiana_secretary_of_state,1,1)))) %>%
  
  # Renaming "louisiana_secretary_of_state" as "precinct"
  select(precinct = louisiana_secretary_of_state, district, parish) %>%
  
  # Formatting the columns correctly
  mutate(district = parse_number(district),
         precinct = substr(precinct, 4, nchar(precinct)),
         precinct = str_replace(precinct, " ", "/"),
         parish = str_to_title(parish)
  )

# Senate Dataframe
senate_df <- read_csv("precinct_data_2.csv") %>%
  clean_names() %>%
  
  # Removing unnecessary data
  filter(!str_detect(tolower(louisiana_secretary_of_state), 
                     "precinct count|total parishes|total precincts|production|office jurisdiction report|for election")) %>%
  
  # Adding a district Column
  mutate(
    district = ifelse(
      str_detect(louisiana_secretary_of_state, "State Senator"),
      louisiana_secretary_of_state,
      NA)
  ) %>%
  
  # Filling in missing districts downwards
  fill(district) %>%
  
  # Removing the districts from the "louisiana_secretary_of_state" column
  filter(!str_detect(louisiana_secretary_of_state, "State Senator")) %>%
  
  # Adding a parish Column
  mutate(
    parish = ifelse(
      is.na(as.numeric(substring(louisiana_secretary_of_state,1,1))),
      louisiana_secretary_of_state,
      NA)
  ) %>%
  
  # Filling in missing parishes downwards
  fill(parish) %>%
  
  # Removing the parishes from the "louisiana_secretary_of_state" column
  filter(!is.na(as.numeric(substring(louisiana_secretary_of_state,1,1)))) %>%
  
  # Renaming "louisiana_secretary_of_state" as "precinct"
  select(precinct = louisiana_secretary_of_state, district, parish) %>%
  
  # Formatting the columns correctly
  mutate(district = parse_number(district),
         precinct = substr(precinct, 4, nchar(precinct)),
         precinct = str_replace(precinct, " ", "/"),
         parish = str_to_title(parish)
  ) %>%
  filter(precinct != "")

# -------------------- Merging Districts with voting data ----------------------
# House voting data
house_act_1 = house_district_x_voting(house_df, act_1_data)
house_act_2 = house_district_x_voting(house_df, act_2_data)
house_act_3 = house_district_x_voting(house_df, act_3_data)
house_act_4 = house_district_x_voting(house_df, act_4_data)

# Senate voting data
senate_act_1 = house_district_x_voting(senate_df, act_1_data)
senate_act_2 = house_district_x_voting(senate_df, act_2_data)
senate_act_3 = house_district_x_voting(senate_df, act_3_data)
senate_act_4 = house_district_x_voting(senate_df, act_4_data)

# ------------------------- Checking merge accuracy ----------------------------
# Precincts not shared between datasets
setdiff(house_act_1$precinct,act_1_data$precinct)
setdiff(senate_act_1$precinct,act_1_data$precinct)

length(house_act_1$precinct) == length(act_1_data$precinct)
length(senate_act_1$precinct) == length(act_1_data$precinct)

# Number of precincts per parish
senate_df %>%
  group_by(precinct,parish) %>%
  summarize(count = n()) %>%
  arrange(-count)

house_df %>%
  group_by(precinct,parish) %>%
  summarize(count = n()) %>%
  arrange(-count)

# ---------------------------- Analyzing data ---------------------------------

# House analysis
house_act_1_analysis <- house_act_1 %>%
  get_analysis()
house_act_2_analysis <- house_act_2 %>%
  get_analysis()
house_act_3_analysis <- house_act_3 %>%
  get_analysis()
house_act_4_analysis <- house_act_4 %>%
  get_analysis()

# Senate analysis
senate_act_1_analysis <- senate_act_1 %>%
  get_analysis()
senate_act_2_analysis <- senate_act_2 %>%
  get_analysis()
senate_act_3_analysis <- senate_act_3 %>%
  get_analysis()
senate_act_4_analysis <- senate_act_4 %>%
  get_analysis()

# Creating a "analyzed_voting_data" directory 
dir.create("analyzed_voting_data")

# Writing analysis as CSV
write_csv(house_act_1_analysis, "analyzed_voting_data/house_act_1_analysis.csv")
write_csv(house_act_2_analysis, "analyzed_voting_data/house_act_2_analysis.csv")
write_csv(house_act_3_analysis, "analyzed_voting_data/house_act_3_analysis.csv")
write_csv(house_act_4_analysis, "analyzed_voting_data/house_act_4_analysis.csv")

write_csv(senate_act_1_analysis, "analyzed_voting_data/senate_act_1_analysis.csv")
write_csv(senate_act_2_analysis, "analyzed_voting_data/senate_act_2_analysis.csv")
write_csv(senate_act_3_analysis, "analyzed_voting_data/senate_act_3_analysis.csv")
write_csv(senate_act_4_analysis, "analyzed_voting_data/senate_act_4_analysis.csv")
