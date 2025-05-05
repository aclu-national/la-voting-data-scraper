#--------------------------------------------------------------------------
# Author: Elijah Appelson
# Last Updated: May 5th, 2025
# Summary: This script is used to convert the "districts.pdf" file into a
# CSV with the necessary variables.
#--------------------------------------------------------------------------

# Loading Libraries
library(pdftools)
library(tidyverse)

# Path to file
file <- "public_records/districts.pdf"

# Reading file as pdf
text <- pdf_text(file)

# Combining all pages into a single string
full_text <- paste(text, collapse = "\n")

# Splitting the file by line
lines <- strsplit(full_text, "\n")[[1]]

# Removing white spaces
lines <- trimws(lines)

# Splitting the lines by spaces
data_list <- lines %>%
  map(~str_split(.x, "\\s{2,}", simplify = TRUE)) %>%
  map(~.x[!is.na(.x)])

# Combining the lists of lines into a single dataframe
df <- bind_rows(lapply(data_list, function(x) as_tibble(t(x))))

# Renaming the columns
colnames(df) <- c("Parish", "Ward", "Precinct", "Poll Place Name", "Poll Place Location", "Office", "District", "City")

# Creating a final dataframe
df_final <- df %>%
  select("Parish", "Ward", "Precinct", "Poll Place Name", "Poll Place Location", "Office", "District", "City") %>%
  filter(Parish != "Parish",
         Parish != "") %>%
  mutate(Parish = str_to_title(str_sub(Parish,1,-4))) %>%
  clean_names()

write_csv(df_final, "public_records/extracted_data/districts.csv")
