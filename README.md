# Louisiana Voting Data Scraper

## Summary 
This project was created to download precinct-level voting data from the March 28, 2025 Louisiana election, convert those precincts into Senate and House districts, and analyze the proportion of people who voted against Act 1, Act 2, Act 3, and Act 4 in those districts. 

### voting_data_scraper.R
This script downloads all voting data by precinct and turns them into cohesive sets of CSV's for each act. 

### district_aggregator.R
This script uses the precincts within each district to calculate the percentage of no-votes per district.
