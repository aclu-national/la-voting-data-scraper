# ----------------------- Loading libraries and data --------------------------
# Loading Libraries
library(tidyverse)
library(ggplot2)
library(showtext)

# Loading data
house_act_1_analysis <- read_csv("analyzed_voting_data/house_act_1_analysis.csv")
house_act_2_analysis <- read_csv("analyzed_voting_data/house_act_2_analysis.csv")
house_act_3_analysis <- read_csv("analyzed_voting_data/house_act_3_analysis.csv")
house_act_4_analysis <- read_csv("analyzed_voting_data/house_act_4_analysis.csv")

senate_act_1_rough_analysis <- read_csv("analyzed_voting_data/senate_act_1_rough_analysis.csv")
senate_act_2_rough_analysis <- read_csv("analyzed_voting_data/senate_act_2_rough_analysis.csv")
senate_act_3_rough_analysis <- read_csv("analyzed_voting_data/senate_act_3_rough_analysis.csv")
senate_act_4_rough_analysis <- read_csv("analyzed_voting_data/senate_act_4_rough_analysis.csv")

# ------------------ Creating plot directory and visualizer --------------------
# Creating a "plots" directory
dir.create("plots")

# Defining a plot creation function
plot_pct_no_analysis <- function(chamber = "house", act = 1) {
  
  # Create based on chamber and act
  if (chamber == "senate") {
    dataset_name <- paste0("senate_act_", act, "_rough_analysis")
  } else {
    dataset_name <- paste0(chamber, "_act_", act, "_analysis")
  }
  
  # Getting data
  data <- get(dataset_name)
  
  # Counting % over 50%
  pct_over_50 <- data %>% 
    filter(pct_no > 50) %>% 
    nrow() / nrow(data) * 100
  
  # Creating the plot
  p <- data %>%
    mutate(fill_color = ifelse(pct_no > 50, "#0055AA", "#A3DBE3")) %>% 
    ggplot(aes(x = reorder(district, -pct_no), y = pct_no, fill = fill_color)) +
    geom_bar(stat = "identity", color = "transparent", size = 0.5) + 
    theme(axis.line = element_line(colour = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) + 
    theme(
      axis.text.x = element_text(size = 7, angle = 90, hjust = 1),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      legend.position = "none", 
      plot.margin = margin(10, 10, 10, 10)
    ) +
    scale_fill_identity() +
    labs(
      title = paste("Percent No on Act", act),
      subtitle = paste0(round(pct_over_50, 2), "% of districts had over 50% of people vote no."),
      x = "District",
      y = "Percent Voting No"
    )
  
  # Saving as JPG
  ggsave(paste0("plots/plot_pct_no_analysis_", chamber, "_act_", act, ".jpg"), plot = p, width = 10, height = 6, dpi = 300)
}
# ---------------------------- Running Visualizer -----------------------------

# Running the function
plot_pct_no_analysis("house", 1)
plot_pct_no_analysis("house", 2)
plot_pct_no_analysis("house", 3)
plot_pct_no_analysis("house", 4)

plot_pct_no_analysis("senate", 1)
plot_pct_no_analysis("senate", 2)
plot_pct_no_analysis("senate", 3)
plot_pct_no_analysis("senate", 4)
