library("dplyr")
library("ggplot2")
library(stringr)
library(plotly)

austen_df <- read.csv('~/Desktop/INFO-201A/Checkouts_by_Austen.csv', stringsAsFactors = FALSE)

austen_df$Title <- tolower(austen_df$Title)

# Filter for all 6 novels 
austen_novels_df <- austen_df %>% 
  filter(str_detect(Title, "sense and sensibility|pride and prejudice|mansfield park|emma|persuasion|northanger abbey") 
         & str_detect(Title, "zombies|sea monsters|drama", negate = TRUE))

# Add duplicate title column (new column Name)
austen_novels2_df <- austen_novels_df %>% 
  mutate(Name = Title)

# Replace with simple titles
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "sense and sensibility")] <- "Sense and Sensibility"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "pride and prejudice")] <- "Pride and Prejudice"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "mansfield park")] <- "Mansfield Park"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "emma")] <- "Emma"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "persuasion")] <- "Persuasion"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "northanger abbey")] <- "Northanger Abbey"

# How many times was each novel checked out? 
austen_novel_groups_df <- austen_novels2_df %>% 
  group_by(Name) %>% 
  summarize(total_checkouts = sum(Checkouts))

# Bar chart
all_novels <- ggplot(austen_novel_groups_df) +
  geom_col(aes(y = reorder(Name, +total_checkouts), x = total_checkouts, fill = Name)) + 
  labs(title = "Total Checkouts by Jane Austen Novels from 2005 - 2023",
       y = "Novel Title",
       x = "Total Checkouts",
       fill = "Title")

ggplotly(all_novels)
