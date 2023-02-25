library("dplyr")
library("ggplot2")
library(stringr)

austen_df <- read.csv('~/Desktop/INFO-201A/Checkouts_by_Austen.csv', stringsAsFactors = FALSE)

austen_df$Title <- tolower(austen_df$Title)

# Filter for all 6 novels 
austen_novels_df <- austen_df %>% 
  filter(str_detect(Title, "sense and sensibility|pride and prejudice|mansfield park|emma|persuasion|Northanger Abbey") 
         & str_detect(Title, "zombies|sea monsters|drama", negate = TRUE))

# Add duplicate title column (new column Name)
austen_novels2_df <- austen_novels_df %>% 
  mutate(Name = Title)

# Replace 
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "sense and sensibility")] <- "Sense and Sensibility"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "pride and prejudice")] <- "Pride and Prejudice"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "mansfield park")] <- "Mansfield Park"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "emma")] <- "Emma"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "persuasion")] <- "Persuasion"
austen_novels2_df$Name[str_detect(austen_novels2_df$Name, "northanger abbey")] <- "Northanger Abbey"

# Chart 1: How many times were each novel checked out?
# Group by book
austen_novel_groups_df <- austen_novels2_df %>% 
  group_by(Name) %>% 
  summarize(total_checkouts = sum(Checkouts))

chart1 <- ggplot(austen_novel_groups_df) +
  geom_col(aes(x = Name, y = total_checkouts, fill = Name)) + 
  labs(title = "Total Checkouts by Jane Austen Novels from 2005 - 2023",
       x = "Novel Title",
       y = "Total Checkouts",
       fill = "Title")
