library(stringr)
library("dplyr")
library("ggplot2")
library("plotly")

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

# Add Date Column
austen_novels3_df <- austen_novels2_df %>% 
  mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01") )

austen_novels3_df$Date <- as.Date(austen_novels3_df$Date, format = "%Y-%m-%d")

austen_novels_by_date.df <- austen_novels3_df %>% 
  group_by(Date, Name) %>% 
  summarize(monthly_checkouts = sum(Checkouts))


line_plot <- ggplot(austen_novels_by_date.df) +
  geom_line(aes(x = as.Date(Date), 
                y = monthly_checkouts,
                color = Name)) +
  labs(title = "Monthly Checkouts of Jane Austen Novels",
       x = "Date",
       y = "Total Checkouts",
       color = "Title") 

ggplotly(line_plot)
