austen_df <- read.csv('~/Desktop/INFO-201A/Checkouts_by_Austen.csv', stringsAsFactors = FALSE)
library(stringr)
library("dplyr")

austen_df$Title <- tolower(austen_df$Title)

# Filter for all 6 novels 
austen_novels_df <- austen_df %>% 
  filter(str_detect(Title, "sense and sensibility|pride and prejudice|mansfield park|emma|persuasion|Northanger Abbey") & str_detect(Title, "zombies|sea monsters|drama", negate = TRUE))

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

# Question 1
# a) What year had the most checkouts for Jane Austen's novels?
year_max_checkouts <- austen_novels2_df %>% 
  group_by(CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(CheckoutYear)

# b) How many books were checked out that year?
max_checkouts <- austen_novels2_df %>% 
  group_by(CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(total_checkouts)

# Question 2
# a) Which novel was the most popular of all time?
popular_novel <- austen_novels2_df %>% 
  group_by(Name) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(Name)

# b) How many times was this book checked out?
popular_novel_checkouts <- austen_novels2_df %>% 
  group_by(Name) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(total_checkouts)

# Question 3
# a) Which novel was the least popular of all time?
unpopular_novel <- austen_novels2_df %>% 
  group_by(Name) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts)) %>% 
  pull(Name)

# b) How many times was this book checked out?
unpopular_novel_checkouts <- austen_novels2_df %>% 
  group_by(Name) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts)) %>% 
  pull(total_checkouts)

# Question 4
# a) How has number of print books and ebooks changed over the years?

