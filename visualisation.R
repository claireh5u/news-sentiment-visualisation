# install libraries
library(jsonlite)

data <- fromJSON("/Users/clairehsu/Documents/university of sheffield/data science/introduction in data science/formatted.json")

df <- data.frame(
  headline = data$headline, # extract headlines
  category = data$category, # extract categories
  date = data$date # extract date
)

# convert all text to lowercase
df$headline <- tolower(df$headline)

# remove punctuation
df$headline <- gsub("[[:punct:]]", "", df$headline)

# lemmatise
library(usethis)
library(devtools)
library(textstem)

# lemmatise
df$headline <- lemmatize_strings(df$headline)

# tokenise
library(tidyverse)
library(tidytext)
library(textclean) 

# tokenise
tokenized_data <- df %>%
  unnest_tokens(word, headline, drop = FALSE)

# remove stop words
without_stopwords <-tokenized_data %>%
  anti_join(stop_words, by ="word")




# create a category distribution over time
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# convert the date column to date format 
df$date <- as.Date(df$date)

# extract year from the date
df$year <- format(df$date, "%Y")

# aggregate data: count headlines per category per year
category_distribution <- df %>%
  group_by(year, category) %>%
  summarise(count = n(), .groups = "drop")



# generate 45 colors from a brewer palette
my_colors <- colorRampPalette(brewer.pal(12, "Set3"))(45)

# visualize the distribution
ggplot(category_distribution, aes(x = year, y = count, color = category, group = category)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Category Distribution Over Time",
       x = "Year",
       y = "Number of Headlines",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = my_colors)




category_map <- c(
  "ARTS" = "culture",
  "ARTS & CULTURE" = "culture",
  "BLACK VOICES" = "culture",
  "BUSINESS" = "business",
  "COLLEGE" = "culture",
  "COMEDY" = "culture",
  "CRIME" = "crime",
  "CULTURE & ARTS" = "culture",
  "DIVORCE" = "lifestyle",
  "EDUCATION" = "culture",
  "ENTERTAINMENT" = "culture",
  "ENVIRONMENT" = "environment",
  "FIFTY" = "other",
  "FOOD & DRINK" = "lifestyle",
  "GOOD NEWS" = "other",
  "GREEN" = "environment",
  "HEALTHY LIVING" = "lifestyle",
  "HOME & LIVING" = "lifestyle",
  "IMPACT" = "other",
  "LATINO VOICES" = "culture",
  "MEDIA" = "other",
  "MONEY" = "business",
  "PARENTING" = "lifestyle",
  "PARENTS" = "lifestyle",
  "POLITICS" = "politics",
  "QUEER VOICES" = "culture",
  "RELIGION" = "lifestyle",
  "SCIENCE" = "technology",
  "SPORTS" = "other",
  "STYLE" = "lifestyle",
  "STYLE & BEAUTY" = "lifestyle",
  "TASTE" = "lifestyle",
  "TECH" = "technology",
  "THE WORLDPOST" = "world news",
  "TRAVEL" = "lifestyle",
  "U.S. NEWS" = "world news",
  "WEDDINGS" = "lifestyle",
  "WEIRD NEWS" = "other",
  "WELLNESS" = "lifestyle",
  "WOMEN" = "culture",
  "WORLD NEWS" = "world news",
  "WORLDPOST" = "world news"
)

category_distribution <- category_distribution %>%
  mutate(new_category = recode(category, !!!category_map))

ggplot(category_distribution, aes(x = year, y = count, color = new_category, group = new_category)) +
  geom_line(size = 0.7) +
  labs(title = "Category Distribution Over Time",
       x = "Year",
       y = "Number of Headlines",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = my_colors)


#--------------------------
# heatmap correlation matrix
install.packages("reshape2")  # For reshaping the data
install.packages("corrplot")  # For easy correlation plots

# transform to wide format
wide_data <- pivot_wider(category_distribution, names_from = new_category, values_from = count, values_fill = 0)

# calculate the correlation Matrix
cor_matrix <- cor(wide_data[,-1])  # Exclude the "Year" column

library(corrplot)

corrplot(cor_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "upper", tl.col = "black", tl.srt = 45)

#-----------------------------------

# with tokenize headlines and join with AFINN
sentiment_data <- without_stopwords %>%
  inner_join(afinn_lexicon, by = "word") %>%
  group_by(date) %>%
  summarise(daily_sentiment = sum(value))

sentiment_data$date <- as.Date(sentiment_data$date)
# Aggregate by month
monthly_sentiment <- sentiment_data %>%
  mutate(month = floor_date(date, "month")) %>% # Group by month
  group_by(month) %>%
  summarise(net_sentiment = sum(daily_sentiment)) # Sum sentiment per month

# visualize sentiment trends
ggplot(monthly_sentiment, aes(x = month, y = net_sentiment)) +
  geom_line(color = "darkolivegreen", size = 0.8) +
  labs(title = "Sentiment Trends Over Time (AFINN)",
       x = "Month",
       y = "Net Sentiment Score") +
  theme_minimal()


# Category-Specific Sentiment Trends
category_sentiment <- without_stopwords %>%
  mutate(new_category = recode(category, !!!category_map))

afinn_lexicon <- get_sentiments("afinn")

category_sentiment <- without_stopwords %>%
  inner_join(afinn_lexicon, by = "word") %>%
  group_by(date, category) %>%
  summarise(net_sentiment = sum(value))

category_sentiment$date <- as.Date(category_sentiment$date)

# aggregate by month and category
monthly_category_sentiment <- category_sentiment %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month, new_category) %>%
  summarise(net_sentiment = sum(net_sentiment))

# plot category-specific trends
ggplot(monthly_category_sentiment, aes(x = month, y = net_sentiment, color = new_category)) +
  geom_line(size = 0.5) +
  labs(title = "Sentiment Trends by Category (AFINN)",
       x = "Month",
       y = "Net Sentiment Score",
       color = "Category") +
  theme_minimal()


# word cloud
install.packages("wordcloud")
library(wordcloud)

word_freq <- without_stopwords %>%
  count(word, sort = TRUE)

wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100, colors = brewer.pal(8, "Dark2"))



## sentiment comparison across categories_boxplot
# aggregate sentiment by category
category_sentiment <- category_sentiment %>%
  group_by(new_category) %>%
  summarise(avg_sentiment = mean(net_sentiment))

# boxplot
ggplot(category_sentiment, aes(x = new_category, y = net_sentiment, fill = new_category)) +
  geom_boxplot(outlier.shape=NA) +
  labs(title = "Sentiment Comparison Across Categories", x = "Category", y = "Sentiment") +
  theme_minimal()

#------------------------

# install libraries
library(jsonlite)

data <- fromJSON("/Users/clairehsu/Documents/university of sheffield/data science/introduction in data science/formatted.json")

df <- data.frame(
  headline = data$headline, # extract headlines
  category = data$category, # extract categories
  date = data$date # extract date
)

# convert all text to lowercase
df$headline <- tolower(df$headline)

# remove punctuation
df$headline <- gsub("[[:punct:]]", "", df$headline)

# lemmatise
library(usethis)
library(devtools)
library(textstem)

# lemmatise
df$headline <- lemmatize_strings(df$headline)

# tokenise
library(tidyverse)
library(tidytext)
library(textclean) 
library(dplyr)

# new category
category_map <- c(
  "ARTS" = "culture",
  "ARTS & CULTURE" = "culture",
  "BLACK VOICES" = "culture",
  "BUSINESS" = "business",
  "COLLEGE" = "culture",
  "COMEDY" = "culture",
  "CRIME" = "crime",
  "CULTURE & ARTS" = "culture",
  "DIVORCE" = "lifestyle",
  "EDUCATION" = "culture",
  "ENTERTAINMENT" = "culture",
  "ENVIRONMENT" = "environment",
  "FIFTY" = "other",
  "FOOD & DRINK" = "lifestyle",
  "GOOD NEWS" = "other",
  "GREEN" = "environment",
  "HEALTHY LIVING" = "lifestyle",
  "HOME & LIVING" = "lifestyle",
  "IMPACT" = "other",
  "LATINO VOICES" = "culture",
  "MEDIA" = "other",
  "MONEY" = "business",
  "PARENTING" = "lifestyle",
  "PARENTS" = "lifestyle",
  "POLITICS" = "politics",
  "QUEER VOICES" = "culture",
  "RELIGION" = "lifestyle",
  "SCIENCE" = "technology",
  "SPORTS" = "other",
  "STYLE" = "lifestyle",
  "STYLE & BEAUTY" = "lifestyle",
  "TASTE" = "lifestyle",
  "TECH" = "technology",
  "THE WORLDPOST" = "world news",
  "TRAVEL" = "lifestyle",
  "U.S. NEWS" = "world news",
  "WEDDINGS" = "lifestyle",
  "WEIRD NEWS" = "other",
  "WELLNESS" = "lifestyle",
  "WOMEN" = "culture",
  "WORLD NEWS" = "world news",
  "WORLDPOST" = "world news"
)

df <- df %>%
  mutate(category = recode(category, !!!category_map))

# convert the date column to date format 
df$date <- as.Date(df$date)

# add year from the date
df <- df %>%
  mutate(year = format(df$date, "%Y"))
df <- df %>%
  mutate(month = format(df$date, "%m"))

# ----------------------------------------------

#sentiment for each headline
# install and load the syuzhet package
install.packages("syuzhet")
library(syuzhet)

sentiment_scores <- get_sentiment(df$headline, method = "afinn")

df$sentiment <- sentiment_scores

# ---------------------------------------------
# visualize the distribution
# aggregate data: count headlines per category per year
category_distribution <- df %>%
  group_by(year, category) %>%
  summarise(count = n(), .groups = "drop")

library(ggplot2)

ggplot(category_distribution, aes(x = year, y = count, color = category, group = category)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Category Distribution Over Time",
       x = "Year",
       y = "Number of Headlines",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = color)


# Sentiment Distribution Over Time by category
df$year_month <- format(df$date, "%Y-%m-01")
df$year_month <- as.Date(df$year_month)
ggplot(df, aes(x = year_month, y = sentiment, color = category)) +
  geom_point() +
  geom_rug() +
  labs(title = "Sentiment Distribution Over Time by category",
       x = "Year",
       y = "Sentiment",
       fill = "Category") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

library(lubridate)
monthly_sentiment <- df %>%
  mutate(month = floor_date(date, "month")) %>% # group by month
  group_by(month, category) %>%
  summarise(sentiment = sum(sentiment))

ggplot(monthly_sentiment, aes(x = month, y = sentiment, color = category)) +
  geom_line() +
  labs(title = "Sentiment Distribution Over Time by category",
       x = "Year",
       y = "Sentiment",
       fill = "Category") +
  theme_minimal()

# overall
overall_sentiment <- df %>%
  mutate(month = floor_date(date, "month")) %>% # group by month
  group_by(month) %>%
  summarise(sentiment = sum(sentiment))

ggplot(overall_sentiment, aes(x = month, y = sentiment)) +
  geom_line(color = "darkolivegreen") +
  labs(title = "Sentiment Distribution Over Time",
       x = "Year",
       y = "Sentiment") +
  theme_minimal()

# ----------------------------------------------------

## plot to compare sentiment
library(tidytext)
nrc_lexicon <- get_sentiments("nrc")

# tokenise and remove stop words
sentiment_data <- df %>%
  unnest_tokens(word, headline, drop = FALSE) %>%
  anti_join(stop_words, by ="word") %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(date, sentiment, sort = TRUE) # group by date and sentiment type

install.packages("yarrr")
library(yarrr)

sentiment_colors <- setNames(piratepal(palette = "yarrr_mix")[1:10],
                             unique(sentiment_data$sentiment))

yarrr_mix <- usecol(c(piratepal("nemo"), piratepal("bugs")))

ggplot(sentiment_data, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "News Distribution 2012-2022 by sentiment",
       x = "sentiment",
       y = "sentiment count") +
  theme_minimal() +
  scale_fill_manual(values = yarrr_mix)

sentiment_data$year_month <- format(sentiment_data$date, "%Y-%m-01")
sentiment_data$year <- format(sentiment_data$date, "%Y")

ggplot(sentiment_data, aes(x = year, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "News Distribution Over Time by sentiment",
       x = "year",
       y = "sentiment count") +
  theme_minimal() +
  scale_fill_manual(values = yarrr_mix)


# x-year, y-sentiment, point-headline xxxxx
library(syuzhet)
sentiment_scores <- get_sentiment(df$headline, method = "afinn")
df$sentiment <- sentiment_scores



# --------------------------------------
# word cloud
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

politics_data <- df %>%
  filter(category == "politics")

# create a text corpus
corpus <- Corpus(VectorSource(politics_data$headline))

# preprocess the text
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%  # convert to lowercase
  tm_map(removePunctuation) %>%             # remove punctuation
  tm_map(removeNumbers) %>%                 # remove numbers
  tm_map(removeWords, stopwords("en"))      # remove stopwords

# create a Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)

# get word frequencies
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)

# create a word cloud
wordcloud(
  words = names(word_freq),       # Words
  freq = word_freq,               # Frequencies
  min.freq = 5,                   # Minimum frequency
  max.words = 100,                # Maximum words
  random.order = FALSE,           # Ordered by frequency
  colors = brewer.pal(8, "Dark2") # Color palette
)


# word cloud advanced xxxxx
library(dplyr)

set.seed(123)
df <- df %>%
  unnest_tokens(word, headline, drop = FALSE) %>%
  anti_join(stop_words, by ="word")

# create a text corpus
corpus <- Corpus(VectorSource(df$headline))

# preprocess the text
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%  # convert to lowercase
  tm_map(removePunctuation) %>%             # remove punctuation
  tm_map(removeNumbers) %>%                 # remove numbers
  tm_map(removeWords, stopwords("en"))

# create a Term-Document Matrix

tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.99)
tdm_matrix <- as.matrix(tdm)

# get word frequencies
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)

# create a word cloud
wordcloud(
  words = names(word_freq),       # Words
  freq = word_freq,               # Frequencies
  min.freq = 2,                   # Minimum frequency
  max.words = 100,                # Maximum words
  random.order = FALSE,           # Ordered by frequency
  colors = brewer.pal(8, "Dark2") # Color palette
)

# --------------------------------------

# word cloud advanced 2 xxxxx
# Group headlines by category
category_texts <- df %>%
  group_by(category) %>%
  summarise(text = paste(headline, collapse = " ")) %>%
  ungroup()

library(wesanderson)
palette <- wes_palette("FantasticFox1", n = nrow(category_texts), type = "continuous")

library(wordcloud)
for (i in 1:nrow(category_texts)) {
  category <- category_texts$category[i]
  text <- category_texts$text[i]
  color <- palette[i %% length(palette) + 1]
  
  # Generate word cloud with the same color
  cat("Generating word cloud for category:", category, "with color:", color, "\n")
  wordcloud(words = text,
            scale = c(5, 0.8),       
            max.words = 100,
            random.order = FALSE,
            colors = color) 
}


# renew color
ggplot(category_distribution, aes(x = year, y = count, color = category, group = category)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Category Distribution Over Time",
       x = "Year",
       y = "Number of Headlines",
       fill = "Category") +
  theme_minimal() +
  scale_color_manual(values = palette)

df$year_month <- format(df$date, "%Y-%m-01")
df$year_month <- as.Date(df$year_month)

sentiment_scores <- get_sentiment(df$headline, method = "afinn")
df$sentiment <- sentiment_scores

ggplot(df, aes(x = year_month, y = sentiment, color = category)) +
  geom_point() +
  geom_rug() +
  labs(title = "Sentiment Distribution Over Time by category",
       x = "Year",
       y = "Sentiment",
       fill = "Category") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(values = palette)

monthly_sentiment <- df %>%
  mutate(month = floor_date(date, "month")) %>% # group by month
  group_by(month, category) %>%
  summarise(sentiment = sum(sentiment))

ggplot(monthly_sentiment, aes(x = month, y = sentiment, color = category)) +
  geom_line() +
  labs(title = "Sentiment Distribution Over Time by category",
       x = "Year",
       y = "Sentiment",
       fill = "Category") +
  theme_minimal() +
  scale_color_manual(values = palette)