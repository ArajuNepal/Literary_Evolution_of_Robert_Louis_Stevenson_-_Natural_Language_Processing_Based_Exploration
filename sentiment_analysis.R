####### SENTIMENT ANALYSIS ----

## Loading the sentiment dictionaries
afinn <- get_sentiments("afinn")
afinn                                  #has values between -5 and 5

# Download this first in your machine and then load it in R : https://github.com/aesuli/SentiWordNet
sentiword <- read.delim("SentiWordNet_3.0.0.txt", comment.char = "#", header = FALSE)
sentiword

# Download this first in your machine and then load it in R : https://sentic.net/downloads/ then go to SenticNet under English Resources
senticnet <- read_excel("senticnet/senticnet.xlsx")
senticnet


## Total unique words in each book
#Catriona
total_catriona_words <- n_distinct(catriona_unigram$word)
total_catriona_words
#Treasure Island
total_treasue_island_words <- n_distinct(treasure_island_unigram$word)
total_treasue_island_words



## Calculating the proportion of words in dictionaries and books 

# Catriona and afinn
# Join to keep only words in both datasets : afinn and catriona
catriona_with_afinn <- catriona_unigram %>%      
  inner_join(afinn, by = "word")    
# Count how many unique words in Catriona matched AFINN
catriona_afinn_matched_words <- n_distinct(catriona_with_afinn$word)
# Proportion
catriona_afinn_proportion_matched <- round((catriona_afinn_matched_words / total_catriona_words) * 100,2)


#Treasure Island and afinn 
# Join to keep only words in both datasets : afinn and treasure island
treasure_island_with_afinn <- treasure_island_unigram %>%
  inner_join(afinn, by = "word")
# Count how many unique words in Treasure Island matched AFINN
treasure_island_afinn_matched_words <- n_distinct(treasure_island_with_afinn$word)
# Proportion
treasure_island_afinn_proportion_matched <- round((treasure_island_afinn_matched_words / total_treasue_island_words) * 100,2)


#Catriona and senticnet
senticnet <- senticnet %>%
  select(word = CONCEPT) 
catriona_with_senticnet <- catriona_unigram %>%
  inner_join(senticnet, by = "word")
catriona_senticnet_matched_words <- n_distinct(catriona_with_senticnet$word)
catriona_senticnet_proportion_matched <- round((catriona_senticnet_matched_words / total_catriona_words) * 100,2)


#Treasure Island and senticnet
treasure_island_with_senticnet <- treasure_island_unigram %>%
  inner_join(senticnet, by = "word")
treasure_island_senticnet_matched_words <- n_distinct(treasure_island_with_senticnet$word)
treasure_island_senticnet_proportion_matched <- round((treasure_island_senticnet_matched_words / total_treasue_island_words) * 100,2)


#Catriona and sentiword 
sentiword <- sentiword %>%
  select(word = V5) 
catriona_with_sentiword <- catriona_unigram %>%
  inner_join(sentiword, by = "word")
catriona_sentiword_matched_words <- n_distinct(catriona_with_sentiword$word)
catriona_sentiword_proportion_matched <- round((catriona_sentiword_matched_words / total_catriona_words) * 100,2)


#Treasure Island and sentiword 
treasure_island_with_sentiword <- treasure_island_unigram %>%
  inner_join(sentiword, by = "word")
treasure_island_sentiword_matched_words <- n_distinct(treasure_island_with_sentiword$word)
treasure_island_sentiword_proportion_matched <- round((treasure_island_sentiword_matched_words / total_treasue_island_words) * 100,2)



# Displaying the proportions
sentiment_analysis_dictionary_comparison_table <- tibble(
  Dictionary = c("AFINN", "SentiWordNet", "SenticNet"),
  `Catriona (%)` = c(
    catriona_afinn_proportion_matched,
    catriona_sentiword_proportion_matched,
    catriona_senticnet_proportion_matched
  ),
  `Treasure Island (%)` = c(
    treasure_island_afinn_proportion_matched,
    treasure_island_sentiword_proportion_matched,
    treasure_island_senticnet_proportion_matched
  )
)
sentiment_analysis_dictionary_comparison_table



#Calculating sentiment score from positive and negative sentiment scores for each word in sentiword dictionary
sentiword_processed <- sentiwordnet %>%
  select(word = V5, pos_score = V3, neg_score = V4) %>%
  mutate(sentiment_score = pos_score - neg_score) %>%
  group_by(word) %>%
  summarise(sentiment_score = mean(sentiment_score, na.rm = TRUE), .groups = 'drop')
sentiword_processed



# Join with SentiWordNet sentiment scores
catriona_sentiword_sentiment <- catriona_unigram %>%
  left_join(sentiword_processed, by = "word") %>%
  replace_na(list(sentiment_score = 0)) %>%      # Assign 0 to words not found in SentiWordNet
  mutate(contribution = n * sentiment_score)
catriona_sentiword_sentiment

# Calculate average sentiment for the book
average_sentiment_catriona_unigrams <- catriona_sentiword_sentiment %>%
  summarise(
    total_n = sum(n, na.rm = TRUE),
    total_contribution = sum(contribution, na.rm = TRUE),
    average_sentiment = total_contribution / total_n
  )
average_sentiment_catriona_unigrams


# Join with SentiWordNet sentiment scores
treasure_island_sentiword_sentiment <- treasure_island_unigram %>%
  left_join(sentiword_processed, by = "word") %>%
  replace_na(list(sentiment_score = 0)) %>%  # Assign 0 to words not found in SentiWordNet
  mutate(contribution = n * sentiment_score)
treasure_island_sentiword_sentiment

# Calculate average sentiment for the book
average_sentiment_treasure_island_unigrams <- treasure_island_sentiword_sentiment %>%
  summarise(
    total_n = sum(n, na.rm = TRUE),
    total_contribution = sum(contribution, na.rm = TRUE),
    average_sentiment = total_contribution / total_n
  )
average_sentiment_treasure_island_unigrams




## Sentiment analysis on sentences 

# Clean the raw data as we did in data cleaning (except removing fullstop and question mark)
cleaning <- novels %>% 
  
  mutate(book = if_else(gutenberg_id == 30870, "Catriona","Treasure Island")) %>% select(-gutenberg_id) %>%
  
  filter(!(book == "Catriona" & (linenumber >= 1 & linenumber <= 189 ))) %>%         #removing preface and other non-narrative text from Catriona (lines 1 to 189) 
  
  filter(!(book == "Treasure Island" & (linenumber <= 224 | linenumber %in% c(250, 1867, 2306, 3792, 4636, 5749, 7534,7688)))) %>%  # Removing known noisy lines in Treasure Island (e.g., illustrations, page numbers, etc.)
  
  mutate(text = str_trim(text)) %>%
  
  filter(text != "" & text != "	") %>%                                               #removing empty rows from the tibble
  
  mutate(text = str_replace_all(text, "[[:digit:]]+","")) %>%                        #removing the occurrence of numbers
  
  mutate(text = str_replace_all(text, "(?<=\\b)[ivxlcdm]+\\.(?=\\s|$)", "")) %>%     #removing the occurrence of roman numbers
  
  mutate(text = str_replace_all(text, "'s","")) %>%                                  #eg: replacing all he's to he
  
  mutate(text = str_replace_all(text, '[,!@#$%^&*()/\\[\\]{};:”“<>\\|`_+~=]', "")) %>% #removing the occurrence of symbols
  
  mutate(text = str_replace_all(text, " th ", " ")) %>%                              #removing instances like ' th ', ' rd', ' st ', ' nd '
  mutate(text = str_replace_all(text, " rd", " ")) %>%   
  mutate(text = str_replace_all(text, " st ", " ")) %>%   
  mutate(text = str_replace_all(text, " nd ", " ")) %>%
  
  mutate(text = str_replace_all(text, "-+", " ")) %>%                                #replacing the occurrence of hypen and multiple occurrences of hypen with a whitespace
  
  filter(!startsWith(text, "illustration")) %>%  
  
  mutate(parts = if_else(book == "Treasure Island", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(parts = if_else(!is.na(parts), row_num, NA), next_row_num = parts + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(parts %in% row_num)) %>% 
  select(-parts,-row_num, -next_row_num) %>%                                         #removing rows that mention part number and part name in the row after that 
  
  mutate(parts = if_else(book == "Catriona", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(parts = if_else(!is.na(parts), row_num, NA), next_row_num = parts + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(parts %in% row_num)) %>% 
  select(-parts,-row_num, -next_row_num) %>%                                         #removing rows that mention part number and part name in the row after that 
  
  mutate(chapters = if_else(book == "Treasure Island", str_extract(text, "^chapter [ivxlcdm]+"),NA), row_num = row_number()) %>% 
  mutate(chapters = if_else(!is.na(chapters), row_num, NA), next_row_num = chapters + 1 ) %>% 
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(chapters %in% row_num)) %>% 
  select(-chapters, -row_num, -next_row_num) %>%                                     #removing rows that mentions chapter number and chapter name in the row after that
  
  mutate(chapters_catriona = if_else(book == "Catriona", str_extract(text, "^chapter [ivxlcdm]+"),NA), row_num = row_number()) %>% 
  mutate(chapters_catriona = if_else(!is.na(chapters_catriona), row_num, NA), next_row_num = chapters_catriona + 1 ) %>% 
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(chapters_catriona %in% row_num)) %>% 
  select(-chapters_catriona,-next_row_num, -row_num, -next_row_num) %>%               #removing rows that mentions chapter number and chapter name in the row after that
  
  select(book, text)


# Collapse the rows into separate rows for each book 
full_text_by_book <- cleaning %>%
  group_by(book) %>%
  summarise(full_text_content = str_c(text, collapse = " ")) %>%
  ungroup()
full_text_by_book


# Extract and store each sentence in each row
processed_text <- full_text_by_book %>%
  mutate(sentence = str_extract_all(full_text_content, ".+?[.?](?:\\s+|$)")) %>%      # The regex captures the punctuation at the end
  unnest(sentence) %>%                                                                # Expand the list of segments into individual rows
  select(book, sentence_text = sentence) %>%
  mutate(sentence_text = str_squish(sentence_text)) %>%                               # Clean up extra whitespace and remove any empty segments
  filter(sentence_text != "") 
processed_text


# Add sentence id for Catriona
catriona_processed_text <- processed_text %>%
  filter(book == "Catriona") %>%
  mutate(sentence_id = row_number())
catriona_processed_text

# Add sentence id for Treasure Island
treasure_island_processed_text <- processed_text %>% 
  filter(book == "Treasure Island") %>% 
  mutate(sentence_id = row_number())
treasure_island_processed_text


# Calculate sentiment score for each word in Catirona based on SentiWordNet dictionary
catriona_sentence_sentiment <- catriona_processed_text %>%
  unnest_tokens(word, sentence_text) %>%
  # Count total words per sentence before joining with sentiment lexicon
  group_by(sentence_id) %>%
  mutate(total_words_in_sentence = n()) %>%
  ungroup() %>%
  left_join(sentiword_processed, by = "word") %>%
  mutate(sentiment_score = replace_na(sentiment_score, 0)) %>%
  group_by(sentence_id, total_words_in_sentence) %>% # Group by both to retain total_words_in_sentence
  summarise(sum_sentiment = sum(sentiment_score),.groups = "drop") %>%
  mutate(average_sentiment = sum_sentiment / total_words_in_sentence) %>% # Calculate average
  right_join(catriona_processed_text, by = "sentence_id") %>%
  mutate(average_sentiment = replace_na(average_sentiment, 0)) %>% # Replace NA for sentences with no sentiment words
  arrange(sentence_id)
catriona_sentence_sentiment


# Calculate sentiment score for each word in Treasure Island based on SentiWordNet dictionary
treasure_island_sentence_sentiment <- treasure_island_processed_text %>%
  unnest_tokens(word, sentence_text) %>%
  # Count total words per sentence before joining with sentiment lexicon
  group_by(sentence_id) %>%
  mutate(total_words_in_sentence = n()) %>%
  ungroup() %>%
  left_join(sentiword_processed, by = "word") %>%
  mutate(sentiment_score = replace_na(sentiment_score, 0)) %>%
  group_by(sentence_id, total_words_in_sentence) %>% # Group by both to retain total_words_in_sentence
  summarise(sum_sentiment = sum(sentiment_score),.groups = "drop") %>%
  mutate(average_sentiment = sum_sentiment / total_words_in_sentence) %>% # Calculate average
  right_join(treasure_island_processed_text, by = "sentence_id") %>%
  mutate(average_sentiment = replace_na(average_sentiment, 0)) %>% # Replace NA for sentences with no sentiment words
  arrange(sentence_id)
treasure_island_sentence_sentiment


# Create chunks : one chunk = 100 sentences or rows
catriona_sentence_sentiment_plot <- catriona_sentence_sentiment %>%
  mutate(index = sentence_id %/% 100) %>%                                             # Create chunk index
  group_by(book, index) %>%                                                           # Group by both book and index
  summarise(avg_sentiment = mean(average_sentiment, na.rm = TRUE), .groups = "drop")
catriona_sentence_sentiment_plot

treasure_island_sentence_sentiment_plot <- treasure_island_sentence_sentiment %>%
  mutate(index = sentence_id %/% 100) %>%                                             # Create chunk index
  group_by(book, index) %>%                                                           # Group by both book and index
  summarise(avg_sentiment = mean(average_sentiment, na.rm = TRUE), .groups = "drop")
treasure_island_sentence_sentiment_plot


# Determine the minimum number of chunks
min_chunks <- min(
  max(catriona_sentence_sentiment_plot$index),
  max(treasure_island_sentence_sentiment_plot$index)
)

# Truncate both datasets to the minimum number of chunks
catriona_trimmed <- catriona_sentence_sentiment_plot %>%
  filter(index <= min_chunks)

treasure_island_trimmed <- treasure_island_sentence_sentiment_plot %>%
  filter(index <= min_chunks)

# Define the rolling window size
window_size <- 5  # Adjust as needed

# Apply rolling mean
catriona_trimmed <- catriona_trimmed %>%
  arrange(index) %>%
  mutate(rolling_avg_sentiment = rollmean(avg_sentiment, k = window_size, fill = NA, align = "right"))

treasure_island_trimmed <- treasure_island_trimmed %>%
  arrange(index) %>%
  mutate(rolling_avg_sentiment = rollmean(avg_sentiment, k = window_size, fill = NA, align = "right"))


# Combine the datasets
combined_sentiment_plot <- bind_rows(
  catriona_trimmed %>% mutate(book = "Catriona"),
  treasure_island_trimmed %>% mutate(book = "Treasure Island")
)

# Plot the rolling average sentiment
sentence_level_sentiment_analysis <- ggplot(combined_sentiment_plot, aes(x = index, y = rolling_avg_sentiment, color = book)) +
  geom_line() +
  scale_color_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) +
  labs(
    title = "Sentiment Trajectory by Book (Rolling Average)",
    x = "Narrative Segment Index",
    y = "Rolling Average Sentiment Score"
  ) +
  theme_minimal()

sentence_level_sentiment_analysis


