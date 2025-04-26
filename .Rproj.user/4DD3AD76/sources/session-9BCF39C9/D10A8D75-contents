# Description: Studying differences/similarities between 'Treasure Island' and 'Catriona' using NLP
# Author: Araju Mepal
# Last modified: April 10, 2025


## Data cleaning ----
cleaned_set_step1 <- novels %>% 
  
  mutate(book = if_else(gutenberg_id == 30870, "Catriona","Treasure Island")) %>% select(-gutenberg_id) %>%
  
  filter(!(book == "Catriona" & (linenumber >= 1 & linenumber <= 189 ))) %>%    # Removing preface and other non-narrative text from Catriona (lines 1 to 189) 
  
  filter(!(book == "Treasure Island" & (linenumber <= 224 | linenumber %in% c(250, 1867, 2306, 3792, 4636, 5749, 7534,7688)))) %>%  # Removing known noisy lines in Treasure Island (e.g., illustrations, page numbers, etc.)
  
  mutate(text = str_trim(text)) %>%
  
  filter(text != "" & text != "	") %>% #removing empty rows from the tibble
  
  mutate(text = str_replace_all(text, "[[:digit:]]+","")) %>% #removing the occurrence of numbers
  
  mutate(text = str_replace_all(text, "(?<=\\b)[ivxlcdm]+\\.(?=\\s|$)", "")) %>% #removing the occurrence of roman numbers
  
  mutate(text = str_replace_all(text, "'s","")) %>% # eg: replacing all he's to he
  
  mutate(text = str_replace_all(text, '[,.!@#$%^&*()/\\[\\]{};:”“<>?\\|`_+~=]', "")) %>% #removing the occurrence of symbols
  
  mutate(text = str_replace_all(text, "-+", " ")) %>% #replacing the occurrence of hypen and multiple occurrences of hypen with a whitespace
  
  filter(!startsWith(text, "illustration")) %>%  
  
  mutate(parts = if_else(book == "Treasure Island", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(parts = if_else(!is.na(parts), row_num, NA), next_row_num = parts + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(parts %in% row_num)) %>% 
  select(-parts,-row_num, -next_row_num) %>% #removing rows that mention part number and part name in the row after that 
  
  mutate(parts = if_else(book == "Catriona", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(parts = if_else(!is.na(parts), row_num, NA), next_row_num = parts + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(parts %in% row_num)) %>% 
  select(-parts,-row_num, -next_row_num) %>% #removing rows that mention part number and part name in the row after that 
  
  mutate(chapters = if_else(book == "Treasure Island", str_extract(text, "^chapter [ivxlcdm]+"),NA), row_num = row_number()) %>% 
  mutate(chapters = if_else(!is.na(chapters), row_num, NA), next_row_num = chapters + 1 ) %>% 
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(chapters %in% row_num)) %>% 
  select(-chapters, -row_num, -next_row_num) %>% #removing rows that mentions chapter number and chapter name in the row after that
  
  mutate(chapters_catriona = if_else(book == "Catriona", str_extract(text, "^chapter [ivxlcdm]+"),NA), row_num = row_number()) %>% 
  mutate(chapters_catriona = if_else(!is.na(chapters_catriona), row_num, NA), next_row_num = chapters_catriona + 1 ) %>% 
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(chapters_catriona %in% row_num)) %>% 
  select(-chapters_catriona,-next_row_num, -row_num, -next_row_num) #removing rows that mentions chapter number and chapter name in the row after that




# Tokenizing the dataset after first step of data cleaning
tokenized_set_draft <- cleaned_set_step1 %>% 
  mutate(text = str_replace_all(text, '""', "")) %>%
  unnest_tokens(word, text) %>% 
  mutate(word = str_trim(word)) %>% 
  count(book, word, sort = TRUE)



# Final step of data cleaning
cleaned_set <- cleaned_set_step1 %>%
  mutate(text = str_replace_all(text, c("a'body" = "everybody", "a'thegether" = "altogether", "a'thing" = "anything","\\bken\\b" = "know", 
                                        "\\bkens\\b" = "knows","\\bapt\\b" = "appropriate","\\bane\\b" = "one", "\\bae\\b" = "one","\\bye\\b" = "you", 
                                        "\\baye\\b" = "yes","\\bwi\\b" = "with","\\bcap'ns\\b" = "captains","\\bcap'n\\b" = "captain","\\bem\\b" = "them",
                                        "\\bna\\b" = "no", "\\bnaething\\b" = "nothing","\\bcam\\b" = "call","\\bfro\\b" = "from", "\\been\\b" = "eyes",
                                        "\\bnae\\b" = "no", "\\bauld\\b" = "old", "\\bgude\\b" = "god", "\\bpalfour\\b" = "balfour", "wouldnae" = "would not",
                                        "didnae" = "did not", "hadnae" = "had not", "couldnae" = "could not", "wasnae"= "was not","isnae" = "is not", 
                                        "\\bquo'\\b" = "quote","\\bwhaur\\b" = "where", "\\bither\\b" = "either", "\\bthon\\b" = "that", "\\blikit\\b" = "liked", 
                                        "\\bweemen\\b" = "women","\\bye're\\b" = "you are", "\\bye'll\\b" = "you will", "\\baweel\\b" = "oh well", 
                                        "\\bsae\\b" = "so","\\bnane\\b" = "not"))) %>%       
  mutate(text = str_replace_all(text, "\'", ""))       # Replacing Scottish English variants with standard English equivalents for consistency




# Tokenizing the dataset after complete data cleaning
tokenized_set <- cleaned_set %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = str_trim(word)) %>% 
  count(book, word, sort = TRUE)


total_words_in_corpus <- tokenized_set %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

tokenized_set <- left_join(tokenized_set, total_words_in_corpus)




## Stop words ----
data(stop_words, package = "tidytext")



# Adding custom stopwords to the existing stopwords dataset
additional_stopwords <- tibble(word = c("yo", "ho", "ye", "ah", "eh", "em", "oho", "aha", "ay", "ot", "yon", "ere", "hm",
                                        "hae", "twa","wee", "ain", "awa", "ony", "aff", "ee", "im"), lexicon = "custom")

final_stopwords_set <- bind_rows(stop_words, additional_stopwords)

stopwords_removed_set <- tokenized_set %>% 
  select(-total) %>% 
  anti_join(final_stopwords_set, by = "word") %>%
  mutate(text = str_replace_all(word, '\'', "")) %>%
  select(-text)



# Total words in the corpus
total_words_after_stopwords_removal <- stopwords_removed_set %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

word_count_after_stopwords_removal <- left_join(stopwords_removed_set, total_words_after_stopwords_removal)



# Checking the occurrences of additional stopwords in each book 
additional_stopword_counts <- tokenized_set %>%
  filter(word %in% additional_stopwords$word) %>%  
  group_by(book, word) %>%  
  summarise(count = sum(n), .groups = 'drop')



# Total unique stopwords for each book
unique_stopwords_counts <- tokenized_set %>%
  filter(word %in% additional_stopwords$word) %>%
  group_by(book) %>%
  summarise(unique_stopwords = n_distinct(word))



# Visualisation of count of additional stopwords in each book
additional_stopwords_in_each_book_visualisation <- ggplot(unique_stopwords_counts, aes(x = book, y = unique_stopwords, fill = book)) +
  geom_bar(stat = "identity", width = 0.3) +
  theme_minimal() +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  labs(title = "Total unique stopwords per book",
       x = "Book",
       y = "Total unique stopwords") +
  coord_cartesian(ylim = c(0,25)) +
  scale_x_discrete() + 
  theme(
    plot.title = element_text(size = 9),         
    axis.title = element_text(size = 9),         
    axis.text = element_text(size = 8),           
    legend.title = element_text(size = 8),       
    legend.text = element_text(size = 8)
  )



# Count of common additional stopwords in the books
common_additional_stopwords <- additional_stopword_counts %>%
  group_by(word) %>%
  filter(n_distinct(book) > 1) %>%
  ungroup()



# Visualisation of common stopwords in both books with their counts
common_additional_stopwords_visualisation <- ggplot(common_additional_stopwords, aes(x = word, y = count, fill = book)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +  # Position dodge makes bars for each book side by side
  labs(x = "Word", y = "Count", title = "Word counts in Catriona and Treasure Island") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_cartesian(ylim = c(0,40)) +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  theme(
    plot.title = element_text(size = 9),         
    axis.title = element_text(size = 9),         
    axis.text = element_text(size = 8),           
    legend.title = element_text(size = 8),       
    legend.text = element_text(size = 8)
  )




## Lemmatization ----
lemmatized_words <- word_count_after_stopwords_removal %>% 
  mutate(lemmatized_word = lemmatize_words(word)) 

# Grouping lemmatized duplicates (e.g., "run", "running") and summing their word counts
duplicate_rows <- lemmatized_words %>%
  group_by(lemmatized_word,book) %>%  
  filter(n() > 1) %>% 
  arrange(lemmatized_word)       


dataset_after_lemmatization <- duplicate_rows %>% 
  group_by(lemmatized_word, book) %>% 
  summarize(n = sum(n),                  # Sum the 'n' values for each group
            total = first(total),        # Keep the 'total' from the first occurrence of each group
            book = first(book),          # Keep the 'book' from the first occurrence
            word = first(word)) %>%      # Keep the 'word' from the first occurrence
  ungroup() %>% 
  select(-word) %>%
  mutate(lemmatized_word = str_replace_all(lemmatized_word, c("ken" = "know")))
