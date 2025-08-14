## Categorising words into simple and complex categories

prefixes <- c("un", "re", "in", "im", "il", "ir", "dis", "pre", "mis", "non", "re","over", "under","sub", 
              "inter", "super", "semi", "anti", "mid", "de", "trans", "auto", "extra", "fore", "bi", "tri", 
              "mono", "multi", "co", "counter", "en", "em", "pro", "post", "micro", "macro", "hyper", "ultra")
suffixes <- c("ing", "ed", "er", "est", "ness", "ment", "ly", "able", "ible", "ful", "less", "ous", "ive", "tion", 
              "sion", "al", "ial", "ic", "ity", "ty", "ant", "ent", "ism", "ist", "en", "ize", "ise", "ward", "wise", 
              "hood", "ship", "ate", "y", "dom","s")

# A list created by manually checking for exceptions in the dataset; this is not a fixed list  
exception_list <- c("none", "council", "income", "extent", "seven", "serious", "cool", "christ", "forward", "post", "middle", "lady", 
                    "relate","brother", "enemy", "green", "separate", "heavy", "conduct", "enjoy", "worry", "wise", "winter")

              
#Create regex patterns from prefixes and suffixes 
prefix_pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")
suffix_pattern <- paste0("(", paste(suffixes, collapse = "|"), ")$")

#Categorising
unigrams_categorised <- stopwords_removed_set %>% 
  mutate(has_prefix = str_detect(word, prefix_pattern),
         has_suffix = str_detect(word, suffix_pattern),
         lemma = lemmatize_words(word),
         is_lemma_same = word == lemma,
         is_exception = word %in% exception_list,
         is_complex = (has_prefix | has_suffix) & !is_lemma_same & !is_exception,
         complexity_type = case_when(is_complex ~ "Complex", TRUE ~ "Simple"))
unigrams_categorised


# Write the above dataset to a csv file to locally save in your computer to view in detail
write.csv(unigrams_categorised, "unigrams_categorised.csv")


# Calculating total words in each book
total_words <- unigrams_categorised %>% 
  group_by(book) %>%
  summarise(total = n())

# Word counts for each book and each type
word_counts <- unigrams_categorised %>%
  group_by(book, complexity_type) %>%
  summarise(count = n()) %>%
  ungroup()


# Generating percentages
percentage_table <- word_counts %>% 
  left_join(total_words, by = "book") %>%
  mutate(percentage = round((count / total) * 100, 2)) %>%
  select(book, complexity_type, percentage)
percentage_table

percentage_table_wide <- percentage_table %>%
  pivot_wider(
    names_from = complexity_type,
    values_from = percentage
  )
percentage_table_wide




# https://r-packages.io/datasets/data_syllables_en?

