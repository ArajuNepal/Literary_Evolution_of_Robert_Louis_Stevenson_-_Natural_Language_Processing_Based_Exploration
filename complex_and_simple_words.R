## Categorising words into simple and complex categories

#First download all the csv files from brown corpus and then read it
lemma <- read.csv("brown_corpus/lemmas.csv")
adjective <- read.csv("brown_corpus/adjectives.csv")
adverb <- read.csv("brown_corpus/adverb.csv")
conjunction <- read.csv("brown_corpus/conjunction.csv")
noun <- read.csv("brown_corpus/noun.csv")
preposition <- read.csv("brown_corpus/preposition.csv")
pronoun <- read.csv("brown_corpus/pronoun.csv")
verb <- read.csv("brown_corpus/verb.csv")
word <- read.csv("brown_corpus/words.csv")


# Bind all the above data into a single dataset
brown_corpus <- bind_rows(
  lemma,
  adjective,
  adverb,
  conjunction,
  noun,
  preposition,
  pronoun,
  verb,
  word
)


#Create a tibble, rename the column names, remove the first two rows
brown_corpus <- as_tibble(brown_corpus) %>%
  rename(
    word = corpus,
    brown_freq = preloaded.brown_1
  ) %>%
  slice(-(1:2)) %>%    # removing first two rows
  mutate(brown_freq = as.integer(brown_freq))


#Create a new dataframe where each unique word from brown_corpus appears only once
brown_corpus_clean <- brown_corpus %>%
  group_by(word) %>%
  summarise(brown_freq = max(brown_freq, na.rm = TRUE)) %>%
  ungroup()
brown_corpus_clean


#Check if there are duplicates
brown_corpus_clean %>%
  count(word) %>%
  filter(n > 1)


#Add length and syllables of each word in the tibble
dataset_with_brown_frequency <- tokenized_set %>%
  left_join(brown_corpus_clean, by = "word") %>%
  #mutate(word_length = nchar(word)) %>%       # length of each word
  #mutate(syllables = nsyllable(word)) %>%     # count syllable in each word
  arrange(brown_freq)


prefixes <- c("un", "re", "in", "im", "il", "ir", "dis", "pre", "mis", "non", "re","over", "under","sub", 
              "inter", "super", "semi", "anti", "mid", "de", "trans", "auto", "extra", "fore", "bi", "tri", 
              "mono", "multi", "co", "counter", "en", "em", "pro", "post", "micro", "macro", "hyper", "ultra")
suffixes <- c("ing", "ed", "er", "est", "ness", "ment", "ly", "able", "ible", "ful", "less", "ous", "ive", "tion", 
              "sion", "al", "ial", "ic", "ity", "ty", "ant", "ent", "ism", "ist", "en", "ize", "ise", "ward", "wise", 
              "hood", "ship", "ate", "y", "dom","s")
# words like none, council, income, extent, seven, serious,cool, christ, forward, post, middle,lady, relate, brother, enemy, green, separate, heavy, conduct, enjoy, worry, wise, winter
# are exceptions; prefix use like non, co, 

#Create regex patterns from prefixes and suffixes 
prefix_pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")
suffix_pattern <- paste0("(", paste(suffixes, collapse = "|"), ")$")

#Categorising
unigrams_categorised <- dataset_with_brown_frequency %>% 
  mutate(has_prefix = str_detect(word, prefix_pattern),
         has_suffix = str_detect(word, suffix_pattern),
         lemma = lemmatize_words(word),
         is_lemma_same = word == lemma,
         is_complex = (has_prefix | has_suffix) & !is_lemma_same,
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

