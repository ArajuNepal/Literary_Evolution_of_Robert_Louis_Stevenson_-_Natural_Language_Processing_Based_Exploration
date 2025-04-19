# Description: Studying differences/similarities between 'Treasure Island' and 'Catriona' using NLP
# Author: Araju Mepal
# Last modified: March 12, 2025

## break into separate chunks 


## Load libraries ----
pacman::p_load(gutenbergr, dplyr, tidytext, stringr, writexl, SnowballC, textstem, ggplot2, forcats, ggwordcloud, 
               tidyr, textdata, patchwork, reshape2, tokenizers, topicmodels, zoo, tm, wordcloud2, ldatuning)


## Load data ----
books <- gutenberg_download(c(27780,30870), mirror = "https://www.gutenberg.org/")
books


## Separate by book id and include line number and chapter number ----
novels <- books %>% 
  mutate(text = tolower(text)) %>% 
  group_by(gutenberg_id) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [ivxlcdm]+$"))), 
         part = cumsum(str_detect(text, regex("^part [ivxlcdm]+$")))) %>% 
  ungroup()
novels


## Data cleaning ----
cleaned_set <- novels %>% 
  
  mutate(book = if_else(gutenberg_id == 30870, "Catriona","Treasure Island")) %>% select(-gutenberg_id) %>%
  
  filter(!(book == "Catriona" & (linenumber >= 1 & linenumber <= 189 ))) %>%  
  
  filter(!(book == "Treasure Island" & (linenumber <= 224 | linenumber %in% c(250, 1867, 2306, 3792, 4636, 5749, 7534,7688)))) %>%
  
  mutate(text = str_trim(text)) %>%
  
  filter(text != "" & text != "	") %>% #removing empty rows from the tibble
  
  mutate(text = str_replace_all(text, "[[:digit:]]+","")) %>% #removing the occurrence of numbers
  
  mutate(text = str_replace_all(text, "(?<=\\b)[ivxlcdm]+\\.(?=\\s|$)", "")) %>% #removing the occurrence of roman numbers
  
  mutate(text = str_replace_all(text, "'s","")) %>% # eg: replacing all he's to he
  
  mutate(text = str_replace_all(text, '[,.!@#$%^&*()/\\[\\]{};:”“<>?\\|`_+~=""\']', "")) %>% #removing the occurrence of symbols -> there could be a few that's missing currently
  
  mutate(text = str_replace_all(text, "-+", " ")) %>% #replacing the occurrence of hypen and multiple occurrences of hypen with a whitespace\
  
  filter(!startsWith(text, "illustration")) %>%  #removing illustration
  
  mutate(parts = if_else(book == "Treasure Island", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(parts = if_else(!is.na(parts), row_num, NA), next_row_num = parts + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(parts %in% row_num)) %>% 
  select(-parts,-row_num, -next_row_num) %>% #removing rows that mention part number and the row after that 
  
  mutate(parts = if_else(book == "Catriona", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(parts = if_else(!is.na(parts), row_num, NA), next_row_num = parts + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(parts %in% row_num)) %>% 
  select(-parts,-row_num, -next_row_num) %>% #removing rows that mention part number and the row after that 
  
  mutate(chapters = if_else(book == "Treasure Island", str_extract(text, "^chapter [ivxlcdm]+"),NA), row_num = row_number()) %>% 
  mutate(chapters = if_else(!is.na(chapters), row_num, NA), next_row_num = chapters + 1 ) %>% 
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(chapters %in% row_num)) %>% 
  select(-chapters, -row_num, -next_row_num) %>% #removing rows that mention chapter number and the row after that
  
  mutate(chapters_catriona = if_else(book == "Catriona", str_extract(text, "^chapter [ivxlcdm]+"),NA), row_num = row_number()) %>% 
  mutate(chapters_catriona = if_else(!is.na(chapters_catriona), row_num, NA), next_row_num = chapters_catriona + 1 ) %>% 
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(chapters_catriona %in% row_num)) %>% 
  select(-chapters_catriona,-next_row_num, -row_num, -next_row_num) %>%
  
  mutate(text = str_replace_all(text, c("abody" = "everybody", "athegether" = "altogether", "athing" = "anything","\\bken\\b" = "know", 
                                        "\\bkens\\b" = "knows"," apt " = "appropriate"," ane " = "one", " ae " = "one"," ye " = "you", 
                                        " aye " = "yes"," wi " = "with","\\bcapn\\b " = "captain"," em " = "them"," na " = "no", 
                                        " naething " = "nothing"," cam " = "call"," fro " = "from", " een " = "eyes", "nae" = "no", 
                                        "auld" = "old", "\\bgude\\b" = "god")))

cleaned_set
View(cleaned_set)


## Tokenization ----
tokenized_set <- cleaned_set %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = str_trim(word)) %>% 
  count(book, word, sort = TRUE)
tokenized_set
View(tokenized_set)

tidy_books <- cleaned_set %>% 
  unnest_tokens(word, text)
tidy_books 
View(tidy_books)

total_words_in_corpus <- tokenized_set %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words_in_corpus


tokenized_set <- left_join(tokenized_set, total_words_in_corpus)
tokenized_set



## Stop words ----
data(stop_words, package = "tidytext")
#stop_words <- stop_words %>% rename(stopwords = word)
stop_words
View(stop_words)

#adding stopwords to the exsiting stopwords dataset
additional_stopwords <- tibble(word = c("yo", "ho", "im", "ye", "ah", "eh", "em", "oho", "aha", "ay", "ot", "yon", "ere", 
                                        "hes","shes","hae", "twa","wee", "ain", "awa", "ony", "aff"), lexicon = "custom")
additional_stopwords    # check in which book there was more?
final_stopwords_set <- bind_rows(stop_words, additional_stopwords)
final_stopwords_set
#aha denoted exclamation, will be helpful in sentiment analysis 
#h'm is a vocal sound expressing hesitation or thought
#fy - an expression of disgust or disapproval
#ay = yes


stopwords_removed_set <- tokenized_set %>% 
  select(-total) %>% 
  anti_join(final_stopwords_set, by = "word")   #filter(!(word %in% stop_words$stopwords))
stopwords_removed_set
View(stopwords_removed_set)

#weird_words <- stopwords_removed_set %>% 
# filter(word %in% c("abe","aft", "ah","ax", "ay","bog","doo","ebb","eh","em","ere","ese","fir","fro","hed","ho",
#                   "ile","ing", "itt","keg ","mes","nne","noo", "oho","ont","ony","ont","pps","ps","si","tis",
#                  "ud","uns ","ye ","ae","tis","tit","yaw ","wot ","ay", "yo", "ing","aff","aha","ain","ane",
#                 "apt","ava","awa","aye","ca","cam","dae","db","dhu","ee","een","eh","ere","esk","fa",
#                "fro","fu","fy","gae","gey","gie","goo","hm","ha","hae","het","ho","jee","kep","kye",
#               "la","mar","na","nae","neb","ot","oig","ony","ou","pe","ps","pu","quo","rei","sae","sax","sib",
#              "sic","sma","ta","tae","tis","tit","twa","uam","wee","wi","wha","xi","ye","yon","abody","athegether",
#             "athing","aefauld","agog","agricola","aheid","ahint","aince ","ainsel","airn","ajee","allusion",
#            "alpin","amang","anent","anither","anster","ardshiel","askit","awaur","ayont","cad"))

#weird_words
#View(weird_words)

#word_count_after_stopwords_removal <- stopwords_removed_set %>% count(book, word, sort = TRUE) %>% arrange(book) %>% arrange(desc(n)) #arrange(book, word)   #group_by(book, word) %>% summarise(count = n(), .groups = "drop")
#word_count_after_stopwords_removal
#View(word_count_after_stopwords_removal)


## Total words in the corpus ----
total_words_after_stopwords_removal <- stopwords_removed_set %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words_after_stopwords_removal 


word_count_after_stopwords_removal <- left_join(stopwords_removed_set, total_words_after_stopwords_removal)
word_count_after_stopwords_removal

## most common word in the entire corpus
word_count_after_stopwords_removal %>%
  filter(n > 100) %>%
  mutate(word = reorder_within(word,n, book)) %>%
  ggplot(aes(n,word)) +
  geom_col() + 
  labs(y = NULL, title = "Most common words in the corpus after stop words removal")


## Lemmatization ----
lemmatized_words <- word_count_after_stopwords_removal %>% 
  mutate(lemmatized_word = lemmatize_words(word)) #%>% arrange(book,n) #use of function
lemmatized_words  
View(lemmatized_words)


duplicate_rows <- lemmatized_words %>%
  group_by(lemmatized_word,book) %>%  # Group by the 'lemmatized_word' column
  filter(n() > 1) %>% 
  arrange(lemmatized_word)       
duplicate_rows 


after_lemmatization <- duplicate_rows %>% 
  group_by(lemmatized_word, book) %>% 
  summarize(n = sum(n),                  # Sum the 'n' values for each group
            total = first(total),        # Keep the 'total' from the first occurrence of each group
            book = first(book),          # Keep the 'book' from the first occurrence
            word = first(word)) %>%         # Keep the 'word' from the first occurrence
  ungroup() %>% 
  select(-word) %>%
  mutate(lemmatized_word = str_replace_all(lemmatized_word, c("ken" = "know")))
after_lemmatization #%>% filter(lemmatized_word == "capn")
View(after_lemmatization)
# check for the un lemmatozed words

unigrams <- after_lemmatization %>% 
  select(book, lemmatized_word, n, total) %>% 
  mutate(word = lemmatized_word) %>% 
  select(-lemmatized_word)
unigrams
View(unigrams)

treasure_island_tibble <- unigrams %>% 
  filter(book == "Treasure Island") %>% arrange(desc(n))
treasure_island_tibble 
head(treasure_island_tibble, 100)
View(head(treasure_island_tibble, 100))


catriona_tibble <- unigrams %>% 
  filter(book == "Catriona") %>% arrange(desc(n))
catriona_tibble
View(head(catriona_tibble, 100))


## Set graph theme ----
theme_set(theme_bw())


## Wordclouds ----
set.seed(1234)

### after removing stop words and after implementing lemmatization
## Treasure Island wordcloud  color =  factor(sample.int(3, nrow(head(treasure_island_tibble, 100)), replace = TRUE))
ggplot(head(treasure_island_tibble, 75), aes(label = word, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()


## Catriona wordcloud
ggplot(head(catriona_tibble, 75), aes(label = word, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()


## Removing character names ----
treasure_island_without_characters <- treasure_island_tibble %>% 
  filter(!word %in% c('jim', 'hawkins', 'billy', 'bones','black','dog', 'squire', 'trelawney','doctor', 'livesey','captain', 'smollett',
                      'john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth')) %>% arrange(desc(n))
treasure_island_without_characters
View(head(treasure_island_without_characters,100))


catriona_without_characters <- catriona_tibble %>%
  filter(!word %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange',
                      'james','glens','macgregor','drummond','duke','argyll','simon','fraser','prophet',
                      'peden','hugh','palliser')) %>% arrange(desc(n))
catriona_without_characters
View(head(catriona_without_characters,100))


## Wordclouds after removing character names ----
ggplot(head(treasure_island_without_characters, 75), aes(label = word, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()


ggplot(head(catriona_without_characters, 75), aes(label = word, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) +
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()



## Bigrams ----
bigrams <- cleaned_set %>%      
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% final_stopwords_set$word) %>%
  filter(!word2 %in% final_stopwords_set$word) %>%
  mutate(word1 = lemmatize_words(word1),word2 = lemmatize_words(word2)) %>%
  count(word1, word2, book, sort = TRUE) 
bigrams    # this does better lemmatization of the bigrams
View(bigrams)

#bigrams   View(bigrams)


united_bigrams <- bigrams %>% 
  unite(bigram, word1, word2, sep = " ") 
united_bigrams

bigrams_treasure_island <- united_bigrams %>% 
  filter(book == "Treasure Island") 
bigrams_treasure_island
View(head(bigrams_treasure_island,100))

bigrams_catriona <- united_bigrams %>% 
  filter(book == "Catriona") 
bigrams_catriona
View(head(bigrams_catriona,50))


## Wordclouds for bigrams
ggplot(head(bigrams_treasure_island, 50), aes(label = bigram, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()
#ran did not change to run even after lemmatization

ggplot(head(bigrams_catriona, 50), aes(label = bigram, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()

## Removing character names from the corpus ----
bigrams_treasure_island_nocharacters <- bigrams_treasure_island %>% 
  filter(!bigram %in% c('jim hawkins','billy bone','black dog','squire trelawney','doctor livesey','captain smollett', 'flint ship', 'tom morgan','return silver','cry silver',
                        'john silver','ben gunn','benjamin gunn','abraham gray','israel hands','captain flint','tom redruth','mate bill','job anderson','answer morgan')) %>% arrange(desc(n))
bigrams_treasure_island_nocharacters
View(head(bigrams_treasure_island_nocharacters,100))


bigrams_catriona_nocharacters <- bigrams_catriona %>%
  filter(!bigram %in% c('captain sing','miss grant','david balfour','miss drummond','alan breck','captain sang','james stewart','lord advocate','captain pallister','alan button','tod lapraik',
                        'cry alan','cry stewart','sheriff miller','simon fraser','tam dale','charles stewart', 'father james','james mores','king george','lady grange','lady allardyce',
                        'miss barbara','mistress grant','poor james','alan air','captain palliser','catriona drummond','charlie stewart','catriona thief','catriona return','andie scougal',
                        'andie dale','alison hastie','alias james','alan look','alan cry','dear annie','dauvit balfour','cry andie','cry robin','catriona father','arm david',
                        'drummond alias', 'drummond colour','drummond ogilvy','advocate grant','prince charlie','mungo campbell')) %>% arrange(desc(n))
bigrams_catriona_nocharacters
View(head(bigrams_catriona_nocharacters,100))

##Need to clean these more


## Wordcloud for bigrams without character names ----
ggplot(head(bigrams_treasure_island_nocharacters, 50), aes(label = bigram, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) +   
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()

ggplot(head(bigrams_catriona_nocharacters, 50), aes(label = bigram, size = n)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10, trans = power_trans(1/.7)) + 
  #scale_color_gradient(low = "pink", high = "red") + 
  theme_minimal()



## tf_idf of unigrams, bigrams, and their bar graph ----

#### after stopwords removal, after lemmatization
unigram_tf_idf <- unigrams %>% 
  bind_tf_idf(word, book, n) %>% 
  select(-total) %>% arrange(desc(tf_idf))
unigram_tf_idf 


unigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 25) %>%   #takes top 15 
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL, title = "tf-idf of unigrams")


bigrams_tf_idf <- united_bigrams %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))
bigrams_tf_idf

bigrams_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL, title = "tf-idf of bigrams")


## tf-idf after removing character names 
unigrams_tf_idf_without_characters <- unigrams %>% 
  filter(!word %in% c('jim', 'hawkins', 'billy', 'bones','black','dog', 'squire', 'trelawney','doctor', 'livesey','captain', 'smollett',
                      'john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth','david','balfour','alan','breck',
                      'stewart','lord','william','grant','prestongrange','james','glens','macgregor','drummond','duke','argyll','simon',
                      'fraser','prophet','peden','hugh','palliser')) %>%
  bind_tf_idf(word, book, n) %>% 
  select(-total) %>% arrange(desc(tf_idf))
## here add character names from catriona too 
unigrams_tf_idf_without_characters


unigrams_tf_idf_without_characters %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL, title = "tf-idf of unigrams without character names")


bigrams_tf_idf_without_characters <- united_bigrams %>% 
  filter(!bigram %in% c('jim hawkins','billy bone','black dog','squire trelawney','doctor livesey','captain smollett', 'flint ship', 'tom morgan','return silver','cry silver',
                        'john silver','ben gunn','benjamin gunn','abraham gray','israel hands','captain flint','tom redruth','mate bill','job anderson','answer morgan',
                        'captain sing','miss grant','david balfour','miss drummond','alan breck','captain sang','james stewart','lord advocate','captain pallister',
                        'alan button','tod lapraik','cry alan','cry stewart','sheriff miller','simon fraser','tam dale','charles stewart', 'father james','james mores',
                        'king george','lady grange','lady allardyce','miss barbara','mistress grant','poor james','alan air','captain palliser','catriona drummond',
                        'charlie stewart','catriona thief','catriona return','andie scougal','andie dale','alison hastie','alias james','alan look','alan cry','dear annie',
                        'dauvit balfour','cry andie','cry robin','catriona father','arm david','drummond alias', 'drummond colour','drummond ogilvy','advocate grant',
                        'prince charlie','mungo campbell','barbara grant')) %>%
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))
bigrams_tf_idf_without_characters

bigrams_tf_idf_without_characters %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL, title = "tf-idf of bigrams without character names")


# in catriona : what is solan goose? plirig letter? nane sae? mill lade? lee lane?


### TOPIC MODELLING ----
books_dtm <- unigrams %>%  # this data is after stopwords removal and after lemmatization 
  cast_dtm(book, word,n)    #cast_dtm creates a document-term matrix from the dataset, each row is book, column represents word, n = frequency of each word in each book
books_dtm 

values_for_books <- FindTopicsNumber(       # determine optimal number of topics for LDA model
  books_dtm,
  topics = seq(1, 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009","Arun2010","Deveaud2014"),
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL 
)

FindTopicsNumber_plot(values_for_books)
#Griffith : higher value indicates better 
#Cao : lower value is better 
#Arun : lower value is better 
#Deveaud : higher value is better
#Based on the graph,  8  is optimal or 7 or 6 

books_lda <- LDA(books_dtm, k = 6, control = list(seed = 1234))
books_lda
book_topics <- tidy(books_lda, matrix = "beta")    # probability of each word belonging to each topic
book_topics  
#this turned the model into a one-topic-per-term-per-row format. Eg: the term 'ability' has a 0.0000613 probability of being generated from topic 1
#LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms <- book_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%     # get top 10 words for each topic
  ungroup() %>%
  arrange(topic, -beta)
book_topics_top_terms


book_topics_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()


#beta_wide <- book_topics %>% 
#  mutate(topic = paste0("topic", topic)) %>%
#  pivot_wider(names_from = topic, values_from = beta) %>%
#  filter(topic1 > 0.001 | topic2 > 0.001) %>%
# mutate(log_ratio = log2(topic2/topic1))
#beta_wide  

#Now visualising the words which have the greatest differences between the two topics



#Per-document-per-topic probabilities -> this could be applied to check which parts belong to which book
#books_documents <- tidy(books_lda, matrix = "gamma")
#books_documents


# topic modelling based on each part of each book (Lemmatization isn't done for this method)
books_words <- cleaned_set %>%
  unnest_tokens(word, text) %>%
  anti_join(final_stopwords_set) %>%
  count(part, word, book, sort = TRUE)
books_words

book_dtm_ti <- books_words %>%
  filter(book == "Treasure Island") %>%
  cast_dtm(part, word, n)
book_dtm_ti

values_for_ti <- FindTopicsNumber(
  book_dtm_ti,
  topics = seq(1, 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009","Arun2010","Deveaud2014"),
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL 
)
FindTopicsNumber_plot(values_for_ti)
#maybe 6 or 7 topics

book_dtm_c <- books_words %>%
  filter(book == "Catriona") %>%
  cast_dtm(part, word, n)
book_dtm_c

values_for_c <- FindTopicsNumber(
  book_dtm_c,
  topics = seq(1, 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009","Arun2010","Deveaud2014"),
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL 
)

FindTopicsNumber_plot(values_for_c)
# 5 topics seem to be sensible, 7 or 8 


book_lda_ti <- LDA(book_dtm_ti, k = 6, control = list(seed = 1234))
book_lda_ti

book_lda_c <- LDA(book_dtm_c, k = 4, control = list(seed = 1234))
book_lda_c

part_topics_ti <- tidy(book_lda_ti, matrix = "beta")
part_topics_ti

part_topics_c <- tidy(book_lda_c, matrix = "beta")
part_topics_c

top_words_ti <- part_topics_ti %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_words_ti

top_words_c <- part_topics_c %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_words_c

top_words_ti_vis <- top_words_ti %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()+
  labs(title = "Visualisation of top words in Treasure Island based on parts")
top_words_ti_vis 


# topic modelling on all 9 parts together

top_words_c_vis <- top_words_c %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()+
  labs(title = "Visualisation of top words in Catriona based on parts")
top_words_c_vis


## LDA on total parts of both books together
books_words <- cleaned_set %>%
  unnest_tokens(word, text) %>%
  anti_join(final_stopwords_set) %>%
  count(part, word, book, sort = TRUE)
books_words

book_dtm <- books_words %>%
  cast_dtm(part, word, n)
book_dtm

values_for_book_dtm <- FindTopicsNumber(
  book_dtm,
  topics = seq(1, 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009","Arun2010","Deveaud2014"),
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL 
)
FindTopicsNumber_plot(values_for_book_dtm)

parts_lda <- LDA(book_dtm, k = 6, control = list(seed = 1234))
parts_lda

part_topics <- tidy(parts_lda, matrix = "beta")
part_topics

top_words_parts <- part_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_words_parts


top_words_vis <- top_words_parts %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()+
  labs(title = "Visualisation of top words in both books based on parts")
top_words_vis 






## tokenize each book into single sentences. 

## plot wordcloud showing negative and positive words for each novel after removing character names 
## implement this: https://rpubs.com/OrliKhaim/DATA607_Week_10
## and then do the same for sentences of the books too.

## Writing data to excel sheet
sets <- list(
  "bigrams" = bigrams,
  "bigrams_then_lemmatize" = bigrams_then_lemmatize
  # "Cleaned_Set" = cleaned_set,
  #"lemmatized_words" = lemmatized_words,
  #"after_lemmatization" = after_lemmatization
  #"Word_Count_Before_Stemming" = word_count_before_stemming,
  #"Word_Count_After_Stemming" = word_count_after_stemming,
  #"Word_Count_After_Lemmatization" = word_count_after_lemmatization,
  #"treasure_without_characters" = treasure_island_without_characters
  #"word_count_after_stopwords" = word_count_after_stopwords_removal,
  #"lemmatized_words" = lemmatized_words
)


# Write all tibbles into the same Excel file with different sheets
write_xlsx(sets, path = "Data_Cleaning.xlsx")




