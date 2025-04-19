#gutenbergWorks <- gutenberg_works(title == "Treasure Island")
#gutenbergWorks
#books_by_stevenson <- gutenberg_works(author == "Stevenson, Robert Louis") #%>% select(title, author, rights)  #to-do : check when each of the books were published
#books_by_stevenson

#treasure_island_novel <- gutenberg_download(27780,  mirror = "https://www.gutenberg.org/") #, this function downloads the book in plain text format that is txt file
#treasure_island_novel <- treasure_island_novel %>% mutate(line = 1:n()) %>% select(line, text)
#treasure_island_novel

#catriona_novel <- gutenberg_download(30870, mirror = "https://www.gutenberg.org/")
#catriona_novel


#stemmed_words <- lemmatized_words %>% mutate(stemmed_word = wordStem(word, language = "en"))
#stemmed_words
#View(stemmed_words)

#word_count_after_stemming <- stemmed_words %>% group_by(book, stemmed_word) %>% summarise(count = n(), .groups = "drop")
#word_count_after_stemming
#View(word_count_after_stemming)

#freq_by_rank <- stemmed_words %>% group_by(book) %>% mutate(rank = row_number(), term_frequency = n/total) %>% ungroup()
#freq_by_rank

#words_tibble <- stemmed_words %>% select(book, stemmed_word, n, total) %>% mutate(word = stemmed_word) %>% select(-stemmed_word)
#words_tibble



# Keep your data as a single string, clean it, 
[[:punct:]]+ -> this is for all punctuations 
[[:digit::]]+ -> this is for all numbers 



#Visualisation 
book_tf_idf %>% group_by(book) %>% slice_max(tf_idf, n = 25) %>% ungroup() %>% ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) + geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free") + labs(x = "tf-idf", y = NULL)



#Wordcloud
treasure_island_tibble <- book_tf_idf %>% filter(book == "Treasure Island")
treasure_island_tibble 

catriona_tibble <- book_tf_idf %>% filter(book == "Catriona")
catriona_tibble


treasure_island_wordcloud_palette <- brewer.pal(8,"Dark2")   #8 colors can be selected from palette Dark2
wordcloud(treasure_island_tibble$word, treasure_island_tibble$tf_idf, max.words = 50, min.freq = 0.0005, random.order = FALSE, colors = treasure_island_wordcloud_palette, font = 2, scale = c(3, 0.5))
#words with higher tf_idf will appear larger, random.order = FALSE -> the words will be arranged in descending order of importance, words with highest tf_idf will appear in the center, c(3,0.5) = (font more largest tf_idf, font for smallest tf_idf)

catriona_wordcloud_palette <- brewer.pal(8,"Dark2")
wordcloud(catriona_tibble$word, catriona_tibble$tf_idf, max.words = 50, min.freq = 0.0005, random.order = FALSE, colors = catriona_wordcloud_palette, font = 2 )

check <- bigrams %>% filter(book == 'Catriona') %>%
  filter(word1 %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange',
                      'james','glens','macgregor','drummond','duke','argyll','simon','fraser','prophet',
                      'peden','hugh','palliser') | word2 %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange',
                                                                'james','glens','macgregor','drummond','duke','argyll','simon','fraser','prophet',
                                                                'peden','hugh','palliser') )
check
View(check)



## Bigrams with stop words ----
bigrams_with_stopwords <- cleaned_set %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, book, sort = TRUE)
bigrams_with_stopwords




novels_with_part <- books %>% 
  mutate(text = tolower(text)) %>% 
  group_by(gutenberg_id) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [ivxlcdm]+$"))), 
         part = cumsum(str_detect(text, regex("^part [ivxlcdm]+$")))) %>% 
  ungroup()
novels_with_part
View(novels_with_part)


## Data cleaning ----
cleaned_topic_modelling <- novels_with_part %>% 
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
  
  mutate(text = str_replace_all(text, c("abody" = "everybody", "athegether" = "altogether", 
                                        "athing" = "anything"," apt " = "appropriate"," ane " = "one", " ae " = "one",
                                        " ye " = "you", " aye " = "yes"," wi " = "with"," capn " = "captain","naething" = "nothing",
                                        " em " = "them"," na " = "no", " cam " = "call"," fro " = "from", " een " = "eyes", " nae " = "no")))
cleaned_topic_modelling
View(cleaned_topic_modelling)
cleaned_topic_modelling %>% filter(book == "Catriona") 




titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds",
            "Pride and Prejudice", 
            "Great Expectations")
library(gutenbergr)

book <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")
book

by_chapter <- book %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

by_chapter








############################# the following codes are incomplete, there is work to be done here #############################
####### SENTIMENT ANALYSIS ----
afinn <- get_sentiments("afinn")
afinn
View(afinn)
afinn %>% arrange(desc(value))
afinn %>% arrange(value)

#test
not_words <- bigrams_with_stopwords %>% filter(word1 == "not") %>% inner_join(afinn, by = c(word2 = "word")) %>% count(word2, value, sort = TRUE) %>% arrange(desc(n))
not_words    
#here, the most common sentiment-associated word to follow "not" was "afraid" which would typically have a score - 2


##What common words follow each particular negation ----
negation_words <- c("not","no","never","without","none","nobody","nor","nowhere","neither")

#Most common positive or negative words to follow negation_words
negated_words_treasure_island <- bigrams_with_stopwords %>% 
  filter(book == "Treasure Island") %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(afinn, by = c(word2 = "word")) %>% 
  count(word2, value, sort = TRUE) %>% 
  arrange(desc(n))
negated_words_treasure_island

negated_words_treasure_island %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"negation words\"",
       title = "Most common words associated with the negation words for Treasure Island")


negated_words_catriona <- bigrams_with_stopwords %>%
  filter(book == "Catriona") %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(afinn, by = c(word2 = "word")) %>% 
  count(word2, value, sort = TRUE) %>% 
  arrange(desc(n))
negated_words_catriona

negated_words_catriona %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"negation words\"",
       title = "Most common words associated with the negation words for Catriona")




bing <- get_sentiments("bing") 
bing
bing_sentiment <- tokenized_set %>%
  filter(book == "Catriona") %>%
  inner_join(bing, by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>%
  arrange(desc(n))
bing_sentiment

## sentiments of the books based on bing dictionary
books_bing_sentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)
books_bing_sentiment

ggplot(books_bing_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free_x")


nrc <- read.table("NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt") 
nrc <- tibble(nrc) %>% rename(word = V1, sentiment = V2, level = V3) %>% filter(sentiment %in% c("positive","negative")) %>% filter(level == 1)
nrc 

##sentiments of the books based on afinn dictionary
treasure_island_afinn <- tidy_books %>%
  filter(book == "Treasure Island") %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "afinn")
treasure_island_afinn

treasure_island_bing_nrc_loughran <- bind_rows(
  tidy_books %>% filter(book == "Treasure Island") %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."), 
  tidy_books %>% filter(book == "Treasure Island") %>%
    inner_join(get_sentiments("loughran") %>% filter(sentiment %in% c("positive","negative"))) %>%
    mutate(method = "loughran")
  #,
  #tidy_books %>% filter(book == "Treasure Island") %>%
  #   inner_join(nrc, by = "word") %>%
  #  mutate(method = "nrc")
) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)
treasure_island_bing_nrc_loughran



#nrc_duplicates <- nrc %>%
# group_by(word) %>%
#filter(n() > 1) %>%
#  select(word, sentiment, level)
#nrc_duplicates 

catriona_afinn <- tidy_books %>%
  filter(book == "Catriona") %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "afinn")
catriona_afinn

catriona_bing_nrc_loughran <- bind_rows(
  tidy_books %>% filter(book == "Catriona") %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."), 
  tidy_books %>% filter(book == "Catriona") %>%
    inner_join(get_sentiments("loughran") %>%
                 filter(sentiment %in% c("positive","negative"))
    ) %>%
    mutate(method = "loughran")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)
catriona_bing_nrc_loughran


#plots of sentiments of each book based on each dictionary
treasure_island_sentiment_plot <- bind_rows(treasure_island_afinn, treasure_island_bing_nrc_loughran) %>%
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_x")+
  labs(title = "Treasure Island")

catriona_sentiment_plot <- bind_rows(catriona_afinn, catriona_bing_nrc_loughran) %>%
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_x")+
  labs(title = "Catriona")

treasure_island_sentiment_plot + catriona_sentiment_plot


## checking why loughran has more negatives than other dictionaries in the plot 
get_sentiments("bing") %>% count(sentiment)
get_sentiments("loughran") %>% filter(sentiment %in% c("positive","negative")) %>% count(sentiment)

## Most common positive and negative words ----

### Treasure Island : 
bing_word_counts_treasure_island <- tidy_books %>% 
  filter(book == "Treasure Island") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts_treasure_island

loughran_word_counts_treasure_island <- tidy_books %>% 
  filter(book == "Treasure Island") %>%
  inner_join(get_sentiments("loughran") %>% filter(sentiment %in% c("positive","negative"))) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
loughran_word_counts_treasure_island

#### Visualisation
bing_word_counts_treasure_island_vis <- bing_word_counts_treasure_island %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL, title = "Treasure Island - Bing") + 
  coord_flip()


loughran_word_counts_treasure_island_vis <- loughran_word_counts_treasure_island %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL, title = "Treasure Island - Loughran") + 
  coord_flip()



### Catriona:
bing_word_counts_catriona <- tidy_books %>% 
  filter(book == "Catriona") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts_catriona

loughran_word_counts_catriona <- tidy_books %>% 
  filter(book == "Catriona") %>%
  inner_join(get_sentiments("loughran") %>% filter(sentiment %in% c("positive","negative"))) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
loughran_word_counts_catriona


#### Visualsation
bing_word_counts_catriona_vis <- bing_word_counts_catriona %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL, title = "Catriona - Bing") + 
  coord_flip()


loughran_word_counts_catriona_vis <- loughran_word_counts_catriona %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL, title = "Catriona - Loughran") + 
  coord_flip()

bing_word_counts_treasure_island_vis + loughran_word_counts_treasure_island_vis + bing_word_counts_catriona_vis + loughran_word_counts_catriona_vis


#get_sentiments("nrc")  
#unlink("~/Library/Caches/textdata/nrc", recursive = TRUE)

#Both lexicons have more negative than positive words, but the ratio of negative to positive
#words is higher in the Loughran lexicon than the Bing lexicon, This will contribute to the effect we see in the plot above
loughran <- get_sentiments("loughran")
unique(loughran$sentiment)

# based on this classification, check which book has more words with negative sentiments, 
# based on all the classification, do the same, and check which book has more 

## Worcloud based on sentiments of Bing dictionary 
tidy_books %>%
  filter(book == "Treasure Island") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("gray80","gray20"),
    max.words = 100
  )

tidy_books %>%
  filter(book == "Catriona") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("gray80","gray20"),
    max.words = 100
  )


#try sentiment analysis on sentences, then chapterwise
novels <- books %>% 
  mutate(text = tolower(text)) %>% 
  group_by(gutenberg_id) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("chapter [ivxlcdm]+")))) %>% 
  ungroup()
novels

data_as_string <- paste(novels$text, collaps = " ")
data_as_string



## Data cleaning ----
cleaned_set_with_fullstop_and_numbers <- novels %>% 
  mutate(book = if_else(gutenberg_id == 30870, "Catriona","Treasure Island")) %>% select(-gutenberg_id) %>%
  filter(!(book == "Catriona" & (linenumber >= 1 & linenumber <= 189 ))) %>%  
  filter(!(book == "Treasure Island" & (linenumber <= 224 | linenumber %in% c(250, 1867, 2306, 3792, 4636, 5749, 7534,7688)))) %>%
  mutate(text = str_trim(text)) %>%
  filter(text != "" & text != "	") %>% #removing empty rows from the tibble
  #mutate(text = str_replace_all(text, "[[:digit:]]+","")) %>% #removing the occurrence of numbers
  mutate(text = str_replace_all(text, "(?<=\\b)[ivxlcdm]+\\.(?=\\s|$)", "")) %>% #removing the occurrence of roman numbers
  mutate(text = str_replace_all(text, '[,!@#$%^&*()/\\[\\]{};:”“<>?\\|`_+~=""\']', "")) %>% #removing the occurrence of symbols -> there could be a few that's missing currently
  mutate(text = str_replace_all(text, "-+", " ")) %>% #replacing the occurrence of hypen and multiple occurrences of hypen with a whitespace\
  filter(!startsWith(text, "illustration")) %>%  #removing illustration
  
  mutate(part = if_else(book == "Treasure Island", str_extract(text, "^part [ivxlcdm]+"), NA), row_num = row_number()) %>% 
  mutate(part = if_else(!is.na(part), row_num, NA), next_row_num = part + 1 ) %>%
  filter(!(row_num %in% next_row_num)) %>% 
  filter(!(part %in% row_num)) %>% 
  select(-part,-row_num, -next_row_num) %>% #removing rows that mention part number and the row after that 
  
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
  
  mutate(text = str_replace_all(text, c("a'body" = "everybody", 
                                        "a'thegether" = "altogether", 
                                        "a'thing" = "anything")))

cleaned_set_with_fullstop_and_numbers

#line = 250, 1867, 2306, 3792, 4636, 5749, 7534, 
sentence_tokenized_set <- cleaned_set_with_fullstop_and_numbers %>% 
  unnest_tokens(sentence, text, token = "sentences")  
#count(book, word, sort = TRUE)
sentence_tokenized_set
View(sentence_tokenized_set)

test <- cleaned_set_with_fullstop_and_numbers %>% unlist(tokenize_sentences(text))
test
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# implement unsupervised sentiment analysis 
# implement topic modelling

#Should we do sentiment analysis after stopwords removal or before?








## Zipf's law ----
freq_by_rank <- tokenized_set %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), term_frequency = n/total) %>% 
  ungroup() #%>% arrange(desc(rank))
freq_by_rank


freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = book)) + 
  geom_line(linewidth = 1.1, alpha = 0.8) + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Rank", y = "Term Freqency") 

values <- lm(log10(term_frequency) ~ log10(rank), data = freq_by_rank)
values

#here slope is close to -1, ideally we want the slope to be -1, which implies that the book follows the standard pattern


#Now plotting the fitted power law
freq_by_rank %>% ggplot(aes(rank, term_frequency, color = book)) + 
  geom_abline(intercept = values$coefficients["(Intercept)"], slope = values$coefficients["log10(rank)"], color = "gray50", linetype = 2) + 
  geom_line(linedisth = 1.1, alpha = 0.8) + 
  scale_x_log10() + 
  scale_y_log10()

#tail(freq_by_rank)


rank_subset <- freq_by_rank %>% 
  filter(rank < 1000, rank > 10)
rank_subset

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)  # slope here is more closer to -1 than the slope we got from the full set


# both books comply with the standard form -> Zipf's law -> rank is inversely proportional to the term frequency




# Topic modelling by parts (total parts of both the books)
books_words <- cleaned_set %>%
  unnest_tokens(word, text) %>%
  anti_join(final_stopwords_set) %>%
  count(part, word, book, sort = TRUE)

# Calculating the dtm matrix
book_dtm <- books_words %>%
  cast_dtm(part, word, n)


# Finding optimal topic number
values_for_parts <- FindTopicsNumber(
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

part_wise_topicmodeling_number_of_topics <- FindTopicsNumber_plot(values_for_parts)
part_wise_topicmodeling_number_of_topics


# Implementing LDA
parts_lda <- LDA(book_dtm, k = 6, control = list(seed = 1234))

part_topics <- tidy(parts_lda, matrix = "beta")

part_topics_top_terms <- part_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%
  ungroup() %>%
  arrange(topic, -beta)


parts_topic_gamma <- tidy(parts_lda, matrix = "gamma")   # gamma = topic proportion per document 
parts_topic_gamma
View(parts_topic_gamma)


#Visualising out of 6 topics we obtained for the corpus, the proportion of topics in each part
parts_topic_gamma %>%
  ggplot(aes(x = document, y = gamma, fill = factor(topic))) +
  geom_col(position = "fill") +
  labs(title = "Proportion of topics in each part",
       x = "Part", y = "Topic proportion", fill = "Topic") +
  theme_minimal() +
  scale_fill_manual(values = c("#cd9fcc", "#82667f", "#7e61ab", "#a1a1ce", "#9656ce", "#313178"))




# Call the function for the second dataset
folder_path_2 <- "top99_words_per_topic_partwise"
latex_tables(part_topics_top_terms, folder_path_2)




citation("gutenbergr")
citation("SnowballC") 
citation("textstem")
citation("textdata") 
citation("tokenizers") 
citation("topicmodels") 
citation("tm") 
citation("ldatuning") 
citation("xtable")
citation("ggwordcloud")
