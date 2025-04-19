
# Removing the conversational texts from the corpus
treasure_island_text_string <- cleaned_set_step1 %>%
  filter(book == "Treasure Island") %>%
  pull(text) %>%  # Pull the 'text' column
  paste(collapse = " ") 
#treasure_island_text_string

catriona_text_string <- cleaned_set_step1 %>%
  filter(book == "Catriona") %>%
  pull(text) %>%
  paste(collapse = " ")
#catriona_text_string

treasure_island_conversation_removed <-  gsub('".*?"', '', treasure_island_text_string)

unique_words_in_treasure_island <- treasure_island_conversation_removed %>%
  unique() %>%
  tibble(text = .) %>% 
  unnest_tokens(word, text) %>%
  select(word) %>%
  filter(!word %in% final_stopwords_set$word) %>%
  filter(!word %in% c('jim', 'hawkins', 'billy', 'bones','black','dog', 'squire', 'trelawney','doctor', 'livesey',
                      'captain', 'smollett','john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth')) %>%
  count(word, sort = TRUE) %>%
  arrange(n)
#unique_words_in_treasure_island

### Shortcomings : we filtered out character names but words like silver, bones, hands could also mean something else than a name, so this might change other dynamics

catriona_conversation_removed <- gsub('".*?"', '', catriona_text_string)

unique_words_in_catriona <- catriona_conversation_removed %>%
  unique() %>%
  tibble(text = .) %>%
  unnest_tokens(word, text) %>%
  select(word) %>%
  filter(!word %in% final_stopwords_set$word) %>%
  filter(!word %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange','james','glens',
                      'macgregor','drummond','duke','argyll','simon','fraser','prophet','peden','hugh','palliser', "lovat",
                      "doig", "ogilvy","bohaldie")) %>%
  count(word, sort = TRUE) %>%
  arrange(n) 
#unique_words_in_catriona


common_unique_words <- unique_words_in_treasure_island %>% filter(n == 1) %>%
  inner_join(
    unique_words_in_catriona %>% filter(n == 1), 
    by = "word", suffix = c("_treasure_island", "_catriona"))
#common_unique_words





# compare which book had more of which english words: easy words, difficult words, english words that were used before 1700 or after 1700
#ngram_data <- ngram('ship', year_start = 1700) %>% arrange(desc(Frequency))
#ngram_data
