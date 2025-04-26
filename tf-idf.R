## TF-IDF of unigrams, bigrams, and their bar graph ----

# Calculating tf-idf of unigrams
unigram_tf_idf <- unigrams %>% 
  bind_tf_idf(word, book, n) %>% 
  select(-total) %>% arrange(desc(tf_idf))

# Visualisation of tf-idf of unigrams
unigrams_tf_idf_visualisation <- unigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 25, with_ties = FALSE) %>%   #takes top 25 
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) 



# Calculating tf-idf of bigrams
bigrams_tf_idf <- united_bigrams %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))

# Visualisation of tf-idf of bigrams 
bigrams_tf_idf_visualisation <- bigrams_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 25, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d"))



## Calculating tf-idf after removing character names 
unigrams_tf_idf_without_characters <- unigrams %>% 
  filter(!word %in% c('jim', 'hawkins', 'billy', 'bones','black','dog', 'squire', 'trelawney','doctor', 'livesey','captain', 'smollett',
                      'john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth','david','balfour','alan','breck',
                      'stewart','lord','william','grant','prestongrange','james','glens','macgregor','drummond','duke','argyll','simon',
                      'fraser','prophet','peden','hugh','palliser')) %>%
  bind_tf_idf(word, book, n) %>% 
  select(-total) %>% arrange(desc(tf_idf))


# Visualisation of tf-idf of unigrams after removing character names
unigrams_tf_idf_without_characters_visualisation <- unigrams_tf_idf_without_characters %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 25, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d"))



# Calculating tf-idf of bigrams after removing character names 
bigrams_tf_idf_without_characters <- bigrams %>% 
  filter(
    !word1 %in% c('jim', 'hawkins', 'billy', 'bones','bone', 'black','dog', 'squire', 'trelawney','doctor', 'livesey','captain', 'smollett','tom','morgan','benjamin',
                  'abraham','hands','hand','redruth','bill','anderson','john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth', 'george','merry') &
      
      !word2 %in% c('jim', 'hawkins', 'billy', 'bones','bone', 'black','dog', 'squire', 'trelawney','doctor', 'livesey','captain', 'smollett','tom','morgan','benjamin',
                    'abraham','hands','hand','redruth','bill','anderson','john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth', 'george','merry') &
      
      !word1 %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange','captain','sing','grant','sang','advocate','tod','lapraik',
                    'miller','sheriff','tam','dale','charles','james','glens','macgregor','drummond','duke','argyll','simon','fraser','prophet','george','grange','allardyce',
                    'barbara','catriona','charlie','andie','alison','annie','dauvit','robin','campbell', 'peden','hugh','palliser') &
      
      !word2 %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange','captain','sing','grant','sang','advocate','tod','lapraik',
                    'miller','sheriff','tam','dale','charles','james','glens','macgregor','drummond','duke','argyll','simon','fraser','prophet','george','grange','allardyce',
                    'barbara','catriona','charlie','andie','alison','annie','dauvit','robin','campbell', 'peden','hugh','palliser')
  ) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))


# Visualisation of if-idf of bigrams after removing character names
bigrams_tf_idf_without_characters_visualisation <- bigrams_tf_idf_without_characters %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 25, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d"))

