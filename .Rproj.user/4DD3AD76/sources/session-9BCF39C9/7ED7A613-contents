## Word frequencies 
# Unigrams ----
unigrams <- dataset_after_lemmatization %>% 
  select(book, lemmatized_word, n, total) %>% 
  mutate(word = lemmatized_word) %>% 
  select(-lemmatized_word)

treasure_island_unigram <- unigrams %>% 
  filter(book == "Treasure Island") %>% arrange(desc(n))

catriona_unigram <- unigrams %>% 
  filter(book == "Catriona") %>% arrange(desc(n))




## Set graph theme ----
theme_set(theme_bw())




## Wordclouds ----
set.seed(1234)

# Extracting top 50 unigrams for Treasure Island
treasure_island_unigram_top <- head(treasure_island_unigram, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

# Treasure Island unigrams wordcloud
treasure_island_wordcloud <- ggplot(treasure_island_unigram_top, aes(label = word, size = n, color = color_group)) + 
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.65)) + 
  scale_color_manual(values = c("group1" = "#c8775d", 
                                "group2" = "#866042", 
                                "group3" = "#bd701e")) +
  theme_minimal() +
  theme(legend.position = "none")
treasure_island_wordcloud


# Extracting top 50 unigrams for Catriona
catriona_unigram_top <- head(catriona_unigram, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

# Catriona unigrams wordcloud
catriona_wordcloud <- ggplot(head(catriona_unigram_top, 50), aes(label = word, size = n, color = color_group)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  scale_color_manual(values = c("group1" = "#657b9e", 
                                "group2" = "#5e93cf", 
                                "group3" = "#223692")) +
  theme_minimal()





## Removing character names ----
treasure_island_without_characters_unigrams <- treasure_island_unigram %>% 
  filter(!word %in% c('jim', 'hawkins', 'billy', 'bones','black','dog', 'squire', 'trelawney','doctor', 'livesey','captain', 'smollett',
                      'john', 'silver','ben', 'gunn','pew','israel', 'hands','flint','tom', 'redruth', 'george','merry')) %>% arrange(desc(n))

catriona_without_characters_unigrams <- catriona_unigram %>%
  filter(!word %in% c('david','balfour','alan','breck','stewart','lord','william','grant','prestongrange',
                      'james','glens','macgregor','drummond','duke','argyll','simon','fraser','prophet',
                      'peden','hugh','palliser')) %>% arrange(desc(n))




## Wordclouds after removing character names ----
# Treasure Island unigrams without characters
treasure_island_unigram_no_characters <- head(treasure_island_without_characters_unigrams, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

treasure_island_no_characters_wordcloud <- ggplot(head(treasure_island_unigram_no_characters, 50), aes(label = word, size = n, color = color_group)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  scale_color_manual(values = c("group1" = "#c8775d", 
                                "group2" = "#866042", 
                                "group3" = "#bd701e")) +
  theme_minimal()



# Catriona unigrams without characters
catriona_unigram_no_characters <- head(catriona_without_characters_unigrams, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

catriona_no_characters_wordcloud <- ggplot(head(catriona_unigram_no_characters, 50), aes(label = word, size = n, color = color_group)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) +
  scale_color_manual(values = c("group1" = "#657b9e", 
                                "group2" = "#5e93cf", 
                                "group3" = "#223692")) +
  theme_minimal()




## Bigrams ----
bigrams <- cleaned_set %>%      
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  mutate(word1 = lemmatize_words(word1), word2 = lemmatize_words(word2)) %>%
  filter(!(word1 %in% final_stopwords_set$word), !(word2 %in% final_stopwords_set$word)) %>%
  count(word1, word2, book, sort = TRUE) 


# Separate and united bigrams for each book
bigrams_treasure_island <- bigrams %>% 
  filter(book == "Treasure Island") 

bigrams_treasure_island_united <- bigrams_treasure_island %>% 
  unite(bigram, word1, word2, sep = " ")


bigrams_catriona <- bigrams %>% 
  filter(book == "Catriona") 

bigrams_catriona_united <- bigrams_catriona %>% 
  unite(bigram, word1, word2, sep = " ")



# Treasure Island bigrams wordcloud
treasure_island_bigrams <- head(bigrams_treasure_island_united, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

treasure_island_bigrams_wordcloud <- ggplot(treasure_island_bigrams, aes(label = bigram, size = n, color = color_group)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  scale_color_manual(values = c("group1" = "#c8775d", 
                                "group2" = "#866042", 
                                "group3" = "#bd701e")) +
  theme_minimal()


# Catriona bigrams wordcloud
catriona_bigrams <- head(bigrams_catriona_united, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

catriona_bigrams_wordcloud <- ggplot(catriona_bigrams, aes(label = bigram, size = n, color = color_group)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) + 
  scale_color_manual(values = c("group1" = "#657b9e", 
                                "group2" = "#5e93cf", 
                                "group3" = "#223692")) +
  theme_minimal()




# Removing character names from bigrams
bigrams_without_characters <- bigrams %>% 
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
  )

# Separating bigrams set for each book
bigrams_treasure_island_without_characters <- bigrams_without_characters %>% 
  filter(book == "Treasure Island") %>% 
  unite(bigram, word1, word2, sep = " ")

bigrams_catriona_without_characters <- bigrams_without_characters %>% 
  filter(book == "Catriona") %>% 
  filter(!word1 %in% c('1','2','3','4','5')) %>% 
  unite(bigram, word1, word2, sep = " ")



# Wordcloud for bigrams without character names 
# For Treasure Island
treasure_island_bigrams_no_characters <- head(bigrams_treasure_island_without_characters, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

treasure_island_bigrams_no_characters_wordcloud <- ggplot(treasure_island_bigrams_no_characters, 
                                                          aes(label = bigram, size = n, color = color_group)) +   
  geom_text_wordcloud() + 
  scale_size_area(max_size = 20, trans = power_trans(1/.7)) +   
  scale_color_manual(values = c("group1" = "#c8775d", 
                                "group2" = "#866042", 
                                "group3" = "#bd701e")) +
  theme_minimal()



# For Catriona
catriona_bigrams_no_characters <- head(bigrams_catriona_without_characters, 50) %>%
  mutate(color_group = rep(c("group1", "group2", "group3"), length.out = n()))

catriona_bigrams_no_characters_wordcloud <- ggplot(catriona_bigrams_no_characters, 
                                                   aes(label = bigram, size = n, color = color_group)) +  # the value 3 is the number of colors used in the wordcloud
  geom_text_wordcloud() + 
  scale_size_area(max_size = 10, trans = power_trans(1/.7)) + 
  scale_color_manual(values = c("group1" = "#657b9e", 
                                "group2" = "#5e93cf", 
                                "group3" = "#223692")) +
  theme_minimal()




# List of scottish english terms used in the corpus ----
scottish_english <- c("a'body","a'thegether","a'thing","ken","kens","apt","ane","ae","ye","aye","wi","cap'n","em","na",
                      "naething","cam","fro","een","nae","auld","gude","em","uns",
                      "aha","ay","ot","yon","ere","hae","twa","wee", "ain","awa","ony","aff","wot","ese","wha",
                      "ud","si","ps","pps","abe","aft","ax","ay","bog","ebb","doo","eh","em","ere","ont","een","ho",
                      "fir","fro","hed","ile","ing","itt","keg","mes","nne","noo","oho","ont","ony","ee","het","pu",
                      "aff","ain","ane","apt","ava","awa","ay","aye","ca","cam","dae","db","dhu","ps","quo","pe",
                      "eh","ere","esk","fa","fro","fu","fy","gae","gey","gie","goo","h'm","ha","hae","jee","kep","kye",
                      "la","mar","na","neb","o't","oig","ony","ou","rei","sae","sax","sib","sic","sma","ta","ye",
                      "tae","tis","tit","twa","uam","wee","wi","yon","unco", "frae", "brae","wouldnae","didnae", "hadnae",
                      "couldnae", "wasnae", "isnae", "quo'", "whaur", "ither", "thon", "fash", "likit", "knowe", "fecht",
                      "fyke", "weemen", "ajee", "aweel","sae","nane")


#Detecting the scottish english terms in the corpus and visualising it 
scottish_english_detection <-  tokenized_set_draft %>% 
  filter(word %in% scottish_english) %>%
  group_by(book, word) %>%
  summarise(total_count = sum(n)) %>%
  arrange(desc(total_count))


scottish_english_words_visualisations <- ggplot(head(scottish_english_detection,50), aes(x = reorder(word, total_count), y = total_count, fill = book)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~book, scales = "free_y") + # creates separate plots for each book
  theme_minimal() +
  labs(title = "Word counts for each book", x = "Words", y = "Total count") +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 9),         
    axis.title = element_text(size = 9),         
    axis.text = element_text(size = 8),           
    legend.title = element_text(size = 8),       
    legend.text = element_text(size = 8)
  ) + 
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d"))  



#Extracting the common scottish english words in both books
common_scottish_english_words <- scottish_english_detection %>%
  group_by(word) %>%
  filter(n_distinct(book) > 1) %>%
  ungroup() %>%
  arrange(word)

word_summary <- common_scottish_english_words %>%
  group_by(word, book) %>%
  summarise(count = sum(total_count)) %>%
  ungroup()

#Plotting the common scottish words with it's count for each book
common_scottish_english_words_visualisation <- ggplot(common_scottish_english_words, aes(x = word, y = total_count, fill = book)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Common Scottish english terms in Catriona vs Treasure Island", x = "Word", y = "Total Count") +
  scale_fill_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) +
  theme_minimal() 



