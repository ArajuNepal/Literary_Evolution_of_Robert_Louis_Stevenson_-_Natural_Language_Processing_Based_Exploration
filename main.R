# Description: Main file for studying differences/similarities between 'Treasure Island' and 'Catriona' using NLP
# Author: Araju Mepal
# Last modified: April 10, 2025


## Load libraries ----
pacman::p_load(gutenbergr, dplyr, tidytext, stringr, SnowballC, textstem, ggplot2, forcats, 
               tidyr, tokenizers, topicmodels, tm, ldatuning, xtable, ggwordcloud)


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



## Linking to other files ----
source("data_cleaning.R")
source("tf-idf.R")
source("word_frequencies.R")
source("topic_modelling.R")


visualisation_folder_path <- "visualisations"
if (!dir.exists(visualisation_folder_path)) {
  dir.create(visualisation_folder_path)
}


## Displaying the outputs ---- 
print(paste("The tokenized set is as follows:"))
tokenized_set


print(paste("The final set of stopwords, that is available stop_words set plus custom stopwords is as follows:"))
final_stopwords_set


print(paste("The set of words after removal of stopwords is as follows:"))
stopwords_removed_set


print(paste("Visualisation of additional stopwords in each of the book:"))
ggsave("visualisations/additional_stopwords_in_each_book_visualisation.eps", plot = additional_stopwords_in_each_book_visualisation, width = 4, height = 4, dpi = 300)
ggsave("visualisations/additional_stopwords_in_each_book_visualisation.jpg", plot = additional_stopwords_in_each_book_visualisation, width = 4, height = 4, dpi = 300)


print(paste("Visualisation of common additional stopwords in each book:"))
ggsave("visualisations/common_additional_stopwords_visualisation.eps", plot = common_additional_stopwords_visualisation, width = 4, height = 4, dpi = 300)
ggsave("visualisations/common_additional_stopwords_visualisation.jpg", plot = common_additional_stopwords_visualisation, width = 4, height = 4, dpi = 300)


print(paste("List of manually identified Scottish English terms from the corpus is as follows:"))
scottish_english_detection
ggsave("visualisations/scottish_english_words_visualisations.eps", plot = scottish_english_words_visualisations, width = 5, height = 7, dpi = 300)
ggsave("visualisations/scottish_english_words_visualisations.jpg", plot = scottish_english_words_visualisations, width = 5, height = 7, dpi = 300)


print(paste("List of common Scottish English words present in both the books:"))
common_scottish_english_words
ggsave("visualisations/common_scottish_english_words_visualisation.eps", plot = common_scottish_english_words_visualisation, width = 5, height = 4, dpi = 300)
ggsave("visualisations/common_scottish_english_words_visualisation.jpg", plot = common_scottish_english_words_visualisation, width = 5, height = 4, dpi = 300)


print(paste("List of Treasure Island unigrams:"))
treasure_island_unigram
ggsave("visualisations/treasure_island_wordcloud.eps", plot = treasure_island_wordcloud, width = 4.5, height = 5, dpi = 300)
ggsave("visualisations/treasure_island_wordcloud.jpg", plot = treasure_island_wordcloud, width = 4.5, height = 5, dpi = 300)


print(paste("List of Catriona unigrams:"))
catriona_unigram
ggsave("visualisations/catriona_wordcloud.eps", plot = catriona_wordcloud, width = 5.5, height = 5.5, dpi = 300)
ggsave("visualisations/catriona_wordcloud.jpg", plot = catriona_wordcloud, width = 5.5, height = 5.5, dpi = 300)


print(paste("List of Treasure Island unigrams without characters:"))
treasure_island_without_characters_unigrams
ggsave("visualisations/treasure_island_no_characters_wordcloud.eps", plot = treasure_island_no_characters_wordcloud, width = 5, height = 4.5, dpi = 300)
ggsave("visualisations/treasure_island_no_characters_wordcloud.jpg", plot = treasure_island_no_characters_wordcloud, width = 5, height = 4.5, dpi = 300)


print(paste("List of Catriona unigrams without characters:"))
catriona_without_characters_unigrams
ggsave("visualisations/catriona_no_characters_wordcloud.eps", plot = catriona_no_characters_wordcloud, width = 6, height = 5.5, dpi = 300)
ggsave("visualisations/catriona_no_characters_wordcloud.jpg", plot = catriona_no_characters_wordcloud, width = 6, height = 5.5, dpi = 300)


print(paste("List of Treasure Island bigrams:"))
bigrams_treasure_island
ggsave("visualisations/treasure_island_bigrams_wordcloud.eps", plot = treasure_island_bigrams_wordcloud, width = 6, height = 6, dpi = 300)
ggsave("visualisations/treasure_island_bigrams_wordcloud.jpg", plot = treasure_island_bigrams_wordcloud, width = 6, height = 6, dpi = 300)


print(paste("List of Catriona bigrams:"))
bigrams_catriona
ggsave("visualisations/catriona_bigrams_wordcloud.eps", plot = catriona_bigrams_wordcloud, width = 5, height = 5, dpi = 300)
ggsave("visualisations/catriona_bigrams_wordcloud.jpg", plot = catriona_bigrams_wordcloud, width = 5, height = 5, dpi = 300)


print(paste("List of Treasure Island bigrams without characters:"))
bigrams_treasure_island_nocharacters
ggsave("visualisations/treasure_island_bigrams_no_characters_wordcloud.eps", plot = treasure_island_bigrams_no_characters_wordcloud, width = 7, height = 6, dpi = 300)
ggsave("visualisations/treasure_island_bigrams_no_characters_wordcloud.jpg", plot = treasure_island_bigrams_no_characters_wordcloud, width = 7, height = 6, dpi = 300)


print(paste("List of Catriona bigrams without characters:"))
bigrams_catriona_nocharacters
ggsave("visualisations/catriona_bigrams_no_characters_wordcloud.eps", plot = catriona_bigrams_no_characters_wordcloud, width = 5, height = 4.5, dpi = 300)
ggsave("visualisations/catriona_bigrams_no_characters_wordcloud.jpg", plot = catriona_bigrams_no_characters_wordcloud, width = 5, height = 4.5, dpi = 300)


print(paste("List of TF-IDF values of unigrams of both books:"))
unigram_tf_idf
ggsave("visualisations/unigrams_tf_idf_visualisation.eps", plot = unigrams_tf_idf_visualisation, width = 6, height = 7.5, dpi = 300)
ggsave("visualisations/unigrams_tf_idf_visualisation.jpg", plot = unigrams_tf_idf_visualisation, width = 6, height = 7.5, dpi = 300)


print(paste("List of TF-IDF values of unigrams of both books without characters:"))
unigrams_tf_idf_without_characters
ggsave("visualisations/unigrams_tf_idf_without_characters_visualisation.eps", plot = unigrams_tf_idf_without_characters_visualisation, width = 6, height = 7.5, dpi = 300)
ggsave("visualisations/unigrams_tf_idf_without_characters_visualisation.jpg", plot = unigrams_tf_idf_without_characters_visualisation, width = 6, height = 7.5, dpi = 300)


print(paste("List of TF-IDF values of bigrams of both books:"))
bigrams_tf_idf
ggsave("visualisations/bigrams_tf_idf_visualisation.eps", plot = bigrams_tf_idf_visualisation, width = 6, height = 7.5, dpi = 300)
ggsave("visualisations/bigrams_tf_idf_visualisation.jpg", plot = bigrams_tf_idf_visualisation, width = 6, height = 7.5, dpi = 300)


print(paste("List of TF-IDF values of bigrams of both books without characters:"))
bigrams_tf_idf_without_characters
ggsave("visualisations/bigrams_tf_idf_without_characters_visualisation.eps", plot = bigrams_tf_idf_without_characters_visualisation, width = 6, height = 7.5, dpi = 300)
ggsave("visualisations/bigrams_tf_idf_without_characters_visualisation.jpg", plot = bigrams_tf_idf_without_characters_visualisation, width = 6, height = 7.5, dpi = 300)


print(paste("Visualisation of graphs to select optimal topic number for topic modelling:"))
postscript("visualisations/book_wise_topicmodeling_number_of_topics.eps", width = 5, height = 5, horizontal = FALSE, onefile = FALSE, paper = "special")
FindTopicsNumber_plot(values_for_books)
dev.off()
ggsave("visualisations/book_wise_topicmodeling_number_of_topics.jpg", plot =book_wise_topicmodeling_number_of_topics , width = 5, height = 5, dpi = 300)



print(paste("Visualisation showing each topic proportion for the books:"))
ggsave("visualisations/proportion_of_each_topic_visualisation.eps", plot = proportion_of_each_topic_visualisation, width = 6, height = 5.5, dpi = 300)
ggsave("visualisations/proportion_of_each_topic_visualisation.jpg", plot = proportion_of_each_topic_visualisation, width = 6, height = 5.5, dpi = 300)





