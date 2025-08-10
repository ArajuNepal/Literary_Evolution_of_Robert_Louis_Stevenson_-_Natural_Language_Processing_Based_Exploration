## Topic modelling ----
# Topic modelling by book
# Calculating the dtm matrix
books_dtm <- unigrams %>%  # This data is after stopwords removal and after lemmatization 
  cast_dtm(book, word,n)    # cast_dtm creates a document-term matrix from the dataset, each row is book, column represents word, n = frequency of each word in each book

# Finding optimal topic number 
values_for_books <- FindTopicsNumber(       # Determine optimal number of topics for LDA model
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

#Griffith : higher value indicates better 
#Cao : lower value is better 
#Arun : lower value is better 
#Deveaud : higher value is better

book_wise_topicmodeling_number_of_topics <- FindTopicsNumber_plot(values_for_books)
book_wise_topicmodeling_number_of_topics



# Topic number = 4

# Implementing LDA
books_lda_4 <- LDA(books_dtm, k = 4, control = list(seed = 1234))

#terms(books_lda_4,5) # you can see top 5 words for each topic
book_topics_4 <- tidy(books_lda_4, matrix = "beta")    # Each row gives the robability(beta) of a word belonging to a specific topic

#This turned the model into a one-topic-per-term-per-row format. LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms_4 <- book_topics_4 %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%     # Get top 99 words for each topic
  ungroup() %>%
  arrange(topic, -beta)

book_topic_gamma_4 <- tidy(books_lda_4, matrix = "gamma")   # Gamma matrix : probability distribution of topics within each book (document) 

# Topic distribution table for topic number 4
topic_distribution_table_4 <- book_topic_gamma_4 %>%
  mutate(gamma = gamma * 100) %>%  # converting to percentage
  pivot_wider(names_from = document, values_from = gamma) %>%  # spreading documents into columns
  arrange(topic) %>% 
  rename(
    `Topic` = topic,
    `Catriona (%)` = `Catriona`,
    `Treasure Island (%)` = `Treasure Island`
  ) %>%
  mutate(
    `Catriona (%)` = if_else(`Catriona (%)` < 0.001, "< 10^-6", sprintf("%.2f", `Catriona (%)`)),
    `Treasure Island (%)` = if_else(`Treasure Island (%)` < 0.001, "< 10^-6", sprintf("%.2f", `Treasure Island (%)`))
  )



# Topic number = 5

# Implementing LDA
books_lda_5 <- LDA(books_dtm, k = 5, control = list(seed = 1234))

#terms(books_lda,5) # you can see top 5 words for each topic
book_topics_5 <- tidy(books_lda_5, matrix = "beta")    # Each row gives the robability(beta) of a word belonging to a specific topic

#This turned the model into a one-topic-per-term-per-row format. LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms_5 <- book_topics_5 %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%     # Get top 99 words for each topic
  ungroup() %>%
  arrange(topic, -beta)

book_topic_gamma_5 <- tidy(books_lda_5, matrix = "gamma")   # Gamma matrix : probability distribution of topics within each book (document) 

# Topic distribution table for topic number 6
topic_distribution_table_5 <- book_topic_gamma_5 %>%
  mutate(gamma = gamma * 100) %>%  # converting to percentage
  pivot_wider(names_from = document, values_from = gamma) %>%  # spreading documents into columns
  arrange(topic) %>% 
  rename(
    `Topic` = topic,
    `Catriona (%)` = `Catriona`,
    `Treasure Island (%)` = `Treasure Island`
  ) %>%
  mutate(
    `Catriona (%)` = if_else(`Catriona (%)` < 0.001, "< 10^-6", sprintf("%.2f", `Catriona (%)`)),
    `Treasure Island (%)` = if_else(`Treasure Island (%)` < 0.001, "< 10^-6", sprintf("%.2f", `Treasure Island (%)`))
  )



# Topic number = 6

# Implementing LDA
books_lda_6 <- LDA(books_dtm, k = 6, control = list(seed = 1234))

#terms(books_lda,5) # you can see top 5 words for each topic
book_topics_6 <- tidy(books_lda_6, matrix = "beta")    # Each row gives the robability(beta) of a word belonging to a specific topic

#This turned the model into a one-topic-per-term-per-row format. LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms_6 <- book_topics_6 %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%     # Get top 99 words for each topic
  ungroup() %>%
  arrange(topic, -beta)

book_topic_gamma_6 <- tidy(books_lda_6, matrix = "gamma")   # Gamma matrix : probability distribution of topics within each book (document) 

# Topic distribution table for topic number 6
topic_distribution_table_6 <- book_topic_gamma_6 %>%
  mutate(gamma = gamma * 100) %>%  # converting to percentage
  pivot_wider(names_from = document, values_from = gamma) %>%  # spreading documents into columns
  arrange(topic) %>% 
  rename(
    `Topic` = topic,
    `Catriona (%)` = `Catriona`,
    `Treasure Island (%)` = `Treasure Island`
  ) %>%
  mutate(
    `Catriona (%)` = if_else(`Catriona (%)` < 0.001, "< 10^-6", sprintf("%.2f", `Catriona (%)`)),
    `Treasure Island (%)` = if_else(`Treasure Island (%)` < 0.001, "< 10^-6", sprintf("%.2f", `Treasure Island (%)`))
  )



# Topic number = 7 

# Implementing LDA
books_lda_7 <- LDA(books_dtm, k = 7, control = list(seed = 1234))

#terms(books_lda,5) # you can see top 5 words for each topic
book_topics_7 <- tidy(books_lda_7, matrix = "beta")    # Each row gives the robability(beta) of a word belonging to a specific topic

#This turned the model into a one-topic-per-term-per-row format. LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms_7 <- book_topics_7 %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%     # Get top 99 words for each topic
  ungroup() %>%
  arrange(topic, -beta)

book_topic_gamma_7 <- tidy(books_lda_7, matrix = "gamma")   # Gamma matrix : probability distribution of topics within each book (document) 

# Topic distribution table for topic number 7
topic_distribution_table_7 <- book_topic_gamma_7 %>%
  mutate(gamma = gamma * 100) %>%  # converting to percentage
  pivot_wider(names_from = document, values_from = gamma) %>%  # spreading documents into columns
  arrange(topic) %>% 
  rename(
    `Topic` = topic,
    `Catriona (%)` = `Catriona`,
    `Treasure Island (%)` = `Treasure Island`
  )



# Topic number = 8 

# Implementing LDA
books_lda_8 <- LDA(books_dtm, k = 8, control = list(seed = 1234))

#terms(books_lda,5) # you can see top 5 words for each topic
book_topics_8 <- tidy(books_lda_8, matrix = "beta")    # Each row gives the robability(beta) of a word belonging to a specific topic

#This turned the model into a one-topic-per-term-per-row format. LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms_8 <- book_topics_8 %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%     # Get top 99 words for each topic
  ungroup() %>%
  arrange(topic, -beta)

book_topic_gamma_8 <- tidy(books_lda_8, matrix = "gamma")   # Gamma matrix : probability distribution of topics within each book (document) 

# Topic distribution table for topic number 8
topic_distribution_table_8 <- book_topic_gamma_8 %>%
  mutate(gamma = gamma * 100) %>%  # converting to percentage
  pivot_wider(names_from = document, values_from = gamma) %>%  # spreading documents into columns
  arrange(topic) %>% 
  rename(
    `Topic` = topic,
    `Catriona (%)` = `Catriona`,
    `Treasure Island (%)` = `Treasure Island`
  )



# Line charts
graph_table_4 <- ggplot(book_topic_gamma_4, aes(x = topic, y = gamma, color = document, group = document)) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 3) +
  labs(title = "For 4 topics",
       x = "Topic Number",
       y = "Gamma (Topic Proportion)") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(
    breaks = seq(0, 0.6, by = 0.1),  # Sets breaks at 0, 0.1, 0.2, ..., 1
    limits = c(0, 0.6)  # Ensures the y-axis starts at 0 and ends at 1
  ) +
  scale_color_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  theme_minimal() + 
  theme(legend.position = "none")


graph_table_5 <- ggplot(book_topic_gamma_5, aes(x = topic, y = gamma, color = document, group = document)) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 3) +
  labs(title = "For 5 topics",
       x = "Topic Number",
       y = "Gamma (Topic Proportion)") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(
    breaks = seq(0, 0.6, by = 0.1),  # Sets breaks at 0, 0.1, 0.2, ..., 1
    limits = c(0, 0.6)  # Ensures the y-axis starts at 0 and ends at 1
  ) +
  scale_color_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  theme_minimal() + 
  theme(legend.position = "none")


graph_table_6 <- ggplot(book_topic_gamma_6, aes(x = topic, y = gamma, color = document, group = document)) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 3) +
  labs(title = "For 6 topics",
       x = "Topic Number",
       y = "Gamma (Topic Proportion)") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(
    breaks = seq(0, 0.6, by = 0.1),  # Sets breaks at 0, 0.1, 0.2, ..., 1
    limits = c(0, 0.6)  # Ensures the y-axis starts at 0 and ends at 1
  ) +
  scale_color_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  theme_minimal() + 
  theme(legend.position = "none")


graph_table_7 <- ggplot(book_topic_gamma_7, aes(x = topic, y = gamma, color = document, group = document)) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 3) +
  labs(title = "For 7 topics",
       x = "Topic Number",
       y = "Gamma (Topic Proportion)") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(
    breaks = seq(0, 0.6, by = 0.1),  # Sets breaks at 0, 0.1, 0.2, ..., 1
    limits = c(0, 0.6)  # Ensures the y-axis starts at 0 and ends at 1
  ) +
  scale_color_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  theme_minimal() + 
  theme(legend.position = "none")


graph_table_8 <- ggplot(book_topic_gamma_8, aes(x = topic, y = gamma, color = document, group = document)) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 3) +
  labs(title = "For 8 topics",
       x = "Topic Number",
       y = "Gamma (Topic Proportion)") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(
    breaks = seq(0, 0.6, by = 0.1),  # Sets breaks at 0, 0.1, 0.2, ..., 1
    limits = c(0, 0.6)  # Ensures the y-axis starts at 0 and ends at 1
  ) +
  scale_color_manual(values = c("Catriona" = "#657b9e", "Treasure Island" = "#c8775d")) + 
  theme_minimal() + 
  theme(legend.position = "none")



# Checking for overlapping topics in the books
topic_present <- book_topic_gamma %>% 
  filter(gamma > 0.05) %>% 
  mutate(topic = as.factor(topic)) %>%
  group_by(topic) %>%
  summarise(books_present = list(unique(document))) %>%
  ungroup()

topics_in_both_books <- topic_present %>%
  filter(lengths(books_present) == 2)
topics_in_both_books



# Generating tables with top 99 terms for each topic for both topic modelling by book and topic modelling by parts
latex_tables <- function(data, folder_path) {
  if (!dir.exists(folder_path)){
    dir.create(folder_path)
  }
  
  for (i in 1:6) {
    # Extract top 99 words for each topic
    m_top_1 <- data %>% 
      filter(topic == i) %>% 
      arrange(desc(beta)) %>%
      select(-topic) %>%
      head(n = 99)
    
    # Add rank column
    m_top_1 <- m_top_1 %>% mutate(rank = 1:99) %>% select(rank, everything())
    
    # Split top 99 words into three groups: 1-33, 34-66, and 67-99
    m1 <- m_top_1 %>% filter(rank <= 33)
    m2 <- m_top_1 %>% filter(rank >= 34 & rank <= 66)
    m3 <- m_top_1 %>% filter(rank >= 67 & rank <= 99)
    
    # Add columns for second and third groups
    m1['rank2'] <- m2$rank
    m1['term2'] <- m2$term
    m1['beta2'] <- m2$beta
    m1['rank3'] <- m3$rank
    m1['term3'] <- m3$term
    m1['beta3'] <- m3$beta
    
    # Create caption and label for LaTeX
    cptn <- paste("\\small{", "Top 99 terms for Topic", as.character(i), "with their probabilities.}", sep = " ")
    #  cptn <- paste("Top 99 terms for Topic", as.character(i), "with their probabilities.", sep = " ")
    lbl <- paste("topic_", as.character(i), "_terms", sep = "")
    
    # Generate LaTeX file path
    fn <- file.path(folder_path, paste(lbl, ".tex", sep = ""))
    
    # Output LaTeX table to file
    print(xtable(m1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), 
          file = fn, include.rownames = FALSE)
    
    # Optionally, print out the filename for confirmation
    print(paste("Top 99 words for each topic:", fn))
  }
}

# Call the function for the first dataset
folder_path_1 <- "top99_words_per_topic_bookwise"
latex_tables(book_topics_top_terms, folder_path_1)


