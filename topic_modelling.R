## Topic modelling ----
# Topic modelling by book
# Calculating the dtm matrix
books_dtm <- unigrams %>%  # this data is after stopwords removal and after lemmatization 
  cast_dtm(book, word,n)    #cast_dtm creates a document-term matrix from the dataset, each row is book, column represents word, n = frequency of each word in each book

# Finding optimal topic number 
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

#Griffith : higher value indicates better 
#Cao : lower value is better 
#Arun : lower value is better 
#Deveaud : higher value is better

book_wise_topicmodeling_number_of_topics <- FindTopicsNumber_plot(values_for_books)
book_wise_topicmodeling_number_of_topics


# Implementing LDA
books_lda <- LDA(books_dtm, k = 6, control = list(seed = 1234))

#terms(books_lda,5) # you can see top 5 words for each topic
book_topics <- tidy(books_lda, matrix = "beta")    # probability of each word belonging to each topic


#This turned the model into a one-topic-per-term-per-row format. 
#LDA generates a model that associates each document, here book, with a probability distribution over topics
book_topics_top_terms <- book_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 99) %>%     # get top 10 words for each topic
  ungroup() %>%
  arrange(topic, -beta)


book_topic_gamma <- tidy(books_lda, matrix = "gamma")   # gamma = topic proportion per document 
book_topic_gamma


#Visualising out of 6 topics we obtained for the corpus, the proportion of topics in each book
proportion_of_each_topic_visualisation <- book_topic_gamma %>%
  ggplot(aes(x = document, y = gamma, fill = factor(topic), label = round(gamma,2))) +
  geom_col(position = "fill") +
  geom_text(aes(group = topic), position = position_fill(vjust = 0.5), size = 2.5, color = "white" ) + 
  labs(title = "Proportion of topics in each book",
       x = "Book", y = "Topic proportion", fill = "Topic") +
  theme_minimal() + 
  scale_fill_manual(
    values = c(
      "1" = "#657b9e",
      "2" = "#5e93cf",
      "3" = "#223692",
      "4" = "#c8775d", 
      "5" = "#866042",
      "6" = "#bd701e"
    ), ,
    labels = c(
      "1" = "Topic 1 - Adventure related words & interaction",
      "2" = "Topic 2 - Emotional theme",
      "3" = "Topic 3 - Adventure",
      "4" = "Topic 4 - Loss",
      "5" = "Topic 5 - Family",
      "6" = "Topic 6 - Adventure and exploration"
    )
  )




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


