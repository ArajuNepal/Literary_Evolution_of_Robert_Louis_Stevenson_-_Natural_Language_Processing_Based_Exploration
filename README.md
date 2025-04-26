
# Digging for Treasure using NLP

Analysing two standalone novels by an author to understand the evolution of the author's writing style using NLP in R programming language. 

## How to run the code
1. Clone this repository of download the ZIP file.
2. Open the '.Rproj' file in R Studio.
3. Go to the 'main.R' file and run the code using any of the following methods : 
  - Highlight the lines of code you want to run and press 'Ctrl + Enter' (Windows) or 'Cmd + Enter' (Mac).
  - Or, run source("main.R") in the console.
  - Or, select the entire script and click on the 'Run' icon in R Studio.
When you run the script, a folder named `visualisations` will be created. The visualizations will be stored in **.jpg** and **.eps** formats. Open the **.jpg** files for viewing, as **.eps** files are designed for use in LaTeX reports.


## R Environment and Dependencies 

The version of R on which the project is built is **4.3.2**. The packages used in the project are as follows:

- gutenbergr : Download and process texts from Project Gutenberg.
- dplyr : Used for data manipulation.
- tidytext : Tidy up data and analyse data.
- string : work with strings and regular expressions.
- SnowballC : Perform word stemming to reduce words to their root forms.
- textstem : Used for text processing like stemming and lemmatization.
- ggplot2 : Data visualisatio
- forcats : Handling of the categorical (factor) variables.
- tidyr : Reshape and tidy the dataset to make them easier to work with.
- tokenizers : Break text into tokens such as words, sentences or n-grams.
- topicmodels : Implement topic modelling algorithms like LDA.
- tm : Offers tools for text mining.
- ldatuning : Find optimal number of topics in topic modelling using diagnostic metrics.
- xtable : Convert R tables to LaTeX or HTML.
- ggwordcloud : Create word cloud using ggplot2.


## Data collection
Both the books were extracted from Project Gutenberg (https://www.gutenberg.org/).
- Treasure Island(https://www.gutenberg.org/ebooks/27780)
- Catriona(https://www.gutenberg.org/ebooks/589)


## R files and what they do 
This project has the following files:

- **data_cleaning.R** : This script performs comprehensive text preprocessing and cleaning, tokenizing the text, removing stopwords, and lemmatizing words.
                    It also generates basic word frequency statistics and visualizations of stopword distributions, preparing the dataset for further NLP analysis.

- **word_frequencies.R** : This script generates word clouds for unigrams and bigrams with and without character names.
- **tf_idf.R** : This script calculates tf-idf values for unigrams and bigrams with and without character names and visualizes them.
- **topic_modelling.R** : This script implements LDA on the entire corpus (both books are considered as one document) and visualises the topics and top words in each topic.
- **main.R** : The script is a comprehensive NLP pipeline that displays the outputs for all the work done in the files explained above.
 
## Contact

If you have questions or feedback, feel free to contact me at araju7nepal@gmail.com




