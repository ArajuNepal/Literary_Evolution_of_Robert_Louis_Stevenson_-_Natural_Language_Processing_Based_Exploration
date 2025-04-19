# This file downloads the book 'Treasure Island' from Project Gutenberg, does
# basic cleaning of the book, and saves the data as ? files.
#
# Author: Ashley Dennis-Henderson
# Last Modified: March 2025

## Load Libraries ----

pacman::p_load(tidyverse, tidytext, gutenbergr)


## Load Book from Project Gutenberg ----

book <- gutenberg_download(27780, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")


## Basic Cleaning of Book ----

book_text_only <- book %>% tail(nrow(book) - 228) %>% head(7458)  # Remove header and footer

#book_as_string <- paste(book_text_only$text, collapse = " ")  # Convert book to a single string

# Separate Book by Chapters and Parts:

book_chapters <- book_text_only %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE))),
         part = cumsum(str_detect(text, 
                                  regex("^part [\\divxlc]",
                                        ignore_case = TRUE)))) %>%
  ungroup()

# Remove Empty Lines:

book_chapters_clean <- book_chapters %>% 
  filter(!(text == ""))

# Remove Chapter and Part Headings:

count_ch <- 0  # Initiate a count for chapters

count_part <- 0  # Initiate a count for parts

ch <- 0  

part <- 0

rows <- c()  # Initiate empty string of rows to be removed

for (i in 1:nrow(book_chapters_clean)){  # For every row of our data
  
  temp_ch <- book_chapters_clean[i,4]  # Get the chapter number for that row
  
  temp_part <- book_chapters_clean[i,5]  # Get the part number for that row
  
  if (!(temp_part == part)){  # If the part number for our row is not equal to our previous part number (i.e. have moved to a new part)
    
    part <- temp_part  # Set the new part number
    
    count_part <- 1  # Set the part count to 1
    
    rows <- c(rows, i)  # Add row to those to be removed
    
  }
  else if (temp_part == part) {  # Otherwise
    
    count_part <- count_part + 1  # Add to our part count
    
    if(count_part < 3){  # The part headings take up two rows, so if our count is less than 3
      
      rows <- c(rows, i)  # Add row to those to be removed
      
    }
  }
  
  if (!(temp_ch == ch)){  # If the chapter number for our row is not equal to the previous chapter number (i.e. we have moved to a new chapter)
    
    ch <- temp_ch  # Set the new chapter number
    
    count_ch <- 1  # Set the chapter count to 1
    
    rows <- c(rows, i)  # Add row to those to be removed
    
  }
  else if (temp_ch == ch) {  # Otherwise
    
    count_ch <- count_ch + 1  # Add to our chapter count
    
    if(count_ch < 3){  # The chapter headings take up two rows, so if our count is less than 3
      
      rows <- c(rows, i)  # Add row to those to be removed
    }
  }
  
}

rows <- unique(rows)  # Get a list of unique rows to be removed

# Remove those Rows:

book_chapters_clean2 <- book_chapters_clean %>%
  filter(!row_number() %in% rows)  

# Combine Text for each Part/Chapter Pair:

book_chapters_clean3 <- book_chapters_clean2 %>%
  select(part, chapter, text) %>%
  group_by(part, chapter) %>%
  mutate(text = paste0(text, collapse = " ")) %>%
  slice(1) %>%
  ungroup()

# Remove []:

book_chapters_clean4 <- book_chapters_clean3 

book_chapters_clean4$text <- str_remove_all(book_chapters_clean4$text, "\\[[:print:]+?\\]")

# Remove Stop Words:

data(stop_words)  # Load stop words

tidy_book <- book_chapters_clean4 %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

## Save Data as CSV ----

write.csv(book_chapters_clean4, "treasure_island.csv")

