# Jane Austen ----

tidy_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate( 
    linenumber = row_number(), 
    chapter = cumsum(str_detect(text,  
                                regex("^chapter [\\divxlc]",  
                                      ignore_case = TRUE)))) %>% 
  ungroup() %>% 
  unnest_tokens(word, text) 

tidy_books

bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("afinn")) %>% 
  count(book, chapter, word, value) %>% 
  ungroup() |> 
  filter(chapter != 0)

chapter_scores <- bing_word_counts |> 
  mutate(score = n * value) |> 
  group_by(book, chapter) |> 
  summarise(ch_score = sum(score))

chapter_scores

ggplot(chapter_scores) +
  geom_tile(aes(x = chapter, y = book, fill = ch_score),
            colour = "white", height = 0.75) +
  scale_fill_gradient(low = "black", high = "cyan2") +
  labs(y = "") +
  theme_minimal()

# pdftools ----

pdf.file <- "docs/file.pdf"

pdf.text <- pdftools::pdf_text(pdf.file)
cat(pdf.text[[2]]) 

as_tibble(pdf.text) |> 
  unnest_tokens(word, value) |> 
  anti_join(stop_words) |> 
  count(word, sort = T) |> 
  tail()
