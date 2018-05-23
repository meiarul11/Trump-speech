#Sentiment Analysis on trumph speech's using nrc lexicon
library(tm)

library(tidytext)

library(tibble)

text <- read_lines("full_speech.txt")

text1 <- removeWords(text ,"applause")

text2 <- tibble(line=1:length(text), text=text1)

titext <- text2 %>%
  unnest_tokens(word, text)

titext %>%
  anti_join(stop_words, by="word") %>%
  count(word, sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col() +
  coord_flip()

library(tidytext)

library(tidyr)


textfull <- readLines("full_speech.txt")

text1 <- removeWords(textfull ,"applause")

text2 <- tibble(line=1:74, text=text1)

text2_bigrams <- text2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

text2_bigrams <- text2_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

text2_bigrams <- text2_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

text2_bigrams_tf <- text2_bigrams %>%
  unite(bigram, word1, word2, sep = " ")


text2_bigrams_tf %>%
  count(bigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(x=bigram, y=n)) +
  geom_col() +
  coord_flip()

textfull <- readLines("full_speech.txt")

text1 <- removeWords(textfull ,"applause")

text2 <- tibble(line=1:74, text=text1)

text2_bigrams <- text2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

text2_bigrams <- text2_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

text2_bigrams <- text2_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


text2_bigrams1 <-  filter(text2_bigrams, word2 != "trump")
text2_bigrams2 <- filter(text2_bigrams1, word1 != "not")
text2_bigrams3 <- filter(text2_bigrams2, word1 != "never")
text2_bigrams4 <- filter(text2_bigrams3, word1 != "no")

nrcsen <- get_sentiments("nrc")

text2_bigrams4 %>%
  inner_join (nrcsen, by=c("word2" = "word")) %>%
  count(word2, sentiment, sort = TRUE) %>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ggplot(aes(x=word2, y=n)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()
