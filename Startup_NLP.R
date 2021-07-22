##### packages ########

library(readr)
library(tidyr)
library(dplyr)
library(quanteda)
library(stringr)
library(ggplot2)
library(quanteda.textstats)
library(tidyverse)
library(igraph)
library(ggraph)
library(xlsx)
library(readtext)
library(GGally)
library(wordcloud)
library(reshape2)
library(forcats)
library(widyr)
library(tm)
library(topicmodels)
library(scales)
library(mallet)
library(readr)
library(striprtf)
library(PerformanceAnalytics)
library(HunMineR)
library(factoextra)
library(text2vec)
library(readr)
library(stringr)
library(e1071)
library(SparseM)



####  StopWords ####

StopW_quan <- tibble(text = stopwords("hungarian")) %>% 
  rename(lemma = text)

custom_stop_words <- custom_stop_words %>% 
  rename(lemma = word) %>% 
  select(-lexicon)

#####  beolvasás - tisztítás  ########

##### PreSEED #####

Startup_Banding_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "Banding", year = "2020", cycle = "PreSeed", series = "PreSeed") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 


##### SEED #####

Startup_Lockhek_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "Lockhek", year = "2020", cycle = "Seed", series = "Seed") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_IBookR_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "IBookR", year = "2021", cycle = "Seed", series = "Seed") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 


##### PostSeed_SeriesA  #####

Startup_Day2day_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "Day2day", year = "2020", cycle = "PostSeed", series = "SeriesA") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_LockhekPS_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "LockhekPS", year = "2020", cycle = "PostSeed", series = "SeriesA") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_EMed_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "EMed", year = "2020", cycle = "PostSeed", series = "SeriesA") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_Briefly_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "Briefly", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_Bedrockfarm_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "Bedrockfarm", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_EdesVaros_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "EdesVaros", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 


##### PostSeed_SeriesB  #####
Startup_ViddL_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "ViddL", year = "2021", cycle = "PostSeed", series = "SeriesB") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Startup_Pentech_lemma <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "SCONJ") %>% 
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features) %>%
  mutate(name = "Pentech", year = "2021", cycle = "PostSeed", series = "SeriesB") %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

####  Corpus ####

Startup_Corpus_lemma <- Startup_Banding_lemma %>% 
  add_row(Startup_Lockhek_lemma) %>% 
  add_row(Startup_IBookR_lemma) %>% 
  add_row(Startup_Day2day_lemma) %>% 
  add_row(Startup_LockhekPS_lemma) %>% 
  add_row(Startup_EMed_lemma) %>% 
  add_row(Startup_Briefly_lemma) %>% 
  add_row(Startup_Bedrockfarm_lemma) %>% 
  add_row(Startup_EdesVaros_lemma) %>% 
  add_row(Startup_ViddL_lemma) %>% 
  add_row(Startup_Pentech_lemma) %>% 
  select(-token)


Startup_Corpus <- Startup_Corpus_lemma %>% 
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") %>%
  rename(word = lemma)

Startup_Corpus <- Startup_Corpus %>%
  mutate(word = str_replace_all(string = word, pattern = "exi", replacement = "exit" ))%>%
  mutate(word = str_replace_all(string = word, pattern = "negyedéves", replacement = "negyedév" ))%>%
  mutate(word = str_replace_all(string = word, pattern = "Terminal", replacement = "DesignTerminal" ))

##### DFM - DTM  ########  
  
dfm_corpus <- Startup_Corpus %>%
  count(word,name, year, cycle, series) %>%
  mutate(total = sum(n)) %>% 
  mutate(n.total = n / sum(n))

dfm_corpus <- dfm_corpus %>%
  filter(n > 20) %>%
  arrange(desc(n.total))

dfmplot1 <- ggplot(dfm_corpus, aes(n.total, word)) +
  geom_col() +
  labs(y = NULL)

#ggsave("dfmplot1.jpg")

#######################################
############# tf-idf ##################
#######################################


corpus_tf_idf <- Startup_Corpus %>%
  count(word,name, year, cycle, series) %>%
  mutate(total = sum(n)) %>% 
  mutate(n.total = n/total)


sub_corpus_tf_idf <- subset(corpus_tf_idf, n.total <  0.0005)

tf1 <- ggplot(sub_corpus_tf_idf, aes(x=n.total, fill = name)) +
  geom_histogram(show.legend = FALSE, bins = 10) +
  facet_wrap(~name, ncol = 4, scales = "free_y")

# ggsave("tf1.jpg")


tf2 <- ggplot(sub_corpus_tf_idf, aes(x=n.total, fill = name)) +
  geom_histogram(show.legend = FALSE, bins = 10) +
  facet_wrap(~series, ncol = 4, scales = "free_y")

# ggsave("tf2.jpg")


### Zipf’s law - log-log coordinates ###

freq_by_rank <- corpus_tf_idf %>% 
  arrange(desc(n)) %>%
  group_by(name) %>%
  mutate(rank = row_number(), 
         `term frequency` = n.total) %>%
  ungroup()


freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = name)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()  

rank_subset <- freq_by_rank %>% 
  filter(rank < 100,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)


tf_idf_Zips <- freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = name)) + 
  geom_abline(intercept = 0.0026, slope = -0.576, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = T) + 
  scale_x_log10() +
  scale_y_log10()

#ggsave("tf_idf_Zips.jpg")

corpus2_tf_idf <- corpus_tf_idf %>%
  bind_tf_idf(word, name, n)

corpus2_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


tf_idf_corpus <- corpus2_tf_idf %>%
  group_by(name) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#ggsave("tf_idf_corpus.jpg")

corpus2_tf_idf$cycle <- factor(corpus2_tf_idf$cycle,
                               levels = c("PreSeed", "Seed", "PostSeed"))
corpus2_tf_idf %>%
  group_by(cycle) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = cycle)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(.~cycle, scales = "free") +
  labs(x = "tf-idf", y = NULL)


corpus2_tf_idf %>%
  group_by(series) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = series)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~series, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# summary #

Startup_CorpusSum <- Startup_Corpus %>% 
  count(word,name, year, cycle, series)

df1_summary<-as.data.frame(Startup_CorpusSum %>% 
  summarise(
  mean_wordcount = mean(n),
  std_dev = sd(n),
  min_wordc = min(n),
  max_wordc = max(n)
))



df2_summary<-as.data.frame(Startup_CorpusSum %>% 
  group_by(name) %>% 
  summarise(
    mean_wordcount = mean(n),
    std_dev = sd(n),
    min_wordc = min(n),
    max_wordc = max(n)
  ))


df3_summary<-as.data.frame(Startup_CorpusSum %>% 
  group_by(cycle) %>% 
  summarise(
    mean_wordcount = mean(n),
    std_dev = sd(n),
    min_wordc = min(n),
    max_wordc = max(n)
  ))


df4_summary<-as.data.frame(Startup_CorpusSum %>% 
  group_by(series) %>% 
  summarise(
    mean_wordcount = mean(n),
    std_dev = sd(n),
    min_wordc = min(n),
    max_wordc = max(n)
  )) 


### ### ### ### ### ### ### ### ### ### 
###### n-grams and correlations ###### 
###### bigrams, trigrams  ### ### ### 
### ### ### ### ### ### ### ### ### ###


Startup_Banding_lemma_chr <- as.character(Startup_Banding_lemma$lemma) 
Startup_Banding_lemma_chr <- paste(Startup_Banding_lemma_chr, collapse = " ")
Startup_Lockhek_lemma_chr <- as.character(Startup_Lockhek_lemma$lemma) 
Startup_Lockhek_lemma_chr <- paste(Startup_Lockhek_lemma_chr, collapse = " ")
Startup_IBookR_lemma_chr <- as.character(Startup_IBookR_lemma$lemma) 
Startup_IBookR_lemma_chr <- paste(Startup_IBookR_lemma_chr, collapse = " ")
Startup_Day2day_lemma_chr <- as.character(Startup_Day2day_lemma$lemma) 
Startup_Day2day_lemma_chr <- paste(Startup_Day2day_lemma_chr, collapse = " ")
Startup_LockhekPS_lemma_chr <- as.character(Startup_LockhekPS_lemma$lemma) 
Startup_LockhekPS_lemma_chr <- paste(Startup_LockhekPS_lemma_chr, collapse = " ")
Startup_EMed_lemma_chr <- as.character(Startup_EMed_lemma$lemma) 
Startup_EMed_lemma_chr <- paste(Startup_EMed_lemma_chr, collapse = " ")
Startup_Briefly_lemma_chr <- as.character(Startup_Briefly_lemma$lemma) 
Startup_Briefly_lemma_chr <- paste(Startup_Briefly_lemma_chr, collapse = " ")
Startup_Bedrockfarm_lemma_chr <- as.character(Startup_Bedrockfarm_lemma$lemma) 
Startup_Bedrockfarm_lemma_chr <- paste(Startup_Bedrockfarm_lemma_chr, collapse = " ")
Startup_EdesVaros_lemma_chr <- as.character(Startup_EdesVaros_lemma$lemma) 
Startup_EdesVaros_lemma_chr <- paste(Startup_EdesVaros_lemma_chr, collapse = " ")
Startup_ViddL_lemma_chr <- as.character(Startup_ViddL_lemma$lemma) 
Startup_ViddL_lemma_chr <- paste(Startup_ViddL_lemma_chr, collapse = " ")
Startup_Pentech_lemma_chr <- as.character(Startup_Pentech_lemma$lemma) 
Startup_Pentech_lemma_chr <- paste(Startup_Pentech_lemma, collapse = " ")


#### Bigrams ####


Bigram_Banding <- tibble(text = Startup_Banding_lemma_chr) %>%
  mutate(name = "Banding", year = "2020", cycle = "PreSeed", series = "PreSeed") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_Lockhek <- tibble(text = Startup_Lockhek_lemma_chr) %>%
  mutate(name = "Lockhek", year = "2020", cycle = "Seed", series = "Seed") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_IBookR <- tibble(text = Startup_IBookR_lemma_chr) %>%
  mutate(name = "IBookR", year = "2021", cycle = "Seed", series = "Seed") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_Day2day <- tibble(text = Startup_Day2day_lemma_chr) %>%
  mutate(name = "Day2day", year = "2020", cycle = "PostSeed", series = "SeriesA") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_LockhekPS <- tibble(text = Startup_LockhekPS_lemma_chr) %>%
  mutate(name = "LockhekPS", year = "2020", cycle = "PostSeed", series = "SeriesA") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_EMed <- tibble(text = Startup_EMed_lemma_chr) %>%
  mutate(name = "EMed", year = "2020", cycle = "PostSeed", series = "SeriesA") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_Briefly <- tibble(text = Startup_Briefly_lemma_chr) %>%
  mutate(name = "Briefly", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_Bedrockfarm <- tibble(text = Startup_Bedrockfarm_lemma_chr) %>%
  mutate(name = "Bedrockfarm", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_EdesVaros <- tibble(text = Startup_EdesVaros_lemma_chr) %>%
  mutate(name = "EdesVaros", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_ViddL <- tibble(text = Startup_ViddL_lemma_chr) %>%
  mutate(name = "ViddL", year = "2021", cycle = "PostSeed", series = "SeriesB") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigram_Pentech <- tibble(text = Startup_Pentech_lemma_chr) %>%
  mutate(name = "Pentech", year = "2021", cycle = "PostSeed", series = "SeriesB") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(name, year, cycle, series, bigram, sort = TRUE)

Bigrams_filtered_Banding <- Bigram_Banding %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_Lockhek <- Bigram_Lockhek %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_IBookR <- Bigram_IBookR %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_Day2day <- Bigram_Day2day %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_LockhekPS <- Bigram_LockhekPS %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_EMed <- Bigram_EMed %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_Briefly <- Bigram_Briefly %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_Bedrockfarm <- Bigram_Bedrockfarm %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_EdesVaros <- Bigram_EdesVaros %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_ViddL <- Bigram_ViddL %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)

Bigrams_filtered_Pentech <- Bigram_Pentech %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$lemma) %>%
  filter(!word2 %in% custom_stop_words$lemma)


Bigram_counts_Banding <- Bigrams_filtered_Banding %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_Lockhek <- Bigrams_filtered_Lockhek %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_IBookR <- Bigrams_filtered_IBookR %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_Day2day <- Bigrams_filtered_Day2day %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_LockhekPS <- Bigrams_filtered_LockhekPS %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_EMed <- Bigrams_filtered_EMed %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_Briefly <- Bigrams_filtered_Briefly %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_Bedrockfarm <- Bigrams_filtered_Bedrockfarm %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_EdesVaros <- Bigrams_filtered_EdesVaros %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_ViddL <- Bigrams_filtered_ViddL %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)

Bigram_counts_Pentech <- Bigrams_filtered_Pentech %>% 
  count(name, year, cycle, series, word1, word2, sort = TRUE)


#### Tigrams ####

tigram_Banding <-
  tibble(text = Startup_Banding_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "Banding", year = "2020", cycle = "PreSeed", series = "PreSeed") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series,word1, word2, word3, sort = TRUE)

tigram_Lockhek <-
  tibble(text = Startup_Lockhek_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "Lockhek", year = "2020", cycle = "Seed", series = "Seed") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_IBookR <-
  tibble(text = Startup_IBookR_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "IBookR", year = "2021", cycle = "Seed", series = "Seed") %>% 
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_Day2day <-
  tibble(text = Startup_Day2day_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "Day2day", year = "2020", cycle = "PostSeed", series = "SeriesA") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_LockhekPS <-
  tibble(text = Startup_LockhekPS_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "LockhekPS", year = "2020", cycle = "PostSeed", series = "SeriesA") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_EMed <-
  tibble(text = Startup_EMed_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "EMed", year = "2020", cycle = "PostSeed", series = "SeriesA") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_Briefly <-
  tibble(text = Startup_Briefly_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "Briefly", year = "2021", cycle = "PostSeed", series = "SeriesA") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_Bedrockfarm <-
  tibble(text = Startup_Bedrockfarm_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "Bedrockfarm", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_EdesVaros <-
  tibble(text = Startup_EdesVaros_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "EdesVaros", year = "2021", cycle = "PostSeed", series = "SeriesA") %>% 
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_ViddL <-
  tibble(text = Startup_ViddL_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "ViddL", year = "2021", cycle = "PostSeed", series = "SeriesB") %>%
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)

tigram_Pentech <-
  tibble(text = Startup_Pentech_lemma_chr) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(name = "Pentech", year = "2021", cycle = "PostSeed", series = "SeriesB") %>% 
  filter(!word1 %in% custom_stop_words$lemma,
         !word2 %in% custom_stop_words$lemma,
         !word3 %in% custom_stop_words$lemma) %>%
  count(name, year, cycle, series, word1, word2, word3, sort = TRUE)


##### corpus

Startup_Corpus_chr <- as.character(Startup_Corpus$word) 
Startup_Corpus_chr <- paste(Startup_Corpus_chr, collapse = " ")


Corpus_bigram_filtered <-  Bigram_counts_Banding %>% 
  add_row(Bigram_counts_Lockhek) %>% 
  add_row(Bigram_counts_IBookR) %>% 
  add_row(Bigram_counts_Day2day) %>% 
  add_row(Bigram_counts_LockhekPS) %>% 
  add_row(Bigram_counts_EMed) %>% 
  add_row(Bigram_counts_Briefly) %>% 
  add_row(Bigram_counts_Bedrockfarm) %>% 
  add_row(Bigram_counts_EdesVaros) %>% 
  add_row(Bigram_counts_ViddL) %>% 
  add_row(Bigram_counts_Pentech) 


Corpus_tigram_filtered <-  tigram_Banding %>% 
  add_row(tigram_Lockhek) %>% 
  add_row(tigram_IBookR) %>% 
  add_row(tigram_Day2day) %>% 
  add_row(tigram_LockhekPS) %>% 
  add_row(tigram_EMed) %>% 
  add_row(tigram_Briefly) %>% 
  add_row(tigram_Bedrockfarm) %>% 
  add_row(tigram_EdesVaros) %>% 
  add_row(tigram_ViddL) %>% 
  add_row(tigram_Pentech) 
  





########## analyzing bigrams

Bigrams_filtered_Banding %>%
  filter(word2 == "startup") %>%
  count(name, year, cycle, series, word1, sort = TRUE)


################################################
######## HÁLÓZAT###############################
################################################


set.seed(2022)

##### PreSeed  

Bigram_PreSeed <- Corpus_bigram_filtered %>% 
  filter(series == "PreSeed") 

bigram_graph_PreSeed <- Bigram_PreSeed %>%
  select(- name, - year, - cycle, - series, -n) %>% 
  graph_from_data_frame()

hist(degree(bigram_graph_PreSeed)) # ez alapján határoztam meg a degree számot
bigram_graph_PreSeed2 <- induced_subgraph(bigram_graph_PreSeed, degree(bigram_graph_PreSeed) > 20)

graph_PreSeed <- ggraph(bigram_graph_PreSeed2, 
       layout = "stress") +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(2.5, 'mm')),color = "gray") +
  geom_node_point(size = 3, color = "yellow", show.legend = FALSE) +
  geom_node_text(aes(label = name),size=3, vjust = 1, hjust = 1) +
  theme_void()

#ggsave("graph_PreSeed.jpg")

##### Seed  

Bigram_Seed <- Corpus_bigram_filtered %>% 
  filter(series == "Seed") 

bigram_graph_Seed <- Bigram_Seed %>%
  select(- name, - year, - cycle, - series, -n) %>%  
  graph_from_data_frame()

hist(degree(bigram_graph_Seed)) 
bigram_graph_Seed2 <- induced_subgraph(bigram_graph_Seed, degree(bigram_graph_Seed) > 25)

graph_Seed <- ggraph(bigram_graph_Seed2, 
       layout = "stress") +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(2.5, 'mm')),color = "gray") +
  geom_node_point(size = 3, color = "red", show.legend = FALSE) +
  geom_node_text(aes(label = name),size=3, vjust = 1, hjust = 1) +
  theme_void()

#ggsave("graph_Seed.jpg")

##### PostSeed_SeriesA

Bigram_PostSeedSA <- Corpus_bigram_filtered %>% 
  filter(series == "SeriesA") 

bigram_graph_PostSeedSA <- Bigram_PostSeedSA %>%
  select(- name, - year, - cycle, - series, -n) %>%  
  graph_from_data_frame()

hist(degree(bigram_graph_PostSeedSA)) 
bigram_graph_PostSeedSA2 <- induced_subgraph(bigram_graph_PostSeedSA, degree(bigram_graph_PostSeedSA) > 90)

graph_PostSeedSA2 <- ggraph(bigram_graph_PostSeedSA2, 
       layout = "stress") +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(2.5, 'mm')),color = "gray") +
  geom_node_point(size = 3, color = "green", show.legend = FALSE) +
  geom_node_text(aes(label = name),size=3, vjust = 1, hjust = 1) +
  theme_void()

#ggsave("graph_PostSeedSA2.jpg")

##### PostSeed_SeriesB  #####

Bigram_PostSeedSB <- Corpus_bigram_filtered %>% 
  filter(series == "SeriesB") 

bigram_graph_PostSeedSB <- Bigram_PostSeedSB %>%
  select(- name, - year, - cycle, - series, -n) %>%  
  graph_from_data_frame()

hist(degree(bigram_graph_PostSeedSB)) 
bigram_graph_PostSeedSB2 <- induced_subgraph(bigram_graph_PostSeedSB, degree(bigram_graph_PostSeedSB) > 25)

graph_PostSeedSB <- ggraph(bigram_graph_PostSeedSB2, 
       layout = "stress") +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(2.5, 'mm')),color = "gray") +
  geom_node_point(size = 3, color = "blue", show.legend = FALSE) +
  geom_node_text(aes(label = name),size=3, vjust = 1, hjust = 1) +
  theme_void()

#ggsave("graph_PostSeedSB.jpg")

#  Corpus

bigram_graph_Corpus <- Corpus_bigram_filtered %>%
  select(- name, - year, - cycle, - series, -n) %>% 
  graph_from_data_frame()


hist(degree(bigram_graph_Corpus)) 
bigram_graph_Corpus2 <- induced_subgraph(bigram_graph_Corpus, degree(bigram_graph_Corpus) > 150)

graph_Corpus  <- ggraph(bigram_graph_Corpus2, 
       layout = "stress") +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(2.5, 'mm')),color = "gray") +
  geom_node_point(size = 3, color = "blue", show.legend = FALSE) +
  geom_node_text(aes(label = name),size=3, vjust = 1, hjust = 1) +
  theme_void()

#ggsave("graph_Corpus.jpg")

# Statistics

summary(bigram_graph_PreSeed)
summary(bigram_graph_Seed)
summary(bigram_graph_PostSeedSA)
summary(bigram_graph_PostSeedSB)
summary(bigram_graph_Corpus)

graph.density(bigram_graph_PreSeed,loop=FALSE)
graph.density(bigram_graph_Seed,loop=FALSE)
graph.density(bigram_graph_PostSeedSA,loop=FALSE)
graph.density(bigram_graph_PostSeedSB,loop=FALSE)
graph.density(bigram_graph_Corpus,loop=FALSE)


diameter(bigram_graph_PreSeed)
diameter(bigram_graph_Seed)
diameter(bigram_graph_PostSeedSA)
diameter(bigram_graph_PostSeedSB)
diameter(bigram_graph_Corpus)

average.path.length(bigram_graph_PreSeed)
average.path.length(bigram_graph_Seed)
average.path.length(bigram_graph_PostSeedSA)
average.path.length(bigram_graph_PostSeedSB)
average.path.length(bigram_graph_Corpus)

transitivity(bigram_graph_PreSeed)
transitivity(bigram_graph_Seed)
transitivity(bigram_graph_PostSeedSA)
transitivity(bigram_graph_PostSeedSB)
transitivity(bigram_graph_Corpus)

mean_distance(bigram_graph_PreSeed)
mean_distance(bigram_graph_Seed)
mean_distance(bigram_graph_PostSeedSA)
mean_distance(bigram_graph_PostSeedSB)
mean_distance(bigram_graph_Corpus)



# Lexikai diverzitás

Startup_Banding_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_Banding_lemma_chr2 <- as.character(Startup_Banding_lemma2$lemma) 
Startup_Banding_lemma_chr2 <- paste(Startup_Banding_lemma_chr2, collapse = " ")

Startup_Lockhek_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_Lockhek_lemma_chr2 <- as.character(Startup_Lockhek_lemma2$lemma) 
Startup_Lockhek_lemma_chr2 <- paste(Startup_Lockhek_lemma_chr2, collapse = " ")

Startup_IBookR_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_IBookR_lemma_chr2 <- as.character(Startup_IBookR_lemma2$lemma) 
Startup_IBookR_lemma_chr2 <- paste(Startup_IBookR_lemma_chr2, collapse = " ")


Startup_Day2day_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_Day2day_lemma_chr2 <- as.character(Startup_Day2day_lemma2$lemma) 
Startup_Day2day_lemma_chr2 <- paste(Startup_Day2day_lemma_chr2, collapse = " ")

Startup_LockhekPS_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_LockhekPS_lemma_chr2 <- as.character(Startup_LockhekPS_lemma2$lemma) 
Startup_LockhekPS_lemma_chr2 <- paste(Startup_LockhekPS_lemma_chr2, collapse = " ")

Startup_EMed_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_EMed_lemma_chr2 <- as.character(Startup_EMed_lemma2$lemma) 
Startup_EMed_lemma_chr2 <- paste(Startup_EMed_lemma_chr2, collapse = " ")

Startup_Briefly_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_Briefly_lemma_chr2 <- as.character(Startup_Briefly_lemma2$lemma) 
Startup_Briefly_lemma_chr2 <- paste(Startup_Briefly_lemma_chr2, collapse = " ")

Startup_Bedrockfarm_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_Bedrockfarm_lemma_chr2 <- as.character(Startup_Bedrockfarm_lemma2$lemma) 
Startup_Bedrockfarm_lemma_chr2 <- paste(Startup_Bedrockfarm_lemma_chr2, collapse = " ")

Startup_EdesVaros_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_EdesVaros_lemma_chr2 <- as.character(Startup_EdesVaros_lemma2$lemma) 
Startup_EdesVaros_lemma_chr2 <- paste(Startup_EdesVaros_lemma_chr2, collapse = " ")

Startup_ViddL_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_ViddL_lemma_chr2 <- as.character(Startup_ViddL_lemma2$lemma) 
Startup_ViddL_lemma_chr2 <- paste(Startup_ViddL_lemma_chr2, collapse = " ")

Startup_Pentech_lemma2 <- read.csv("*.csv", sep = ';') %>%
  rename(token = Column1, lemma = Column2, POS_tag = Column3, morfologic_features = Column4) %>% 
  filter(!POS_tag == "PUNCT")  %>% 
  filter(!POS_tag == "ADP") %>% 
  filter(!POS_tag == "ADV") %>% 
  filter(!POS_tag == "CONJ") %>% 
  filter(!POS_tag == "DET") %>% 
  filter(!POS_tag == "PART") %>% 
  filter(!POS_tag == "PRON") %>% 
  filter(!POS_tag == "NUM") %>% 
  filter(!POS_tag == "SCONJ") %>%
  filter(!POS_tag == "PROPN") %>%
  filter(!POS_tag == "INTJ") %>%
  select(-morfologic_features, -token, -POS_tag) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 
Startup_Pentech_lemma_chr2 <- as.character(Startup_Pentech_lemma2$lemma) 
Startup_Pentech_lemma_chr2 <- paste(Startup_Pentech_lemma_chr2, collapse = " ")


# Lemmákkal

df_text <- readtext(
  "*.txt",
  docvarsfrom = "filenames",
  dvsep = "_",
  docvarnames = c("nev", "year", "cycle", "series")
)

corpus_df_text <- corpus(df_text)

Startup_Corpus_dfm <- corpus_df_text %>% 
  tokens(
    remove_punct = T
  ) %>% 
  dfm()


Startup_Corpus_dfm %>% 
  textstat_lexdiv(mesure="CTTR") %>% 
  arrange(desc("CTTR"))

dfm_lexdiv <- Startup_Corpus_dfm
cttr_score <- unlist(textstat_lexdiv(dfm_lexdiv, mesure="CTTR") [, 2] )
docvars(dfm_lexdiv, "CTTR") <- cttr_score
dfm_lexdiv1 <- docvars(dfm_lexdiv)



div_df <- Startup_Corpus_dfm %>% 
  textstat_lexdiv(mesure = "TTR", "CTTR")


textstat_readability(x= corpus_df_text, mesure= "Flesch.Kincaid")


# Jaccard

jaccard <-  Startup_Corpus_dfm %>%
  dfm_weight("prop") %>%
  textstat_simil(margin = "documents",
                 method = "jaccard")

textstat_dist(x=Startup_Corpus_dfm,
              margin = "documents",
              method = "euclidean" )

dist <- Startup_Corpus_dfm %>% 
  textstat_dist(margin = "documents",
                method = "euclidean" )

hierachikus_klaszter <- hclust(as.dist(dist))
hierachikus_klaszter_jac <- hclust(as.dist(jaccard))

plot(hierachikus_klaszter, type = "rectangle", ylab = "Euclidean")
plot(hierachikus_klaszter_jac, type = "rectangle", ylab = "Jaccard")


# Tokennel

df_text2 <- readtext(
  "Startup/QLexikaFiles/*.txt",
  docvarsfrom = "filenames",
  dvsep = "_",
  docvarnames = c("nev", "year", "cycle", "series")
)

corpus_df_text2 <- corpus(df_text2)

Startup_Corpus_dfm2 <- corpus_df_text2 %>% 
  tokens(
    remove_punct = T,
    remove_separators = T,
    split_hyphens = T
  ) %>% 
  dfm() %>% 
  dfm_remove(pattern = custom_stop_words) %>%
  dfm_remove(pattern = stopwords("hungarian"))


Startup_Corpus_dfm2 %>% 
  textstat_lexdiv(mesure="CTTR") %>% 
  arrange(desc("CTTR"))

dfm_lexdiv2 <- Startup_Corpus_dfm2
cttr_score2 <- unlist(textstat_lexdiv(dfm_lexdiv2, mesure="CTTR") [, 2] )
docvars(dfm_lexdiv2, "CTTR") <- cttr_score2
dfm_lexdiv2 <- docvars(dfm_lexdiv2)



div_df2 <- Startup_Corpus_dfm2 %>% 
  textstat_lexdiv(mesure = "TTR", "CTTR")


FleschKincaid_text <- textstat_readability(x= corpus_df_text2, mesure= "Flesch.Kincaid")

f_k <- textstat_readability(x= corpus_df_text2, mesure= "Flesch.Kincaid") 

f_k <- f_k  %>% 
  cbind(df_text2$nev, df_text2$year, df_text2$cycle, df_text2$series)

f_k_plot <- ggplot(f_k, aes(df_text2$series, Flesch)) +
  geom_boxplot() +
  labs(
    x= NULL,
    y = "Flesch-Kincaid index"
  )


# Jaccard

jaccard2 <-  Startup_Corpus_dfm2 %>%
  dfm_weight("prop") %>%
  textstat_simil(margin = "documents",
                 method = "jaccard")

textstat_dist(x=Startup_Corpus_dfm2,
              margin = "documents",
              method = "euclidean" )

dist2 <- Startup_Corpus_dfm2 %>% 
  textstat_dist(margin = "documents",
                method = "euclidean" )

hierachikus_klaszter2 <- hclust(as.dist(dist2))
hierachikus_klaszter_jac2 <- hclust(as.dist(jaccard2))

hc3plot <- plot(hierachikus_klaszter2, type = "rectangle", ylab = "Euclidean")
hc4plot <- plot(hierachikus_klaszter_jac2, type = "rectangle", ylab = "Jaccard")

#ggsave("hc3plot.jpg")
#ggsave("hc4plot.jpg")




#######################################
############# Szentiment #############
#######################################


Corpus_szentiment <- Startup_Corpus

# Lexikonok
Neg_Lexikon_0 <- read.delim("*.txt") 
Neg_Lexikon <- Neg_Lexikon_0 %>%
  mutate(Neg_Lexikon_0, sentiment = "negative")

Pos_Lexikon_0 <- read.delim("*.txt")
Pos_Lexikon <-   Pos_Lexikon_0 %>%
  mutate(Pos_Lexikon_0, sentiment = "positive")

Lexikon <- Neg_Lexikon %>%
  add_row(Pos_Lexikon)


Corpus_sentiment <- Corpus_szentiment %>%
  inner_join(Lexikon) %>%
  count(name, index = row_number(), sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

Corpus_sentiment

Egyeni_sentiment_plot <- ggplot(Corpus_sentiment, aes(index, sentiment, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(
    x= NULL,
    y = "Sentiment index"
  ) +
  facet_wrap(~name, ncol = 4, scales = "free_x")

Corpus_sentiment_plot <- ggplot(Corpus_sentiment, aes(index, sentiment, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(
    x= NULL,
    y = "Sentiment index"
  )

# ggsave("Egyeni_sentiment_plot.jpg")
# ggsave("Corpus_sentiment_plot.jpg")

mean(Corpus_sentiment$positive)
sd(Corpus_sentiment$positive)
mean(Corpus_sentiment$negative)
sd(Corpus_sentiment$negative)
mean(Corpus_sentiment$sentiment)
sd(Corpus_sentiment$sentiment)

# Most common positive and negative words

Corpus_sentiment_word <- Corpus_szentiment %>%
  inner_join(Lexikon) %>%
  count(word, sentiment, sort = TRUE) 
Corpus_sentiment_word


Corpus_sentiment_word_plot <- Corpus_sentiment_word %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#ggsave("Corpus_sentiment_word_plot.jpg")

# Wordclouds

Corpus_szentiment %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


Corpus_szentiment_wc <- Corpus_szentiment %>%
  inner_join(Lexikon) %>%
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100,random.order=FALSE, title.colors=c("black","black"))

# ggsave("Corpus_szentiment_wc.jpg")


Corpus_szentiment2 <- Startup_Corpus %>%
  count(name, word, cycle, series, sort = TRUE)

Corpus_sentiment_word2 <- Corpus_szentiment2 %>%
  inner_join(Lexikon) %>%
  count(word, sentiment, name, cycle, series, sort = TRUE) 
Corpus_sentiment_word2

################

PreSeed_sentiment <- Corpus_sentiment_word2 %>%
  count(word, sentiment, series) %>%
  filter(series == "PreSeed") %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#ggsave("PreSeed_sentiment.png")

Seed_sentiment <- Corpus_sentiment_word2 %>%
  count(word, sentiment, series) %>%
  filter(series == "Seed") %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#ggsave("Seed_sentiment.png")

SeriesA_sentiment <- Corpus_sentiment_word2 %>%
  count(word, sentiment, series) %>%
  filter(series == "SeriesA") %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#ggsave("SeriesA_sentiment.png")

SeriesB_sentiment <- Corpus_sentiment_word2 %>%
  count(word, sentiment, series) %>%
  filter(series == "SeriesB") %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#ggsave("SeriesB_sentiment.png")

###################################
##### ##### Topic modeling ########
###################################


klaszerszam <- fviz_nbclust(
  as.matrix(Startup_Corpus_dfm),
  kmeans,
  method = "wss",
  k.max = 10,
  linecolor = "black"
) +
  labs(
    title = NULL,
    x = "Klaszterek száma",
    y = "Klasztereken belüli négyzetösszeg"
  )

# ggsave("klaszerszam.png")

### DocumentTermMatrix objects

Corpus_all_TM <- Startup_Corpus %>% 
  select(-POS_tag) %>% 
  rename(lemma = word) %>%
  anti_join(StopW_quan, by = "lemma") %>%
  anti_join(custom_stop_words, by = "lemma") 

Corpus_topic_td <- Corpus_all_TM %>%
  rename(word = lemma) %>%
  count(name, year, cycle, series, word) %>%
  cast_dtm(name, word, n)


Corpus_lda <- LDA(Corpus_topic_td, k = 3, control = list(seed = 1234))
Corpus_lda

### Word-topic probabilities

Corpus_topics <- tidy(Corpus_lda, matrix = "beta")
Corpus_topics

Corpus_top_terms <- Corpus_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)


Corpus_top_terms <- Corpus_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(x = expression(beta)) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ggsave("Corpus_top_terms.png")

# greatest difference in β

beta_wide <- Corpus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio21 = log2(topic2 / topic1)) %>%
  mutate(log_ratio23 = log2(topic2 / topic3)) %>%
  mutate(log_ratio31 = log2(topic3 / topic1)) 
beta_wide

beta_wide_topic21 <- beta_wide %>%
  slice_max(abs(log_ratio21), n = 20) %>%
  mutate(term = reorder(term, log_ratio21)) %>%
  ggplot(aes(log_ratio21, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL)
ggsave("beta_wide_topic21.png")


beta_wide_topic23 <- beta_wide %>%
  slice_max(abs(log_ratio23), n = 20) %>%
  mutate(term = reorder(term, log_ratio23)) %>%
  ggplot(aes(log_ratio23, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 3", y = NULL)
ggsave("beta_wide_topic23.png")

beta_wide_topic31 <- beta_wide %>%
  slice_max(abs(log_ratio31), n = 20) %>%
  mutate(term = reorder(term, log_ratio31)) %>%
  ggplot(aes(log_ratio31, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 3 / topic 1", y = NULL)
ggsave("beta_wide_topic31.png")


Corpus_gamma <- tidy(Corpus_lda, matrix = "gamma")
Corpus_gamma
write.xlsx(Corpus_gamma, "Corpus_gamma.xlsx")

Egyéni_topik_megoszlas_gamma <- Corpus_gamma %>%
  mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

#ggsave("Egyéni_topik_megoszlas_gamma.png")

Corpus_gamma2 <- read.csv("Startup/Gamma/Gamma_s.csv", sep = ';')

Series_topik_megoszlas_gamma <- Corpus_gamma2 %>%
  mutate(series = reorder(series, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ series) +
  labs(x = "topic", y = expression(gamma))

#ggsave("Series_topik_megoszlas_gamma.png")


#################################################################
############# Szóbeágyazás #######################################
#################################################################

df_text <- readtext(
  "*.txt",
  docvarsfrom = "filenames",
  dvsep = "_",
  docvarnames = c("nev", "year", "cycle", "series")
)

corpus_df_text <- corpus(df_text)

corpus_lemma <- tokens(corpus_df_text)

features <- dfm(corpus_lemma) %>%
  dfm_trim(min_termfreq = 5) %>% 
  featnames()
  
corpus_lemma <- tokens_select(corpus_lemma, features, padding = T)

corpus_fcm <- fcm(
  x = corpus_lemma,
  context = "window",
  count = "weighted",
  weight = 1 / (1:5),
  tri = T
)

glove <- GlobalVectors$new(
  rank = 300,
  x_max = 10,
  learning_rate = 0.1
)

corpus_main <-glove$fit_transform(
  corpus_fcm,
  n_iter = 10,
  convergence_tol = 0.01
)

corpus_context <- glove$components

corpus_word_vectors <- corpus_main + t(corpus_context)

teszt <- corpus_word_vectors ["startup", , drop = F ]
cos_sim_rom <- sim2(
  x = corpus_word_vectors,
  y = teszt,
  method = "cosine",
  norm = "l2"
) 

head(sort(cos_sim_rom [, 1], decreasing = T))

show_vector <- function(vectors, pattern, n=5) {
  term <- corpus_word_vectors [pattern, , drop = F ]
  cos_sim <- sim2(
    x = vectors,
    y = term,
    method = "cosine",
    norm = "l2"
  ) 
  cos_sim_head <- head(sort(cos_sim [, 1], decreasing = T), n)
  output <- enframe(cos_sim_head, name = "term", value = "dist")
  return(output)
}


show_vector(corpus_word_vectors, "startup", 10)
show_vector(corpus_word_vectors, "vállalkozás", 10)
show_vector(corpus_word_vectors, "cég", 10)
show_vector(corpus_word_vectors, "kft", 10)


startup <- corpus_word_vectors["startup", , drop=F] -
  corpus_word_vectors["vállalkozás", , drop=F] +
  corpus_word_vectors["cég", , drop=F] +
  corpus_word_vectors["kft", , drop=F] 

cos_sim_startup <- textstat_simil(
  x=as.dfm(corpus_word_vectors),
  y=as.dfm(startup),
  method="cosine"
  )

head(sort(cos_sim_startup[, 1], decreasing = T))

corpus_embedding_df <- as.data.frame(corpus_word_vectors[, c(1:2)]) %>% 
  rownames_to_column(var = "words")

embedding_plot <-  function(data, keywords) { 
  data %>% 
    filter(words %in% keywords) %>% 
    ggplot(aes(V1, V2, label = words)) +
    labs(
      x= "Első dimenzó",
      y= "Második dimenzió"
    ) +
    geom_text() +
    xlim(-1, 1) +
    ylim(-1, 1)
  }

words_selected <- c("startup", "vállalkozás", "cég", "kft")
embedding_plot(corpus_embedding_df, keywords = words_selected)


startup_em_plot <- embedding_plot(corpus_embedding_df, keywords = words_selected)
# ggsave("startup_em_plot.png")


words_selected <- c("startup", "vállalkozás", "cég", "kft", "MVP", "ügyfél", 
                    "csatorna", "partner", "pénz", "befektető", "mentor", "probléma", "motiváció", "szeret", "csinál",
                    "siker", "segít", "érték", "dolgozik", "exit", "cél", "idő", "ötlet", "év", "nő", "inkubátor", "misszió")
embedding_plot(corpus_embedding_df, keywords = words_selected)

startup_em_plot2 <- embedding_plot(corpus_embedding_df, keywords = words_selected)
# ggsave("startup_em_plot2.png")



########################################################
############## Szövegösszehasonlítás ############################
########################################################

## Lexikai hasonlóság

# Jaccard hasonlóság

df_text <- readtext(
  "*.txt",
)

corpus_df_text <- corpus(df_text)

corpus_lemma <- tokens(corpus_df_text)

corpus_dfm <- dfm(corpus_lemma) %>% 
  dfm_tfidf()

jaccard <-  textstat_simil(corpus_dfm, method = "jaccard")  %>% 
  as.data.frame()


# Koszinusz hasonlóság

koszinusz <-  textstat_simil(corpus_dfm, method = "cosine") %>% 
  as.data.frame()

# Hoterkép  
  
jaccardhasonlosag_plot <- ggplot(jaccard, aes(document1, document2, fill= jaccard)) + 
  geom_tile() +
  scale_fill_viridis() +
  labs(
    x = NULL,
    y = NULL,
    fill = "Jaccard-hasonlóság"
    ) 

#ggsave("jaccardhasonlosag_plot.png")

koszinuszhasonlosag_plot <- ggplot(koszinusz, aes(document1, document2, fill= cosine)) + 
  geom_tile() +
  scale_fill_viridis() +
  labs(
    x = NULL,
    y = NULL,
    fill = "Koszinusz-hasonlóság"
  ) 

#ggsave("koszinuszhasonlosag_plot.png")
