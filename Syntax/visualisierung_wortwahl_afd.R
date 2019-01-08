
library(quanteda)
library(tidyverse)
library(magrittr)
library(topicmodels)
library(stm)
library(Rtsne)
library(geometry)
library(rsvd)
library(broom)

# Datensätze Laden 

load("Ergebnisdaten/corpus_09_13.R")
reden09<- all_reden

load("Ergebnisdaten/corpus_13_17.R")
reden13 <- all_reden

load("Ergebnisdaten/corpus_okt17_okt18.R")
reden17 <- all_corpus

all_corpus <- NULL
all_reden<- NULL


# Nach Parteien sortieren 
party_regex<- c("SPD", "CDU\\/CSU", "AfD", "DIE LINKE", "FDP", "LINKEN", "B.NDNIS.90\\/DIE.GR.NEN", "B.NDNIS.90\\/ DIE.GR.NEN", "Grüne")


reden09$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))
reden09$party         %<>% str_extract(paste(party_regex, collapse = "|"))
reden13$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))
reden13$party         %<>% str_extract(paste(party_regex, collapse = "|"))

reden09$speaker_party %<>% str_replace("LINKEN", "DIE LINKE")
reden09$party         %<>% str_replace("LINKEN", "DIE LINKE")
reden13$speaker_party %<>% str_replace("LINKEN", "DIE LINKE")
reden13$party         %<>% str_replace("LINKEN", "DIE LINKE")
reden17$speaker_party %<>% str_replace("LINKEN", "DIE LINKE")
reden17$party         %<>% str_replace("LINKEN", "DIE LINKE")

reden09$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
reden09$party         %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
reden13$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
reden13$party         %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
reden17$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
reden17$party         %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
reden17$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")

reden09$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
reden09$party         %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
reden13$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
reden13$party         %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
reden17$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
reden17$party         %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
reden17$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")

# Party Vector
party_vector_09 <- reden09$party %>% unique()
party_vector_13 <- reden13$party %>% unique()
party_vector_17 <- reden17$party %>% unique()
party_vector_13<- party_vector_13[!party_vector_13 %in% c("AfD","FDP")]
party_vector <- party_vector_17


## Quanteda Visualisierungen: 

reden_corpus<- all_corpus%>% select(speaker, speaker_party, date, speech, speech_id)%>% distinct()
corpus <- corpus(reden_corpus, text_field = "speech", docid_field = "speech_id")


# Häufigsten Wörter der Parteien im Vergleich
word_list <- c("beifall", "afd", "spd", "grünen", "90", "cdu", "bündnis", "csu", "dr", "herr", "damen",
               "beim", "dass", "abgeordneten", "müssen", "fdp", "sowie", "linke", "herren", "ja", "linken",
               "mehr", "sagen", "kollegen", "dank", "gibt", "kolleginnen", "schon", "liebe", "mal", "immer",
               "geht", "abg", "ganz", "dafür", "kollege", "frau", "präsident", "vizepräsident","wolfgang",
               "unsere", "gerade", "deshalb", "gesagt", "letzten", "tun", "genau", "gar", "wurde", 
               "wer", "möchte", "darauf", "wäre", "stellen", "lachen", "ulli", "kai", "dr", "zuruf", "zurufe", "000","18", "drucksache")



dfm_weight_pres <- corpus %>%
  corpus_subset(.,speaker_party %in% party_vector) %>%
  dfm(remove = c(stopwords("de"), word_list), remove_punct = TRUE) %>%
  dfm_weight(scheme = "prop")

freq_weight <- textstat_frequency(dfm_weight_pres, n = 10, groups = "speaker_party")

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")
ggsave(plot = last_plot(), filename = "Manuscript/Grafiken/17_HäufigsteWörter.png")

##Häufigsten Wörter insgesamt 
dfm_17 <- corpus %>%
  corpus_subset(.,speaker_party %in% party_vector) %>%
  dfm(remove = c(stopwords("de"), word_list), remove_punct = TRUE)

features_dfm_inaug <- textstat_frequency(dfm_17, n = 50)

# Sort by reverse frequency order
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))

ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#----------- Comparison Cloud Klappt so mäßig 

corpus_subset(corpus, 
              speaker_party %in% c("AfD", "SPD", "CDU/CSU")) %>%
  dfm(groups = "speaker_party", remove = c(stopwords("de"), word_list), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE)

# Topicmodels to be answered ... 

# Topicmodel 2017
news_dfm <- dfm(corpus, remove_punct = TRUE, remove = c(stopwords('de'), word_list)) %>% 
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
           max_docfreq = 0.3, docfreq_type = "prop")
news_dfm <- news_dfm[ntoken(news_dfm) > 0,]
dtm <- convert(news_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 7)
terms(lda, 15)

# Topicmodel 2013-2017 

reden_corpus<- reden17%>% select(speaker, speaker_party, date, speech, speech_id)%>% distinct()
corpus <- corpus(reden_corpus, text_field = "speech", docid_field = "speech_id")

news_dfm <- dfm(corpus, remove_punct = TRUE, remove = c(stopwords('de'), word_list)) %>% 
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
           max_docfreq = 0.3, docfreq_type = "prop")
news_dfm <- news_dfm[ntoken(news_dfm) > 0,]
dtm <- convert(news_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 50)
terms(lda, 4)

# Topicmodel 2009-2017
reden_corpus<- all_reden%>% select(speaker, speaker_party, date, speech, speech_id)%>% distinct()
corpus <- corpus(reden_corpus, text_field = "speech", docid_field = "speech_id")

news_dfm <- dfm(corpus, remove_punct = TRUE, remove = c(stopwords('de'), word_list)) %>% 
  dfm_trim(min_termfreq = 0.9, termfreq_type = "quantile")
#news_dfm <- news_dfm[ntoken(news_dfm) > 0,]
#dtm <- convert(news_dfm, to = "topicmodels")

test <- readCorpus(dtm, type = "slam")

hi<-stm(news_dfm, init.type = "Spectral", K=0)

lda <- LDA(dtm, k = 20, method = "Gibbs")

to <- tidy(hi)
install.packages()

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



