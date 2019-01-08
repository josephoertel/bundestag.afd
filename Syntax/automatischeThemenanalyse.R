library(LSS)
library(tidyverse)
library(magrittr)
library(visNetwork)
library(sna)
library(network)
library(quanteda)

load("Ergebnisdaten/korpus09_13.RData")

reden09 <- all_reden

load("Ergebnisdaten/korpus13_17.RData")

reden13 <- all_reden

load("Ergebnisdaten/korpus_okt17_okt18.RData")

reden17 <- all_corpus

word_list <- c("beifall", "afd", "spd", "grünen", "90", "cdu", "bündnis", "csu", "dr", "herr", "damen",
               "beim", "dass", "abgeordneten", "müssen", "fdp", "sowie", "linke", "herren", "ja", "linken",
               "mehr", "sagen", "kollegen", "dank", "gibt", "kolleginnen", "schon", "liebe", "mal", "immer",
               "geht", "abg", "ganz", "dafür", "kollege", "frau", "präsident", "vizepräsident","wolfgang",
               "unsere", "gerade", "deshalb", "gesagt", "letzten", "tun", "genau", "gar", "wurde", 
               "wer", "möchte", "darauf", "wäre", "stellen", "lachen", "ulli", "kai", "dr", "zuruf", "zurufe", "000","18", "drucksache",
               "dass","mehr","schon","seit","sagt","immer","wurde","beim","heute","gibt","erst","geht")

my_dict_ref <- dictionary(list(migration = c("migrat*", 
                                         "migrant*",
                                         "immigration*", 
                                         "flüchtling*",
                                         "einwander*",
                                         "asyl*",
                                         "ausländer*", 
                                         "flucht*")))
 
my_dict_umwelt <- dictionary(list(migration = c("umwelt*",
                                             "energie*",
                                             "klima*",
                                             "nachhaltig*",
                                             "kohle*",
                                             "emission*",
                                             "abgase*")))


my_dict_arbeit_und_soziales <- dictionary(list(migration = c("arbeit*",
                                                "hartz*",
                                                "lohn*",
                                                "tarif*",
                                                "wirtschaft*",
                                                "industrie*",
                                                "gerneration*")))

my_dict_sicherheit <- dictionary(list(migration = c("terror*",
                                                   "sicherheit*",
                                                    "grenz*",
                                                    "polizei*",
                                                    "überwachung*")))

my_dict_bildung <- dictionary(list(migration = c("schul*",
                                                    "bildung*",
                                                    "jugend*",
                                                    "kindergarten*",
                                                    "kita*",
                                                    "lehr*",
                                                    "ausbildung*",
                                                    "unterricht*",
                                                    "universtität*",
                                                    "hochschul*",
                                                    "grundschule*",
                                                    "gymnasium*",
                                                    "schüler*",
                                                 "student")))


my_dict_außenpolitik <- dictionary(list(migration = c("international*",
                                                      "grenzüberschreitend*",
                                                      "supranational*",
                                                      "außenpolitik*",
                                                      "dipolmat*",
                                                      "uno")))

my_dict_gesundheit <- dictionary(list(migration = c("medizin*",
                                                      "gesundheit*",
                                                      "krank*",
                                                      "pflege*",
                                                    "arzt")))


my_dict_finanzen <- dictionary(list(migration = c("haushalt*",
                                                    "finanzen*",
                                                    "steuern*")))

my_dict_bau <- dictionary(list(migration = c("bau*",
                                                  "wohn*",
                                                  "miet*",
                                                  "stadtentwicklung*")))

my_dict_justiz <- dictionary(list(migration = c("justiz*",
                                             "verfassung*",
                                             "grundrecht*",
                                             "grundgesetz*",
                                             "haft*",
                                             "gericht*")))

my_dict_kultur <- dictionary(list(migration = c("kultur*",
                                                "sport*",
                                                "medien*",
                                                "künstler*",
                                                "religion*",
                                                "kirchen*",
                                                "rundfunk*")))




all_reden <- NULL 

reden13$speech_id %<>% as.character()
reden09$speech_id %<>% as.character()

all_corpus <- bind_rows(reden09, reden13, reden17)
reden_corpus<- all_corpus%>% select(speaker, speaker_party, date, speech, speech_id)%>% distinct()

corpus <- corpus(reden_corpus, text_field = "speech", docid_field = "speech_id")

toks_sent <- corpus %>% 
  corpus_reshape('sentences') %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(c(stopwords("de"),word_list)) 

#mt_sent <- toks_sent %>% 
#  dfm(remove = c(stopwords("de"), word_list)) %>% 
#  dfm_select('^[0-9a-zA-Z]+$', valuetype = 'regex') %>% 
#  dfm_trim(min_termfreq = 10)



migration    <- head(char_keyness(toks_sent, my_dict_ref, remove_pattern = FALSE, sort = TRUE), 500)
umwelt       <- head(char_keyness(toks_sent, my_dict_umwelt, remove_pattern = FALSE, sort = TRUE), 500)
bildung      <- head(char_keyness(toks_sent, my_dict_bildung, remove_pattern = FALSE, sort = TRUE), 500)
arbeit       <- head(char_keyness(toks_sent, my_dict_arbeit_und_soziales, remove_pattern = FALSE, sort = TRUE), 500)
sicherheit   <- head(char_keyness(toks_sent, my_dict_sicherheit, remove_pattern = FALSE, sort = TRUE), 500)
außenpolitik <- head(char_keyness(toks_sent, my_dict_außenpolitik, remove_pattern = FALSE, sort = TRUE), 500)
gesundheit   <- head(char_keyness(toks_sent, my_dict_gesundheit, remove_pattern = FALSE, sort = TRUE), 500)
finanzen     <- head(char_keyness(toks_sent, my_dict_finanzen, remove_pattern = FALSE, sort = TRUE), 500)
bau          <- head(char_keyness(toks_sent, my_dict_bau, remove_pattern = FALSE, sort = TRUE), 500)
justiz       <- head(char_keyness(toks_sent, my_dict_justiz, remove_pattern = FALSE, sort = TRUE), 500)
kultur       <- head(char_keyness(toks_sent, my_dict_kultur, remove_pattern = FALSE, sort = TRUE), 500)


reden17 %<>% select(speaker, speaker_party, date, speech, speech_id) %>% distinct()
sample(umwelt, 30)
#write.csv(migration, file = "migration.csv", row.names = F)



for (i in 1:length(reden17$speech)){
  
  worte             <- reden17$speech[i] %>% tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% tokens_remove(c(stopwords("de"),word_list))%>% tokens_tolower() %>% unlist()
  reden17$migration[i]    <- length(which(worte %in% migration))
  reden17$umwelt[i]       <- length(which(worte %in% umwelt      ))
  reden17$bildung[i]      <- length(which(worte %in% bildung     ))
  reden17$arbeit[i]       <- length(which(worte %in% arbeit      ))
  reden17$sicherheit[i]   <- length(which(worte %in% sicherheit  ))
  reden17$außenpolitik[i] <- length(which(worte %in% außenpolitik))
  reden17$gesundheit[i]   <- length(which(worte %in% gesundheit  ))
  reden17$finanzen[i]     <- length(which(worte %in% finanzen    ))
  reden17$bau[i]          <- length(which(worte %in% bau         ))
  reden17$justiz[i]       <- length(which(worte %in% justiz      ))
  reden17$kultur[i]       <- length(which(worte %in% kultur      ))
  reden17$sum[i]          <- rowSums(reden17[i, c(6:16)])
}


reden17 %<>% mutate(Migration = migration/sum)
reden17 %<>% mutate(Umwelt = umwelt      /sum)
reden17 %<>% mutate(Bildung = bildung     /sum)
reden17 %<>% mutate(Arbeit = arbeit      /sum)
reden17 %<>% mutate(Sicherheit = sicherheit  /sum)
reden17 %<>% mutate(Außenpolitik = außenpolitik/sum)
reden17 %<>% mutate(Gesundheit = gesundheit  /sum)
reden17 %<>% mutate(Finanzen = finanzen    /sum)
reden17 %<>% mutate(Bau = bau         /sum)
reden17 %<>% mutate(Justiz = justiz      /sum)
reden17 %<>% mutate(Kultur = kultur      /sum)

themen_matrix <- reden17 %>% select(speaker_party, date, c(18:28))
themen_matrix[3:13] %<>% apply(., c(1,2), function(x) {ifelse(any(x > 0.2), 1, 0)}) %>% 
  replace_na(0) 

gesamt_matrix <- themen_matrix[,c(3:13)] %>% as.matrix()

afd_matrix <- themen_matrix %>% filter(speaker_party == "AfD") %>% select(c(3:13)) %>%
  as.matrix()

gruen_matrix <- themen_matrix %>% filter(speaker_party == "FDP") %>% select(c(3:13)) %>%
  as.matrix()




themen_matrix <- afd_matrix

plot_network <- function(x, n){
cooc_matrix <- t(x)%*%x
diag(cooc_matrix) <- 0 
cooc_matrix[cooc_matrix < n] <- 0
test.gr <- graph_from_adjacency_matrix(cooc_matrix, mode="undirected", weighted=T)
test.visn <- toVisNetworkData(test.gr)
test.visn$edges$value <- test.visn$edges$weight
visNetwork(test.visn$nodes, test.visn$edges)%>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123)
}
 

plot_network(gesamt_matrix, 10)
plot_network(afd_matrix, 4)


# Reden, mit Migration und Bau 

speech_matrix <- reden17 %>% select(speaker_party, date, speech, c(18:28))
speech_matrix[4:14] %<>% apply(., c(1,2), function(x) {ifelse(any(x > 0.2), 1, 0)}) %>% 
  replace_na(0) 
mig_bau  <- speech_matrix %>% filter(speaker_party == "AfD", Migration == 1, Bau == 1)
mig_kult <- themen_matrix %>% filter(speaker_party == "AfD", migration_i == 1, kultur_i == 1)
mig_just <- themen_matrix %>% filter(speaker_party == "AfD", migration_i == 1, justiz_i == 1)

# 4 ist best Example 
mig_bau$speech[2]

mig_kult$speech[4]

mig_just$speech[2]


