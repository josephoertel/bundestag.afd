library(LSS)
library(tidyverse)
library(magrittr)
library(visNetwork)
library(networkD3)
library(igraph)
library(sna)


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

topic <- "migration"
i <- 1
test_topic <- function (topic, x) {
  topic_new <- paste0(topic, "_i")
  if (reden17[x, topic]/reden17$sum[x] >= 0.2){
    reden17[x, topic_new] <- 1 
  } else {
    reden17[x, topic_new] <- 0 
  }
}



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


reden17 %<>% mutate(migration_i = migration/sum)
reden17 %<>% mutate(umwelt_i = umwelt      /sum)
reden17 %<>% mutate(bildung_i = bildung     /sum)
reden17 %<>% mutate(arbeit_i = arbeit      /sum)
reden17 %<>% mutate(sicherheit_i = sicherheit  /sum)
reden17 %<>% mutate(außenpolitik_i = außenpolitik/sum)
reden17 %<>% mutate(gesundheit_i = gesundheit  /sum)
reden17 %<>% mutate(finanzen_i = finanzen    /sum)
reden17 %<>% mutate(bau_i = bau         /sum)
reden17 %<>% mutate(justiz_i = justiz      /sum)
reden17 %<>% mutate(kultur_i = kultur      /sum)

t <- apply(reden17[,18:28], c(1,2), function(x) {ifelse(any(x > 0.2), 1, 0)})
t %<>% replace_na(0)
t %<>% as.matrix()
x <- t(t)
w <- x%*%t
w %<>% as.matrix()

f <- colSums(t)/nrow(t)

f%*%t(f)

w_n <- as.network(w,directed = F)

plot.network(w_n,label = colnames(t))





