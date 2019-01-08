library(xlsx)
library(tidyverse)
library(magrittr)
library(rlist)
library(purrr)

# 1. Stichprobe laden 


load("stichprobe_inhalt.RData")

# 2. Gesamtcorpus laden 

load("Ergebnisdaten/korpus09_13.RData")
reden09 <- all_reden
load("Ergebnisdaten/korpus13_17.RData")
reden13 <- all_reden
load("Ergebnisdaten/korpus_okt17_okt18.RData")
reden17 <- all_corpus
rm(all_corpus)
rm(all_reden)
reden13$speech_id %<>% as.character()
reden09$speech_id %<>% as.character()

all_korpus <- bind_rows(reden09, reden13, reden17) %>% filter(type == "Kommentar") %>% select(-c(type, party_action, top, speaker_id, beschreibung, entschuldigteAbgeordnete))


stichprobe_zwischenrufe <- left_join( all_sample, all_korpus, by = "speech")
stichprobe_zwischenrufe %<>% select(-c(speaker_party.x, speech_id.x, speaker.x))
stichprobe_zwischenrufe %<>% na.omit()


write.csv(stichprobe_zwischenrufe, "stichprobe_zwischenrufe.csv", fileEncoding = "UTF-8")
