#library(spacyr)
library(quanteda)
library(tidyverse)
library(plyr)
library(koRpus)
library(koRpus.lang.de)
library(stringi)
library(magrittr)
library(lubridate)
install.koRpus.lang("de")
#spacy_initialize(model = "de")
#spacy_finalize()


load("Ergebnisdaten/corpus_09_13.R")
reden09<- all_reden

load("Ergebnisdaten/corpus_13_17.R")
reden13 <- all_reden

load("Ergebnisdaten/corpus_okt17_okt18.R")
reden17 <- all_corpus

all_corpus <- NULL
all_reden<- NULL

party_vector_17 <- reden17$party %>% unique()


# Nach Parteien sortieren 


party_regex<- c("SPD", "CDU\\/CSU", "AfD", "DIE LINKE", "FDP", "LINKEN", "B.NDNIS.90\\/DIE.GR.NEN", "B.NDNIS.90\\/ DIE.GR.NEN")

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

# 1. Datensatz auf Reden reduzieren
reden09%<>% select(date, speaker_party, speech)%>% distinct()
reden13%<>% select(date, speaker_party, speech)%>% distinct()
reden17%<>% select(date, speaker_party, speech)%>% distinct()

#2. Nur Reden von Parteien auswählen

reden09<- reden09[!is.na(reden09$speaker_party),]
reden13<- reden13[!is.na(reden13$speaker_party),]
reden17<- reden17[!is.na(reden17$speaker_party),]

#3. Zu einem Datensatz: 
reden09$date %<>% dmy()
reden13$date %<>% dmy()
all_sample <- bind_rows(reden09, reden13, reden17) %>% filter(speaker_party %in% party_vector_17)

#4. Nach Monaten und Partein gruppieren 
all_sample %<>% group_by(month=floor_date(date, "month"))%>% group_by(speaker_party) 
all_sample<- ddply(all_sample, .(speaker_party, month), summarize, text=paste(speech, collapse=""))
all_sample %<>% filter(stri_length(all_sample$text) > 200)


# Readability Formel nach dem an das deutsche Angepasste FRE
# koRpus Package  #all_sample[34,] Error 

#for (i in 1: length(all_sample$text)){
# first_half <- substring(all_sample[i, "text"], 1, nchar(all_sample[i, "text"])/2) %>% tokenize(txt = .,format = "obj", lang = "de") %>% flesch(parameters = "de") %>% .@Flesch %>% as.data.frame() %>%.[,2] 
#  second_half<- substring(all_sample[i, "text"], nchar(all_sample[i, "text"])/2+1, nchar(all_sample[i, "text"])) %>% tokenize(txt = .,format = "obj", lang = "de") %>% flesch(parameters = "de") %>% .@Flesch %>% as.data.frame() %>%.[,2] 
#  all_sample[i,"FRE"] <- mean(c(first_half, second_half))
#print(i)
#  }


for (i in 89: length(all_sample$text)){
  all_sample[i, "FRE"] <- tokenize(txt = all_sample[i, "text"],format = "obj", lang = "de", fileEncoding = "UTF-8", ign.comp = FALSE)%>% flesch(parameters = "de") %>% .@Flesch %>% as.data.frame() %>%.[,2] 
  print(i)
} 


# Save results 
readability09_17 <- all_sample 
save(readability09_17, file= "Ergebnisdaten/readability09_17.R")

load("Ergebnisdaten/readability09_17.R")

# Plot 
readability09_17 %>%
ggplot(aes(x= month, y= FRE, col = speaker_party)) +
  geom_line()

ggsave(plot = last_plot(), filename = "Manuscript/Grafiken/Readability_line.png")

