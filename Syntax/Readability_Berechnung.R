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


load("Ergebnisdaten/korpus09_13.RData")
reden09<- all_reden

load("Ergebnisdaten/korpus13_17.RData")
reden13 <- all_reden

load("Ergebnisdaten/korpus_okt17_okt18.RData")
reden17 <- all_corpus

rm(all_corpus)
rm(all_reden)

party_vector_17 <- reden17$party %>% unique()


# Nach Parteien sortieren 


party_regex<- c("SPD", "CDU\\/CSU", "AfD", "DIE LINKE", "FDP", "LINKEN", "B.NDNIS.90\\/DIE.GR.NEN", "B.NDNIS.90\\/ DIE.GR.NEN")

reden09$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))
reden09$party         %<>% str_extract(paste(party_regex, collapse = "|"))
reden13$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))
reden13$party         %<>% str_extract(paste(party_regex, collapse = "|"))



# 1. Datensatz auf Reden reduzieren
reden09%<>% select(date, speaker_party, speech)%>% distinct()
reden13%<>% select(date, speaker_party, speech)%>% distinct()
reden17%<>% select(date, speaker_party, speech)%>% distinct()

#2. Nur Reden von Parteien auswählen

reden09<- reden09[!is.na(reden09$speaker_party),]
reden13<- reden13[!is.na(reden13$speaker_party),]
reden17<- reden17[!is.na(reden17$speaker_party),]

#3. Zu einem Datensatz: 

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


for (i in 1: length(all_sample$text)){
  all_sample[i, "FRE"] <- tokenize(txt = all_sample[i, "text"],format = "obj", lang = "de", fileEncoding = "UTF-8", ign.comp = FALSE)%>% flesch(parameters = "de") %>% .@Flesch %>% as.data.frame() %>%.[,2] 
  print(i)
} 


# Save results 
readability09_17 <- all_sample 
save(readability09_17, file= "Ergebnisdaten/readability09_17.R")

load("Ergebnisdaten/readability09_17.R")

# Plot 
readability_mean <- readability09_17 %>% filter(month > as.Date("2017-10-01")) %>%
group_by(speaker_party) %>% 
summarise(mean = mean(FRE)) %>% .[order(.$mean),]  
readability_mean$speaker_party %<>% str_replace("BÜNDNIS 90/ DIE GRÜNEN", "GRÜNE")
readability_mean %>% 
ggplot(aes(y = mean, x = reorder(speaker_party, mean))) +
  geom_bar(stat = "identity")+
  coord_flip(ylim=c(57.5, 61.5)) +
  labs(title = "Readability Index (2017) nach Ahmstadt")+
  theme(axis.title=element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_fill_manual(values=c("gray84", "grey37"))+
  theme(legend.title = element_blank())

ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/Readability_line.png")

