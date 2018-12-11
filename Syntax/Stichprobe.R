library("xlsx")
library("magrittr")
library("tidyverse")

## Stichprobe für Redeninhalte: 


load("Ergebnisdaten/korpus_okt09_okt10.RData")
reden09<- reden_okt

load("Ergebnisdaten/korpus_okt13_okt14.RData")
reden13 <- reden_okt

load("Ergebnisdaten/korpus_okt17_okt18.RData")
reden17 <- all_corpus

rm(all_corpus)
rm(reden_okt)

## Stichprobenziehen: 
# 1. Datum der Rede. 2. Redener 3. Redner Party 4. Rede Anfang 5. X_Redeinhalt 6. X_Topinhalt 7. X_Anzahl der
#Angreifenden Kommentare 8. X_Parteien_der Angreifenden Kommentare

# 1. Datensatz auf Reden reduzieren
reden09%<>% select(sitzungs_id, date, speaker, speaker_party, speech_id, speech)%>% distinct()
reden13%<>% select(sitzungs_id, date, speaker, speaker_party, speech_id, speech)%>% distinct()
reden17%<>% select(sitzungs_id, date, speaker, speaker_party, speech_id, speech)%>% distinct()

#2. Nur Reden von Parteien auswählen

reden09<- reden09[!is.na(reden09$speaker_party),]
reden13<- reden13[!is.na(reden13$speaker_party),]
reden17<- reden17[!is.na(reden17$speaker_party),]

#Plenarprotokolle 
reden17 <- reden17[!is.na(reden17$sitzungs_id),]

set.seed(123)
plenar09 <- reden09$sitzungs_id %>% unique() %>% sample(60)
plenar13 <- reden13$sitzungs_id %>% unique() %>% sample(60)
plenar17 <- reden17$sitzungs_id %>% unique() 


# 4. Stichprobe ziehen: Pro Plenardokument jeweils eine Rede 
sample09 <- reden09[1,]
sample13 <- reden13[1,]
sample17 <- reden17[1,]

set.seed(123)
for (i in 1:length(plenar09)) {
  sample09[i, ] <- filter(reden09, sitzungs_id == plenar09[i]) %>% sample_n(1)
}
 
for (i in 1:length(plenar13)) {
  sample13[i,] <- filter(reden13, sitzungs_id == plenar13[i]) %>% sample_n(1)
}


for (i in 1:length(plenar17)) {
  sample17[i,] <- filter(reden17, sitzungs_id == plenar17[i]) %>% sample_n(1)
}


sample09$speech_id%<>% as.character()
sample13$speech_id%<>% as.character()
all_sample <- bind_rows(sample09, sample13, sample17)


# 4.2 Für der Reli- Test werden jedem noch einmal 10 Reden hinzugefügt


all_sample$X_Redeinhalt <- NA
all_sample$X_Topinhalt  <- NA 
all_sample$X_Sonstiges  <- NA

# 5. Reden auf Anfangswörter kürzen: 

all_sample$speech %<>% strtrim(50)

all_sample %<>% sample_n(179)


# Reden zuteilen

Vivian <- bind_rows(all_sample[1:45, ], all_sample[85:90, ], all_sample[170:175, ])
Nils <- bind_rows(all_sample[45:90, ], all_sample[30:35, ], all_sample[165:170, ])
Mehmet <-bind_rows(all_sample[90:179, ], all_sample[25:30, ], all_sample[80:85, ])


# 6. Stichprobe in Excel-Datei:

write.xlsx(Vivian, file ="vivian_inhalt.xlsx",showNA = F)
write.xlsx(Nils, file ="nils_inhalt.xlsx",showNA = F)
write.xlsx(Mehmet, file ="mehmet_inhalt.xlsx",showNA = F)

