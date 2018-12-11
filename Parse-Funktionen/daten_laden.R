# Pakete laden
library(xml2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)


# Stammdaten (für Namenvektor) laden
stammdaten <- read_xml("MDB_STAMMDATEN.XML")


#Parteien Einlesen
party_regex<- c("SPD", "CDU\\/CSU", "AfD", "DIE LINKE", "FDP", "LINKEN", "B.NDNIS.90\\/DIE.GR.NEN", "B.NDNIS.90\\/ DIE.GR.NEN")


# Funktionen laden
load("Parse-Funktionen/parse_vor17.R")


# 2009-2013

#DataFrame initatialisieren
all_reden<- data_frame()

# Funktion ausführen
x <- list.files(path = "reden2009-2013/", pattern = "\\.xml$")
map(x, parse_vor17, ordner = "reden2009-2013/")

# Datum wird angepasst:
all_reden$date <- all_reden$date %>% dmy()

# Parteien werden standardisiert
all_reden$speaker_party %<>% str_replace("LINKEN", "DIE LINKE")
all_reden$party         %<>% str_replace("LINKEN", "DIE LINKE")
all_reden$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
all_reden$party         %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
all_reden$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
all_reden$party         %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")

# Daten speichern
save(all_reden, file = "Ergebnisdaten/korpus09_13.RData")

# Oktober 2009 - Oktober 2010
reden_okt <- all_reden %>% filter(.$date >= as.Date("2009-10-01") & .$date <= as.Date("2010-11-01"))
save(reden_okt, file= "Ergebnisdaten/korpus_okt09_okt10.RData")




# 2013-2017
#DataFrame initatialisieren
all_reden<- data_frame()

# Funktion ausführen
x <- list.files(path = "reden2013_2017/", pattern = "\\.xml$")
map(x, parse_vor17, ordner = "reden2013_2017/")

# Datum wird angepasst:
all_reden$date <- all_reden$date %>% dmy()

# Parteien werden standardisiert
all_reden$speaker_party %<>% str_replace("LINKEN", "DIE LINKE")
all_reden$party         %<>% str_replace("LINKEN", "DIE LINKE")
all_reden$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
all_reden$party         %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
all_reden$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
all_reden$party         %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")

#Speichern der Daten
save(all_reden, file = "Ergebnisdaten/korpus13_17.RData")

# Oktober 2013 - Oktober 2014
reden_okt <- all_reden %>% filter(.$date >= as.Date("2013-10-01") & .$date <= as.Date("2014-11-01"))
save(reden_okt, file= "Ergebnisdaten/korpus_okt13_okt14.RData")




# 2017 - 2018

# Funktion Laden
load(file = "Parse-Funktionen/parse_nach17.R")

# Corpus initatialisieren
all_corpus<- data_frame()

# Funktion ausführen
x <- list.files(path = "reden2017-heute/", pattern = "\\.xml$")
map(x, parse_nach17, ordner = "reden2017-heute/")
all_corpus$date <- all_corpus$date %>% str_remove(., "^[^(\\d{1-2}.)]*") %>% dmy()

# Parteien standardisieren 
all_corpus$party         %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
all_corpus$speaker_party %<>% str_replace("BÜNDNIS 90\\/ DIE GRÜNEN","GRÜNE")
all_corpus$party         %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
all_corpus$speaker_party %<>% str_replace("B.NDNIS.90\\/DIE.GR.NEN", "GRÜNE")
all_corpus$speaker_party %<>% str_replace("LINKEN", "DIE LINKE")
all_corpus$party         %<>% str_replace("LINKEN", "DIE LINKE")


save(all_corpus, file ="Ergebnisdaten/korpus_okt17_okt18.RData")