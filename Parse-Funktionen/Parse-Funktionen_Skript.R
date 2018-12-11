
## Function Parse_vor17----------

parse_vor17 <- function (ordner = "", x) {

# Plenarprotokoll einlesen
bundestag <- read_xml(paste0(ordner, x))

#Daten, die für das ganze Plenarprotokoll gelten
datum <- xml_find_first(bundestag, "//DATUM") %>% xml_text()
sitzungsnr <- xml_find_first(bundestag,"//NR")%>% xml_text()
wahlperiode<- xml_find_first(bundestag,"//WAHLPERIODE")%>% xml_text()
text <- xml_find_first(bundestag,"//TEXT")%>% xml_text()
text %<>% str_replace_all(pattern = "\\n", replacement = " ")
text %<>% str_remove_all(pattern = "- ")
text %<>% str_remove("(?<=Schluss:).*")

# Liste aller Abgeordneten, die an der Wahlperiode Beteiligt waren
stammdaten <- read_xml("MDB_STAMMDATEN.XML")
Nachnamen <- xml_find_all(stammdaten, "//NACHNAME") %>%xml_text()
Vornamen <- xml_find_all(stammdaten, "//VORNAME") %>%xml_text()
Namen <- paste(Vornamen, Nachnamen)
namenregex <- paste0(Namen, collapse = "|")


# Aufgliederung nach Reden---------------

reden <- str_split(text, paste0(c("(?<=.)(?=(",namenregex,")(?=(:| (\\(.{1,40}\\):))))"),collapse = "")) %>% unlist()

# Datum und Sitzungnummer
reden%<>% as_data_frame()
reden$date <- datum
reden$sitzungs_id <- sitzungsnr




# Alle Informationen zu jeder Rede werden gesucht, Kommentare werden als Liste in eine Zelle geschrieben 
for (i in 1: length(reden$value)) {
  reden[i, "redner"] <- str_extract(reden[i,1],"^[^(|:]+") 
  reden[i, "speaker_party"] <- str_extract(reden[i, 1], "(?<=\\().+?(?=\\))(?=\\):)")
  reden[i, "id"] <- (i+z)
  reden[i, "kommentare"] <- list(str_extract_all(reden[i, 1], "\\(([^)]{2,})\\)(?!:)"))
  }

# Die erste Texteinheit, also alles, was vor dem ersten Redner steht (Inhaltsverzeichnis etc.), wird entfernt
reden <- reden[-1,]

# Die Reden-ID wird zugeteilt 
reden %<>% mutate(id = as.numeric(factor(value)))
reden %<>% group_by(id)
reden %<>% unnest()

# Daten nach Kommentaren aufteilen, Kommentare mit einem Bindestrich kommen von zwei unterschiedlichen Akteuren und sind
#daher zu Trennen. Der Bindestrich ist nicht der Bindestrich von der Tastatur 

reden$kommentare %<>% as.character()
reden$kommentare <- strsplit(reden$kommentare, "–")
reden %<>% unnest()

# Es wird die Art von Interaktion eindeutig zugeordnet 
reden$type <- str_extract_all(reden$kommentare, paste(c("Beifall", "Lachen", "Heiterkeit", "Zuruf", "Zurufe", "Widerspruch", "Anhaltender Beifall"), collapse ="|"))
reden$type[str_detect(reden$kommentare,"\\[.*\\]")] <- "Kommentar"
reden$type[str_detect(reden$kommentare,"(?=.*Beifall)(?=.*\\[.*\\])")]<- "Beifall"
reden$type[str_detect(reden$kommentare,"(?=.*Heiterkeit)(?=.*\\[.*\\])")]<- "Heiterkeit"
reden$type[str_detect(reden$kommentare,"(?=.*Zuruf)(?=.*\\[.*\\])")]<- "Zuruf"
reden %<>% group_by(kommentare)
reden %<>% unnest()
reden$kommentare <- str_split(reden$kommentare, "sowie | und ")
reden$type %<>% as.character()
reden %<>% group_by(type)
reden %<>% unnest()
reden$party <- str_extract_all(reden$kommentare, paste(party_regex, collapse = "|")) 

# Kommentare, die auf mehrere Parteien zutreffen, werden noch mal extra in eine Zeile geschrieben, dass in jeder Zeile maximal
# eine Partei steht 
reden %<>% group_by(kommentare)
reden %<>% unnest()


# Festlegen, ob die ganze Partei beteiligt war, oder nur ein Teil der Partei 
reden$party %<>% as.character()
reden$party_action[str_detect(reden$kommentare, ("([aA]bg\\w{0,9})|(\\[.{1,20}\\])"))] <- 0
reden$party_action[is.na(reden$party_action)]<- 1

# Variabelen Standardisieren
reden %<>% rename(speech = value) 
reden %<>% rename(speaker = redner) 
reden %<>% rename(speech_id = id) 
reden %<>% rename(kommentar = kommentare) 

# Alle Reden werden zusammengefuegt 
all_reden <<- bind_rows(all_reden, reden)




}
save(parse_vor17, file = "Parse-Funktionen/parse_vor17.R")
# Funktionsende


#Function_Parse_nach_2017

parse_nach17 <- function (ordner ="", x) {
  bundestag <- read_xml(paste0(ordner, x))
  
  
  
  # Daten, die für das gesamte Protokoll gelten: 
  
  datum <- xml_find_first(bundestag, "//datum") %>% xml_text()
  sitzungsnr <- xml_find_first(bundestag,"//sitzungsnr")%>% xml_text()
  wahlperiode<- xml_find_first(bundestag,"//wahlperiode")%>% xml_text()
  EntschuldigteAbgeordnete <- (xml_find_all(bundestag, "//anlagen-text [@anlagen-typ = 'Liste der entschuldigten Abgeordneten']//tbody/tr")) %>% xml_text()%>% list()
  
  
  
  # Aufgliederung nach TOP's---------------
  sitzungsverlauf <- xml_find_all(bundestag, "//sitzungsverlauf")
  top <- xml_find_all(sitzungsverlauf, "//tagesordnungspunkt")
  
  # Inhalt von TOP -----------------
  
  top_inhalt <- vector('character')
  for (i in 1:length (top)){
    top_inhalt[i] <- xml_find_all(sitzungsverlauf, paste0("//tagesordnungspunkt[position () =",i, "]/rede[position ()=",i,"]/parent::* /p [not(@klasse = 'T_Drs')]")) %>% 
      xml_text() %>%
      .[-length(.)] %>% 
      unlist(.) %>% 
      toString(.)
  }
  
  # Tagesordnungspunkt Name 
  top_name <- xml_attr(xml_find_all(sitzungsverlauf, "//tagesordnungspunkt"), attr = "top-id")
  
  corpus<- tibble("date" = datum, "sitzungs_id" = sitzungsnr, "top" = top_name, "beschreibung" = top_inhalt, "entschuldigteAbgeordnete"= EntschuldigteAbgeordnete)
  
  # Aufgliederung der TOP's nach Reden 
  reden_df<- data_frame()
  komm_df <- data_frame()
  komm_list<- list()
  reden <- xml_find_all(sitzungsverlauf, "//rede")
  kommentare <- xml_find_all(sitzungsverlauf, "//kommentar")
  
  
  for (i in 1:length (reden)){
    reden_df[i ,"speech_id"] <- reden[[i]] %>% xml_attr(. ,attr= "id")
    .<- reden[[i]] %>% xml_attr(. ,attr= "id") 
    reden_df[i, "top"] <- xml_find_all(sitzungsverlauf, paste0("//","rede [@ id = '",.,"']/parent::*"))%>% xml_attr(. ,attr= "top-id")
    reden_df[i, "speech"]<- xml_find_all(sitzungsverlauf, paste0("//","rede [@ id = '",.,"']")) %>% xml_text()
    reden_df[i, "speaker_id"] <-xml_find_first(sitzungsverlauf, paste0("//","rede [@ id = '",.,"']/p[@klasse = 'redner']/redner")) %>% xml_attr(, attr = "id")
    reden_df[i, "speaker"] <- xml_find_all(sitzungsverlauf, paste0("//","rede [@ id = '",.,"']/p[@klasse = 'redner']/redner/name/vorname |//","rede [@ id = '",.,"']/p[@klasse = 'redner']/redner/name/nachname")) %>% xml_text() %>% .[1:2] %>% paste(., collapse = " ")
    reden_df[i, "speaker_party"] <- xml_find_first(sitzungsverlauf, paste0("//","rede [@ id = '",.,"']/p[@klasse = 'redner']//fraktion |//","rede [@ id = '",.,"']/p[@klasse = 'redner']//rolle_kurz ")) %>% xml_text() 
    komm_list[[i]] <- xml_find_all(sitzungsverlauf, paste0("//","rede [@ id = '",.,"']/kommentar")) %>% xml_text()
  }
  
  # Kommentare + RedenID zum späteren Joinen
  komm_df<- enframe(komm_list, name = "speech_id", value = "kommentar")
  
  for (i in 1:length (reden)){
    komm_df[i, "speech_id"] <- reden[[i]] %>% xml_attr(. ,attr= "id")
  }
  komm_df %<>% group_by(speech_id)
  komm_df %<>% unnest()
  
  komm_df$kommentar <- strsplit(komm_df$kommentar, "–", fixed = T)
  komm_df %<>% group_by(speech_id)
  komm_df %<>% unnest()
  
  komm_df$type <- str_extract_all(komm_df$kommentar, paste(c("Beifall", "Lachen", "Heiterkeit", "Zuruf", "Zurufe", "Widerspruch"), collapse ="|"))
  komm_df$type[str_detect(komm_df$kommentar,"\\[.*\\]")] <- "Kommentar"
  komm_df$type[str_detect(komm_df$kommentar,"(?=.*Beifall)(?=.*\\[.*\\])")]<- "Beifall"
  komm_df$type[str_detect(komm_df$kommentare,"(?=.*Beifall)(?=.*\\[.*\\])")]<- "Beifall"
  komm_df$type[str_detect(komm_df$kommentare,"(?=.*Heiterkeit)(?=.*\\[.*\\])")]<- "Heiterkeit"
  komm_df$type[str_detect(komm_df$kommentare,"(?=.*Zuruf)(?=.*\\[.*\\])")]<- "Zuruf"
  komm_df %<>% group_by(kommentar)
  komm_df %<>% unnest()
  komm_df$kommentar <- str_split(komm_df$kommentar, "sowie | und ")
  komm_df$type %<>% as.character()
  komm_df %<>% group_by(type)
  komm_df %<>% unnest()
  komm_df$party <- str_extract_all(komm_df$kommentar, paste(party_regex, collapse = "|")) 
  komm_df %<>% group_by(kommentar)
  komm_df %<>% unnest()
  komm_df$party %<>% as.character()
  komm_df$party_action[str_detect(komm_df$ kommentar, ("[aA]bg\\w{0,9}"))] <- 0
  komm_df$party_action[is.na(komm_df$party_action)]<- 1
  
  
  # Join
  
  .<-left_join(reden_df, corpus, "top")
  big_corpus <- left_join(komm_df,., "speech_id")
  all_corpus <<- bind_rows(all_corpus, big_corpus)
  
}
# End of function--------------------------------------------
save(parse_nach17, file = "Parse-Funktionen/parse_nach17.R")

