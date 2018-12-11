library(tidyverse)
library(magrittr)
library(xlsx)

load("Ergebnisdaten/korpus09_13.RData")

reden09 <- all_reden

load("Ergebnisdaten/korpus13_17.RData")

reden13 <- all_reden

load("Ergebnisdaten/korpus_okt17_okt18.RData")

reden17 <- all_corpus

reden13$speech_id %<>% as.character()
reden09$speech_id %<>% as.character()

all_corpus <- bind_rows(reden09, reden13, reden17)

all_reden.x <- all_corpus %>% filter(type %in% c("Zuruf","Widerspruch","Kommentar","Lachen"))
all_reden.y <- all_reden.x %>% group_by(speech) %>% summarise(n_zwischenrufe = n())
reden_corpus<- all_corpus %>% select(speaker_party, date, speech, speech_id)%>% distinct()
reden_corpus <- left_join(reden_corpus, all_reden.y, by = "speech")

all_corpus <- NULL
reden09 <- NULL
reden13 <- NULL
reden17 <- NULL
all_reden <- NULL 
# Anzahl ausrufezeichen.


for (i in 1:length(reden_corpus$speech)){
  
  worte <- reden_corpus$speech[i] %>% quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% tokens_tolower() %>% unlist()
  reden_corpus[i,"anzahl_ausrufezeichen"] <- length(unlist((str_extract_all(reden_corpus[i,"speech"], "\\!"))))
  reden_corpus[i, "anzahl_wörter"] <- length(unlist(str_split(reden_corpus[i,"speech"], " ")))
  reden_corpus[i, "anzahl_sätze"] <- length(unlist(str_extract_all(reden_corpus[i,"speech"], "\\!|\\?|\\.")))
  reden_corpus[i, "anzahl_fragezeichen"] <- length(unlist(str_extract_all(reden_corpus[i,"speech"], "\\?")))
  reden_corpus[i, "anzahl_kommas"] <- length(unlist(str_extract_all(reden_corpus[i,"speech"], ",")))
  reden_corpus[i, "n_komm_friedlich"] <- length(which(worte %in% liste_freundlich))
  reden_corpus[i, "n_komm_unfriedlich"] <- length(which(worte %in% liste_unfreundlich))
  
}
vivian <- reden_corpus %>% select(-speech)
rm(corpus)
rm(all_reden.x)
rm(all_reden.y)
rm(toks_sent)
rm(reden_corpus)

vivian$date %<>% as.character()


write_excel_csv(vivian, path = "Vivian.csv")

 
liste_freundlich <- c("bitte*" ,
                       "dank*" ,
                      "gerne",
                      "herzlich*",
                      "verehrte*",
                      "dankbar*",
                      "geehrte*",
                      "würde",
                      "könnte",
                      "würde",
                      "möchte",
                      "hätte",
                      "sollte" ,
                      "wäre",
                      "dürfte",
                      "dürfen",
                      "auseinandersetzung",
                      "konflikt",
                      "sorg*",
                      "fried*",
                      "respekt*",
                      "mensch*",
                      "betroffene*",
                      "hilf*",
                      "hoffnung*",
                      "gemeinsam*",
                      "zusammen*",
                      "ja",
                      "vertrauen*",
                      "trau*",
                      "stütze*",
                      "ausgleich*",
                      "angleich*",
                      "sinn*",
                      "denk*",
                      "gedanken*",
                      "perspektiv*",
                      "dafür*",
                      "fürsorg*",
                      "orientier*",
                      "bewusst*",
                      "zukunft",
                      "vorsorg*",
                      "berat*",
                      "sprechen",
                      "absprech*",
                      "sachlich*",
                      "fundiert*",
                      "ausgewogen",
                      "verhältnis*",
                      "möglichkeit*",
                      "interesse*",
                      "gleich*",
                      "ähnlich*",
                      "brücke*",
                      "verbind*",
                      "angleich*",
                      "zuhöre*",
                      "vorsicht*",
                      "umsicht*",
                      "geduld*",
                      "besonnen*",
                      "besinn*",
                      "gedanke*"

 )

liste_unfreundlich <- c("urteil*","schwäch*",
"schwach*","quatsch*",
"gerede","infam",
"recht","straf*",
"macht*","krimin*",
"inzucht","einfach*",
"lüge*","erfind*",
"erfunden","verharmlos*",
"schrott*","konsequenz*",
"popanz","alber*",
"märchen*","nein",
"gegenteil","kommunist*",
"nazi*","unverschämt*",
"schand*","blöd*",
"unsinn*","krieg*",
"kampf*","lust*",
"provo*","leist*",
"volk","falsch*",
"scheiter*","gewalt*",
"jawohl","sieg*",
"mach*","durchsetz*",
"durchgreife*","betrug",
"betrüg*","erschleiche*",
"erschwindel*","rausschmeiße*",
"rausschmiss","rauswerf*",
"vernicht*","endgültig",
"klar","angst",
"ekel*","migrant*",
"ausländer*","stärke",
"konsequen*","entartete*",
"wille","wollen",
"gerecht*","krank*",
"träum*","hass*",
"gott","behaupt*",
"behauptung*","unverschämt*",
"ahnung","propaganda",
"schwafel*","heuch*",
"abart*","beweis*",
"schlag*","schläg*",
"pack*","blind*",
"gehorsam*","verhöhn*",
"hohn","spott*",
"reiß*","zerreiß*",
"stech*","bodenlos*",
"frech*","aggressiv*",
"täter*")
