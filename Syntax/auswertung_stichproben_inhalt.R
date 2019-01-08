library(xlsx)
library(tidyverse)
library(magrittr)
library(rlist)
library(purrr)

# Auswertung der inhaltlichen Stichprobe 



vivian <- read.xlsx(file ="Ergebnisdaten/Fertige_Stichproben/vivian_inhalt.xlsx", sheetIndex = 1, encoding = "UTF-8")
nils <- read.xlsx(file ="Ergebnisdaten/Fertige_Stichproben/nils_inhalt.xlsx", sheetIndex = 1, encoding = "UTF-8")
mehmet <- read.xlsx(file ="Ergebnisdaten/Fertige_Stichproben/mehmet_inhalt.xlsx", sheetIndex = 1, encoding = "UTF-8")

vivian$kodierer <- "vivian"
nils$kodierer <- "nils"
mehmet$kodierer <- "mehmet"


gesamt_inhalt <- bind_rows(vivian, nils, mehmet)%>% select(speech_id, speaker_party, date, rede = X_Redeinhalt, top = X_Topinhalt, kodierer)


# Speech_ID korrigieren

for (i in 1:length(gesamt_inhalt$speech_id)) {
  if (gesamt_inhalt[i, "date"] < as.Date("22-10-2013", "%d-%m-%Y")) 
    gesamt_inhalt[i, "speech_id"]<- paste0(gesamt_inhalt[i, "speech_id"], "_09")
}


# Visuelle Auswertung 
inhalt <- gesamt_inhalt[!duplicated(gesamt_inhalt$speech_id),]

inhalt$rede <- str_extract_all(inhalt$rede, paste(c("101","102", "103", "104", "105", "106", "107",
                                                                  "108", "109", "110", "111", "112", "113", "114",
                                                                  "115", "116", "200", "201", "202","203", "204", "205", 
                                                                  "300", "400", "500"
), collapse = "|"))
#inhalt%<>%group_by(speech_id)%>% unnest()

inhalt$top <-  str_extract_all(inhalt$top, paste(c("101","102", "103", "104", "105", "106", "107",
                                                                 "108", "109", "110", "111", "112", "113", "114",
                                                                 "115", "116", "200", "201", "202","203", "204", "205", 
                                                                 "300", "400", "500"
), collapse = "|"))
#inhalt%<>%group_by(speech_id)%>% unnest()


# factor lists 

list1 <- c("101",
           "102",
           "103",
           "104",
           "105",
           "106",
           "107",
           "108",
           "109",
           "110",
           "111",
           "112", 
           "113",
           "114",
           "115",
           "116",
           "119",
           "200",
           "201",
           "202",
           "203",
           "204",
           "205", 
           "206",
           "219",
           "300",
           "400",
           "500")


list2 <- c("Finanzen",
           "Wirtschaft und Arbeit",
           "Umwelt, Klima und Energiewirtschaft",
           "Ländlicher Raum und Agrapolitik", 
           "Verkehr und Infrastruktur",
           "Digitalisierung",
           "Soziales und Inklusion", 
           "Wohnungsbau",
           "Gesundheit und Pflege",
           "Kultur, Sport, Kunst und Medien",
           "Sicherheit",
           "Verbraucherschutz",
           "Migration und Integration",
           "Bildungspolitik und Jugend",
           "Wissenschaft und Forschung",
           "Justiz und Verfassung",
           "Innenpolitik Sonstiges",
           #######################
           "Außenpolitik",
           "Deutsche Außenpolitik nach Staat",
           "Deutsche Außenhandelspolitik",
           "Internationale Menschenrechte",
           "Internationale Finanzpolitik",
           "Internationale Sicherheitspolitik",
           "Internationale Entwicklungspolitik",
           "Außenpolitik Sonstiges",
           ####################
           "Interna des Bundestages",
           "Sonstiges",
           "Regierungserklärung")


party_regex<- c("SPD", "CDU\\/CSU", "AfD", "DIE LINKE", "FDP", "LINKEN", "B.NDNIS.90\\/DIE.GR.NEN", "B.NDNIS.90\\/ DIE.GR.NEN")
inhalt$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))

inhalt$speaker_party %<>%str_replace_all(.,"B.NDNIS.90\\/ DIE.GR.NEN", "BÜNDNIS 90/DIE GRÜNEN")

for (i in 1:length(inhalt$rede)){
  inhalt[i, "not_intop"] <- sum(!(unlist(inhalt[i ,"rede"])) %in% unlist(inhalt[i, "top"]))
}


inhalt2 <- inhalt[, -c(5,7)]
inhalt2 %<>% group_by(speech_id)%>% unnest()

inhalt2$rede.f <- inhalt2$rede %>% factor(., list1, list2)

# Inhalt: Visualisierung von Distanz zum TOP, inhalt2 zur Visualisierung der Themen 

# In Datum aufteilen 

top09 <- inhalt %>% filter(., inhalt$date < as.Date("22-10-2013", "%d-%m-%Y")) 
top13<- inhalt %>% filter(., inhalt$date < as.Date("22-10-2017", "%d-%m-%Y") & inhalt$date >= as.Date("22-10-2013", "%d-%m-%Y") ) 
top17 <- inhalt %>% filter(., inhalt$date > as.Date("22-10-2017", "%d-%m-%Y")) 

# In Datum aufteilen 

inhalt09 <- inhalt2[inhalt2$date < as.Date("22-10-2013", "%d-%m-%Y"),] 
inhalt13 <- inhalt2[inhalt2$date < as.Date("22-10-2017", "%d-%m-%Y") & inhalt2$date >= as.Date("22-10-2013", "%d-%m-%Y"),] 
inhalt17 <- inhalt2[inhalt2$date > as.Date("22-10-2017", "%d-%m-%Y"),]


# Distanz zum Top 
w<- top09 %>% group_by(speaker_party)%>% summarise(not_intop = sum(not_intop), reden = length(unlist(rede)), perc = not_intop/reden)
w$legislaturperiode <- "09"

t<- top13 %>% group_by(speaker_party)%>% summarise(not_intop = sum(not_intop), reden = length(unlist(rede)), perc = not_intop/reden)
  t$legislaturperiode <- "13"

z<- top17 %>% group_by(speaker_party)%>% summarise(not_intop = sum(not_intop), reden = length(unlist(rede)), perc = not_intop/reden)
  z$legislaturperiode <- "17"

g<- bind_rows(w, t, z )
g <- g[-16,]

ggplot(g, aes(x= speaker_party, y=perc, fill = legislaturperiode))+
  geom_bar(stat= "identity", position = "dodge", width = 0.5, color = "black")+
  scale_fill_manual(values=c("blue4", "grey0", "gray73"))+
  coord_flip(ylim=c(0.33,0.74))+
  theme_bw()+
  theme(axis.title = element_blank())
  
 

ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/distop.png")
# Visualisierung der Themen Inhalte 2009

t<- table(inhalt09$rede.f) %>% as.data.frame() %>% arrange(desc(Freq)) %>% mutate(order = row_number())
z<- t$Freq %>% sum()
t %>%  
ggplot(aes(x = reorder(Var1, -order), y = Freq/z)) + 
  geom_bar(stat="identity",colour="black")+ 
  theme(axis.title=element_blank())+
  coord_flip()+
  ylim(0.0, 0.18)+
  theme_bw()+
  theme(text = element_text(size=15))+
  theme(axis.title = element_blank())
ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/Inhalt09.png")
  
t<- table(inhalt13$rede.f) %>% as.data.frame() %>% arrange(desc(Freq)) %>% mutate(order = row_number()) 
z<- t$Freq %>% sum()
t %>%  
  ggplot(aes(x = reorder(Var1, -order), y = Freq/z)) + 
  geom_bar(stat="identity",colour="black")+ 
  theme(axis.title=element_blank())+
  coord_flip()+
  ylim(0.0, 0.18)+
  theme_bw()+
  theme(text = element_text(size=15))+
  theme(axis.title = element_blank())
ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/Inhalt13.png")


t<- table(inhalt17$rede.f) %>% as.data.frame() %>% arrange(desc(Freq)) %>% mutate(order = row_number()) 
z<- t$Freq %>% sum()
t %>%  
  ggplot(aes(x = reorder(Var1, -order), y = Freq/z)) + 
  geom_bar(stat="identity",colour="black")+ 
  theme(axis.title=element_blank())+
  ylim(0.0, 0.18)+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size=15))+
  theme(axis.title = element_blank())
ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/Inhalt14.png")




