library(tidyverse)
library(magrittr)
library(lubridate)
library(xtable)

# Datensätze Laden 

load("Ergebnisdaten/korpus09_13.RData")
reden09<- all_reden

load("Ergebnisdaten/korpus13_17.RData")
reden13 <- all_reden

load("Ergebnisdaten/korpus_okt17_okt18.RData")
reden17 <- all_corpus

all_corpus <- NULL
all_reden<- NULL





# Nach Parteien sortieren 
party_regex<- c("SPD", "CDU\\/CSU", "AfD", "DIE LINKE", "FDP", "LINKEN", "B.NDNIS.90\\/DIE.GR.NEN", "B.NDNIS.90\\/ DIE.GR.NEN", "Grüne")


reden09$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))
reden09$party         %<>% str_extract(paste(party_regex, collapse = "|"))
reden13$speaker_party %<>% str_extract(paste(party_regex, collapse = "|"))
reden13$party         %<>% str_extract(paste(party_regex, collapse = "|"))


# Party Vector
party_vector_09 <- reden09$party %>% unique()
party_vector_13 <- reden13$party %>% unique()
party_vector_17 <- reden17$party %>% unique()
party_vector_13<- party_vector_13[!party_vector_13 %in% c("AfD","FDP")]
party_vector <- party_vector_17



# Tabellen erstellen: 
Tabelle_17 <- reden17 %>% 
  filter(party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party, type) %>% 
  spread(type, n) %>%
  mutate(sum = rowSums(.[2:6])) %>% 
  arrange(desc(sum))
Tabelle_13 <- reden13 %>% 
  filter(party %in% party_vector_13, speaker_party %in% party_vector_13) %>% 
  count(party, type) %>% 
  spread(type, n) %>%
  mutate(sum = rowSums(.[2:6])) %>% 
  arrange(desc(sum))
Tabelle_09 <- reden09 %>% 
  filter(party %in% party_vector_09, speaker_party %in% party_vector_09) %>% 
  count(party, type) %>% 
  spread(type, n) %>%
  mutate(sum = rowSums(.[2:6])) %>% 
  arrange(desc(sum))

tabell_all <- bind_rows(Tabelle_17,Tabelle_13, Tabelle_09)
tabell_all <- tabell_all[-c(17,18,11), 1:8]
tabell_all$Periode <- c("17-18","17-18","17-18","17-18","17-18","17-18","13-17","13-17","13-17","13-17","09-13","09-13","09-13","09-13","09-13")
# xtable(tabell_all, include.rownames = FALSE)

## Wer klatscht für wen?

df <- reden17

df_beifall_self <- df %>%  
  filter(type == "Beifall", party == speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party) %>% 
  rename(self = n)

df_beifall_others <- df %>% 
  filter(type == "Beifall", party != speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party)  %>% 
  rename(others = n)

df_beifall_all <- df %>%
  filter(type == "Beifall", party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party) %>% 
  rename(all = n)

  
  df_beifall_all %>% 
  left_join(df_beifall_self, by = "party") %>% 
  left_join(df_beifall_others, by = "party") %>% 
  mutate(share_self = self/all,
         share_others = others/all) %>% 
  arrange(desc(share_self)) %>% 
  mutate(order = row_number()) %>% 
  tidyr::gather(type, factor, 5:6) %>% 
  ggplot(aes(x = reorder(party, -order), y = factor, group = type, fill = type)) +
  geom_bar(stat = "identity",colour="black") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Anteil des Beifalls für die eigene Fraktion 2017-2018")+
    theme(axis.title=element_blank())+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    scale_fill_manual(values=c("gray84", "grey37"))+
    theme(legend.title = element_blank())
  ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/17_eigenklatschanteil.png")
  
  df <- reden13
  
  df_beifall_self <- df %>%  
    filter(type == "Beifall", party == speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
    count(party) %>% 
    rename(self = n)
  
  df_beifall_others <- df %>% 
    filter(type == "Beifall", party != speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
    count(party)  %>% 
    rename(others = n)
  
  df_beifall_all <- df %>%
    filter(type == "Beifall", party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
    count(party) %>% 
    rename(all = n)
  
  
  df_beifall_all %>% 
    left_join(df_beifall_self, by = "party") %>% 
    left_join(df_beifall_others, by = "party") %>% 
    mutate(share_self = self/all,
           share_others = others/all) %>% 
    arrange(desc(share_self)) %>% 
    mutate(order = row_number()) %>% 
    tidyr::gather(type, factor, 5:6) %>% 
    ggplot(aes(x = reorder(party, -order), y = factor, group = type, fill = type)) +
    geom_bar(stat = "identity",colour="black") +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(title = "Anteil des Beifalls für die eigene Fraktion 2013-2017")+
    theme(axis.title=element_blank())+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    scale_fill_manual(values=c("gray84", "grey37"))+
    theme(legend.title = element_blank())
  #ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/13_eigenklatschanteil.png")  
  
  ## Wer klatscht für wen? 
  
  Tabelle_Klatschen09 <- reden09 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Beifall") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  Tabelle_Klatschen09$leg <- "Legislaturperiode 09-13"
  
  Tabelle_Klatschen13 <- reden13 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Beifall") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  Tabelle_Klatschen13$leg <- "Legislaturperiode 13-17"
  
  Tabelle_Klatschen17 <- reden17 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Beifall") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  Tabelle_Klatschen17$leg <- "Legislaturperiode 17-18"
 
  Tabelle_Klatschen_ALL <- bind_rows(Tabelle_Klatschen09, Tabelle_Klatschen13, Tabelle_Klatschen17)
  
  Tabelle_Klatschen_all <- Tabelle_Klatschen_ALL %>% group_by(leg, speaker_party) %>% mutate(sum_n = sum(n)) %>% mutate(perc = n/sum_n)
  
  Tabelle_Klatschen_ALL %>%
  ggplot(aes(party, n))+ 
    geom_bar(stat = "identity")+
    facet_grid(speaker_party~leg)+
    theme_bw()+
    theme(axis.title = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Vergleich des Beifalls über 3 Legislaturperioden")
  
  ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/13_17WerfürWen.png") 

  
  Tabelle_Klatschen_all <- Tabelle_Klatschen_ALL %>% group_by(leg, speaker_party) %>% mutate(sum_n = sum(n)) %>% mutate(perc = n/sum_n)
  
  Tabelle_Klatschen_all %>%
    ggplot(aes(x = party, y= perc))+ 
    geom_bar(stat = "identity")+
    facet_grid(speaker_party~leg)+
    geom_label(
    data    = Tabelle_Klatschen_all,
    mapping = aes(x = Inf, y = Inf, label = sum_n),stat = "identity",
    hjust   = 1.3,
    vjust   = 1.5
    )+
    theme_bw()+
    theme(axis.title = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Vergleich des Beifalls über 3 Legislaturperioden, prozentuale Aufteilung")
  
  ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/13_17WerfürWen_prozent.png") 
  
    
  
  
  
  
  
  
## Beifall für die Eigenen und für die Fremden Parteien im Zeitverlauf ## 

reden09$speech_id%<>% as.character()
reden13$speech_id%<>% as.character()


all_corpus <- reden17
df_beifall_self<- all_corpus %>% group_by(month=floor_date(date, "month")) %>% 
  filter(type == "Beifall", party == speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party) %>% 
  rename(self = n)

df_beifall_others <- all_corpus %>% group_by(month=floor_date(date, "month"))%>% 
  filter(type == "Beifall", party != speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party)  %>% 
  rename(others = n)


df_beifall_all <- all_corpus %>% group_by(month=floor_date(date, "month"))%>% 
  filter(type == "Beifall", party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party) %>% 
  rename(all = n)

df_beifall_all %>% 
  left_join(df_beifall_self, by = c("party","month")) %>% 
  left_join(df_beifall_others, by = c("party", "month")) %>% 
  mutate(share_self = self/all,
  share_others = others/all) %>% 
  arrange(desc(share_self)) %>%
  group_by(party) %>% 
  ggplot( aes(x = month, y = share_self, col = party)) +
  geom_line(na.rm = T, size = 2)+
  scale_color_manual(values = c("AfD" = "blue",
                                "FDP" = "yellow",
                                "GRÜNE" = "green",
                                "DIE LINKE" = "purple",
                                "SPD" = "red",
                                "CDU/CSU" = "black"),
                     name = "Parteien") + 
  labs(x = "Monate", 
       y = "Beifallsanteil für die eigene Partei",
       title = "Welche Partei klatscht für sich selbst am meisten?")+
  scale_y_continuous(labels=scales::percent)
ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/13_17Zeitverlauf_Klatschen.png") 

# Geschlossenheit der Parteien.

df <- reden17
df$party_action %<>% as.character()
  
 df_plot <- df %>% 
  filter(type == "Beifall", party != speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party, party_action) %>% 
  spread(party_action, n) %>%
  mutate(sum =  `0` +`1`) %>% 
  arrange(desc(sum)) %>% 
  mutate(order = row_number()) %>% 
  rename(ganze_partei = `0`, teile_der_partei = `1`) %>% 
  gather(party_action, value, -party, -sum, -order) 
 
 df_plot2 <- df_plot %>% spread(party_action,value) %>% mutate(perc = teile_der_partei/sum)
  
 
 ggplot(df_plot, aes(x = reorder(party, -order), y = value, group = party_action, fill = party_action)) +
  geom_bar(stat = "identity", colour = "black")+
  coord_flip() + 
  labs(title = "Wenn eine Partei für andere klatscht, wie oft klatscht dann die ganze Fraktion \n und wann nur Teile einer Fraktion?")+
   theme(axis.title=element_blank())+
   theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
   scale_fill_manual(values=c("gray84", "grey37"))+
   theme(legend.title = element_blank())

 ggplot(df_plot2, aes(x= reorder(party, perc), y = perc))+
  geom_bar(stat = "identity", colour = "black")+
coord_flip() + 
  labs(title = "Wenn eine Partei für andere klatscht, wie oft klatscht dann die ganze Fraktion \n und wann nur Teile einer Fraktion?")+
           theme(axis.title=element_blank())+
           theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
           scale_fill_manual(values=c("gray84", "grey37"))+
           theme(legend.title = element_blank())
  
  ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/Geschlossenheit17.png") 
  

df <- reden13
df$party_action %<>% as.character()

df_plot <- df %>% 
  filter(type == "Beifall", party != speaker_party, party %in% party_vector_17, speaker_party %in% party_vector_17) %>% 
  count(party, party_action) %>% 
  spread(party_action, n) %>%
  mutate(sum =  `0` +`1`) %>% 
  arrange(desc(sum)) %>% 
  mutate(order = row_number()) %>% 
  rename(ganze_partei = `0`, teile_der_partei = `1`) %>% 
  gather(party_action, value, -party, -sum, -order) 
  
df_plot2 <- df_plot %>% spread(party_action,value) %>% mutate(perc = teile_der_partei/sum)

  
  ggplot(aes(x = reorder(party, -order), y = value, group = party_action, fill = party_action)) +
  geom_bar(stat = "identity", colour = "black") +
  coord_flip() + 
  labs(title = "Wenn eine Partei für andere klatscht, wie oft klatscht dann die ganze Fraktion \n und wann nur Teile einer Fraktion?")+
  theme(axis.title=element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_fill_manual(values=c("gray84", "grey37"))+
  theme(legend.title = element_blank())


ggplot(df_plot2, aes(x= reorder(party, perc), y = perc))+
  geom_bar(stat = "identity", colour = "black")+
  coord_flip() + 
  labs(title = "Wenn eine Partei für andere klatscht, wie oft klatscht dann die ganze Fraktion \n und wann nur Teile einer Fraktion? (2013-2017)")+
  theme(axis.title=element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_fill_manual(values=c("gray84", "grey37"))+
  theme(legend.title = element_blank())
ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/Geschlossenheit13.png")

#########################################


# Welche Fraktion ruft wie oft einer anderen zu 2017?
  df_zuruf_17 <- reden17 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Zuruf") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  df_zuruf_17$leg <- "Legislaturperiode 17-18"
  
# Welche Fraktion ruft wie oft einer anderen zu 2013?
  df_zuruf_13 <- reden13 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Zuruf") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  df_zuruf_13$leg <- "Legislaturperiode 13-17"
  
# Welche Fraktion ruft wie oft einer anderen zu 2009?
  df_zuruf_09 <- reden09 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Zuruf") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  df_zuruf_09$leg <- "Legislaturperiode 09-13"

df_zuruf_all <- bind_rows(df_zuruf_09, df_zuruf_13, df_zuruf_17)
df_zuruf_all$type <- "Zuruf" 
  ## Wer über wen Lacht? 
  
  Tabelle_Klatschen09 <- reden09 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Lachen") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  Tabelle_Klatschen09$leg <- "Legislaturperiode 09-13"
  
  Tabelle_Klatschen13 <- reden13 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Lachen") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  Tabelle_Klatschen13$leg <- "Legislaturperiode 13-17"
  
  Tabelle_Klatschen17 <- reden17 %>% 
    filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Lachen") %>% 
    count(party, speaker_party) %>% 
    arrange(desc(n))
  Tabelle_Klatschen17$leg <- "Legislaturperiode 17-18"

  df_lachen_all <- bind_rows(Tabelle_Klatschen09, Tabelle_Klatschen13, Tabelle_Klatschen17)
df_lachen_all$type <- "Lachen"


# Welche Fraktion ruft wie oft einer anderen zu 2017?
df_zuruf_17 <- reden17 %>% 
  filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Widerspruch") %>% 
  count(party, speaker_party) %>% 
  arrange(desc(n))
df_zuruf_17$leg <- "Legislaturperiode 17-18"

# Welche Fraktion ruft wie oft einer anderen zu 2013?
df_zuruf_13 <- reden13 %>% 
  filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Widerspruch") %>% 
  count(party, speaker_party) %>% 
  arrange(desc(n))
df_zuruf_13$leg <- "Legislaturperiode 13-17"

# Welche Fraktion ruft wie oft einer anderen zu 2009?
df_zuruf_09 <- reden09 %>% 
  filter(party %in% party_vector, speaker_party %in% party_vector, party != speaker_party, type == "Widerspruch") %>% 
  count(party, speaker_party) %>% 
  arrange(desc(n))
df_zuruf_09$leg <- "Legislaturperiode 09-13"

df_widerspruch_all <- bind_rows(df_zuruf_09, df_zuruf_13, df_zuruf_17)
df_widerspruch_all$type <- "Widerspruch"

df_negativ_all <- bind_rows(df_widerspruch_all, df_zuruf_all, df_lachen_all)

df_negativ_all_leg <- df_negativ_all %>% group_by(leg) %>% mutate(sum_n = sum(n)) %>% mutate(perc = n/sum_n)
df_negativ_all_perc <- df_negativ_all %>% group_by(leg, speaker_party) %>% mutate(sum_n = sum(n)) %>% mutate(perc = n/sum_n)


df_negativ_all_leg %>%
  ggplot(aes(x = party, y= perc, fill = type))+ 
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(speaker_party~leg)+
  geom_text(
    data    = df_negativ_all_leg,
    mapping = aes(x = Inf, y = Inf, label = sum_n),stat = "identity",
    hjust   = 1.3,
    vjust   = 1.5
  )+
  theme_bw()+
  theme(axis.title = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("blue4", "grey0", "gray73"))+
  labs(title = "Negative Aktionen, standardisiert auf Legislaturperiode")

ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/13_17negativ_leg.png") 


df_negativ_all_perc %>%
  ggplot(aes(x = party, y= perc, fill = type))+ 
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(speaker_party~leg)+
  geom_text(
    data    = df_negativ_all_perc,
    mapping = aes(x = Inf, y = Inf, label = sum_n),stat = "identity",
    hjust   = 1.3,
    vjust   = 1.5
  )+
  theme_bw()+
  theme(axis.title = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("blue4", "grey0", "gray73"))+
  labs(title = "Negative Aktionen, standardisiert auf Legislaturperiode und Partei")

ggsave(plot = last_plot(), filename = "Manuscript/All_Text/btspeeches_manuscript-master/Grafiken/13_17negativ_perc.png") 







      