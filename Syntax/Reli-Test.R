library(tidyverse)
library(readxl)
library(xtable)
d1 = read_excel("Ergebnisdaten/Fertige_Stichproben/nils_inhalt.xlsx")
d2 = read_excel("Ergebnisdaten/Fertige_Stichproben/vivian_inhalt.xlsx")
d3 = read_excel("Ergebnisdaten/Fertige_Stichproben/mehmet_inhalt.xlsx")



# Speech_ID korrigieren

for (i in 1:length(d1$speech_id)) {
  if (d1[i, "date"] < as.Date("22-10-2013", "%d-%m-%Y")) 
    d1[i, "speech_id"]<- paste0(gesamt_inhalt[i, "speech_id"], "_09")
}

for (i in 1:length(d2$speech_id)) {
  if (d2[i, "date"] < as.Date("22-10-2013", "%d-%m-%Y")) 
    d2[i, "speech_id"]<- paste0(gesamt_inhalt[i, "speech_id"], "_09")
}

for (i in 1:length(d3$speech_id)) {
  if (d3[i, "date"] < as.Date("22-10-2013", "%d-%m-%Y")) 
    d3[i, "speech_id"]<- paste0(gesamt_inhalt[i, "speech_id"], "_09")
}






nils = d1 %>% mutate(redeinhalt = str_split(X_Redeinhalt, ",|\\.")) %>% unnest(redeinhalt) %>%
  mutate(redeinhalt = str_c("v", str_trim(redeinhalt)), value = 1, coder = "Nils") %>%
  distinct(speech_id, redeinhalt, .keep_all = T) %>%
  spread(redeinhalt, value, fill = 0) %>%
  select(coder, speech_id, v101:v300)


vivian = d2 %>% mutate(redeinhalt = str_split(X_Redeinhalt, ",|\\.")) %>% unnest(redeinhalt) %>%
  mutate(redeinhalt = str_c("v", str_trim(redeinhalt)), value = 1, coder = "Vivian") %>%
  distinct(speech_id, redeinhalt, .keep_all = T) %>%
  spread(redeinhalt, value, fill = 0) %>%
  select(coder, speech_id, v101:v300)

mehmet = d3 %>% mutate(redeinhalt = str_split(X_Redeinhalt, ",|\\.")) %>% unnest(redeinhalt) %>%
  mutate(redeinhalt = str_c("v", str_trim(redeinhalt)), value = 1, coder = "Mehmet") %>%
  distinct(speech_id, redeinhalt, .keep_all = T) %>%
  spread(redeinhalt, value, fill = 0) %>%
  select(coder, speech_id, v101:v300)

d = bind_rows(nils, vivian, mehmet)
names(d)
# Pro Variable (hier v101) ändern

reli <- data.frame()
for (i in 3:27) {
  t<- d %>% select(coder, speech_id, names(d)[i]) %>% spread(coder, names(d)[i]) %>%
    summarise(variable = names(d)[i],
              Rel_Mehmet_Nils = mean(Mehmet == Nils, na.rm = T),
              Rel_Mehmet_Vivian = mean(Mehmet == Vivian, na.rm = T),
              Rel_Vivian_Nils = mean(Vivian == Nils, na.rm = T),
              Durchschnitt = mean(c(Rel_Mehmet_Vivian, Rel_Mehmet_Vivian, Rel_Vivian_Nils)))
  reli <<- bind_rows(t, reli)
  }

xtable(reli)
