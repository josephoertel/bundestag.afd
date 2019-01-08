# Zeppelin Projekt
## Veränderung der Kommunikation im Bundestag seit dem Einzug der AfD 2017

Die Auswertungen sind in folgender Form organisiert: 

Die Datei *Stammdaten* enthält alle Stammdaten der Politiker 

Der Ordner *Ergebnisdaten* enthält alle Korpora (Reden von 2009 - 2017, sowie jeweils ein Jahr nach Legislaturperioden beginn) 

Der Ordner *Manuscript* enthält alle Grafiken, etc. und das LaTeX Dokument, so dass alle Grafiken, gleich in LateX mit verändert werden

Die Ordner *reden* enthalten die XML-Files 

Der Ordner *Parse-Funktionen* enthält die zwei Funktionen, einmal für vor2017 mit RegeX und einmal nach2017 mit XML-Nodes. Außerdem sind die Skripte für die Funktionen und die Syntax *daten-laden* enthalten, die die Funktionen anwendet und noch Parteien und Datum anpasst. Die Syntax *daten-laden* auszuführen dauert etwa 3 Studen, daher ist der Output davon in *Ergebnisdaten* gespeichert.  

Der Ordner *Syntax* enthält alle Skripte nach Aufgaben aufgeteilt:
  * *auswertung_sprache.R* enthält das Skript, das die Exel-Datei für Vivians Hypothese erstellt hat 
  * *auswertung_stichprobe_inhalt.R* enthält die Auswertung der Stichprobe der inhaltlichen Kodierung 
  * *Erstellung_Stichprbe_Inhalt.R* ist das Skript, das die inhaltliche Stichprobe gezogen und auf die Kodierer aufgeteilt hat 
  * *Erstellung_Stichprbe_Kommentare.R* ist das Skript, das die Stichprobe zu den Zwischenrufen gezogen hat.
  * *automatischeThemenanalyse.R* enthält das Skript, das mit Latent-Semantic-Scaling themenspezifische Wörterbücher erstellt und angewendet hat
  * *Readability_Berechnung.R* enthält die Berechnung vom Readability Index nach FLESCH 
  * *Reli-Test.R* enthält den Reliabilitätstest 
  * *visualisierung_afd_isolierung.R* enthält die quantitativen Visualisierung der Interaktionen im Bundestag
  * *visualisierung_wortwahl_afd.R* enthält die quanteda funktionen mit denen die relative Worthäufigkeit berechnet wurde 
  
 

