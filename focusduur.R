### focusduur berekenen ####




# grote fan van tidyverse

library(tidyverse)

### bronbestand inkorten voor Hayco ####

grotetabel <- read.csv("data_out/grotetabel.csv", stringsAsFactors = F)

grotetabelFocusduur <- grotetabel %>%
  #oefenen met 1 sessie
  filter(participant == "De Hemel", session_number == 2)%>%

  select(participant, session_number, jump_number, start_position, end_position, start_time, 
         end_time, jump_duration, jump_size_charsPlus, jump_type)%>%
  rename(event_type = jump_type,
         event_size_in_chars = jump_size_charsPlus,
         event_duration = jump_duration)%>%
  slice_head(n = 127)

# tabel bekijken

view(grotetabelFocusduur)

# eigenlijk heb ik meerdere sessies van meerdere deelnemers
# de code zou dus met een gegroepeerde dataset op elke sessie moeten worden uitgevoerd


write.csv(grotetabelFocusduur, "testdataFocusduur.csv")

### Vanaf hier kolom toevoegen #####

# inlezen testtabel
focusduur <- read.csv("testdataFocusduur.csv", stringsAsFactors = F)

# van alle acties (onder event_type) heb ik de duur in ms berekend (event_duration)
# Bij de focus-events is dat lastig, omdat deze eerst nog afgebakend moeten worden.
# als er bij event_type 'focus' staat, betekent dat dat er een wissel is in werkscherm. 
# de schrijver gaat dan ofwel in ofwel uit zijn werkdocument
# als de regels die op zo'n focusevent volgen een lege 'event_size_in_chars' hebben
# dan horen ze bij het bovenstaande focusevent 
# de eerste regel eronder waarbij er weer een cijfertje in de 'event_size_in_chars' staat
# is dan de grens van het focusevent (dan is de schrijver weer in het werkdocument actief.)
# er zijn ook focusevents van maar 1 regel (maar niet in dit stukje tabel)

# In deze tabel zitten 2 focusevents: eentje vanaf jumpnumber 21 t/m jumpnummer 33 
# en eentje vanaf jumpnr 103 t/m jumpnr 127. 

# per focusevent wil ik graag de duur weten om toe te voegen aan de event_duration kolom
#
# end_time laatste regel die erbij hoort - start_time eerste regel die erbij hoort
# of optellen van event_duration waardes van alle regels die bij dat event horen natuurlijk


# Ik wil ook een nieuwe kolom event_type2 waarbij elk (geaggregeerd) focusevent 1 regel beslaat
# dus de regels die er nu los onder staan helemaal weg

# ehh dat betekent dat de Jump_number kolom dan ook opnieuw aangemaakt moet worden 
# maar dat kan ik zelf.

  
  
  
