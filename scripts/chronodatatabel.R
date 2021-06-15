### chrononummer op alle sessies plakken
### werkt zo voor geen meter 

datazonderpauzes <- read.csv('data/pauzefiltereddata.csv')
summaryTekst <- read.csv("data/summaryTekst.csv", stringsAsFactors = F)

sumtekst2 <- summaryTekst %>%
  group_by(participant, session_number)%>%
  select(session_number, chrononumber)


datachrononumb <- datazonderpauzes %>%
  left_join(sumtekst2, by= "participant", "session_number")

head(datachrononumb)
