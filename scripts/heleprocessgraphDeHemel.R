# holistische process graph - DeHemel
library(tidyverse)
### eerst chrononummer aan hele GA plakken met left_join

datazonderpauzes <- read.csv('data/pauzefiltereddata.csv')
summaryTekst <- read.csv("data/summaryTekst.csv", stringsAsFactors = F)
DeHemelalles <- datazonderpauzes %>%
  filter(participant == "De Hemel")



chrononumDeHemel <- summaryTekst %>%
  filter(participant == "De Hemel")%>%

  group_by(session_number)%>%
  select(session_number, chrononumber)

#view(chrononumDeHemel)

DeHemelalles2 <- DeHemelalles %>%
  left_join(chrononumDeHemel, by= "session_number")


# view(DeHemelalles2)
## ok, dan optellende holistische event_eindtijden toevoegen


DeHemelalles2$Eindtijd2 <- DeHemelalles2$spell_end


#NA's in de chrononumbers > 
DeHemelallesNA <- DeHemelalles2 %>%
  filter(is.na(chrononumber) == TRUE)
view(DeHemelallesNA)

# eerste stukje van eerste sessie lijkt het.. 

DeHemelalles2 <- DeHemelalles2%>%
 filter(is.na(chrononumber) == FALSE)


DeHemelalles2 <- as.data.frame(DeHemelalles2)
#tweede sessie van de set data noemen

for (i in 2:nrow(DeHemelalles2)) { 
  
  prev <- nrow(DeHemelalles2[DeHemelalles2$chrononumber==i-1,])
  
  DeHemelalles2[DeHemelalles2$chrononumber==i,]$Eindtijd2 <- DeHemelalles2[DeHemelalles2$chrononumber==i,]$endTime_Fixed + DeHemelalles2[DeHemelalles2$chrononumber==i-1,]$Eindtijd2[prev]
}

#summary(DeHemelalles2)


#bij focus events staat positionFull op 0 (dat lijkt nonlini maar is het dus niet)
#focus events nog op een andere manier laten zien in de grafiek
DeHemelalles3 <- DeHemelalles2 %>%
  filter(type != "focus")%>%
mutate(eindtijdInMinu = Eindtijd2/60000)
  
#colnames(DeHemelalles3)    


#########  plaatje 

library(firatheme)

graphDeHemel <- ggplot(DeHemelalles3, aes(eindtijdInMinu, positionFull))+
  geom_line(aes(eindtijdInMinu, positionFull), colour = firaCols[2])+
  geom_line(aes(eindtijdInMinu, doclengthFull), colour = firaCols[1])+
  geom_line(aes(eindtijdInMinu, charProduction), colour = firaCols[3])+
  labs(y = "characters", x = "Time in minutes", title = "process graph DeHemel", subtitle = "vertical bars represent sessions",
       tag = "pink line = cursor position\n blue line = document length\n green line = characters produced")+theme_fira()
 
graphDeHemel
#, subtitle = "vertical bars represent sessions",
#tag = "pink line = cursor position\n blue line = document length\n green line = characters produced"

linesessions = DeHemelalles3 %>%
  group_by(chrononumber) %>%
  summarise(xvalue=abs(max(eindtijdInMinu)))

graphDeHemel + geom_vline(data = linesessions, aes(xintercept = xvalue), color = firaCols[5])+
  geom_text(mapping = aes(x = xvalue,
                          y = 0,
                          label = chrononumber,
                          hjust = 1.5,
                          vjust = 0.5),
            data = linesessions, colour = firaCols[5])
               

#######losse sessies 

lossesessie <- DeHemelalles3 %>%
  filter(chrononumber == 5)

graphDeHemellos <- ggplot(lossesessie, aes(eindtijdInMinu, positionFull))+
  geom_line(aes(eindtijdInMinu, positionFull), colour = firaCols[2])+
  geom_line(aes(eindtijdInMinu, doclengthFull), colour = firaCols[1])+
  geom_line(aes(eindtijdInMinu, charProduction), colour = firaCols[3])+
  labs(y = "characters", x = "Time in minutes", title = "process graph DeHemel - 5th session",
       tag = "pink line = cursor position\n blue line = document length\n green line = characters produced")+theme_fira()

graphDeHemellos

