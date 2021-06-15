# holistische process graph - emilia
library(tidyverse)
### eerst chrononummer aan hele GA plakken met left_join

datazonderpauzes <- read.csv('data/pauzefiltereddata.csv')
summaryTekst <- read.csv("data/summaryTekst.csv", stringsAsFactors = F)
Emiliaalles <- datazonderpauzes %>%
  filter(participant == "Emilia")



chrononumEmilia <- summaryTekst %>%
  filter(participant == "Emilia")%>%

  group_by(session_number)%>%
  select(session_number, chrononumber)

#view(chrononumEmilia)

Emiliaalles2 <- Emiliaalles %>%
  left_join(chrononumEmilia, by= "session_number")


write.csv(Emiliaalles2, "emiliaprocessgraphshiny.csv", row.names = F)



# view(Emiliaalles2)
## ok, dan optellende holistische event_eindtijden toevoegen


Emiliaalles2$Eindtijd2 <- Emiliaalles2$spell_end


#NA's in de chrononumbers > 
EmiliaallesNA <- Emiliaalles2 %>%
  filter(is.na(chrononumber) == TRUE)
view(EmiliaallesNA)

# eerste stukje van eerste sessie lijkt het.. 

Emiliaalles2 <- Emiliaalles2%>%
 filter(is.na(chrononumber) == FALSE)


Emiliaalles2 <- as.data.frame(Emiliaalles2)
#tweede sessie van de set data noemen

for (i in 2:nrow(Emiliaalles2)) { 
  
  prev <- nrow(Emiliaalles2[Emiliaalles2$chrononumber==i-1,])
  
  Emiliaalles2[Emiliaalles2$chrononumber==i,]$Eindtijd2 <- Emiliaalles2[Emiliaalles2$chrononumber==i,]$endTime_Fixed + Emiliaalles2[Emiliaalles2$chrononumber==i-1,]$Eindtijd2[prev]
}

summary(Emiliaalles2)


#bij focus events staat positionFull op 0 (dat lijkt nonlini maar is het dus niet)
#focus events nog op een andere manier laten zien in de grafiek
Emiliaalles3 <- Emiliaalles2 %>%
  filter(type != "focus")%>%
mutate(eindtijdInMinu = Eindtijd2/60000)
  
#colnames(Emiliaalles3)    


#########  plaatje 

library(firatheme)

graphEmilia <- ggplot(Emiliaalles3, aes(eindtijdInMinu, positionFull))+
  geom_line(aes(eindtijdInMinu, positionFull), colour = firaCols[2])+
  geom_line(aes(eindtijdInMinu, doclengthFull), colour = firaCols[1])+
  geom_line(aes(eindtijdInMinu, charProduction), colour = firaCols[3])+
  labs(y = "characters", x = "Time in minutes", title = "process graph Emilia", subtitle = "vertical bars represent sessions",
       tag = "pink line = cursor position\n blue line = document length\n green line = characters produced")+theme_fira()
 
graphEmilia
#, subtitle = "vertical bars represent sessions",
#tag = "pink line = cursor position\n blue line = document length\n green line = characters produced"

linesessions = Emiliaalles3 %>%
  group_by(chrononumber) %>%
  summarise(xvalue=abs(max(eindtijdInMinu)))

graphEmilia + geom_vline(data = linesessions, aes(xintercept = xvalue), color = firaCols[5])+
  geom_text(mapping = aes(x = xvalue,
                          y = 0,
                          label = chrononumber,
                          hjust = 1.5,
                          vjust = 0.5),
            data = linesessions, colour = firaCols[5])
               

#######losse sessies 

lossesessie <- Emiliaalles3 %>%
  filter(chrononumber == 5)

graphEmilialos <- ggplot(lossesessie, aes(eindtijdInMinu, positionFull))+
  geom_line(aes(eindtijdInMinu, positionFull), colour = firaCols[2])+
  geom_line(aes(eindtijdInMinu, doclengthFull), colour = firaCols[1])+
  geom_line(aes(eindtijdInMinu, charProduction), colour = firaCols[3])+
  labs(y = "characters", x = "Time in minutes", title = "process graph Emilia - 5th session",
       tag = "pink line = cursor position\n blue line = document length\n green line = characters produced")+theme_fira()

graphEmilialos

