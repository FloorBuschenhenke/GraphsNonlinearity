# holistische process graph - Zorah
library(tidyverse)
# eerst chrononummer aan hele GA plakken met left_join

datazonderpauzes <- read.csv('pauzefiltereddata.csv')
summaryTekst <- read.csv("data_out/summaryTekst.csv", stringsAsFactors = F)
Zorahalles <- datazonderpauzes %>%
  filter(participant == "Zorah")


chrononumZorah <- summaryTekst %>%
  filter(participant == "Zorah")%>%
  group_by(session_number)%>%
  select(session_number, chrononumber)

view(chrononumZorah)

Zorahalles2 <- Zorahalles %>%
  left_join(chrononumZorah, by= "session_number")


# view(Zorahalles2)
## ok, dan optellende holistische event_eindtijden toevoegen


Zorahalles2$Eindtijd2 <- Zorahalles2$spell_end


#NA's in de chrononumbers > 
ZorahallesNA <- Zorahalles2 %>%
  filter(is.na(chrononumber) == TRUE)
view(ZorahallesNA)

# geen NA's

##Zorahalles2 <- Zorahalles2%>%
 ## filter(is.na(chrononumber) == FALSE)


Zorahalles2 <- as.data.frame(Zorahalles2)
#tweede sessie van de set data noemen

for (i in 2:nrow(Zorahalles2)) { 
  
  prev <- nrow(Zorahalles2[Zorahalles2$chrononumber==i-1,])
  
  Zorahalles2[Zorahalles2$chrononumber==i,]$Eindtijd2 <- Zorahalles2[Zorahalles2$chrononumber==i,]$endTime_Fixed + Zorahalles2[Zorahalles2$chrononumber==i-1,]$Eindtijd2[prev]
}

summary(Zorahalles2)


#bij focus events staat positionFull op 0 (dat lijkt nonlini maar is het dus niet)
#focus events nog op een andere manier laten zien in de grafiek
Zorahalles3 <- Zorahalles2 %>%
  filter(type != "focus")%>%
mutate(eindtijdInMinu = Eindtijd2/60000)
  
#colnames(Zorahalles3)    

graphZorah <- ggplot(Zorahalles3, aes(eindtijdInMinu, positionFull))+
  geom_line(aes(eindtijdInMinu, positionFull), colour = "green")+
  geom_line(aes(eindtijdInMinu, doclengthFull), colour = "violet")+
  geom_line(aes(eindtijdInMinu, charProduction), colour = "black")+
  labs(y = "characters", title = "process graph Jente Postuma")
 
graphZorah
#, subtitle = "verticale strepen & blauwe nummers zijn sessies",
#tag = "green line = cursor position\n violet line = document length\n black line = characters produced"

linesessions = Zorahalles3 %>%
  group_by(chrononumber) %>%
  summarise(xvalue=abs(max(eindtijdInMinu)))

graphZorah + geom_vline(data = linesessions, aes(xintercept = xvalue))+
  geom_text(mapping = aes(x = xvalue,
                          y = 0,
                          label = chrononumber,
                          hjust = 1.5,
                          vjust = 0.5),
            data = linesessions, colour = 'blue')
               


