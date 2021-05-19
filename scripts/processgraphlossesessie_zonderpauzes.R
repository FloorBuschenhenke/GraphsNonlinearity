## process graph losse sessie - zonderpauze-data

 library(tidyverse)
 library(ggthemes)
 library(hrbrthemes)
library(firatheme)
 
 options(scipen = "999")
 
 #https://github.com/vankesteren/firatheme
 
## theme_set(theme_bw())  past het voor alle figuren in de file aan (in de werksesssie?)
 
datazonderpauzes <- read.csv("data_out/pauzefiltereddata.csv", stringsAsFactors = F)

## deze werkt op losse sessies

colnames(datazonderpauzes)

oefensessietje <- datazonderpauzes %>%
  filter(participant == 'Zorah') %>%
  filter(session_number == 6 | session_number == 7 | session_number == 8)%>%
  mutate(time_minutes = spell_end/60000)



ggplot(oefensessietje, aes(time_minutes, positionFull))+
  geom_line(aes(time_minutes, positionFull), colour = "violet")+ 
  geom_line(aes(time_minutes, doclengthFull), colour = "blue")+
  geom_line(aes(time_minutes, charProduction), colour = "black")+
  labs(x = "time_minutes", y = "characters", title = "process graph Zorah", subtitle = "cursorposition = violet, doclength = blue, characterproduction = black")+
  facet_wrap(~session_number)+theme_fira()


#linetype = "dotdash"  voor stippellijntje

view(oefensessietje)

oefensessietje <- oefensessietje %>%
  mutate (session_number = as.character(session_number))

#oefen scatterplot voor de firakleurtjes

ggplot(oefensessietje, aes(time_minutes, doclengthFull, color = session_number))+
  geom_point()+theme_fira()+ scale_color_fira()


                                  