
## session summary of actions count


library(tidyverse)
library(firatheme)
theme_set(theme_fira())

grotetabelzonderoutliers <- read.csv("grotetabelzonderoutliers.csv")
names(grotetabelzonderoutliers)
## dit is nog met slechte duur voor focus (en weggefilterde mouseevents daarbinnen)

sessionsumActions <- grotetabelzonderoutliers %>%
  group_by(participant, chrononumber, event_type)%>%
  summarise(count = n())

view(sessionsumActions)

ggplot(sessionsumActions, aes(chrononumber, count, fill = event_type))+
  geom_col()+
  facet_wrap(~participant)+
  scale_fill_fira()

### alleen voor Emilia 

sessionsumActionsEmilia <- sessionsumActions%>%
  group_by(chrononumber)%>%
  filter(participant == "Emilia")%>%
   mutate(totallines = sum(count),
          perc = (count/totallines)*100,
          perc = round(perc, digits = 3))

view(sessionsumActionsEmilia)

### met percentage 
ggplot(sessionsumActionsEmilia, aes(chrononumber, perc, fill = event_type))+
  geom_col()+labs( title = "Session summary of Emilia's process", subtitle = "Percentages of actions within each event type",
                   x = "chronological session number", y = "percentages")+
  scale_fill_fira()+scale_x_continuous(breaks = seq(1,18,1))


### dit is met n()
ggplot(sessionsumActionsEmilia, aes(chrononumber, count, fill = event_type))+
  geom_col()+labs( title = "Session summary of Emilia's process", subtitle = "Count of actions",
                   x = "chronological session number")+
   scale_fill_fira()+scale_x_continuous(breaks = seq(1,18,1))


