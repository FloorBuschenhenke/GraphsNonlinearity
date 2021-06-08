## visualising chronology of event_types in sessions

library(tidyverse)
library(firatheme)
theme_set(theme_fira())

grotetabelzonderoutliers <- read.csv("grotetabelzonderoutliers.csv")
names(grotetabelzonderoutliers)
## dit is nog met slechte duur voor focus (en weggefilterde mouseevents daarbinnen)

testje <- grotetabelzonderoutliers %>%
  filter(participant == "Emilia", chrononumber == 5)

ggplot(testje, aes(jump_number, jump_duration, fill = event_type))+ geom_col()

##
options(scipen = "999")

ggplot(testje, aes(jump_number, jump_duration, colour = event_type))+ 
  geom_jitter()+
   geom_rug( position = "jitter", size = 0.9, length = unit(0.1, "npc"), outside = T )+
  coord_cartesian(clip = "off")+ theme(
    axis.text.x = element_blank())+
  labs(title = "Emilia's process, 5th session", subtitle = "events are ordered chronologically along the x-axis",
       x = "")+
  scale_color_fira()
 
#geom_point() 

 # scale_y_continuous(expand = c(0.1, 0.1))
# voor in de rug >
## length = unit(0.05, "npc")
## alpha = transparantie
# colour kan je bij rug op zwart zetten bijv