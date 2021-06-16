## duration plaatjes maken - materiaal Emilia

# op basis van hayco's pythonscript focusduur berekend



library(tidyverse)

### materiaal selecteren  ####

grotetabel <- read.csv("grotetabel2.csv", stringsAsFactors = F)

grotetabelFocusduurEm <- grotetabel %>%
  #oefenen met 1 sessie
  filter(participant == "Emilia")%>%
  select(participant, session_number, chrononumber, jump_number, start_position, end_position, start_time, 
         end_time, jump_duration, jump_size_charsPlus, jump_type)%>%
  rename(event_type = jump_type,
         event_size_in_chars = jump_size_charsPlus,
         event_duration = jump_duration)


# tabel bekijken

view(grotetabelFocusduurEm)


write.csv(grotetabelFocusduurEm, "dataFocusduurEm.csv")

#### python en weer terug ####
### dit als input voor focusscript (jupyter notebook, 'focusduur'mapje op users/F/)
## de output van de python bewerking is de aggregate-file hiero 
## dit kan per schrijver gerund, hij ziet de sessiegrenzen


focusfeestEm1 <- read.csv('aggregateEm.csv', stringsAsFactors = F)
 focusfeestEm <- focusfeestEm1 %>%
   filter(chrononumber != "NA")
   
   
   ## drop_na(chrononumber)


actiegraphemilia <- focusfeestEm %>%
  filter(chrononumber == 5)
# filter(participant == 'Emilia', session_number < 3, event_type != 'jump')

#names(actiegraphemilia)

library(firatheme)

## plot voor 1 sessie####
ActiePlotEm <- ggplot(actiegraphemilia, aes(jump_number, event_size_in_chars))
Plot2 <- ActiePlotEm + geom_point(aes(jump_number, event_size_in_chars, colour = event_type), size = 2)+
  labs(x = "actions", y = "size in characters", title = "activities per session", subtitle = "Emilia's process")+
  facet_grid(cols = vars(chrononumber))+ 
  theme_fira()+scale_color_fira()

Plot2
Plot2 + theme(axis.text.x = element_blank())


## plot voor hele proces ####
ActiePlotEmA <- ggplot(focusfeestEm, aes(jump_number, event_size_in_chars))
Plot3 <- ActiePlotEmA + geom_point(aes(jump_number, event_size_in_chars, colour = event_type), size = 2)+
  labs(x = "actions", y = "size in characters", title = "activities per session", subtitle = "Emilia's process")+
  facet_grid(cols = vars(chrononumber))+ 
  theme_fira()+scale_color_fira()

Plot3 + theme(axis.text.x = element_blank())

##### duration vars ####
options(scipen = "999")
view(actiegraphemilia)
ActiePlotEm2 <- ggplot(actiegraphemilia, aes(jump_number, event_duration))
Plot4 <- ActiePlotEm2 + geom_col(aes(jump_number, event_duration, fill = event_type), position = "dodge", width = 4)+
  labs(y = "event duration", x = "chronological ordering of events", title = " duration of actions per session", subtitle = "Emilia's process")+
  theme_fira()+scale_fill_fira()

#+facet_wrap(~event_type)

#facet_grid(cols = vars(chrononumber))+ 
#+scale_fill_fira()

Plot4


ActiePlotEm3 <- actiegraphemilia%>%
  filter(event_type != "focus")
  
Plot5 <-   ggplot(ActiePlotEm3, aes(jump_number, event_duration))+
  geom_col(aes(jump_number, event_duration, fill = event_type), position = "dodge", width = 4)+
  labs(y = "event duration", x = "chronological ordering of events", title = " duration of actions per session", subtitle = "Emilia's process")+
  theme_fira()+scale_fill_fira()

Plot5

######lollipop chart####

library(ggpubr)
ggdotchart(ActiePlotEm3, x = "jump_number", y = "event_duration",
           color = "event_type",                                # Color by groups
           palette = c("blue", "red", "green", "yellow"), # Custom color palette
                               
           add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr()                        # ggplot2 theme
)


# Sort value in descending order
# sorting = "ascending", 

#####timeline graph####
library(vistime)

names(actiegraphemilia)
vistime(actiegraphemilia, 
        col.event = "event_type", 
        col.start = "start_time",
        col.end = "end_time")
   
### helaas... not of class 'POSIXct'    
  ### https://stackoverflow.com/questions/48426873/r-timeline-without-dates
## https://cran.r-project.org/web/packages/vistime/vignettes/vistime-vignette.html
     

## eerst starttime en endtime omzetten dus 
ms <- 1243000042234
ms_to_date = function(ms, t0="2020-12-08", timezone) {
  ## @ms: a numeric vector of milliseconds (big integers of 13 digits)
  ## @t0: a string of the format "yyyy-mm-dd", specifying the date that
  ##      corresponds to 0 millisecond
  ## @timezone: a string specifying a timezone that can be recognized by R
  ## return: a POSIXct vector representing calendar dates and times        
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}     

actiongraphemiliaPosix <- actiegraphemilia %>%
  mutate(starttimePosix = ms_to_date(start_time, timezone = "Europe/Amsterdam"),
         endtimePosix = ms_to_date(end_time, timezone = "Europe/Amsterdam"))

view(actiongraphemiliaPosix)


#####timeline graph####
library(vistime)

names(actiongraphemiliaPosix)
vistime(actiongraphemiliaPosix, 
        col.event = "event_type", 
        col.start = "starttimePosix",
        col.end = "endtimePosix")

## kleiner fragment van de sessie nemen, dit werkt op zich maar is lelijk
# nl onleesbare naamlabels per event

## met kleiner stukje sessie, deze voor artikel ####

actiongraphemiliafragmentje <- actiongraphemiliaPosix %>%
slice_head(n = 75)

vistime(actiongraphemiliafragmentje, 
        col.event = "event_type", 
        col.start = "starttimePosix",
        col.end = "endtimePosix",
        title = "Emilia's 5th session - first 75 actions - interactive timeline",
        show_labels = F)

#labels = per event. staan niet zo mooooi