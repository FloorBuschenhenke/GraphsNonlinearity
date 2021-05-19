# van general analysis file naar visuals van nonlineariteit



########### alles bij elkaar in 1 script

### onderdelen: 
### uit load_data (voor csv - zie voor xml nonlinielftalproject)(kolomnamen zijn anders bij xml)
## uit jump_events (non-lini analyse)
## wat staat er in jumpsevents_episodes_files?? nu niet hieronder)
## visuals maken 

########### 1. inlezen van lijstje aan bestanden #############

# voegt oa kolomn session_number toe 


##########----------- Load data -----------------------------------------#######
# load packages
library(dplyr)


# list all files in repository
files <- list.files(pattern = "_GA.csv", path = "data/", full.names = T)

# function to read GA file 
read_GAs <- function(file){
  data <- read.csv(file, sep = ";", 
                   stringsAsFactors = F, header = T, fileEncoding = "UTF-16LE") %>%
    # extract filenumber from file name
    mutate(session_number = gsub('.*_(\\d{1,3})_.*', '\\1', file)) %>%
    
    # remove unwanted columns 
    select(-sessionID_Text_Language__SL_, -sessionID_Age__S_,
           -sessionID_Gender__S_, -sessionID_Group__S_, 
           -sessionID_Experience__S_,
           -sessionID_Session__S_, -sessionID_filepath__S_)
}

# read all GA files at once
all_data_list <- lapply(files, read_GAs)

# put them into a dataframe
all_data <- do.call(rbind, all_data_list)

# write data
write.csv(all_data, "data_out/all_data.csv", row.names = F)

##########---------------------------------------------------------------#######

##### 2.  uit Jump_events (non-lini analyse) ####################

# load packages
library(tidyverse)

# no scientific notion
options(scipen = 999)

##########----------- Load data -----------------------------------------#######



##### dit is de versie met twee pauzevrije ( >5 minuten) begin- en eindtijdkolommen
## spell_start is gefilderde begintijd
## spell_end is gefilterde eindtijd
##startTimeFixed & endtimeFixed = starttijd op 0 gezet (IL probleempje)

all_data <- read.csv("data/pauzefiltereddata.csv", stringsAsFactors = F)
#colnames(all_data)

##########------------ Add info -----------------------------------------#######
arrow_keys <- c("UP", "DOWN", "LEFT", "RIGHT", "END", "HOME", "PAGE_DOWN",
                "PAGE_UP")

# check all possible keystrokes 
keys <- all_data %>% filter(type =="keyboard")
keyst <- data.frame(output = unique(keys$output), 
                    stringsAsFactors = F) 

keyst_add <- keyst %>%
  mutate(length = nchar(as.character(output)),
         keytype = ifelse((length < 3 & output != "UP") | 
                            grepl("OEM_", output), "visible_char",
                          ifelse(output %in% c("SPACE", "RETURN", "TAB"),
                                 "whitespace",
                                 ifelse(grepl("UP|DOWN|LEFT|RIGHT|END|HOME", output),
                                        "arrow_key",
                                        ifelse(output %in% c("BACK", "DELETE"),
                                               "delete_key",
                                               "function_key")))))


# add boundaries for non-linearity
data_add <- all_data %>%
  # remove keystrokes outside doc (e.g., save as XXX)
  filter(!(type == "keyboard" & is.na(positionFull))) %>%
  
  left_join(keyst_add) %>%
  # calculate per session/file separately
  group_by(participant, session_number) %>%
  mutate(jump_start = ifelse(
    row_number() == 1 |
      #1)	When a typist moves from typing a character to a mouse event 
      #   (click, movement, scroll, selection), or vice versa.
      (type == "keyboard" & 
         (keytype %in% c("visible_char", "whitespace") | 
            output == "CAPS LOCK" |
            (keytype == "function_key" & 
               pauseLocationFull == "COMBINATION KEY"))  &
         (lag(type) == "mouse" | type == "replacement")) | 
      ((type == "mouse" | lag(type) == "replacement")  &
         lag(type) == "keyboard" & 
         (lag(keytype) %in% c("visible_char", "whitespace")) |
         output == "CAPS LOCK") |
      #2) When a typists moves from an insertion to another event, or vice versa.
      (lag(type) %in% c("insert") & 
         type != lag(type) |
         (type %in% c("insert")  &
            lag(type) != type)) |
      #3)	When a typist moves from typing a character to typing an arrow key, 
      #   or vice versa.
      (type == "keyboard" & 
         (keytype %in% c("visible_char", "whitespace") |
            output == "CAPS LOCK" |
            (keytype == "function_key" & 
               pauseLocationFull == "COMBINATION KEY")) &
         lag(type) == "keyboard" & lag(type) == "arrow_key") | 
      (type == "keyboard" & keytype == "arrow_key"
       & (lag(type) == "keyboard" & 
            lag(type) %in% c("visible_char", "whitespace"))  | 
         lag(type) == "replacement")  |
      #4)	When a typists moves from a keystroke or mouse event to a 
      #   delete/backspace keypress, or vice versa.
      (type %in% c("mouse","keyboard", "insert") & 
         (is.na(keytype) | keytype != "delete_key") &
         lag(type) == "keyboard" & lag(keytype) == "delete_key") | 
      (type == "keyboard" & keytype == "delete_key"
       & lag(type) %in% c("mouse","keyboard", "insert") & 
         !lag(output) %in% c("DELETE", "BACK")) |  
      #5)	When a typist moves from one mode of deletion to another (e.g., from 
      #   delete key to backspace key press).
      (keytype == "delete_key" & lag(keytype) == "delete_key" & 
         (output) != lag(output)) |
      #6)	When a typist moves from the main text to a different source 
      #   (e.g., online dictionary)
      (lag(type) %in% c("focus") & 
         type != lag(type)) |
      (type %in% c("focus") & 
         type != lead(type)) , 1, 0),
    
    # set to zero if selection is directly followed by insert/delete 
    # (series of deletions count as one event)
    #delete - replacement
    jump_start = ifelse((type == "replacement" &
                           lag(output) == "DELETE" & 
                           spell_start == lag(spell_start) &
                           spell_end == lag(spell_end))
                        | (lag(type) == "replacement" &
                             output == "DELETE" &
                             lead(type) == "replacement"  & 
                             lag(output,2) == "DELETE" ) |
                          (type == "replacement" &
                             lag(type) == "replacement") |
                          is.na(jump_start), 0, jump_start),
    # create count number of linear event
    jump_number = ifelse(jump_start == 1,
                         cumsum(jump_start == 1 |
                                  row_number() == 1), NA),
    prev_loc = lag(pauseLocationFull),
    next_loc = lead(pauseLocationFull),
    endTime_session = max(spell_end)
  ) %>%
  fill(jump_number) 

###toevoeging om character counts te corrigeren (probleem materiaal Gie; start bij 0 dan paar regels later heel hoog
## inlezen bestandslengte dus pas in regel x ipv regel 1 
##mutate(charProduction = replace(charProduction == 0, NA))  werkt niet, argument values is missing w
## werkt ook niet: data_add[4][data_add[4] == 0] <- NA
## werkt niet ifelse(charProduction == 0, is.na(charProduction), charProduction))
## werkt niet; replace(charProduction, 0, NA) 
## in die kolom de 0 vervangen door NA > 



data_add$charProduction[data_add$charProduction == 0] <- NA  

#head(data_add)

write.csv(data_add, "data_out/data_add_zonderpauzes.csv", row.names = F)


##########------------ summarize jump events --------------------------#######
# summary statistics for each jump event
jump_add <- data_add %>%
  group_by(participant, session_number, jump_number) %>%
  summarize(
    action_types = paste(unique(type), collapse = ", "),
    key_types = paste(unique(keytype), collapse = ", ")
  ) %>%
  mutate(
    jump_type = ifelse(grepl("delete_key", key_types), "delete",
                       ifelse(grepl("focus", action_types), "focus",
                              ifelse(action_types == "insert", "insert",
                                     ifelse(action_types %in% c("keyboard", "keyboard, replacement") &
                                              grepl("visible_char|whitespace", key_types), "typing",
                                            "jump"))))
  )



write.csv(jump_add, "data_out/jump_add_zonderpauzes.csv", row.names = F)


####################### 3. plaatjes maken ###################################################



##########-----------  -----------------------------------------#######



jumptabel <- read.csv("data_out/jump_add_zonderpauzes.csv", stringsAsFactors = F)

data_add <- read.csv("data_out/data_add_zonderpauzes.csv", stringsAsFactors = F)

#colnames(jumptabel)
#colnames(data_add)

##############


# samenvoegen van de twee tabellen


grotetabel <- data_add %>%
  left_join(jumptabel, by = c("participant", "session_number", "jump_number"))%>%
  
  # remove first part of the session (not really a jump)
  filter(jump_number != 1) %>%
  group_by(participant, session_number, jump_number) %>%
  summarize(
    start_id_GA = first(id),
    
    start_position = first(positionFull),
    end_position = last(positionFull),
    
    start_time = first(spell_start),
    end_time = last(spell_end),
    
    n_events = n(),
    
    doclength = first(doclengthFull)) %>%
  
  
  mutate(   
    jump_duration = end_time - start_time,
    ## 0 vervangen door NA
    start_position = ifelse(start_position == 0, NA, start_position),
    
    jump_size_chars = end_position - start_position,
    
    #converting all jump sizes into positive values for adding up later
    jump_size_charsPlus = ifelse(jump_size_chars < 0, jump_size_chars *-1, jump_size_chars),
  ) %>%
  left_join(jumptabel, by = c("participant", "session_number", "jump_number"))




# write data to file
write.csv(grotetabel, "data_out/grotetabel.csv", row.names = F)


############################ plaatje maken ################################################

## welke activiteiten vinden wanneer plaats? perspectief: sessie (ook vergelijken met paar sessies)

grotetabel <- read.csv("data_out/grotetabel.csv", stringsAsFactors = F) 

## grote tabel is output van de nonlineariteitsanalyse 

## hier moeten nog chrononummers naast de sessie(idfx)nummers komen ##

summaryTekst <- read.csv("data/summaryTekst.csv", stringsAsFactors = F)


chrononummers <- summaryTekst %>%
   group_by(participant, session_number)%>%
  select(participant, session_number, chrononumber)

#view(chrononummers)

grotetabel2 <- grotetabel %>%
  group_by(participant, session_number)%>%
  left_join(chrononummers)

  
 
## de max jump size beinvloed de lees/werkbaarheid vd graphs
grotetabelzonderoutliers <- grotetabel2 %>%
  filter(jump_size_charsPlus < 2000)%>%
  mutate( jump_type = ifelse(jump_type == "insert", "selection", jump_type))%>%
  rename( event_type = jump_type)%>%
  filter(!(event_type == "jump" & jump_size_charsPlus == 0))

# rename ook   evt   distance_in_chars_plus = jump_size_charsPlus,
#      event_number = jump_number) nu in assen-labels gedaan omdat andere scripts ook met deze variabelen werken


### activities per session ### 

library(firatheme)
#library(tidyverse)

##voor proces emilia
actiegraphemilia <- grotetabelzonderoutliers %>%
  filter(participant == 'Emilia', chrononumber == 5, event_type != 'jump')
 # filter(participant == 'Emilia', session_number < 3, event_type != 'jump')

#colnames(actiegraphemilia)

ActiePlotEm <- ggplot(actiegraphemilia, aes(jump_number, jump_size_charsPlus))
Plot2 <- ActiePlotEm + geom_point(aes(jump_number, jump_size_charsPlus, colour = event_type))+
  labs(x = "actions", y = "size in characters", title = "activities per session", subtitle = "Emilia's process")+
  facet_grid(cols = vars(chrononumber))+ 
  theme_fira()+scale_color_fira()

Plot2
Plot2 + theme(axis.text.x = element_blank())

### ok, de jumps maken het lastig om de andere acties goed te kunnen zien - zijn veel 'kleiner

## meer op elkaar geplakt 
## grote jumps eruit helpt wel, of alle jumps er even uit (versies met en zonder?)
 # ja, even zonder jumps gedaan - maakt de rest wel stuk duidelijker
# je zou eigenlijk alle soorten events aan/uit moeten kunnen zetten voor interactieve v
## threshold grote jumps ook aanp
## meer dan 3 a 4 sessies wordt toch minder leesbaar..

event_type_graph_minoutliers <- ggplot(grotetabelzonderoutliers, aes(jump_number, jump_size_charsPlus))
event_type_graph_minoutliers + geom_point(aes(jump_number, jump_size_charsPlus, colour = event_type, shape = event_type))+
  labs(x = "event_number", y = "size in characters", title = "activities per session", subtitle = "sample set De Hemel")+
  facet_grid(cols = vars(session_number))+ theme_fivethirtyeight()+ scale_color_gdocs()



#canva_pal()
# + canva_pal cute, voor max vier groepen
#plaatje verbeteren: 
# theme_fivethirtyeight()  ook leuk
# theme_few() mooi minimal
# theme_bw(base_family = "Playfair")
# theme_economist()
#  theme_solarized()
# theme_stata()

#scale_color_paletteer_d(ggthemes, calc)

################# plaatje 2: process graph van een losse sessie 

# sommige sessies hadden gekke starttijd, dat eerst gefixt (voor mijn nieuwe materiaal)

#process graphs maken

library(tidyverse)

#dit is al mijn nieuwe materiaal ( 11 schrijvers)
datatimefixed <- read.csv("data_out/timefixeddata.csv")

## deze werkt op losse sessies

oefensessietje <- datatimefixed %>%
  filter(participant == 'De Hemel', session_number == "2" | session_number == "4")

ggplot(oefensessietje, aes(endTime_Fixed, positionFull))+
  geom_line(aes(endTime_Fixed, positionFull), colour = "violet")+ 
  geom_line(aes(endTime_Fixed, doclengthFull), colour = "blue")+
  geom_line(aes(endTime_Fixed, charProduction), colour = "black")+
  labs(x = "endTime_ms", y = "characters", title = "process graph", subtitle = "cursorposition = violet, doclength = blue, characterproduction = black")+
  facet_grid(cols = vars(session_number))


#linetype = "dotdash"  voor stippellijntje

## voor een grafiek van alle sessies van 1 schrijver moet ik nog een nieuwe tijdskolom toevoegen
# waarbij de tijd oploopt over de sessies heen

# en bij grafiek zelf de sessies aanduiden ( liefst door verticale lijnen)
#geom_vline(xintercept = 3) (verticale lijn toevoegen op coordinaat 3)






################ plaatje 3: process graph hele proces
## hiervoor moeten er nog twee kolommen toegevoegd worden aan de data (chronologische sessienummering en een 
## opgetelde eindtijd per event, zodat de tijd (x-as) over de sessies heen doorloopt)

##########------------ Add info -----------------------------------------#######
#make two new columns with the added-up start time and end time per event (counting over the session boundaries)
#add end time of the previous session to the start time & end time of all events in the next session


## nu voor De Hemel

# eerst chrononummer aan hele GA plakken met left_join
## sessie 28 was leeg maar zat er nog in, foutje

summaryTekst <- read.csv("data_out/summaryTekst.csv", stringsAsFactors = F)
DeHemelalles <- datatimefixed %>%
  filter(participant == "De Hemel", session_number != "28")


chrononumHemel <- summaryTekst %>%
  filter(participant == "De Hemel")%>%
  group_by(session_number)%>%
  select(session_number, chrononumber)

view(chrononumHemel)

DeHemelalles2 <- DeHemelalles %>%
  left_join(chrononumHemel, by= "session_number")


#  view(DeHemelalles2)

##  optellende holistische event_eindtijden toevoegen

DeHemelalles2$Eindtijd2 <- DeHemelalles2$endTime_Fixed
DeHemelalles2 <- as.data.frame(DeHemelalles2)
#tweede sessie van de set data noemen

for (i in 2:nrow(DeHemelalles2)) { 
  
  prev <- nrow(DeHemelalles2[DeHemelalles2$chrononumber==i-1,])
  
  DeHemelalles2[DeHemelalles2$chrononumber==i,]$Eindtijd2 <- DeHemelalles2[DeHemelalles2$chrononumber==i,]$endTime_Fixed + DeHemelalles2[DeHemelalles2$chrononumber==i-1,]$Eindtijd2[prev]
}

#colnames(DeHemelalles2)

# summary(DeHemelalles2)
#script hierboven lijkt er erg lang over te doen (rode bolletje), maar volgens de summary is ie gewoon klaar duss..

#focusevents even eruit - gaat position op 0 of NA
## evt ook de startacties voor inlezen van bestand eruit met
##  filter(is.na(doclengthFull) == FALSE) (dan misschin eerst 0 omzetten in NA vanaf sessie 2)

DeHemelalles3 <- DeHemelalles2%>%
  filter(type != "focus")%>%
  mutate(eindtijdInMinu = Eindtijd2/60000)

processgraphDeHemelAlles <- ggplot(DeHemelalles3, aes(eindtijdInMinu, positionFull))+
  geom_line(aes(eindtijdInMinu, positionFull), colour = "darkorange")+
  geom_line(aes(eindtijdInMinu, doclengthFull), colour = "violet")+
  geom_line(aes(eindtijdInMinu, charProduction), colour = "black")+
  labs(y = "characters", title = "process graph De Hemel", subtitle = "verticale strepen & blauwe nummers zijn sessies",
       tag = "orange line = cursor position\n violet line = document length\n black line = characters produced")

processgraphDeHemelAlles

linesessions = DeHemelalles3 %>%
  group_by(chrononumber) %>%
  summarise(xvalue=abs(max(eindtijdInMinu)))

# library(ggrepel) (doet het nog niet - zou tekst uit elkaar moeten plaatsen)#geom_text_repel

processgraphDeHemelAlles + geom_vline(data = linesessions, aes(xintercept = xvalue))+
  geom_text(mapping = aes(x = xvalue,
                          y = 0,
                          label = chrononumber,
                          hjust = 1.5,
                          vjust = 0.5),
            data = linesessions, colour = 'blue')









