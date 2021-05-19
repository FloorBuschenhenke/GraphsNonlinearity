# load packages
library(tidyverse)

# no scientific notion
options(scipen = 999)

##########----------- Load data -----------------------------------------#######

library(tidyverse)

##### dit is de versie met twee pauzevrije ( >5 minuten) begin- en eindtijdkolommen
## spell_start is gefilderde begintijd
## spell_end is gefilterde eindtijd

all_dataX <- read.csv("data/pauzefiltereddata.csv", stringsAsFactors = F)
#colnames(all_data)

#deze input heeft row names (kolom X, 'groups')is niet de bedoeling
all_data <- all_dataX %>%
  select(-X.1, -Npauzes, -pauzestatus)

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

write.csv(data_add, "data_add_zonderpauzes.csv", row.names = F)


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



write.csv(jump_add, "jump_add_zonderpauzes.csv", row.names = F)



##############
## Calculate characteristics for JUMPS ONLY 
sum_jump <- data_add %>%
  left_join(jump_add, by = c("participant", "session_number", "jump_number")) %>%
  # remove first part of the session (not really a jump) & focus on jumps only
  filter(jump_number != 1, jump_type == "jump") %>%
  group_by(participant, session_number, jump_number) %>%
  summarize(
    start_id_GA = first(id),
    start_time_rel = first(spell_start)/first(endTime_session),
    jump_pause = first(pauseTime),
    start_position = first(positionFull),
    end_position = last(positionFull),
    start_position_rel = start_position/first(doclengthFull),
    end_position_rel = end_position/last(doclengthFull),
    start_position_edge = first(doclengthFull)-start_position,
    end_position_edge = last(doclengthFull)-end_position,
    start_location = first(prev_loc),
    end_location = last(next_loc),
    start_eventtype = first(type),
    end_eventtype = last(type),
    start_time = first(spell_start),
    end_time = last(spell_end),
    doclength = first(doclengthFull),
    # counts of types of events within jump
    n_events = n(),
    n_scroll_movements = sum(output == "Scroll"),
    n_selections = sum(type == "replacement"),
    n_arrowkeys = sum(keytype == "arrow_key")
    ) %>%
  mutate(   
    jump_duration = end_time - start_time,
    ## 0 vervangen door NA
    start_position = ifelse(start_position == 0, NA, start_position),
    start_position_rel = ifelse(start_position_rel == 0, NA, start_position_rel),
    start_position_edge = ifelse(start_position_edge == 0, NA, start_position_edge),
    jump_size_chars = end_position - start_position,
    # added fb
    direction = ifelse(jump_size_chars < 0, "backwards", "forwards"),
    # slopes (delta characters/ delta time)
    jump_slope = (jump_size_chars)/jump_duration,
    #converting all jump sizes into positive values for adding up later
    jump_size_charsPlus = ifelse(jump_size_chars < 0, jump_size_chars *-1, jump_size_chars),
    # adding jump size relative to document size at that moment
    jump_size_rel = jump_size_charsPlus/doclength
  ) 

#view(sum_jump) 


# write data to file
write.csv(sum_jump, "data_out/sum_jump_zonderpauzes.csv", row.names = F)



# filter irrelevant jumps:

jump_filt <- sum_jump %>%
  filter(jump_size_chars != 0)

#view(jump_filt)
write.csv(jump_filt, "data_out/jump_filt_zonderpauzes.csv", row.names = F)


###################
#  Calculate characteristics for FOCUS ONLY 
sum_focus <- data_add %>%
  mutate(
    prev_pos = lag(positionFull),
    next_pos = lead(positionFull),
    prev_event = lag(type),
    next_event = lead(type)
  ) %>%
  left_join(jump_add, by = c("participant", "session_number", "jump_number")) %>%
  # remove first part (not really a jump) & focus on focus events only
  filter(jump_number != 1, jump_type == "focus") %>%
    group_by(participant, session_number, jump_number) %>%
    summarize(
      start_id_GA = first(id),
      start_time_rel = first(spell_start)/first(endTime_session),
      jump_pause = first(pauseTime),
      #last position in text from which the focus event started
      start_position = first(prev_pos),
      #first position in text after the writer returned from the focus event 
      end_position = last(next_pos),
      start_position_rel = start_position/first(doclengthFull),
      end_position_rel = end_position/last(doclengthFull),
      start_position_edge = first(doclengthFull)-start_position,
      end_position_edge = last(doclengthFull)-end_position,
      start_location = first(prev_loc),
      end_location = last(next_loc),
      # last event in text from which the focus event started
      start_eventtype = first(prev_event),
      # first event in text after the writer returned from the focus event
      end_eventtype = last(next_event),
      start_time = first(spell_start),
      end_time = last(spell_end)) %>%
    mutate(   
      jump_duration = end_time - start_time,
      start_position = ifelse(start_position == 0, NA, start_position),
      start_position_rel = ifelse(start_position_rel == 0, NA, start_position_rel),
      start_position_edge = ifelse(start_position_edge == 0, NA, start_position_edge),
      jump_size_chars = end_position - start_position,
      # slopes (delta characters/ delta time)
      jump_slope = (jump_size_chars)/jump_duration
    )   

#######################
# calculate size of text production chunks -fb



sum_typing <- data_add %>%
  left_join(jump_add, by = c("participant", "session_number", "jump_number")) %>%
  # remove first part of the session (not really a jump) & focus on typing only
  filter(jump_number != 1, jump_type == "typing") %>%
  group_by(participant, session_number, jump_number) %>%
  summarize(
    start_id_GA = first(id),
    start_position = first(positionFull),
    end_position = last(positionFull),
    start_time = first(spell_start),
    end_time = last(spell_end),
    start_position_rel = start_position/first(doclengthFull),
    start_position_edge = first(doclengthFull)-start_position,
  ) %>%
  mutate(   
    typing_duration = end_time - start_time,
    typing_size_chars = end_position - start_position,
    start_position_rel = ifelse(start_position_rel == 0, NA, start_position_rel),
    start_position_edge = ifelse(start_position_edge == 0, NA, start_position_edge))


################################################################################  
# summary statistics for each session (focus event)
## dit werkt niet - is niet de tijd tussen focusevents
sum_sum_focus <- sum_focus %>%
  group_by(participant, session_number)%>%
  summarize(
    focus_time = sum(jump_duration)
  )

# summary statistics for each session (all)
sum_session <- data_add %>%
  group_by(participant, session_number)%>%
  ## dit is met botte bijl maar ok
  filter(is.na(charProduction) != T)%>%
  summarize(
    total_time = last(spell_end) - first(spell_start),
    total_time_seconds = total_time/1000,
    total_time_minutes = total_time_seconds/60,
    total_charproduced = last(charProduction) - first(charProduction) ) %>%
  
    ## kan nog op chrononummer later
   ## mutate(char_produced2 = ifelse( session_number == min(session_number), total_charproduced, total_charproduced - lag(total_charproduced))) %>%
  left_join(sum_sum_focus)

 testcharproduced2 <- sum_session%>%
  filter(participant == "rovarij2")
 view(testcharproduced2)
 
 #count(sum_session, participant)
# TODO totalcharproduced is soort doclength - ja nog fixen (door expliciet na's te laten skippen)
 # charproduced2 werkt nu wel goed op sessiebasis 
 # voor andere intervallen moet ik charproduced wel fixen TODO


#added total jump time per session FB
sum_session2 <- jump_filt %>%
  group_by(participant, session_number)%>%
  summarize(
    Jump_time = sum(jump_duration, na.rm = T)
  ) %>% left_join(sum_session)
#view(sum_session2)

##added TYPING chunks (size & duration & position relative to leading edge, also total amount of chars typed)
sum_session3 <- sum_typing %>%
  group_by(participant, session_number)%>%
  summarise(
    MeanTypingChars = mean(typing_size_chars),
    sdTypingChars = sd(typing_size_chars),
    totalTypingChars = sum(typing_size_chars, na.rm = TRUE),
    MeanDurationTyping = mean(typing_duration),
    sdDurationTyping = sd(typing_duration),
    MeanTypingPositionRel = mean(start_position_rel),
    sdTypingPositionRel = sd(start_position_rel),
    MeanTypingPositionEdge = mean(start_position_edge),
    sdTypingPositionEdge = sd(start_position_edge),
    totalTypingChars = sum(typing_size_chars)
  ) %>% left_join(sum_session2)

#view(sum_session3)

