#process graphs maken

library(tidyverse)

datatimefixed <- read.csv("data_out_timefix/timefixeddata.csv")

## deze werkt op losse sessies
 
 oefensessietje <- datatimefixed %>%
   filter(participant == 'De Hemel', session_number == "7")

  ggplot(oefensessietje, aes(endTime_Fixed, positionFull))+
   geom_line(aes(endTime_Fixed, positionFull), colour = "violet")+ 
   geom_line(aes(endTime_Fixed, doclengthFull), colour = "blue")+
   geom_line(aes(endTime_Fixed, charProduction), colour = "black")+
  labs(x = "endTime_ms", y = "characters", title = "process graph", subtitle = "cursorposition = violet, doclength = blue, characterproduction = black")
   

 #linetype = "dotdash"  voor stippellijntje
 
## voor een grafiek van alle sessies van 1 schrijver moet ik nog een nieuwe tijdskolom toevoegen
  # waarbij de tijd oploopt over de sessies heen
  
# en bij grafiek zelf de sessies aanduiden ( liefst door verticale lijnen)
  #geom_vline(xintercept = 3) (verticale lijn toevoegen op coordinaat 3)
  
  
  ##########------------ Add info -----------------------------------------#######
  #make two new columns with the added-up start time and end time per event (counting over the session boundaries)
  #add end time of the previous session to the start time & end time of all events in the next session
  
  
  
  ## test werkt
  oefendata <- read.csv2("data_out/fictievedatasummary.csv")
  
  testtijdopteller <- oefendata %>%
    filter(participant == "julie")
   
     testtijdopteller$Eindtijd2 <- testtijdopteller$endTimeFixed
  
  #tweede sessie van de set data noemen
  
  for (i in 2:nrow(testtijdopteller)) { 
    
    prev <- nrow(testtijdopteller[testtijdopteller$session_number==i-1,])
    
       testtijdopteller[testtijdopteller$session_number==i,]$Eindtijd2 <- testtijdopteller[testtijdopteller$session_number==i,]$endTimeFixed + testtijdopteller[testtijdopteller$session_number==i-1,]$Eindtijd2[prev]
  }
  
 view(testtijdopteller)
  
  
   
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
  ## ok, dan optellende holistische event_eindtijden toevoegen
 
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
 DeHemelalles3 <- DeHemelalles2%>%
   filter(type != "focus")%>%
   mutate(eindtijdInMinu = Eindtijd2/60000)

 ggplot(DeHemelalles3, aes(Eindtijd2, positionFull))+
          geom_line(aes(Eindtijd2, positionFull), colour = "black")
 
 ## mm, veel scherpe sprongen naar einde van doc - assen nog wat aanpassen?
 
 
 
 
 