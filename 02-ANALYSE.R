####################################################################################
####################################################################################
#                                 Kickstarter project                              #
####################################################################################
####################################################################################

####################################################################################
#                                     Analyse data                                 #
####################################################################################

####################################################################################
#                                     Graphs                                       #
####################################################################################

which_game<-"pvt_004"

starcraft_scouting %>%
  filter(game==which_game) -> data_game


#############################################
#                Per Cycle                  #
#############################################

data_game %>%
  group_by(cycle) %>%
  summarize(losses           = sum(losses),
            observable.units = sum(observable.units),
            observed.losses  = sum(observed.losses),
            production       = sum(production),
            scouting         = sum(scouting),
            vision           = sum(vision)) -> data_game_cycle

#Losses
highchart() %>% 
  hc_add_series_labels_values(data_game_cycle$cycle, 
                              data_game_cycle$losses, 
                              type = "bar",
                              colorByPoint = FALSE) %>%
  hc_xAxis(categories = data_game_cycle$cycle) %>%
  hc_title(text = "Game by cycle : Losses") %>%
  hc_legend(enabled = FALSE)

#observable.units
highchart() %>% 
  hc_add_series_labels_values(data_game_cycle$cycle, 
                              data_game_cycle$observable.units, 
                              type = "bar",
                              colorByPoint = FALSE) %>%

  hc_xAxis(categories = data_game_cycle$cycle) %>%
  hc_title(text = "Game by cycle : observable.units") %>%
  hc_legend(enabled = FALSE)

#observed.losses
highchart() %>% 
  hc_add_series_labels_values(data_game_cycle$cycle, 
                              data_game_cycle$observed.losses, 
                              type = "bar",
                              colorByPoint = FALSE) %>%
  hc_xAxis(categories = data_game_cycle$cycle) %>%
  hc_title(text = "Game by cycle : observed.losses") %>%
  hc_legend(enabled = FALSE)



#production
highchart() %>% 
  hc_add_series_labels_values(data_game_cycle$cycle, 
                              data_game_cycle$production, 
                              type = "bar",
                              colorByPoint = FALSE) %>%
  
  hc_xAxis(categories = data_game_cycle$cycle) %>%
  hc_title(text = "Game by cycle : production") %>%
  hc_legend(enabled = FALSE)

#scouting
highchart() %>% 
  hc_add_series_labels_values(data_game_cycle$cycle, 
                              data_game_cycle$scouting, 
                              type = "bar",
                              colorByPoint = FALSE) %>%
  
  hc_xAxis(categories = data_game_cycle$cycle) %>%
  hc_title(text = "Game by cycle : scouting") %>%
  hc_legend(enabled = FALSE)

#vision
highchart() %>% 
  hc_add_series_labels_values(data_game_cycle$cycle, 
                              data_game_cycle$vision, 
                              type = "bar",
                              colorByPoint = FALSE) %>%
  
  hc_xAxis(categories = data_game_cycle$cycle) %>%
  hc_title(text = "Game by cycle : vision") %>%
  hc_legend(enabled = FALSE)


#############################################
#                Per Unit                   #
#############################################

data_game %>%
  filter(production>0) %>%
  select(cycle,unit,production)-> data_game_unit

data_game_unit$unit<-unlist(lapply(strsplit(as.character(data_game_unit$unit),split=" "),
                            FUN=function(x) x[2]))


nodes <- data.frame(id     = c(1:nrow(data_game_unit)), 
                    group  = data_game_unit$unit,
                    label  = paste(data_game_unit$unit,
                                   data_game_unit$cycle,sep=" : "),
                    value  = data_game_unit$production)

edges <- data.frame(from   = c(1:(nrow(data_game_unit)-1)), 
                    to     = c(2:nrow(data_game_unit)))

visNetwork(nodes, edges,width="100%", main="Game path") %>% 
  visEdges(arrows ="to") %>%
  visInteraction(navigationButtons = TRUE)

