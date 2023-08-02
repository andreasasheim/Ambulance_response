library(dplyr)
library(tidyr)

source("fuction_checkAmbulanceState.R")


# Data has one line per incident. Transform to one line per potential responding ambulace per incident
d = data %>% select(incident_id,call_time,starts_with("resource")) %>%
  pivot_longer(starts_with("resource_id"),values_to = "resource_id_potential",values_drop_na = F)
# Check busy status for each potential ambulance at call time
d <- checkAmbulanceState(d,data_ambulance_status)

# Pivot back to one line per incident
d = d %>% select(incident_id,name,busy) %>%
  pivot_wider(names_from = "name",values_from = "busy",values_fill=0)
names(dd)[names(dd)=="resource_id1"] <- "busy_id1"
names(dd)[names(dd)=="resource_id2"] <- "busy_id2"
names(dd)[names(dd)=="resource_id3"] <- "busy_id3"
names(dd)[names(dd)=="resource_id4"] <- "busy_id4"
names(dd)[names(dd)=="resource_id5"] <- "busy_id5"

# Add computed variables to data
data = merge(data,d,by=c("incident_id"),all.x=T)


# Compute the busy indicator
data = data %>% mutate(probability_of_busy= prob1*busy_id1+prob2*busy_id2+prob3*busy_id3+prob4*busy_id4+prob5*busy_id5) 

