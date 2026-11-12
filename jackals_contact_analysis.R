require(units)
require(dplyr)
require(ggplot2)
require(move2)
require(wildlifeDI)
require(sf)
require(igraph)





#Black-backed jackal, Etosha National Park, Namibia - https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study304875150

jackals <- movebank_download_study("Black-backed jackal, Etosha National Park, Namibia",
                                   sensor_type_id = "gps",
                                   'license-md5'='317da200a36e001f293f10af9f6d26d1')


jackals
unique(jackals$individual_local_identifier)

#a few dyads jennifer used: cm44 and cm69, cm08 and cm09, cm




jackals.0809 <- jackals %>%
  filter(individual_local_identifier=='CM08'|
         individual_local_identifier=='CM09')
jackals.0809

jackals.08 <- jackals %>%
  filter(individual_local_identifier=='CM08')
jackals.09 <- jackals %>%
  filter(individual_local_identifier=='CM09')

GetSimultaneous(jackals.08, jackals.09, tc=60) #6450
GetSimultaneous(jackals.08, jackals.09, tc=2.5*60) #6618  <-
GetSimultaneous(jackals.08, jackals.09, tc=5*60) #6618
GetSimultaneous(jackals.08, jackals.09, tc=7.5*60) #6618


ggplot(data=jackals.0809)+
  geom_sf(aes(col=individual_local_identifier))






jackals.4469 <- jackals %>%
  filter(individual_local_identifier=='CM44'|
           individual_local_identifier=='CM69')
jackals.4469

jackals.44 <- jackals %>%
  filter(individual_local_identifier=='CM44')
jackals.69 <- jackals %>%
  filter(individual_local_identifier=='CM69')

GetSimultaneous(jackals.44, jackals.69, tc=60)


ggplot(data=jackals.4469)+
  geom_sf(aes(col=individual_local_identifier))






dcPlot(jackals.0809, tc=2.5*60, dmax=5500)
dcPlot(jackals.0809, tc=2.5*60, dmax=1500)

 dcPlot(jackals.4469, tc=60, dmax=5500)


#jam used 100, 500, and 1000m distance thresholds to define contact


#1. identify contacts in the dyad (for each threshold)

cons.0809.100 <- conProcess(jackals.0809, tc=2.5*60, dc=100)
table(cons.0809.100$contact)

cons.0809.500 <- conProcess(jackals.0809, tc=2.5*60, dc=500)
table(cons.0809.500$contact)

cons.0809.1000 <- conProcess(jackals.0809, tc=2.5*60, dc=1000)
table(cons.0809.1000$contact)


#2. group contacts into unique phases of continuous interaction - use jed's pc of 60 mins (sort of arbitary here) - just gonna stick with dc=100m for now

phas.0809.100 <- conPhase(cons.0809.100, pc=60*60)

glimpse(phas.0809.100)
unique(phas.0809.100$contact_pha)

consum <- phas.0809.100 |>
  st_drop_geometry() |>
  filter(!is.na(contact_pha)) |>
  dplyr::group_by(contact_pha) |>
  dplyr::summarise(
    nfix = n(),
    t1 = min(timestamp),
    t2 = max(timestamp),
    duration = max(timestamp)-min(timestamp),
    avg_d = mean(contact_d,na.rm=T),
    min_d = min(contact_d,na.rm=T),
    max_d = max(contact_d,na.rm=T)
  ) 
consum



#3. map all contacts

conall <- cons.0809.100 |>
  subset(contact == 1)

ggplot(data=jackals.0809)+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=conall)


#4. map first contact in each contact phase

pha_fir <- phas.0809.100 |>
  filter(!is.na(contact_pha)) |>
  group_by(contact_pha) |>
  filter(row_number()==1)

ggplot(data=jackals.0809)+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=pha_fir)


#5. map contact phases as lines

pha_lin <- phas.0809.100 |>
  filter(!is.na(contact_pha)) |>
  group_by(contact_pha) |>
  summarise(n = dplyr::n(),do_union=FALSE) |>
  filter(n > 1) |>
  st_cast("LINESTRING")


ggplot(data=jackals.0809)+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=pha_lin)



#6. derive the contact network (using the 10 individuals selected in Miller 2021)

unique(jackals$individual_local_identifier)

jackals_ntwk <- jackals %>%
  filter(individual_local_identifier %in% c('CM08','CM09','CM44','CM69','CM10','CM83','CM47', 'CM62','CM05','CM15'))

unique(jackals_ntwk$individual_local_identifier)


cons <- conProcess(jackals_ntwk,dc=100,tc=2.5*60,return='contacts')

glimpse(cons)

tab_cnt <- cons |>
  count(id1,id2)
gr <- graph_from_data_frame(tab_cnt,directed=FALSE)
E(gr)$weight <- tab_cnt$n

plot(gr)


Lixn(jackals.0809, tc=2.5*60)
Lixn(jackals.4469, tc=2.5*60)

HAI(jackals.0809, tc=2.5*60, dc=100)
HAI(jackals.0809, tc=2.5*60, dc=500)
HAI(jackals.0809, tc=2.5*60, dc=1000)

HAI(jackals.4469, tc=2.5*60, dc=100)
HAI(jackals.4469, tc=2.5*60, dc=500)
HAI(jackals.4469, tc=2.5*60, dc=1000)


