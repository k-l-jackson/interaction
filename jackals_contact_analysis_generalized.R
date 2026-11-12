require(units)
require(dplyr)
require(ggplot2)
require(move2)
require(wildlifeDI)
require(sf)
require(igraph)


#select dyad (see network for potential pairs)

j1 <- 'CM05'
j2 <- 'CM09'



#load dataset

#Black-backed jackal, Etosha National Park, Namibia - https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study304875150

jackals <- movebank_download_study("Black-backed jackal, Etosha National Park, Namibia",
                                   sensor_type_id = "gps",
                                   'license-md5'='317da200a36e001f293f10af9f6d26d1')


jackals



#######################################################################################

jackals.working <- jackals %>%
  filter(individual_local_identifier==j1|
           individual_local_identifier==j2)
jackals.working

jackals.1 <- jackals %>%
  filter(individual_local_identifier==j1)
jackals.2 <- jackals %>%
  filter(individual_local_identifier==j2)

#GetSimultaneous(jackals.1, jackals.2, tc=60) 
GetSimultaneous(jackals.1, jackals.2, tc=2.5*60) 
#GetSimultaneous(jackals.1, jackals.2, tc=5*60) 
#GetSimultaneous(jackals.1, jackals.2, tc=7.5*60) 


ggplot(data=jackals.working)+
  geom_sf(aes(col=individual_local_identifier))

glimpse(jackals.working)

#generate minimum convex polygon (HR)

mcp_sf <- jackals.working %>%
  group_by(individual_local_identifier) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_convex_hull()

ggplot(data=jackals.working)+
  geom_sf(data=mcp_sf, fill=NA, aes(col=individual_local_identifier))+
  geom_sf(aes(col=individual_local_identifier))

#check overlap

overlap <- st_intersection(mcp_sf[1,],mcp_sf[2,])

ggplot(data=jackals.working)+
  geom_sf(data=mcp_sf, fill=NA, aes(col=individual_local_identifier))+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=overlap)

area_o <- st_area(overlap)
area_j1 <- st_area(mcp_sf[1,])
area_j2 <- st_area(mcp_sf[2,])


overlap_summary <- tibble(
                          ID1=character(),
                          ID2=character(),
                          pct_overlap_j1=double(),
                          pct_overlap_j2=double()
                          )
overlap_summary <- overlap_summary %>%
  add_row(
    ID1=j1, 
    ID2=j2, 
    pct_overlap_j1=as.double((area_o/area_j1)*100), 
    pct_overlap_j2=as.double((area_o/area_j2)*100)
  )

print(overlap_summary)


dcPlot(jackals.working, tc=2.5*60, dmax=5500)
dcPlot(jackals.working, tc=2.5*60, dmax=1500)



#jam used 100, 500, and 1000m distance thresholds to define contact


#1. identify contacts in the dyad (for each threshold)

cons.100 <- conProcess(jackals.working, tc=2.5*60, dc=100)
table(cons.100$contact)

cons.500 <- conProcess(jackals.working, tc=2.5*60, dc=500)
table(cons.500$contact)

cons.1000 <- conProcess(jackals.working, tc=2.5*60, dc=1000)
table(cons.1000$contact)


#2. group contacts into unique phases of continuous interaction - use jed's pc of 60 mins (sort of arbitary here) - just gonna stick with dc=100m for now

phas.100 <- conPhase(cons.100, pc=60*60)

glimpse(phas.100)
unique(phas.100$contact_pha)

consum <- phas.100 |>
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

conall <- cons.100 |>
  subset(contact == 1)

ggplot(data=jackals.working)+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=conall)


#4. map first contact in each contact phase

pha_fir <- phas.100 |>
  filter(!is.na(contact_pha)) |>
  group_by(contact_pha) |>
  filter(row_number()==1)

ggplot(data=jackals.working)+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=pha_fir)


#5. map contact phases as lines

pha_lin <- phas.100 |>
  filter(!is.na(contact_pha)) |>
  group_by(contact_pha) |>
  summarise(n = dplyr::n(),do_union=FALSE) |>
  filter(n > 1) |>
  st_cast("LINESTRING")


ggplot(data=jackals.working)+
  geom_sf(aes(col=individual_local_identifier))+
  geom_sf(data=pha_lin)


#########################################################################

#check Lixn


Lixn(jackals.working, tc=2.5*60)


#check HAI

HAI(jackals.working, tc=2.5*60, dc=100)
HAI(jackals.working, tc=2.5*60, dc=500)
HAI(jackals.working, tc=2.5*60, dc=1000)



