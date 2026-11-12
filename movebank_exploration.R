require(units)
require(dplyr)
require(ggplot2)
require(move2)
require(rnaturalearth)

require(move2)

require(wildlifeDI)

require(sf)

# track <- movebank_download_study("Galapagos Albatrosses",
#                                  sensor_type_id = "gps"
# )
# track
# 
# 
# ggplot() +
#   theme_linedraw() +
#   geom_sf(data = track) +
#   geom_sf(data = mt_track_lines(track), aes(color = `individual_local_identifier`)) +
#   coord_sf(
#     crs = sf::st_crs("+proj=aeqd +lon_0=-83 +lat_0=-6 +units=km"),
#     xlim = c(-1000, 600),
#     ylim = c(-800, 700)
#   )
# 
# head(track)


######################################################################################################


#Olympic Cougar Project

cougars <- movebank_download_study("Olympic Cougar Project",
            sensor_type_id = "gps",
            'license-md5'='1198d389e5b16f02945e1d5fa131bee0'
)

help("movebank_download_study")
#   Al, Angie, Aziah, Bunny, Damir - Al intersects with all 4
# ->Damir and Aziah
#   Edgar and Aziah (Edgar-Damir maybe family?)
#   Freddy intersects with Angie and Bunny
#   Ghost and Bunny - lots of overlap
#   Ghost and Aziah - little overlap

head(cougars)
glimpse(cougars)
class(cougars)

#Deployment IDs
#Aziah - Aziah_87535 - 7479815930
#Damir - Damir_87516 - 7479816056

unique(cougars$deployment_id)

# glimpse(Aziah_87535)
# glimpse(Damir_87516)
# 
# range(Damir_87516$timestamp)
# range(Aziah_87535$timestamp)
# start <- "2021-05-01 00:00:00 UTC"
# end <- "2021-05-15 00:00:00 UTC"
# 
# Damir_working <- Damir_87516 %>%
#   filter(timestamp>start, timestamp<end)
# 
# Aziah_working <- Aziah_87535 %>%
#   filter(timestamp>start, timestamp<end)
# 
# range(Aziah_working$timestamp)
# range(Damir_working$timestamp)

cougars_working <- cougars %>%
  filter(event_id%in%Aziah_87535$`event-id`|
           event_id%in%Damir_87516$`event-id`
         )

unique(cougars_working$deployment_id)
class(cougars_working)

resolution(cougars_working$timestamp)
#60


ggplot(cougars_working) + 
  geom_sf(aes(color=as.factor(deployment_id)))

#traj - cougars_working (contains both tracks)

#traj1
cougars_damir <- cougars %>%
  filter(event_id%in%Damir_87516$`event-id`)

#traj2
cougars_aziah <- cougars %>%
  filter(event_id%in%Aziah_87535$`event-id`)

###################################################################################################

#Minta's Coefficient

#run Lixn with default arguments

Lixn(cougars_working)
#Lixn=0.514, pixn=1.69e-5



#now explore parameters appropriate to the data:
#    hr (optional)spatial polygon \code{sf} object associated with the home range (or some other form of) spatial range estimate for each individual in \code{traj}. The hr polygon should have a corresponding ID column with the same column name as in \code{traj}. If NULL (the default) the MCP home range estimate will be used for each individual.
#    tc time threshold for determining simultaneous fixes (in seconds) -- see function: \code{GetSimultaneous}.


## time threshold

cougars_sim0 <- GetSimultaneous(cougars_damir, cougars_aziah, tc=0)
#yields 554 fixes (277 pairs)
cougars_sim60 <- GetSimultaneous(cougars_damir, cougars_aziah, tc=60)
#yields 1026 fixes (513 pairs)
GetSimultaneous(cougars_damir, cougars_aziah, tc=120)
#yields 1026 fixes
GetSimultaneous(cougars_damir, cougars_aziah, tc=300)
#yields 1026 fixes

table(cougars_working$deployment_id)
table(cougars_sim0$deployment_id)
table(cougars_sim60$deployment_id)

?GetSimultaneous



#   tc=60 seconds

ggplot(cougars_sim60) + 
  geom_sf(aes(color=as.factor(deployment_id)))


?Lixn



#run Lixn with appropriate parameters


Lixn(cougars_working, tc=60)
#Lixn=0.536 pixn=8.7e-10

###################################################################################################


#Half-Weight Association Index



#   hr	(optional)spatial polygon sf object associated with the home range (or some other form of) spatial range estimate for each individual in traj. The hr polygon should have a corresponding ID column with the same column name as in traj. If NULL (the default) the MCP home range estimate will be used for each individual.
#   tc	time threshold for determining simultaneous fixes – see function: GetSimultaneous.
#   dc	distance tolerance limit (in appropriate units) for defining when two fixes are spatially together.


##  tc=60 seconds

##  distance tolerance limit


#contact analysis: https://cran.r-project.org/web/packages/wildlifeDI/vignettes/wildlifeDI-vignette-contact_analysis.html


dcPlot(cougars_working, tc=60, dmax=10000)

?dcPlot

?conProcess

contacts <- conProcess(cougars_working, tc=60)

#these aren't running, so I'm just going to try out some thresholds

HAI(cougars_working, tc=60, dc=50)
HAI(cougars_working, tc=60, dc=100)
HAI(cougars_working, tc=60, dc=500)
HAI(cougars_working, tc=60, dc=1000)
HAI(cougars_working, tc=60, dc=5000)
HAI(cougars_working, tc=60, dc=10000)
HAI(cougars_working, tc=60, dc=50000)

cougars_damir |>
  mt_distance() |>
  head()

cougars_working |>
  mt_distance() |>
  head()


#units: m

?st_convex_hull

hr_damir <- cougars_damir |>
  st_convex_hull()

class(hr_damir)



hr_damir

ggplot(hr_damir) +
  geom_sf()
