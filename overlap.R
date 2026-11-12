require(units)
require(dplyr)
require(ggplot2)
require(move2)
require(wildlifeDI)
require(sf)
require(igraph)


#load telemetry data
jackals <- movebank_download_study("Black-backed jackal, Etosha National Park, Namibia",
                                   sensor_type_id = "gps",
                                   'license-md5'='317da200a36e001f293f10af9f6d26d1')

#based on prior work, selected 10 individuals known to be connected
jackal_list <- c('CM08',
                 'CM09',
                 'CM44',
                 'CM69',
                 'CM10',
                 'CM83',
                 'CM47',
                 'CM62',
                 'CM05',
                 'CM15'
)

#filter to smaller dataset to facilitate computing
jackals.ntwk <- jackals %>%
  filter(individual_local_identifier %in% jackal_list)

#create empty tibble to store results
overlap_summary <- tibble(
  ID1=character(),
  ID2=character(),
  pct_overlap_j1=double(),
  pct_overlap_j2=double()
)




for(i in jackal_list) {
  for(j in jackal_list) {
      
      j1 <- i
      j2 <- j
      
      jackals.working <- jackals.ntwk %>%           #filter to desired dyad
        filter(individual_local_identifier==j1|
                 individual_local_identifier==j2)
      
      mcp_sf <- jackals.working %>%                 #generate minimum convex hulls (home ranges)
        group_by(individual_local_identifier) %>%
        summarize(geometry = st_union(geometry)) %>%
        st_convex_hull()
      
      overlap <- st_intersection(mcp_sf%>%           #calculate overlap of polygons
                                   filter(individual_local_identifier==j1),
                                 mcp_sf%>%
                                   filter(individual_local_identifier==j2))
      
      area_o <- st_area(overlap)                      #calculate area of overlap
      area_j1 <- st_area(mcp_sf%>%
                           filter(individual_local_identifier==j1))
      area_j2 <- st_area(mcp_sf%>%
                           filter(individual_local_identifier==j2))
      
      overlap_summary <- overlap_summary %>%          #append to data frame
        add_row(
          ID1=j1, 
          ID2=j2, 
          pct_overlap_j1=as.double((area_o/area_j1)*100), #percent intersect relative to area A
          pct_overlap_j2=as.double((area_o/area_j2)*100)  #percent intersect relative to area B
        )
  }
}

print(overlap_summary)

